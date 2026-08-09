(in-package :web-skeleton)

;;; ===========================================================================
;;; JWT Validation (ES256 / RFC 7519)
;;;
;;; Verifies JWTs signed with ES256 (ECDSA P-256 + SHA-256).
;;; Parses JWKS key sets for key rotation support.
;;; Uses the framework's JSON parser (src/json.lisp).
;;; ===========================================================================

(defparameter *jwt-clock-skew* 60
  "Seconds of clock skew tolerance for JWT exp/nbf checks.
   Accounts for clock drift between the token issuer and this server.")

;;; ---------------------------------------------------------------------------
;;; JWKS parsing
;;; ---------------------------------------------------------------------------

(defstruct jwt-key
  "An ES256 public key from a JWKS key set."
  (kid "" :type string)
  (x   (make-array 0 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (*)))
  (y   (make-array 0 :element-type '(unsigned-byte 8))
       :type (simple-array (unsigned-byte 8) (*))))

(defun %jwks-decode-coordinate (b64 name)
  "Decode a JWKS EC coordinate, restating a codec error as a JWKS error.
   BASE64URL-DECODE rejects non-canonical input, and a 32-byte
   coordinate encodes to 43 characters — a three-character final group,
   so the trailing-bit check applies to every JWKS this server loads.
   Unwrapped, a bad coordinate surfaces mid-rotation as a bare
   \"base64: ...\" from a function with no handler-case, which reads as
   a framework bug to whoever is holding the log. Same reasoning as the
   missing-coordinate guard below, and the same stakes as it: this path
   raises rather than returning NIL, so it fails all verification, not
   one request."
  (handler-case (base64url-decode b64)
    (error (e)
      (error "JWKS: EC P-256 key ~a is not valid base64url (~a)" name e))))

(defun parse-jwks (json-string)
  "Parse a JWKS JSON string into a list of JWT-KEY structs.
   Extracts ES256 keys only (kty=EC, crv=P-256). Rejects JWKS sets
   containing two EC keys with the same kid — an accidental duplicate
   during a rotation window would make (FIND kid ...) silently pick
   whichever one sorted first, and that's rarely what the issuer
   meant. Missing kty or crv are skipped cleanly (not type-erroring
   on STRING= against NIL)."
  (let* ((jwks (json-parse json-string))
         (keys-array (json-get jwks "keys"))
         ;; "keys" must be an array. A JWKS with "keys": {} or a scalar is
         ;; malformed per RFC 7517 §5, and LOOP FOR ... IN on a non-list
         ;; raises a raw SBCL type error out of a function with no
         ;; handler-case. Reject with the same shape as every other JWKS
         ;; complaint so an issuer bug reads as an issuer bug.
         (keys-array (cond ((null keys-array) nil)
                           ((listp keys-array) keys-array)
                           (t (error "JWKS: \"keys\" must be an array"))))
         (keys
          (loop for key-obj in keys-array
                for kty = (json-get key-obj "kty")
                for crv = (json-get key-obj "crv")
                when (and kty crv
                          (string= kty "EC")
                          (string= crv "P-256"))
                collect (let ((x-b64 (json-get key-obj "x"))
                              (y-b64 (json-get key-obj "y")))
                          ;; Guard before base64url-decode so a JWKS
                          ;; with a missing coordinate raises a clean
                          ;; "missing x or y" error instead of a
                          ;; type-error from inside BASE64URL-DECODE —
                          ;; rotating on a coordinate-missing issuer
                          ;; key should look like an issuer bug, not
                          ;; a framework bug, to the operator reading
                          ;; the log.
                          (unless (and x-b64 y-b64)
                            (error "JWKS: EC P-256 key missing x or y"))
                          (let ((x (%jwks-decode-coordinate x-b64 "x"))
                                (y (%jwks-decode-coordinate y-b64 "y")))
                            (unless (and (= (length x) 32) (= (length y) 32))
                              (error "JWKS: EC P-256 key coordinates must be 32 bytes"))
                            (make-jwt-key
                             :kid (or (json-get key-obj "kid") "")
                             :x x :y y))))))
    ;; Only dedup explicit (non-empty) kids. RFC 7517 §4.5 says kid
    ;; is OPTIONAL; a minimal static JWKS or a rotation-window set
    ;; with two kidless keys is spec-legal. Treating the "" default
    ;; as a real kid collapsed both into a single-slot collision and
    ;; raised on legitimate input. jwt-verify's single-key fallback
    ;; (no kid in the token) still works — it picks when the key
    ;; list has exactly one entry.
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (k keys)
        (let ((kid (jwt-key-kid k)))
          (unless (zerop (length kid))
            (when (gethash kid seen)
              (error "JWKS: duplicate kid ~s" kid))
            (setf (gethash kid seen) t)))))
    keys))

;;; ---------------------------------------------------------------------------
;;; JWT verification
;;; ---------------------------------------------------------------------------

(defun jwt-verify (token keys)
  "Verify a JWT token string against a list of JWT-KEY structs.
   Returns the claims as a JSON-OBJECT if valid, NIL if invalid — read
   them with JWT-CLAIM (or JSON-GET, which JWT-CLAIM is a thin alias for).
   Checks: algorithm is ES256, signature is valid, token is not expired.

   The claims value is always non-NIL on success, including for a token
   whose payload is {}. It used to be a bare alist, so an empty-but-valid
   claim set came back as NIL and was indistinguishable from a rejected
   token at the call site."
  (handler-case
      (let ((parts (jwt-split token)))
        (unless (= (length parts) 3)
          (return-from jwt-verify nil))
        (let* ((header-b64 (first parts))
               (payload-b64 (second parts))
               (sig-b64 (third parts))
               ;; Decode header and payload
               (header (json-parse (sb-ext:octets-to-string
                                    (base64url-decode header-b64)
                                    :external-format :utf-8)))
               (alg (json-get header "alg"))
               (kid (json-get header "kid")))
          ;; Must be ES256
          (unless (string= alg "ES256")
            (return-from jwt-verify nil))
          ;; Reject any token carrying a crit header parameter
          ;; (RFC 7515 §4.1.11). crit lists extensions the recipient
          ;; must understand and process; we implement none, so any
          ;; non-empty crit is an instant reject. (A malformed empty
          ;; crit array parses to NIL and slips past this check — it
          ;; is spec-invalid but carries no extension claim, so the
          ;; simpler test is enough for the security-critical case.)
          (when (json-get header "crit")
            (return-from jwt-verify nil))
          ;; Find matching key (reject kidless tokens when multiple keys exist)
          (let ((key (cond
                       (kid (find kid keys :key #'jwt-key-kid :test #'string=))
                       ((= (length keys) 1) (first keys))
                       (t nil))))
            (unless key
              (return-from jwt-verify nil))
            ;; Decode claims and check exp/nbf BEFORE expensive crypto.
            ;; This prevents DoS via expired tokens forcing ECDSA verification.
            ;; Safe: we already parse unauthenticated JSON for the header above.
            (let* ((claims (json-parse (sb-ext:octets-to-string
                                        (base64url-decode payload-b64)
                                        :external-format :utf-8)))
                   (now (jwt-current-time)))
              (let ((exp (json-get claims "exp")))
                (when exp
                  (unless (numberp exp)
                    (return-from jwt-verify nil))
                  (when (<= exp (- now *jwt-clock-skew*))
                    (return-from jwt-verify nil))))
              (let ((nbf (json-get claims "nbf")))
                (when nbf
                  (unless (numberp nbf)
                    (return-from jwt-verify nil))
                  (when (> nbf (+ now *jwt-clock-skew*))
                    (return-from jwt-verify nil))))
              ;; Claims are valid — now verify signature
              (let* ((signing-input (concatenate 'string header-b64 "." payload-b64))
                     (hash (sha256 (sb-ext:string-to-octets signing-input
                                                             :external-format :ascii)))
                     (sig (base64url-decode sig-b64)))
                (unless (and (= (length sig) 64)
                             (ecdsa-verify-p256 hash sig
                                                (jwt-key-x key) (jwt-key-y key)))
                  (return-from jwt-verify nil))
                claims)))))
    (error () nil)))

(defun jwt-claim (claims key)
  "Extract a claim value from the JSON-OBJECT returned by JWT-VERIFY."
  (json-get claims key))

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun jwt-split (token)
  "Split a JWT token on dots. Returns a list of 3 strings for a
   well-formed JWS Compact Serialization, or NIL for any token with
   more or fewer dots, or any token carrying base64 padding. Bails out
   as soon as a fourth dot appears so a malformed token with many dots
   does not allocate O(dots) substrings before rejection."
  (let ((parts nil)
        (start 0)
        (dots 0))
    (loop for i from 0 below (length token)
          for c = (char token i)
          do (cond
               ;; RFC 7515 §2: every JWS segment is base64url with all
               ;; trailing '=' omitted. '=' is the only illegal character
               ;; worth naming here — '+', '/' and whitespace are absent
               ;; from the base64url table, so they raise out of
               ;; BASE64URL-DECODE into JWT-VERIFY's handler and turn
               ;; themselves away. '=' decodes successfully, and that is
               ;; the whole reason this case exists.
               ;;
               ;; It matters for the signature segment, the one part of
               ;; a token outside the signed input — "sig" and "sig=="
               ;; decode to the same 64 bytes,
               ;; so both verify and one signature gets two token
               ;; strings. Padding the header or payload changes the
               ;; bytes the signature is checked against, so those two
               ;; defend themselves; the rule is applied to all three
               ;; anyway because it is one rule, and this is already
               ;; where a malformed token is turned away.
               ((char= c #\=)
                (return-from jwt-split nil))
               ((char= c #\.)
                (incf dots)
                (when (> dots 2)
                  (return-from jwt-split nil))
                (push (subseq token start i) parts)
                (setf start (1+ i)))))
    (unless (= dots 2)
      (return-from jwt-split nil))
    (push (subseq token start) parts)
    (nreverse parts)))

(defun jwt-current-time ()
  "Return current time as a Unix epoch timestamp."
  ;; CL universal time starts at 1900-01-01, Unix epoch at 1970-01-01
  (- (get-universal-time) 2208988800))
