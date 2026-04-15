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
         (keys
          (loop for key-obj in keys-array
                for kty = (json-get key-obj "kty")
                for crv = (json-get key-obj "crv")
                when (and kty crv
                          (string= kty "EC")
                          (string= crv "P-256"))
                collect (let ((x (base64url-decode (json-get key-obj "x")))
                              (y (base64url-decode (json-get key-obj "y"))))
                          (unless (and (= (length x) 32) (= (length y) 32))
                            (error "JWKS: EC P-256 key coordinates must be 32 bytes"))
                          (make-jwt-key
                           :kid (or (json-get key-obj "kid") "")
                           :x x :y y)))))
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (k keys)
        (let ((kid (jwt-key-kid k)))
          (when (and (> (length kid) 0) (gethash kid seen))
            (error "JWKS: duplicate kid ~s" kid))
          (setf (gethash kid seen) t))))
    keys))

;;; ---------------------------------------------------------------------------
;;; JWT verification
;;; ---------------------------------------------------------------------------

(defun jwt-verify (token keys)
  "Verify a JWT token string against a list of JWT-KEY structs.
   Returns the claims as an alist if valid, NIL if invalid.
   Checks: algorithm is ES256, signature is valid, token is not expired."
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
  "Extract a claim value from a JWT claims alist."
  (json-get claims key))

;;; ---------------------------------------------------------------------------
;;; Internal helpers
;;; ---------------------------------------------------------------------------

(defun jwt-split (token)
  "Split a JWT token on dots. Returns a list of strings."
  (let ((parts nil)
        (start 0))
    (loop for i from 0 below (length token)
          when (char= (char token i) #\.)
          do (push (subseq token start i) parts)
             (setf start (1+ i)))
    (push (subseq token start) parts)
    (nreverse parts)))

(defun jwt-current-time ()
  "Return current time as a Unix epoch timestamp."
  ;; CL universal time starts at 1900-01-01, Unix epoch at 1970-01-01
  (- (get-universal-time) 2208988800))
