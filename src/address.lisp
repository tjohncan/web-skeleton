(in-package :web-skeleton)

;;; ===========================================================================
;;; IP address classification
;;;
;;; IS-PUBLIC-ADDRESS-P answers "is this byte vector a publicly routable
;;; IP?" for SSRF allowlisting. A handler that proxies an attacker-controlled
;;; URL wants to reject loopback, RFC 1918 private, link-local, CGNAT,
;;; multicast, cloud metadata endpoints, and so on before calling
;;; HTTP-FETCH / DEFER-TO-FETCH.
;;;
;;; Usage
;;;
;;;   (defun handle-proxy (req)
;;;     (let* ((url  (get-query-param (http-request-query req) "url"))
;;;            (host (and url (host-from-url url)))    ; app-level parser
;;;            (ip   (and host (parse-ipv4-literal host))))
;;;       (unless (and ip (is-public-address-p ip :inet))
;;;         (return-from handle-proxy (make-error-response 403)))
;;;       (defer-to-fetch :get url :then ...)))
;;;
;;; The helper is deliberately unaware of hostnames or URL schemes — it
;;; answers "given a concrete address, is it safe to dial?" and nothing
;;; more. Apps that accept hostnames must resolve first (via their own
;;; DNS path) and then call this on each resolved address.
;;; ===========================================================================

(defun ipv4-public-p (bytes)
  "Return T if the 4-byte vector BYTES is a publicly routable IPv4."
  (let ((a (aref bytes 0))
        (b (aref bytes 1))
        (c (aref bytes 2)))
    (cond
      ;; 0.0.0.0/8 — \"this network\", unspecified
      ((zerop a) nil)
      ;; 10.0.0.0/8 — RFC 1918 private
      ((= a 10) nil)
      ;; 100.64.0.0/10 — RFC 6598 CGNAT
      ((and (= a 100) (<= 64 b 127)) nil)
      ;; 127.0.0.0/8 — loopback
      ((= a 127) nil)
      ;; 169.254.0.0/16 — link-local (includes 169.254.169.254 metadata)
      ((and (= a 169) (= b 254)) nil)
      ;; 172.16.0.0/12 — RFC 1918 private
      ((and (= a 172) (<= 16 b 31)) nil)
      ;; 192.0.0.0/24 — IETF protocol assignments
      ((and (= a 192) (= b 0) (= c 0)) nil)
      ;; 192.0.2.0/24 — TEST-NET-1 (documentation)
      ((and (= a 192) (= b 0) (= c 2)) nil)
      ;; 192.168.0.0/16 — RFC 1918 private
      ((and (= a 192) (= b 168)) nil)
      ;; 198.18.0.0/15 — benchmarking (RFC 2544)
      ((and (= a 198) (or (= b 18) (= b 19))) nil)
      ;; 198.51.100.0/24 — TEST-NET-2 (documentation)
      ((and (= a 198) (= b 51) (= c 100)) nil)
      ;; 203.0.113.0/24 — TEST-NET-3 (documentation)
      ((and (= a 203) (= b 0) (= c 113)) nil)
      ;; 224.0.0.0/4 — multicast
      ((<= 224 a 239) nil)
      ;; 240.0.0.0/4 — reserved / future use (includes 255.255.255.255)
      ((>= a 240) nil)
      (t t))))

(defun ipv6-public-p (bytes)
  "Return T if the 16-byte vector BYTES is a publicly routable IPv6."
  (let ((b0 (aref bytes 0)))
    (cond
      ;; ::/120 — ::, ::1, and the deprecated IPv4-compatible prefix.
      ((loop for i from 0 below 15 always (zerop (aref bytes i))) nil)
      ;; ::ffff:0:0/96 — IPv4-mapped IPv6. Classify via the embedded v4
      ;; so an attacker cannot rewrite 127.0.0.1 as ::ffff:127.0.0.1.
      ((and (loop for i from 0 below 10 always (zerop (aref bytes i)))
            (= (aref bytes 10) #xff)
            (= (aref bytes 11) #xff))
       (let ((v4 (make-array 4 :element-type '(unsigned-byte 8))))
         (replace v4 bytes :start2 12 :end2 16)
         (ipv4-public-p v4)))
      ;; 64:ff9b::/96 — NAT64 well-known prefix, same unwrap logic.
      ((and (= (aref bytes 0) 0) (= (aref bytes 1) #x64)
            (= (aref bytes 2) #xff) (= (aref bytes 3) #x9b)
            (loop for i from 4 below 12 always (zerop (aref bytes i))))
       (let ((v4 (make-array 4 :element-type '(unsigned-byte 8))))
         (replace v4 bytes :start2 12 :end2 16)
         (ipv4-public-p v4)))
      ;; 2001:db8::/32 — documentation
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x0d) (= (aref bytes 3) #xb8))
       nil)
      ;; fc00::/7 — unique local (RFC 4193), covers AWS fd00:ec2::254
      ((= (logand b0 #xfe) #xfc) nil)
      ;; fe00::/8 — link-local, deprecated site-local, reserved
      ((= b0 #xfe) nil)
      ;; ff00::/8 — multicast
      ((= b0 #xff) nil)
      (t t))))

(defun is-public-address-p (bytes family)
  "Return T if BYTES is a publicly routable IP address, NIL otherwise.
   FAMILY is :INET for a 4-byte IPv4 or :INET6 for a 16-byte IPv6.
   A wrong-length BYTES or unknown FAMILY returns NIL — callers get
   the conservative answer.

   Rejects: loopback (127/8, ::1), link-local (169.254/16, fe80::/10),
   RFC 1918 private (10/8, 172.16/12, 192.168/16), RFC 6598 CGNAT
   (100.64/10), RFC 4193 unique local (fc00::/7), multicast (224/4,
   ff00::/8), documentation prefixes (TEST-NET-1/2/3, 2001:db8::/32),
   reserved / future, unspecified, benchmarking, and cloud metadata
   IPs (169.254.169.254, fd00:ec2::254 via fc00::/7).

   IPv4-mapped IPv6 (::ffff:0:0/96) and NAT64 (64:ff9b::/96) are
   unwrapped and classified via their embedded IPv4 — an attacker
   cannot bypass the check by rewriting 127.0.0.1 as ::ffff:127.0.0.1.

   This helper answers \"is this concrete address safe to dial?\"
   It does not resolve hostnames. Apps that accept hostnames must
   resolve first and call this on each resolved address."
  (case family
    (:inet  (and (= (length bytes) 4)  (ipv4-public-p bytes)))
    (:inet6 (and (= (length bytes) 16) (ipv6-public-p bytes)))
    (otherwise nil)))
