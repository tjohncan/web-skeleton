(in-package :web-skeleton)

;;; ===========================================================================
;;; IP address classification
;;;
;;; IS-PUBLIC-ADDRESS-P answers "is this byte vector a publicly routable
;;; IP?" — loopback, RFC 1918 private, link-local, CGNAT, multicast,
;;; documentation prefixes and cloud metadata endpoints all answer NIL.
;;; It classifies one concrete address and does nothing else. It knows
;;; nothing about hostnames, URL schemes, or when a dial is about to
;;; happen.
;;;
;;; Knowing when is *FETCH-ADDRESS-FILTER*'s job, and the two are meant
;;; to be used together. That pairing is the whole integration:
;;;
;;;   (setf *fetch-address-filter*
;;;         (lambda (ip family host)
;;;           (declare (ignore host))
;;;           (is-public-address-p ip family)))
;;;
;;; Set once before START-SERVER. The framework then consults it for
;;; every address an outbound fetch is about to dial, IP literals
;;; included — those skip DNS entirely, so a resolver-only check would
;;; walk straight past http://169.254.169.254/.
;;;
;;; Why the filter and not a check in the handler: an app that resolves a
;;; hostname itself, approves the address, and then hands DEFER-TO-FETCH
;;; the *name* has checked one resolution and dialed another. The
;;; framework resolves again, and an attacker's nameserver is free to
;;; answer differently the second time. Nothing the app does closes that,
;;; because the app does not control the dial. The filter runs on the
;;; resolution that is actually used, which is the only place it can be
;;; closed.
;;;
;;; Calling this directly is fine when the address is one the app already
;;; holds — an allowlist entry, a literal it parsed. Checking an address
;;; and then passing a name is the shape to avoid.
;;;
;;; DEPLOYMENT.md "Fetch URL safety (SSRF)" carries the rest: what a
;;; refusal does to an in-flight fetch, and why an allowlist of upstream
;;; hosts is better still where the app can name them.
;;; ===========================================================================

(defun format-ip (bytes)
  "Format an address byte vector for logs. 4 bytes → dotted quad,
   16 bytes → colon-separated lowercase hex groups (no :: compression —
   a log line wants unambiguous identity, not canonical form). Anything
   else → \"<addr>\". Bracketless: a caller needing the RFC 3986
   authority form adds its own brackets."
  (case (length bytes)
    (4  (format nil "~{~d~^.~}" (coerce bytes 'list)))
    (16 (format nil "~(~{~x~^:~}~)"
                (loop for i from 0 below 16 by 2
                      collect (logior (ash (aref bytes i) 8)
                                      (aref bytes (1+ i))))))
    (t  "<addr>")))

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
      ;; 192.88.99.0/24 — 6to4 relay anycast. Deprecated by RFC 7526,
      ;; which is the reason to refuse it rather than to skip it: the
      ;; relays are gone, so a packet sent here reaches whatever picked
      ;; up the anycast prefix afterwards. Still routed, still not a
      ;; destination an app meant to reach.
      ((and (= a 192) (= b 88) (= c 99)) nil)
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
      ;; ::ffff:0:0/96 — IPv4-mapped IPv6 (RFC 4291 §2.5.5.2).
      ;; Classify via the embedded v4 so an attacker cannot rewrite
      ;; 127.0.0.1 as ::ffff:127.0.0.1. Must come before the
      ;; IPv4-compatible branch below because that one's all-zero
      ;; prefix is a superset of this one's 0x...ffff prefix.
      ((and (loop for i from 0 below 10 always (zerop (aref bytes i)))
            (= (aref bytes 10) #xff)
            (= (aref bytes 11) #xff))
       (let ((v4 (make-array 4 :element-type '(unsigned-byte 8))))
         (replace v4 bytes :start2 12 :end2 16)
         (ipv4-public-p v4)))
      ;; ::a.b.c.d — deprecated IPv4-compatible IPv6 (RFC 4291
      ;; §2.5.5.1). The prefix is retired and modern Linux doesn't
      ;; special-case it (unlike the mapped prefix above), so
      ;; routing to ::127.0.0.1 on current kernels goes through
      ;; normal IPv6 routing and fails — but the address is
      ;; semantically IPv4, and the framework's policy is to
      ;; reject every dangerous-looking address. Unwrap and
      ;; classify via the embedded v4 for consistency with the
      ;; mapped branch. Also subsumes the old ::/120 check: ::
      ;; unwraps to 0.0.0.0 (rejected), ::1 unwraps to 0.0.0.1
      ;; (rejected — zero first octet).
      ((loop for i from 0 below 12 always (zerop (aref bytes i)))
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
      ;; 2002::/16 — 6to4 encapsulation (RFC 3056). The IPv4 payload
      ;; lives in bytes 2..5; the rest of the address is an arbitrary
      ;; host identifier. 6to4 is deprecated (RFC 7526) and current
      ;; Linux kernels do not route 2002::/16 by default, so this is
      ;; primarily shape-consistency with the ::ffff: and 64:ff9b::
      ;; branches above — the framework's policy is to reject every
      ;; private-shaped address whether its v4 carrier is live on
      ;; the host or not. An attacker who rewrites 10.0.0.1 as
      ;; 2002:0a00:0001:: cannot then use it to reach an internal
      ;; host.
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x02))
       (let ((v4 (make-array 4 :element-type '(unsigned-byte 8))))
         (replace v4 bytes :start2 2 :end2 6)
         (ipv4-public-p v4)))
      ;; 2001:db8::/32 — documentation
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x0d) (= (aref bytes 3) #xb8))
       nil)
      ;; 2001::/32 — Teredo (RFC 4380). The one with teeth in this group:
      ;; bytes 4-7 are the Teredo server's IPv4 address and bytes 12-15 are
      ;; the client's, obfuscated by complement — so a Teredo address is a
      ;; wrapper around two IPv4 addresses in exactly the sense ::ffff: and
      ;; 2002:: are, and the stated policy for those is to refuse the
      ;; private-shaped ones whether or not the carrier is live. Refused
      ;; whole rather than unwrapped: the client field is complemented and
      ;; the server field is the relay rather than the destination, so there
      ;; is no single carried address to hand IPV4-PUBLIC-P.
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x00) (= (aref bytes 3) #x00))
       nil)
      ;; 2001:2::/48 — benchmarking (RFC 5180), the v6 counterpart of
      ;; 198.18.0.0/15.
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x00) (= (aref bytes 3) #x02)
            (= (aref bytes 4) #x00) (= (aref bytes 5) #x00))
       nil)
      ;; 2001:10::/28 — ORCHID (RFC 4843), non-routable by definition.
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x00) (= (logand (aref bytes 3) #xf0) #x10))
       nil)
      ;; 100::/64 — discard-only (RFC 6666). Traffic to it is dropped, so
      ;; nothing is reachable there and a fetch aimed at it can only be a
      ;; mistake or a probe.
      ((and (= (aref bytes 0) #x01) (= (aref bytes 1) #x00)
            (every #'zerop (subseq bytes 2 8)))
       nil)
      ;; fc00::/7 — unique local (RFC 4193), covers AWS fd00:ec2::254
      ((= (logand b0 #xfe) #xfc) nil)
      ;; fe00::/8 — a conservative superset of link-local (fe80::/10),
      ;; deprecated site-local (fec0::/10), and everything else in the
      ;; IETF-reserved fe00::/8 range. None of it is publicly routable,
      ;; so the /8 gate is both simpler and safer than narrow matches.
      ((= b0 #xfe) nil)
      ;; ff00::/8 — multicast
      ((= b0 #xff) nil)
      (t t))))

(defun is-public-address-p (bytes family)
  "Return T if BYTES is a publicly routable IP address, NIL otherwise.
   FAMILY is :INET for a 4-byte IPv4 or :INET6 for a 16-byte IPv6.
   A wrong-length BYTES or unknown FAMILY returns NIL — callers get
   the conservative answer.

   Rejects: loopback (127/8, ::1), link-local (169.254/16, fe00::/8 superset incl. fe80::/10),
   RFC 1918 private (10/8, 172.16/12, 192.168/16), RFC 6598 CGNAT
   (100.64/10), RFC 4193 unique local (fc00::/7), multicast (224/4,
   ff00::/8), documentation prefixes (TEST-NET-1/2/3, 2001:db8::/32),
   reserved / future, unspecified, benchmarking, and cloud metadata
   IPs (169.254.169.254, fd00:ec2::254 via fc00::/7).

   Also on the v6 side: 6to4 (2002::/16, unwrapped), Teredo (2001::/32,
   refused whole — its two embedded IPv4 addresses are a relay and a
   complemented client, so there is no single carried address to unwrap),
   IPv6 benchmarking (2001:2::/48), ORCHID (2001:10::/28) and discard-only
   (100::/64).

   The list is exhaustive on purpose. This filter's value is that its
   coverage can be read off rather than inferred, so a prefix added to the
   code and not to this list costs more than one left out of both.

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
