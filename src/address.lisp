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
      ;; mapped branch. This also covers ::/120, which needs no
      ;; check of its own: :: unwraps to 0.0.0.0 (rejected), ::1
      ;; unwraps to 0.0.0.1 (rejected — zero first octet).
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
      ;; Everything outside 2000::/3. IANA allocates global unicast from
      ;; 2000::/3 and nowhere else; the rest of the space is reserved,
      ;; local, multicast, or a special-purpose block the registry marks
      ;; not globally reachable. So the test outside it is what public
      ;; *is*, not a list of what it is not. Beyond the unspecified address
      ;; and loopback, which the IPv4-compatible unwrap above already sends
      ;; to 0.0.0.0/8, it refuses at once:
      ;; discard-only 100::/64 (RFC 6666), the dummy prefix 100:0:0:1::/64
      ;; (RFC 9780), the local-use translation prefix 64:ff9b:1::/48 (RFC
      ;; 8215) — whose v4 is not in bytes 12..15, so the NAT64 unwrap above
      ;; rightly does not take it — the IPv4-translated ::ffff:0:0:0/96,
      ;; SRv6 SIDs 5f00::/16 (RFC 9602), unique local fc00::/7 (AWS's
      ;; fd00:ec2::254 among them), link-local fe80::/10 and the reserved
      ;; fe00::/8 around it, and multicast ff00::/8.
      ;;
      ;; And whatever is assigned out of reserved space later, which is
      ;; where 5f00::/16 came from. A list of exceptions to public misses
      ;; the block nobody has added yet; a statement of what public is
      ;; does not. If IANA ever opens unicast outside 2000::/3, this
      ;; refuses it until the clause is widened — the direction for an
      ;; SSRF filter to be wrong in.
      ((/= (logand b0 #xe0) #x20) nil)
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
      ;; 2001::/23 — IETF Protocol Assignments, refused whole: the v6
      ;; counterpart of 192.0.0.0/24, refused whole for the same reason.
      ;; The registry marks the block not globally reachable unless a
      ;; more specific allocation says otherwise, and the ones that do are
      ;; protocol machinery rather than anywhere an application fetches
      ;; from by URL: anycast service addresses, AMT relays, AS112-v6, the
      ;; drone-ID prefix. The rest is benchmarking (2001:2::/48, the
      ;; counterpart of 198.18.0.0/15), unassigned space, and three that
      ;; need saying:
      ;;
      ;;   Teredo, 2001::/32 (RFC 4380), is the one with teeth. Bytes 4-7
      ;;   are the Teredo server's IPv4 address and bytes 12-15 the
      ;;   client's, obfuscated by complement — a wrapper around two IPv4
      ;;   addresses in exactly the sense ::ffff: and 2002:: are, and the
      ;;   policy for those is to refuse the private-shaped ones whether or
      ;;   not the carrier is live. It cannot be unwrapped instead: the
      ;;   client field is complemented and the server field is the relay
      ;;   rather than the destination, so there is no single carried
      ;;   address to hand IPV4-PUBLIC-P.
      ;;
      ;;   ORCHIDv2, 2001:20::/28 (RFC 7343), is an identifier space — a
      ;;   hash of a key standing where an address would — not a locator,
      ;;   though the registry marks it reachable.
      ;;
      ;;   2001:10::/28 was RFC 4843's ORCHID, expired and returned to the
      ;;   pool, which is a reason to refuse it rather than to skip it, as
      ;;   the 2002::/16 and 192.88.99.0/24 clauses give: a prefix returned
      ;;   to the pool reaches whoever is handed it next.
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (<= (aref bytes 2) #x01))
       nil)
      ;; 2001:db8::/32 — documentation (RFC 3849).
      ((and (= (aref bytes 0) #x20) (= (aref bytes 1) #x01)
            (= (aref bytes 2) #x0d) (= (aref bytes 3) #xb8))
       nil)
      ;; 3fff::/20 — documentation (RFC 9637), the second block, sized for
      ;; examples that need room for whole networks.
      ((and (= (aref bytes 0) #x3f) (= (aref bytes 1) #xff)
            (< (aref bytes 2) #x10))
       nil)
      (t t))))

(defun is-public-address-p (bytes family)
  "Return T if BYTES is a publicly routable IP address, NIL otherwise.
   FAMILY is :INET for a 4-byte IPv4 or :INET6 for a 16-byte IPv6.
   A wrong-length BYTES or unknown FAMILY returns NIL — callers get
   the conservative answer.

   IPv4 rejects: unspecified (0/8), RFC 1918 private (10/8, 172.16/12,
   192.168/16), RFC 6598 CGNAT (100.64/10), loopback (127/8), link-local
   (169.254/16, which holds the cloud metadata address 169.254.169.254),
   IETF protocol assignments (192.0.0.0/24), documentation (TEST-NET-1/2/3),
   6to4 relay anycast (192.88.99.0/24), benchmarking (198.18/15),
   multicast (224/4), and reserved and broadcast (240/4).

   IPv6 is public only inside global unicast, 2000::/3 — the one range
   IANA allocates unicast from. Everything outside it is refused, which
   covers loopback and unspecified, discard-only 100::/64, the dummy
   prefix 100:0:0:1::/64, the local-use translation prefix 64:ff9b:1::/48,
   IPv4-translated ::ffff:0:0:0/96, SRv6 SIDs 5f00::/16, unique local
   fc00::/7 (the AWS metadata address fd00:ec2::254 included),
   link-local fe80::/10 within the reserved fe00::/8, multicast ff00::/8,
   and every reserved range. Inside it, refused: IETF protocol
   assignments 2001::/23 (Teredo, benchmarking, ORCHID and ORCHIDv2
   among them — Teredo's two embedded IPv4 addresses are a relay and a
   complemented client, so there is no single carried address to unwrap),
   and documentation 2001:db8::/32 and 3fff::/20.

   Three IPv4-in-IPv6 forms are unwrapped and classified by the IPv4
   they carry rather than refused: IPv4-mapped ::ffff:0:0/96,
   IPv4-compatible ::/96, and NAT64 64:ff9b::/96, ahead of the 2000::/3
   test that would otherwise refuse them; 6to4 2002::/16 inside it. An
   attacker cannot bypass the check by rewriting 127.0.0.1 as
   ::ffff:127.0.0.1.

   The list is exhaustive on purpose. This filter's value is that its
   coverage can be read off rather than inferred, so a prefix added to the
   code and not to this list costs more than one left out of both.

   This helper answers \"is this concrete address safe to dial?\"
   It does not resolve hostnames. Apps that accept hostnames must
   resolve first and call this on each resolved address."
  (case family
    (:inet  (and (= (length bytes) 4)  (ipv4-public-p bytes)))
    (:inet6 (and (= (length bytes) 16) (ipv6-public-p bytes)))
    (otherwise nil)))
