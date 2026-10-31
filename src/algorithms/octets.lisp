(in-package :web-skeleton)

;;; ===========================================================================
;;; The input contract of the digests, the MAC and the verifier
;;;
;;; SHA1, SHA256 and ECDSA-VERIFY-P256 each have a pure-Lisp implementation
;;; and, once web-skeleton-tls is loaded, a libssl one swapped into the same
;;; function cell; HMAC-SHA256 calls whichever SHA256 is there. Both sides of
;;; the swap take their input through OCTETS-ARGUMENT, so an exported name
;;; accepts and refuses the same things whether or not libssl is loaded.
;;;
;;; The libssl side is why this has to be one function and not two habits.
;;; It hands its input to the C library by address, and SB-SYS:VECTOR-SAP
;;; takes any specialized vector without asking what its elements are. A
;;; Lisp string is one — SBCL stores a character string at four bytes a
;;; character — so a string given to it would be hashed as the first
;;; (LENGTH DATA) bytes of that storage: a quarter of the characters, and a
;;; digest anyone could reproduce by matching that quarter. Nothing would
;;; raise. The pure-Lisp side refuses a string, but libssl is what CI, the
;;; Dockerfile and every application that fetches over HTTPS load, so the
;;; configuration that would show the mistake is the one fewest run — and an
;;; HMAC keyed with a long string would depend on the first quarter of the
;;; secret alone.
;;; ===========================================================================

(defun octets-argument (data caller)
  "DATA as a simple (UNSIGNED-BYTE 8) vector, for CALLER to hash, MAC or
   verify.

   A simple one is returned as it is. A non-simple one — adjustable, with a
   fill pointer, displaced — is copied, up to its fill pointer, so a buffer
   an application accumulated into gives the same answer under either
   implementation; the copy is also what the libssl side can take the
   address of.

   Anything else is refused, strings above all. The bytes of a string
   depend on an encoding, and choosing one silently would be choosing it
   wrong for somebody; the error says how to choose. It describes DATA by
   its kind and never by its value or its length, because DATA may be a
   key."
  (typecase data
    ((simple-array (unsigned-byte 8) (*)) data)
    ((array (unsigned-byte 8) (*))
     (coerce data '(simple-array (unsigned-byte 8) (*))))
    (string
     (error "~(~a~) takes an (unsigned-byte 8) vector, not a string. A string ~
             has no bytes until an encoding is chosen: ~
             (sb-ext:string-to-octets string :external-format :utf-8)."
            caller))
    (array
     (error "~(~a~) takes an (unsigned-byte 8) vector, not an array of ~(~s~). ~
             Bytes held some other way can be copied into one: ~
             (coerce bytes '(vector (unsigned-byte 8)))."
            caller (array-element-type data)))
    (t
     (error "~(~a~) takes an (unsigned-byte 8) vector, not ~(~s~)."
            caller (type-of data)))))
