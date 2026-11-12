(in-package :web-skeleton)

;;; ===========================================================================
;;; SipHash-2-4 (Aumasson and Bernstein, 2012)
;;;
;;; A keyed hash: 64 bits out of a 128-bit key and a message. It was made
;;; for hash tables whose keys someone else chooses. A table files a key
;;; under its hash, and whoever can work the hash out can choose keys that
;;; all land in one place, so every lookup walks them all. Without the
;;; key the hash of a message cannot be worked out, whatever is known of
;;; other messages' hashes.
;;;
;;; 2-4 is the number of rounds: two for each eight bytes of message, and
;;; four at the end. It is the variant its authors recommend, and the one
;;; other languages' tables use or began with.
;;;
;;; Here for STORE-KEY-HASH, which a store files its keys under. The
;;; pieces are inline functions on the four words of state, passed in and
;;; handed back as values, so a caller that keeps the state in variables
;;; of its own keeps it in registers. SIPHASH-2-4 puts them together for
;;; a byte vector, and WITH-SIPHASH-2-4 for a message that arrives a byte
;;; at a time.
;;; ===========================================================================

(defstruct (sip-key (:constructor make-sip-key (k0 k1))
                    (:copier nil))
  "A SipHash key: 128 bits as two 64-bit words, K0 the first eight of its
   sixteen bytes read little-endian and K1 the last eight."
  (k0 0 :type (unsigned-byte 64))
  (k1 0 :type (unsigned-byte 64)))

(defun sip-key-from-octets (octets)
  "The SIP-KEY that the first sixteen of OCTETS spell."
  (flet ((word (start)
           (loop for i from 0 below 8
                 sum (ash (aref octets (+ start i)) (* 8 i)))))
    (make-sip-key (word 0) (word 8))))

(declaim (inline u64+ u64-rotate-left
                 sip-initial-state sip-round sip-compress sip-finalize))

(defun u64+ (a b)
  "Add two 64-bit integers with wraparound."
  (declare (type (unsigned-byte 64) a b))
  (logand #xFFFFFFFFFFFFFFFF (+ a b)))

(defun u64-rotate-left (n count)
  "Rotate 64-bit integer N left by COUNT bits."
  (declare (type (unsigned-byte 64) n)
           (type (integer 1 63) count))
  (logand #xFFFFFFFFFFFFFFFF
          (logior (ash n count)
                  (ash n (- count 64)))))

(defun sip-initial-state (key)
  "The four words of state a hash under KEY starts from, as values: the
   constants that spell \"somepseudorandomlygeneratedbytes\", each mixed
   with half of the key."
  (declare (type sip-key key))
  (let ((k0 (sip-key-k0 key))
        (k1 (sip-key-k1 key)))
    (values (logxor k0 #x736f6d6570736575)
            (logxor k1 #x646f72616e646f6d)
            (logxor k0 #x6c7967656e657261)
            (logxor k1 #x7465646279746573))))

(defun sip-round (v0 v1 v2 v3)
  "One SipRound over the state, which comes back as values."
  (declare (type (unsigned-byte 64) v0 v1 v2 v3))
  (setf v0 (u64+ v0 v1)
        v1 (u64-rotate-left v1 13)
        v1 (logxor v1 v0)
        v0 (u64-rotate-left v0 32)
        v2 (u64+ v2 v3)
        v3 (u64-rotate-left v3 16)
        v3 (logxor v3 v2)
        v0 (u64+ v0 v3)
        v3 (u64-rotate-left v3 21)
        v3 (logxor v3 v0)
        v2 (u64+ v2 v1)
        v1 (u64-rotate-left v1 17)
        v1 (logxor v1 v2)
        v2 (u64-rotate-left v2 32))
  (values v0 v1 v2 v3))

(defun sip-compress (v0 v1 v2 v3 word)
  "Take WORD, the next eight bytes of the message read little-endian,
   into the state: two rounds. The state comes back as values."
  (declare (type (unsigned-byte 64) v0 v1 v2 v3 word))
  (setf v3 (logxor v3 word))
  (multiple-value-setq (v0 v1 v2 v3) (sip-round v0 v1 v2 v3))
  (multiple-value-setq (v0 v1 v2 v3) (sip-round v0 v1 v2 v3))
  (values (logxor v0 word) v1 v2 v3))

(defun sip-finalize (v0 v1 v2 v3)
  "The 64-bit hash, of a state that has taken the whole message: four
   rounds more. The last word taken is the bytes left over after the
   whole words, with the message's length modulo 256 as its top byte."
  (declare (type (unsigned-byte 64) v0 v1 v2 v3))
  (setf v2 (logxor v2 #xFF))
  (dotimes (i 4)
    (multiple-value-setq (v0 v1 v2 v3) (sip-round v0 v1 v2 v3)))
  (logxor v0 v1 v2 v3))

(defun siphash-2-4 (key octets)
  "SipHash-2-4 of OCTETS, a byte vector, under KEY, a SIP-KEY. Returns
   the 64-bit hash as an integer; the reference implementation writes the
   same value out as eight bytes, little-endian."
  (declare (type sip-key key)
           (type (simple-array (unsigned-byte 8) (*)) octets))
  (let* ((length (length octets))
         (whole (- length (mod length 8))))
    (flet ((word-at (start count)
             ;; COUNT bytes from START, the first the lowest.
             (let ((word 0))
               (declare (type (unsigned-byte 64) word))
               (dotimes (i count word)
                 (setf word (logior word (ash (aref octets (+ start i))
                                              (* 8 i))))))))
      (declare (inline word-at))
      (multiple-value-bind (v0 v1 v2 v3) (sip-initial-state key)
        (loop for start from 0 below whole by 8
              do (multiple-value-setq (v0 v1 v2 v3)
                   (sip-compress v0 v1 v2 v3 (word-at start 8))))
        (multiple-value-setq (v0 v1 v2 v3)
          (sip-compress v0 v1 v2 v3
                        (logior (word-at whole (- length whole))
                                (ash (logand length #xFF) 56))))
        (sip-finalize v0 v1 v2 v3)))))

(defmacro with-siphash-2-4 ((absorb finish key) &body body)
  "Run BODY with SipHash-2-4 under KEY, a SIP-KEY, ready to take its
   message a byte at a time. In BODY, (ABSORB byte) gives it the next
   byte, and (FINISH) answers the 64-bit hash of the bytes given. FINISH
   is called once, and nothing is absorbed after it.

   For a message that is walked rather than held: the bytes need never
   be gathered into a vector. Both names are local functions over
   variables of the macro's own, so the state is not an object and a hash
   allocates nothing. It costs more a byte than SIPHASH-2-4 does, each
   byte being a call."
  (let ((v0 (gensym "V0")) (v1 (gensym "V1"))
        (v2 (gensym "V2")) (v3 (gensym "V3"))
        (word (gensym "WORD")) (filled (gensym "FILLED"))
        (total (gensym "TOTAL")) (compress (gensym "COMPRESS")))
    `(multiple-value-bind (,v0 ,v1 ,v2 ,v3) (sip-initial-state ,key)
       (declare (type (unsigned-byte 64) ,v0 ,v1 ,v2 ,v3))
       ;; The message word being filled, how many bytes it holds, and the
       ;; count of bytes absorbed, kept modulo 256 because that is all of
       ;; it the last word carries.
       (let ((,word 0)
             (,filled 0)
             (,total 0))
         (declare (type (unsigned-byte 64) ,word)
                  (type (integer 0 8) ,filled)
                  (type (unsigned-byte 8) ,total))
         (labels ((,compress ()
                    (multiple-value-setq (,v0 ,v1 ,v2 ,v3)
                      (sip-compress ,v0 ,v1 ,v2 ,v3 ,word))
                    (setf ,word 0
                          ,filled 0))
                  (,absorb (byte)
                    (declare (type (unsigned-byte 8) byte))
                    (setf ,word (logior ,word
                                        (ash byte
                                             (* 8 (the (integer 0 7) ,filled))))
                          ,filled (1+ ,filled)
                          ,total (logand #xFF (1+ ,total)))
                    (when (= ,filled 8)
                      (,compress))
                    nil)
                  (,finish ()
                    ;; The last word: the bytes left over, and the count in
                    ;; its top byte, which they never reach.
                    (setf ,word (logior ,word (ash ,total 56)))
                    (,compress)
                    (sip-finalize ,v0 ,v1 ,v2 ,v3)))
           (declare (ignorable (function ,absorb) (function ,finish)))
           ,@body)))))
