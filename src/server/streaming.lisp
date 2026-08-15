;;;; streaming.lisp — server-generated streaming responses
;;;;
;;;; The framework has three chunked *decoders* and no encoder: everything
;;;; chunked it has ever seen arrived from an upstream. Producing a stream
;;;; is the other direction, and it starts here.
;;;;
;;;; This file is deliberately not fetch.lisp. The decoders live there
;;;; because they belong to reading an upstream response; encoding belongs
;;;; to writing our own.

(in-package :web-skeleton)

;;; ---------------------------------------------------------------------------
;;; Chunked transfer encoding — the producing side
;;;
;;; Encode to the intersection of what the three readers require, not to
;;; what any one of them forgives. DECODE-CHUNKED-BODY demands strict CRLF
;;; after both the size line and the data ("Lax trailing CRLF is a smuggling
;;; primitive"); PARSE-CHUNKED-SIZE-LINE tolerates BWS around the hex and
;;; strips chunk-extensions; CHUNKED-BODY-COMPLETE-P is lax on purpose and
;;; validates nothing. Their tolerances exist to survive other people's
;;; output. They are not a licence for ours, so what goes on the wire is
;;; bare lowercase hex, CRLF, data, CRLF — no extensions, no whitespace,
;;; nothing any strict downstream re-parser could read differently.
;;; ---------------------------------------------------------------------------

(defun chunked-terminator ()
  "The last-chunk and its empty trailer section: 0 CRLF CRLF.

   Its own entry point, and that is the whole point of it. If the
   terminator could also be produced by encoding a zero-length payload,
   then any producer that happened to emit nothing — a filter that
   matched no rows this tick, a serializer handed an empty batch — would
   end the message early while emitting bytes every decoder accepts.
   The stream would be truncated, not corrupted, so nothing would raise
   anywhere and the reader would simply believe it had the whole thing.
   A generated property can be made to catch that; a function that cannot
   emit the byte sequence does not have the failure mode. Same shape as
   CONNECTION-QUEUE-WRITE beside CONNECTION-APPEND-WRITE: two doors named
   for intent, so the dangerous one is never reached by accident.

   Shared and never mutated, like the pre-built ping frame — the write
   queue holds vectors by reference and advances a per-connection offset
   through them."
  (load-time-value
   (sb-ext:string-to-octets
    (coerce (list #\0 #\Return #\Newline #\Return #\Newline) 'string)
    :external-format :ascii)
   t))

(defun encode-chunk (bytes &key (start 0) (end (length bytes)))
  "Frame BYTES[START..END) as one chunk. Returns the framed vector, or
   NIL when the range is empty.

   An empty write is a no-op rather than an error. A streaming producer
   naturally has nothing to say on some passes, so refusing would put a
   guard at every call site, and the cost of one missing guard is a
   truncated stream — the exact failure CHUNKED-TERMINATOR is split out
   to prevent. Nothing is lost by dropping it, because an empty chunk
   carries nothing.

   That reasoning is about chunked framing and stops here. It must not
   climb to the streaming surface above: an empty *SSE* write is not
   nothing — a bare comment line is the keepalive that stops an
   intermediary reaping an idle stream, and swallowing it would show up
   only as connections dying behind a proxy, in the deployment the docs
   assume and in none of the tests."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((len (- end start)))
    (when (plusp len)
      (let* ((header (sb-ext:string-to-octets
                      (format nil "~(~x~)~c~c" len #\Return #\Newline)
                      :external-format :ascii))
             (hlen (length header))
             (out (make-array (+ hlen len 2)
                              :element-type '(unsigned-byte 8))))
        (replace out header)
        (replace out bytes :start1 hlen :start2 start :end2 end)
        (setf (aref out (+ hlen len)) 13
              (aref out (+ hlen len 1)) 10)
        out))))
