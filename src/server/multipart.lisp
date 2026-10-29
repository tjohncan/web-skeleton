;;;; multipart.lisp — multipart/form-data, buffered or as it arrives
;;;;
;;;; A form with a file input is sent as multipart/form-data (RFC 7578, on
;;;; RFC 2046 §5.1): the body is a run of parts, each a few header fields
;;;; and then its content, divided by a boundary line whose text the client
;;;; chose and named in the request's Content-Type. PARSE-MULTIPART reads a
;;;; body that has been buffered, from :HANDLER. RECEIVE-MULTIPART is a body
;;;; plan for a body handler to return: it reads the body as it arrives, so
;;;; a file can go to disk or on to an upstream without the body ever being
;;;; held. Both are one parser, MULTIPART-STEP, which takes bytes as they
;;;; come and answers one event at a time.
;;;;
;;;; Where the format leaves room for two readings, the parser refuses the
;;;; body rather than choosing one. A form field is what a filter in front
;;;; of an application inspects, and a body the filter and the application
;;;; read differently carries a field past the filter. So:
;;;;
;;;;   - a line that begins with the boundary is a delimiter, or the body is
;;;;     refused; it is never content (RFC 2046 §5.1.1 matches the boundary
;;;;     against the start of a line, not the whole line)
;;;;   - every line the format defines ends in CRLF; a bare LF ends none
;;;;   - a part has one Content-Disposition, of type form-data, with one
;;;;     name; at most one filename, Content-Type and
;;;;     Content-Transfer-Encoding; and no parameter in the extended form,
;;;;     filename* among them (RFC 7578 §4.2)
;;;;   - a Content-Transfer-Encoding other than 7bit, 8bit or binary is
;;;;     refused, neither decoded nor passed through as content (§4.7)
;;;;   - the request's Content-Type names one boundary, of the characters
;;;;     RFC 2046 allows
;;;;
;;;; What the format says to ignore is read and dropped: a preamble before
;;;; the first boundary, transport padding after one, an epilogue after the
;;;; last, and header fields in a part other than those three.
;;;;
;;;; Names and filenames are read the way browsers write them, following
;;;; the HTML standard's form encoding: a quoted value is the characters
;;;; between its quotes, and a backslash among them is an ordinary
;;;; character, as in a Windows path. A browser writes a quote inside a
;;;; value as %22, CR as %0D and LF as %0A, and leaves % itself alone, so
;;;; those escapes cannot be undone reliably. A value is returned as sent.

(in-package :web-skeleton)

(defconstant +max-part-header-fields+ 16
  "Header fields one part may carry. RFC 7578 gives a part three that mean
   anything — Content-Disposition, Content-Type and Content-Transfer-Encoding
   — and says the rest are to be ignored, so sixteen leaves room for a
   client's extras. Fixed rather than a knob: the bytes of a part's head are
   bounded by :MAX-PART-HEAD-SIZE already, and this bounds how many fields
   those bytes become, each one a cons and two strings.")

(defstruct (form-part (:constructor %make-form-part))
  "One part of a multipart/form-data body.

     NAME          its Content-Disposition's name: the form field's name
     FILENAME      its filename as the client sent it, or NIL when it sent
                   none. A file input sends one, \"\" when no file was
                   chosen; a text field does not. Browsers send a file's
                   base name, or for a folder upload its path below the
                   chosen folder. Either is a label from the client and not
                   a path to use as one: check every segment before any of
                   it names something on disk.
     CONTENT-TYPE  its Content-Type as sent, or NIL; RFC 7578 reads a part
                   without one as text/plain
     HEADERS       every header field of the part, as an alist of
                   (lowercase-name . value) in order, as HTTP-REQUEST-HEADERS
     BODY          its content, or NIL for a part RECEIVE-MULTIPART streamed
                   to :ON-PART-DATA"
  (name "" :type string)
  (filename nil :type (or null string))
  (content-type nil :type (or null string))
  (headers nil :type list)
  (body nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defun find-form-part (parts name)
  "The first of PARTS whose name is NAME, or NIL. A form can send one name
   more than once — a file input that takes several files, a select that
   takes several options — and FIND-FORM-PARTS returns all of them."
  (find name parts :key #'form-part-name :test #'string=))

(defun find-form-parts (parts name)
  "Every one of PARTS whose name is NAME, in order."
  (remove-if-not (lambda (part) (string= (form-part-name part) name)) parts))

(defun %excerpt (text)
  "TEXT, cut to 64 characters for an error message. The text is a client's,
   and a message is a log line."
  (if (> (length text) 64)
      (concatenate 'string (subseq text 0 64) "...")
      text))

(defun form-part-text (part)
  "PART's content decoded as UTF-8, or NIL for a part that was streamed.

   A browser sends a text field in the charset of the page the form is
   on, UTF-8 for a page served as UTF-8, and gives the part no Content-Type
   of its own. Raises HTTP-PARSE-ERROR, answered 400, when the bytes are not
   UTF-8."
  (let ((body (form-part-body part)))
    (when body
      (handler-case (sb-ext:octets-to-string body :external-format :utf-8)
        (sb-int:character-decoding-error ()
          (http-parse-error "form field ~a is not UTF-8"
                            (%excerpt (form-part-name part))))))))

;;; ---------------------------------------------------------------------------
;;; Header field parameters
;;; ---------------------------------------------------------------------------

(defun %token-char-p (char)
  "True for a tchar, the characters RFC 9110 lets a token hold."
  (let ((code (char-code char)))
    (and (< code 128) (tchar-byte-p code))))

(defun %parameters-head (value)
  "What VALUE, a header field value with parameters, says before them: the
   text before its first ;, without the whitespace around it."
  (string-trim '(#\Space #\Tab) (subseq value 0 (or (position #\; value)
                                                     (length value)))))

(defun %parse-parameters (value context)
  "The parameters of VALUE, a header field value of the form
   ITEM *( OWS \";\" OWS [ NAME OWS \"=\" OWS VALUE ] ), as an alist of
   (lowercase-name . value) in order.

   A value is a token, or a quoted string read as browsers write one: the
   characters between its quotes, a backslash among them an ordinary
   character. The whitespace around = is MIME's, which allows it, where
   RFC 9110's grammar for HTTP's own fields does not. Raises
   HTTP-PARSE-ERROR (400), naming CONTEXT, for anything else, and for a
   parameter given twice: which of the two a reader keeps is the kind of
   choice two readers make differently."
  (let ((len (length value))
        (i (or (position #\; value) (length value)))
        (params '())
        ;; The names so far, to find one given twice. A table, not a search
        ;; of PARAMS: a part head of the default 16 KiB holds thousands of
        ;; parameters, and each searching all those before it costs the
        ;; square of their count in worker time.
        (seen (make-hash-table :test #'equal)))
    (labels ((fail (what &rest args)
               (http-parse-error "~a: ~?" context what args))
             (skip-ows ()
               (loop while (and (< i len) (member (char value i) '(#\Space #\Tab)))
                     do (incf i)))
             (token-end (from)
               (or (position-if-not #'%token-char-p value :start from) len)))
      (loop
        (skip-ows)
        (when (>= i len) (return))
        (unless (char= (char value i) #\;)
          (fail "expected ; at ~s" (%excerpt (subseq value i))))
        (incf i)
        (skip-ows)
        ;; RFC 9110 lets a parameter be empty: \"a;;b=c\" and a trailing ;.
        (when (and (< i len) (char/= (char value i) #\;))
          (let* ((name-end (token-end i))
                 (name (string-downcase (subseq value i name-end))))
            (when (= name-end i)
              (fail "a parameter without a name at ~s" (%excerpt (subseq value i))))
            (setf i name-end)
            (skip-ows)
            (unless (and (< i len) (char= (char value i) #\=))
              (fail "parameter ~a has no value" (%excerpt name)))
            (incf i)
            (skip-ows)
            (let ((text (if (and (< i len) (char= (char value i) #\"))
                            (let ((close (position #\" value :start (1+ i))))
                              (unless close
                                (fail "parameter ~a: a quoted value with no closing quote"
                                      (%excerpt name)))
                              (prog1 (subseq value (1+ i) close)
                                (setf i (1+ close))))
                            (let ((text-end (token-end i)))
                              (when (= text-end i)
                                (fail "parameter ~a has no value" (%excerpt name)))
                              (prog1 (subseq value i text-end)
                                (setf i text-end))))))
              (when (gethash name seen)
                (fail "parameter ~a given twice" (%excerpt name)))
              (setf (gethash name seen) t)
              (push (cons name text) params))))))
    (nreverse params)))

(defun %boundary-char-p (char)
  "True for a character RFC 2046 §5.1.1 allows in a boundary."
  (or (char<= #\0 char #\9)
      (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (find char "'()+_,-./:=? ")))

(defun multipart-boundary (request)
  "The boundary of REQUEST's multipart/form-data body, as bytes, read from
   its Content-Type.

   Raises HTTP-PARSE-ERROR: 415 when the Content-Type is not
   multipart/form-data or is missing; 400 when it is multipart/form-data
   without exactly one boundary of the characters RFC 2046 allows, from one
   to seventy of them and not ending in a space, and when the request
   carries more than one Content-Type."
  (let ((types (get-headers request "content-type")))
    (when (rest types)
      (http-parse-error "more than one Content-Type"))
    (let ((type (first types)))
      (unless type
        (http-reject 415 "no Content-Type, where multipart/form-data is expected"))
      (let ((media (%parameters-head type)))
        (unless (string-equal media "multipart/form-data")
          (http-reject 415 "Content-Type ~a is not multipart/form-data"
                       (%excerpt media))))
      (let ((boundary (cdr (assoc "boundary" (%parse-parameters type "Content-Type")
                                  :test #'string=))))
        (unless boundary
          (http-parse-error "multipart/form-data without a boundary"))
        (unless (and (<= 1 (length boundary) 70)
                     (every #'%boundary-char-p boundary)
                     (char/= (char boundary (1- (length boundary))) #\Space))
          (http-parse-error "multipart/form-data boundary ~s is not one RFC 2046 allows"
                            (%excerpt boundary)))
        (sb-ext:string-to-octets boundary :external-format :ascii)))))

(defun %parse-disposition (value)
  "The name and filename of a part, from its Content-Disposition VALUE, as
   (values NAME FILENAME). Raises HTTP-PARSE-ERROR (400) unless VALUE is
   form-data with a name, at most one filename, and no parameter in the
   extended form: RFC 7578 §4.2 forbids filename*, and a filename* beside a
   filename is two names for one file."
  (let ((type (%parameters-head value)))
    (unless (string-equal type "form-data")
      (http-parse-error "multipart: a part's Content-Disposition is ~a, not form-data"
                        (%excerpt type))))
  (let ((params (%parse-parameters value "multipart: a part's Content-Disposition")))
    (dolist (param params)
      (when (find #\* (car param))
        (http-parse-error "multipart: Content-Disposition parameter ~a is not accepted; ~
                           multipart/form-data has no extended form"
                          (%excerpt (car param)))))
    (let ((name (assoc "name" params :test #'string=)))
      (unless name
        (http-parse-error "multipart: a part without a name"))
      (values (cdr name) (cdr (assoc "filename" params :test #'string=))))))

;;; ---------------------------------------------------------------------------
;;; The parser
;;; ---------------------------------------------------------------------------

(defstruct (multipart-parser (:constructor %make-multipart-parser))
  "Where the parse of one multipart/form-data body has got. See
   MULTIPART-STEP."
  ;; CRLF \"--\" boundary, and Horspool's shift for each byte value: how far
  ;; the delimiter can move along when that byte is under its last position.
  (delimiter nil :type (simple-array (unsigned-byte 8) (*)))
  (shift nil :type (simple-array (unsigned-byte 8) (256)))
  ;; :PREAMBLE, :DATA, :AFTER-BOUNDARY, :CLOSE-DASH, :PADDING, :BOUNDARY-CR,
  ;; :HEAD, :EPILOGUE, :EPILOGUE-CR or :DONE.
  (state :preamble :type keyword)
  ;; Bytes held from earlier input because a delimiter may begin among them
  ;; and the input ran out before it could tell: HELD[HELD-START,
  ;; HELD-FILL), shorter than the delimiter. HELD-VIRTUAL of them, at
  ;; HELD-START, are a CRLF the parser put there and the input never sent:
  ;; the one that lets a delimiter's dashes begin the body, or a part's
  ;; content, with no CRLF of their own before them. They are never content.
  (held nil :type (simple-array (unsigned-byte 8) (*)))
  (held-start 0 :type fixnum)
  (held-fill 0 :type fixnum)
  (held-virtual 0 :type fixnum)
  ;; A part's head, gathered here when it does not arrive in one piece.
  (head nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (head-fill 0 :type fixnum)
  (max-parts nil :type (or null (integer 0)))
  (max-part-head-size 0 :type (integer 0))
  ;; Parts begun so far, against MAX-PARTS.
  (parts 0 :type fixnum)
  ;; The part whose head was read last; whether its content is still
  ;; arriving, so that :DATA and :PART-END are its; and the bytes of its
  ;; head, blank line included.
  (part nil :type (or null form-part))
  (part-open nil :type boolean)
  (head-length 0 :type fixnum))

(defun %hold-virtual-crlf (parser)
  "Hold a CRLF the input never sent, ahead of whatever comes next. A
   delimiter is CRLF, two dashes and the boundary, but at the start of a
   body and at the start of a part's content its CRLF can be missing: the
   body begins with the dashes, or a part with no content has its head's
   blank line serve as the delimiter's CRLF. Holding one CRLF there finds
   the delimiter in both, and the same search serves every place one can
   be."
  (let ((held (multipart-parser-held parser)))
    (setf (aref held 0) 13
          (aref held 1) 10
          (multipart-parser-held-start parser) 0
          (multipart-parser-held-fill parser) 2
          (multipart-parser-held-virtual parser) 2)))

(defun make-multipart-parser (boundary &key max-parts (max-part-head-size 16384))
  "A parser for a multipart/form-data body whose boundary is BOUNDARY, the
   bytes MULTIPART-BOUNDARY returns. MAX-PARTS is the most parts the body
   may have, or NIL for no limit; MAX-PART-HEAD-SIZE the most bytes one
   part's head may take, its blank line included."
  (let* ((m (+ 4 (length boundary)))
         (delimiter (make-array m :element-type '(unsigned-byte 8)))
         (shift (make-array 256 :element-type '(unsigned-byte 8) :initial-element m))
         (parser nil))
    (setf (aref delimiter 0) 13
          (aref delimiter 1) 10
          (aref delimiter 2) 45
          (aref delimiter 3) 45)
    (replace delimiter boundary :start1 4)
    (loop for i from 0 below (1- m)
          do (setf (aref shift (aref delimiter i)) (- m 1 i)))
    (setf parser (%make-multipart-parser
                  :delimiter delimiter
                  :shift shift
                  :held (make-array (1- m) :element-type '(unsigned-byte 8))
                  :max-parts max-parts
                  :max-part-head-size max-part-head-size))
    (%hold-virtual-crlf parser)
    parser))

(defun multipart-parser-done-p (parser)
  "True once PARSER has read the close delimiter and the end of its line:
   the body is complete, and anything after it is the epilogue, which is
   ignored."
  (and (member (multipart-parser-state parser) '(:epilogue :done)) t))

(defun %delimiter-search (delimiter shift buf start end)
  "Search BUF[START, END) for DELIMITER by Horspool's method. Returns
   (values WHOLE PARTIAL): WHOLE, where its first whole occurrence begins,
   or NIL; and when there is none, PARTIAL, the first position from which
   the bytes to END are the start of DELIMITER with the rest still to come,
   or NIL."
  (declare (type (simple-array (unsigned-byte 8) (*)) delimiter buf)
           (type (simple-array (unsigned-byte 8) (256)) shift)
           (type fixnum start end))
  (let* ((m (length delimiter))
         (last (1- m))
         (j start))
    (declare (type fixnum m last j))
    (loop while (<= (+ j m) end)
          do (let ((k last))
               (declare (type fixnum k))
               (loop while (and (>= k 0)
                                (= (aref buf (+ j k)) (aref delimiter k)))
                     do (decf k))
               (when (< k 0)
                 (return-from %delimiter-search (values j nil)))
               (incf j (aref shift (aref buf (+ j last))))))
    ;; From J on the delimiter cannot fit before END. Every position before J
    ;; was compared whole, or passed over by a shift, and a shift rules out a
    ;; partial match as surely as a whole one: the byte it was read from lies
    ;; before END, inside the stretch a partial match would have to agree on.
    (loop for i of-type fixnum from j below end
          when (and (= (aref buf i) 13)
                    (loop for k of-type fixnum from 1 below (- end i)
                          always (= (aref buf (+ i k)) (aref delimiter k))))
            do (return-from %delimiter-search (values nil i)))
    (values nil nil)))

(defun %held-candidate (parser buf pos end)
  "The first place among PARSER's held bytes where the delimiter may begin,
   given them and BUF[POS, END) after them. Returns (values I WHOLE): I its
   index in the held buffer, or NIL; WHOLE true when the delimiter is all
   there."
  (let* ((held (multipart-parser-held parser))
         (fill (multipart-parser-held-fill parser))
         (delimiter (multipart-parser-delimiter parser))
         (m (length delimiter)))
    (loop for i from (multipart-parser-held-start parser) below fill
          when (= (aref held i) 13)
            do (let ((k 0)
                     (agrees t))
                 ;; The held bytes from I are fewer than the delimiter's.
                 (loop for h from i below fill
                       while agrees
                       do (if (= (aref held h) (aref delimiter k))
                              (incf k)
                              (setf agrees nil)))
                 (loop for b from pos below end
                       while (and agrees (< k m))
                       do (if (= (aref buf b) (aref delimiter k))
                              (incf k)
                              (setf agrees nil)))
                 (when agrees
                   (return-from %held-candidate (values i (= k m))))))
    (values nil nil)))

(defun %scan-for-delimiter (parser buf pos end)
  "Look for the delimiter in what PARSER holds and in BUF[POS, END).
   Returns (values RESULT NEXT SOURCE FROM TO):

     :CONTENT    SOURCE[FROM, TO) is content: bytes before a delimiter, or
                 bytes no delimiter can begin in. The next call goes on from
                 NEXT.
     :DELIMITER  a delimiter, ending at NEXT, with nothing before it that
                 has not already been returned as content.
     :MORE       the input ran out where a delimiter may begin. What may be
                 its start is held, and NEXT is END."
  (let ((held (multipart-parser-held parser))
        (start (multipart-parser-held-start parser))
        (fill (multipart-parser-held-fill parser))
        (delimiter (multipart-parser-delimiter parser)))
    (flet ((drop-held ()
             (setf (multipart-parser-held-start parser) 0
                   (multipart-parser-held-fill parser) 0
                   (multipart-parser-held-virtual parser) 0)))
      (when (< start fill)
        (multiple-value-bind (i whole) (%held-candidate parser buf pos end)
          (let ((from (+ start (multipart-parser-held-virtual parser)))
                (to (or i fill)))
            (cond
              ;; Held bytes the input sent, before the place a delimiter may
              ;; begin, or all of them when there is no such place: content.
              ((< from to)
               (if i
                   (setf (multipart-parser-held-start parser) i
                         (multipart-parser-held-virtual parser) 0)
                   (drop-held))
               (return-from %scan-for-delimiter (values :content pos held from to)))
              ;; Only the virtual CRLF was held, and no delimiter follows it.
              ((null i) (drop-held))
              ;; I is the held bytes' start from here on.
              (whole
               (drop-held)
               (return-from %scan-for-delimiter
                 (values :delimiter (+ pos (- (length delimiter) (- fill i))))))
              (t
               ;; A delimiter may begin at I, and the input ran out first:
               ;; every byte of it agrees, so all of it joins what is held.
               (let ((kept (- fill i)))
                 (replace held held :start2 i :end2 fill)
                 (replace held buf :start1 kept :start2 pos :end2 end)
                 (setf (multipart-parser-held-start parser) 0
                       (multipart-parser-held-fill parser) (+ kept (- end pos))))
               (return-from %scan-for-delimiter (values :more end)))))))
      (multiple-value-bind (whole partial)
          (%delimiter-search delimiter (multipart-parser-shift parser) buf pos end)
        (cond
          (whole
           (if (< pos whole)
               (values :content whole buf pos whole)
               (values :delimiter (+ whole (length delimiter)))))
          (partial
           (replace held buf :start2 partial :end2 end)
           (setf (multipart-parser-held-start parser) 0
                 (multipart-parser-held-fill parser) (- end partial)
                 (multipart-parser-held-virtual parser) 0)
           (if (< pos partial)
               (values :content end buf pos partial)
               (values :more end)))
          ((< pos end) (values :content end buf pos end))
          (t (values :more end)))))))

(defun %not-a-delimiter ()
  (http-parse-error "multipart: a line begins with the boundary and is not a delimiter"))

(defun %no-header-fields ()
  (http-parse-error "multipart: a part with no header fields, so no Content-Disposition"))

(defun %head-too-large (limit)
  (http-reject 413 "multipart: a part's head is larger than ~d bytes" limit))

(defun %head-room (parser n)
  "PARSER's head buffer, grown to hold at least N bytes, keeping what it
   has gathered."
  (let ((head (multipart-parser-head parser)))
    (if (and head (>= (length head) n))
        head
        (let ((fresh (make-array (min (multipart-parser-max-part-head-size parser)
                                      (max n 1024 (* 2 (if head (length head) 0))))
                                 :element-type '(unsigned-byte 8))))
          (when head
            (replace fresh head :end2 (multipart-parser-head-fill parser)))
          (setf (multipart-parser-head parser) fresh)))))

(defun %begin-part (parser src start end)
  "Read the part head SRC[START, END), its blank line included, into the
   part MULTIPART-STEP announces with :PART, and set PARSER to read its
   content."
  (let ((max (multipart-parser-max-parts parser)))
    (when (and max (>= (multipart-parser-parts parser) max))
      (http-reject 413 "multipart: more than ~d part~:p" max)))
  (let* ((limit (multipart-parser-max-part-head-size parser))
         (headers
           ;; A part's head is held to its own budget, and a field over it
           ;; is a body over a limit: 413, where the request's own head
           ;; answers 431.
           (let ((*max-total-header-bytes* limit)
                 (*max-header-line-length* limit)
                 (*max-header-count* +max-part-header-fields+))
             (handler-case (parse-headers-bytes src start end)
               (http-parse-error (e)
                 (if (eql (http-parse-error-status e) 431)
                     (http-reject 413 "multipart: a part's head: ~a"
                                  (http-parse-error-message e))
                     (http-parse-error "multipart: a part's head: ~a"
                                       (http-parse-error-message e))))))))
    (flet ((only (name)
             (let ((values (loop for (field . value) in headers
                                 when (string-equal field name) collect value)))
               (when (rest values)
                 (http-parse-error "multipart: a part with more than one ~a" name))
               (first values))))
      (let ((disposition (only "Content-Disposition"))
            (content-type (only "Content-Type"))
            (encoding (only "Content-Transfer-Encoding")))
        (unless disposition
          (http-parse-error "multipart: a part without a Content-Disposition"))
        (when (and encoding
                   (not (member encoding '("7bit" "8bit" "binary") :test #'string-equal)))
          (http-parse-error "multipart: Content-Transfer-Encoding ~a is not accepted; ~
                             only 7bit, 8bit and binary, which leave the content as sent"
                            (%excerpt encoding)))
        (multiple-value-bind (name filename) (%parse-disposition disposition)
          (incf (multipart-parser-parts parser))
          (setf (multipart-parser-part parser)
                (%make-form-part :name name :filename filename
                                 :content-type content-type :headers headers)
                (multipart-parser-part-open parser) t
                (multipart-parser-head-length parser) (- end start)
                (multipart-parser-state parser) :data)
          (%hold-virtual-crlf parser))))))

(defun multipart-step (parser buf pos end)
  "Read BUF[POS, END), the next bytes of PARSER's body, as far as the next
   event, and return (values EVENT NEXT SOURCE FROM TO):

     :MORE      every byte to END is read, and there is no event in them.
                NEXT is END.
     :PART      a part's head is complete. (MULTIPART-PARSER-PART PARSER) is
                its FORM-PART, with no body.
     :DATA      a run of that part's content, SOURCE[FROM, TO). SOURCE is
                BUF, or a buffer of the parser's own that the next call may
                overwrite, so a caller that keeps the bytes copies them.
     :PART-END  that part's content is complete.

   NEXT is how far into BUF this read. The next call passes the bytes from
   there on, and anything that has arrived after them: what the parser
   needs of the bytes before NEXT — the start of what may be a delimiter, a
   head not yet ended — it holds itself. A caller may stop between events
   and resume from NEXT later; nothing is read twice.

   Raises HTTP-PARSE-ERROR once the bytes that show a fault have arrived:
   400 for a body that is not multipart/form-data as this file reads it,
   413 for one past a limit. MULTIPART-PARSER-DONE-P says, once the body has
   ended, whether it ended where a body may."
  (loop
    (ecase (multipart-parser-state parser)
      ((:preamble :data)
       (multiple-value-bind (result next source from to)
           (%scan-for-delimiter parser buf pos end)
         (setf pos next)
         (ecase result
           ;; A preamble's content is no part's, and is dropped.
           (:content
            (when (eq (multipart-parser-state parser) :data)
              (return (values :data pos source from to))))
           (:delimiter
            (setf (multipart-parser-state parser) :after-boundary))
           (:more
            (return (values :more pos))))))
      ;; After the boundary: \"--\" for the last delimiter, or transport
      ;; padding and CRLF for one that a part follows.
      (:after-boundary
       (when (>= pos end) (return (values :more pos)))
       (let ((byte (aref buf pos)))
         (incf pos)
         (setf (multipart-parser-state parser)
               (case byte
                 (45 :close-dash)
                 ((32 9) :padding)
                 (13 :boundary-cr)
                 (t (%not-a-delimiter))))))
      (:close-dash
       (when (>= pos end) (return (values :more pos)))
       (unless (= (aref buf pos) 45) (%not-a-delimiter))
       (incf pos)
       (setf (multipart-parser-state parser) :epilogue)
       (when (multipart-parser-part-open parser)
         (setf (multipart-parser-part-open parser) nil)
         (return (values :part-end pos))))
      (:padding
       (when (>= pos end) (return (values :more pos)))
       (let ((byte (aref buf pos)))
         (incf pos)
         (case byte
           ((32 9))
           (13 (setf (multipart-parser-state parser) :boundary-cr))
           (t (%not-a-delimiter)))))
      (:boundary-cr
       (when (>= pos end) (return (values :more pos)))
       (unless (= (aref buf pos) 10) (%not-a-delimiter))
       (incf pos)
       (setf (multipart-parser-state parser) :head
             (multipart-parser-head-fill parser) 0)
       (when (multipart-parser-part-open parser)
         (setf (multipart-parser-part-open parser) nil)
         (return (values :part-end pos))))
      (:head
       (let ((limit (multipart-parser-max-part-head-size parser))
             (fill (multipart-parser-head-fill parser)))
         (cond
           ((>= pos end) (return (values :more pos)))
           ((zerop fill)
            ;; The head's first bytes: if all of it is here, read it where
            ;; it lies.
            (when (and (< (1+ pos) end)
                       (= (aref buf pos) 13)
                       (= (aref buf (1+ pos)) 10))
              (%no-header-fields))
            (let ((stop (scan-crlf-crlf buf pos (min end (+ pos limit)))))
              (cond
                (stop
                 (%begin-part parser buf pos (+ stop 4))
                 (return (values :part (+ stop 4))))
                ((>= (- end pos) limit) (%head-too-large limit))
                (t
                 (replace (%head-room parser (- end pos)) buf :start2 pos :end2 end)
                 (setf (multipart-parser-head-fill parser) (- end pos))
                 (return (values :more end))))))
           (t
            ;; More of a head gathered from earlier input.
            (let* ((take (min (- limit fill) (- end pos)))
                   (filled (+ fill take))
                   (head (%head-room parser filled)))
              (replace head buf :start1 fill :start2 pos :end2 (+ pos take))
              (when (and (>= filled 2) (= (aref head 0) 13) (= (aref head 1) 10))
                (%no-header-fields))
              (let ((stop (scan-crlf-crlf head (max 0 (- fill 3)) filled)))
                (cond
                  (stop
                   (setf (multipart-parser-head-fill parser) 0)
                   (%begin-part parser head 0 (+ stop 4))
                   ;; The head took the bytes of BUF up to its blank line's end.
                   (return (values :part (+ pos (- (+ stop 4) fill)))))
                  ((>= filled limit) (%head-too-large limit))
                  (t
                   (setf (multipart-parser-head-fill parser) filled)
                   (return (values :more end))))))))))
      ;; After the last delimiter: transport padding, and then the end of
      ;; the body or CRLF and an epilogue, which is ignored.
      (:epilogue
       (when (>= pos end) (return (values :more pos)))
       (let ((byte (aref buf pos)))
         (incf pos)
         (case byte
           ((32 9))
           (13 (setf (multipart-parser-state parser) :epilogue-cr))
           (t (http-parse-error "multipart: bytes after the close delimiter ~
                                 that do not end its line")))))
      (:epilogue-cr
       (when (>= pos end) (return (values :more pos)))
       (unless (= (aref buf pos) 10)
         (http-parse-error "multipart: bytes after the close delimiter ~
                            that do not end its line"))
       (incf pos)
       (setf (multipart-parser-state parser) :done))
      (:done
       (return (values :more end))))))

(defun %run-append (buf filled source start end)
  "Copy SOURCE[START, END), a run of a part's body, onto BUF after its
   first FILLED bytes. Returns BUF, or the larger vector that replaces it,
   or NIL for an empty run onto NIL.

   One buffer for a part's runs, not a list of them. A run is what one
   read brought, and a client sending a byte at a time makes a run of
   every byte: kept apiece, each would cost a cell and a vector header
   besides, many times what the limit on collected bytes counts. The
   buffer doubles as it grows, so it holds at most twice what it counts,
   and a part in many runs is copied a constant number of times over."
  (let ((need (+ filled (- end start))))
    (when (> need (length buf))
      (let ((grown (make-array (max need (* 2 (length buf)))
                               :element-type '(unsigned-byte 8))))
        (when buf
          (replace grown buf :end2 filled))
        (setf buf grown)))
    (when buf
      (replace buf source :start1 filled :start2 start :end2 end))
    buf))

(defun %run-bytes (buf filled)
  "The FILLED bytes %RUN-APPEND has collected in BUF, as a vector of their
   own length: BUF itself when it is exactly full."
  (cond
    ((null buf) (make-array 0 :element-type '(unsigned-byte 8)))
    ((= filled (length buf)) buf)
    (t (subseq buf 0 filled))))

(defun %check-part-limits (who max-parts max-part-head-size)
  (unless (or (null max-parts) (and (integerp max-parts) (<= 0 max-parts)))
    (error "~a: :max-parts is ~s; it must be a non-negative integer, or NIL ~
            for no limit."
           who max-parts))
  (unless (and (integerp max-part-head-size) (plusp max-part-head-size))
    (error "~a: :max-part-head-size is ~s; it must be a positive integer of bytes."
           who max-part-head-size)))

;;; ---------------------------------------------------------------------------
;;; A buffered body
;;; ---------------------------------------------------------------------------

(defun parse-multipart (request &key (max-parts 1000) (max-part-head-size 16384))
  "The parts of REQUEST's multipart/form-data body, a list of FORM-PART in
   the order they were sent, each with its content in its BODY. For a body
   that has been read whole, by :HANDLER; RECEIVE-MULTIPART reads one as it
   arrives.

     MAX-PARTS           the most parts the body may have, 1000 by default,
                         or NIL for no limit
     MAX-PART-HEAD-SIZE  the most bytes one part's head may take, its blank
                         line included, 16 KiB by default

   Raises HTTP-PARSE-ERROR, which the framework answers with its status
   when a handler lets it go: 415 when the Content-Type is not
   multipart/form-data, 400 for a body that is not one as this file reads
   it (see its header), 413 past a limit. The message says which.

   The body is already in memory, under the cap its request was read
   with; the parts are copied out of it, so the parse costs that much
   again. A body with no parts, which a browser sends for a form with no
   fields to submit, gives NIL."
  (%check-part-limits "parse-multipart" max-parts max-part-head-size)
  (let* ((boundary (multipart-boundary request))
         (body (or (http-request-body request)
                   (make-array 0 :element-type '(unsigned-byte 8))))
         (end (length body))
         (parser (make-multipart-parser boundary :max-parts max-parts
                                                 :max-part-head-size max-part-head-size))
         (pos 0)
         (parts '())
         (runs nil)
         (filled 0))
    (loop
      (multiple-value-bind (event next source from to)
          (multipart-step parser body pos end)
        (setf pos next)
        (ecase event
          (:more (return))
          (:part (setf runs nil filled 0))
          (:data (setf runs (%run-append runs filled source from to))
                 (incf filled (- to from)))
          (:part-end
           (let ((part (multipart-parser-part parser)))
             (setf (form-part-body part) (%run-bytes runs filled))
             (push part parts))))))
    (unless (multipart-parser-done-p parser)
      (http-parse-error "multipart: the body ended before its close delimiter"))
    (nreverse parts)))

;;; ---------------------------------------------------------------------------
;;; A body received as it arrives
;;; ---------------------------------------------------------------------------

(defun receive-multipart (request &key on-part on-part-data on-part-end on-end on-abort
                                       (max-size *max-body-size*)
                                       (max-parts 1000)
                                       (max-part-head-size 16384)
                                       (max-collected-size *max-body-size*))
  "A body plan, for a body handler to return: read REQUEST's
   multipart/form-data body as it arrives, a part at a time, and let ON-END
   answer the request. Each part is either collected, its content held and
   handed to ON-END, or streamed to the application as it arrives and never
   held — so a form's text fields can be read whole while its files go to
   disk, or on to an upstream, a run at a time.

   ON-PART       (CONN PART) — a part's head has arrived: PART is its
                 FORM-PART, with no body yet. Answers what becomes of it:
                   NIL            collected: its content is held, and the
                                  part reaches ON-END with it. The default,
                                  when there is no ON-PART.
                   :STREAM        streamed: its content goes to ON-PART-DATA
                                  as it arrives and is not held, and the
                                  part does not reach ON-END
                   an HTTP-RESPONSE
                                  the body is refused, as RECEIVE-BODY's
                                  ON-DATA refuses one: the response is sent
                                  and the connection closes after it
                 Anything else is an error, since a misspelt :STREAM would
                 otherwise be a collected file.
   ON-PART-DATA  (CONN PART BYTES) — the next run of a streamed part's
                 content, a fresh vector the application may keep. Its value
                 is ignored unless it is :PAUSE or an HTTP-RESPONSE, as
                 RECEIVE-BODY's ON-DATA: :PAUSE holds the rest of the body,
                 from the byte after this run, until BODY-RESUME.
   ON-PART-END   (CONN PART) — a part's content is complete; for a collected
                 part, its BODY is set. For every part, so a field that
                 decides whether the rest is wanted — a token sent before a
                 file — can refuse the body before the file is read. Its
                 value is read as ON-PART-DATA's.
   ON-END        (CONN PARTS) — the body is complete. PARTS is the collected
                 parts in the order they were sent. Answers the request, in
                 any shape :HANDLER may answer with but :UPGRADE.
   ON-ABORT      (CONN REASON) — as RECEIVE-BODY's: once, and never once
                 ON-END has returned. :MALFORMED covers a body that is not
                 multipart/form-data as this file reads it (400), and
                 :TOO-LARGE every limit below as well as MAX-SIZE (413).

   MAX-SIZE            the cap on the whole body, as RECEIVE-BODY's:
                       *MAX-BODY-SIZE* by default, or NIL for none
   MAX-PARTS           the most parts, 1000 by default, or NIL for no limit
   MAX-PART-HEAD-SIZE  the most bytes one part's head may take, its blank
                       line included, 16 KiB by default
   MAX-COLLECTED-SIZE  the most bytes held for ON-END: the content and head
                       of every collected part. *MAX-BODY-SIZE* by default,
                       or NIL for no limit. What makes the memory a received
                       form costs a known amount, whatever MAX-SIZE allows.

   The request's Content-Type is read here, at the head: a request that is
   not multipart/form-data raises HTTP-PARSE-ERROR, which the body handler
   lets go to answer it 415 — or 400, for a Content-Type with no usable
   boundary — before any of its body is read. No plan was accepted, so
   ON-ABORT does not hear of it.

   A pause, from ON-PART-DATA or ON-PART-END, holds the body where it falls,
   within a read as well as at the end of one: nothing more is parsed,
   nothing more is called, and the bytes after the pause wait for
   BODY-RESUME. A part can be forwarded upstream a piece at a time this way,
   with FETCH-INTO on CONN, as RECEIVE-BODY's ON-DATA forwards a body.

   Everything handed over is provisional until ON-END: a body found to be
   malformed, or cut short, has had its earlier parts and runs delivered by
   the time ON-ABORT says so. The callbacks run on the worker, as
   RECEIVE-BODY's do, and hold it while they run."
  (unless (%callable-p on-end)
    (error "receive-multipart: :on-end is ~s; it must be a function of two ~
            arguments (the connection and the collected parts) that returns ~
            the response."
           on-end))
  (loop for (key fn) on (list :on-part on-part :on-part-data on-part-data
                              :on-part-end on-part-end :on-abort on-abort)
          by #'cddr
        unless (or (null fn) (%callable-p fn))
          do (error "receive-multipart: ~(~s~) is ~s; it must be a function, or NIL."
                    key fn))
  (unless (or (null max-size) (and (integerp max-size) (<= 0 max-size)))
    (error "receive-multipart: :max-size is ~s; it must be a non-negative ~
            integer of bytes, or NIL for no cap."
           max-size))
  (%check-part-limits "receive-multipart" max-parts max-part-head-size)
  (unless (or (null max-collected-size)
              (and (integerp max-collected-size) (<= 0 max-collected-size)))
    (error "receive-multipart: :max-collected-size is ~s; it must be a ~
            non-negative integer of bytes, or NIL for no limit."
           max-collected-size))
  (let ((parser (make-multipart-parser (multipart-boundary request)
                                       :max-parts max-parts
                                       :max-part-head-size max-part-head-size))
        (collected '())
        (held 0)
        (runs nil)
        (filled 0)
        (streaming nil)
        (aborted nil)
        (plan nil))
    (labels ((hold (conn n)
               (incf held n)
               (when (and max-collected-size (> held max-collected-size))
                 (notify-body-aborted conn :too-large)
                 (http-reject 413 "multipart: the parts collected for :on-end ~
                                   passed ~d bytes"
                              max-collected-size)))
             (feed (conn buf from to)
               ;; Each event in turn, until the run is spent or an answer
               ;; stops it. After every callback, whether the body is still
               ;; this plan's: one that ended it from inside — BODY-REFUSE
               ;; called rather than its response returned — has left
               ;; nothing to read the rest for.
               (let ((pos from))
                 (macrolet ((answer (form)
                              `(let ((verdict ,form))
                                 (cond
                                   ((not (eq (connection-body-receiver conn) plan))
                                    (return (values nil to)))
                                   ((typep verdict 'http-response)
                                    (return (values verdict to)))
                                   ((eq verdict :pause)
                                    (return (values :pause pos)))))))
                   (loop
                     (multiple-value-bind (event next source s e)
                         (handler-case (multipart-step parser buf pos to)
                           (http-parse-error (c)
                             (notify-body-aborted
                              conn (if (eql (http-parse-error-status c) 413)
                                       :too-large
                                       :malformed))
                             (error c)))
                       (setf pos next)
                       (ecase event
                         (:more (return (values nil to)))
                         (:part
                          (let* ((part (multipart-parser-part parser))
                                 (verdict (and on-part (funcall on-part conn part))))
                            (cond
                              ((not (eq (connection-body-receiver conn) plan))
                               (return (values nil to)))
                              ((typep verdict 'http-response)
                               (return (values verdict to)))
                              ((eq verdict :stream)
                               (unless on-part-data
                                 (error "receive-multipart: :on-part answered :STREAM ~
                                         for part ~s, and there is no :on-part-data ~
                                         to stream it to"
                                        (form-part-name part)))
                               (setf streaming t))
                              ((null verdict)
                               (setf streaming nil
                                     runs nil
                                     filled 0)
                               (hold conn (multipart-parser-head-length parser)))
                              (t
                               (error "receive-multipart: :on-part answered ~s for ~
                                       part ~s; it answers NIL to collect the part, ~
                                       :STREAM to stream it, or an HTTP-RESPONSE to ~
                                       refuse the body"
                                      verdict (form-part-name part))))))
                         (:data
                          (if streaming
                              (answer (funcall on-part-data conn
                                               (multipart-parser-part parser)
                                               (subseq source s e)))
                              (progn
                                (hold conn (- e s))
                                (setf runs (%run-append runs filled source s e))
                                (incf filled (- e s)))))
                         (:part-end
                          (let ((part (multipart-parser-part parser)))
                            (unless streaming
                              (setf (form-part-body part) (%run-bytes runs filled)
                                    runs nil
                                    filled 0)
                              (push part collected))
                            (when on-part-end
                              (answer (funcall on-part-end conn part)))))))))))
             (end-body (conn)
               ;; The body ended where the framework reads its end. Whether
               ;; it ended where a multipart body may is this parser's to
               ;; say: a close delimiter that never came is a malformed body,
               ;; and ON-ABORT hears that before the raise is answered.
               (unless (multipart-parser-done-p parser)
                 (abort-received-body plan conn :malformed)
                 (http-parse-error "multipart: the body ended before its close delimiter"))
               (funcall on-end conn (reverse collected)))
             (abort-body (conn reason)
               ;; Once. END-BODY's own abort is followed by the :ERROR its
               ;; raise earns, which is the same ending heard twice.
               (unless aborted
                 (setf aborted t)
                 (when on-abort
                   (funcall on-abort conn reason)))))
      (setf plan (%make-body-plan :kind :receive
                                  :max-size (or max-size most-positive-fixnum)
                                  :feed #'feed
                                  :on-end #'end-body
                                  :on-abort #'abort-body))
      plan)))
