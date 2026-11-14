(in-package :web-skeleton)

;;; ===========================================================================
;;; Logging
;;;
;;; Four levels: DEBUG, INFO, WARN, ERROR.
;;; Configurable minimum level via *log-level*.
;;; Timestamps on every line.
;;; ===========================================================================

(defvar *log-lock* (sb-thread:make-mutex :name "log")
  "Serializes log output so lines from concurrent threads don't interleave.

   This is the one lock every worker contends for. Sharing nothing in the
   request hot path is the framework's defining property — connections,
   the DNS cache, the scratch buffers and /dev/urandom are per-worker
   precisely so that no lock is needed — and LOG-MSG holds this one
   across both the FORMAT and the FORCE-OUTPUT.

   At the :INFO default that costs nothing measurable: a request that
   parses and dispatches cleanly logs nothing at all, and neither does a
   WebSocket's upgrade or close. What a client can cause at will, such as
   a malformed request, a bad frame, or an error a handler raises on what
   it was sent, goes through LOG-THROTTLED, which logs once a minute per
   worker. At :DEBUG it is several acquisitions
   per request, with every worker serialized behind them.

   Two consequences worth knowing before building on it. An access log
   would put this on the hot path by construction — one line per request,
   every request, every worker, through one mutex with a FORCE-OUTPUT
   inside it. And *LOG-STREAM* pointed at a pipe to a log shipper that
   stalls does not slow one connection; it stops the server, because the
   stalled write is holding the lock the other workers need.

   The shape of a fix is per-worker buffers drained on the existing
   maintenance tick. Not done: nothing in the framework logs often enough
   to pay for it. Recorded so that whoever adds something that does
   meets this first rather than afterwards.")

(defparameter *log-levels* '(:debug :info :warn :error)
  "Ordered from least to most severe.")

(defparameter *log-level* :info
  "Minimum level to output. Set to :debug to see everything.")

(defparameter *log-stream* nil
  "Stream to write log output to. NIL uses *standard-output*.")

(defun log-level-value (level)
  "Return the numeric severity of LEVEL.

   A level not in *LOG-LEVELS* counts as the most severe, so a line logged
   at a misspelt level, :WARNING say, is shown rather than dropped.
   START-SERVER refuses a *LOG-LEVEL* not in the list, where the same rule
   would quiet everything but errors."
  (or (position level *log-levels*) (1- (length *log-levels*))))

(defun timestamp ()
  "Return current UTC time as YYYY-MM-DD hh:mm:ss.mmm.
   Millisecond precision so log-parsing tools can preserve event
   order within a single second. sb-ext:get-time-of-day returns
   (values unix-sec microsec); the 2208988800 offset converts Unix
   epoch (1970) to universal-time epoch (1900) so DECODE-UNIVERSAL-
   TIME formats the date portion unchanged."
  (multiple-value-bind (unix-sec usec) (sb-ext:get-time-of-day)
    (multiple-value-bind (sec min hour day month year)
        (decode-universal-time (+ unix-sec 2208988800) 0)
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d"
              year month day hour min sec (floor usec 1000)))))

(defun log-msg (level format-string &rest args)
  "Log a message at LEVEL. Suppressed if below *log-level*.
   A broken-pipe or closed *log-stream* falls back to *error-output*
   rather than raising into the worker's hot path — a missed log line
   is less harmful than a logger that crashes the request pipeline.
   Holds *LOG-LOCK* across the format and the flush; see that variable
   before putting this on a per-request path."
  (when (>= (log-level-value level) (log-level-value *log-level*))
    (sb-thread:with-mutex (*log-lock*)
      (let ((stream (or *log-stream* *standard-output*))
            ;; One call is one line. ~S on any structure otherwise wraps at
            ;; *PRINT-RIGHT-MARGIN*, turning one call into a dozen lines that
            ;; no grep can reassemble — and each of them is another line
            ;; through this mutex for every reader who then has to widen
            ;; their filter to find the rest of it.
            (*print-pretty* nil)
            ;; And bounded. A cyclic or enormous value logged by mistake
            ;; should truncate, not fill a disk, on a path that holds the one
            ;; lock every worker contends for.
            (*print-length* 100)
            (*print-level* 8))
        (handler-case
            (progn
              (format stream "~a [~a] ~a~%"
                      (timestamp)
                      (string-upcase (symbol-name level))
                      (escape-control-characters
                       (format nil "~?" format-string args)))
              (force-output stream))
          (error ()
            (ignore-errors
             (format *error-output*
                     "~a [~a] (log stream failed) ~a~%"
                     (timestamp)
                     (string-upcase (symbol-name level))
                     (escape-control-characters
                      (format nil "~?" format-string args)))
             (force-output *error-output*))))))))

(defun escape-control-characters (string)
  "STRING with each control character written as \\xNN, its code in hex:
   C0, DEL and C1 alike, so a line break or a terminal escape in it reads
   as text. A backslash is written as \\x5C, as nginx's escape=default
   writes it, so a \\x in a line is always one written here: a client's
   own four characters \\x0A would otherwise read as a line feed it sent.

   The Unicode line and paragraph separators and the bidirectional
   controls are written as \\uXXXX: U+2028 and U+2029, which a browser's
   log viewer breaks a line at, and the marks, embeddings, overrides and
   isolates (U+061C, U+200E-F, U+202A-E, U+2066-9), which reorder how the
   rest of a line reads. Four digits rather than two, as \\x2028 would
   read as \\x20 and then 28.

   Done here, once for every line, because what a line says is often a
   client's: a header value, a path, a JSON parser's complaint about the
   character it met. A raw line feed in it would end the line and start a
   forged one, and an ESC would reach the terminal of anyone tailing the
   log. One call is one line, whatever it is given."
  (flet ((escaped-p (c)
           (let ((code (char-code c)))
             (or (< code 32) (= code 92) (<= 127 code 159)
                 (= code #x061C) (<= #x200E code #x200F)
                 (<= #x2028 code #x202E) (<= #x2066 code #x2069)))))
    (if (notany #'escaped-p string)
        string
        (with-output-to-string (out)
          (loop for c across string
                do (cond ((not (escaped-p c)) (write-char c out))
                         ((< (char-code c) 256)
                          (format out "\\x~2,'0X" (char-code c)))
                         (t (format out "\\u~4,'0X" (char-code c)))))))))

(defvar *log-throttle* nil
  "This worker's throttled log lines, a hash table from a kind to
   (LOGGED-AT HELD-BACK LEVEL): when the kind's line was logged, how many
   of its kind have been held back since, and the level they log at.
   Bound per worker by RUN-WORKER; NIL outside one, where LOG-THROTTLED
   logs every line.")

(defun log-throttled (level kind format-string &rest args)
  "Log a line a client can cause at will — a malformed request, a bad
   WebSocket frame. The first of KIND logs at once, and the ones that
   follow it within a minute are counted and not logged. When the minute
   is up their count is logged, in a line of its own that names the kind,
   and the kind is forgotten: its next line is a first again.

   Every line holds *LOG-LOCK*, the one lock every worker contends for,
   across a format and a flush, and these cost a client one write each. A
   flood of them logged line by line would serialize every worker on
   logging, and a stalled log pipe would stop them all. The census already
   counts each refusal, so the log need only say they are happening.

   A kind's minute is ended by whichever comes first: its next line,
   here, or the worker's once-a-second pass, in FLUSH-LOG-THROTTLE. The
   pass is what reports a burst that has ended: left to the kind's next
   line, the count would wait however long that line took to come. What a
   kind held back in the minute a worker stops in goes unreported.

   The one throttle the framework's logging has: REPORT-REFUSAL's
   connection-limit line and RUN-EVENT-LOOP's :ON-TICK error report come
   through here too, each under its own KIND.

   KIND is a keyword, or a list of one and what tells its lines apart, as
   LOG-HANDLER-ERROR's is of a keyword and a condition's type. A worker's
   table compares them with EQUAL. What a list holds has to be something
   there are few of: the table keeps an entry for each kind with a line
   in the last minute."
  (let ((table *log-throttle*))
    (if (null table)
        (apply #'log-msg level format-string args)
        (let ((entry (gethash kind table))
              (now (monotonic-seconds)))
          (cond
            ((and entry (< (- now (first entry)) 60))
             (incf (second entry)))
            (t
             ;; The first of its kind, or the first since a minute that the
             ;; worker's pass has not come round to end.
             (when entry
               (end-throttled-minute kind entry now))
             (setf (gethash kind table) (list now 0 level))
             (apply #'log-msg level format-string args)))))))

(defun end-throttled-minute (kind entry now)
  "Log how many lines of KIND were held back since its line was logged,
   when any were: a kind that held nothing back says nothing, or one
   error a day would be followed by a count of none. ENTRY is KIND's in
   *LOG-THROTTLE*, which the caller then replaces or removes.

   The line names the kind in lower case, a list's first element with the
   rest in parentheses after it: \"parse-error: 3 more lines held back in
   the last 60 seconds\", or \"handler-error (type-error): ...\" for an
   error that escaped a handler."
  (destructuring-bind (logged-at held-back level) entry
    (when (plusp held-back)
      (log-msg level "~a: ~d more line~:p held back in the last ~d seconds"
               (if (consp kind)
                   (format nil "~(~a (~{~a~^ ~})~)" (first kind) (rest kind))
                   (format nil "~(~a~)" kind))
               held-back (- now logged-at)))))

(defun flush-log-throttle (now)
  "End the minute of each kind in this worker's *LOG-THROTTLE* whose line
   was logged a minute or more before NOW, a MONOTONIC-SECONDS reading:
   its count is logged if it held any lines back, and the kind is
   forgotten. Nothing to do outside a worker.

   Called from the worker's once-a-second pass, so a count is logged
   within a second or so of its minute being up, whether or not another
   line of that kind ever comes. Forgetting the kind is also what keeps
   the table to the kinds seen in the last minute."
  (let ((table *log-throttle*))
    (when table
      ;; MAPHASH allows the entry it is visiting to be removed from the
      ;; table under it, and no entry but that one.
      (maphash (lambda (kind entry)
                 (when (>= (- now (first entry)) 60)
                   (end-throttled-minute kind entry now)
                   (remhash kind table)))
               table))))

(defun log-debug (format-string &rest args)
  (apply #'log-msg :debug format-string args))

(defun log-info (format-string &rest args)
  (apply #'log-msg :info format-string args))

(defun log-warn (format-string &rest args)
  (apply #'log-msg :warn format-string args))

(defun log-error (format-string &rest args)
  (apply #'log-msg :error format-string args))
