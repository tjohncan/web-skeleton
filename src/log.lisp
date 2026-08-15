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
   parses and dispatches cleanly logs nothing at all. At :DEBUG it is
   several acquisitions per request, with every worker serialized behind
   them.

   Two consequences worth knowing before building on it. An access log
   would put this on the hot path by construction — one line per request,
   every request, every worker, through one mutex with a FORCE-OUTPUT
   inside it. And *LOG-STREAM* pointed at a pipe to a log shipper that
   stalls does not slow one connection; it stops the server, because the
   stalled write is holding the lock the other workers need.

   The shape of a fix is per-worker buffers drained on the existing
   maintenance tick. Not done: nothing in the framework logs often enough
   today to pay for it. Recorded so that whoever adds something that does
   meets this first rather than afterwards.")

(defparameter *log-levels* '(:debug :info :warn :error)
  "Ordered from least to most severe.")

(defparameter *log-level* :info
  "Minimum level to output. Set to :debug to see everything.")

(defparameter *log-stream* nil
  "Stream to write log output to. NIL uses *standard-output*.")

(defun log-level-value (level)
  "Return the numeric severity of LEVEL."
  (or (position level *log-levels*) 0))

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
      (let ((stream (or *log-stream* *standard-output*)))
        (handler-case
            (progn
              (format stream "~a [~a] ~?~%"
                      (timestamp)
                      (string-upcase (symbol-name level))
                      format-string args)
              (force-output stream))
          (error ()
            (ignore-errors
             (format *error-output*
                     "~a [~a] (log stream failed) ~?~%"
                     (timestamp)
                     (string-upcase (symbol-name level))
                     format-string args)
             (force-output *error-output*))))))))

(defun log-debug (format-string &rest args)
  (apply #'log-msg :debug format-string args))

(defun log-info (format-string &rest args)
  (apply #'log-msg :info format-string args))

(defun log-warn (format-string &rest args)
  (apply #'log-msg :warn format-string args))

(defun log-error (format-string &rest args)
  (apply #'log-msg :error format-string args))
