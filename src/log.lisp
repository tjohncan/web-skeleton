(in-package :web-skeleton)

;;; ===========================================================================
;;; Logging
;;;
;;; Four levels: DEBUG, INFO, WARN, ERROR.
;;; Configurable minimum level via *log-level*.
;;; Timestamps on every line.
;;; ===========================================================================

(defvar *log-lock* (sb-thread:make-mutex :name "log")
  "Serializes log output so lines from concurrent threads don't interleave.")

(defparameter *log-levels* '(:debug :info :warn :error)
  "Ordered from least to most severe.")

(defparameter *log-level* :info
  "Minimum level to output. Set to :debug to see everything.")

(defparameter *log-stream* *standard-output*
  "Stream to write log output to.")

(defun log-level-value (level)
  "Return the numeric severity of LEVEL."
  (or (position level *log-levels*) 0))

(defun timestamp ()
  "Return current UTC time as YYYY-MM-DDThh:mm:ssZ."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun log-msg (level format-string &rest args)
  "Log a message at LEVEL. Suppressed if below *log-level*."
  (when (>= (log-level-value level) (log-level-value *log-level*))
    (sb-thread:with-mutex (*log-lock*)
      (format *log-stream* "~a [~a] ~?~%"
              (timestamp)
              (string-upcase (symbol-name level))
              format-string args)
      (force-output *log-stream*))))

(defun log-debug (format-string &rest args)
  (apply #'log-msg :debug format-string args))

(defun log-info (format-string &rest args)
  (apply #'log-msg :info format-string args))

(defun log-warn (format-string &rest args)
  (apply #'log-msg :warn format-string args))

(defun log-error (format-string &rest args)
  (apply #'log-msg :error format-string args))
