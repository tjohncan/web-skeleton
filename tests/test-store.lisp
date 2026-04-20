(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Concurrent store tests
;;; ===========================================================================

(defun test-store-basics ()
  (format t "~%Store basics~%")
  (let ((s (make-store)))
    ;; Empty state
    (check "empty: count" (store-count s) 0)
    (check "empty: get missing returns nil/nil"
           (multiple-value-list (store-get s "missing"))
           '(nil nil))
    (check "empty: delete missing returns nil"
           (store-delete s "missing") nil)
    ;; Set + get round-trip
    (store-set s "alpha" 1)
    (store-set s "beta" 2)
    (check "after set: count" (store-count s) 2)
    (check "after set: get alpha returns value + present"
           (multiple-value-list (store-get s "alpha"))
           '(1 t))
    (check "after set: get beta returns value + present"
           (multiple-value-list (store-get s "beta"))
           '(2 t))
    ;; Overwrite
    (store-set s "alpha" 10)
    (check "overwrite: get returns new value"
           (store-get s "alpha") 10)
    ;; Explicit NIL value is distinguishable from missing via PRESENT-P
    (store-set s "nullable" nil)
    (check "nil value stored: value is nil"
           (store-get s "nullable") nil)
    (check "nil value stored: present-p is t"
           (nth-value 1 (store-get s "nullable")) t)
    ;; Delete
    (check "delete present: returns t"
           (store-delete s "alpha") t)
    (check "delete already-gone: returns nil"
           (store-delete s "alpha") nil)
    (check "after delete: count"
           (store-count s) 2)))

(defun test-store-update ()
  (format t "~%Store update~%")
  (let ((s (make-store)))
    ;; Update on missing key: fn receives NIL, creates entry
    (check "update miss returns new value"
           (store-update s "count" (lambda (old) (1+ (or old 0))))
           1)
    (check "update miss created entry"
           (store-get s "count") 1)
    ;; Repeated updates compose
    (store-update s "count" (lambda (old) (1+ old)))
    (store-update s "count" (lambda (old) (1+ old)))
    (check "update hit increments cleanly"
           (store-get s "count") 3)
    ;; Plist sugar: merge into fresh key
    (store-update-plist s "new-session" :foo 1 :bar 2)
    (let ((sess (store-get s "new-session")))
      (check "plist-merge created entry with :foo"
             (getf sess :foo) 1)
      (check "plist-merge created entry with :bar"
             (getf sess :bar) 2))
    ;; Plist sugar: merge into existing key preserves old values
    (store-set s "session" '(:created-at 100))
    (store-update-plist s "session" :access-token "tok" :expires-at 200)
    (let ((sess (store-get s "session")))
      (check "plist-merge preserves existing :created-at"
             (getf sess :created-at) 100)
      (check "plist-merge adds :access-token"
             (getf sess :access-token) "tok")
      (check "plist-merge adds :expires-at"
             (getf sess :expires-at) 200))
    ;; Plist sugar: overwrite a key already present
    (store-update-plist s "session" :access-token "tok2")
    (check "plist-merge overwrites existing value"
           (getf (store-get s "session") :access-token) "tok2")))

(defun test-store-map ()
  (format t "~%Store map~%")
  (let ((s (make-store)))
    (store-set s "a" 1)
    (store-set s "b" 2)
    (store-set s "c" 3)
    (let ((pairs '()))
      (store-map s (lambda (k v) (push (cons k v) pairs)))
      (check "store-map visits every entry"
             (length pairs) 3)
      (check "store-map delivers a=1"
             (cdr (assoc "a" pairs :test #'equal)) 1)
      (check "store-map delivers b=2"
             (cdr (assoc "b" pairs :test #'equal)) 2)
      (check "store-map delivers c=3"
             (cdr (assoc "c" pairs :test #'equal)) 3))))

(defun test-store-validation ()
  (format t "~%Store validation~%")
  (flet ((signals-error-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (error () t))))
    (check "expiry-fn without reap-interval is an error"
           (signals-error-p
            (lambda ()
              (make-store :expiry-fn (lambda (k v)
                                       (declare (ignore k v)) t))))
           t)
    (check "reap-interval without expiry-fn is an error"
           (signals-error-p
            (lambda () (make-store :reap-interval 1)))
           t)
    (check "neither is fine (no reaper)"
           (storep (make-store)) t)))

(defun test-store-reaper ()
  (format t "~%Store reaper~%")
  ;; Rebind *shutdown-hooks* locally so this test's reaper registration
  ;; does not pollute framework state, and we can terminate the reaper
  ;; via run-shutdown-hooks at the end.
  (let ((web-skeleton::*shutdown-hooks* nil))
    (let ((s (make-store
              :expiry-fn (lambda (k v)
                           (declare (ignore k))
                           (eq v :doomed))
              :reap-interval 0.2)))
      (store-set s "keep-a"   :alive)
      (store-set s "keep-b"   :alive)
      (store-set s "doomed-a" :doomed)
      (store-set s "doomed-b" :doomed)
      (check "before reap: count is 4"
             (store-count s) 4)
      ;; Wait for the reaper to sweep at least once. Interval is
      ;; 0.2s; sleeping 0.5s gives ~2.5x margin against thread-start
      ;; latency, scheduler jitter, and the reaper's 0.1s slice.
      (sleep 0.5)
      (check "after reap: count is 2 (doomed entries gone)"
             (store-count s) 2)
      (check "after reap: keep-a still alive"
             (store-get s "keep-a") :alive)
      (check "after reap: doomed-a is gone"
             (multiple-value-list (store-get s "doomed-a"))
             '(nil nil))
      ;; Stop the reaper by running the local shutdown hooks.
      (let ((web-skeleton:*log-level* :error)
            (web-skeleton:*log-stream* (make-broadcast-stream)))
        (web-skeleton::run-shutdown-hooks)))))

(defun test-store-thread-safety ()
  (format t "~%Store thread safety~%")
  ;; N threads each increment the same counter M times via store-update.
  ;; If the mutex works, the final count is exactly N*M. Without it, lost
  ;; updates produce a count below N*M.
  (let* ((s (make-store))
         (n-threads 10)
         (increments-per-thread 200)
         (threads '()))
    (store-set s "counter" 0)
    (dotimes (i n-threads)
      (push (sb-thread:make-thread
             (lambda ()
               (loop repeat increments-per-thread
                     do (store-update s "counter"
                                      (lambda (old) (1+ old)))))
             :name (format nil "bump-~d" i))
            threads))
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    (check "concurrent store-update serializes cleanly"
           (store-get s "counter")
           (* n-threads increments-per-thread))))

;;; ---------------------------------------------------------------------------
;;; Runner
;;; ---------------------------------------------------------------------------

(defun test-store ()
  (setf *tests-passed* 0
        *tests-failed* 0
        *failed-names* nil)
  (format t "~%=== Store Tests ===~%")
  (test-store-basics)
  (test-store-update)
  (test-store-map)
  (test-store-validation)
  (test-store-reaper)
  (test-store-thread-safety)
  (report-suite "Store")
  (zerop *tests-failed*))
