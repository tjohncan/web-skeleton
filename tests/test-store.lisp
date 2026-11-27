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
      ;; latency and scheduler jitter.
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

(defun test-store-reaper-stops-at-once ()
  "STOP-STORE-REAPER wakes a reaper in the middle of its interval rather
   than waiting it out, and takes the store's shutdown hook back out, so a
   store an application retires is held by nothing of the framework's.

   The interval is a minute: a stop that only flagged the reaper would
   leave it asleep for that minute, and STOP-STORE-REAPER's join would give
   up after its five seconds with the thread still alive."
  (format t "~%Store reaper: stopped at once, and let go~%")
  (let ((web-skeleton::*shutdown-hooks* nil))
    (let* ((s (make-store :expiry-fn (lambda (k v) (declare (ignore k v)) nil)
                          :reap-interval 60))
           (thread (web-skeleton::store-reaper-thread s)))
      (sleep 0.2)
      (check "retire: the store's hook is registered"
             (length web-skeleton::*shutdown-hooks*) 1)
      (let ((start (get-internal-real-time)))
        (stop-store-reaper s)
        (check "retire: the stop returns at once, not at the interval"
               (< (/ (- (get-internal-real-time) start)
                     internal-time-units-per-second)
                  1)
               t))
      (check "retire: the reaper thread has ended"
             (sb-thread:thread-alive-p thread) nil)
      (check "retire: and the hook is gone with it"
             web-skeleton::*shutdown-hooks* nil)
      (check "retire: a second stop is harmless"
             (progn (stop-store-reaper s) :ok) :ok))))

(defun test-store-reaper-survives-a-serious-condition ()
  "An expiry predicate that runs out of stack, or out of the time it gave
   itself, costs one sweep, not the reaper.

   The reaper is a thread of its own with nothing above it, so a condition
   its catch lets past ends it — and SBCL ends the process with it when the
   debugger is disabled. Neither STORAGE-CONDITION nor SB-EXT:TIMEOUT is an
   ERROR, so a catch on ERROR alone is that catch. The predicate runs out
   of stack on its first call and out of time on its second, each ending
   its sweep; the entries a later sweep would reap being gone is the
   evidence that a later sweep happened."
  (format t "~%Store reaper: a predicate out of stack, then out of time~%")
  (let ((web-skeleton::*shutdown-hooks* nil)
        (calls 0))
    (call-with-escapes-ending-the-thread
     (lambda ()
       (let ((s (make-store
                 :expiry-fn (lambda (k v)
                              (declare (ignore k))
                              (case (incf calls)
                                (1 (%exhaust-the-stack 0))
                                (2 (%run-out-of-time)))
                              (eq v :doomed))
                 :reap-interval 0.2)))
         (store-set s "keep" :alive)
         (store-set s "doomed" :doomed)
         ;; The first two sweeps are the ones that fail; a third fits in
         ;; the wait with room for the scheduler.
         (sleep 1.2)
         (check "reaper: the predicate did run out of stack, then time"
                (>= calls 2) t)
         (check "reaper: and a later sweep still reaped"
                (store-count s) 1)
         (let ((web-skeleton:*log-level* :error)
               (web-skeleton:*log-stream* (make-broadcast-stream)))
           (web-skeleton::run-shutdown-hooks)))))))

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
;;; The hash a store files its keys under
;;; ---------------------------------------------------------------------------

(defun test-store-key-hash-is-siphash-of-the-whole-key ()
  "STORE-KEY-HASH is SipHash-2-4, under the process's secret, of bytes
   that say all of a key: a string's every character, an integer's value,
   a list's every element, each kind marked and each string and integer
   with its length before it.

   The bytes are written out here by hand for one key of each kind, and
   SIPHASH-2-4, which the reference vectors check, is asked for their
   hash under a secret bound for the test. A hash that left part of a key
   out, or ran two parts together, would be one a client could choose
   keys against whatever the secret, by varying what was left out."
  (format t "~%Store: a key's hash is SipHash of the whole key~%")
  (let* ((web-skeleton::*store-hash-secret*
           (web-skeleton::make-sip-key #x0706050403020100 #x0F0E0D0C0B0A0908))
         (nil-hash (sxhash nil)))
    (flet ((of-bytes (&rest bytes)
             (ldb (byte 62 0)
                  (web-skeleton::siphash-2-4
                   web-skeleton::*store-hash-secret*
                   (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
           (eight (n)
             (loop for i below 8 collect (ldb (byte 8 (* 8 i)) n))))
      (check "key hash: a string, by its length and its characters"
             (web-skeleton::store-key-hash "abc")
             (of-bytes 1 3 0 0 0 0 0 0 97 98 99))
      (check "key hash: a character past ASCII, as UTF-8 writes it"
             (web-skeleton::store-key-hash
              (coerce (list #\a (code-char #xE9) (code-char #x20AC) (code-char #x1F600))
                      'string))
             (of-bytes 1 4 0 0 0 0 0 0
                       97 #xC3 #xA9 #xE2 #x82 #xAC #xF0 #x9F #x98 #x80))
      (check "key hash: a small integer, by its value"
             (web-skeleton::store-key-hash 5)
             (of-bytes 2 1 0 0 0 0 0 0 5))
      (check "key hash: a negative one, in two's complement"
             (web-skeleton::store-key-hash -1)
             (of-bytes 2 1 0 0 0 0 0 0 255))
      (check "key hash: one that needs a second byte"
             (web-skeleton::store-key-hash 256)
             (of-bytes 2 2 0 0 0 0 0 0 0 1))
      (check "key hash: a list, by every element and its end"
             (web-skeleton::store-key-hash (list "a" 7))
             (apply #'of-bytes
                    (append '(3 1 1 0 0 0 0 0 0 97
                              3 2 1 0 0 0 0 0 0 7
                              4)
                            (eight nil-hash))))
      (check "key hash: anything else, by its SXHASH"
             (web-skeleton::store-key-hash :a-keyword)
             (apply #'of-bytes 4 (eight (sxhash :a-keyword))))
      (check "key hash: a non-negative fixnum, as a hash table wants"
             (typep (web-skeleton::store-key-hash "abc") '(and fixnum (integer 0)))
             t))
    ;; A string of ASCII alone is hashed a word at a time, and any other
    ;; key a byte at a time. The two have to agree wherever both apply, or
    ;; a key filed as one kind of string is not found as another. Lengths
    ;; 0 to 20 put every count of characters in the last word, and cross
    ;; two word boundaries.
    (let ((secret web-skeleton::*store-hash-secret*))
      (check "key hash: ASCII strings of 0 to 20 characters, both ways alike"
             (loop for n from 0 to 20
                   for s = (subseq "abcdefghijklmnopqrstuvwxyz" 0 n)
                   for base = (coerce s 'simple-base-string)
                   unless (= (web-skeleton::walked-key-hash secret s)
                             (web-skeleton::ascii-string-key-hash secret s)
                             (web-skeleton::ascii-base-string-key-hash secret base))
                     collect n)
             nil)
      (check "key hash: the quick way declines a string that is not all ASCII"
             (web-skeleton::ascii-string-key-hash
              secret (coerce (list #\a #\b (code-char #xE9)) 'string))
             nil))
    ;; Another secret, another hash: the one thing a client lacks.
    (check "key hash: under another secret, another hash"
           (let ((one (web-skeleton::store-key-hash "abc"))
                 (other (let ((web-skeleton::*store-hash-secret*
                                (web-skeleton::make-sip-key 1 2)))
                          (web-skeleton::store-key-hash "abc"))))
             (= one other))
           nil)))

(defun test-store-key-hash-agrees-with-equal ()
  "Keys that are EQUAL hash alike, which is what lets the hash stand in a
   table whose test is EQUAL: a string however it is held, an integer
   whatever its size, a list made twice. And keys that only read alike do
   not: a string and the integer it spells, a string and a list of it,
   two lists that split the same characters differently."
  (format t "~%Store: equal keys hash alike~%")
  (flet ((hash (key) (web-skeleton::store-key-hash key)))
    (let ((plain "session-id")
          (base (coerce "session-id" 'simple-base-string))
          (grown (make-array 32 :element-type 'character
                                :fill-pointer 0 :adjustable t)))
      (loop for c across "session-id" do (vector-push-extend c grown))
      (check "equal: a base string and a character string"
             (list (equal plain base) (= (hash plain) (hash base))) '(t t))
      (check "equal: a string with a fill pointer and room to spare"
             (list (equal plain grown) (= (hash plain) (hash grown))) '(t t)))
    (check "equal: a list made twice"
           (= (hash (list "tenant" 42 (list "a" "b")))
              (hash (list "tenant" 42 (list "a" "b"))))
           t)
    (check "equal: a bignum made twice"
           (= (hash (expt 2 100)) (hash (* (expt 2 50) (expt 2 50)))) t)
    (check "unequal: a string and the integer it spells"
           (= (hash "42") (hash 42)) nil)
    (check "unequal: a string and a list of it"
           (= (hash "a") (hash (list "a"))) nil)
    (check "unequal: the same characters split differently"
           (= (hash (list "ab" "c")) (hash (list "a" "bc"))) nil)
    (check "unequal: an integer and its negative"
           (= (hash 255) (hash -255)) nil))
  ;; And in a store: what goes in under one is found under the other.
  (let ((s (make-store)))
    (store-set s (list "tenant" 42) :found)
    (store-set s (coerce "name" 'simple-base-string) :found-too)
    (check "equal: a list key is found by an equal list"
           (store-get s (list "tenant" 42)) :found)
    (check "equal: a base string key is found by a character string"
           (store-get s (copy-seq "name")) :found-too)))

(defun test-store-default-table-hashes-with-the-secret ()
  "A store made with the default TEST files its keys under
   STORE-KEY-HASH, and one made with another TEST does not.

   Counted, by wrapping the function before the stores are made: a table
   takes its hash function when it is created. The EQL store is the
   control, and says the count is of the keyed stores' lookups and not of
   something else calling the function. Each lookup is by a key made for
   it, since a table may answer for the very object it was last asked
   about without hashing it again."
  (format t "~%Store: the default table hashes under the secret~%")
  (let ((calls 0))
    (sb-int:encapsulate 'web-skeleton::store-key-hash 'counted
                        (lambda (f key)
                          (incf calls)
                          (funcall f key)))
    (unwind-protect
         (let ((plain (make-store :test 'eql))
               (by-name (make-store))
               (by-function (make-store :test #'equal)))
           (store-set plain 1 :one)
           (store-get plain 1)
           (check "hashing: an EQL store never asks for a keyed hash" calls 0)
           (store-set by-name (copy-seq "k") :v)
           (check "hashing: the default store asks for one to file a key"
                  (plusp calls) t)
           (let ((so-far calls))
             (check "hashing: it finds the key by an equal one"
                    (store-get by-name (copy-seq "k")) :v)
             (check "hashing: having asked again to look" (> calls so-far) t))
           (let ((so-far calls))
             (store-set by-function (copy-seq "k") :v)
             (check "hashing: a store given #'EQUAL asks too" (> calls so-far) t))
           (check "hashing: and finds what it was given"
                  (store-get by-function (copy-seq "k")) :v))
      (sb-int:unencapsulate 'web-skeleton::store-key-hash 'counted))))

(defun test-store-keys-known-by-identity-are-filed-by-the-object ()
  "A store made with the default TEST files a key EQUAL knows only by
   identity, a vector that is not a string or a structure or a symbol, in
   a table SBCL hashes by the object, and not under STORE-KEY-HASH.

   STORE-KEY-HASH has only SXHASH to tell two such keys apart by, and
   SXHASH answers alike for any two vectors that are not strings: filed
   under it, the two thousand byte vectors here would sit in one place,
   each walked past at every lookup. Where they are filed is what is
   checked, by the count of each table, with a string key as the control
   that the other table is still the one for what a client chooses.

   Every operation then finds such a key where it is filed: by the object
   and not by another that holds the same, as EQUAL has it."
  (format t "~%Store: a key known by identity is filed by the object~%")
  (flet ((octets (i)
           (let ((v (make-array 4 :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
             (setf (aref v 0) (mod i 256)
                   (aref v 1) (floor i 256))
             v)))
    (let* ((s (make-store :expiry-fn (lambda (k v)
                                       (declare (ignore k))
                                       (eq v :doomed))
                          :reap-interval 3600))
           (vectors (loop for i below 2000 collect (octets i)))
           (object (make-json-object nil))
           (one (first vectors)))
      (unwind-protect
           (progn
             (dolist (v vectors) (store-set s v :a-vector))
             (store-set s "a string" :a-string)
             (store-set s object :an-object)
             (store-set s :a-symbol :a-symbol)
             (check "identity: vectors, a structure and a symbol are in the table SBCL hashes"
                    (list (hash-table-count (web-skeleton::store-table s))
                          (hash-table-count (web-skeleton::store-others s)))
                    '(1 2002))
             (check "identity: the count is of both tables" (store-count s) 2003)
             (check "identity: each is found by the object"
                    (list (store-get s one) (store-get s object)
                          (store-get s :a-symbol) (store-get s "a string"))
                    '(:a-vector :an-object :a-symbol :a-string))
             (check "identity: and not by another vector that holds the same"
                    (multiple-value-list (store-get s (copy-seq one)))
                    '(nil nil))
             (check "identity: STORE-UPDATE reads and writes it where it is filed"
                    (list (store-update s one (lambda (old) (list old :updated)))
                          (store-get s one)
                          (store-count s))
                    '((:a-vector :updated) (:a-vector :updated) 2003))
             (check "identity: STORE-MAP visits both tables"
                    (let ((seen 0))
                      (store-map s (lambda (k v) (declare (ignore k v)) (incf seen)))
                      seen)
                    2003)
             (check "identity: STORE-DELETE removes it, once"
                    (list (store-delete s one) (store-delete s one)
                          (store-count s))
                    '(t nil 2002))
             (store-set s (second vectors) :doomed)
             (store-set s "a string" :doomed)
             (check "identity: the reaper's sweep removes from both tables"
                    (list (web-skeleton::reap-store s) (store-count s)
                          (store-get s (second vectors)) (store-get s "a string"))
                    '(2 2000 nil nil)))
        (stop-store-reaper s))))
  (let ((plain (make-store :test 'eql))
        (key (vector 1 2)))
    (store-set plain key :found)
    (check "identity: a store with another TEST has the one table, and files there"
           (list (web-skeleton::store-others plain) (store-get plain key)
                 (store-count plain))
           '(nil :found 1))))

(defun test-store-secret-is-not-saved-with-an-image ()
  "The stores' secret is not carried into a saved image, and a store
   saved with entries in it still finds them in the process started from
   the image.

   FORGET-STORE-HASH-SECRET is on SBCL's save hooks, which
   SAVE-LISP-AND-DIE runs before it writes. Called here as the save would
   call it: the secret is gone and its words are zero. The store then
   has 200 entries filed under a secret nobody holds, which is how a
   started process finds it. The first use draws the process a secret and
   files them again, so every one is found, removed and counted as
   before. Left where they were, they would be looked for in the wrong
   places, and nearly all of them missed."
  (format t "~%Store: the secret is not saved, and saved entries are found~%")
  (check "secret: forgetting it is on the save hooks"
         (and (member 'web-skeleton::forget-store-hash-secret sb-ext:*save-hooks*) t)
         t)
  (let ((s (make-store))
        (keys (loop for i below 200 collect (format nil "key-~d" i))))
    (dolist (k keys) (store-set s k (length k)))
    (let ((before web-skeleton::*store-hash-secret*))
      (web-skeleton::forget-store-hash-secret)
      (check "secret: a save leaves the image with none"
             web-skeleton::*store-hash-secret* nil)
      (check "secret: and the one it had is zeroed"
             (list (web-skeleton::sip-key-k0 before) (web-skeleton::sip-key-k1 before))
             '(0 0))
      (check "refiled: of 200 entries, none is missed under the secret drawn next"
             (loop for k in keys
                   count (not (eql (store-get s k) (length k))))
             0)
      (check "refiled: that secret is another one"
             (let ((after web-skeleton::*store-hash-secret*))
               (and after (not (eq after before))
                    (not (and (zerop (web-skeleton::sip-key-k0 after))
                              (zerop (web-skeleton::sip-key-k1 after))))))
             t)
      (check "refiled: the count is as it was" (store-count s) 200)
      (check "refiled: and an entry can be removed, once"
             (list (store-delete s "key-7") (store-delete s "key-7")
                   (store-count s))
             '(t nil 199)))))

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
  (test-store-reaper-stops-at-once)
  (test-store-reaper-survives-a-serious-condition)
  (test-store-thread-safety)
  (test-store-key-hash-is-siphash-of-the-whole-key)
  (test-store-key-hash-agrees-with-equal)
  (test-store-default-table-hashes-with-the-secret)
  (test-store-keys-known-by-identity-are-filed-by-the-object)
  (test-store-secret-is-not-saved-with-an-image)
  (report-suite "Store")
  (zerop *tests-failed*))
