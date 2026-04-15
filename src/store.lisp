(in-package :web-skeleton)

;;; ===========================================================================
;;; Concurrent keyed store with optional expiry reaper
;;;
;;; Thread-safe hash-table-backed store for app-level state: sessions,
;;; caches, rate-limit counters, dedup tables. All operations hold an
;;; internal mutex, so stores are safe to share across worker threads in
;;; the same process.
;;;
;;; When :EXPIRY-FN and :REAP-INTERVAL are supplied to MAKE-STORE, a
;;; background thread sweeps the store every REAP-INTERVAL seconds and
;;; removes entries where (FUNCALL EXPIRY-FN K V) returns truthy. The
;;; reaper's stop function is registered in *SHUTDOWN-HOOKS* so SIGTERM
;;; or graceful shutdown tears it down without app-side bookkeeping.
;;;
;;; This primitive commits to mechanics (locking, iteration, reaping),
;;; not to policy. It knows nothing about sessions, expiry semantics,
;;; or what values look like — that is the app's job, expressed through
;;; the :EXPIRY-FN predicate.
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Struct
;;; ---------------------------------------------------------------------------

(defstruct (store (:constructor %make-store)
                  (:copier nil)
                  (:predicate storep))
  (table          nil :type hash-table)
  (lock           nil)
  (expiry-fn      nil :type (or null function))
  ;; REAP-INTERVAL accepts any positive real — integer in production
  ;; (seconds between sweeps), float permitted so the test suite can
  ;; use sub-second intervals without a real 2-second sleep.
  (reap-interval  nil :type (or null (real (0))))
  (reaper-thread  nil)
  (stop-requested nil))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(defun make-store (&key (test 'equal) expiry-fn reap-interval)
  "Create a thread-safe keyed store.
   TEST is the hash-table equality predicate (default EQUAL for string
   keys; EQL or EQUALP also valid).
   EXPIRY-FN and REAP-INTERVAL together enable the optional background
   reaper: the reaper wakes every REAP-INTERVAL seconds and removes
   entries where (FUNCALL EXPIRY-FN K V) returns truthy. Supplying one
   without the other is an error — ambiguous intent. The app-supplied
   predicate should be cheap: it runs under the store's mutex during
   the sweep and blocks concurrent access for the duration of the walk."
  (when (or (and expiry-fn (null reap-interval))
            (and reap-interval (null expiry-fn)))
    (error "make-store: :expiry-fn and :reap-interval must be supplied together"))
  (let ((store (%make-store
                :table (make-hash-table :test test)
                :lock  (sb-thread:make-mutex :name "store")
                :expiry-fn expiry-fn
                :reap-interval reap-interval)))
    (when expiry-fn
      (start-store-reaper store))
    store))

(defun store-get (store key)
  "Return (values VALUE PRESENT-P) for KEY in STORE.
   PRESENT-P distinguishes an explicit NIL value from a missing key.
   Callers that never store NIL can ignore the second value."
  (sb-thread:with-mutex ((store-lock store))
    (gethash key (store-table store))))

(defun store-set (store key value)
  "Set KEY to VALUE in STORE. Returns VALUE."
  (sb-thread:with-mutex ((store-lock store))
    (setf (gethash key (store-table store)) value)))

(defun store-update (store key fn)
  "Atomically update the value for KEY by calling FN on the current value.
   FN receives the current value (NIL if the key is absent) and returns
   the new value. The whole read-modify-write runs under the store's
   mutex, so concurrent STORE-UPDATE calls for the same key serialize
   cleanly — compare-and-swap for free. Returns the new value."
  (sb-thread:with-mutex ((store-lock store))
    (let ((new (funcall fn (gethash key (store-table store)))))
      (setf (gethash key (store-table store)) new)
      new)))

(defun store-update-plist (store key &rest kv)
  "Plist-merge convenience over STORE-UPDATE. Each key in KV is set in
   the plist stored at KEY (overwriting if present, appending if not).
   Creates a fresh plist from KV if the key is absent. Equivalent to
   STORE-UPDATE with a plist-merging lambda, without the call-site
   boilerplate.

   Example:
     (store-update-plist *sessions* sid
                         :access-token tok
                         :expires-at exp)"
  (store-update store key
                (lambda (old)
                  (let ((result old))
                    (loop for (k v) on kv by #'cddr
                          do (setf (getf result k) v))
                    result))))

(defun store-delete (store key)
  "Remove KEY from STORE. Returns T if it was present, NIL if not."
  (sb-thread:with-mutex ((store-lock store))
    (remhash key (store-table store))))

(defun store-count (store)
  "Return the number of entries in STORE."
  (sb-thread:with-mutex ((store-lock store))
    (hash-table-count (store-table store))))

(defun store-map (store fn)
  "Call (FN KEY VALUE) for each entry in STORE under the store's mutex.
   FN must not call any STORE-* operation on STORE — that deadlocks on
   the mutex — and must not mutate the table directly (REMHASH or SETF
   GETHASH) during iteration. Intended for read-only inspection: dump
   on shutdown, collect keys for later deletion, snapshot state.
   Downstream apps rarely need this; the optional reaper subsumes the
   common walk-and-delete pattern. Returns NIL."
  (sb-thread:with-mutex ((store-lock store))
    (maphash fn (store-table store)))
  nil)

;;; ---------------------------------------------------------------------------
;;; Reaper thread
;;; ---------------------------------------------------------------------------

(defun reap-store (store)
  "Walk STORE and remove entries where the expiry predicate returns truthy.
   Returns the count of entries removed. Collects doomed keys first, then
   deletes — mutating the hash table during MAPHASH is undefined. The whole
   collect-and-delete runs under the store's mutex so concurrent SETs
   between collect and delete cannot cause spurious removal of a value
   a caller just replaced."
  (let ((expiry-fn (store-expiry-fn store))
        (doomed '()))
    (sb-thread:with-mutex ((store-lock store))
      (maphash (lambda (k v)
                 (when (funcall expiry-fn k v)
                   (push k doomed)))
               (store-table store))
      (dolist (k doomed)
        (remhash k (store-table store))))
    (length doomed)))

(defun start-store-reaper (store)
  "Spawn the reaper thread for STORE and register its stop function in
   *SHUTDOWN-HOOKS*. Called by MAKE-STORE when EXPIRY-FN is supplied;
   not a public entry point."
  (let ((interval (store-reap-interval store)))
    (setf (store-reaper-thread store)
          (sb-thread:make-thread
           (lambda ()
             (loop until (store-stop-requested store) do
               ;; Sleep the configured interval in 100 ms slices so a
               ;; shutdown request is noticed within one slice regardless
               ;; of how long REAP-INTERVAL is. Polling beats
               ;; TERMINATE-THREAD — interrupting MAPHASH mid-sweep is
               ;; async-unsafe, and the 100 ms idle cost is nothing next
               ;; to a reap-interval typically measured in minutes.
               (let ((deadline (+ (get-internal-real-time)
                                  (* interval
                                     internal-time-units-per-second))))
                 (loop until (or (store-stop-requested store)
                                 (>= (get-internal-real-time) deadline))
                       do (sleep 0.1)))
               (unless (store-stop-requested store)
                 (handler-case
                     (let ((n (reap-store store)))
                       (when (> n 0)
                         (log-info "store reaped ~d entr~:@p" n)))
                   (error (e)
                     (log-error "store reaper error: ~a" e))))))
           :name "web-skeleton-store-reaper"))
    (register-cleanup (lambda () (stop-store-reaper store)))))

(defun stop-store-reaper (store)
  "Stop STORE's reaper thread. Safe to call on a store with no reaper,
   and safe to call multiple times. Sets STOP-REQUESTED and waits for
   the reaper to notice on its next slice — within ~100 ms in the
   common case, capped at 5 s if the reaper is stuck inside a sweep
   that the expiry predicate made slow."
  (let ((thread (store-reaper-thread store)))
    (setf (store-stop-requested store) t
          (store-reaper-thread store)  nil)
    (when (and thread (sb-thread:thread-alive-p thread))
      (handler-case
          (sb-thread:join-thread thread :default nil :timeout 5)
        (error () nil)))))
