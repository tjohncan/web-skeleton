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
  ;; KEYED is true of a store whose table hashes with STORE-KEY-HASH, and
  ;; FILED-UNDER is then the secret its entries are filed under: see
  ;; STORE-TABLE-IN-USE. OTHERS is such a store's second table, for the
  ;; keys EQUAL knows only by identity: see STORE-TABLE-FOR.
  (keyed          nil)
  (filed-under    nil)
  (others         nil :type (or null hash-table))
  (expiry-fn      nil :type (or null function))
  ;; REAP-INTERVAL accepts any positive real — integer in production
  ;; (seconds between sweeps), float permitted so the test suite can
  ;; use sub-second intervals without a real 2-second sleep.
  (reap-interval  nil :type (or null (real (0))))
  (reaper-thread  nil)
  (stop-requested nil)
  ;; Signalled by STOP-STORE-REAPER; the reaper sleeps on it between
  ;; sweeps, so a stop wakes it at once.
  (wake (sb-thread:make-semaphore :name "store-reaper"))
  ;; The shutdown hook that stops the reaper, taken back out of
  ;; *SHUTDOWN-HOOKS* when the reaper is stopped another way, so a store
  ;; an application retires is not held there until the run ends.
  (cleanup nil))

;;; ---------------------------------------------------------------------------
;;; The hash a store files its keys under
;;;
;;; A store is where an application keeps what it knows about a client,
;;; and the key is often the client's own: a username, an API key, an
;;; address. SBCL's hash of a key can be worked out by anyone, so such
;;; keys can be chosen to land in one bucket of the table, and each one
;;; added then costs a walk over all of them, under the store's one
;;; mutex, until they expire. STORE-KEY-HASH is SipHash-2-4 under a
;;; secret instead, and where a key lands cannot be worked out without
;;; it.
;;;
;;; The secret is one for the process, and it is drawn when a store first
;;; needs it, not when this file is loaded: a binary built with
;;; SAVE-LISP-AND-DIE would otherwise carry it, the same in every process
;;; started from that binary and readable out of the file. For the same
;;; reason a save forgets it (FORGET-STORE-HASH-SECRET, on SBCL's save
;;; hooks), and a store saved with entries in it files them again under
;;; the secret the started process draws (STORE-TABLE-IN-USE).
;;; ---------------------------------------------------------------------------

(defvar *store-hash-secret* nil
  "The SIP-KEY this process's stores hash their keys under, or NIL until
   one is needed. Read through STORE-HASH-SECRET, which draws it.")

(defun store-hash-secret ()
  "The secret this process's stores hash under, drawn from the system's
   random source the first time it is asked for. Two threads asking at
   once agree on one: the second to finish takes the first's."
  (or *store-hash-secret*
      (let ((new (sip-key-from-octets (random-bytes 16))))
        (or (sb-ext:compare-and-swap (symbol-value '*store-hash-secret*)
                                     nil new)
            new))))

(defun forget-store-hash-secret ()
  "Leave this image with no store secret, so one saved from it carries
   none and a process started from it draws its own. On
   SB-EXT:*SAVE-HOOKS*, which SAVE-LISP-AND-DIE runs before it writes.

   The words of the old secret are zeroed as well as let go of, since a
   store holding entries still points at it, to know them filed under a
   secret that is gone."
  (let ((secret *store-hash-secret*))
    (when secret
      (setf (sip-key-k0 secret) 0
            (sip-key-k1 secret) 0
            *store-hash-secret* nil))))

(pushnew 'forget-store-hash-secret sb-ext:*save-hooks*)

;;; What is hashed is a run of bytes that says all of a key, and says it
;;; one way. Each kind of key starts with a mark of its own. A string or
;;; an integer follows the mark with its length in seven bytes, and then
;;; with what it is: a string's characters as UTF-8 would write them, an
;;; integer's value in two's complement. A cons is its mark, its CAR and
;;; its CDR. Where one part of a key ends is therefore never in doubt:
;;; ("ab" "c") and ("a" "bc") are different runs, and so are "42" and 42.

(defun walked-key-hash (secret key)
  "SipHash-2-4 under SECRET of the bytes that say KEY, for any key, as
   the non-negative fixnum a hash table wants. KEY is walked and its
   bytes absorbed one at a time; a string of ASCII alone has a quicker
   way to the same hash, ASCII-STRING-KEY-HASH."
  (declare (type sip-key secret))
  (with-siphash-2-4 (absorb finish secret)
    (labels ((absorb-length (n)
               ;; The seven bytes after a mark, which with it make a word.
               (dotimes (i 7)
                 (absorb (ldb (byte 8 (* 8 i)) n))))
             (absorb-character (code)
               ;; As UTF-8 would write it, one byte for ASCII and up to
               ;; four otherwise. A surrogate goes in as any other
               ;; character of its range: this is a hash's input and not
               ;; text to be read back.
               (declare (type (integer 0 #x10FFFF) code))
               (cond ((< code #x80)
                      (absorb code))
                     ((< code #x800)
                      (absorb (logior #xC0 (ash code -6)))
                      (absorb (logior #x80 (logand code #x3F))))
                     ((< code #x10000)
                      (absorb (logior #xE0 (ash code -12)))
                      (absorb (logior #x80 (logand (ash code -6) #x3F)))
                      (absorb (logior #x80 (logand code #x3F))))
                     (t
                      (absorb (logior #xF0 (ash code -18)))
                      (absorb (logior #x80 (logand (ash code -12) #x3F)))
                      (absorb (logior #x80 (logand (ash code -6) #x3F)))
                      (absorb (logior #x80 (logand code #x3F))))))
             (absorb-key (x)
               (loop
                 (typecase x
                   (string
                    (absorb 1)
                    (absorb-length (length x))
                    (loop for c across x
                          do (absorb-character (char-code c)))
                    (return))
                   (integer
                    (let ((bytes (ceiling (1+ (integer-length x)) 8)))
                      (absorb 2)
                      (absorb-length bytes)
                      (dotimes (i bytes)
                        (absorb (ldb (byte 8 (* 8 i)) x))))
                    (return))
                   ;; The CAR by a call and the CDR by going round again,
                   ;; so a long list is a loop and not a deep recursion.
                   (cons
                    (absorb 3)
                    (absorb-key (car x))
                    (setf x (cdr x)))
                   (t
                    (absorb 4)
                    (let ((hash (sxhash x)))
                      (dotimes (i 8)
                        (absorb (ldb (byte 8 (* 8 i)) hash))))
                    (return))))))
      (absorb-key key)
      (ldb (byte 62 0) (finish)))))

(macrolet ((define-ascii-string-key-hash (name type)
             `(defun ,name (secret string)
                "WALKED-KEY-HASH's answer for STRING, a simple string, if
   every character of it is ASCII, and NIL if one is not.

   Such a string is most of what stores are keyed by, a token or a name
   or an address, and its bytes are its characters. So they are taken
   eight at a time straight out of the string into the words SipHash
   works on, after the word that is a string's mark and length, with the
   state in variables of this function's own: several times quicker than
   absorbing them one by one."
                (declare (type sip-key secret)
                         (type ,type string))
                (let* ((length (length string))
                       (whole (- length (mod length 8))))
                  (flet ((word-at (start count)
                           ;; COUNT characters from START, the first the
                           ;; lowest byte.
                           (declare (type (integer 0 8) count))
                           (let ((word 0))
                             (declare (type (unsigned-byte 64) word))
                             (dotimes (i count word)
                               (let ((code (char-code (schar string (+ start i)))))
                                 (when (>= code #x80)
                                   (return-from ,name nil))
                                 (setf word (logior word
                                                    (ash (logand code #x7F)
                                                         (* 8 i)))))))))
                    (declare (inline word-at))
                    (multiple-value-bind (v0 v1 v2 v3) (sip-initial-state secret)
                      (multiple-value-setq (v0 v1 v2 v3)
                        (sip-compress v0 v1 v2 v3
                                      (logior 1 (ash (ldb (byte 56 0) length) 8))))
                      (loop for start from 0 below whole by 8
                            do (multiple-value-setq (v0 v1 v2 v3)
                                 (sip-compress v0 v1 v2 v3 (word-at start 8))))
                      ;; The last word: the characters left over, and in
                      ;; its top byte the count of bytes hashed, the first
                      ;; word's eight and one for each character.
                      (multiple-value-setq (v0 v1 v2 v3)
                        (sip-compress v0 v1 v2 v3
                                      (logior (word-at whole (- length whole))
                                              (ash (logand (+ 8 length) #xFF) 56))))
                      (ldb (byte 62 0) (sip-finalize v0 v1 v2 v3))))))))
  (define-ascii-string-key-hash ascii-string-key-hash
    (simple-array character (*)))
  (define-ascii-string-key-hash ascii-base-string-key-hash
    simple-base-string))

(defun store-key-hash (key)
  "The hash a store's table files KEY under: SipHash-2-4 of KEY under
   this process's secret, as the non-negative fixnum a hash table wants.

   Equal keys hash alike, as EQUAL has them equal. A string is hashed by
   its characters, every one, whatever kind of string holds them; an
   integer by its value; a cons by its CAR and its CDR, so a list by all
   of its elements. Those are what a key made from a client's input is
   built of, and each is hashed whole, with its length, so no two such
   keys hash alike by construction.

   Anything else, a character, a float, a pathname, or a symbol inside a
   list, goes in as its SXHASH, which is equal for EQUAL objects by
   definition. The secret still decides where it lands, but two that
   SXHASH cannot tell apart land together. That is all of some kinds of
   object, every vector that is not a string for one, which is why a key
   EQUAL knows only by identity is not filed under this hash at all: see
   STORE-TABLE-FOR. Inside a list such a vector is hashed here all the
   same, so list keys that differ only in one land together, as in SBCL's
   own EQUAL table. EQUAL compares that vector by identity, so such a key
   is found only by a list holding the same vector, never by one holding
   another with the same bytes; DEPLOYMENT.md's \"Concurrent store\" says
   to make it a string."
  (let ((secret (store-hash-secret)))
    (or (typecase key
          ((simple-array character (*))
           (ascii-string-key-hash secret key))
          (simple-base-string
           (ascii-base-string-key-hash secret key)))
        (walked-key-hash secret key))))

(defun store-table-in-use (store)
  "STORE's table, ready to have a key looked up in it. Called with the
   store's mutex held, before a key is hashed in it: by STORE-TABLE-FOR
   for an operation on one key, and by the reaper's sweep.

   A keyed table's entries are filed under the secret that was current
   when they went in, and FILED-UNDER remembers which. It differs from
   the process's secret in two cases. In a store not yet used, where it
   is NIL and there is nothing to refile. And in a store that was saved
   into an image with entries in it and is being used in a process
   started from that image: the secret those were filed under was
   forgotten at the save, so they are taken out and put back under this
   process's, once, here."
  (let ((table (store-table store)))
    (when (store-keyed store)
      (let ((secret (store-hash-secret)))
        (unless (eq (store-filed-under store) secret)
          (when (plusp (hash-table-count table))
            (let ((entries '()))
              (maphash (lambda (k v) (push (cons k v) entries)) table)
              (clrhash table)
              (loop for (k . v) in entries
                    do (setf (gethash k table) v))))
          (setf (store-filed-under store) secret))))
    table))

(defun store-table-for (store key)
  "The table of STORE that KEY is filed in, ready to have it looked up.
   Called with the store's mutex held, by every operation on one key.

   A keyed store has two. EQUAL compares a string, a cons, a number, a
   character, a bit vector and a pathname by what they hold. Those are
   what a client's input is made into, and they are filed under
   STORE-KEY-HASH. Every other key EQUAL compares by identity: a symbol,
   a structure, a vector that is not a string. Such a key is the one
   object or it is not, which no client chooses, and it is filed in
   OTHERS, a table SBCL hashes itself, by the object.

   STORE-KEY-HASH would have only SXHASH to tell two such keys apart by,
   and SXHASH answers alike for any two vectors that are not strings.
   Filed under it, a store keyed by byte vectors would keep every one in
   the same place and walk them all at each lookup."
  (if (and (store-keyed store)
           (not (typep key '(or string cons number character
                                bit-vector pathname))))
      (store-others store)
      (store-table-in-use store)))

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
   the sweep and blocks concurrent access for the duration of the walk.

   With the default TEST, keys are hashed by STORE-KEY-HASH, under a
   secret, so a store may be keyed by what a client chooses: a client
   cannot choose keys that land together. A key EQUAL knows only by
   identity, a symbol or a structure or a vector that is not a string, is
   filed by the object, as SBCL files it. A store made with another TEST
   hashes as SBCL does throughout, which anyone can work out; key such a
   store by something the server chooses."
  (when (or (and expiry-fn (null reap-interval))
            (and reap-interval (null expiry-fn)))
    (error "make-store: :expiry-fn and :reap-interval must be supplied together"))
  (let* ((keyed (and (member test (list 'equal #'equal)) t))
         (store (%make-store
                 :table (if keyed
                            (make-hash-table :test 'equal
                                             :hash-function 'store-key-hash)
                            (make-hash-table :test test))
                 :keyed keyed
                 :others (and keyed (make-hash-table :test 'equal))
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
    (gethash key (store-table-for store key))))

(defun store-set (store key value)
  "Set KEY to VALUE in STORE. Returns VALUE."
  (sb-thread:with-mutex ((store-lock store))
    (setf (gethash key (store-table-for store key)) value)))

(defun store-update (store key fn)
  "Atomically update the value for KEY by calling FN on the current value.
   FN receives the current value (NIL if the key is absent) and returns
   the new value. The whole read-modify-write runs under the store's
   mutex, so concurrent STORE-UPDATE calls for the same key serialize
   cleanly — compare-and-swap for free. Returns the new value."
  (sb-thread:with-mutex ((store-lock store))
    (let* ((table (store-table-for store key))
           (new (funcall fn (gethash key table))))
      (setf (gethash key table) new)
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
                  (let ((result (copy-list old)))
                    (loop for (k v) on kv by #'cddr
                          do (setf (getf result k) v))
                    result))))

(defun store-delete (store key)
  "Remove KEY from STORE. Returns T if it was present, NIL if not."
  (sb-thread:with-mutex ((store-lock store))
    (remhash key (store-table-for store key))))

(defun store-count (store)
  "Return the number of entries in STORE."
  (sb-thread:with-mutex ((store-lock store))
    (let ((others (store-others store)))
      (+ (hash-table-count (store-table store))
         (if others (hash-table-count others) 0)))))

(defun store-map (store fn)
  "Call (FN KEY VALUE) for each entry in STORE under the store's mutex.
   FN must not call any STORE-* operation on STORE — that deadlocks on
   the mutex — and must not mutate the table directly (REMHASH or SETF
   GETHASH) during iteration. Intended for read-only inspection: dump
   on shutdown, collect keys for later deletion, snapshot state.
   Downstream apps rarely need this; the optional reaper subsumes the
   common walk-and-delete pattern. Returns NIL."
  (sb-thread:with-mutex ((store-lock store))
    (maphash fn (store-table store))
    (let ((others (store-others store)))
      (when others
        (maphash fn others))))
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
        (removed 0))
    (sb-thread:with-mutex ((store-lock store))
      ;; Each of the store's tables in turn: see STORE-TABLE-FOR.
      (dolist (table (list (store-table-in-use store) (store-others store)))
        (when table
          (let ((doomed '()))
            (maphash (lambda (k v)
                       (when (funcall expiry-fn k v)
                         (push k doomed)))
                     table)
            (dolist (k doomed)
              (remhash k table))
            (incf removed (length doomed))))))
    removed))

(defun start-store-reaper (store)
  "Spawn the reaper thread for STORE and register its stop function in
   *SHUTDOWN-HOOKS*. Called by MAKE-STORE when EXPIRY-FN is supplied;
   not a public entry point."
  (let ((interval (store-reap-interval store)))
    (setf (store-reaper-thread store)
          (sb-thread:make-thread
           (lambda ()
             ;; ARM visibility: read stop-requested under the mutex so
             ;; the write in stop-store-reaper is fenced. The reaper
             ;; already acquires the lock during sweeps, so this adds
             ;; zero contention.
             (loop until (sb-thread:with-mutex ((store-lock store))
                           (store-stop-requested store))
                   do
               ;; The interval is slept on the store's semaphore, which
               ;; STOP-STORE-REAPER signals, so a stop ends the wait at once
               ;; and an idle store costs no wake-up between sweeps. Waking
               ;; the thread beats TERMINATE-THREAD, which would interrupt
               ;; MAPHASH mid-sweep, an async-unsafe point; and it beats
               ;; polling the flag on a short sleep, which costs every store
               ;; a wake-up and a lock round trip several times a second for
               ;; the life of the process.
               (sb-thread:wait-on-semaphore (store-wake store) :timeout interval)
               (unless (sb-thread:with-mutex ((store-lock store))
                         (store-stop-requested store))
                 (handler-case
                     (let ((n (reap-store store)))
                       (when (> n 0)
                         (log-info "store reaped ~d entr~:@p" n)))
                   ;; SERIOUS-CONDITION, not ERROR, as every catch around
                   ;; application code takes it: see HANDLE-CLIENT-READ's
                   ;; last catch. EXPIRY-FN is application code, and this
                   ;; thread has nothing above it to catch what this misses.
                   (serious-condition (e)
                     (log-error "store reaper error: ~a" e))))))
           :name "web-skeleton-store-reaper"))
    (setf (store-cleanup store)
          (register-cleanup (lambda () (stop-store-reaper store))))))

(defun stop-store-reaper (store)
  "Stop STORE's reaper thread, so the store expires nothing more and its
   reaper no longer holds it. A shutdown hook does this for every store
   when START-SERVER returns; an application retiring a store before then
   calls it itself. Safe to call on a store with no reaper, and safe to
   call more than once. Sets STOP-REQUESTED and wakes the reaper, then
   waits for it to end: at once between sweeps, and at most 5 s if it is
   inside a sweep the expiry predicate made slow."
  (multiple-value-bind (thread cleanup)
      (sb-thread:with-mutex ((store-lock store))
        (multiple-value-prog1 (values (store-reaper-thread store)
                                      (store-cleanup store))
          (setf (store-stop-requested store) t
                (store-reaper-thread store)  nil
                (store-cleanup store)        nil)))
    (sb-thread:signal-semaphore (store-wake store))
    (when cleanup
      (unregister-cleanup cleanup))
    (when (and thread (sb-thread:thread-alive-p thread))
      (handler-case
          (sb-thread:join-thread thread :default nil :timeout 5)
        (error () nil)))))
