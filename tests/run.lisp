(in-package :web-skeleton-tests)

;;; ===========================================================================
;;; Test utilities and runner
;;;
;;; This file holds the two assertion macros every test in the suite goes
;;; through (CHECK, CHECK-ERROR), the per-suite reporting, and TEST, which
;;; runs everything and is what run-tests.lisp calls.
;;;
;;; ---------------------------------------------------------------------------
;;; The suites
;;;
;;;   test-algorithms  SHA-1, SHA-256, HMAC, hex, base64, ECDSA, random
;;;   test-json        JSON parser and serializer
;;;   test-server      HTTP parsing and response building, URL and query,
;;;                    routing, connection state, WebSocket, static files,
;;;                    outbound fetch, JWT
;;;   test-store       concurrent keyed store and its reaper
;;;   test-properties  generated-input invariants, and agreement between
;;;                    implementations that are meant to be equivalent
;;;   test-harness     the live-server harness's own round-trips
;;;   test-tls         TLS registration; skips when libssl is absent
;;;
;;; TEST runs all seven, then re-runs the crypto suite as
;;; TEST-PURE-LISP-CRYPTO if libssl is loaded. That second pass is not
;;; redundant: when libssl is present it has swapped its own
;;; implementations into the sha1 / sha256 / ecdsa symbol cells, so the
;;; first pass never touched the pure-Lisp originals in
;;; src/algorithms/. The re-run swaps them back so an edit there is
;;; caught on a machine that has libssl too.
;;;
;;; ---------------------------------------------------------------------------
;;; Running
;;;
;;;   sbcl --non-interactive --load run-tests.lisp     ; everything, exit 1 on fail
;;;   (web-skeleton-tests:test-server)                 ; one suite, from a REPL
;;;
;;; CI runs the same entry point with the fasl cache deleted first, and
;;; fails the build on any "caught WARNING" or "caught STYLE-WARNING" in
;;; the output as well as on a failing test. A warning-free cold build is
;;; therefore part of the contract, not a nicety — an undefined function
;;; or an unused binding fails CI exactly as a bad assertion does. Run it
;;; the same way locally before believing a change is finished.
;;;
;;; ---------------------------------------------------------------------------
;;; Writing a test
;;;
;;; CHECK compares with EQUAL and prints both values on failure, so
;;; prefer expressions that reduce to something legible — a list, a
;;; length, a status code — over a struct whose printed form fills the
;;; screen. CHECK-ERROR asserts that a form signals, and notes the
;;; condition type when it is not an HTTP-PARSE-ERROR.
;;;
;;; Test names are printed verbatim and are the only thing a reader of a
;;; failing CI log gets, so name the behaviour and not the function:
;;; "range: empty resource, suffix range unsatisfiable" survives a
;;; rename; "test parse-byte-range 7" does not.
;;;
;;; Two styles are available, both from web-skeleton-test-harness:
;;; WITH-TEST-SERVER for a real round-trip against a live single-worker
;;; server on an ephemeral port, and MAKE-TEST-REQUEST for building a
;;; request struct and calling a handler directly. Prefer the second
;;; unless the thing under test is the network path itself — it is
;;; faster and it cannot hang.
;;;
;;; Dynamic bindings do not cross SB-THREAD:MAKE-THREAD. A LET around a
;;; special will not be seen by a worker the harness starts, so a test
;;; that needs the server to observe a changed parameter has to SETF it
;;; globally and restore it afterwards.
;;;
;;; ---------------------------------------------------------------------------
;;; Checking that a check tests something
;;;
;;; A test that passes against the pre-fix code is not coverage. It is
;;; possible to write an assertion that names the right behaviour, reads
;;; correctly, passes, and would pass just as well with the fix removed —
;;; because it fails an earlier guard, or asserts a value both versions
;;; produce, or proves a binding exists without proving anything reads
;;; it. Rereading it will not reveal this. Removing the fix will.
;;;
;;; So for anything security-relevant: revert the fix and confirm the
;;; check fails. Two ways, neither touching the working tree:
;;;
;;;   - reimplement the pre-fix function inside a probe and show
;;;     old-accepts / new-rejects on the same input
;;;   - redefine it in a loaded image and re-run the suite:
;;;     (in-package :web-skeleton), defun, then (web-skeleton-tests:test)
;;;
;;; Six things that make the second one lie:
;;;
;;;   - IN-PACKAGE only works at top level. It acts when the reader reads
;;;     it, so an IN-PACKAGE nested inside a HANDLER-BIND or a LET has
;;;     already been read — along with everything after it — in the old
;;;     package. A DEFUN meant for an internal web-skeleton function then
;;;     silently defines a same-named symbol in the test package and the
;;;     original runs untouched. Exported names hide this, because :USE
;;;     inherits the symbol and the redefinition lands correctly; internal
;;;     ones do not. Write WEB-SKELETON::NAME explicitly and the question
;;;     does not arise.
;;;   - One process per revert. Running TEST twice in one image deadlocks
;;;     on the listeners the harness binds.
;;;   - (SYMBOL-FUNCTION 'F), never #'F, when capturing an original to
;;;     call from its replacement. #'F resolves through the fdefn and so
;;;     follows the redefinition — the wrapper calls itself forever.
;;;   - Macros are already expanded into their compiled callers, so
;;;     redefining one in the image changes nothing. Revert a function it
;;;     expands into instead.
;;;   - If the revert breaks shared infrastructure, assert against the
;;;     affected function directly rather than through TEST. A revert
;;;     that takes the harness down produces a hang, not a result.
;;;   - An inlined or block-compiled callee does not follow a redefinition
;;;     at all: the caller holds the expansion, not the fdefn. Nothing here
;;;     is declaimed INLINE today, and TEST-DISCARD-AVAILABLE-WANT-WRITE is
;;;     the first test that depends on that staying true — it stubs
;;;     CONNECTION-READ-INTO and asserts what its caller does with the
;;;     answer. Declaim that function INLINE and the test passes against
;;;     the original code while appearing to drive the stub.
;;;
;;; The same rule holds outside testing: do not enter a multi-step state
;;; change without first confirming every intermediate step passes.
;;; ===========================================================================

(defvar *crlf* (coerce '(#\Return #\Newline) 'string)
  "CRLF string for constructing test request data.")

;;; ---------------------------------------------------------------------------
;;; Reaching into the optional TLS system
;;;
;;; Two helpers rather than one, and using both together is the point.
;;; web-skeleton-tls is optional — run-tests.lisp loads it at runtime and
;;; this system does not depend on it — so a test that wants one of its
;;; functions cannot name the symbol literally: that interns it at read
;;; time and emits undefined-function warnings on every compile of a tree
;;; without TLS, which fails CI.
;;;
;;; The wrong way to solve that is to gate on the symbol itself, with
;;; FBOUNDP or a bare FIND-SYMBOL, and infer from a miss that libssl is
;;; absent. Such a guard cannot tell "TLS not loaded" from "that function
;;; was renamed or deleted", so the second case skips silently on a
;;; machine where TLS is loaded and the assertion was meant to run.
;;; Deleting TLS-STREAM-RESPONSE did exactly that to five assertions in
;;; test-properties: green suite, lost coverage, visible only as a
;;; per-suite count dropping.
;;;
;;; So: ask TLS-LOADED-P whether the system is there, and inside that
;;; branch use TLS-SYM, which raises on a miss because by then a miss can
;;; only mean the name moved.
;;; ---------------------------------------------------------------------------

(defun tls-loaded-p ()
  "True when web-skeleton-tls is in the image. *HTTPS-STREAM-FN* is the
   signal because registering it is the last thing tls.lisp does on load,
   so it is set only once the FFI bindings and the crypto swaps have all
   succeeded."
  (not (null web-skeleton:*https-stream-fn*)))

(defun tls-sym (name)
  "Resolve a WEB-SKELETON symbol that exists only once web-skeleton-tls
   is loaded, raising if it is not there.

   The raise is the feature. Call this only from inside a TLS-LOADED-P
   branch, where a missing symbol cannot mean 'TLS is absent' and must
   therefore mean the name changed — which should stop the suite rather
   than quietly remove a test from it."
  (or (find-symbol name :web-skeleton)
      (error "web-skeleton::~a not found. web-skeleton-tls is loaded, so ~
              this name has moved and whatever tested it is no longer ~
              testing anything."
             name)))

(defvar *tests-passed* 0)

(defvar *tests-skipped* 0
  "Assertions a run declined to make, across every suite.

   Global rather than per-suite, and reset once by TEST rather than by each
   suite's entry defun: what it answers is a question about the whole run.

   It exists because a skip is a silent loss of coverage and this tree has
   already paid for one. TEST-PROPERTIES records the case in its own words —
   a FIND-SYMBOL miss turned five assertions into SKIPs and the suite stayed
   green while losing them. A totals line that says `0 skipped` cannot do
   that; one that omits the number leaves every reader to assume it.")

(defun skip (reason)
  "Decline an assertion, visibly and countably.

   Prints in CHECK's register so a skip reads like the assertion it replaces
   rather than like a comment, and counts, so the totals line carries it."
  (format t "  SKIP  ~a~%" reason)
  (incf *tests-skipped*))

(defvar *tests-failed* 0)
(defvar *failed-names* nil
  "Names of failing tests in the currently-running suite.
   Populated by CHECK / CHECK-ERROR, reset per suite, replayed by
   REPORT-SUITE at the summary line so a long run's failures are
   legible without scrolling.")
(defvar *all-failed-names* nil
  "Names of failing tests across every suite in the current TEST run.
   Appended to by REPORT-SUITE at each suite's summary; reset only
   by TEST at the top of the run. Echoed after the grand total so
   a full-suite run surfaces the complete failure list once more —
   scrolling up through every suite's block is not required.")

(defmacro attempt (&body body)
  "Evaluate BODY, answering its value, or the error text if it raised.

   For a CHECK whose subject can raise — a reader, a writer, anything over a
   transport. An uncaught raise ends the run mid-file: no failure list, no
   totals, and every later assertion unexecuted. That makes the check
   unreadable by the discipline every revert here is read under, which is
   the full failure list, three runs. A raise that becomes a failed CHECK
   carrying the condition text costs nothing and stays countable."
  `(handler-case (progn ,@body)
     (error (e) (princ-to-string e))))

(defmacro check (name expr expected)
  "Assert that EXPR produces EXPECTED. Logs pass/fail."
  `(let ((result ,expr))
     (if (equal result ,expected)
         (progn
           (format t "  PASS  ~a~%" ,name)
           (incf *tests-passed*))
         (progn
           (format t "  FAIL  ~a~%    expected: ~s~%         got: ~s~%"
                   ,name ,expected result)
           (incf *tests-failed*)
           (push ,name *failed-names*)))))

(defmacro check-error (name expr)
  "Assert that EXPR signals an HTTP-PARSE-ERROR (or any error as fallback)."
  `(if (handler-case (progn ,expr nil)
         (http-parse-error () t)
         (error (e)
           (format t "  NOTE  ~a caught ~a (not http-parse-error)~%" ,name (type-of e))
           t))
       (progn
         (format t "  PASS  ~a~%" ,name)
         (incf *tests-passed*))
       (progn
         (format t "  FAIL  ~a (expected error, got none)~%" ,name)
         (incf *tests-failed*)
         (push ,name *failed-names*))))

(defun report-suite (suite-name &optional suffix)
  "Emit the per-suite summary line, preceded by the list of failing
   test names when any ran. Replayed names are in declaration order
   (CHECK / CHECK-ERROR push onto the head, so walk in reverse).
   Appends (SUITE-NAME . TEST-NAME) pairs onto *ALL-FAILED-NAMES* so
   TEST can echo the full cross-suite list after the grand total —
   the per-suite block scrolls offscreen on a long run; the replay
   keeps the failing names adjacent to the final pass/fail line,
   tagged with their suite so triage does not require re-scrolling
   to match a name back to where it fired. SUFFIX goes on the
   pass/fail line (e.g. \"(pure-Lisp)\" for the re-verification
   pass). Called at the end of each suite's entry defun."
  (when *failed-names*
    (format t "~%Failed:~%")
    (dolist (name (reverse *failed-names*))
      (format t "  ~a~%" name))
    (setf *all-failed-names*
          (append *all-failed-names*
                  (mapcar (lambda (n) (cons suite-name n))
                          (reverse *failed-names*)))))
  (format t "~%~d passed, ~d failed~@[ ~a~]~%~%"
          *tests-passed* *tests-failed* suffix))

(defun test ()
  "Run all tests. Returns T if all passed."
  (let ((all-passed t)
        (total-passed 0)
        (total-failed 0))
    (setf *all-failed-names* nil
          *tests-skipped* 0)
    (dolist (suite '(test-algorithms test-json test-server test-store test-properties
                     test-harness test-tls))
      (unless (funcall suite)
        (setf all-passed nil))
      (incf total-passed *tests-passed*)
      (incf total-failed *tests-failed*))
    ;; When libssl is loaded, the earlier TEST-ALGORITHMS run
    ;; exercised the libssl-backed sha1 / sha256 / ecdsa-verify-p256
    ;; via the swapped symbol cells — the pure-Lisp originals went
    ;; completely untested. Re-run the crypto suite with the
    ;; function cells temporarily swapped back, so edits to
    ;; src/algorithms/*.lisp are caught on libssl-enabled machines
    ;; too. No-op when libssl is absent: the default function
    ;; cells already are the pure-Lisp versions, so there is
    ;; nothing to re-verify.
    (when web-skeleton:*https-stream-fn*
      (unless (test-pure-lisp-crypto)
        (setf all-passed nil))
      (incf total-passed *tests-passed*)
      (incf total-failed *tests-failed*))
    (when *all-failed-names*
      (format t "Failed across all suites:~%")
      (dolist (entry *all-failed-names*)
        (format t "  [~a] ~a~%" (car entry) (cdr entry)))
      (format t "~%"))
    (format t "~d passed, ~d failed, ~d skipped across all suites~%"
            total-passed total-failed *tests-skipped*)
    (if all-passed
        (format t "=== ALL TESTS PASSED ===~%~%")
        (format t "=== SOME TESTS FAILED ===~%~%"))
    all-passed))
