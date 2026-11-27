(in-package :web-skeleton)

;;; ===========================================================================
;;; The clock intervals are measured on
;;;
;;; Two questions get asked of a clock, and they want different clocks.
;;; What the date is: a Date header, a log line's timestamp, a token's
;;; expiry. And how long it has been: since a connection last moved,
;;; until a fetch is given up on, since the last sweep.
;;;
;;; The first is the system's clock, which is set: stepped by NTP after a
;;; boot or a resume, resynced by a hypervisor when its host wakes,
;;; changed by hand, or years out on a device with no battery behind it
;;; until the network corrects it. Intervals taken on that clock move
;;; with every step. Forward, and everything waiting is overdue at once:
;;; each parked fetch answered 504, each body and each pending write
;;; closed as stalled. Backward, and nothing is due until the clock has
;;; made the step up again, the event loop's own once-a-second work
;;; included.
;;;
;;; So every interval the framework measures is read here, off a clock
;;; that is never set, and GET-UNIVERSAL-TIME is left for what is a date.
;;; An application has both questions too, and MONOTONIC-SECONDS is
;;; exported for the second.
;;; ===========================================================================

(defconstant +monotonic-seconds-start+ 1000000000
  "What MONOTONIC-SECONDS reads when a process starts, about: a billion
   seconds, some thirty years.")

(defun monotonic-seconds ()
  "Whole seconds on a clock that only runs: for a timestamp that will be
   subtracted from another, and a deadline that will be compared with
   one. The framework reads every timeout from it, and an application
   reads its own intervals: how long a session has been idle, a rate
   limit's window, how long the process has been up. What is a date, a
   token's expiry or a line's timestamp, is GET-UNIVERSAL-TIME's.

   Never a date. Only the difference of two readings means anything, and
   only within one process: every process starts at the same reading, so
   one kept in a saved image, written to disk or handed to another
   process compares with nothing.

   Read from GET-INTERNAL-REAL-TIME, which SBCL takes on Linux from a
   monotonic clock: setting the system's clock does not move it, and it
   does not run while the machine is suspended.

   Readings start high rather than at zero. A slot that holds one is 0
   until it is set, and 0 is then what it should be: longer ago than any
   timeout, and never a reading."
  (+ +monotonic-seconds-start+
     (floor (get-internal-real-time) internal-time-units-per-second)))
