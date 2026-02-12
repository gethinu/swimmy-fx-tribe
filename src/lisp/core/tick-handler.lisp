(in-package :swimmy.main)

;;; ===========================================
;;; TICK HANDLER (Orchestrator)
;;; ===========================================
;;; The central nervous system of the Lisp Brain.
;;; 
;;; REFACTORED (SRP - Expert Panel):
;;; - Maintenance -> scheduler.lisp
;;; - Narrative -> narrative.lisp
;;; - Message Dispatch -> message-dispatcher.lisp
;;; - Execution -> executor.lisp
;;;
;;; Remaining Responsibilities:
;;; - ZMQ Subscriber Loop (The "Pump")
;;; - High-level Error Handling for the Loop
;;; - Profiling wrappers

;;; ----------------------------------------------------------------------------
;;; STATE
;;; ----------------------------------------------------------------------------

(defparameter *last-candle-time* 0)
(defparameter *candle-history* nil) 
(defparameter *candle-histories* (make-hash-table :test 'equal)) 
(defparameter *candle-histories-tf* (make-hash-table :test 'equal)) 
(defvar *processed-candle-time* (make-hash-table :test 'equal))
(defvar *history-process-cache* (make-hash-table :test 'equal))
(defvar *last-guardian-heartbeat* 0)
(defvar *last-dream-time* 0)
(defparameter *dream-interval* 3600) 
(defvar *dream-cycle* 0)
(defvar *initial-backtest-done* nil) ; Phase 11 P1: Initialization Flag

(defun get-history ()
  "Get M1 candles (Legacy wrapper)"
  *candle-history*)

(defun get-history-tf (symbol tf)
  "Get Multi-Timeframe Candles"
  (if (gethash symbol *candle-histories-tf*)
      (gethash tf (gethash symbol *candle-histories-tf*))
      nil))

(defun update-candle (bid symbol)
  "Update per-symbol M1 candle history from live ticks."
  (let* ((now (get-universal-time))
         (current-bucket (floor now 60)))

    (unless (and symbol bid)
      (return-from update-candle nil))

    (let* ((history (or (gethash symbol *candle-histories*) *candle-history*))
           (latest (and history (first history)))
           (updated-history history))
      (if (and latest (= (floor (candle-timestamp latest) 60) current-bucket))
          ;; Update current candle (same minute bucket).
          (progn
            (when (> bid (candle-high latest)) (setf (candle-high latest) bid))
            (when (< bid (candle-low latest)) (setf (candle-low latest) bid))
            (setf (candle-close latest) bid)
            (incf (candle-volume latest)))
          ;; New minute candle.
          (push (make-candle :timestamp now
                             :open bid
                             :high bid
                             :low bid
                             :close bid
                             :volume 1)
                updated-history))

      (setf (gethash symbol *candle-histories*) updated-history)
      (unless (gethash symbol *candle-histories-tf*)
        (setf (gethash symbol *candle-histories-tf*) (make-hash-table :test 'equal)))
      (setf (gethash "M1" (gethash symbol *candle-histories-tf*)) updated-history)
      ;; Keep legacy single-history pointer aligned with the latest processed symbol.
      (setf *candle-history* updated-history))

    (setf *last-candle-time* now)))

;;; ----------------------------------------------------------------------------
;;; MAIN LOOP
;;; ----------------------------------------------------------------------------

(defun run-tick-loop ()
  "Main Loop - Subscribes to ZMQ"
  (format t "[TICK] 🚀 Starting Tick Handler Loop...~%")
  
  (loop
    (handler-case
        (let ((msg (pzmq:recv-string swimmy.globals:*subscriber*)))
           (process-msg msg) ; Delegate to message-dispatcher
           ;; Run Scheduler (Throttled inside)
           (run-periodic-maintenance))
      (error (e)
        (format t "[TICK] 🛑 Loop Error: ~a~%" e)
        (let ((restart-p (find-restart 'abort)))
          (when restart-p (invoke-restart restart-p)))))))
