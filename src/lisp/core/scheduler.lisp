(in-package :swimmy.main)

;;; ==========================================
;;; SCHEDULER - Extracted from tick-handler.lisp (SRP Refactor)
;;; ==========================================
;;; Handles:
;;; - Periodic maintenance (60s throttle)
;;; - Dream cycle (1hr self-throttle)
;;; - Heartbeat checks
;;; - New day reset
;;; - 23:00 Daily Report trigger

(defparameter *last-maintenance-time* 0)
(defvar *last-new-day* nil "Tracks the last day number we processed for day rollover")
(defvar *daily-report-sent-today* nil "Prevents duplicate daily reports")

(defun run-periodic-maintenance ()
  "Handle periodic maintenance tasks (backtests, ecosystem, evolution) - Throttled 60s
   STRUCTURE NOTE (V8.3):
   - L54-65: Prediction/Backtest → 60s throttle (from this function)
   - L67-92: Dream Cycle → Self-throttled by *dream-interval* (3600s)
   - L94-95: Discord Heartbeat → Self-throttled internally"
  (let ((now (get-universal-time)))
    ;; PROFILE SECTION 1
    (with-profiling "maintenance-section-1"
      ;; SECTION 1: 60s Throttled Operations (Prediction, Backtest)
      (when (> (- now *last-maintenance-time*) 59)
        (setf *last-maintenance-time* now)
        
        ;; Scheduled tasks check
        (check-scheduled-tasks now)
        
        ;; V12.6: Existing Backtest Logic (batch-backtest-knowledge)
        (when (> (length *candle-history*) 100)
          (batch-backtest-knowledge))
        
        ;; Phase 6: Lifecycle Review (Unbenching Check)
        (when (fboundp 'perform-daily-lifecycle-review)
          (perform-daily-lifecycle-review))))
    ;; PROFILE SECTION 2
    (with-profiling "maintenance-section-2"
      ;; SECTION 2: Self-Throttled Operations (Dream Cycle - 1hr)
      (unless (numberp *last-dream-time*) (setf *last-dream-time* 0))
      (when (> (- now *last-dream-time*) *dream-interval*)
        (setf *last-dream-time* now)
        (format t "[DREAM CYCLE #~d] 💭 1-hour cycle...~%" *dream-cycle*)
        (when (> (length *candle-history*) 100)
          ;; V41.9: Removed legacy undefined checks (check-ecosystem-diversity, etc.)
          ;; Only keeping confirmed recurring tasks
          nil)
        ;; V6.17: Restart evolution if stagnating
        (check-evolution)
        (evolve-population-via-mutation)
        (incf *dream-cycle*)
        (when (and (zerop (mod *dream-cycle* 60)) (fboundp 'save-state))
          (funcall 'save-state))))
    ;; PROFILE SECTION 3
    (with-profiling "maintenance-section-3"
      ;; SECTION 3: Self-Throttled Operations (Discord Heartbeat)
      (when (fboundp 'swimmy.engine::check-discord-heartbeat)
        (swimmy.engine::check-discord-heartbeat))
      ;; P7: Check for stress test trigger flag
      (when (fboundp 'swimmy.school::check-stress-test-trigger)
        (swimmy.school::check-stress-test-trigger)))))

(defun check-scheduled-tasks (&optional (now (get-universal-time)))
  "Check and execute scheduled tasks based on time of day."
  (multiple-value-bind (s m h date month year day-of-week dst-p tz)
      (decode-universal-time now)
    (declare (ignore s m month year dst-p tz)) ;; Don't ignore day-of-week
    
    ;; 1. New Day Processing (Reset Logic)
    (when (or (null *last-new-day*) (not (= date *last-new-day*)))
      (format t "~%[SCHEDULER] 📅 NEW DAY DETECTED: ~a~%" date)
      (setf *last-new-day* date)
      (setf *daily-report-sent-today* nil) ;; Reset flag
      
      ;; Reset Daily Metrics
      (setf *daily-pnl* 0.0)
      (setf *daily-trades* 0)
      (setf *consecutive-wins* 0)
      (setf *consecutive-losses* 0)
      
      ;; CRITICAL FIX V8.0: Reset Daily PnL and DD
      ;; Note: Using :: for internal symbol access
      (setf swimmy.engine::*daily-pnl* 0.0)
      (setf swimmy.engine::*max-drawdown* 0.0)
      
      ;; V47.7: Q-table daily decay (1%/day) - López de Prado overfitting prevention
      (when (fboundp 'swimmy.school::decay-q-table)
        (funcall 'swimmy.school::decay-q-table))
      
      ;; Persist reset
      (when (fboundp 'swimmy.engine::save-state)
        (funcall 'swimmy.engine::save-state)))
        
    ;; 2. Scheduled Report (23:00 Trigger)
    (when (and (>= h 23) (not *daily-report-sent-today*))
      (format t "[SCHEDULER] ⏰ 23:00 Trigger - Sending Daily Report...~%")
      (send-daily-tribal-narrative)
      (setf *daily-report-sent-today* t))))
