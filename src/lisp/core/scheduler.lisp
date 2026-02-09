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
(defvar *daily-report-sent-today* nil "Prevents duplicate daily reports")
(defvar *advisor-report-sent-today* nil "Prevents duplicate advisor reports")
(defvar *daily-pnl-aggregation-sent-today* nil "Prevents duplicate daily pnl aggregation")
;; (defvar *breeding-cycle-run-today* nil) ;; REMOVED: V50.3 Now run periodically

(defparameter *last-evolution-time* 0 "Timestamp of last evolution run")
(defparameter *evolution-interval* 1800 "30 minutes (Musk's Order)")

;; V49.1: Data-driven Schedule (Martin Fowler Refactor)
(defparameter *scheduled-events*
  '((:advisor-report :hour 8  :sent-flag *advisor-report-sent-today* :fn send-morning-papers)
    (:daily-report   :hour 23 :sent-flag *daily-report-sent-today*   :fn send-nightly-reports))
    ;; (:breeding-cycle ...) REMOVED -> Moved to periodic maintenance
  "List of scheduled tasks with their trigger hour and tracking flags.")

(defun run-evolution-factory ()
  "Evolution Factory Trigger (Phase 21) - 30min Loop"
  (format t "[SCHEDULER] 🧬 Evolution Factory Triggered (~d sec since last)...~%" 
          (- (get-universal-time) *last-evolution-time*))
  (when (fboundp 'swimmy.school::process-breeding-cycle)
    (funcall 'swimmy.school::process-breeding-cycle)))

(defun send-morning-papers ()
  "Morning paper trigger"
  (format t "[SCHEDULER] ⏰ 08:00 Running Advisor Council...~%")
  (when (fboundp 'swimmy.school::run-advisor-council)
    (funcall 'swimmy.school::run-advisor-council)))

(defun send-nightly-reports ()
  "Nightly reports trigger (Daily + Weekly)"
  (multiple-value-bind (s m h date month year day-of-week) (decode-universal-time (get-universal-time))
    (declare (ignore s m h date month year))
    (format t "[SCHEDULER] ⏰ 23:00 Sending Nightly Reports...~%")
    (send-daily-status-report)
    ;; Weekly Summary (Sunday - DOW 6)
    (when (= day-of-week 6)
      (format t "[SCHEDULER] 📊 Sunday detected - Generating Weekly Summary...~%")
      (when (fboundp 'swimmy.shell::generate-weekly-summary)
        (funcall 'swimmy.shell::generate-weekly-summary)))))

(defun maybe-trigger-heartbeat-now ()
  "If .opus/heartbeat.now exists, send a heartbeat and delete the trigger file."
  (let ((trigger (swimmy.core::swimmy-path ".opus/heartbeat.now")))
    (when (probe-file trigger)
      (when (fboundp 'swimmy.engine::send-discord-heartbeat)
        (ignore-errors (swimmy.engine::send-discord-heartbeat)))
      (ignore-errors (delete-file trigger)))))

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
        
        (when (fboundp 'swimmy.school::maybe-send-evolution-report)
          (funcall 'swimmy.school::maybe-send-evolution-report :now now :reason "scheduled"))
        (when (fboundp 'swimmy.school::maybe-alert-evolution-report-staleness)
          (funcall 'swimmy.school::maybe-alert-evolution-report-staleness :now now))
        
        ;; V12.6: Existing Backtest Logic (batch-backtest-knowledge)
        (when (> (length *candle-history*) 100)
          (batch-backtest-knowledge))
        
        ;; Phase 6: Lifecycle Review
        (when (fboundp 'perform-daily-lifecycle-review)
          (perform-daily-lifecycle-review))
        
        ;; Phase 19: Global Macro Data Reload (Every 15 mins)
        (when (> (- now (or swimmy.school::*last-macro-load-time* 0)) 900) ; 15 mins * 60s
          (when (fboundp 'swimmy.school::load-macro-data)
            (funcall 'swimmy.school::load-macro-data)
            (setf swimmy.school::*last-macro-load-time* now)))
            
        ;; Phase 21: Evolution Factory (Every 30 mins) - V50.3 Musk Speedup
        (when (> (- now *last-evolution-time*) *evolution-interval*)
          (setf *last-evolution-time* now)
          (run-evolution-factory))))
            
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
      (when (fboundp 'swimmy.main::maybe-trigger-heartbeat-now)
        (swimmy.main::maybe-trigger-heartbeat-now))
      (when (fboundp 'swimmy.engine::check-discord-heartbeat)
        (swimmy.engine::check-discord-heartbeat))
      ;; Deferred founder backtests (rate-limited; avoid blocking recv loop)
      (when (fboundp 'swimmy.school::maybe-flush-deferred-founders)
        (swimmy.school::maybe-flush-deferred-founders))
      ;; P7: Check for stress test trigger flag
      (when (fboundp 'swimmy.school::check-stress-test-trigger)
        (swimmy.school::check-stress-test-trigger))
      ;; Flush batched alert buffers in brain loop (Stagnant C-Rank / Max Age)
      (when (fboundp 'swimmy.core:check-timeout-flushes)
        (swimmy.core:check-timeout-flushes)))))

(defun check-scheduled-tasks (&optional (now (get-universal-time)))
  "Check and execute scheduled tasks based on data-driven configuration."
  (multiple-value-bind (s m h date month year day-of-week dst-p tz)
      (decode-universal-time now)
    (declare (ignore s dst-p tz day-of-week))
    (let ((day-key (+ (* year 10000) (* month 100) date)))
    
    ;; 1. New Day Processing (Reset Logic)
      (when (or (null *last-new-day*) (not (= day-key *last-new-day*)))
      (format t "~%[SCHEDULER] 📅 NEW DAY DETECTED: ~a~%" date)
        (setf *last-new-day* day-key)
      (setf *daily-report-sent-today* nil)
      (setf *advisor-report-sent-today* nil)
      (setf *daily-pnl-aggregation-sent-today* nil)
      ;; *breeding-cycle-run-today* removed
      (setf *daily-pnl* 0.0)
      (setf *daily-trade-count* 0)
      (setf *consecutive-wins* 0)
      (setf *consecutive-losses* 0)
      (setf swimmy.engine::*daily-pnl* 0.0)
      (setf swimmy.engine::*max-drawdown* 0.0)
      (when (fboundp 'swimmy.school::decay-q-table)
        (funcall 'swimmy.school::decay-q-table))
      (when (fboundp 'swimmy.engine::save-state)
        (funcall 'swimmy.engine::save-state)))

    ;; Sync daily report sent flag from persisted date
    (setf *daily-report-sent-today*
          (and (numberp *daily-report-last-date*)
               (= *daily-report-last-date* day-key)))
        
    ;; 2. Generic Task Runner (Iterates *scheduled-events*)
    (dolist (event *scheduled-events*)
      (let* ((event-name (car event))
             (trigger-h (getf (cdr event) :hour))
             (sent-sym (getf (cdr event) :sent-flag))
             (task-fn (getf (cdr event) :fn)))
        (when (and (>= h trigger-h) (not (symbol-value sent-sym)))
          (setf (symbol-value sent-sym) t) ;; Claim executed first
          (when (eq event-name :daily-report)
            (setf *daily-report-last-date* day-key))
          (funcall task-fn))))

    ;; 3. Daily PnL Aggregation (00:10)
    (when (and (= h 0) (>= m 10) (not *daily-pnl-aggregation-sent-today*))
      (setf *daily-pnl-aggregation-sent-today* t)
      (format t "[SCHEDULER] ⏰ 00:10 Aggregating daily PnL...~%")
      (when (fboundp 'swimmy.school::refresh-strategy-daily-pnl)
        (funcall 'swimmy.school::refresh-strategy-daily-pnl))
      (when (fboundp 'swimmy.school::refresh-pair-strategies)
        (funcall 'swimmy.school::refresh-pair-strategies))
      (when (fboundp 'swimmy.school::refresh-pair-active-defs)
          (funcall 'swimmy.school::refresh-pair-active-defs))))))
