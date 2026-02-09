;;; ============================================================================
;;; engine/ledger.lisp - State Persistence (Hickey's Warning)
;;; ============================================================================
;;; Saves and loads system state to survive restarts.
;;; "A system that forgets its positions is Stupid, not Simple." - Rich Hickey
;;; ============================================================================

(in-package :swimmy.engine)

(defparameter *state-file-path* 
  (merge-pathnames ".swimmy/state.sexp" (user-homedir-pathname))
  "Path to the persistent state file.")

;;; ==========================================
;;; SAVE STATE
;;; ==========================================

(defun ensure-state-directory ()
  "Ensure the .swimmy directory exists."
  (let ((dir (make-pathname :directory (pathname-directory *state-file-path*))))
    (ensure-directories-exist dir)))

(defun arm-to-plist (arm idx)
  "Convert an arm and its state to a property list for serialization."
  (let ((state (gethash idx *arm-states*)))
    (list :index idx
          :params (car arm)
          :wins (cadr arm)
          :losses (cddr arm)
          :state state)))

(defun save-state ()
  "Save current system state to disk atomically."
  (ensure-state-directory)
  (let ((tmp-path (merge-pathnames ".state.tmp" *state-file-path*)))
    (handler-case
        (progn
          (with-open-file (out tmp-path 
                               :direction :output 
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (let* ((arms-data (loop for arm in *arms*
                                    for idx from 0
                                    collect (arm-to-plist arm idx)))
                   (state-obj (list :version "1.0"
                                    :timestamp (get-universal-time)
                                    :arms arms-data
                                    :portfolio-indices *portfolio-indices*
                                    :daily-pnl (if (boundp '*daily-pnl*) *daily-pnl* 0)
                                    :accumulated-pnl (if (boundp '*accumulated-pnl*) *accumulated-pnl* 0)
                                    :daily-trade-count (if (boundp '*daily-trade-count*) *daily-trade-count* 0)
                                    :consecutive-wins (if (boundp '*consecutive-wins*) *consecutive-wins* 0)
                                    :consecutive-losses (if (boundp '*consecutive-losses*) *consecutive-losses* 0)
                                    :danger-level (if (boundp '*danger-level*) *danger-level* 0)
                                    :system-state (if (boundp '*system-state*) *system-state* nil)
                                    :trading-enabled (if (boundp '*trading-enabled*) *trading-enabled* nil)
                                    :last-prediction (if (boundp '*last-prediction*) *last-prediction* nil)
                                    :last-confidence (if (boundp '*last-confidence*) *last-confidence* nil)
                                    :current-regime (if (boundp '*current-regime*) *current-regime* nil)
                                    :volatility-regime (if (boundp '*volatility-regime*) *volatility-regime* nil)
                                    :current-equity (if (boundp '*current-equity*) *current-equity* 0.0)
                                    :peak-equity (if (boundp '*peak-equity*) *peak-equity* 0.0)
                                    :max-drawdown (if (boundp '*max-drawdown*) *max-drawdown* 0.0)
                                    :monitoring-peak-equity (if (boundp '*monitoring-peak-equity*) *monitoring-peak-equity* 0.0)
                                    :monitoring-drawdown (if (boundp '*monitoring-drawdown*) *monitoring-drawdown* 0.0)
                                    :current-drawdown (if (boundp '*current-drawdown*) *current-drawdown* 0.0)
                                    :last-account-info-time (if (boundp '*last-account-info-time*) *last-account-info-time* 0)
                                    :last-new-day (if (boundp '*last-new-day*) *last-new-day* nil)
                                    :daily-report-last-date (if (boundp '*daily-report-last-date*) *daily-report-last-date* nil)
                                    :daily-report-sent-today (if (boundp '*daily-report-sent-today*) *daily-report-sent-today* nil)
                                    ;; V7.1 Persistence (Andrew Ng)
                                    :failure-log (if (boundp '*failure-log*) *failure-log* nil)
                                    :success-log (if (boundp '*success-log*) *success-log* nil)
                                    :evolved-strategies (if (boundp '*evolved-strategies*) *evolved-strategies* nil))))
              ;; Simple JSON-like output (Lisp S-expression for now)
              (format out "~s~%" state-obj)))
          ;; Atomic rename to prevent corruption
          (rename-file tmp-path *state-file-path*)
          (format t "[LEDGER] 💾 State saved atomically to ~a~%" *state-file-path*)
          
          ;; V47.5: Save Q-table for persistence across restarts
          (when (fboundp 'swimmy.school::save-q-table)
            (funcall 'swimmy.school::save-q-table)))
      (error (e)
        (format t "[LEDGER] ❌ Save failed: ~a~%" e)))))


;;; ==========================================
;;; LOAD STATE
;;; ==========================================

(defun plist-to-arm (plist)
  "Convert a property list back to an arm structure."
  (let ((params (getf plist :params))
        (wins (getf plist :wins))
        (losses (getf plist :losses)))
    (cons params (cons wins losses))))

(defun load-state ()
  "Load system state from disk if it exists."
  (when (probe-file *state-file-path*)
    (handler-case
        (with-open-file (in *state-file-path* :direction :input)
          (let ((state-obj (read in nil nil)))
            (when state-obj
              (let ((arms-data (getf state-obj :arms))
                    (portfolio (getf state-obj :portfolio-indices))
                    (daily (getf state-obj :daily-pnl))
                    (acc (getf state-obj :accumulated-pnl))
                    (daily-trades (getf state-obj :daily-trade-count))
                    (cons-wins (getf state-obj :consecutive-wins))
                    (cons-losses (getf state-obj :consecutive-losses))
                    (danger (getf state-obj :danger-level))
                    (system-state (getf state-obj :system-state :missing))
                    (trading-enabled (getf state-obj :trading-enabled :missing))
                    (last-prediction (getf state-obj :last-prediction :missing))
                    (last-confidence (getf state-obj :last-confidence))
                    (current-regime (getf state-obj :current-regime :missing))
                    (volatility-regime (getf state-obj :volatility-regime :missing))
                    (current-equity (getf state-obj :current-equity))
                    (peak-equity (getf state-obj :peak-equity))
                    (max-dd (getf state-obj :max-drawdown))
                    (monitoring-peak (getf state-obj :monitoring-peak-equity))
                    (monitoring-dd (getf state-obj :monitoring-drawdown))
                    (current-dd (getf state-obj :current-drawdown))
                    (last-account-info-time (getf state-obj :last-account-info-time))
                    (last-new-day (getf state-obj :last-new-day :missing))
                    (daily-report-last-date (getf state-obj :daily-report-last-date :missing))
                    (daily-report-sent-today (getf state-obj :daily-report-sent-today :missing))
                    (fails (getf state-obj :failure-log))
                    (succs (getf state-obj :success-log))
                    (strats (getf state-obj :evolved-strategies)))
                ;; Restore arms
                (setf *arms* (mapcar #'plist-to-arm arms-data))
                ;; Restore arm-states hash table
                (clrhash *arm-states*)
                (dolist (plist arms-data)
                  (let ((idx (getf plist :index))
                        (state (getf plist :state)))
                    (when state
                      (setf (gethash idx *arm-states*) state))))
                ;; Restore portfolio
                (when portfolio
                  (setf *portfolio-indices* portfolio))
                ;; Restore PnL
                (when (boundp '*daily-pnl*)
                  (setf *daily-pnl* (or daily 0)))
                (when (boundp '*accumulated-pnl*)
                  (setf *accumulated-pnl* (or acc 0)))
                (when (boundp '*daily-trade-count*)
                  (setf *daily-trade-count* (or daily-trades 0)))
                (when (boundp '*consecutive-wins*)
                  (setf *consecutive-wins* (or cons-wins 0)))
                (when (boundp '*consecutive-losses*)
                  (setf *consecutive-losses* (or cons-losses 0)))
                (when (boundp '*danger-level*)
                  (setf *danger-level* (or danger 0)))
                (when (and (boundp '*system-state*) (not (eq system-state :missing)))
                  (setf *system-state* system-state))
                (when (and (boundp '*trading-enabled*) (not (eq trading-enabled :missing)))
                  (setf *trading-enabled* trading-enabled))
                (when (and (boundp '*last-prediction*) (not (eq last-prediction :missing)))
                  (setf *last-prediction* last-prediction))
                (when (boundp '*last-confidence*)
                  (setf *last-confidence* (or last-confidence 0.0)))
                (when (and (boundp '*current-regime*) (not (eq current-regime :missing)))
                  (setf *current-regime* current-regime))
                (when (and (boundp '*volatility-regime*) (not (eq volatility-regime :missing)))
                  (setf *volatility-regime* volatility-regime))
                (when (and (boundp '*current-equity*) (numberp current-equity))
                  (setf *current-equity* current-equity))
                (when (and (boundp '*peak-equity*) (numberp peak-equity))
                  (setf *peak-equity* peak-equity))
                (when (and (boundp '*max-drawdown*) (numberp max-dd))
                  (setf *max-drawdown* max-dd))
                (when (and (boundp '*monitoring-peak-equity*) (numberp monitoring-peak))
                  (setf *monitoring-peak-equity* monitoring-peak))
                (when (and (boundp '*monitoring-drawdown*) (numberp monitoring-dd))
                  (setf *monitoring-drawdown* monitoring-dd))
                (when (and (boundp '*current-drawdown*) (numberp current-dd))
                  (setf *current-drawdown* current-dd))
                (when (and (boundp '*last-account-info-time*) (numberp last-account-info-time))
                  (setf *last-account-info-time* last-account-info-time))
                (when (and (boundp '*last-new-day*) (not (eq last-new-day :missing)))
                  (setf *last-new-day* last-new-day))
                (when (and (boundp '*daily-report-last-date*) (not (eq daily-report-last-date :missing)))
                  (setf *daily-report-last-date* daily-report-last-date))
                (when (and (boundp '*daily-report-sent-today*) (not (eq daily-report-sent-today :missing)))
                  (setf *daily-report-sent-today* daily-report-sent-today))
                
                ;; V7.1 Restore Learning Data
                (when (boundp '*failure-log*)
                  (setf *failure-log* fails))
                (when (boundp '*success-log*)
                  (setf *success-log* succs))
                (when (and strats (boundp '*evolved-strategies*))
                  (setf *evolved-strategies* strats))
                
                (format t "[LEDGER] 📂 State restored: ~d arms, portfolio ~a, ~d strategies~%" 
                        (length *arms*) *portfolio-indices* (length (or strats nil)))
                
                ;; V8.6: Restore Cold Reality Metrics (Sharpe, DD) from logs
                (when (fboundp 'recalculate-portfolio-stats)
                  (recalculate-portfolio-stats))))))
      (error (e)
        (format t "[LEDGER] ⚠️ Load failed: ~a~%" e)))))

(format t "[ENGINE] ledger.lisp loaded~%")
