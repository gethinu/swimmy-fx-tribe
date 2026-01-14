;; risk-manager.lisp - Consolidated Risk Management System
;; V6.13: Unified risk management from school.lisp, school-fortress.lisp, brain.lisp
;; Taleb's Antifragility + Thorp's Kelly Criterion

(in-package :swimmy.engine)

;;; ==========================================
;;; RISK MANAGER API
;;; ==========================================

(defvar *risk-alert-throttle* (make-hash-table :test 'equal)
  "Timestamp of last alert for (symbol action reason) to prevent spam")

(defun risk-check-all (symbol direction lot category)
  "Unified risk check before trade execution. Returns (approved-p adjusted-lot reason)"
  (let ((checks nil)
        (final-lot lot)
        (approved t))
    
    ;; 1. Danger cooldown check
    (when (and (fboundp 'danger-cooldown-active-p) (danger-cooldown-active-p))
      (push "DANGER_COOLDOWN" checks)
      (setf approved nil))
    
    ;; 2. Resignation check
    (when (and (boundp '*has-resigned-today*) *has-resigned-today*)
      (push "RESIGNED" checks)
      (setf approved nil))
    
    ;; 3. Correlation exposure check (Relaxed V7.0: 0.8 -> 0.95)
    (when (fboundp 'check-correlation-risk)
      (let ((corr-risk (check-correlation-risk symbol direction)))
        (when (> corr-risk 0.95)
          (push "HIGH_CORRELATION" checks)
          (setf final-lot (* final-lot 0.5)))))
    
    ;; 4. Symbol exposure check (Relaxed V7.0: 0.15 -> 0.30)
    (when (fboundp 'get-symbol-exposure)
      (let ((exposure (get-symbol-exposure symbol)))
        (when (> exposure (or *max-symbol-exposure* 0.30))
          (push "SYMBOL_EXPOSURE_LIMIT" checks)
          (setf final-lot (* final-lot 0.5)))))
    
    ;; 5. P0: Daily loss limit PRE-OPEN CHECK (BLOCK, not just warn)
    ;; This prevents opening new positions after hitting loss limit
    (when (and (boundp '*daily-pnl*) (< *daily-pnl* -5000))
      (push "DAILY_LOSS_LIMIT_EXCEEDED" checks)
      (format t "[L] 🛑 PRE-OPEN BLOCK: Daily loss ¥~,0f exceeds ¥-5000 limit~%" *daily-pnl*)
      (setf approved nil))
    
    ;; 6. Gotobi adjustment (USDJPY only)
    (when (and (fboundp 'apply-gotobi-adjustment) (string= symbol "USDJPY"))
      (let ((adj (apply-gotobi-adjustment symbol direction)))
        (when adj (setf final-lot (* final-lot adj)))))
    
    ;; 7. London edge adjustment (EURUSD/GBPUSD)
    (when (and (fboundp 'apply-london-edge) 
               (or (string= symbol "EURUSD") (string= symbol "GBPUSD")))
      (let ((adj (apply-london-edge symbol direction)))
        (when adj (setf final-lot (* final-lot adj)))))
    
    ;; 8. Volatility check - V6.5: Just log, don't reduce further (already handled by school-volatility)
    (when (and (boundp '*current-volatility-state*) 
               (eq *current-volatility-state* :extreme))
      (push "EXTREME_VOLATILITY" checks))
      
    ;; 9. TALEB'S GATEKEEPER - V8.5: Allow 0.01 for tiered sizing
    (when (< final-lot 0.01)
      (setf final-lot 0.01))  ; Minimum for proving ground strategies
    
    ;; 10. MUSK'S PANEL DECISION: Tiered lot sizing for low-Sharpe strategies
    ;; Sharpe 0.0 ~ 0.3 = cap at 0.01 lot (proving ground)
    ;; Sharpe > 0.3 = normal lot calculation allowed
    ;; Note: Strategy sharpe is not available here, this is handled in school.lisp
    
    ;; Return results (Max cap increased to 0.50, min lowered to 0.01)
    (values approved 
            (max 0.01 (min 0.50 final-lot))
            (if checks (format nil "~{~a~^, ~}" checks) "APPROVED"))))

(defun safe-order (action symbol lot sl tp &optional (magic 0) (comment nil))
  "Safe wrapper for placing orders via ZeroMQ. Enforces all risk checks via CENTRALIZED RISK GATEWAY."
  (let ((daily-pnl (if (boundp '*daily-pnl*) *daily-pnl* 0.0))
        (equity (if (boundp '*current-equity*) *current-equity* 0.0))
        (cons-losses (if (boundp '*consecutive-losses*) *consecutive-losses* 0)))
        
    ;; 1. External Authority Check (Phase 3 Risk Gateway)
    (multiple-value-bind (approved-p reason)
        (request-trade-approval action symbol lot daily-pnl equity cons-losses)
      
      (if approved-p
          ;; 2. Local Logic Checks (Correlation, etc.)
          (multiple-value-bind (local-approved adjusted-lot local-reason)
              (risk-check-all symbol action lot :standard)
            
            (if local-approved
                (progn
                  (log-info (format nil "RISK APPROVED: ~a ~a (Lot: ~,2f)" action symbol adjusted-lot)
                            :data (jsown:new-js 
                                    ("type" "trade_approved")
                                    ("action" action)
                                    ("symbol" symbol)
                                    ("adjusted_lot" adjusted-lot)))
                  (let ((msg (jsown:to-json 
                               (jsown:new-js 
                                 ("action" action)
                                 ("symbol" symbol)
                                 ("lot" (read-from-string (format nil "~,2f" adjusted-lot)))
                                 ("sl" sl)
                                 ("tp" tp)
                                 ("magic" magic)
                                 ("comment" (or comment ""))))))
                    (pzmq:send *cmd-publisher* msg)
                    t))
                (progn
                  ;; Local Denial
                  (log-warn (format nil "LOCAL RISK DENIED: ~a ~a Reason: ~a" action symbol local-reason)
                            :data (jsown:new-js ("type" "trade_denied") ("reason" local-reason)))
                  (notify-discord (format nil "🛡️ Local Risk Denied: ~a ~a (~a)" action symbol local-reason) :color 15158332)
                  nil)))
          (progn
            ;; Gateway Denial (Taleb's Authority)
            (log-warn (format nil "GATEWAY DENIED: ~a ~a Reason: ~a" action symbol reason)
                      :data (jsown:new-js ("type" "gateway_denied") ("reason" reason)))
            
            ;; Throttling Logic for Discord (5 minutes = 300 seconds)
            (let* ((alert-key (format nil "~a-~a-~a" symbol action reason))
                   (last-time (gethash alert-key *risk-alert-throttle*))
                   (now (get-universal-time)))
              (when (or (null last-time) (> (- now last-time) 300))
                (setf (gethash alert-key *risk-alert-throttle*) now)
                (notify-discord (format nil "🛡️ GATEKEEPER BLOCKED: ~a ~a (~a)" action symbol reason) :color 15158332)))
            nil)))))

(defun get-risk-summary ()
  "Get current risk state summary for monitoring"
  (format nil "D:~d C:~d V:~a P:~,0f" 
          (if (boundp '*danger-level*) *danger-level* 0)
          (if (boundp '*consecutive-losses*) *consecutive-losses* 0)
          (if (boundp '*current-volatility-state*) *current-volatility-state* :?)
          (if (boundp '*daily-pnl*) *daily-pnl* 0)))

;;; ==========================================
;;; MIGRATED FROM engine/risk.lisp (Via Negativa)
;;; ==========================================

;; Hard Constraints (Taleb's Rules)
(defparameter *max-daily-loss-percent* 1.0 "Hard stop: Max 1% equity loss per day")
(defparameter *max-leverage* 10.0 "Hard stop: Max leverage")
(defparameter *ruin-probability-threshold* 0.001 "Max acceptable ruin probability")
(defparameter *risk-state-lock* (bt:make-lock) "Thread safety for risk updates")

(defun get-effective-risk-capital ()
  "Calculate effective capital for risk limits.
   Returns *current-equity* if valid, otherwise *min-safe-capital* with a WARNING.
   (Refactored V19.8 for Expert Panel Feedback)"
  (if (and (boundp '*current-equity*) (numberp *current-equity*) (> *current-equity* 0))
      *current-equity*
      (progn
        (when (boundp '*min-safe-capital*)
          (format t "[RISK] ⚠️ EQUITY UNKNOWN OR ZERO. Using Conservative Fallback: ¥~,0f~%" *min-safe-capital*)
          *min-safe-capital*))))

(defun trading-allowed-p ()
  "AUTHORITATIVE check if trading is allowed (Tiered Risk V19.8 - Equity Dynamic)."
  (bt:with-lock-held (*risk-state-lock*)
    (let ((capital (get-effective-risk-capital)))
      (unless capital
        (format t "[RISK] ⛔ CRITICAL: Capital definition missing! Trading Blocked.~%")
        (return-from trading-allowed-p nil))
        
      (cond
        ;; 0. HARD DECK (Ruin Prevention)
        ((and (boundp '*max-drawdown*) (boundp '*hard-deck-drawdown-pct*)
              (> *max-drawdown* *hard-deck-drawdown-pct*))
         (format t "[RISK] ⛔ HARD DECK HIT: DD~,2f% > ~,2f%~%" *max-drawdown* *hard-deck-drawdown-pct*)
         nil)
         
        ;; 1. Monthly Limit
        ((and (boundp '*monthly-pnl*) (boundp '*monthly-loss-limit-pct*)
              (< *monthly-pnl* 0)
              (< (/ *monthly-pnl* capital) (/ *monthly-loss-limit-pct* 100.0)))
         (format t "[RISK] 🗓️ Monthly Limit Hit: PnL ~,2f < ~a%~%" *monthly-pnl* *monthly-loss-limit-pct*)
         nil)
         
        ;; 2. Weekly Limit
        ((and (boundp '*weekly-pnl*) (boundp '*weekly-loss-limit-pct*)
              (< *weekly-pnl* 0)
              (< (/ *weekly-pnl* capital) (/ *weekly-loss-limit-pct* 100.0)))
         (format t "[RISK] 🔄 Weekly Limit Hit: PnL ~,2f < ~a%~%" *weekly-pnl* *weekly-loss-limit-pct*)
         nil)
         
        ;; 3. Daily Limit (Percentage based)
        ((and (boundp '*daily-pnl*) (boundp '*daily-loss-limit-pct*)
              (< *daily-pnl* 0)
              (< (/ *daily-pnl* capital) (/ *daily-loss-limit-pct* 100.0)))
         (format t "[RISK] 🌅 Daily Limit Hit: PnL ~,2f < ~a%~%" *daily-pnl* *daily-loss-limit-pct*)
         nil)
         
        ;; 4. Legacy Daily Fixed Limit (Fallback)
        ((and (boundp '*daily-pnl*) (boundp '*daily-loss-limit*)
              (< *daily-pnl* *daily-loss-limit*))
         (format t "[RISK] ⛔ DAILY LOSS LIMIT HIT: ¥~,0f < ¥~,0f~%" *daily-pnl* *daily-loss-limit*)
         nil)
        
        ;; 5. Resignation Logic
        ((and (fboundp 'has-resigned-p) (has-resigned-p))
         (format t "[RISK] 🏳️ RESIGNED TODAY~%")
         nil)
        
        ;; 6. Danger Level
        ((and (boundp '*danger-level*) (> *danger-level* 3))
         (format t "[RISK] ⚠️ EXTREME DANGER LEVEL: ~d~%" *danger-level*)
         nil)
        
        (t t)))))

(defun update-drawdown (pnl)
  "Update drawdown tracking. Returns current drawdown percentage."
  (bt:with-lock-held (*risk-state-lock*)
    (incf *current-equity* pnl)
    (when (> *current-equity* *peak-equity*)
      (setf *peak-equity* *current-equity*))
    
    (let ((dd (if (> *peak-equity* 0)
                  (* 100 (/ (- *peak-equity* *current-equity*) *peak-equity*))
                  0)))
      (when (> dd *max-drawdown*)
        (setf *max-drawdown* dd))
      (when (> dd *max-dd-percent*)
        (format t "[RISK] ⚠️ DRAWDOWN ALERT: ~,2f%~%" dd))
      dd)))

(defun calculate-lot-size ()
  "Calculate safe lot size based on Taleb's conservative principles (Kelly/4)."
  (let* ((win-rate (/ (max 1 *success-count*) (max 1 *total-trades*)))
         (win-loss-ratio 1.5) ; Assumed conservative R:R
         (kelly (- win-rate (/ (- 1 win-rate) win-loss-ratio)))
         (half-kelly (* 0.5 (max 0 kelly))) ; Kelly/2
         (quarter-kelly (* 0.5 half-kelly)) ; Kelly/4 (Taleb style)
         
         ;; Factors
         (dd-factor (max 0.5 (- 1.0 (/ *max-drawdown* 100))))
         (equity-factor (if (> *current-equity* 0) 
                            (min 2.0 (+ 1.0 (/ *current-equity* 1000))) 
                            1.0))
         (base (if (boundp '*base-lot-size*) *base-lot-size* 0.01)))
    
    ;; Base sizing logic
    (let ((size (max 0.01 (* base dd-factor equity-factor))))
      
      ;; Apply Kelly Constraint if valid statistics exist (>30 trades)
      (when (> *total-trades* 30)
        (setf size (* size (max 0.2 (min 1.5 (+ 0.5 quarter-kelly))))))
        
      size)))

(format t "[L] 🛡️ risk-manager.lisp loaded - Unified risk management active~%")
