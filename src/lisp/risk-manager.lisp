;; risk-manager.lisp - Consolidated Risk Management System
;; V6.13: Unified risk management from school.lisp, school-fortress.lisp, brain.lisp
;; Taleb's Antifragility + Thorp's Kelly Criterion

(in-package :swimmy.engine)

;;; ==========================================
;;; RISK MANAGER API
;;; ==========================================

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

(defun safe-order (action symbol lot sl tp &optional (magic 0))
  "Safe wrapper for placing orders via ZeroMQ. Enforces all risk checks."
  (multiple-value-bind (approved adjusted-lot reason)
      (risk-check-all symbol action lot :standard)
    (if approved
        (progn
          (log-info (format nil "RISK APPROVED: ~a ~a (Lot: ~,2f)" action symbol adjusted-lot)
                    :data (jsown:new-js 
                            ("type" "trade_approved")
                            ("action" action)
                            ("symbol" symbol)
                            ("requested_lot" lot)
                            ("adjusted_lot" adjusted-lot)
                            ("reason" reason)))
          (let ((msg (jsown:to-json 
                       (jsown:new-js 
                         ("action" action)
                         ("symbol" symbol)
                         ("lot" (read-from-string (format nil "~,2f" adjusted-lot)))
                         ("sl" sl)
                         ("tp" tp)
                         ("magic" magic)))))
            (pzmq:send *cmd-publisher* msg)
            t))
        (progn
          (log-warn (format nil "RISK DENIED: ~a ~a Reason: ~a" action symbol reason)
                    :data (jsown:new-js 
                            ("type" "trade_denied")
                            ("action" action)
                            ("symbol" symbol)
                            ("requested_lot" lot)
                            ("reason" reason)))
          (notify-discord (format nil "🛡️ Risk Manager Denied: ~a ~a (~a)" action symbol reason) :color 15158332)
          nil))))

(defun get-risk-summary ()
  "Get current risk state summary for monitoring"
  (format nil "D:~d C:~d V:~a P:~,0f" 
          (if (boundp '*danger-level*) *danger-level* 0)
          (if (boundp '*consecutive-losses*) *consecutive-losses* 0)
          (if (boundp '*current-volatility-state*) *current-volatility-state* :?)
          (if (boundp '*daily-pnl*) *daily-pnl* 0)))

(format t "[L] 🛡️ risk-manager.lisp loaded - Unified risk management active~%")
