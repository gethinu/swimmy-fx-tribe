;; risk-manager.lisp - Consolidated Risk Management System
;; V6.13: Unified risk management from school.lisp, school-fortress.lisp, brain.lisp
;; Taleb's Antifragility + Thorp's Kelly Criterion

(in-package :swimmy.engine)

;;; ==========================================
;;; RISK MANAGER API
;;; ==========================================

(defvar *risk-alert-throttle* (make-hash-table :test 'equal)
  "Timestamp of last alert for (symbol action reason) to prevent spam")

;;; ==========================================
;;; MECU: CURRENCY DECOMPOSITION & EXPOSURE
;;; ==========================================

(defun decompose-pair (symbol)
  "Split a symbol like 'USDJPY' into (BASE QUOTE) list."
  (let ((str (if (symbolp symbol) (symbol-name symbol) symbol)))
    (cond
      ((= (length str) 6)
       (list (subseq str 0 3) (subseq str 3 6)))
      ((search "JPY" str)
       (list (subseq str 0 (search "JPY" str)) "JPY"))
      (t (list str "USD"))))) ; Fallback

(defun get-price-to-jpy (currency)
  "Get conversion rate from CURRENCY to JPY (Account Currency)."
  (let ((pairs (list (format nil "~aJPY" currency) "USDJPY")))
    (block found
      (dolist (p pairs)
        (let ((price (gethash p swimmy.globals::*current-candles*)))
          (when price 
            (return-from found (swimmy.globals::candle-close price)))))
      1.0))) ; Fallback or JPY itself

(defun calculate-portfolio-exposure ()
  "Calculate aggregate currency exposure (Gross & Net Notional) as ratio of Equity.
   Returns (values exposures gross-total net-directional)."
  (let ((exposures (make-hash-table :test 'equal))
        (equity (get-effective-risk-capital))
        (gross 0.0)
        (net 0.0)
        (lot-size 100000.0)) ; Standard FX Lot
    
    (flet ((add-exposure (currency amount-jpy)
             (incf (gethash currency exposures 0.0) amount-jpy)))
      
      ;; 1. Scan Arm States (Active Positions)
      (when (boundp 'swimmy.engine::*arm-states*)
        (maphash (lambda (idx state)
                   (let ((pos (swimmy.globals::arm-state-position state))
                         (symbol (swimmy.globals::arm-state-symbol state))
                         (size (swimmy.globals::arm-state-size state)))
                     (when (and pos symbol size)
                       (let* ((parts (decompose-pair symbol))
                              (base (first parts))
                              (quote (second parts))
                              (price (get-price-to-jpy base))
                              (notional-base (* size lot-size price))
                              (dir (if (eq pos :LONG) 1.0 -1.0)))
                         ;; Long Base, Short Quote
                         (add-exposure base (* dir notional-base))
                         (add-exposure quote (* dir -1.0 notional-base))))))
                 swimmy.engine::*arm-states*))
      
      ;; 2. Scan Pending Orders (Future Exposure)
      (when (boundp 'swimmy.globals::*pending-orders*)
        (maphash (lambda (uuid entry)
                   (declare (ignore uuid))
                   (let* ((msg (third entry))
                          (symbol (jsown:val msg "symbol"))
                          (action (jsown:val msg "action"))
                          (size (jsown:val msg "volume"))
                          (parts (decompose-pair symbol))
                          (base (first parts))
                          (quote (second parts))
                          (price (get-price-to-jpy base))
                          (notional-base (* size lot-size price))
                          (dir (if (string= action "BUY") 1.0 -1.0)))
                     (add-exposure base (* dir notional-base))
                     (add-exposure quote (* dir -1.0 notional-base))))
                 swimmy.globals::*pending-orders*)))

    ;; 3. Aggregate totals normalized by Equity
    (maphash (lambda (curr val-jpy)
               (let ((ratio (/ val-jpy equity)))
                 (setf (gethash curr exposures) ratio)
                 (incf gross (abs ratio))
                 (incf net ratio)))
             exposures)
    
    (values exposures gross net)))

(defun risk-check-all (symbol direction lot category)
  "Unified risk check before trade execution (MECU Integrated). 
   Returns (values approved-p adjusted-lot reason)"
  (let ((checks nil)
        (final-lot lot)
        (approved t))
    
    ;; 0. MECU HARD GATES (Highest Priority)
    (multiple-value-bind (exposures gross net) (calculate-portfolio-exposure)
      (let* ((parts (decompose-pair symbol))
             (base (first parts))
             (quote (second parts))
             (price (get-price-to-jpy base))
             (equity (get-effective-risk-capital))
             (candidate-notional (* lot 100000.0 price))
             (candidate-ratio (/ candidate-notional equity))
             (dir (if (eq direction :BUY) 1.0 -1.0)))
        
        ;; A. Gross Exposure Gate
        (when (> (+ gross (abs (* 2.0 candidate-ratio))) (or (bound-and-true-p 'swimmy.school::*max-gross-exposure-pct*) 0.30))
          (push "MECU_GROSS_EXCEEDED" checks)
          (setf approved nil))
        
        ;; B. Net Directional Gate
        (when (> (abs (+ net (* dir candidate-ratio))) (or (bound-and-true-p 'swimmy.school::*max-net-exposure-pct*) 0.15))
          (push "MECU_NET_EXCEEDED" checks)
          (setf approved nil))
          
        ;; C. Single Currency Gate (Base & Quote)
        (let ((base-after (+ (gethash base exposures 0.0) (* dir candidate-ratio)))
              (quote-after (+ (gethash quote exposures 0.0) (* dir -1.0 candidate-ratio)))
              (limit (or (bound-and-true-p 'swimmy.school::*max-currency-exposure-pct*) 0.10)))
          (when (or (> (abs base-after) limit) (> (abs quote-after) limit))
            (push "MECU_CURRENCY_LIMIT" checks)
            (setf approved nil)))))

    ;; 1. PRIORITY GUARD: Dynamic Drawdown Check (Hard Stop at 25%)
    (when (and (boundp 'swimmy.globals::*monitoring-drawdown*) 
               (>= swimmy.globals::*monitoring-drawdown* 25.0))
      (push "DYNAMIC_DD_LIMIT_25%" checks)
      (setf approved nil))

    ;; 2. Danger/Resignation checks
    (when (and (fboundp 'danger-cooldown-active-p) (danger-cooldown-active-p))
      (push "DANGER_COOLDOWN" checks) (setf approved nil))
    (when (and (boundp 'swimmy.school::*has-resigned-today*) swimmy.school::*has-resigned-today*)
      (push "RESIGNED" checks) (setf approved nil))
    
    ;; 3. SIZING PHASE (If still approved)
    (when approved
      ;; Apply Regime Sizing
      (when (and (boundp 'swimmy.globals::*volatility-regime*)
                 (eq swimmy.globals::*volatility-regime* :high))
        (setf final-lot (* final-lot 0.5))
        (push "HIGH_VOL_SIZING" checks))
      
      ;; Apply Correlation/Exposure reductions (Legacy Layer)
      (let ((corr-risk (check-correlation-risk symbol direction)))
        (when (> corr-risk 0.95)
          (push "HIGH_CORRELATION" checks)
          (setf final-lot (* final-lot 0.5)))))

    ;; 4. MIN/MAX CAPS
    (setf final-lot (max 0.01 (min 0.50 final-lot)))

    ;; 5. NOTIFY MECU STATUS (If denied)
    (unless approved
      (notify-mecu-snapshot symbol direction lot (format nil "~{~a~^, ~}" checks)))

    (values approved final-lot (if checks (format nil "~{~a~^, ~}" checks) "APPROVED"))))

(defun bound-and-true-p (sym)
  (if (and (boundp sym) (symbol-value sym)) (symbol-value sym) nil))

(defun notify-mecu-snapshot (symbol direction lot reason)
  "Log current exposure snapshot to Discord when MECU blocks a trade."
  (multiple-value-bind (exposures gross net) (calculate-portfolio-exposure)
    (let ((msg (format nil "🛡️ **MECU BLOCK: ~a ~a (~,2f lot)**~%**Reason:** ~a~%~%**Current Portfolio:**~%• Gross Exposure: ~,2f% (Limit: ~a%)~%• Net Directional: ~,2f% (Limit: ~a%)~%~%**Currency Breakdown:**~%"
                       direction symbol lot reason
                       (* 100 gross) (or (bound-and-true-p 'swimmy.school::*max-gross-exposure-pct*) 30)
                       (* 100 net) (or (bound-and-true-p 'swimmy.school::*max-net-exposure-pct*) 15))))
      (maphash (lambda (curr ratio)
                 (setf msg (concatenate 'string msg (format nil "  - ~a: ~,1f%~%" curr (* 100 ratio)))))
               exposures)
      (when (fboundp 'swimmy.core:notify-discord)
        (swimmy.core:notify-discord msg :color 15158332)))))

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
                  (let* ((order-obj (make-order-message 
                                      "RiskManager" ;; Default Strategy ID
                                      symbol 
                                      action 
                                      adjusted-lot 
                                      0.0 ;; Price 0 for market
                                      sl tp 
                                      :magic magic 
                                      :comment (or comment "")))
                         (uuid (swimmy.core:sexp-alist-get order-obj "id"))
                         (msg-str (swimmy.core:encode-sexp order-obj)))
                    
                    ;; Retry Logic (Phase 7): Store pending order
                    (when (boundp '*pending-orders*)
                      (setf (gethash uuid *pending-orders*) 
                            (list (get-universal-time) 0 order-obj)))

                    (pzmq:send *cmd-publisher* msg-str)
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
