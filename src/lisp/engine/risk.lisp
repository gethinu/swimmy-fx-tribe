;;; ============================================================================
;;; engine/risk.lisp - Risk Management Authority
;;; ============================================================================
;;; "Ruin Avoidance" - Taleb's Hard Constraints
;;; Part of "The Efficient Gardener" refactoring (Phase 5)
;;;
;;; Centralizes all risk logic:
;;; - Loss limits (Daily/Total)
;;; - Drawdown tracking
;;; - Position sizing (Kelly/4)
;;; - Hard stops (Guardian rules)
;;; ============================================================================

(in-package :swimmy.engine)

;;; ==========================================
;;; RISK PARAMETERS
;;; ==========================================
;;; Defined in config.lisp, typically, but logic is here.
;;; We expect these to be bound globally.

;; Hard Constraints (Taleb's Rules)
(defparameter *max-daily-loss-percent* 1.0 "Hard stop: Max 1% equity loss per day")
(defparameter *max-leverage* 10.0 "Hard stop: Max leverage")
(defparameter *ruin-probability-threshold* 0.001 "Max acceptable ruin probability")
(defparameter *risk-tolerance* :conservative "Risk tolerance: :conservative, :moderate, :aggressive")

;; State
(defparameter *risk-state-lock* (bt:make-lock) "Thread safety for risk updates")

;;; ==========================================
;;; CORE RISK CHECKS
;;; ==========================================

(defun trading-allowed-p ()
  "AUTHORITATIVE check if trading is allowed."
  (bt:with-lock-held (*risk-state-lock*)
    (cond
      ;; 1. Daily Loss Limit
      ((< *daily-pnl* *daily-loss-limit*)
       (format t "[RISK] ⛔ DAILY LOSS LIMIT HIT: ¥~,0f < ¥~,0f~%" 
               *daily-pnl* *daily-loss-limit*)
       nil)
      
      ;; 2. Max Drawdown Hard Stop
      ((> *max-drawdown* *max-dd-percent*)
       (format t "[RISK] ⛔ MAX DRAWDOWN HIT: ~,2f% > ~,2f%~%" 
               *max-drawdown* *max-dd-percent*)
       nil)
      
      ;; 3. Resignation Logic (Psychological Stop)
      ((and (fboundp 'has-resigned-p) (has-resigned-p))
       (format t "[RISK] 🏳️ RESIGNED TODAY~%")
       nil)
      
      ;; 4. Danger Level (Market Conditions)
      ((and (boundp '*danger-level*) (> *danger-level* 3))
       (format t "[RISK] ⚠️ EXTREME DANGER LEVEL: ~d~%" *danger-level*)
       nil)
      
      (t t))))

(defun check-hard-constraints (proposed-lot-size symbol)
  "Check strict constraints for a specific trade proposal."
  (let ((margin-req (* proposed-lot-size 100000 0.04))) ; Approx 25x max leverage allowed by broker
    (cond
      ;; Leverage check
      ((> margin-req *current-equity*)
       (format t "[RISK] ❌ INSUFFICIENT MARGIN: Req ¥~,0f > Eq ¥~,0f~%" 
               margin-req *current-equity*)
       nil)
      
      ;; Max Total Exposure Check
      ((and (boundp '*max-total-exposure*)
            (> (+ margin-req (calculate-current-exposure)) *max-total-exposure*))
       (format t "[RISK] ❌ MAX EXPOSURE EXCEEDED~%")
       nil)
       
      (t t))))

(defun calculate-current-exposure ()
  "Calculate current total exposure (placeholder)."
  ;; Needs connection to position list. For now assume 0 if not tracked.
  ;; In Phase 6 (trading split), position tracking will be robust.
  0.0)

;;; ==========================================
;;; RISK METRICS UPDATE
;;; ==========================================

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
      
      ;; Log warning if crossing threshold
      (when (> dd *max-dd-percent*)
        (format t "[RISK] ⚠️ DRAWDOWN ALERT: ~,2f%~%" dd))
      
      dd)))

;;; ==========================================
;;; POSITION SIZING (Kelly/4)
;;; ==========================================

(defun calculate-lot-size ()
  "Calculate safe lot size based on Taleb's conservative principles (Kelly/4)."
  (let* ((win-rate (/ (max 1 *success-count*) (max 1 *total-trades*)))
         (win-loss-ratio 1.5) ; Assumed conservative R:R, explicitly conservative
         (kelly (- win-rate (/ (- 1 win-rate) win-loss-ratio)))
         (half-kelly (* 0.5 (max 0 kelly))) ; Kelly/2
         (quarter-kelly (* 0.5 half-kelly)) ; Kelly/4 (Taleb style)
         
         ;; Factors
         (dd-factor (max 0.5 (- 1.0 (/ *max-drawdown* 100))))
         (equity-factor (if (> *current-equity* 0) 
                            (min 2.0 (+ 1.0 (/ *current-equity* 1000))) 
                            1.0)))
    
    ;; Base sizing logic
    (let ((size (max 0.01 (* *base-lot-size* dd-factor equity-factor))))
      
      ;; Apply Kelly Constraint if valid statistics exist (>30 trades)
      (when (> *total-trades* 30)
        ;; Convert Kelly % to lots (approximate: 1 lot = 100k units ~ 10M yen / 25 lev = 400k margin)
        ;; Simplified: Modulate *base-lot-size* by Kelly factor
        (setf size (* size (max 0.2 (min 1.5 (+ 0.5 quarter-kelly))))))
        
      size)))

;;; ==========================================
;;; DAILY TARGET & LIMITS (From Goals)
;;; ==========================================

(defun calculate-daily-risk-limit (daily-target risk-tolerance)
  "Pure function to decide risk limit based on target and tolerance."
  (case risk-tolerance
    (:conservative (ceiling (* daily-target 0.5)))   ; Risk 50% of target
    (:moderate     (ceiling (* daily-target 1.0)))   ; Risk 100% of target  
    (:aggressive   (ceiling (* daily-target 1.5)))   ; Risk 150% of target
    (otherwise     daily-target)))

(defun set-daily-loss-limit-from-target (daily-target)
  "Update the global loss limit based on today's target."
  (setf *daily-loss-limit* 
        (- (calculate-daily-risk-limit daily-target *risk-tolerance*)))
  (format t "[RISK] 🛡️ Daily Loss Limit Set: ¥~d (Tolerance: ~a)~%" 
          *daily-loss-limit* *risk-tolerance*))

(defun get-risk-summary ()
  "Get summary of risk state"
  (format nil "Risk Status: Drawdown ~,2f% (Max ~,2f%), Daily PnL ¥~,0f (Limit ¥~,0f)"
          *max-drawdown* *max-dd-percent* *daily-pnl* *daily-loss-limit*))

(format t "[ENGINE] risk.lisp loaded (Taleb Rules Active)~%")
