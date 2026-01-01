;; school-constitution.lisp - The Supreme Law of Swimmy
;; V7.0: "The Dual Monarchy" - Lisp governs Rust
;; Defines the fundamental rules that cannot be broken by any strategy.

(defpackage :swimmy.constitution
  (:use :cl :jsown)
  (:export :judge-action
           :*max-daily-loss*
           :*min-equity-threshold*))

(in-package :swimmy.constitution)

;;; ============================================================
;;; CONSTITUTIONAL PARAMETERS
;;; ============================================================

(defparameter *max-daily-loss* 50000)       ;; 1日の最大損失許容額 (-50,000 JPY)
(defparameter *min-equity-threshold* 900000) ;; 絶対防衛ライン (口座残高)

;; Shared state references (expected to be updated by server.lisp or school-state.lisp)
(defvar *current-equity* 1000000)
(defvar *daily-start-equity* 1000000)

;;; ============================================================
;;; JUDICIAL BRANCH (審査部門)
;;; ============================================================

(defun validate-contract (proposal)
  "Graham's Fix: Strict Schema Validation.
   Returns T if proposal meets the contract, NIL otherwise."
  (and (jsown:keyp proposal "action")
       (jsown:keyp proposal "agent")
       ;; If PnL/Equity are missing, we can default, but let's encourage strictness
       t))

(defun judge-action (proposal)
  "Review a proposed action against the Constitution.
   Returns (values VERDICT REASON)"
  
  ;; Graham Fix: Schema Contract
  (unless (validate-contract proposal)
    (return-from judge-action 
      (values "REJECTED" "Constitution Violation: Invalid JSON Contract (Graham's Strictness)")))

  (let* ((action (jsown:val proposal "action"))
         (pnl (if (jsown:keyp proposal "pnl") (jsown:val proposal "pnl") 0))
         (current-dd (- *daily-start-equity* *current-equity*)))
    
    ;; Update equity state if provided (for simulation/tracking)
    (when (jsown:keyp proposal "equity")
      (setf *current-equity* (jsown:val proposal "equity")))
    
    (cond
      ;; Article 1: solvency (破産防止)
      ((< *current-equity* *min-equity-threshold*)
       (values "REJECTED" 
               (format nil "Article 1 Violation: Equity ~A < Threshold ~A" 
                       *current-equity* *min-equity-threshold*)))
      
      ;; Article 2: Fixed Loss Limit (Taleb's Fix)
      ;; We reject the "House Money" theory. Risk limits are absolute.
      ((> current-dd *max-daily-loss*)
       (if (member action '("BUY" "SELL" "ENTRY") :test #'string=)
           (values "REJECTED" 
                   (format nil "Article 2 Violation: Daily Loss ~A > Limit ~A (Risk is absolute)" 
                           current-dd *max-daily-loss*))
           (values "APPROVED" "Emergency Exit Allowed")))

      ;; Article 3: Smart Regime Check (Graham's Logic)
      ;; If market is RANGING, reject TREND strategies.
      ;; If market is TRENDING, reject MEAN-REVERSION strategies.
      (t
       (let ((regime (if (jsown:keyp proposal "regime") (jsown:val proposal "regime") "UNKNOWN"))
             (strategy-type (if (jsown:keyp proposal "strategy_type") (jsown:val proposal "strategy_type") "UNKNOWN")))
         
         (cond
           ;; Case A: Ranging Market vs Trend Strategy
           ((and (string= regime "RANGING") (string= strategy-type "TREND"))
            (values "REJECTED" "Article 3: Trend Strategy forbidden in Ranging Market"))
           
           ;; Case B: Trending Market vs Oscillator Strategy
           ((and (string= regime "TRENDING") (string= strategy-type "MEAN_REVERSION"))
            (values "REJECTED" "Article 3: Mean Reversion forbidden in Trending Market"))
           
           ;; Article 4: Profit Lock-in (Using pnl variable)
           ;; If we have unrealized profit > 20000, tighten new entry requirements
           ((and (> pnl 20000) 
                 (member action '("BUY" "SELL" "ENTRY") :test #'string=)
                 (> current-dd (* *max-daily-loss* 0.5)))
            (values "REJECTED" 
                    (format nil "Article 4: Profit Lock-in ~A - DD ~A exceeds 50% limit when protecting gains" 
                            pnl current-dd)))
           
           ;; Constitution Upheld
           (t
            (values "APPROVED" 
                    (format nil "Constitution Respected. DD: ~A < Limit: ~A | PnL: ~A" 
                            current-dd *max-daily-loss* pnl))))))))

(format t "[L] 📜 school-constitution.lisp loaded - The Law is Active.~%")
