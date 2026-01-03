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

(defparameter *max-daily-loss-base* 50000)  ;; Base limit (1日の最大損失許容額)
(defparameter *max-daily-loss* 50000)       ;; Dynamic limit (adjusted by volatility)
(defparameter *min-equity-threshold* 900000) ;; 絶対防衛ライン (口座残高)

;; V7.7++: Taleb Critique #4 - Volatility-based adjustment
(defparameter *current-volatility* 1.0)      ;; Normalized volatility (1.0 = normal)
(defparameter *volatility-multiplier-low* 1.5)  ;; Low vol = can risk more
(defparameter *volatility-multiplier-high* 0.5) ;; High vol = risk less

;; V7.9: Graham Critique #1 - Article 4 Parameters (No magic numbers)
;; Rationale for 40%: We want to lock profits BEFORE we hit 50% of risk budget.
;; Starting at 40% gives us a buffer zone (40-50%) to react before hitting the hard limit.
;; This is a conservative heuristic: lock earlier rather than later.
(defparameter *profit-lock-threshold-ratio* 0.4
  "Ratio of max-daily-loss at which profit lock-in activates. 
   40% chosen to provide 10% buffer before 50% limit (Graham's heuristic).")

;; Rationale for 50%: If you've made profit, never give back more than half.
;; This follows the 'trailing stop' principle - protect your gains.
(defparameter *profit-protection-ratio* 0.5
  "Ratio of unrealized profit to protect. 50% = never give back more than half your gains.")

;; Rationale for 50% of loss limit: Even when protecting profits, 
;; we never allow DD to exceed half the daily loss limit.
(defparameter *max-dd-protection-ratio* 0.5
  "Maximum DD as ratio of max-daily-loss when profit protection is active.")

;; Shared state references (expected to be updated by server.lisp or school-state.lisp)
(defvar *current-equity* 1000000)
(defvar *daily-start-equity* 1000000)

(defun update-dynamic-loss-limit (volatility)
  "V7.7++ Update max-daily-loss based on current volatility.
   Low volatility (< 0.5) = aggressive, allow 1.5x base loss
   Normal volatility (0.5-1.5) = standard base loss
   High volatility (> 1.5) = conservative, allow 0.5x base loss"
  (setf *current-volatility* volatility)
  (let ((multiplier (cond
                      ((< volatility 0.5) *volatility-multiplier-low*)
                      ((> volatility 1.5) *volatility-multiplier-high*)
                      (t 1.0))))
    (setf *max-daily-loss* (floor (* *max-daily-loss-base* multiplier)))
    (format t "[C] 📊 DYNAMIC CONSTITUTION: Vol=~,2f → Loss Limit=~a (×~,1f)~%"
            volatility *max-daily-loss* multiplier)
    *max-daily-loss*))

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
           
           ;; Article 4: Profit Lock-in (Graham Critique #1++: Configurable parameters)
           ;; V7.9+: Uses defparameters for transparency and adjustability
           (t
            (let* ((profit-lock-threshold (* *max-daily-loss* *profit-lock-threshold-ratio*))
                  (max-dd-while-protecting (if (> pnl 0) 
                                               (min (* pnl *profit-protection-ratio*)
                                                    (* *max-daily-loss* *max-dd-protection-ratio*))
                                               *max-daily-loss*)))
              (if (and (> pnl profit-lock-threshold)
                       (member action '("BUY" "SELL" "ENTRY") :test #'string=)
                       (> current-dd max-dd-while-protecting))
                  (values "REJECTED"
                          (format nil "Article 4 (Graham): Profit Lock-in ~A. DD ~A > ~,1f%% of gains (~A)"
                                  pnl current-dd (* 100 *profit-protection-ratio*) max-dd-while-protecting))
                  ;; Constitution Upheld
                  (values "APPROVED" 
                          (format nil "Constitution Respected. DD: ~A < Limit: ~A | PnL: ~A" 
                                  current-dd *max-daily-loss* pnl)))))))))))

(format t "[L] 📜 school-constitution.lisp loaded - The Law is Active.~%")

;;; ============================================================
;;; V7.9+: UNIT TESTS (Graham Critique #1: Test your business logic)
;;; ============================================================

(defun test-article-4 ()
  "Unit tests for Article 4 profit lock-in logic"
  (format t "~%🧪 Running Article 4 Unit Tests...~%")
  (let ((passed 0) (failed 0))
    
    ;; Test 1: No profit, no lock-in (should APPROVE)
    (let* ((proposal (jsown:new-js ("action" "BUY") ("agent" "test") ("pnl" 0)))
           (verdict (judge-action proposal)))
      (if (string= verdict "APPROVED")
          (progn (incf passed) (format t "  ✅ Test 1: No profit → APPROVED~%"))
          (progn (incf failed) (format t "  ❌ Test 1: Expected APPROVED, got ~A~%" verdict))))
    
    ;; Test 2: High profit, low DD (should APPROVE)
    (setf *max-daily-loss* 50000)
    (setf *current-equity* 1020000)
    (setf *daily-start-equity* 1000000)  ;; DD = -20000 (we're UP)
    (let* ((proposal (jsown:new-js ("action" "BUY") ("agent" "test") ("pnl" 30000)))
           (verdict (judge-action proposal)))
      (if (string= verdict "APPROVED")
          (progn (incf passed) (format t "  ✅ Test 2: High profit, no DD → APPROVED~%"))
          (progn (incf failed) (format t "  ❌ Test 2: Expected APPROVED, got ~A~%" verdict))))
    
    ;; Test 3: High profit, high DD (should REJECT - protecting gains)
    (setf *current-equity* 980000)
    (setf *daily-start-equity* 1000000)  ;; DD = 20000
    (let* ((proposal (jsown:new-js ("action" "BUY") ("agent" "test") ("pnl" 30000)))
           (verdict (judge-action proposal)))
      (if (string= verdict "REJECTED")
          (progn (incf passed) (format t "  ✅ Test 3: High profit, high DD → REJECTED (lock-in)~%"))
          (progn (incf failed) (format t "  ❌ Test 3: Expected REJECTED, got ~A~%" verdict))))
    
    ;; Test 4: EXIT action should always be allowed
    (let* ((proposal (jsown:new-js ("action" "EXIT") ("agent" "test") ("pnl" 30000)))
           (verdict (judge-action proposal)))
      (if (string= verdict "APPROVED")
          (progn (incf passed) (format t "  ✅ Test 4: EXIT action → APPROVED~%"))
          (progn (incf failed) (format t "  ❌ Test 4: Expected APPROVED, got ~A~%" verdict))))
    
    ;; Summary
    (format t "~%📊 Article 4 Tests: ~A passed, ~A failed~%" passed failed)
    (= failed 0)))

(defun test-validate-contract ()
  "Unit tests for validate-contract schema validation (Graham's fix)"
  (format t "~%🧪 Running validate-contract Unit Tests...~%")
  (let ((passed 0) (failed 0))
    
    ;; Test 1: Valid contract with action and agent
    (let ((valid-proposal (jsown:new-js ("action" "BUY") ("agent" "test"))))
      (if (validate-contract valid-proposal)
          (progn (incf passed) (format t "  ✅ Test 1: Valid contract → T~%"))
          (progn (incf failed) (format t "  ❌ Test 1: Expected T for valid contract~%"))))
    
    ;; Test 2: Missing action field
    (let ((no-action (jsown:new-js ("agent" "test"))))
      (if (not (validate-contract no-action))
          (progn (incf passed) (format t "  ✅ Test 2: Missing action → NIL~%"))
          (progn (incf failed) (format t "  ❌ Test 2: Expected NIL for missing action~%"))))
    
    ;; Test 3: Missing agent field
    (let ((no-agent (jsown:new-js ("action" "BUY"))))
      (if (not (validate-contract no-agent))
          (progn (incf passed) (format t "  ✅ Test 3: Missing agent → NIL~%"))
          (progn (incf failed) (format t "  ❌ Test 3: Expected NIL for missing agent~%"))))
    
    ;; Test 4: Empty proposal
    (let ((empty (jsown:new-js)))
      (if (not (validate-contract empty))
          (progn (incf passed) (format t "  ✅ Test 4: Empty proposal → NIL~%"))
          (progn (incf failed) (format t "  ❌ Test 4: Expected NIL for empty proposal~%"))))
    
    ;; Summary
    (format t "~%📊 validate-contract Tests: ~A passed, ~A failed~%" passed failed)
    (= failed 0)))

(defun run-all-constitution-tests ()
  "Run all constitution tests. Returns T if all pass."
  (format t "~%═══════════════════════════════════════════════~%")
  (format t "🏛️ CONSTITUTION TEST SUITE (Graham's CI Integration)~%")
  (format t "═══════════════════════════════════════════════~%")
  (let ((all-passed t))
    (unless (test-article-4) (setf all-passed nil))
    (unless (test-validate-contract) (setf all-passed nil))
    (format t "~%═══════════════════════════════════════════════~%")
    (if all-passed
        (format t "✅ ALL TESTS PASSED~%")
        (format t "❌ SOME TESTS FAILED~%"))
    (format t "═══════════════════════════════════════════════~%")
    all-passed))

;; CI Integration Note (Graham):
;; To run tests from command line:
;; sbcl --load school-constitution.lisp --eval '(run-all-constitution-tests)' --quit
;; Or add to Makefile/CI pipeline
