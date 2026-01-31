;;; school-adaptation.lisp - Context-Aware Strategy Adaptation Engine
;;; Phase 4 Implementation (memo3.txt Section 9)

(in-package :swimmy.school)

;;; ============================================================================
;;; ADAPTATION ENGINE
;;; ============================================================================
;;; This module implements the "Adaptation Map" which allows strategies to
;;; dynamically adjust their weight/confidence based on specific market regimes.
;;; 
;;; Key Concepts:
;;; - Adapt Vector: A mapping of Regime -> Score stored in the strategy struct.
;;; - EWMA Edge: Exponentially Weighted Moving Average of returns per regime.
;;; - Context Awareness: Strategies learn WHERE they work, not just THAT they work.

;;; ============================================================================
;;; BAYESIAN ADAPTATION LOGIC (Phase 12)
;;; ============================================================================

(defstruct posterior
  (alpha 1.0)  ; Beta distribution Alpha (Wins + Prior)
  (beta 1.0)   ; Beta distribution Beta (Losses + Prior)
  (mu 0.0)     ; Normal distribution Mean (Exp Return)
  (lambda 1.0) ; Precision (Confidence in Mean)
  (n 0))       ; Observation count

(defun update-bayesian-posterior (posterior pnl)
  "Update Bayesian posterior beliefs based on new PnL observation.
   - Win Rate: Beta-Binomial conjugate update.
   - Return: Normal-Normal conjugate update (Simplified with fixed variance assumption)."
  (let* ((win (if (> pnl 0) 1 0))
         (loss (if (<= pnl 0) 1 0))
         ;; Update Beta params
         (new-alpha (+ (posterior-alpha posterior) win))
         (new-beta (+ (posterior-beta posterior) loss))
         ;; Update Normal params (Recursive mean update)
         (n (posterior-n posterior))
         (new-n (+ n 1))
         (old-mu (posterior-mu posterior))
         ;; Online Mean Update: mu_new = mu_old + (x - mu_old)/n
         (new-mu (+ old-mu (/ (- pnl old-mu) new-n))))
    
    (setf (posterior-alpha posterior) new-alpha)
    (setf (posterior-beta posterior) new-beta)
    (setf (posterior-mu posterior) new-mu)
    (setf (posterior-n posterior) new-n)
    posterior))

(defun get-bayesian-edge-score (posterior)
  "Calculate lower-bound edge score (conservative estimate).
   Score = (WinRate_LowerBound * AvgWin) - (LossRate_UpperBound * AvgLoss)"
  (if posterior
      (let* ((a (posterior-alpha posterior))
             (b (posterior-beta posterior))
             (total (+ a b))
             ;; Mean Win Rate (Expected Value)
             (win-rate (/ a total))
             ;; Approximate 95% Confidence Interval width for Beta ~ 2 * sqrt(p(1-p)/n)
             (std-dev (sqrt (/ (* win-rate (- 1.0 win-rate)) (+ total 1.0))))
             (lower-win-rate (max 0.0 (- win-rate (* 1.65 std-dev)))) ;; 90% CI Lower
             
             (mu (posterior-mu posterior)))
        
        ;; Simplified Score: Expected Return * Confidence Factor
        ;; If we have few samples, std-dev is high, lower-win-rate is low.
        ;; We return a normalized score -1.0 to 1.0
        (let ((raw-score (* lower-win-rate mu)))
             (max -1.0 (min 1.0 (/ raw-score 10.0))))) ;; Normalize: 10 pips avg per trade = 1.0 score
      0.0))

(defun ensure-adapt-vector (strategy)
  "Ensure strategy has a valid hash table for adapt-vector, using Posterior structs"
  (unless (hash-table-p (strategy-adapt-vector strategy))
    (setf (strategy-adapt-vector strategy) (make-hash-table :test 'eq))))

(defun get-regime-key (context)
  "Normalize a context object into a regime keyword."
  (cond
    ((keywordp context) context)
    ((and (listp context) (getf context :regime)) (getf context :regime))
    ((and (hash-table-p context) (gethash :regime context)) (gethash :regime context))
    (t :unknown)))

(defun update-adaptation-weights (strategy context pnl)
  "Update the adaptation vector using Bayesian Logic."
  (when strategy
    (ensure-adapt-vector strategy)
    (let* ((regime (get-regime-key context))
           (table (strategy-adapt-vector strategy))
           (posterior (gethash regime table)))
      
      ;; Initialize if NIL
      (unless posterior
        (setf posterior (make-posterior))
        (setf (gethash regime table) posterior))
      
      ;; Update Beliefs
      (update-bayesian-posterior posterior pnl)
      
      (format t "[ADAPT] 🧠 Bayesian Update for ~a (~a): WinRate ~,2f (n=~d) EstReturn: ~,2f~%" 
              (strategy-name strategy) regime 
              (/ (posterior-alpha posterior) (+ (posterior-alpha posterior) (posterior-beta posterior)))
              (posterior-n posterior)
              (posterior-mu posterior)))))

(defun get-adaptation-multiplier (strategy context)
  "Get the adaptation multiplier based on Bayesian Confidence."
  (if (and strategy (hash-table-p (strategy-adapt-vector strategy)))
      (let* ((regime (get-regime-key context))
             (posterior (gethash regime (strategy-adapt-vector strategy))))
        (if posterior
            (let ((score (get-bayesian-edge-score posterior)))
               ;; Multiplier 0.5 to 2.0 based on score
               (max 0.5 (min 2.0 (+ 1.0 score))))
            1.0))
      1.0))
