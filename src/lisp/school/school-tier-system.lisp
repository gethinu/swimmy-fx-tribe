;;; src/lisp/school/school-tier-system.lisp
;;; ============================================================================
;;; THE PROVING GROUNDS: SELECTION ENGINE (Battlefield Logic)
;;; ============================================================================
;;; Implements the "Tier System" mandated by the Expert Panel (2026-01-18).
;;; 
;;; TIERS:
;;; 1. :incubator - Freshly hunted/bred strategies. Optimize parameters here.
;;; 2. :training - Passed backtest. Paper trading / Forward testing.
;;; 3. :battlefield - THE ELITE. Only Top 4 per category allowed. Real money.
;;; 4. :graveyard - Failed strategies. Retained for record but never executed.
;;; ============================================================================

(in-package :swimmy.school)

;;; V26: 3-Step Ladder System (Expert Panel Request)
;;; 1. :incubator - Freshly born. Defaults here.
;;; 2. :selection - Surviving (Sharpe >= 0.1)
;;; 3. :training - Growth (Sharpe >= 0.5)
;;; 4. :battlefield - Elite (Sharpe >= 1.0)


(defparameter *battlefield-quota* 4 "Maximum number of strategies allowed in Battlefield per category")

(defun get-strategies-by-tier (tier &optional category)
  "Get all strategies in a specific tier, optionally filtered by category."
  (let ((candidates (remove-if-not 
                      (lambda (s) (eq (strategy-tier s) tier))
                      *strategy-knowledge-base*)))
    (if category
        (remove-if-not (lambda (s) (eq (strategy-category s) category)) candidates)
        candidates)))

(defun promote (strategy new-tier reason)
  "Promote a strategy to a higher tier."
  (let ((old-tier (strategy-tier strategy)))
    (setf (strategy-tier strategy) new-tier)
    ;; V23: Auto-promote to :veteran if entering battlefield (Fixes Execution Block)
    (when (eq new-tier :battlefield)
      (setf (strategy-rank strategy) :veteran)
      (format t "[SELECTOR] RANK UP: ~a is now a VETERAN!~%" (strategy-name strategy)))
    (format t "[SELECTOR] PROMOTE: ~a (~a -> ~a) | Reason: ~a~%" 
            (strategy-name strategy) old-tier new-tier reason)))

(defun demote (strategy new-tier reason)
  "Demote a strategy to a lower tier."
  (let ((old-tier (strategy-tier strategy)))
    (setf (strategy-tier strategy) new-tier)
    (format t "[SELECTOR] DEMOTE: ~a (~a -> ~a) | Reason: ~a~%" 
            (strategy-name strategy) old-tier new-tier reason)))

;;; ----------------------------------------------------------------------------
;;; BATTLE ROYALE (The Selection Algorithm)
;;; ----------------------------------------------------------------------------

(defun run-battle-royale (category)
  "Run the selection process for a specific category.
   1. Identify current Battlefield Warriors (Defenders).
   2. Identify Top Training Graduates (Challengers).
   3. Force substitution if Challenger > Weakest Defender."
  
  (format t "[SELECTOR] BATTLE ROYALE START: Category ~a~%" category)
  
  (let* ((defenders (get-strategies-by-tier :battlefield category))
         (challengers (get-strategies-by-tier :training category))
         ;; Sort Defenders by Sharpe (Weakest first)
         (sorted-defenders (sort (copy-list defenders) #'< :key (lambda (s) (or (strategy-sharpe s) -999.0))))
         ;; Sort Challengers by Sharpe (Strongest first)
         (sorted-challengers (sort (copy-list challengers) #'> :key (lambda (s) (or (strategy-sharpe s) -999.0)))))

    ;; 1. Fill Empty Slots
    (let ((slots-remaining (- *battlefield-quota* (length defenders))))
      (when (> slots-remaining 0)
        (format t "[SELECTOR] Filling ~d empty slots...~%" slots-remaining)
        (loop for i from 0 below slots-remaining
              for chal in sorted-challengers
              do (promote chal :battlefield "Empty Slot Fill")
                 (setf sorted-challengers (remove chal sorted-challengers)))))

    ;; 2. Challenge the Weakest
    ;; Re-fetch defenders after fill
    (setf defenders (get-strategies-by-tier :battlefield category))
    (setf sorted-defenders (sort (copy-list defenders) #'< :key (lambda (s) (or (strategy-sharpe s) -999.0))))
    
    (dolist (chal sorted-challengers)
      (let ((weakest-defender (first sorted-defenders)))
        (when weakest-defender
          (let ((chal-score (or (strategy-sharpe chal) 0))
                (def-score (or (strategy-sharpe weakest-defender) 0)))
            
            ;; Challenge Condition: Significant improvement required (buffer)
            (if (> chal-score (+ def-score 0.1)) 
                (progn
                  (format t "[SELECTOR] KNOCKOUT! ~a (S=~,2f) defeats ~a (S=~,2f)~%"
                          (strategy-name chal) chal-score (strategy-name weakest-defender) def-score)
                  
                  ;; Swap
                  (demote weakest-defender :training "Lost Battle Royale")
                  (promote chal :battlefield "Won Battle Royale")
                  
                  ;; Remove from lists to prevent double swap in one run
                  (setf sorted-defenders (rest sorted-defenders)))
                
                (format t "[SELECTOR] ~a (S=~,2f) defends against ~a (S=~,2f)~%"
                        (strategy-name weakest-defender) def-score (strategy-name chal) chal-score))))))))

;;; ----------------------------------------------------------------------------
;;; MERITOCRACY (The Ladder Algorithm)
;;; ----------------------------------------------------------------------------

(defun run-meritocracy-selection ()
  "Run the 'Ladder' selection based on Absolute Thresholds.
   Moves strategies between tiers based on Sharpe Value."
  (format t "[SELECTOR] CLIMBING THE LADDER (Meritocracy Selection)...~%")
  
  (dolist (strat *strategy-knowledge-base*)
    (let ((sharpe (or (strategy-sharpe strat) 0.0))
          (tier (strategy-tier strat)))
      
      (cond 
        ;; 1. Promotion to Battlefield (The Elite)
        ((>= sharpe 1.0)
         (unless (eq tier :battlefield)
           (promote strat :battlefield "Sharpe >= 1.0 (Elite)")))
        
        ;; 2. Promotion to Training (Growth)
        ((>= sharpe 0.5)
         (cond 
           ((eq tier :battlefield) (demote strat :training "Sharpe < 1.0"))
           ((not (eq tier :training)) (promote strat :training "Sharpe >= 0.5"))))
           
        ;; 3. Promotion to Selection (Survival)
        ((>= sharpe 0.1)
         (cond
           ((eq tier :training) (demote strat :selection "Sharpe < 0.5"))
           ((eq tier :battlefield) (demote strat :selection "Sharpe < 0.5 (Crash)"))
           ((not (eq tier :selection)) (promote strat :selection "Sharpe >= 0.1"))))
           
        ;; 4. Demotion to Incubator/Graveyard (Fail)
        (t
         (unless (member tier '(:incubator :graveyard))
           (demote strat :incubator "Sharpe < 0.1")))))))

(defun execute-proving-grounds ()
  "Main entry point. Runs Meritocratic Selection."
  (format t "[SELECTOR] OPENING THE PROVING GROUNDS...~%")
  (run-meritocracy-selection)

  ;; 2. Report Status
  (format t "[SELECTOR] BATTLEFIELD STATUS:~%")
  (dolist (s (get-strategies-by-tier :battlefield))
    (format t "   [~a] ~a (Sharpe: ~,2f)~%" 
            (strategy-category s) (strategy-name s) (strategy-sharpe s))))

;; Auto-run on load (for now, during dev)
;; (execute-proving-grounds)
