;;; ============================================================================
;;; engine/metabolism.lisp - Strategy Lifecycle System
;;; ============================================================================
;;; "The Reaper"
;;; Automates the life and death of strategies:

(in-package :swimmy.engine)
;;; 1. Pruning (Death): Remove weak strategies (Sharpe < 0, etc.)
;;; 2. Spawning (Birth): Replenish population from Dreamer
;;; 3. Promotion (Growth): Move from Nursery to Garden
;;; Part of "The Efficient Gardener" refactoring (Phase 7)
;;; ============================================================================

;; Defvar for forward referencing Dreamer variables
(defvar *evolved-strategies* nil "Pool of strategies from Dreamer")
(defvar *min-arms* 3 "Minimum number of active strategies")
(defvar *max-arms* 10 "Maximum number of active strategies")

;;; ==========================================
;;; UTILITY FUNCTIONS
;;; ==========================================

;; Extract SMA params from indicator list (Legacy logic from Dreamer)
(defun extract-sma-params-safe (indicators)
  (let ((nums nil))
    (dolist (ind indicators)
      (when (listp ind)
        (dolist (x (cdr ind)) (when (numberp x) (push x nums)))))
    (if (>= (length nums) 2)
        (let ((sorted (sort nums #'<)))
          (list (first sorted) (second sorted)))
        '(5 20)))) ; Fallback

(defun strategy-to-arm-params (strat)
  "Convert a Strategy struct (from Dreamer) into an Arm param list."
  ;; Strategy struct accessors might not be loaded yet, use safe dynamic access
  (let ((inds (funcall (find-symbol "STRATEGY-INDICATORS" "CL-USER") strat))
        (sl (funcall (find-symbol "STRATEGY-SL" "CL-USER") strat))
        (tp (funcall (find-symbol "STRATEGY-TP" "CL-USER") strat))
        (vol (funcall (find-symbol "STRATEGY-VOLUME" "CL-USER") strat)))
    (let ((smas (extract-sma-params-safe inds)))
      ;; Return: (sma-s sma-l sl tp vol)
      (list (first smas) (second smas) sl tp vol))))

;;; ==========================================
;;; PRUNING (THE REAPER)
;;; ==========================================

(defun should-prune-p (stats)
  "Determine if an arm should be pruned based on stats."
  (let* ((wins (first stats))
         (losses (second stats))
         (total (+ wins losses))
         (rate (if (> total 0) (/ wins total) 0.5)))
    (cond
      ;; Rule 1: Immediate failure (10 trades, < 30% win rate)
      ((and (> total 10) (< rate 0.3)) t)
      
      ;; Rule 2: Persistent mediocrity (30 trades, < 45% win rate)
      ((and (> total 30) (< rate 0.45)) t)
      
      ;; Rule 3: Zero wins after 5 trades
      ((and (> total 5) (= wins 0)) t)
      
      (t nil))))

(defun prune-strategies ()
  "Remove underperforming strategies and rebuild state."
  (let ((survivors nil)
        (pruned-count 0))
    
    ;; 1. Identify Survivors
    (loop for arm in *arms*
          for idx from 0
          for stats = (cdr arm)
          do (if (should-prune-p stats)
                 (progn
                   (incf pruned-count)
                   (format t "[METABOLISM] 💀 Pruned Arm ~d (Wins: ~a/~a)~%" 
                           idx (first stats) (+ (first stats) (second stats))))
                 (push (cons idx arm) survivors)))
    
    (setf survivors (nreverse survivors))
    
    ;; 2. Rebuild *arms* and State if needed
    (when (> pruned-count 0)
      (let ((new-arms nil)
            (new-states (make-hash-table))
            (new-portfolio nil)
            (map-old-to-new (make-hash-table)))
        
        ;; Map old indices to new
        (loop for (old-idx . arm-data) in survivors
              for new-idx from 0
              do (progn
                   (push arm-data new-arms)
                   (setf (gethash old-idx map-old-to-new) new-idx)
                   ;; Copy state
                   (let ((old-state (gethash old-idx *arm-states*)))
                     (when old-state
                       (setf (gethash new-idx new-states) old-state)))))
        
        ;; Remap portfolio indices
        (dolist (old-p-idx *portfolio-indices*)
          (let ((mapped (gethash old-p-idx map-old-to-new)))
            (when mapped
              (push mapped new-portfolio))))
        
        ;; Update Globals
        (setf *arms* (nreverse new-arms))
        (setf *arm-states* new-states)
        (setf *portfolio-indices* (nreverse new-portfolio))
        
        (format t "[METABOLISM] ♻️ Reorganization detailed: ~d pruned, ~d remaining~%" 
                pruned-count (length *arms*))
        
        ;; Save changes
        (when (fboundp 'save-genome)
          (save-genome))))))

;;; ==========================================
;;; SPAWNING (THE NURSERY)
;;; ==========================================

(defun replenish-arms ()
  "Replenish arms from Evolver pool if population is low."
  (loop while (and (< (length *arms*) *min-arms*)
                   *evolved-strategies*
                   (car *evolved-strategies*))
        do (let* ((strat (pop *evolved-strategies*))
                  (params (strategy-to-arm-params strat))
                  (new-stats (cons 0 0))) ; (wins . losses)
             ;; Add to Arms
             (setf *arms* (append *arms* (list (cons params new-stats))))
             (format t "[METABOLISM] 🌱 Spawning new arm from: ~a (Params: ~a)~%" 
                     (funcall (find-symbol "STRATEGY-NAME" "CL-USER") strat) 
                     params)))
  
  ;; Trigger Dreamer if still low
  (when (< (length *arms*) *min-arms*)
    (when (fboundp 'dream-code)
      (format t "[METABOLISM] 💭 Population critical. Dreaming...~%")
      (funcall (find-symbol "DREAM-CODE" "CL-USER")))))

;;; ==========================================
;;; MAIN CYCLE
;;; ==========================================

(defun run-metabolism ()
  "Execute the metabolic cycle: Prune -> Replenish."
  (format t "[METABOLISM] 🍂 Cycle running...~%")
  (prune-strategies)
  (replenish-arms))

(format t "[ENGINE] metabolism.lisp loaded~%")
