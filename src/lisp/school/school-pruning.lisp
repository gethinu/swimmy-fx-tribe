;;; school-pruning.lisp
;;; ============================================================================
;;; P10: STRATEGY PRUNING & KB OPTIMIZATION
;;; ============================================================================
;;; Purpose: Reduce KB size from 18,349 strategies to ~5,000 target
;;; Methods:
;;; 1. Low Sharpe removal (< 0.05)
;;; 2. Long-term inactive removal (90 days no signal)
;;; 3. Similar strategy removal (distance < 0.1)
;;; ============================================================================

(in-package :swimmy.school)

;;; ============================================================================
;;; CONFIGURATION
;;; ============================================================================

(defparameter *prune-sharpe-threshold* 0.10
  "V49.6: Aggressive culling floor (Taleb/Musk Recommendation)")

(defparameter *prune-inactive-days* 90
  "Days of inactivity before a strategy is considered dormant")

(defparameter *prune-similarity-threshold* 0.1
  "Strategies with distance below this are considered duplicates")

(defparameter *prune-protected-ranks* '(:S :legend :A)
  "Ranks that are protected from pruning (Fortress Mode: A-Rank included)")

(defparameter *kb-hard-cap* 2000
  "V49.6: Target Size for S-Rank density (5000 -> 2000)")

;;; ============================================================================
;;; LOW SHARPE PRUNING
;;; ============================================================================

(defun prune-low-sharpe-strategies ()
  "Remove strategies with Sharpe ratio below threshold and move to physical Grave.
   V48.8: Now physically moves files via swimmy.persistence:move-strategy.
   V49.0: Fortress Mode - A-Rank is immutable."
  (let* ((before-count (length *strategy-knowledge-base*))
         (removed 0))
    (setf *strategy-knowledge-base*
          (remove-if 
           (lambda (strat)
             (let ((sharpe (or (strategy-sharpe strat) 0.0))
                   (rank (strategy-rank strat)))
               ;; 1. Check if Sharpe is below threshold
               (if (< sharpe *prune-sharpe-threshold*)
                   (cond
                     ;; 2. PROTECTION CHECK (Fortress)
                     ((member rank *prune-protected-ranks*)
                      (format t "[PRUNE-BLOCK] 🛡️ Protected ~a Strategy: ~a (Sharpe=~,3f) - SAFE~%"
                              rank (strategy-name strat) sharpe)
                      nil) ;; Do NOT remove
                     
                     ;; 3. NORMAL PRUNING
                     (t
                      (format t "[PRUNE-AUDIT] 🗑️ Low Sharpe: ~a (Sharpe=~,3f Rank=~a) → GRAVEYARD~%"
                              (strategy-name strat) sharpe rank)
                      ;; V48.8: Use send-to-graveyard to save patterns and update memory
                      (send-to-graveyard strat "Operation Black Death (Low Sharpe)")
                      (incf removed)
                      t))
                   ;; Sharpe OK
                   nil))) 
           *strategy-knowledge-base*))
    (format t "[PRUNE] ✅ Purged ~d low-Sharpe strategies (KB: ~d → ~d)~%"
            removed before-count (length *strategy-knowledge-base*))
    removed))

;;; ============================================================================
;;; INACTIVE STRATEGY PRUNING
;;; ============================================================================

(defun prune-inactive-strategies ()
  "Remove strategies that haven't generated signals in *prune-inactive-days*.
   Returns count of removed strategies."
  (let* ((before-count (length *strategy-knowledge-base*))
         (cutoff-time (- (get-universal-time) 
                         (* *prune-inactive-days* 24 60 60)))
         (removed 0))
    (setf *strategy-knowledge-base*
          (remove-if 
           (lambda (strat)
             (let ((last-signal (or (strategy-last-signal-time strat) 0))
                   (rank (strategy-rank strat)))
               (when (and (< last-signal cutoff-time)
                          (> last-signal 0) ; Only prune if ever had a signal
                          (not (member rank *prune-protected-ranks*)))
                 (format t "[PRUNE] 💤 Inactive: ~a (~d days)~%"
                         (strategy-name strat)
                         (floor (/ (- (get-universal-time) last-signal) 
                                   (* 24 60 60))))
                 (incf removed)
                 t)))
           *strategy-knowledge-base*))
    (format t "[PRUNE] ✅ Removed ~d inactive strategies (KB: ~d → ~d)~%"
            removed before-count (length *strategy-knowledge-base*))
    removed))

;;; ============================================================================
;;; SIMILAR STRATEGY PRUNING
;;; ============================================================================

(defun strategy-distance (strat1 strat2)
  "Calculate distance between two strategies.
   Includes: SL, TP, timeframe, indicator type, symbol.
   Returns normalized distance [0.0, 1.0] where 0 = identical.
   P11: Enhanced per Hickey's Expert Panel recommendation."
  (let* ((sl1 (or (strategy-sl strat1) 0.005))
         (sl2 (or (strategy-sl strat2) 0.005))
         (tp1 (or (strategy-tp strat1) 0.01))
         (tp2 (or (strategy-tp strat2) 0.01))
         (tf1 (or (strategy-timeframe strat1) 60))
         (tf2 (or (strategy-timeframe strat2) 60))
         ;; P11: Add indicator type comparison
         (ind1 (car (strategy-indicators strat1)))
         (ind2 (car (strategy-indicators strat2)))
         (ind-type1 (if (listp ind1) (car ind1) ind1))
         (ind-type2 (if (listp ind2) (car ind2) ind2))
         (ind-match (if (equalp ind-type1 ind-type2) 0.0 1.0))
         ;; P11: Add symbol comparison
         (sym1 (or (strategy-symbol strat1) "USDJPY"))
         (sym2 (or (strategy-symbol strat2) "USDJPY"))
         (sym-match (if (string-equal sym1 sym2) 0.0 1.0))
         ;; Normalize differences
         (sl-diff (abs (/ (- sl1 sl2) (max sl1 sl2 0.001))))
         (tp-diff (abs (/ (- tp1 tp2) (max tp1 tp2 0.001))))
         (tf-diff (abs (/ (- tf1 tf2) (max tf1 tf2 1)))))
    ;; Weighted average (P11: adjusted weights)
    ;; indicator 25%, symbol 15%, SL 20%, TP 20%, TF 20%
    (/ (+ (* 0.25 ind-match) 
          (* 0.15 sym-match) 
          (* 0.20 sl-diff) 
          (* 0.20 tp-diff) 
          (* 0.20 tf-diff)) 
       1.0)))

(defun prune-similar-strategies ()
  "Remove duplicate/near-identical strategies, keeping higher Sharpe.
   V48.8: Now physically moves files via swimmy.persistence:move-strategy."
  (let* ((before-count (length *strategy-knowledge-base*))
         (strategies (copy-list *strategy-knowledge-base*))
         (to-remove nil)
         (removed 0))
    ;; Sort by Sharpe descending (keep highest Sharpe)
    (setf strategies 
          (sort strategies #'> 
                :key (lambda (s) (or (strategy-sharpe s) 0.0))))
    ;; Compare each pair
    (loop for i from 0 below (1- (length strategies))
          for strat1 = (nth i strategies)
          do (unless (member strat1 to-remove :test #'eq)
               (loop for j from (1+ i) below (min (+ i 50) (length strategies))
                     for strat2 = (nth j strategies)
                     do (unless (member strat2 to-remove :test #'eq)
                          (when (and (< (strategy-distance strat1 strat2) 
                                        *prune-similarity-threshold*)
                                     (not (member (strategy-rank strat2) 
                                                  *prune-protected-ranks*)))
                            (push strat2 to-remove)
                            (incf removed)
                            (format t "[PRUNE] 👯 Similar: ~a ≈ ~a → GRAVEYARD~%"
                                    (strategy-name strat2) 
                                    (strategy-name strat1))
                            ;; V48.8: Use send-to-graveyard to save patterns and update memory
                            (send-to-graveyard strat2 "Operation Black Death (Similarity)"))))))
    ;; Remove similar strategies from memory
    (setf *strategy-knowledge-base*
          (remove-if (lambda (s) (member s to-remove :test #'eq))
                     *strategy-knowledge-base*))
    (format t "[PRUNE] ✅ Purged ~d similar strategies (KB: ~d → ~d)~%"
            removed before-count (length *strategy-knowledge-base*))
    removed))

(defun enforce-kb-hard-cap ()
  "V48.8: Ensure KB size does not exceed *kb-hard-cap*.
   Removes lowest Sharpe strategies first."
  (let ((current-size (length *strategy-knowledge-base*)))
    (when (> current-size *kb-hard-cap*)
      (let* ((excess (- current-size *kb-hard-cap*))
             ;; Sort by Sharpe ascending (worst first)
             (sorted (sort (copy-list *strategy-knowledge-base*) #'<
                           :key (lambda (s) (or (strategy-sharpe s) 0.0))))
             (to-purge (subseq (remove-if (lambda (s) (member (strategy-rank s) *prune-protected-ranks*))
                                          sorted)
                               0 (min excess current-size))))
        (format t "[PRUNE] 🚨 KB Over Limit (~d > ~d). Purging ~d laggards...~%"
                current-size *kb-hard-cap* (length to-purge))
        (dolist (strat to-purge)
          (send-to-graveyard strat "Operation Black Death (KB Hard Cap)"))
        (format t "[PRUNE] ✅ KB Downsized to ~d strategies.~%" (length *strategy-knowledge-base*))
        (length to-purge)))))

;;; ============================================================================
;;; MAIN PRUNING FUNCTION
;;; ============================================================================

(defun run-kb-pruning (&key (dry-run nil))
  "Run all pruning operations on the Knowledge Base.
   If DRY-RUN is T, only reports what would be removed."
  (format t "~%╔═══════════════════════════════════════════════════════╗~%")
  (format t "║     P10: STRATEGY PRUNING ~a                    ║~%"
          (if dry-run "(DRY RUN)" ""))
  (format t "╠═══════════════════════════════════════════════════════╣~%")
  (format t "║ KB Size Before: ~6d strategies~24@a║~%"
          (length *strategy-knowledge-base*) "")
  (format t "╚═══════════════════════════════════════════════════════╝~%~%")
  
  (if dry-run
      (format t "[PRUNE] Dry run - no changes will be made~%")
      (let ((total-removed 0))
        ;; Phase 1: Low Sharpe
        (format t "~%📊 Phase 1: Low Sharpe Pruning~%")
        (incf total-removed (prune-low-sharpe-strategies))
        
        ;; Phase 2: Inactive
        (format t "~%📊 Phase 2: Inactive Strategy Pruning~%")
        (incf total-removed (prune-inactive-strategies))
        
        ;; Phase 3: Similar
        (format t "~%📊 Phase 3: Similar Strategy Pruning~%")
        (incf total-removed (prune-similar-strategies))
        
        ;; Phase 4: Hard Cap Enforcing (V48.8)
        (format t "~%📊 Phase 4: KB Hard Cap Enforcement~%")
        (incf total-removed (or (enforce-kb-hard-cap) 0))
        
        ;; Summary
        (format t "~%╔═══════════════════════════════════════════════════════╗~%")
        (format t "║     PRUNING COMPLETE~36@a║~%" "")
        (format t "╠═══════════════════════════════════════════════════════╣~%")
        (format t "║ Total Removed:  ~6d strategies~24@a║~%" total-removed "")
        (format t "║ KB Size After:  ~6d strategies~24@a║~%" 
                (length *strategy-knowledge-base*) "")
        (format t "╚═══════════════════════════════════════════════════════╝~%")
        
        total-removed)))

(defun get-kb-statistics ()
  "Get statistics about current Knowledge Base for pruning analysis."
  (let* ((strategies *strategy-knowledge-base*)
         (count (length strategies))
         (low-sharpe (count-if (lambda (s) 
                                 (< (or (strategy-sharpe s) 0.0) 
                                    *prune-sharpe-threshold*))
                               strategies))
         (protected (count-if (lambda (s) 
                                (member (strategy-rank s) 
                                        *prune-protected-ranks*))
                              strategies)))
    (format t "~%📊 KB Statistics:~%")
    (format t "   Total Strategies: ~d~%" count)
    (format t "   Low Sharpe (<~,3f): ~d~%" *prune-sharpe-threshold* low-sharpe)
    (format t "   Protected (S/Legend): ~d~%" protected)
    (format t "   Prunable: ~d~%" (max 0 (- low-sharpe protected)))))
