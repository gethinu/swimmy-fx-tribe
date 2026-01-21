;;; src/lisp/school/school-rank-system.lisp
;;; ============================================================================
;;; B/A/S RANK SYSTEM (V47.0 - Owner's Vision)
;;; ============================================================================
;;; Implements the new rank-based lifecycle per Expert Panel (2026-01-21).
;;;
;;; RANKS:
;;; 1. :B - Phase 1 Backtest passed (Sharpe≥0.1, PF≥1.0, WR≥30%, MaxDD<30%)
;;; 2. :A - CPCV validated (Sharpe≥0.3, PF≥1.2, WR≥40%, MaxDD<20%)
;;; 3. :S - Live trading permitted (Sharpe≥0.5, PF≥1.5, WR≥45%, MaxDD<15%)
;;; 4. :graveyard - Failed strategies (learning data)
;;; 5. :legend - Protected strategies (61 total, never discarded)
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; RANK CONSTANTS
;;; ---------------------------------------------------------------------------

(defparameter *rank-criteria*
  '((:B       :sharpe-min 0.1  :pf-min 1.0  :wr-min 0.30  :maxdd-max 0.30)
    (:A       :sharpe-min 0.3  :pf-min 1.2  :wr-min 0.40  :maxdd-max 0.20)
    (:S       :sharpe-min 0.5  :pf-min 1.5  :wr-min 0.45  :maxdd-max 0.15))
  "Rank criteria thresholds. All conditions must be met (AND logic).")

(defparameter *culling-threshold* 100
  "Number of B-RANK strategies per TF before culling begins.")

(defparameter *a-rank-slots-per-tf* 2
  "Only top 2 strategies per TF can be promoted to A-RANK.")

(defparameter *max-breeding-uses* 3
  "Strategies are discarded after being used 3 times for breeding (Legend exempt).")

;;; ---------------------------------------------------------------------------
;;; RANK UTILITIES
;;; ---------------------------------------------------------------------------

(defun get-rank-criteria (rank)
  "Get criteria plist for a given rank."
  (cdr (assoc rank *rank-criteria*)))

(defun check-rank-criteria (strategy target-rank)
  "Check if strategy meets all criteria for target-rank.
   Returns T if all conditions pass, NIL otherwise."
  (let* ((criteria (get-rank-criteria target-rank))
         (sharpe (or (strategy-sharpe strategy) 0.0))
         (pf (or (strategy-profit-factor strategy) 0.0))
         (wr (or (strategy-win-rate strategy) 0.0))
         (maxdd (or (strategy-max-dd strategy) 1.0)))
    
    (and (>= sharpe (getf criteria :sharpe-min 0))
         (>= pf (getf criteria :pf-min 0))
         (>= wr (getf criteria :wr-min 0))
         (< maxdd (getf criteria :maxdd-max 1.0)))))

(defun get-strategies-by-rank (rank &optional timeframe)
  "Get all strategies with a specific rank, optionally filtered by TF."
  (let ((candidates (remove-if-not 
                      (lambda (s) (eq (strategy-rank s) rank))
                      *strategy-knowledge-base*)))
    (if timeframe
        (remove-if-not (lambda (s) (eql (strategy-timeframe s) timeframe)) candidates)
        candidates)))

(defun count-by-rank-and-tf (rank timeframe)
  "Count strategies with given rank and timeframe."
  (length (get-strategies-by-rank rank timeframe)))

;;; ---------------------------------------------------------------------------
;;; RANK PROMOTION / DEMOTION
;;; ---------------------------------------------------------------------------

(defun promote-rank (strategy new-rank reason)
  "Promote strategy to a higher rank."
  (let ((old-rank (strategy-rank strategy)))
    (setf (strategy-rank strategy) new-rank)
    (format t "[RANK] ⬆️ PROMOTE: ~a (~a → ~a) | ~a~%"
            (strategy-name strategy) old-rank new-rank reason)))

(defun demote-rank (strategy new-rank reason)
  "Demote strategy to a lower rank (or graveyard)."
  (let ((old-rank (strategy-rank strategy)))
    (setf (strategy-rank strategy) new-rank)
    (format t "[RANK] ⬇️ DEMOTE: ~a (~a → ~a) | ~a~%"
            (strategy-name strategy) old-rank new-rank reason)))

(defun send-to-graveyard (strategy reason)
  "Move strategy to graveyard and save failure pattern."
  (setf (strategy-rank strategy) :graveyard)
  (save-failure-pattern strategy)
  (format t "[RANK] 💀 GRAVEYARD: ~a | ~a~%"
          (strategy-name strategy) reason))

(defun save-failure-pattern (strategy)
  "Save failed strategy parameters for learning (avoid same mistakes)."
  ;; TODO: Implement failure DB
  (declare (ignore strategy)))

;;; ---------------------------------------------------------------------------
;;; PHASE 1 EVALUATION (New Strategy → B-RANK or Graveyard)
;;; ---------------------------------------------------------------------------

(defun evaluate-new-strategy (strategy)
  "Evaluate a newly generated strategy against B-RANK criteria.
   Uses Phase 1 backtest (2006-2020, 15 years).
   Returns :B if passed, :graveyard if failed."
  (if (check-rank-criteria strategy :B)
      (progn
        (promote-rank strategy :B 
          (format nil "Phase1 OK: S=~,2f PF=~,2f WR=~,1f% DD=~,1f%"
                  (strategy-sharpe strategy)
                  (strategy-profit-factor strategy)
                  (* 100 (strategy-win-rate strategy))
                  (* 100 (strategy-max-dd strategy))))
        :B)
      (progn
        (send-to-graveyard strategy
          (format nil "Phase1 FAIL: S=~,2f PF=~,2f WR=~,1f% DD=~,1f%"
                  (or (strategy-sharpe strategy) 0)
                  (or (strategy-profit-factor strategy) 0)
                  (* 100 (or (strategy-win-rate strategy) 0))
                  (* 100 (or (strategy-max-dd strategy) 1))))
        :graveyard)))

;;; ---------------------------------------------------------------------------
;;; B-RANK CULLING (100 strategies/TF → Keep best, discard rest)
;;; ---------------------------------------------------------------------------

(defun run-b-rank-culling (timeframe)
  "When B-RANK reaches threshold, keep top performers and discard rest.
   Top 2 per TF are promoted to A-RANK."
  (let* ((b-strategies (get-strategies-by-rank :B timeframe))
         (count (length b-strategies)))
    
    (when (>= count *culling-threshold*)
      (format t "[RANK] 🗡️ CULLING B-RANK (TF=~a, Count=~d)~%" timeframe count)
      
      ;; Sort by composite score (Sharpe + PF bonus)
      (let* ((sorted (sort (copy-list b-strategies) #'>
                           :key (lambda (s) 
                                  (+ (or (strategy-sharpe s) 0)
                                     (* 0.1 (or (strategy-profit-factor s) 0))))))
             (to-promote (subseq sorted 0 (min *a-rank-slots-per-tf* (length sorted))))
             (to-discard (nthcdr *a-rank-slots-per-tf* sorted)))
        
        ;; Promote top 2 to A-RANK
        (dolist (s to-promote)
          (promote-rank s :A "Culling Champion"))
        
        ;; Discard rest
        (dolist (s to-discard)
          (send-to-graveyard s "Culling Loser"))))))

;;; ---------------------------------------------------------------------------
;;; A-RANK EVALUATION (CPCV Validation → S-RANK or back to B)
;;; ---------------------------------------------------------------------------

(defun evaluate-a-rank-strategy (strategy)
  "Evaluate A-RANK strategy with CPCV (2021-2026 OOS).
   If passes S criteria → S-RANK. If fails → B-RANK or Graveyard."
  (if (check-rank-criteria strategy :S)
      (progn
        (promote-rank strategy :S "CPCV Validated - LIVE TRADING PERMITTED")
        :S)
      (if (check-rank-criteria strategy :B)
          (progn
            (demote-rank strategy :B "CPCV Failed - Back to Training")
            :B)
          (progn
            (send-to-graveyard strategy "CPCV Critical Failure")
            :graveyard))))

;;; ---------------------------------------------------------------------------
;;; BREEDING MANAGEMENT
;;; ---------------------------------------------------------------------------

(defun increment-breeding-count (strategy)
  "Increment breeding use count. Discard if reaches limit (Legend exempt)."
  (let ((count (1+ (or (strategy-breeding-count strategy) 0))))
    (setf (strategy-breeding-count strategy) count)
    
    (when (and (>= count *max-breeding-uses*)
               (not (eq (strategy-rank strategy) :legend)))
      (send-to-graveyard strategy 
        (format nil "Breeding limit reached (~d uses)" count)))))

;;; ---------------------------------------------------------------------------
;;; MAIN ENTRY POINT
;;; ---------------------------------------------------------------------------

(defun run-rank-evaluation ()
  "Main rank evaluation cycle.
   1. Evaluate new strategies (→ B or Graveyard)
   2. Cull B-RANK if threshold reached
   3. Validate A-RANK via CPCV (→ S or back)"
  (format t "[RANK] 🏛️ Starting Rank Evaluation Cycle (V47.0)~%")
  
  ;; Process each timeframe
  (dolist (tf '(5 15 60 240 1440 10080))
    ;; Check if culling needed
    (run-b-rank-culling tf))
  
  ;; Report status
  (format t "[RANK] 📊 Status: B=~d A=~d S=~d Graveyard=~d Legend=~d~%"
          (length (get-strategies-by-rank :B))
          (length (get-strategies-by-rank :A))
          (length (get-strategies-by-rank :S))
          (length (get-strategies-by-rank :graveyard))
          (length (get-strategies-by-rank :legend))))

;;; ---------------------------------------------------------------------------
;;; BREEDING HELPERS (V47.0)
;;; ---------------------------------------------------------------------------

(defun can-breed-p (strategy)
  "Check if strategy can be used for breeding.
   Returns T if under breeding limit or is Legend."
  (or (eq (strategy-rank strategy) :legend)
      (< (or (strategy-breeding-count strategy) 0) *max-breeding-uses*)))

(defun run-legend-breeding ()
  "Breed Legend strategies with random B-rank strategies.
   V47.0: Owner's Vision - Legends participate in periodic random breeding."
  (format t "[LEGEND] 👑 Starting Legend Breeding Cycle...~%")
  (let ((legends (get-strategies-by-rank :legend))
        (b-ranks (get-strategies-by-rank :B))
        (bred-count 0))
    
    (when (and legends b-ranks)
      ;; Pick random legend and random B-rank
      (let* ((legend (nth (random (length legends)) legends))
             (b-rank (nth (random (length b-ranks)) b-ranks)))
        
        (when (and legend b-rank)
          (format t "[LEGEND] 🏆 Breeding ~a (Legend) + ~a (B-Rank)~%"
                  (strategy-name legend) (strategy-name b-rank))
          
          ;; Create child using breed-strategies from school-breeder
          (when (fboundp 'breed-strategies)
            (let ((child (breed-strategies legend b-rank)))
              ;; Mark child as having legendary heritage
              (setf (strategy-generation child) 
                    (1+ (max (strategy-generation legend) 
                             (strategy-generation b-rank))))
              ;; Increment B-rank's breeding count (Legend exempt)
              (increment-breeding-count b-rank)
              
              (push child *strategy-knowledge-base*)
              (incf bred-count)
              (format t "[LEGEND] 👶 Royal Child Born: ~a~%" (strategy-name child)))))))
    
    (format t "[LEGEND] 👑 Legend Breeding Complete: ~d children~%" bred-count)
    bred-count))

