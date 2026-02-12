;;; src/lisp/school/school-breeder.lisp
;;; ============================================================================
;;; STRATEGY BREEDING LAB (Evolution Engine)
;;; ============================================================================
;;; Implements "Breeding" as requested by Naval/Musk.
;;; Takes fittest strategies and generates offspring for the Incubator.
;;; ============================================================================

(in-package :swimmy.school)

(defun random-range (min max)
  "Return random number between min and max."
  (+ min (random (- max min))))

(defun mutate-value (val factor)
  "Mutate a numeric value by +/- factor."
  (if (numberp val)
      (let ((delta (* val factor (random-range -1.0 1.0))))
        (+ val delta))
      val))

(defun crossover-indicators (p1 p2)
  "Combine indicators from parent 1 and parent 2."
  (let ((i1 (strategy-indicators p1))
        (i2 (strategy-indicators p2)))
    ;; Simple logic: Take half from each, randomly
    (remove-duplicates (append (subseq i1 0 (ceiling (/ (length i1) 2)))
                               (subseq i2 0 (ceiling (/ (length i2) 2))))
                       :test #'equal)))

(defun mutate-indicators-with-library (indicators category)
  "Mutate indicators by swapping one with a regime-aware alternative.
   V49.5: Prevents 'Genetic Stagnation' using indicators-library."
  (let ((len (length indicators)))
    (when (zerop len)
      (return-from mutate-indicators-with-library indicators))
    (if (> (random 1.0) 0.7) ; 30% chance to swap an indicator
        (let* ((idx (random len))
             (regime (case category
                       (:trend :trend)
                       (:reversion :reversion)
                       (:breakout :breakout)
                       (:scalp :reversion) ; Scalp usually reversions/fast trends
                       (t :trend)))
             (new-indicator (get-random-indicator-for-regime regime)))
        (format t "[BREEDER] 🧬 Indicator Swap: ~a -> ~a (Regime: ~a)~%" 
                (nth idx indicators) new-indicator regime)
        (setf (nth idx indicators) new-indicator)
        indicators)
        indicators)))

(defun mutate-indicator-params (params)
  "Recursively mutate numeric parameters in an indicator list (e.g. RSI 14 -> 15).
   V49.3 Expert Panel: Fixes 'Genetic Stagnation' by exploring parameter space."
  (mapcar (lambda (p)
            (cond
              ((listp p) (mutate-indicator-params p))
              ((numberp p)
               (let ((new-val (mutate-value p 0.1))) 
                 (if (integerp p) (round new-val) new-val)))
              (t p))) 
          params))

(defparameter *pfwr-mutation-bias-enabled* t
  "When T, breeding SL/TP mutation is softly biased toward better parent PF/WR profile.")
(defparameter *pfwr-mutation-bias-strength* 0.5
  "Blend strength (0..1) for PF/WR mutation bias.")
(defparameter *pfwr-target-pf* 1.30
  "Target PF used to determine if parents are underperforming.")
(defparameter *pfwr-target-wr* 0.43
  "Target WR used to determine if parents are underperforming.")
(defparameter *pfwr-min-rr* 0.4
  "Lower bound for TP/SL ratio after PF/WR bias.")
(defparameter *pfwr-max-rr* 4.0
  "Upper bound for TP/SL ratio after PF/WR bias.")
(defparameter *pfwr-wr-recovery-max-rr* 2.8
  "RR cap used when WR deficit dominates PF deficit.")
(defparameter *pfwr-wr-recovery-gap-min* 0.02
  "Minimum WR-vs-PF gap delta before WR-recovery RR cap engages.")
(defparameter *pfwr-wr-recovery-moderate-gap* 0.08
  "WR gap ratio threshold for moderate RR cap tightening.")
(defparameter *pfwr-wr-recovery-severe-gap* 0.12
  "WR gap ratio threshold for aggressive RR cap tightening.")
(defparameter *pfwr-wr-recovery-moderate-cap-rr* 2.1
  "RR cap when WR gap is moderately above target.")
(defparameter *pfwr-wr-recovery-severe-cap-rr* 1.8
  "RR cap when WR gap is severely above target.")
(defparameter *pfwr-pf-recovery-gap-min* 0.04
  "PF-vs-WR gap delta required before PF recovery RR floor engages.")
(defparameter *pfwr-pf-recovery-min-rr* 1.8
  "Minimum RR enforced when PF deficit dominates WR deficit.")
(defparameter *pfwr-neutral-rr* 1.4
  "Balanced RR target used when PF/WR deficits are similar.")
(defparameter *pfwr-neutral-rr-span* 0.8
  "How far RR can tilt from neutral based on PF-vs-WR deficit delta.")
(defparameter *pfwr-anchor-weight* 0.5
  "Weight of anchor-parent RR vs deficit-tilted RR in final PF/WR target.")
(defparameter *pfwr-post-q-rebias-enabled* t
  "When T, breeder re-applies PF/WR bias after Q-value SL/TP selection.")
(defparameter *pfwr-post-q-min-strength* 0.9
  "Minimum PF/WR bias strength used in post-Q rebias pass.")
(defparameter *breeder-priority-use-a-base-score* t
  "When T, breeder parent ranking uses A-base-aware culling score if available.")
(defparameter *breeder-priority-generation-weight* 0.01
  "Generation bonus weight in breeder parent ranking.")
(defparameter *breeder-complement-wr-bonus* 1.5
  "Partner score bonus when candidate satisfies WR target that parent is missing.")
(defparameter *breeder-complement-pf-bonus* 1.5
  "Partner score bonus when candidate satisfies PF target that parent is missing.")
(defparameter *breeder-complement-double-bonus* 0.75
  "Extra bonus when candidate satisfies both missing PF and WR targets.")
(defparameter *breeder-prioritize-complement-partner* t
  "When T, prioritize partners that satisfy at least one PF/WR deficit of the parent.")
(defparameter *breeder-name-seq* 0
  "Monotonic per-process sequence for collision-resistant child naming.")

(defun breeder-name-entropy ()
  "Generate high-entropy suffix for child strategy names."
  (format nil "~d-~d" (get-universal-time) (incf *breeder-name-seq*)))

(defun clamp-breeder-float (value low high)
  (max low (min high value)))

(defun safe-breeder-ratio (num den default)
  (if (and den (> den 0.0))
      (/ num den)
      default))

(defun strategy-pfwr-score (strategy)
  "Score parent quality for PF/WR-guided mutation anchoring."
  (let ((pf (max 0.0 (float (or (strategy-profit-factor strategy) 0.0))))
        (wr (max 0.0 (float (or (strategy-win-rate strategy) 0.0)))))
    (+ (* 0.6 (min pf 2.0))
       (* 0.4 (* 2.0 wr)))))

(defun select-pfwr-anchor-parent (parent1 parent2)
  "Select the parent with stronger PF/WR profile."
  (multiple-value-bind (pf-gap-ratio wr-gap-ratio)
      (pfwr-gap-profile parent1 parent2)
    (let ((wr-delta (- wr-gap-ratio pf-gap-ratio)))
      (if (> wr-delta *pfwr-wr-recovery-gap-min*)
          ;; Under WR pressure, anchor to the higher-WR parent to reduce RR drift.
          (if (>= (float (or (strategy-win-rate parent1) 0.0))
                  (float (or (strategy-win-rate parent2) 0.0)))
              parent1
              parent2)
          (if (>= (strategy-pfwr-score parent1) (strategy-pfwr-score parent2))
              parent1
              parent2)))))

(defun pfwr-gap-profile (parent1 parent2)
  "Return PF and WR gap ratios (0..1) against A-rank targets."
  (let* ((pf1 (max 0.0 (float (or (strategy-profit-factor parent1) 0.0))))
         (pf2 (max 0.0 (float (or (strategy-profit-factor parent2) 0.0))))
         (wr1 (max 0.0 (float (or (strategy-win-rate parent1) 0.0))))
         (wr2 (max 0.0 (float (or (strategy-win-rate parent2) 0.0))))
         (avg-pf (/ (+ pf1 pf2) 2.0))
         (avg-wr (/ (+ wr1 wr2) 2.0))
         (pf-gap (max 0.0 (- *pfwr-target-pf* avg-pf)))
         (wr-gap (max 0.0 (- *pfwr-target-wr* avg-wr)))
         (pf-gap-ratio (safe-breeder-ratio pf-gap *pfwr-target-pf* 0.0))
         (wr-gap-ratio (safe-breeder-ratio wr-gap *pfwr-target-wr* 0.0)))
    (values (clamp-breeder-float pf-gap-ratio 0.0 1.0)
            (clamp-breeder-float wr-gap-ratio 0.0 1.0))))

(defun select-logic-anchor-parent (parent1 parent2)
  "Select which parent's entry/exit logic should anchor the child."
  (multiple-value-bind (pf-gap-ratio wr-gap-ratio)
      (pfwr-gap-profile parent1 parent2)
    (let* ((wr-delta (- wr-gap-ratio pf-gap-ratio))
           (pf-delta (- pf-gap-ratio wr-gap-ratio))
           (wr1 (float (or (strategy-win-rate parent1) 0.0)))
           (wr2 (float (or (strategy-win-rate parent2) 0.0)))
           (pf1 (float (or (strategy-profit-factor parent1) 0.0)))
           (pf2 (float (or (strategy-profit-factor parent2) 0.0))))
      (cond
        ;; WR recovery regime: inherit logic from higher-WR parent.
        ((> wr-delta *pfwr-wr-recovery-gap-min*)
         (if (>= wr1 wr2) parent1 parent2))
        ;; PF recovery regime: inherit logic from higher-PF parent.
        ((>= pf-delta *pfwr-pf-recovery-gap-min*)
         (if (>= pf1 pf2) parent1 parent2))
        ;; Otherwise use blended PF/WR anchor decision.
        (t
         (select-pfwr-anchor-parent parent1 parent2))))))

(defun pfwr-underperformance-pressure (parent1 parent2)
  "Return 0..1 pressure for PF/WR mutation bias."
  (multiple-value-bind (pf-gap-ratio wr-gap-ratio)
      (pfwr-gap-profile parent1 parent2)
    ;; Non-linear amplification so medium deficits still get actionable pressure.
    (clamp-breeder-float (sqrt (max pf-gap-ratio wr-gap-ratio)) 0.0 1.0)))

(defun pfwr-effective-rr-cap (pf-gap-ratio wr-gap-ratio)
  "Return effective RR cap, tightening in stages when WR deficit dominates."
  (let ((wr-delta (- wr-gap-ratio pf-gap-ratio)))
    (cond
      ((<= wr-delta *pfwr-wr-recovery-gap-min*)
       *pfwr-max-rr*)
      ((>= wr-gap-ratio *pfwr-wr-recovery-severe-gap*)
       (min *pfwr-max-rr* *pfwr-wr-recovery-severe-cap-rr*))
      ((>= wr-gap-ratio *pfwr-wr-recovery-moderate-gap*)
       (min *pfwr-max-rr* *pfwr-wr-recovery-moderate-cap-rr*))
      (t
       (min *pfwr-max-rr* *pfwr-wr-recovery-max-rr*)))))

(defun pfwr-effective-rr-floor (pf-gap-ratio wr-gap-ratio)
  "Return effective RR floor, lifting RR when PF deficit dominates."
  (let ((pf-delta (- pf-gap-ratio wr-gap-ratio)))
    (if (>= pf-delta *pfwr-pf-recovery-gap-min*)
        (max *pfwr-min-rr* *pfwr-pf-recovery-min-rr*)
        *pfwr-min-rr*)))

(defun strategy-breeding-priority-score (strategy)
  "Composite parent priority score for breeding partner selection."
  (let* ((rank-bonus (case (strategy-rank strategy)
                       (:legend 3.0)
                       (:S 2.0)
                       (:A 1.0)
                       (t 0.0)))
         (base-score (if (and *breeder-priority-use-a-base-score*
                              (fboundp 'strategy-culling-score))
                         (strategy-culling-score strategy)
                         (score-from-metrics
                          (list :sharpe (strategy-sharpe strategy)
                                :profit-factor (strategy-profit-factor strategy)
                                :win-rate (strategy-win-rate strategy)
                                :max-dd (strategy-max-dd strategy)))))
         (generation-bonus (* (or (strategy-generation strategy) 0)
                              *breeder-priority-generation-weight*)))
    (+ rank-bonus base-score generation-bonus)))

(defun strategy-meets-target-pf-p (strategy &optional (target *pfwr-target-pf*))
  (>= (float (or (strategy-profit-factor strategy) 0.0))
      (float target)))

(defun strategy-meets-target-wr-p (strategy &optional (target *pfwr-target-wr*))
  (>= (float (or (strategy-win-rate strategy) 0.0))
      (float target)))

(defun breeding-partner-complement-bonus (parent candidate)
  "Bonus score when candidate complements parent's missing PF/WR side."
  (let* ((parent-needs-pf (not (strategy-meets-target-pf-p parent)))
         (parent-needs-wr (not (strategy-meets-target-wr-p parent)))
         (cand-has-pf (strategy-meets-target-pf-p candidate))
         (cand-has-wr (strategy-meets-target-wr-p candidate))
         (bonus 0.0))
    (when (and parent-needs-pf cand-has-pf)
      (incf bonus *breeder-complement-pf-bonus*))
    (when (and parent-needs-wr cand-has-wr)
      (incf bonus *breeder-complement-wr-bonus*))
    (when (and parent-needs-pf parent-needs-wr cand-has-pf cand-has-wr)
      (incf bonus *breeder-complement-double-bonus*))
    bonus))

(defun breeding-partner-score (parent candidate)
  "Rank candidate partner by base quality + PF/WR complement bonus."
  (+ (strategy-breeding-priority-score candidate)
     (breeding-partner-complement-bonus parent candidate)))

(defun candidate-complements-parent-p (parent candidate)
  "True when candidate satisfies at least one PF/WR side parent is missing."
  (let ((parent-needs-pf (not (strategy-meets-target-pf-p parent)))
        (parent-needs-wr (not (strategy-meets-target-wr-p parent))))
    (or (and parent-needs-pf (strategy-meets-target-pf-p candidate))
        (and parent-needs-wr (strategy-meets-target-wr-p candidate)))))

(defun apply-pfwr-mutation-bias (child-sl child-tp parent1 parent2)
  "Bias child SL/TP toward a healthier PF/WR profile while keeping risk budget constant."
  (let ((sl (float (or child-sl 0.0)))
        (tp (float (or child-tp 0.0))))
    (if (or (not *pfwr-mutation-bias-enabled*)
            (<= sl 0.0)
            (<= tp 0.0))
        (values child-sl child-tp)
        (let* ((pressure (pfwr-underperformance-pressure parent1 parent2))
               (blend (clamp-breeder-float (* *pfwr-mutation-bias-strength* pressure) 0.0 1.0)))
          (if (<= blend 0.0)
              (values sl tp)
              (multiple-value-bind (pf-gap-ratio wr-gap-ratio)
                  (pfwr-gap-profile parent1 parent2)
                (let* ((anchor (select-pfwr-anchor-parent parent1 parent2))
                     (anchor-sl (max 0.0001 (float (or (strategy-sl anchor) sl))))
                     (anchor-tp (max 0.0001 (float (or (strategy-tp anchor) tp))))
                     (current-rr (/ tp (max 0.0001 sl)))
                     (anchor-rr (/ anchor-tp anchor-sl))
                     (anchor-weight (clamp-breeder-float *pfwr-anchor-weight* 0.0 1.0))
                     (gap-delta (- pf-gap-ratio wr-gap-ratio))
                     (tilt-rr (clamp-breeder-float (+ *pfwr-neutral-rr*
                                                      (* gap-delta *pfwr-neutral-rr-span*))
                                                   *pfwr-min-rr* *pfwr-max-rr*))
                     (target-rr (clamp-breeder-float
                                 (+ (* anchor-weight anchor-rr)
                                    (* (- 1.0 anchor-weight) tilt-rr))
                                 *pfwr-min-rr* *pfwr-max-rr*))
                     (blended-rr (+ (* (- 1.0 blend) current-rr)
                                    (* blend target-rr)))
                     ;; Extra directional push: PF deficit => higher RR, WR deficit => lower RR.
                     (directional-rr (+ blended-rr
                                        (* blend gap-delta *pfwr-neutral-rr-span*)))
                     (rr-cap (pfwr-effective-rr-cap pf-gap-ratio wr-gap-ratio))
                     (rr-floor (min rr-cap
                                    (pfwr-effective-rr-floor pf-gap-ratio wr-gap-ratio)))
                     (final-rr (clamp-breeder-float directional-rr rr-floor rr-cap))
                     (risk-budget (+ sl tp))
                     (new-sl (/ risk-budget (+ 1.0 final-rr)))
                     (new-tp (- risk-budget new-sl)))
                  (values new-sl new-tp))))))))

(defun apply-pfwr-post-q-bias (child-sl child-tp parent1 parent2)
  "Re-apply PF/WR bias after Q-selection so exploit picks respect WR recovery."
  (if (or (not *pfwr-post-q-rebias-enabled*)
          (not *pfwr-mutation-bias-enabled*))
      (values child-sl child-tp)
      (let ((*pfwr-mutation-bias-strength*
              (max *pfwr-mutation-bias-strength* *pfwr-post-q-min-strength*)))
        (apply-pfwr-mutation-bias child-sl child-tp parent1 parent2))))

(defun breed-strategies (parent1 parent2)
  "Create a child strategy from two parents.
   V47.5: Enhanced with P3 graveyard avoidance.
   V47.7: Q-value guided SL/TP selection (20% exploit rate).
   V49.5: Regime-Aware Indicator Mutation."
  (let* ((next-gen (1+ (max (strategy-generation parent1) (strategy-generation parent2))))
         (child-name (format nil "Bred-~a-~a-Gen~d-N~a"
                             (subseq (strategy-name parent1) 0 (min 5 (length (strategy-name parent1))))
                             (random 1000)
                             next-gen
                             (breeder-name-entropy)))
         (tf (strategy-timeframe parent1))
         (dir (or (strategy-direction parent1) :BOTH))
         (sym (or (strategy-symbol parent1) "USDJPY"))
         (logic-parent (select-logic-anchor-parent parent1 parent2))
         (logic-entry (or (strategy-entry logic-parent)
                          (strategy-entry parent1)
                          (strategy-entry parent2)))
         (logic-exit (or (strategy-exit logic-parent)
                         (strategy-exit parent1)
                         (strategy-exit parent2)))
         ;; V49.0: Aggressive Mutation (0.1 -> 0.3)
         (initial-sl (mutate-value (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2.0) 0.3))
         (initial-tp (mutate-value (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2.0) 0.3))
         ;; V49.5: Smart Mutation for Indicators (Regime-Aware)
         (child-is (mutate-indicators-with-library 
                    (mutate-indicator-params (crossover-indicators parent1 parent2))
                    (strategy-category parent1)))
         ;; V47.5: Get avoid regions from graveyard analysis
         (avoid-regions (when (fboundp 'analyze-graveyard-for-avoidance)
                          (analyze-graveyard-for-avoidance)))
         ;; Check if SL/TP falls in avoid region, regenerate if needed
         (child-sl initial-sl)
         (child-tp initial-tp))

    ;; PF/WR-aware mutation bias: nudge child RR toward better parent profile.
    (multiple-value-bind (biased-sl biased-tp)
        (apply-pfwr-mutation-bias child-sl child-tp parent1 parent2)
      (setf child-sl biased-sl
            child-tp biased-tp))
    
    ;; V47.5: Regenerate SL/TP if in avoid region (up to 3 attempts)
    (when (and avoid-regions (fboundp 'should-avoid-params-p))
      (dotimes (i 3)
        (when (should-avoid-params-p child-sl child-tp avoid-regions)
          (format t "[BREEDER] 🚫 SL=~d TP=~d in avoid region, regenerating...~%" 
                  (round child-sl) (round child-tp))
          (setf child-sl (mutate-value initial-sl 0.2))  ; Larger mutation
          (setf child-tp (mutate-value initial-tp 0.2)))))
    
    ;; V47.7: Q-value guided selection (20% exploit, 80% explore)
    ;; Musk Condition: Only apply to breeding, not Scout
    (when (fboundp 'select-sltp-with-q)
      (multiple-value-bind (q-sl q-tp) 
          (select-sltp-with-q tf dir sym child-sl child-tp)
        (setf child-sl q-sl)
        (setf child-tp q-tp)))

    ;; Q-table exploit can reintroduce extreme RR; re-apply PF/WR recovery guard.
    (multiple-value-bind (post-q-sl post-q-tp)
        (apply-pfwr-post-q-bias child-sl child-tp parent1 parent2)
      (setf child-sl post-q-sl
            child-tp post-q-tp))
    
    (make-strategy
      :name child-name
      :category (strategy-category parent1) ;; Inherit from P1
      :timeframe tf
      :direction dir
      :symbol sym
      :generation next-gen
      :sl child-sl
      :tp child-tp
      :volume 0.01
      :indicators child-is
      ;; Logic anchor is selected dynamically from PF/WR deficit profile.
      :entry logic-entry
      :exit logic-exit
      :rank :incubator
      :tier :incubator ;; Legacy storage tier
      :status :active
      :parents (list (strategy-name parent1) (strategy-name parent2)))))


(defun find-diverse-breeding-partner (parent sorted-candidates &key (start-index 0) used-names)
  "Find best viable partner (quality + PF/WR complement), skipping similar/used candidates."
  (let ((best nil)
        (best-score most-negative-double-float)
        (best-complement nil)
        (best-complement-score most-negative-double-float)
        (parent-needs-complement (or (not (strategy-meets-target-pf-p parent))
                                     (not (strategy-meets-target-wr-p parent)))))
    (loop for idx from start-index below (length sorted-candidates)
          for candidate = (nth idx sorted-candidates)
          when (and candidate
                    (not (eq candidate parent))
                    (or (null used-names)
                        (null (gethash (strategy-name candidate) used-names)))
                    (can-breed-p candidate)
                    (strategies-correlation-ok-p parent candidate))
            do (let ((score (breeding-partner-score parent candidate))
                     (complements-p (candidate-complements-parent-p parent candidate)))
                 (when (> score best-score)
                   (setf best candidate
                         best-score score))
                 (when (and complements-p (> score best-complement-score))
                   (setf best-complement candidate
                         best-complement-score score))))
    (if (and *breeder-prioritize-complement-partner*
             parent-needs-complement
             best-complement)
        best-complement
        best)))



(defun run-breeding-cycle ()
  "Breed top strategies from ALL tiers, prioritizing higher generations.
   V45.0: Fixed to allow multi-generational evolution (Gen45+ possible).
   V47.0: Added breeding count limits (3 uses) and parent/child competition.
   V49.0: EVOLUTION INTENSIFICATION (Expert Panel).
   - Max pairs increased: 5 -> 20
   - Mutation rate increased: 10% -> 30%"
  (format t "[BREEDER] 🧬 Starting Breeding Cycle (V49.0 INTENSIFIED)...~%")
  (let ((categories '(:trend :reversion :breakout :scalp))
        (max-pairs-per-category 20)) ;; EXPLOSION: 4x throughput
    (dolist (cat categories)
      (let* ((all-warriors (remove-if-not
                            (lambda (s)
                              (and (eq (strategy-category s) cat)
                                   (not (eq (strategy-rank s) :graveyard))))
                            *strategy-knowledge-base*))
             ;; V48.7: Rank-aware and composite score priority. 
             (sorted (sort (copy-list all-warriors) #'>
                           :key #'strategy-breeding-priority-score)))
        
        ;; V50.2: Enforce Pool Size (Musk's "20 or Die")
        (cull-pool-overflow cat)

	        (let ((used-names (make-hash-table :test 'equal))
	              (pair-index 0))
	          (loop for i from 0 below (length sorted)
	                while (< pair-index max-pairs-per-category)
	                for p1 = (nth i sorted)
	                do (when (and p1
	                              (null (gethash (strategy-name p1) used-names))
	                              (can-breed-p p1))
	                     (let ((p2 (find-diverse-breeding-partner
	                                p1 sorted
	                                :start-index (1+ i)
	                                :used-names used-names)))
	                       (when p2
	                         (incf pair-index)
	                         (setf (gethash (strategy-name p1) used-names) t
	                               (gethash (strategy-name p2) used-names) t)
	                         (format t "[BREEDER] 💕 Breeding Pair ~d (~a): Gen~d ~a (S=~,2f) + Gen~d ~a (S=~,2f)~%"
	                                 pair-index cat
	                                 (or (strategy-generation p1) 0) (strategy-name p1) (or (strategy-sharpe p1) 0)
	                                 (or (strategy-generation p2) 0) (strategy-name p2) (or (strategy-sharpe p2) 0))
	                         (let ((child (breed-strategies p1 p2)))
	                           (increment-breeding-count p1)
	                           (increment-breeding-count p2)
	                           
	                           ;; V49.2: Inherit Regime Intent
	                           (setf (strategy-regime-intent child) (or (when (boundp '*current-regime*) *current-regime*) :unknown))
	                           
	                           ;; Add to KB (No Backtest req for Incubator -> will be screened in Phase 1)
	                           (when (add-to-kb child :breeder :require-bt nil :notify nil)
	                             (save-recruit-to-lisp child)
	                             (format t "[BREEDER] 👶 Born: ~a (Gen~d)~%" (strategy-name child) (strategy-generation child))
	                             
	                             ;; V50.2: Immediate Culling (Survival of the Fittest)
	                             ;; If pool > 20, kill the weakest B-Rank to make room
	                             (cull-pool-overflow cat))))))))))))

(defun cull-pool-overflow (category)
  "Enforce Musk's '20 or Die' rule. 
   If pool size > *b-rank-pool-size*, kill the weakest."
  (let* ((pool (gethash category *category-pools*))
         (limit (if (boundp '*b-rank-pool-size*) *b-rank-pool-size* 20))
         (survivors nil)
         (victims nil))
    (when (> (length pool) limit)
      ;; Sort by composite score (High to Low)
      (let ((sorted (sort (copy-list pool) #'>
                          :key (lambda (s)
                                 (score-from-metrics
                                  (list :sharpe (strategy-sharpe s)
                                        :profit-factor (strategy-profit-factor s)
                                        :win-rate (strategy-win-rate s)
                                        :max-dd (strategy-max-dd s)))))))
        (setf survivors (subseq sorted 0 limit))
        (setf victims (subseq sorted limit))
        
        ;; Update Pool
        (setf (gethash category *category-pools*) survivors)
        
        ;; Kill Victims
        (dolist (victim victims)
          (format t "[DEATHMATCH] 💀 Killing Weakest: ~a (Sharpe: ~,2f)~%" 
                  (strategy-name victim) (strategy-sharpe victim))
          ;; Notify Discord (Musk Requirement)
          (notify-death victim "Pool Overflow (Weakest Link)")
          ;; Remove from KB (Graveyard logic could be added here)
          (setf *strategy-knowledge-base* (delete victim *strategy-knowledge-base*))
          ;; Remove from SQL
          (handler-case
              (progn
                (init-db)
                (execute-non-query "UPDATE strategies SET rank = ':GRAVEYARD' WHERE name = ?" (strategy-name victim)))
            (error () nil)))))))

(defun notify-death (strat reason)
  "Musk: 'I want to see Death.'"
  (swimmy.core:notify-discord-recruit
   (format nil "💀 **STRATEGY EXECUTION**
━━━━━━━━━━━━━━━━━━━━━━
⚰️ Victim: `~a`
📉 Sharpe: ~,2f
⚖️ Verdict: ~a
🛑 Status: TERMINATED"
           (strategy-name strat) (or (strategy-sharpe strat) 0) reason)
   :color 0)) ; Black/Dark for Death

;;; ----------------------------------------------------------------------------
;;; Phase 6b: Persistence Implementation
;;; ----------------------------------------------------------------------------

(defun save-recruit-to-lisp (strat)
  "Save the new strategy to The Great Library (Sharded Persistence)."
  (swimmy.persistence:save-strategy strat)
  (format t "[PERSIST] 💾 Saved recruited strategy ~a to Library~%" (strategy-name strat)))

;;; ----------------------------------------------------------------------------
;; P8: recruit-elite-strategy (Scout) DELETED

;;; ----------------------------------------------------------------------------
;;; Phase 13: Wisdom Native (Civilization Handover)
;;; ----------------------------------------------------------------------------

(defun extract-params-from-strategy (strat)
  "Convert a strategy struct into an optimized-param plist."
  (list :name (strategy-name strat)
        :timeframe (strategy-timeframe strat)
        :sl (strategy-sl strat)
        :tp (strategy-tp strat)
        ;; Extract Indicators (Reverse engineering the strings)
        ;; This is a simplification: We assume SMA-S and SMA-L format for now.
        :indicators (strategy-indicators strat)))
        ;; TODO: Parse indicators more robustly if needed for gene regeneration

(defun save-optimized-params-to-file (params-list)
  "Save the optimized parameters to school-optimized-params.lisp."
  (let ((filepath "src/lisp/school/school-optimized-params.lisp"))
    (with-open-file (stream filepath
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream ";;; school-optimized-params.lisp~%")
      (format stream ";;; GENERATED BY LISP WISDOM ENGINE (analyze-veterans)~%")
      (format stream ";;; DO NOT EDIT MANUALLY.~%")
      (format stream ";;; Generated at: ~a~%~%" (swimmy.shell:get-date-string))
      (format stream "(in-package :swimmy.school)~%~%")
      (format stream "(defparameter *optimized-params*~%")
      (format stream "  '(~%")
      (dolist (p params-list)
        (format stream "    ~s~%" p))
      (format stream "   ))~%"))
    (format t "[WISDOM] 💾 Saved ~d veteran genes to ~a~%" (length params-list) filepath)))

;; V49.2: Strategy Correlation Check (Taleb's Safety Guard)
(defun strategies-correlation-ok-p (p1 p2)
  "Check if two parents are too similar (Clone Prevention).
   Returns NIL if genetic distance is too small."
  (let ((dist (calculate-genetic-distance (extract-genome p1) (extract-genome p2))))
    (if (< dist 0.2)
        (progn
          (format t "[BREEDER] 🚫 Pair too similar (Dist: ~,2f). Skipping Jackpotクローン.~%" dist)
          nil)
        t)))

(defun analyze-veterans ()
  "Analyze the Knowledge Base and extract 'Wisdom' (Best Genes).
   Replaces extract_wisdom.py."
  (format t "[WISDOM] 🧠 Analyzing Veterans for Gene Extraction...~%")
  (let* ((total (length *strategy-knowledge-base*)))
    (format t "[WISDOM] 🔍 De-duplication starting: ~d strategies...~%" total)
    (let* ((t0 (get-internal-real-time))
           (unique-strats (remove-duplicates *strategy-knowledge-base* :key #'strategy-name :test #'string=))
           (dedup-secs (/ (- (get-internal-real-time) t0)
                          internal-time-units-per-second)))
      (format t "[WISDOM] ✅ De-dup complete: ~d unique (~,2fs)~%" (length unique-strats) dedup-secs)
      ;; Filter: Positive Sharpe only, or just take top N?
      ;; For now, let's take anyone with Sharpe > 0.1 to allow early evolution.
      (format t "[WISDOM] 🔍 Filtering candidates (Sharpe > 0.1) from ~d...~%" (length unique-strats))
      (let* ((t1 (get-internal-real-time))
             (candidates (remove-if-not (lambda (s) (and (strategy-sharpe s) (> (strategy-sharpe s) 0.1)))
                                        unique-strats))
             (filter-secs (/ (- (get-internal-real-time) t1)
                             internal-time-units-per-second)))
        (format t "[WISDOM] ✅ Filter complete: ~d candidates (~,2fs)~%" (length candidates) filter-secs)
        ;; Sort by Sharpe
        (format t "[WISDOM] 🔍 Sorting candidates by Sharpe...~%")
        (let* ((t2 (get-internal-real-time))
               (best (sort (copy-list candidates) #'> :key #'strategy-sharpe))
               (sort-secs (/ (- (get-internal-real-time) t2)
                             internal-time-units-per-second)))
          (format t "[WISDOM] ✅ Sort complete (~,2fs)~%" sort-secs)
          ;; Take Top 50
          (let* ((elite (subseq best 0 (min (length best) 50)))
                 (genes (mapcar #'extract-params-from-strategy elite)))
            (format t "[WISDOM] Found ~d candidates (Sharpe > 0.1). Extracting ~d Elite Genes.~%"
                    (length candidates) (length elite))
            (if genes
                (save-optimized-params-to-file genes)
                (format t "[WISDOM] ⚠️ No eligible veterans found. Keeping existing genes.~%"))
            (length genes)))))))

;;; ============================================================================
;;; DEATHMATCH ARENA (Phase 21)
;;; ============================================================================

(defun compete-parent-child (parent child)
  "Parent vs Child Deathmatch (Expert Panel 2026-01-28).
   - If Child is proven stronger (Sharpe), Parent is killed (unless Immortal).
   - If Child is weaker, Child is killed.
   - If Child is untested (Sharpe NIL), it is spared for now."
   (let ((p-sharpe (or (strategy-sharpe parent) -999.0))
         (c-sharpe (strategy-sharpe child))) ;; Child might be untested
     
     (unless c-sharpe
       (format t "[DEATHMATCH] 🐣 Child ~a is untested. Spared for now.~%" (strategy-name child))
       (return-from compete-parent-child :child-untested))

     (cond
       ;; Child Stronger
       ((> c-sharpe p-sharpe)
        (if (strategy-immortal parent)
            (progn
               (format t "[DEATHMATCH] 🛡️ Parent ~a (Sharpe ~,2f) is IMMORTAL. Child ~a (Sharpe ~,2f) co-exists.~%"
                       (strategy-name parent) p-sharpe (strategy-name child) c-sharpe)
               :both-survive)
            (progn
               (format t "[DEATHMATCH] ⚔️ Child ~a (Sharpe ~,2f) KILLS Parent ~a (Sharpe ~,2f)!~%"
                       (strategy-name child) c-sharpe (strategy-name parent) p-sharpe)
               (kill-strategy (strategy-name parent) (format nil "Killed by Child ~a" (strategy-name child)))
               :child-wins)))
       
       ;; Parent Stronger or Equal
       (t
        (format t "[DEATHMATCH] ⚰️ Child ~a (Sharpe ~,2f) failed to surpass Parent ~a (Sharpe ~,2f). Terminated.~%"
                (strategy-name child) c-sharpe (strategy-name parent) p-sharpe)
        (kill-strategy (strategy-name child) (format nil "Failed to beat Parent ~a" (strategy-name parent)))
        :parent-wins))))

;;; ============================================================================
;;; BREEDING CYCLE CONTROLLER
;;; ============================================================================

(defun increment-strategy-ages ()
  "Increment age of all active strategies (Daily)"
  (dolist (s *strategy-knowledge-base*)
    (when (eq (strategy-status s) :active)
      (incf (strategy-age s))
      (when (strategy-immortal s)
        (format t "[AGE] 🛡️ Legendary ~a is Ageless (Age: ~d)~%" (strategy-name s) (strategy-age s))))))

(defvar *last-age-increment-day* nil "YYYYMMDD of last daily age increment")
(defvar *last-stagnant-crank-cull-day* nil "YYYYMMDD of last daily Stagnant C-Rank cull")
(defvar *last-max-age-retire-day* nil "YYYYMMDD of last daily max-age retire sweep")

(defun %day-key (&optional (now (get-universal-time)))
  (multiple-value-bind (s m h date month year) (decode-universal-time now)
    (declare (ignore s m h))
    (+ (* year 10000) (* month 100) date)))

(defun should-run-age-increment-p (day-key)
  (when (or (null *last-age-increment-day*)
            (/= day-key *last-age-increment-day*))
    (setf *last-age-increment-day* day-key)
    t))

(defun should-run-stagnant-crank-cull-p (day-key)
  (when (or (null *last-stagnant-crank-cull-day*)
            (/= day-key *last-stagnant-crank-cull-day*))
    (setf *last-stagnant-crank-cull-day* day-key)
    t))

(defun should-run-max-age-retire-p (day-key)
  (when (or (null *last-max-age-retire-day*)
            (/= day-key *last-max-age-retire-day*))
    (setf *last-max-age-retire-day* day-key)
    t))

(defun cull-stagnant-crank-daily ()
  "Cull Stagnant C-Rank strategies once per day."
  (format t "[CULL] 🧊 Daily Stagnant C-Rank Culling Initiated...~%")
  (dolist (s *strategy-knowledge-base*)
    (when (and (eq (strategy-status s) :active)
               (not (strategy-immortal s))
               (> (strategy-age s) 10))
      (let ((sharpe (or (strategy-sharpe s) -1.0)))
        (when (< sharpe 0.6)
          (kill-strategy (strategy-name s)
                         (format nil "Cull: Stagnant C-Rank (~,2f) after 10 days" sharpe)
                         :reason-code :stagnant-crank))))))

(defun cull-weak-strategies ()
  "Cull weak strategies (Rank C/D) that are older than 5 days.
   (Daily, guarded)"
  (format t "[CULL] 🔪 Weekly Culling Initiated...~%")
  (dolist (s *strategy-knowledge-base*)
    (when (and (eq (strategy-status s) :active)
               (not (strategy-immortal s))
               (> (strategy-age s) 5))
      ;; Check Performance (using existing Grade logic or simple Sharpe)
      (let ((sharpe (or (strategy-sharpe s) -1.0)))
        (cond
          ((< sharpe 0.0) 
           (kill-strategy (strategy-name s) (format nil "Cull: Negative Sharpe (~,2f) after 5 days" sharpe))))))))

(defun retire-max-age-strategies ()
  "Retire active non-immortal strategies that exceed max age."
  (dolist (s *strategy-knowledge-base*)
    (when (and (eq (strategy-status s) :active)
               (not (strategy-immortal s))
               (> (strategy-age s) 30))
      (format t "[EVOLUTION] 👴 ~a reached Max Age (30). Forced Retirement/Breeding...~%"
              (strategy-name s))
      ;; Logic: Breed a child, then retire parent
      ;; For now, just retire to make room.
      (send-to-retired s "Max Age Retirement"))))

(defun process-breeding-cycle (&key (now (get-universal-time)))
  "Main Entry Point: Aging, Culling, and Breeding.
   Called by Morning Ritual."
  (format t "[EVOLUTION] 🧬 Processing Breeding Cycle...~%")
  
  ;; 1. Daily guards
  (let ((day-key (%day-key now)))
    (when (should-run-age-increment-p day-key)
      (increment-strategy-ages))

    ;; 1.5 Daily Stagnant C-Rank Culling (day-key guard)
    (when (should-run-stagnant-crank-cull-p day-key)
      (cull-stagnant-crank-daily))

    ;; 1.6 Daily Max Age Retirement (day-key guard)
    (when (should-run-max-age-retire-p day-key)
      (retire-max-age-strategies)))
  
  ;; 2. Culling (Weekly)
  ;; Morning Ritual is daily. Culling usually Fri Close or Sat Morning.
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time now)
    (declare (ignore s m h d mo y))
    (when (= dow 6) ;; Saturday
      (cull-weak-strategies))))
