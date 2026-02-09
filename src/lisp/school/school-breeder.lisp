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
(defun breed-strategies (parent1 parent2)
  "Create a child strategy from two parents.
   V47.5: Enhanced with P3 graveyard avoidance.
   V47.7: Q-value guided SL/TP selection (20% exploit rate).
   V49.5: Regime-Aware Indicator Mutation."
  (let* ((child-name (format nil "Bred-~a-~a-Gen~d" 
                             (subseq (strategy-name parent1) 0 (min 5 (length (strategy-name parent1))))
                             (random 1000)
                             (1+ (max (strategy-generation parent1) (strategy-generation parent2)))))
         (tf (strategy-timeframe parent1))
         (dir (or (strategy-direction parent1) :BOTH))
         (sym (or (strategy-symbol parent1) "USDJPY"))
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
    
    (make-strategy
      :name child-name
      :category (strategy-category parent1) ;; Inherit from P1
      :timeframe tf
      :direction dir
      :symbol sym
      :generation (1+ (max (strategy-generation parent1) (strategy-generation parent2)))
      :sl child-sl
      :tp child-tp
      :volume 0.01
      :indicators child-is
      ;; For now, inherit P1 entry/exit logic directly, but mutation should happen here too eventually
      :entry (strategy-entry parent1)
      :exit (strategy-exit parent1)
      :rank :incubator
      :tier :incubator ;; Legacy storage tier
      :status :active
      :parents (list (strategy-name parent1) (strategy-name parent2)))))



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
             ;; V48.7: Rank-aware and raw Sharpe priority. 
             (sorted (sort (copy-list all-warriors) #'> 
                           :key (lambda (s) 
                                  (+ (case (strategy-rank s) (:legend 3.0) (:S 2.0) (:A 1.0) (t 0.0))
                                     (or (strategy-sharpe s) 0)
                                     (* (or (strategy-generation s) 0) 0.01))))))
        
        ;; V50.2: Enforce Pool Size (Musk's "20 or Die")
        (cull-pool-overflow cat)

        (loop for i from 0 below (* 2 max-pairs-per-category) by 2
              while (< (1+ i) (length sorted))
              for p1 = (nth i sorted)
              for p2 = (nth (1+ i) sorted)
              do (when (and (can-breed-p p1) (can-breed-p p2))
                   (format t "[BREEDER] 💕 Breeding Pair ~d (~a): Gen~d ~a (S=~,2f) + Gen~d ~a (S=~,2f)~%"
                           (1+ (/ i 2)) cat
                           (or (strategy-generation p1) 0) (strategy-name p1) (or (strategy-sharpe p1) 0)
                           (or (strategy-generation p2) 0) (strategy-name p2) (or (strategy-sharpe p2) 0))
                   
                   ;; V49.2: Correlation Check
                   (when (strategies-correlation-ok-p p1 p2)
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
                         (cull-pool-overflow cat))))))))))

(defun cull-pool-overflow (category)
  "Enforce Musk's '20 or Die' rule. 
   If pool size > *b-rank-pool-size*, kill the weakest."
  (let* ((pool (gethash category *category-pools*))
         (limit (if (boundp '*b-rank-pool-size*) *b-rank-pool-size* 20))
         (survivors nil)
         (victims nil))
    (when (> (length pool) limit)
      ;; Sort by Sharpe (High to Low)
      (let ((sorted (sort (copy-list pool) #'> :key (lambda (s) (or (strategy-sharpe s) -999)))))
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

(defparameter *last-cull-day-key* 0 "Day key of last cull execution")

(defun day-key-from-time (now)
  (multiple-value-bind (_s _m _h date month year) (decode-universal-time now)
    (declare (ignore _s _m _h))
    (+ (* year 10000) (* month 100) date)))

(defun maybe-cull-weak-strategies (&key (now (get-universal-time)))
  (let ((day-key (day-key-from-time now)))
    (unless (= day-key *last-cull-day-key*)
      (setf *last-cull-day-key* day-key)
      (cull-weak-strategies))))

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
           (kill-strategy (strategy-name s) (format nil "Cull: Negative Sharpe (~,2f) after 5 days" sharpe)))
          ((and (< sharpe 0.6) (> (strategy-age s) 10))
           (kill-strategy (strategy-name s) (format nil "Cull: Stagnant C-Rank (~,2f) after 10 days" sharpe))))))))

(defun process-breeding-cycle ()
  "Main Entry Point: Aging, Culling, and Breeding.
   Called by Morning Ritual."
  (format t "[EVOLUTION] 🧬 Processing Breeding Cycle...~%")
  
  ;; 1. Global Aging
  (increment-strategy-ages)
  
  ;; 2. Culling (Daily, guarded)
  (maybe-cull-weak-strategies)
  
  ;; 3. Forced Breeding (Old Age)
  (dolist (s *strategy-knowledge-base*)
    (when (and (eq (strategy-status s) :active)
               (not (strategy-immortal s))
               (> (strategy-age s) 30))
      (format t "[EVOLUTION] 👴 ~a reached Max Age (30). Forced Retirement/Breeding...~%" (strategy-name s))
      ;; Logic: Breed a child, then retire parent
      ;; For now, just Kill (Retire) to make room, assuming Breeding happens via other triggers
      ;; Or trigger a breed event here?
      (send-to-retired s "Max Age Retirement"))))
