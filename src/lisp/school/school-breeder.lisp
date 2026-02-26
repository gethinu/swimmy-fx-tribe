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
(defparameter *pfwr-mutation-bias-strength* 0.7
  "Blend strength (0..1) for PF/WR mutation bias.")
(defparameter *pfwr-target-pf* 1.30
  "Target PF used to determine if parents are underperforming.")
(defparameter *pfwr-target-wr* 0.43
  "Target WR used to determine if parents are underperforming.")
(defparameter *pfwr-s-target-bias-enabled* t
  "When T, PF/WR mutation bias uses S-rank PF/WR gates as targets when BOTH parents are A-rank or above.
Keeps B-rank throughput focused on A gates while pushing A/S genetics toward S readiness.")
(defparameter *pfwr-s-target-pf* 1.70
  "Target PF used for S-target PF/WR mutation bias (A+ parents only).")
(defparameter *pfwr-s-target-wr* 0.50
  "Target WR used for S-target PF/WR mutation bias (A+ parents only).")
(defparameter *pfwr-min-rr* 0.4
  "Lower bound for TP/SL ratio after PF/WR bias.")
(defparameter *pfwr-max-rr* 4.0
  "Upper bound for TP/SL ratio after PF/WR bias.")
(defparameter *pfwr-wr-recovery-max-rr* 2.2
  "RR cap used when WR deficit dominates PF deficit.")
(defparameter *pfwr-wr-recovery-gap-min* 0.02
  "Minimum WR-vs-PF gap delta before WR-recovery RR cap engages.")
(defparameter *pfwr-wr-recovery-moderate-gap* 0.08
  "WR gap ratio threshold for moderate RR cap tightening.")
(defparameter *pfwr-wr-recovery-severe-gap* 0.12
  "WR gap ratio threshold for aggressive RR cap tightening.")
(defparameter *pfwr-wr-recovery-moderate-cap-rr* 1.8
  "RR cap when WR gap is moderately above target.")
(defparameter *pfwr-wr-recovery-severe-cap-rr* 1.6
  "RR cap when WR gap is severely above target.")
(defparameter *pfwr-pf-recovery-gap-min* 0.02
  "PF-vs-WR gap delta required before PF recovery RR floor engages.")
(defparameter *pfwr-pf-recovery-min-rr* 2.1
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
(defparameter *pfwr-complement-min-pressure* 0.35
  "Minimum PF/WR pressure applied when parents are opposite complements (PF-only x WR-only).")
(defparameter *pfwr-complement-stabilize-min-rr* 1.55
  "Lower RR bound used to stabilize opposite-complement pair offspring.")
(defparameter *pfwr-complement-stabilize-max-rr* 1.70
  "Upper RR bound used to stabilize opposite-complement pair offspring.")
(defparameter *pfwr-complement-pf-recovery-min-rr* 1.75
  "Lower RR bound for opposite-complements when WR is already ready but PF still lags.")
(defparameter *pfwr-complement-pf-recovery-max-rr* 2.05
  "Upper RR bound for opposite-complements when WR is already ready but PF still lags.")
(defparameter *pfwr-pf-recovery-scale-gain* 1.1
  "SL/TP scale gain for PF-dominant deficits (increases absolute move distance).")
(defparameter *pfwr-pf-recovery-scale-max* 1.70
  "Maximum SL/TP scale multiplier from PF recovery expansion.")
(defparameter *pfwr-upside-scale-enabled* t
  "When T, WR-ready parents with PF below S-like target get additional scale boost.")
(defparameter *pfwr-upside-target-pf* 1.70
  "Secondary PF target used to push S-gate readiness without touching rank thresholds.")
(defparameter *pfwr-upside-min-wr* 0.47
  "Minimum average parent WR required before upside PF scale boost can activate.")
(defparameter *pfwr-upside-scale-gain* 0.30
  "Maximum additive scale boost (over 1.0) for upside PF recovery mode.")
(defparameter *pfwr-upside-scale-max* 1.40
  "Upper bound of upside PF recovery scale floor.")
(defparameter *pfwr-severe-low-pf-threshold* 1.15
  "Average parent PF threshold that triggers severe PF recovery mode.")
(defparameter *pfwr-severe-wr-ready-threshold* 0.43
  "Average parent WR threshold required to trigger severe PF recovery mode.")
(defparameter *pfwr-severe-min-rr* 2.25
  "RR floor enforced in severe PF recovery mode.")
(defparameter *pfwr-severe-scale-floor* 1.30
  "Minimum SL/TP scale multiplier enforced in severe PF recovery mode.")
(defparameter *pfwr-complement-scale-floor* 1.40
  "Minimum SL/TP scale multiplier for opposite-complement pair stabilization.")
(defparameter *pfwr-wr-only-scale-floor* 1.40
  "Minimum SL/TP scale multiplier when WR deficit is already resolved but PF is still below target.")
(defparameter *breeder-priority-use-a-base-score* t
  "When T, breeder parent ranking uses A-base-aware culling score if available.")
(defparameter *breeder-priority-generation-weight* 0.01
  "Generation bonus weight in breeder parent ranking.")
(defparameter *breeder-priority-cpcv-pass-rate-weight* 0.35
  "Additive bonus weight for CPCV pass-rate in breeder parent ranking. 0 disables.")
(defparameter *breeder-priority-rank-bonus-weight* 0.25
  "Weight applied to raw rank bonus so rank does not dominate weak edge metrics.")
(defparameter *breeder-priority-oos-sharpe-weight* 0.35
  "Additive bonus weight for OOS Sharpe in breeder parent ranking.")
(defparameter *breeder-priority-oos-sharpe-floor* -0.20
  "OOS Sharpe lower bound used for bonus normalization.")
(defparameter *breeder-priority-oos-sharpe-ceiling* 1.20
  "OOS Sharpe upper bound used for bonus normalization.")
(defparameter *breeder-priority-oos-bonus-requires-a-base* t
  "When T, A-base未達の親には OOS bonus を減衰適用する（OOS単独優先の抑制）。")
(defparameter *breeder-priority-oos-bonus-pre-a-scale* 0.20
  "Multiplier (0..1) applied to OOS bonus for parents that fail A-base criteria.")
(defparameter *breeder-priority-threshold-penalty-weight* 0.80
  "Global multiplier for breeder priority threshold deficits.")
(defparameter *breeder-priority-min-sharpe* 0.45
  "Minimum A-base Sharpe expected for breeding parent priority.")
(defparameter *breeder-priority-min-pf* 1.30
  "Minimum A-base PF expected for breeding parent priority.")
(defparameter *breeder-priority-min-wr* 0.43
  "Minimum A-base WR expected for breeding parent priority.")
(defparameter *breeder-priority-max-dd* 0.16
  "Maximum A-base maxdd expected for breeding parent priority.")
(defparameter *breeder-priority-min-oos-sharpe* 0.35
  "Minimum OOS Sharpe expected for breeder edge confidence.")
(defparameter *breeder-priority-penalty-weight-sharpe* 0.25
  "Weight for Sharpe deficit in breeder threshold penalty.")
(defparameter *breeder-priority-penalty-weight-pf* 0.30
  "Weight for PF deficit in breeder threshold penalty.")
(defparameter *breeder-priority-penalty-weight-wr* 0.20
  "Weight for WR deficit in breeder threshold penalty.")
(defparameter *breeder-priority-penalty-weight-maxdd* 0.15
  "Weight for maxdd excess in breeder threshold penalty.")
(defparameter *breeder-priority-penalty-weight-oos* 0.10
  "Weight for OOS Sharpe deficit in breeder threshold penalty.")
(defparameter *breeder-complement-wr-bonus* 1.5
  "Partner score bonus when candidate satisfies WR target that parent is missing.")
(defparameter *breeder-complement-pf-bonus* 1.5
  "Partner score bonus when candidate satisfies PF target that parent is missing.")
(defparameter *breeder-complement-double-bonus* 0.75
  "Extra bonus when candidate satisfies both missing PF and WR targets.")
(defparameter *breeder-complement-min-pf-when-needs-wr* 1.18
  "Minimum PF required for WR-complement candidates to avoid PF collapse pairings.")
(defparameter *breeder-complement-min-pf-floor* 1.08
  "Hard lower bound for WR-complement PF threshold after dynamic relaxation.")
(defparameter *breeder-complement-parent-pf-relax-gain* 0.60
  "How much parent PF surplus relaxes WR-complement minimum PF threshold.")
(defparameter *breeder-complement-cand-wr-relax-gain* 0.50
  "How much candidate WR surplus relaxes WR-complement minimum PF threshold.")
(defparameter *breeder-complement-min-wr-when-needs-pf* 0.38
  "Minimum WR required for PF-complement candidates to avoid over-fragile pairings.")
(defparameter *breeder-near-pf-threshold* 1.18
  "PF threshold treated as near-target when parent mainly needs PF recovery.")
(defparameter *breeder-near-pf-min-wr* 0.43
  "Minimum WR required for near-PF candidates to be considered PF-recovery partners.")
(defparameter *breeder-near-pf-bonus* 1.6
  "Partner score bonus for near-target PF candidates when parent needs PF.")
(defparameter *breeder-wr-only-recovery-min-pf-delta* 0.02
  "Minimum PF improvement required to treat WR-only partner as PF-recovery upgrade.")
(defparameter *breeder-wr-only-recovery-bonus* 0.8
  "Extra score bonus for WR-only x WR-only PF-recovery pairings.")
(defparameter *breeder-prioritize-complement-partner* t
  "When T, prioritize partners that satisfy at least one PF/WR deficit of the parent.")
(defparameter *breeder-min-genetic-distance* 0.02
  "Minimum genetic distance required for a breeding pair (lower allows more diversity attempts).")
(defparameter *breeder-min-genetic-distance-complement* 0.01
  "Relaxed min distance for complement partners (PF/WR opposite-side recovery).")
(defparameter *breeder-min-genetic-distance-partial-recovery* 0.015
  "Relaxed min distance for partial PF/WR recovery candidates.")
(defparameter *breeder-partial-recovery-min-pf-delta* 0.01
  "Minimum PF improvement required to treat a candidate as partial PF recovery.")
(defparameter *breeder-partial-recovery-min-wr-delta* 0.02
  "Minimum WR improvement required to treat a candidate as partial WR recovery.")
(defparameter *breeder-sltp-parent-multiplier-cap* 2.0
  "Maximum multiplier for child SL/TP relative to the stronger parent envelope.")
(defparameter *breeder-pair-blacklist-enabled* t
  "When T, temporarily blacklist parent pairs that repeatedly fail breeder admission.")
(defparameter *breeder-pair-failure-threshold* 1
  "Consecutive add-to-kb failures required before a parent pair is blacklisted.")
(defparameter *breeder-pair-blacklist-cooldown-seconds* 1800
  "Blacklist cooldown duration in seconds before blocked parent pair can be retried.")
(defvar *breeder-pair-failure-stats* (make-hash-table :test 'equal)
  "Pair failure telemetry keyed by canonical parent-name pair.")
(defvar *breeder-current-pair-min-distance* nil
  "Dynamically scoped per-pair min-distance override for correlation gate.")
(defparameter *breeder-name-seq* 0
  "Monotonic per-process sequence for collision-resistant child naming.")
(defparameter *breeder-timeframe-crossover-rate* 0.5
  "Chance (0..1) to inherit parent2 timeframe when parents differ.")
(defparameter *breeder-timeframe-mutation-rate* 0.15
  "Low-frequency chance (0..1) to mutate child timeframe after crossover.")
(defparameter *breeder-timeframe-mutation-same-parent-rate* 0.35
  "Minimum mutation rate (0..1) when both parents share the same timeframe.")
(defparameter *breeder-timeframe-diversity-bonus* 1.0
  "Additive partner score bonus when candidate timeframe differs from parent.")
(defparameter *breeder-timeframe-fallback-minutes* 60
  "Fallback timeframe (minutes) when parent timeframe is missing/invalid.")
(defparameter *breeder-timeframe-excluded-minutes* '(5)
  "Breeder child TF policy exclusions (minutes). Default excludes M5 from S-flow lanes.")
(defparameter *breeder-timeframe-priority-minutes* '(60 300)
  "Breeder child TF policy priorities (minutes). Default prefers H1/H5 lanes.")
(defparameter *breeder-timeframe-short-parent-max-minutes* 30
  "Short-parent threshold (minutes). Used to suppress M15/M30 lock-in in S-flow.")
(defparameter *breeder-timeframe-short-parent-mutation-floor* 0.55
  "Minimum mutation rate (0..1) when both parents are short timeframe.")
(defparameter *breeder-timeframe-short-parent-priority-bonus* 0.75
  "Additional partner score bonus when short-TF parent pairs with priority TF lane (e.g. H1/H5).")
(defparameter *breeder-timeframe-short-parent-short-penalty* 0.75
  "Partner score penalty when short-TF parent pairs again with short TF candidate.")
(defparameter *breeder-timeframe-prioritize-underrepresented* t
  "When T, TF mutation prefers the least-populated timeframe among candidates.")
(defvar *breeder-active-timeframe-counts-cache* nil
  "Dynamically bound cache of active timeframe counts for current breeding cycle.")

(defun breeder-name-entropy ()
  "Generate high-entropy suffix for child strategy names."
  (format nil "~d-~d" (get-universal-time) (incf *breeder-name-seq*)))

(defun clamp-breeder-float (value low high)
  (max low (min high value)))

(defun normalize-breeder-timeframe (tf &optional (fallback *breeder-timeframe-fallback-minutes*))
  "Normalize timeframe to positive minute integer."
  (let* ((resolved (cond
                     ((and (fboundp 'get-tf-minutes))
                      (ignore-errors (get-tf-minutes tf)))
                     ((numberp tf) tf)
                     (t nil)))
         (candidate (if (and (numberp resolved) (> resolved 0))
                        resolved
                        fallback)))
    (max 1 (round (or candidate 60)))))

(defun maybe-normalize-breeder-timeframe (tf)
  "Normalize timeframe to minutes(int) or return NIL when TF cannot be resolved."
  (let ((resolved (cond
                    ((and (fboundp 'get-tf-minutes))
                     (ignore-errors (get-tf-minutes tf)))
                    ((numberp tf) tf)
                    (t nil))))
    (when (and (numberp resolved) (> resolved 0))
      (round resolved))))

(defun normalize-breeder-timeframe-list (items)
  "Normalize and deduplicate timeframe list without fallback insertion."
  (remove-duplicates
   (loop for item in (or items '())
         for normalized = (maybe-normalize-breeder-timeframe item)
         when normalized collect normalized)
   :test #'eql))

(defun breeder-short-timeframe-p (tf)
  "Return T when TF is within configured short-parent threshold."
  (let ((threshold (or (maybe-normalize-breeder-timeframe
                        *breeder-timeframe-short-parent-max-minutes*)
                       30)))
    (and (numberp tf)
         (<= tf threshold))))

(defun breeder-priority-timeframe-p (tf)
  "Return T when TF is in configured priority lane."
  (let ((priority (normalize-breeder-timeframe-list
                   *breeder-timeframe-priority-minutes*)))
    (member tf priority :test #'eql)))

(defun breeder-timeframe-policy-candidates (candidates)
  "Apply breeder timeframe exclusions and priority ordering."
  (let* ((normalized (normalize-breeder-timeframe-list candidates))
         (excluded (normalize-breeder-timeframe-list
                    *breeder-timeframe-excluded-minutes*))
         (allowed (remove-if (lambda (tf)
                               (member tf excluded :test #'eql))
                             normalized))
         (priority-order (normalize-breeder-timeframe-list
                          *breeder-timeframe-priority-minutes*))
         (priority (remove-if-not (lambda (tf)
                                    (member tf allowed :test #'eql))
                                  priority-order))
         (rest (remove-if (lambda (tf)
                            (member tf priority :test #'eql))
                          allowed)))
    (append priority rest)))

(defun breeder-timeframe-mutation-options ()
  "Return bounded TF mutation candidates in minutes."
  (let ((raw-options (if (fboundp 'get-tf-mutation-options)
                         (ignore-errors (get-tf-mutation-options))
                         nil)))
    (or (breeder-timeframe-policy-candidates
         (or (and (listp raw-options) raw-options)
             '(5 15 30 60 240 1440 10080 43200)))
        (breeder-timeframe-policy-candidates '(60 300 15 30 240 1440 10080 43200))
        '(60 300))))

(defun breeder-active-rank-p (rank)
  "Return T when rank is considered active for breeder TF balancing."
  (member rank '(:B :A :S :SCOUT :LEGEND) :test #'eq))

(defun collect-breeder-active-timeframe-counts ()
  "Count active strategy timeframes from in-memory KB."
  (let ((counts (make-hash-table :test #'eql)))
    (dolist (strategy *strategy-knowledge-base* counts)
      (when (and (strategy-p strategy)
                 (breeder-active-rank-p (strategy-rank strategy)))
        (incf (gethash (normalize-breeder-timeframe (strategy-timeframe strategy))
                       counts
                       0))))))

(defun pick-breeder-timeframe-mutation-candidate (candidates &optional counts)
  "Pick mutation timeframe, preferring least-populated candidates when enabled."
  (let ((pool (remove-duplicates (or candidates '()) :test #'eql)))
    (cond
      ((null pool) nil)
      ((or (not *breeder-timeframe-prioritize-underrepresented*)
           (= (length pool) 1))
       (nth (random (length pool)) pool))
      (t
       (let* ((resolved-counts (or counts
                                   *breeder-active-timeframe-counts-cache*
                                   (collect-breeder-active-timeframe-counts)))
              (min-count most-positive-fixnum)
              (best nil))
         (dolist (tf pool)
           (let ((n (if (hash-table-p resolved-counts)
                        (gethash tf resolved-counts 0)
                        0)))
             (cond
               ((< n min-count)
                (setf min-count n
                      best (list tf)))
               ((= n min-count)
                (push tf best)))))
         (if (and best (> (length best) 1))
             (nth (random (length best)) best)
             (car best)))))))

(defun select-breeder-child-timeframe (parent1 parent2)
  "Select child timeframe with crossover + low-frequency mutation for TF diversity."
  (let* ((p1-tf (normalize-breeder-timeframe (strategy-timeframe parent1)))
         (p2-tf (normalize-breeder-timeframe (strategy-timeframe parent2) p1-tf))
         (crossover-rate (clamp-breeder-float
                          (float (or *breeder-timeframe-crossover-rate* 0.0))
                          0.0
                          1.0))
         (base-mutation-rate (clamp-breeder-float
                              (float (or *breeder-timeframe-mutation-rate* 0.0))
                              0.0
                              1.0))
         (same-parent-rate (clamp-breeder-float
                            (float (or *breeder-timeframe-mutation-same-parent-rate* 0.0))
                            0.0
                            1.0))
         (short-parent-floor (clamp-breeder-float
                              (float (or *breeder-timeframe-short-parent-mutation-floor* 0.0))
                              0.0
                              1.0))
         (short-parents-p (and (breeder-short-timeframe-p p1-tf)
                               (breeder-short-timeframe-p p2-tf)))
         (base-selected-mutation-rate (if (= p1-tf p2-tf)
                                          (max base-mutation-rate same-parent-rate)
                                          base-mutation-rate))
         (mutation-rate (if short-parents-p
                            (max base-selected-mutation-rate short-parent-floor)
                            base-selected-mutation-rate))
         (crossover-p (and (/= p1-tf p2-tf)
                           (< (random 1.0) crossover-rate)))
         (base-tf (if crossover-p p2-tf p1-tf))
         (mutation-p (< (random 1.0) mutation-rate))
         (pool (breeder-timeframe-policy-candidates
                (append (list p1-tf p2-tf)
                        (breeder-timeframe-mutation-options))))
         (safe-pool (or pool '(60 300)))
         (policy-base (if (member base-tf safe-pool :test #'eql)
                          base-tf
                          (car safe-pool)))
         (base-policy-changed-p (/= policy-base base-tf))
         (mutation-candidates (remove policy-base safe-pool :test #'eql)))
    (labels ((finalize-tf (candidate mode)
               (let* ((resolved (if (member candidate safe-pool :test #'eql)
                                    candidate
                                    (car safe-pool)))
                      (policy-mode-p (or (/= resolved candidate)
                                         (and (member mode '(:inherit :crossover) :test #'eq)
                                              base-policy-changed-p))))
                 (values resolved
                         (if policy-mode-p :policy mode)
                         p1-tf
                         p2-tf))))
      (cond
        ((and mutation-p mutation-candidates)
         (finalize-tf (or (pick-breeder-timeframe-mutation-candidate mutation-candidates)
                          policy-base)
                      :mutation))
        (crossover-p
         (finalize-tf policy-base :crossover))
        (t
         (finalize-tf policy-base :inherit))))))

(defun breeding-timeframe-diversity-bonus (parent candidate)
  "Return additive partner score adjustment for TF diversity and short-parent lane bias."
  (let* ((bonus (float (or *breeder-timeframe-diversity-bonus* 0.0) 1.0))
         (parent-tf (normalize-breeder-timeframe (strategy-timeframe parent)))
         (candidate-tf (normalize-breeder-timeframe (strategy-timeframe candidate) parent-tf))
         (base-diversity (if (and (> bonus 0.0) (/= parent-tf candidate-tf))
                             bonus
                             0.0))
         (short-parent-p (breeder-short-timeframe-p parent-tf))
         (candidate-short-p (breeder-short-timeframe-p candidate-tf))
         (candidate-priority-p (breeder-priority-timeframe-p candidate-tf))
         (priority-bonus (if (and short-parent-p candidate-priority-p)
                             (float (or *breeder-timeframe-short-parent-priority-bonus* 0.0) 1.0)
                             0.0))
         (short-penalty (if (and short-parent-p candidate-short-p (not candidate-priority-p))
                            (float (or *breeder-timeframe-short-parent-short-penalty* 0.0) 1.0)
                            0.0)))
    (+ base-diversity
       priority-bonus
       (- short-penalty))))

(defun strategy-a-or-above-p (strategy)
  "Return T when STRATEGY is rank A/S/LEGEND (or higher)."
  (let ((rank (and strategy (strategy-rank strategy))))
    (member rank '(:A :S :LEGEND) :test #'eq)))

(defun pfwr-use-s-targets-p (parent1 parent2)
  "Return T when PF/WR mutation bias should target S gates for this parent pair."
  (and *pfwr-s-target-bias-enabled*
       (strategy-a-or-above-p parent1)
       (strategy-a-or-above-p parent2)))

(defun breeding-pair-key (parent1 parent2)
  "Canonical key for a parent pair independent of ordering."
  (let* ((name1 (or (and parent1 (strategy-name parent1)) ""))
         (name2 (or (and parent2 (strategy-name parent2)) ""))
         (left-first (or (string< name1 name2) (string= name1 name2))))
    (if left-first
        (format nil "~a||~a" name1 name2)
        (format nil "~a||~a" name2 name1))))

(defun breeding-pair-blacklisted-p (parent1 parent2 &optional (now (get-universal-time)))
  "Return T when parent pair is in active cooldown blacklist window."
  (if (not *breeder-pair-blacklist-enabled*)
      nil
      (let* ((threshold (max 1 (or *breeder-pair-failure-threshold* 1)))
             (cooldown (max 1 (or *breeder-pair-blacklist-cooldown-seconds* 1)))
             (key (breeding-pair-key parent1 parent2))
             (entry (gethash key *breeder-pair-failure-stats*))
             (fails (or (and entry (getf entry :fails)) 0))
             (last-fail (or (and entry (getf entry :last-fail)) 0))
             (age (if (> last-fail 0) (- now last-fail) cooldown)))
        (if (and (>= fails threshold) (< age cooldown))
            t
            (progn
              ;; Drop stale blacklist entry so old failures do not bias future pairing.
              (when (and entry (>= age cooldown))
                (remhash key *breeder-pair-failure-stats*))
              nil)))))

(defun note-breeding-pair-failure (parent1 parent2 &optional (reason "add-to-kb rejected"))
  "Record a breeder admission failure for a parent pair."
  (let* ((key (breeding-pair-key parent1 parent2))
         (entry (gethash key *breeder-pair-failure-stats*))
         (prev-fails (or (and entry (getf entry :fails)) 0))
         (fails (1+ prev-fails))
         (now (get-universal-time))
         (threshold (max 1 (or *breeder-pair-failure-threshold* 1))))
    (setf (gethash key *breeder-pair-failure-stats*)
          (list :fails fails :last-fail now :last-reason reason))
    (when (and *breeder-pair-blacklist-enabled*
               (>= fails threshold)
               (< prev-fails threshold))
      (format t "[BREEDER] ⛔ Pair blacklist armed: ~a + ~a (fails=~d, cooldown=~ds, reason=~a)~%"
              (strategy-name parent1)
              (strategy-name parent2)
              fails
              (max 1 (or *breeder-pair-blacklist-cooldown-seconds* 1))
              reason))
    fails))

(defun note-breeding-pair-success (parent1 parent2)
  "Reset failure history for a parent pair after successful breeder admission."
  (let ((key (breeding-pair-key parent1 parent2)))
    (remhash key *breeder-pair-failure-stats*)
    t))

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

(defun higher-wr-parent (parent1 parent2)
  "Return parent with higher WR (ties -> parent1)."
  (if (>= (float (or (strategy-win-rate parent1) 0.0))
          (float (or (strategy-win-rate parent2) 0.0)))
      parent1
      parent2))

(defun higher-pf-parent (parent1 parent2)
  "Return parent with higher PF (ties -> parent1)."
  (if (>= (float (or (strategy-profit-factor parent1) 0.0))
          (float (or (strategy-profit-factor parent2) 0.0)))
      parent1
      parent2))

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

(defun pfwr-scale-expansion-factor (pf-gap-ratio wr-gap-ratio blend opposite-complements-p)
  "Return multiplicative SL/TP scale factor for PF recovery (>=1.0)."
  (let* ((pf-delta (max 0.0 (- pf-gap-ratio wr-gap-ratio)))
         (pf-scale (+ 1.0 (* blend pf-delta *pfwr-pf-recovery-scale-gain*)))
         (wr-ready-pf-deficit-p (and (> pf-gap-ratio 0.0)
                                     (<= wr-gap-ratio 0.0)))
         (wr-only-scale (if wr-ready-pf-deficit-p
                            (max pf-scale *pfwr-wr-only-scale-floor*)
                            pf-scale))
         (scale (if opposite-complements-p
                    (max wr-only-scale *pfwr-complement-scale-floor*)
                    wr-only-scale)))
    (clamp-breeder-float scale 1.0 *pfwr-pf-recovery-scale-max*)))

(defun pfwr-upside-scale-floor (avg-pf avg-wr)
  "Return additional scale floor for WR-ready pairs still below upside PF target."
  (if (or (not *pfwr-upside-scale-enabled*)
          (< avg-wr (float *pfwr-upside-min-wr*))
          (>= avg-pf (float *pfwr-upside-target-pf*)))
      1.0
      (let* ((span (max 0.0001 (- (float *pfwr-upside-target-pf*)
                                  (float *pfwr-target-pf*))))
             (pf-gap (max 0.0 (- (float *pfwr-upside-target-pf*) avg-pf)))
             (gap-ratio (clamp-breeder-float (/ pf-gap span) 0.0 1.0))
             (boost (+ 1.0 (* gap-ratio *pfwr-upside-scale-gain*))))
        (clamp-breeder-float boost 1.0 *pfwr-upside-scale-max*))))

(defun %breeder-safe-deficit-ratio (shortfall threshold)
  "Normalize metric shortfall by threshold."
  (if (<= threshold 1.0e-9)
      0.0
      (/ shortfall threshold)))

(defun breeder-priority-oos-bonus (strategy)
  "Return additive OOS quality bonus for breeder priority."
  (let* ((raw (float (or (strategy-oos-sharpe strategy) 0.0) 1.0))
         (lo (float *breeder-priority-oos-sharpe-floor* 1.0))
         (hi (float *breeder-priority-oos-sharpe-ceiling* 1.0))
         (span (max 1.0e-6 (- hi lo)))
         (normalized (clamp-breeder-float (/ (- raw lo) span) 0.0 1.0))
         (a-base-pass
           (if (and *breeder-priority-oos-bonus-requires-a-base*
                    (fboundp 'check-rank-criteria))
               (ignore-errors
                 (check-rank-criteria strategy :A :include-oos nil :include-cpcv nil))
               t))
         (pre-a-scale (clamp-breeder-float
                       (float (or *breeder-priority-oos-bonus-pre-a-scale* 0.20) 1.0)
                       0.0 1.0))
         (scale (if (or (not *breeder-priority-oos-bonus-requires-a-base*)
                        a-base-pass)
                    1.0
                    pre-a-scale)))
    (* (float *breeder-priority-oos-sharpe-weight* 1.0) normalized scale)))

(defun breeder-priority-threshold-penalty (strategy)
  "Return additive penalty for Stage1/OOS threshold deficits."
  (let* ((sharpe (float (or (strategy-sharpe strategy) 0.0) 1.0))
         (pf (float (or (strategy-profit-factor strategy) 0.0) 1.0))
         (wr (float (or (strategy-win-rate strategy) 0.0) 1.0))
         (maxdd (float (or (strategy-max-dd strategy) 1.0) 1.0))
         (oos (float (or (strategy-oos-sharpe strategy) 0.0) 1.0))
         (sh-min (float *breeder-priority-min-sharpe* 1.0))
         (pf-min (float *breeder-priority-min-pf* 1.0))
         (wr-min (float *breeder-priority-min-wr* 1.0))
         (dd-max (float *breeder-priority-max-dd* 1.0))
         (oos-min (float *breeder-priority-min-oos-sharpe* 1.0))
         (sh-def (if (>= sharpe sh-min) 0.0
                     (clamp-breeder-float
                      (%breeder-safe-deficit-ratio (- sh-min sharpe) sh-min)
                      0.0 2.0)))
         (pf-def (if (>= pf pf-min) 0.0
                     (clamp-breeder-float
                      (%breeder-safe-deficit-ratio (- pf-min pf) pf-min)
                      0.0 2.0)))
         (wr-def (if (>= wr wr-min) 0.0
                     (clamp-breeder-float
                      (%breeder-safe-deficit-ratio (- wr-min wr) wr-min)
                      0.0 2.0)))
         (dd-def (if (< maxdd dd-max) 0.0
                     (clamp-breeder-float
                      (%breeder-safe-deficit-ratio (- maxdd dd-max) dd-max)
                      0.0 2.0)))
         (oos-def (if (>= oos oos-min) 0.0
                      (clamp-breeder-float
                       (%breeder-safe-deficit-ratio (- oos-min oos) oos-min)
                       0.0 2.0)))
         (weighted-deficit (+ (* (float *breeder-priority-penalty-weight-sharpe* 1.0) sh-def)
                              (* (float *breeder-priority-penalty-weight-pf* 1.0) pf-def)
                              (* (float *breeder-priority-penalty-weight-wr* 1.0) wr-def)
                              (* (float *breeder-priority-penalty-weight-maxdd* 1.0) dd-def)
                              (* (float *breeder-priority-penalty-weight-oos* 1.0) oos-def))))
    (* (float *breeder-priority-threshold-penalty-weight* 1.0)
       weighted-deficit)))

(defun strategy-breeding-priority-score (strategy)
  "Composite parent priority score for breeding partner selection."
  (let* ((rank-bonus (case (strategy-rank strategy)
                       (:legend 3.0)
                       (:S 2.0)
                       (:A 1.0)
                       (t 0.0)))
         (rank-weight (float (or *breeder-priority-rank-bonus-weight* 0.0) 1.0))
         (base-score (if (and *breeder-priority-use-a-base-score*
                              (fboundp 'strategy-culling-score))
                         (strategy-culling-score strategy)
                         (score-from-metrics
                          (list :sharpe (strategy-sharpe strategy)
                                :profit-factor (strategy-profit-factor strategy)
                                :win-rate (strategy-win-rate strategy)
                                :trades (or (strategy-trades strategy) 0)
                                :max-dd (strategy-max-dd strategy)))))
         (generation-bonus (* (or (strategy-generation strategy) 0)
                              *breeder-priority-generation-weight*))
         (cpcv-pass (float (or (strategy-cpcv-pass-rate strategy) 0.0) 1.0))
         (cpcv-weight (float (or *breeder-priority-cpcv-pass-rate-weight* 0.0) 1.0))
         (cpcv-bonus (* cpcv-weight cpcv-pass))
         (oos-bonus (breeder-priority-oos-bonus strategy))
         (threshold-penalty (breeder-priority-threshold-penalty strategy)))
    (+ (* rank-weight rank-bonus)
       base-score
       generation-bonus
       cpcv-bonus
       oos-bonus
       (- threshold-penalty))))

(defun strategy-meets-target-pf-p (strategy &optional (target *pfwr-target-pf*))
  (>= (float (or (strategy-profit-factor strategy) 0.0))
      (float target)))

(defun strategy-meets-target-wr-p (strategy &optional (target *pfwr-target-wr*))
  (>= (float (or (strategy-win-rate strategy) 0.0))
      (float target)))

(defun strategy-pfwr-class (strategy)
  "Classify strategy relative to PF/WR targets."
  (let ((pf-ok (strategy-meets-target-pf-p strategy))
        (wr-ok (strategy-meets-target-wr-p strategy)))
    (cond
      ((and pf-ok wr-ok) :both)
      (pf-ok :pf-only)
      (wr-ok :wr-only)
      (t :none))))

(defun wr-complement-min-pf-threshold (parent candidate)
  "Return dynamic PF floor for WR-complement candidates.
Relaxes when parent PF surplus and candidate WR surplus are strong, but never below floor."
  (let* ((base (float *breeder-complement-min-pf-when-needs-wr*))
         (floor (float *breeder-complement-min-pf-floor*))
         (parent-pf (float (or (strategy-profit-factor parent) 0.0)))
         (cand-wr (float (or (strategy-win-rate candidate) 0.0)))
         (pf-surplus (max 0.0 (- parent-pf (float *pfwr-target-pf*))))
         (wr-surplus (max 0.0 (- cand-wr (float *pfwr-target-wr*))))
         (relax (+ (* pf-surplus *breeder-complement-parent-pf-relax-gain*)
                   (* wr-surplus *breeder-complement-cand-wr-relax-gain*))))
    (clamp-breeder-float (- base relax) floor base)))

(defun candidate-near-pf-recovery-p (candidate)
  "True when candidate is close to PF target and already meets WR target."
  (let ((cand-pf (float (or (strategy-profit-factor candidate) 0.0)))
        (cand-wr (float (or (strategy-win-rate candidate) 0.0))))
    (and (< cand-pf (float *pfwr-target-pf*))
         (>= cand-pf *breeder-near-pf-threshold*)
         (>= cand-wr *breeder-near-pf-min-wr*))))

(defun candidate-wr-only-pf-recovery-p (parent candidate)
  "True when WR-only parent can improve PF via WR-only higher-PF partner."
  (let ((parent-class (strategy-pfwr-class parent))
        (cand-class (strategy-pfwr-class candidate))
        (parent-pf (float (or (strategy-profit-factor parent) 0.0)))
        (cand-pf (float (or (strategy-profit-factor candidate) 0.0))))
    (and (eq parent-class :wr-only)
         (eq cand-class :wr-only)
         (candidate-near-pf-recovery-p candidate)
         (>= cand-pf (+ parent-pf *breeder-wr-only-recovery-min-pf-delta*)))))

(defun opposite-pfwr-complements-p (parent1 parent2)
  "True when parents are opposite complements: PF-only x WR-only."
  (let ((c1 (strategy-pfwr-class parent1))
        (c2 (strategy-pfwr-class parent2)))
    (or (and (eq c1 :pf-only) (eq c2 :wr-only))
        (and (eq c1 :wr-only) (eq c2 :pf-only)))))

(defun breeding-partner-complement-bonus (parent candidate)
  "Bonus score when candidate complements parent's missing PF/WR side."
  (let* ((parent-needs-pf (not (strategy-meets-target-pf-p parent)))
         (parent-needs-wr (not (strategy-meets-target-wr-p parent)))
         (cand-pf (float (or (strategy-profit-factor candidate) 0.0)))
         (cand-wr (float (or (strategy-win-rate candidate) 0.0)))
         (wr-complement-min-pf (wr-complement-min-pf-threshold parent candidate))
         (cand-has-pf (and (strategy-meets-target-pf-p candidate)
                           (>= cand-wr *breeder-complement-min-wr-when-needs-pf*)))
         (cand-near-pf (candidate-near-pf-recovery-p candidate))
         (cand-wr-only-recovery (candidate-wr-only-pf-recovery-p parent candidate))
         (cand-has-wr (and (strategy-meets-target-wr-p candidate)
                           (>= cand-pf wr-complement-min-pf)))
         (bonus 0.0))
    (when (and parent-needs-pf cand-has-pf)
      (incf bonus *breeder-complement-pf-bonus*))
    (when (and parent-needs-pf (not cand-has-pf) cand-near-pf)
      (incf bonus *breeder-near-pf-bonus*))
    (when cand-wr-only-recovery
      (incf bonus *breeder-wr-only-recovery-bonus*))
    (when (and parent-needs-wr cand-has-wr)
      (incf bonus *breeder-complement-wr-bonus*))
    (when (and parent-needs-pf parent-needs-wr cand-has-pf cand-has-wr)
      (incf bonus *breeder-complement-double-bonus*))
    bonus))

(defun breeding-partner-score (parent candidate)
  "Rank candidate partner by base quality + PF/WR complement bonus."
  (+ (strategy-breeding-priority-score candidate)
     (breeding-partner-complement-bonus parent candidate)
     (breeding-timeframe-diversity-bonus parent candidate)))

(defun candidate-complements-parent-p (parent candidate)
  "True when candidate satisfies at least one PF/WR side parent is missing."
  (let* ((parent-needs-pf (not (strategy-meets-target-pf-p parent)))
         (parent-needs-wr (not (strategy-meets-target-wr-p parent)))
         (cand-pf (float (or (strategy-profit-factor candidate) 0.0)))
         (cand-wr (float (or (strategy-win-rate candidate) 0.0)))
         (wr-complement-min-pf (wr-complement-min-pf-threshold parent candidate))
         (cand-pf-complement-p (and (>= cand-pf (float *pfwr-target-pf*))
                                    (>= cand-wr *breeder-complement-min-wr-when-needs-pf*)))
         (cand-wr-complement-p (and (>= cand-wr (float *pfwr-target-wr*))
                                    (>= cand-pf wr-complement-min-pf))))
    (or (and parent-needs-pf
             (or cand-pf-complement-p
                 (candidate-near-pf-recovery-p candidate)))
        (and parent-needs-wr cand-wr-complement-p))))

(defun candidate-partial-recovery-p (parent candidate)
  "True when candidate materially improves parent's missing PF/WR side with acceptable opposite-side floor."
  (let* ((parent-needs-pf (not (strategy-meets-target-pf-p parent)))
         (parent-needs-wr (not (strategy-meets-target-wr-p parent)))
         (parent-pf (float (or (strategy-profit-factor parent) 0.0)))
         (parent-wr (float (or (strategy-win-rate parent) 0.0)))
         (cand-pf (float (or (strategy-profit-factor candidate) 0.0)))
         (cand-wr (float (or (strategy-win-rate candidate) 0.0)))
         (wr-complement-min-pf (wr-complement-min-pf-threshold parent candidate))
         (pf-improves-p (and (>= cand-pf (+ parent-pf *breeder-partial-recovery-min-pf-delta*))
                             (>= cand-wr *breeder-complement-min-wr-when-needs-pf*)))
         (wr-improves-p (and (>= cand-wr (+ parent-wr *breeder-partial-recovery-min-wr-delta*))
                             (>= cand-pf wr-complement-min-pf))))
    (or (and parent-needs-pf pf-improves-p)
        (and parent-needs-wr wr-improves-p))))

(defun breeding-min-genetic-distance-for-candidate (parent candidate)
  "Return per-candidate min-distance, relaxing threshold only for valid complements."
  (cond
    ((not *breeder-prioritize-complement-partner*)
     *breeder-min-genetic-distance*)
    ((candidate-complements-parent-p parent candidate)
     (min *breeder-min-genetic-distance*
          *breeder-min-genetic-distance-complement*))
    ((candidate-partial-recovery-p parent candidate)
     (min *breeder-min-genetic-distance*
          *breeder-min-genetic-distance-partial-recovery*))
    (t
     *breeder-min-genetic-distance*)))

(defun apply-pfwr-mutation-bias (child-sl child-tp parent1 parent2)
  "Bias child SL/TP toward a healthier PF/WR profile while keeping risk budget constant."
  (let* ((use-s (pfwr-use-s-targets-p parent1 parent2))
         (*pfwr-target-pf* (if use-s *pfwr-s-target-pf* *pfwr-target-pf*))
         (*pfwr-target-wr* (if use-s *pfwr-s-target-wr* *pfwr-target-wr*))
         (sl (float (or child-sl 0.0)))
         (tp (float (or child-tp 0.0))))
    (if (or (not *pfwr-mutation-bias-enabled*)
            (<= sl 0.0)
            (<= tp 0.0))
        (values child-sl child-tp)
        (let* ((opposite-complements-p (opposite-pfwr-complements-p parent1 parent2))
               (pressure (pfwr-underperformance-pressure parent1 parent2))
               (effective-pressure (if opposite-complements-p
                                       (max pressure *pfwr-complement-min-pressure*)
                                       pressure))
               (blend (clamp-breeder-float (* *pfwr-mutation-bias-strength* effective-pressure) 0.0 1.0)))
          (if (<= blend 0.0)
              (let* ((avg-pf (/ (+ (float (or (strategy-profit-factor parent1) 0.0))
                                   (float (or (strategy-profit-factor parent2) 0.0)))
                                2.0))
                     (avg-wr (/ (+ (float (or (strategy-win-rate parent1) 0.0))
                                   (float (or (strategy-win-rate parent2) 0.0)))
                                2.0))
                     (upside-scale-floor (pfwr-upside-scale-floor avg-pf avg-wr)))
                (if (> upside-scale-floor 1.0)
                    (values (* sl upside-scale-floor)
                            (* tp upside-scale-floor))
                    (values sl tp)))
              (multiple-value-bind (pf-gap-ratio wr-gap-ratio)
                  (pfwr-gap-profile parent1 parent2)
                (let* ((anchor (select-pfwr-anchor-parent parent1 parent2))
                       (anchor-sl (max 0.0001 (float (or (strategy-sl anchor) sl))))
                       (anchor-tp (max 0.0001 (float (or (strategy-tp anchor) tp))))
                       (avg-pf (/ (+ (float (or (strategy-profit-factor parent1) 0.0))
                                     (float (or (strategy-profit-factor parent2) 0.0)))
                                  2.0))
                       (avg-wr (/ (+ (float (or (strategy-win-rate parent1) 0.0))
                                     (float (or (strategy-win-rate parent2) 0.0)))
                                  2.0))
                       (severe-low-pf-p (and (<= avg-pf *pfwr-severe-low-pf-threshold*)
                                             (>= avg-wr *pfwr-severe-wr-ready-threshold*)))
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
                       (base-rr-floor (pfwr-effective-rr-floor pf-gap-ratio wr-gap-ratio))
                       (rr-floor (min rr-cap
                                      (if severe-low-pf-p
                                          (max base-rr-floor *pfwr-severe-min-rr*)
                                          base-rr-floor)))
                       (final-rr (clamp-breeder-float directional-rr rr-floor rr-cap))
                       (pf-recovery-complement-p (and opposite-complements-p
                                                      (> pf-gap-ratio 0.0)
                                                      (<= wr-gap-ratio 0.0)))
                       (complement-rr-min (max rr-floor
                                               (if pf-recovery-complement-p
                                                   *pfwr-complement-pf-recovery-min-rr*
                                                   *pfwr-complement-stabilize-min-rr*)))
                       (complement-rr-max (min rr-cap
                                               (if pf-recovery-complement-p
                                                   *pfwr-complement-pf-recovery-max-rr*
                                                   *pfwr-complement-stabilize-max-rr*)))
                       (rr-after-complement (if (and (not severe-low-pf-p)
                                                     opposite-complements-p
                                                     (<= complement-rr-min complement-rr-max))
                                                (clamp-breeder-float final-rr
                                                                     complement-rr-min
                                                                     complement-rr-max)
                                                final-rr))
                       (raw-scale-factor (pfwr-scale-expansion-factor pf-gap-ratio
                                                                      wr-gap-ratio
                                                                      blend
                                                                      opposite-complements-p))
                       (base-scale-factor (if severe-low-pf-p
                                              (max raw-scale-factor *pfwr-severe-scale-floor*)
                                              raw-scale-factor))
                       (upside-scale-floor (pfwr-upside-scale-floor avg-pf avg-wr))
                       (scale-factor (max base-scale-factor upside-scale-floor))
                       (risk-budget (+ sl tp))
                       (new-sl (/ risk-budget (+ 1.0 rr-after-complement)))
                       (new-tp (- risk-budget new-sl))
                       (scaled-sl (* new-sl scale-factor))
                       (scaled-tp (* new-tp scale-factor)))
                  (values scaled-sl scaled-tp))))))))

(defun apply-pfwr-post-q-bias (child-sl child-tp parent1 parent2)
  "Re-apply PF/WR bias after Q-selection so exploit picks respect WR recovery."
  (if (or (not *pfwr-post-q-rebias-enabled*)
          (not *pfwr-mutation-bias-enabled*))
      (values child-sl child-tp)
      (let ((*pfwr-mutation-bias-strength*
              (max *pfwr-mutation-bias-strength* *pfwr-post-q-min-strength*)))
        (apply-pfwr-mutation-bias child-sl child-tp parent1 parent2))))

(defun clamp-child-sltp-to-parent-envelope (child-sl child-tp parent1 parent2)
  "Clamp child SL/TP to a parent-relative envelope to avoid no-trade parameter explosions."
  (let* ((sl (max 0.1 (float (or child-sl 0.1))))
         (tp (max 0.1 (float (or child-tp 0.1))))
         (cap-mult (max 1.0 (float (or *breeder-sltp-parent-multiplier-cap* 2.0))))
         (p1-sl (max 0.1 (float (or (strategy-sl parent1) 0.1))))
         (p2-sl (max 0.1 (float (or (strategy-sl parent2) 0.1))))
         (p1-tp (max 0.1 (float (or (strategy-tp parent1) 0.1))))
         (p2-tp (max 0.1 (float (or (strategy-tp parent2) 0.1))))
         (sl-cap (* (max p1-sl p2-sl) cap-mult))
         (tp-cap (* (max p1-tp p2-tp) cap-mult))
         (clamped-sl (clamp-breeder-float sl 0.1 sl-cap))
         (clamped-tp (clamp-breeder-float tp 0.1 tp-cap)))
    (values clamped-sl clamped-tp)))

(defun strategy-has-usable-breeder-logic-p (strategy)
  "True when strategy has non-empty indicators and entry/exit logic usable for breeding."
  (let ((inds (and strategy (strategy-indicators strategy))))
    (and strategy
         (not (null inds))
         (strategy-entry strategy)
         (strategy-exit strategy))))

(defun find-breeder-logic-donor (category timeframe symbol)
  "Find a same-category donor with usable logic, preferring exact timeframe+symbol match."
  (labels ((usable-p (s)
             (and (strategy-has-usable-breeder-logic-p s)
                  (if (fboundp 'can-breed-p)
                      (can-breed-p s)
                      t)))
           (regime-class (s)
             (if (fboundp 'strategy-regime-class)
                 (strategy-regime-class s)
                 (strategy-category s)))
           (match-exact-p (s)
             (and (eq (regime-class s) category)
                  (equal (or (strategy-timeframe s) timeframe) timeframe)
                  (string= (or (strategy-symbol s) symbol) symbol)))
           (match-symbol-p (s)
             (and (eq (regime-class s) category)
                  (string= (or (strategy-symbol s) symbol) symbol)))
           (match-category-p (s)
             (eq (regime-class s) category)))
    (or (find-if (lambda (s) (and (usable-p s) (match-exact-p s))) *strategy-knowledge-base*)
        (find-if (lambda (s) (and (usable-p s) (match-symbol-p s))) *strategy-knowledge-base*)
        (find-if (lambda (s) (and (usable-p s) (match-category-p s))) *strategy-knowledge-base*))))

(defun default-breeder-logic-genes (category)
  "Return fallback indicators/entry/exit genes for each regime category."
  (case category
    (:breakout
     (values '((sma 20) (atr 14))
             '(or (and (> close sma-20) (> atr 0.0001))
                  (and (< close sma-20) (> atr 0.0001)))
             '(or (> pnl tp)
                  (< pnl (- sl)))))
    (:reversion
     (values '((sma 20) (rsi 14))
             '(or (and (< close sma-20) (< rsi 35))
                  (and (> close sma-20) (> rsi 65)))
             '(or (and (> pnl 0) (>= rsi 50))
                  (> pnl tp)
                  (< pnl (- sl)))))
    (:scalp
     (values '((ema 9) (ema 21) (rsi 4))
             '(or (and (> ema-9 ema-21) (< rsi 35))
                  (and (< ema-9 ema-21) (> rsi 65)))
             '(or (> pnl tp)
                  (< pnl (- sl)))))
    (t
     (values '((sma 20) (sma 50) (rsi 14))
             '(or (and (> close sma-20) (> sma-20 sma-50) (> rsi 55))
                  (and (< close sma-20) (< sma-20 sma-50) (< rsi 45)))
             '(or (> pnl tp)
                  (< pnl (- sl)))))))

(defun ensure-breeder-logic-availability (entry exit indicators category timeframe symbol)
  "Backfill missing entry/exit/indicator genes from a compatible donor strategy."
  (let ((needs-entry (null entry))
        (needs-exit (null exit))
        (needs-indicators (or (null indicators)
                              (and (listp indicators) (null indicators)))))
    (if (not (or needs-entry needs-exit needs-indicators))
        (values entry exit indicators)
        (let ((donor (find-breeder-logic-donor category timeframe symbol)))
          (multiple-value-bind (default-indicators default-entry default-exit)
              (default-breeder-logic-genes category)
            (values (if needs-entry
                        (or (and donor (strategy-entry donor)) default-entry)
                        entry)
                    (if needs-exit
                        (or (and donor (strategy-exit donor)) default-exit)
                        exit)
                  (if needs-indicators
                      (copy-tree (or (and donor (strategy-indicators donor))
                                     default-indicators))
                      indicators)))))))

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
         (tf (normalize-breeder-timeframe (strategy-timeframe parent1)))
         (dir (or (strategy-direction parent1) :BOTH))
         (sym (or (strategy-symbol parent1) "USDJPY"))
         (logic-anchor-parent (select-logic-anchor-parent parent1 parent2))
         (logic-entry-parent (or (higher-wr-parent parent1 parent2) logic-anchor-parent))
         (logic-exit-parent (or (higher-pf-parent parent1 parent2) logic-anchor-parent))
         (logic-entry (or (strategy-entry logic-entry-parent)
                          (strategy-entry logic-anchor-parent)
                          (strategy-entry parent1)
                          (strategy-entry parent2)))
         (logic-exit (or (strategy-exit logic-exit-parent)
                         (strategy-exit logic-anchor-parent)
                         (strategy-exit parent1)
                         (strategy-exit parent2)))
         ;; V49.0: Aggressive Mutation (0.1 -> 0.3)
         (initial-sl (mutate-value (/ (+ (strategy-sl parent1) (strategy-sl parent2)) 2.0) 0.3))
         (initial-tp (mutate-value (/ (+ (strategy-tp parent1) (strategy-tp parent2)) 2.0) 0.3))
         (parent-regime-class (if (fboundp 'strategy-regime-class)
                                  (strategy-regime-class parent1)
                                  (strategy-category parent1)))
         ;; V49.5: Smart Mutation for Indicators (Regime-Aware)
         (child-is (mutate-indicators-with-library 
                    (mutate-indicator-params (crossover-indicators parent1 parent2))
                    parent-regime-class))
         ;; V47.5: Get avoid regions from graveyard analysis
         (avoid-regions (when (fboundp 'analyze-graveyard-for-avoidance)
                          (analyze-graveyard-for-avoidance)))
         ;; Check if SL/TP falls in avoid region, regenerate if needed
         (child-sl initial-sl)
         (child-tp initial-tp))

    (multiple-value-bind (selected-tf tf-mode p1-tf p2-tf)
        (select-breeder-child-timeframe parent1 parent2)
      (setf tf selected-tf)
      (when (member tf-mode '(:crossover :mutation) :test #'eq)
        (format t "[BREEDER] 🕰️ TF ~a: ~a -> ~a (P1=~a P2=~a)~%"
                tf-mode p1-tf tf p1-tf p2-tf)))

    ;; Recover missing logic genes from a compatible donor when both parents are empty.
    (multiple-value-bind (resolved-entry resolved-exit resolved-indicators)
        (ensure-breeder-logic-availability logic-entry
                                           logic-exit
                                           child-is
                                           parent-regime-class
                                           tf
                                           sym)
      (setf logic-entry resolved-entry
            logic-exit resolved-exit
            child-is resolved-indicators))

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
    ;; Musk Condition: Only apply to breeding path.
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

    ;; Keep child SL/TP inside a parent-relative envelope to avoid zero-trade blowups.
    (multiple-value-bind (capped-sl capped-tp)
        (clamp-child-sltp-to-parent-envelope child-sl child-tp parent1 parent2)
      (setf child-sl capped-sl
            child-tp capped-tp))
    
    (make-strategy
      :name child-name
      :category parent-regime-class ;; Inherit regime class from P1
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
        (best-partial-recovery nil)
        (best-partial-recovery-score most-negative-double-float)
        (best-wr-only-recovery nil)
        (best-wr-only-recovery-score most-negative-double-float)
        (parent-wr-only-p (eq (strategy-pfwr-class parent) :wr-only))
        (parent-needs-complement (or (not (strategy-meets-target-pf-p parent))
                                     (not (strategy-meets-target-wr-p parent)))))
    (loop for idx from start-index below (length sorted-candidates)
          for candidate = (nth idx sorted-candidates)
          when (and candidate
                    (not (eq candidate parent))
                    (or (null used-names)
                        (null (gethash (strategy-name candidate) used-names)))
                    (can-breed-p candidate)
                    (not (breeding-pair-blacklisted-p parent candidate)))
            do (let* ((min-distance (breeding-min-genetic-distance-for-candidate parent candidate))
                      (*breeder-current-pair-min-distance* min-distance))
                 (when (strategies-correlation-ok-p parent candidate)
                   (let ((score (breeding-partner-score parent candidate))
                        (complements-p (candidate-complements-parent-p parent candidate))
                        (partial-recovery-p (candidate-partial-recovery-p parent candidate))
                        (wr-only-recovery-p (candidate-wr-only-pf-recovery-p parent candidate)))
                     (when (> score best-score)
                       (setf best candidate
                             best-score score))
                     (when (and complements-p (> score best-complement-score))
                       (setf best-complement candidate
                             best-complement-score score))
                     (when (and partial-recovery-p (> score best-partial-recovery-score))
                       (setf best-partial-recovery candidate
                             best-partial-recovery-score score))
                     (when (and wr-only-recovery-p (> score best-wr-only-recovery-score))
                       (setf best-wr-only-recovery candidate
                             best-wr-only-recovery-score score))))))
    (cond
      ((and *breeder-prioritize-complement-partner*
            parent-wr-only-p
            best-wr-only-recovery)
       best-wr-only-recovery)
      ((and *breeder-prioritize-complement-partner*
            parent-needs-complement
            best-complement)
       best-complement)
      ((and *breeder-prioritize-complement-partner*
            parent-needs-complement
            best-partial-recovery)
       best-partial-recovery)
      (t
       best))))



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
    (let ((*breeder-active-timeframe-counts-cache*
            (collect-breeder-active-timeframe-counts)))
      (dolist (cat categories)
        (let* ((all-warriors (remove-if-not
                              (lambda (s)
                                (and (eq (if (fboundp 'strategy-regime-class)
                                             (strategy-regime-class s)
                                             (strategy-category s))
                                         cat)
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
                             ;; V49.2: Inherit Regime Intent
                             (setf (strategy-regime-intent child)
                                   (or (when (boundp '*current-regime*) *current-regime*)
                                       :unknown))

                             ;; Add to KB (Breeder path requires Phase 1 screening before B-rank)
                             (multiple-value-bind (accepted status)
                                 (add-to-kb child :breeder :require-bt t :notify nil)
                               (if accepted
                                   (let ((effective-status (or status :added)))
                                     (increment-breeding-count p1)
                                     (increment-breeding-count p2)
                                     (note-breeding-pair-success p1 p2)
                                     (if (eq effective-status :queued-phase1)
                                         (format t "[BREEDER] ⏳ Candidate queued for Phase1: ~a (Gen~d)~%"
                                                 (strategy-name child) (strategy-generation child))
                                         (progn
                                           (when (hash-table-p *breeder-active-timeframe-counts-cache*)
                                             (incf (gethash (normalize-breeder-timeframe
                                                             (strategy-timeframe child))
                                                            *breeder-active-timeframe-counts-cache*
                                                            0)))
                                           (save-recruit-to-lisp child)
                                           (format t "[BREEDER] 👶 Born: ~a (Gen~d)~%"
                                                   (strategy-name child) (strategy-generation child))

                                           ;; V50.2: Immediate Culling (Survival of the Fittest)
                                           ;; If pool > 20, kill the weakest B-Rank to make room
                                           (cull-pool-overflow cat))))
                                   (note-breeding-pair-failure p1 p2 "add-to-kb rejected"))))))))))))))

(defun cull-pool-overflow (category)
  "Enforce Musk's '20 or Die' rule. 
   If pool size > *b-rank-pool-size*, kill the weakest."
  (let* ((regime-pool nil)
         (regime-foundp nil)
         (_unused (when (boundp '*regime-pools*)
                    (multiple-value-setq (regime-pool regime-foundp)
                      (gethash category *regime-pools*))))
         (raw-pool (if regime-foundp
                       regime-pool
                       (gethash category *category-pools*)))
         (pool (if (fboundp 'strategy-active-pool-eligible-p)
                   (remove-if-not #'strategy-active-pool-eligible-p raw-pool)
                   raw-pool))
         (limit (if (boundp '*b-rank-pool-size*) *b-rank-pool-size* 20))
         (survivors nil)
         (victims nil))
  (declare (ignore _unused))
    ;; Defensive scrub: never let unevaluated members participate in overflow culling.
    (when (/= (length pool) (length raw-pool))
      (if regime-foundp
          (setf (gethash category *regime-pools*) pool)
        (setf (gethash category *category-pools*) pool)))
    (labels ((founder-overflow-protected-p (strat)
               (let ((sharpe (or (strategy-sharpe strat) 0.0))
                     (pf (or (strategy-profit-factor strat) 0.0))
                     (trades (or (strategy-trades strat) 0))
                     (max-dd (or (strategy-max-dd strat) 1.0)))
                 (and (eq (strategy-rank strat) :B)
                      (fboundp 'founder-phase1-recovery-passed-p)
                      (ignore-errors
                        (founder-phase1-recovery-passed-p strat sharpe pf trades max-dd)))))
             (overflow-cullable-b-p (strat)
               ;; Contract: Overflow deathmatch culls B-rank only.
               ;; Never cull A/S/LEGEND (or immortal) here.
               (and (eq (strategy-rank strat) :B)
                    (not (strategy-immortal strat))
                    (not (founder-overflow-protected-p strat)))))
      (when (> (length pool) limit)
      ;; Sort by composite score (High to Low)
      (let ((sorted (sort (copy-list pool) #'>
                          :key (lambda (s)
                                 (score-from-metrics
                                  (list :sharpe (strategy-sharpe s)
                                        :profit-factor (strategy-profit-factor s)
                                        :win-rate (strategy-win-rate s)
                                        :max-dd (strategy-max-dd s)))))))
        (let* ((protected (remove-if #'overflow-cullable-b-p sorted))
               (cullable (remove-if-not #'overflow-cullable-b-p sorted))
               (cullable-limit (max 0 (- limit (length protected))))
               (cullable-keep (min cullable-limit (length cullable))))
          (when (> (length protected) limit)
            (format t "[DEATHMATCH] 🛡️ Overflow protected-only set exceeds limit: keep=~d limit=~d category=~a~%"
                    (length protected) limit category))
          (setf survivors (append protected (subseq cullable 0 cullable-keep)))
          (setf victims (nthcdr cullable-keep cullable)))
        
        ;; Update the source pool used for culling.
        (if regime-foundp
            (setf (gethash category *regime-pools*) survivors)
          (setf (gethash category *category-pools*) survivors))
        
        ;; Kill Victims
        (dolist (victim victims)
          (format t "[DEATHMATCH] 💀 Killing Weakest: ~a (Sharpe: ~,2f)~%" 
                  (strategy-name victim) (strategy-sharpe victim))
          ;; Also evict from scope pool.
          (let ((scope-key (categorize-strategy victim)))
            (setf (gethash scope-key *category-pools*)
                  (remove victim (gethash scope-key *category-pools*) :test #'eq)))
          ;; Notify Discord (Musk Requirement)
          (notify-death victim "Pool Overflow (Weakest Link)")
          ;; Remove from KB (Graveyard logic could be added here)
          (setf *strategy-knowledge-base* (delete victim *strategy-knowledge-base*))
          ;; Remove from SQL
          (handler-case
              (progn
                (init-db)
                (execute-non-query "UPDATE strategies SET rank = ':GRAVEYARD' WHERE name = ?" (strategy-name victim)))
            (error () nil))))))))

(defun notify-death (strat reason)
  "Musk: 'I want to see Death.'"
  (let ((name (strategy-name strat)))
    (if (and (stringp reason)
             (search "Pool Overflow" reason :test #'char-equal)
             (stringp name)
             (> (length name) 0))
        (swimmy.core::queue-pool-overflow-retire name)
        (swimmy.core:notify-discord-recruit
         (format nil "💀 **STRATEGY EXECUTION**
━━━━━━━━━━━━━━━━━━━━━━
⚰️ Victim: `~a`
📉 Sharpe: ~,2f
⚖️ Verdict: ~a
🛑 Status: TERMINATED"
                 name (or (strategy-sharpe strat) 0) reason)
         :color 0)))) ; Black/Dark for Death

;;; ----------------------------------------------------------------------------
;;; Phase 6b: Persistence Implementation
;;; ----------------------------------------------------------------------------

(defun save-recruit-to-lisp (strat)
  "Save the new strategy to The Great Library (Sharded Persistence)."
  (swimmy.persistence:save-strategy strat)
  (format t "[PERSIST] 💾 Saved recruited strategy ~a to Library~%" (strategy-name strat)))

;;; ----------------------------------------------------------------------------
;; P8: recruit-elite-strategy DELETED

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
  (let* ((dist (calculate-genetic-distance (extract-genome p1) (extract-genome p2)))
         (min-distance (or *breeder-current-pair-min-distance*
                           *breeder-min-genetic-distance*)))
    (if (< dist min-distance)
        (progn
          (format t "[BREEDER] 🚫 Pair too similar (Dist: ~,2f < Min: ~,2f). Skipping Jackpotクローン.~%"
                  dist min-distance)
          nil)
        t)))

(defun %wisdom-push-elite-candidate (candidate elites limit)
  "Insert CANDIDATE into descending SHARPE-sorted ELITES capped by LIMIT."
  (let* ((cap (max 1 (or limit 50)))
         (sorted (sort (cons candidate elites) #'>
                       :key (lambda (s)
                              (or (strategy-sharpe s)
                                  most-negative-double-float)))))
    (if (> (length sorted) cap)
        (nbutlast sorted (- (length sorted) cap))
        sorted)))

(defun analyze-veterans ()
  "Analyze the Knowledge Base and extract 'Wisdom' (Best Genes).
   Replaces extract_wisdom.py."
  (format t "[WISDOM] 🧠 Analyzing Veterans for Gene Extraction...~%")
  (let* ((total (length *strategy-knowledge-base*))
         (elite-limit 50))
    (format t "[WISDOM] 🔍 De-duplication starting: ~d strategies...~%" total)
    (format t "[WISDOM] 🔍 Filtering candidates (Sharpe > 0.1) and streaming Top-~d...~%"
            elite-limit)
    (let* ((t0 (get-internal-real-time))
           (seen (make-hash-table :test 'equal))
           (unique-count 0)
           (candidate-count 0)
           (elite nil))
      ;; Single pass: de-dup + filter + bounded ranking to avoid large transient lists.
      (dolist (s *strategy-knowledge-base*)
        (when s
          (let ((name (strategy-name s)))
            (unless (gethash name seen)
              (setf (gethash name seen) t)
              (incf unique-count)
              (let ((sharpe (strategy-sharpe s)))
                (when (and (numberp sharpe) (> sharpe 0.1))
                  (incf candidate-count)
                  (setf elite (%wisdom-push-elite-candidate s elite elite-limit))))))))
      (let* ((elapsed (/ (- (get-internal-real-time) t0)
                         internal-time-units-per-second))
             (genes (mapcar #'extract-params-from-strategy elite)))
        (format t "[WISDOM] ✅ De-dup complete: ~d unique (~,2fs)~%" unique-count elapsed)
        (format t "[WISDOM] ✅ Filter complete: ~d candidates (~,2fs)~%" candidate-count elapsed)
        (format t "[WISDOM] ✅ Sort complete (~,2fs)~%" elapsed)
        (format t "[WISDOM] Found ~d candidates (Sharpe > 0.1). Extracting ~d Elite Genes.~%"
                candidate-count (length elite))
        (if genes
            (save-optimized-params-to-file genes)
            (format t "[WISDOM] ⚠️ No eligible veterans found. Keeping existing genes.~%"))
        (length genes)))))

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
