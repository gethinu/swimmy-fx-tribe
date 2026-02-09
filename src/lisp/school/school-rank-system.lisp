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
;;; 5. :retired - Max Age archive (low-weight learning)
;;; 6. :legend - Protected strategies (61 total, never discarded)
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; RANK CONSTANTS
;;; ---------------------------------------------------------------------------

(defparameter *rank-criteria*
  '((:B       :sharpe-min 0.1  :pf-min 1.0  :wr-min 0.30  :maxdd-max 0.30)
    (:A       :sharpe-min 0.3  :pf-min 1.2  :wr-min 0.40  :maxdd-max 0.20 :oos-min 0.3)
    (:S       :sharpe-min 0.5
              :cpcv-min 0.5 :cpcv-pass-min 0.5
              :cpcv-pf-min 1.5 :cpcv-wr-min 0.45 :cpcv-maxdd-max 0.15))
  "Rank criteria thresholds. All conditions must be met (AND logic).")

(defparameter *culling-threshold* 10
  "Number of B-RANK strategies per TF before culling begins. (V50.4 Accelerated)")

(defparameter *a-rank-slots-per-tf* 2
  "Only top 2 strategies per TF can be promoted to A-RANK.")

(defparameter *max-breeding-uses* 3
  "Strategies are discarded after being used 3 times for breeding (Legend exempt).")

;;; ---------------------------------------------------------------------------
;;; COMPOSITE SCORE (Multi-Metric)
;;; ---------------------------------------------------------------------------

(defparameter *score-weight-sharpe* 0.45)
(defparameter *score-weight-pf* 0.25)
(defparameter *score-weight-wr* 0.20)
(defparameter *score-weight-maxdd* 0.10)

(defun %clamp (v lo hi)
  (min hi (max lo v)))

(defun %norm (v lo hi)
  (if (<= hi lo)
      0.0
      (let* ((v (float v 1.0))
             (lo (float lo 1.0))
             (hi (float hi 1.0)))
        (/ (- (%clamp v lo hi) lo) (- hi lo)))))

(defun score-from-metrics (metrics)
  "Compute composite score from metrics plist."
  (let* ((sharpe (or (getf metrics :sharpe) 0.0))
         (pf (or (getf metrics :profit-factor) 0.0))
         (wr (or (getf metrics :win-rate) 0.0))
         (dd (or (getf metrics :max-dd) 1.0))
         (n-sharpe (%norm sharpe 0.0 2.0))
         (n-pf (%norm pf 1.0 2.0))
         (n-wr (%norm wr 0.40 0.70))
         (n-dd (%norm dd 0.0 0.20)))
    (+ (* *score-weight-sharpe* n-sharpe)
       (* *score-weight-pf* n-pf)
       (* *score-weight-wr* n-wr)
       (* -1 *score-weight-maxdd* n-dd))))

;;; ---------------------------------------------------------------------------
;;; RETIRED STORAGE
;;; ---------------------------------------------------------------------------

(defparameter *retired-file* "data/memory/retired.sexp")


;;; ---------------------------------------------------------------------------
;;; RANK UTILITIES
;;; ---------------------------------------------------------------------------

(defun get-rank-criteria (rank)
  "Get criteria plist for a given rank."
  (cdr (assoc rank *rank-criteria*)))

(defun check-rank-criteria (strategy target-rank &key (include-oos t) (include-cpcv t))
  "Check if strategy meets all criteria for target-rank.
   Returns T if all conditions pass, NIL otherwise.
   Optional gates: INCLUDE-OOS/INCLUDE-CPCV can be disabled for pre-validation checks."
  (let* ((criteria (get-rank-criteria target-rank))
         (sharpe (or (strategy-sharpe strategy) 0.0))
         (pf (or (strategy-profit-factor strategy) 0.0))
         (wr (or (strategy-win-rate strategy) 0.0))
         (maxdd (or (strategy-max-dd strategy) 1.0)))
    (cond
      ((eq target-rank :S)
       (and (>= sharpe (getf criteria :sharpe-min 0))
            (or (not include-cpcv)
                ;; S判定のPF/WR/MaxDDはCPCV中央値で評価する
                (and (>= (or (strategy-cpcv-median-sharpe strategy) 0.0) (getf criteria :cpcv-min 0))
                     (>= (or (strategy-cpcv-pass-rate strategy) 0.0) (getf criteria :cpcv-pass-min 0))
                     (>= (or (strategy-cpcv-median-pf strategy) 0.0) (getf criteria :cpcv-pf-min 0))
                     (>= (or (strategy-cpcv-median-wr strategy) 0.0) (getf criteria :cpcv-wr-min 0))
                     (< (or (strategy-cpcv-median-maxdd strategy) 1.0) (getf criteria :cpcv-maxdd-max 1.0))))))
      (t
       (and (>= sharpe (getf criteria :sharpe-min 0))
            (>= pf (getf criteria :pf-min 0))
            (>= wr (getf criteria :wr-min 0))
            (< maxdd (getf criteria :maxdd-max 1.0))
            ;; V50.3: Gate Lockdown
            (cond
              ((eq target-rank :A)
               (or (not include-oos)
                   (>= (or (strategy-oos-sharpe strategy) 0.0) (getf criteria :oos-min 0))))
              (t t)))))))

(defun get-strategies-by-rank (rank &optional timeframe direction symbol)
  "Get all strategies with a specific rank, optionally filtered by TF/Direction/Symbol.
   V47.2: Extended for full category filtering."
  (let ((candidates (remove-if-not 
                      (lambda (s) (eq (strategy-rank s) rank))
                      *strategy-knowledge-base*)))
    ;; Apply TF filter
    (when timeframe
      (setf candidates (remove-if-not 
                         (lambda (s) (eql (strategy-timeframe s) timeframe)) 
                         candidates)))
    ;; Apply Direction filter
    (when direction
      (setf candidates (remove-if-not 
                         (lambda (s) (eq (strategy-direction s) direction)) 
                         candidates)))
    ;; Apply Symbol filter
    (when symbol
      (setf candidates (remove-if-not 
                         (lambda (s) (string= (strategy-symbol s) symbol)) 
                         candidates)))
    candidates))

(defun count-by-category (rank timeframe direction symbol)
  "Count strategies with given rank and category."
  (length (get-strategies-by-rank rank timeframe direction symbol)))

;;; ---------------------------------------------------------------------------
;;; V48.1: UNIFIED RANK SETTER (Expert Panel - Naval's DRY)
;;; ---------------------------------------------------------------------------

(defun ensure-rank (strategy new-rank &optional reason)
  "V48.1/V48.2: Single entry point for all rank changes (DRY principle).
   V48.2: Enforces S-RANK slot limits and handles atomic promotion/demotion.
   Handles logging and graveyard pattern saving automatically."
  (bt:with-lock-held (*kb-lock*)
    (%ensure-rank-no-lock strategy new-rank reason)))

(defun %ensure-rank-no-lock (strategy new-rank &optional reason)
  "Internal version of ensure-rank that assumes *kb-lock* is already held."
  (let ((old-rank (strategy-rank strategy)))
    ;; LEGEND保護: レジェンドは墓場送りしない（子世代は別扱い）
    (when (and (eq old-rank :legend) (eq new-rank :graveyard))
      (format t "[RANK] ⚠️ Legend protection: ~a remains :LEGEND (skip graveyard).~%" (strategy-name strategy))
      (return-from %ensure-rank-no-lock old-rank))
    (when (and (eq new-rank :graveyard)
               (oos-request-pending-p (strategy-name strategy)))
      (format t "[RANK] ⏳ OOS pending: skip graveyard for ~a~%" (strategy-name strategy))
      (return-from %ensure-rank-no-lock old-rank))
    (when (and (eq new-rank :S)
               (not (eq old-rank :S))
               (not (check-rank-criteria strategy :S)))
      (format t "[RANK] 🚫 Blocked S promotion for ~a (CPCV criteria missing).~%"
              (strategy-name strategy))
      (when (fboundp 'swimmy.core::emit-telemetry-event)
        (swimmy.core::emit-telemetry-event "rank.promotion.blocked"
          :service "school"
          :severity "warning"
          :correlation-id (strategy-name strategy)
          :data (list :strategy (strategy-name strategy)
                      :old-rank old-rank
                      :new-rank new-rank
                      :promotion-reason reason
                      :block "cpcv-missing")))
      (return-from %ensure-rank-no-lock old-rank))

    (when (not (eq old-rank new-rank))
      ;; Normal rank change. Global Portfolio selection handles S-RANK capacity.
      (setf (strategy-rank strategy) new-rank)
      (format t "[RANK] ~a: ~a → ~a~@[ (~a)~]~%"
              (strategy-name strategy) old-rank new-rank reason)
      
      ;; V49.9: Persist to SQL
      (upsert-strategy strategy)

      (let ((promotion-p (%promotion-p old-rank new-rank)))
        (when promotion-p
          (handler-case
              (notify-noncorrelated-promotion strategy new-rank :promotion-reason reason)
            (error (e)
              (format t "[RANK] ⚠️ Noncorrelation notify failed: ~a~%" e)))))
      
      ;; V48.2: If going to graveyard, DELETE physically from KB and pools immediately (Nassim Taleb: Survival)
      (when (eq new-rank :graveyard)
        (ignore-errors (cancel-oos-request-for-strategy (strategy-name strategy) "graveyard"))
        (save-failure-pattern strategy reason)
        (setf *strategy-knowledge-base* 
              (remove strategy *strategy-knowledge-base* :test #'eq))
        ;; Also remove from category pools
        (let ((cat (categorize-strategy strategy)))
          (setf (gethash cat *category-pools*)
                (remove strategy (gethash cat *category-pools*) :test #'eq)))
        
        ;; P13: Synchronize with File System
        (handler-case
            (swimmy.persistence:move-strategy strategy :graveyard :from-rank old-rank)
          (error (e)
            (format t "[RANK] ⚠️ File move failed: ~a~%" e)))
        
        (format t "[RANK] 🪦 Physically DELETED from Knowledge Base.~%"))

      (when (eq new-rank :retired)
        (save-retired-pattern strategy reason)
        (setf *strategy-knowledge-base*
              (remove strategy *strategy-knowledge-base* :test #'eq))
        ;; Also remove from category pools
        (let ((cat (categorize-strategy strategy)))
          (setf (gethash cat *category-pools*)
                (remove strategy (gethash cat *category-pools*) :test #'eq)))
        ;; Persist to archive
        (handler-case
            (swimmy.persistence:move-strategy strategy :retired :from-rank old-rank)
          (error (e)
            (format t "[RANK] ⚠️ File move failed: ~a~%" e)))
        (format t "[RANK] 🧊 Retired and removed from Knowledge Base.~%")))
    new-rank))

(defun promote-rank (strategy new-rank reason)
  "Promote strategy to a higher rank."
  (ensure-rank strategy new-rank reason))

(defun demote-rank (strategy new-rank reason)
  "Demote strategy to a lower rank (or graveyard)."
  (ensure-rank strategy new-rank reason))

(defun send-to-graveyard (strategy reason)
  "Move strategy to graveyard and save failure pattern."
  (ensure-rank strategy :graveyard reason))

(defun send-to-retired (strategy reason)
  "Move strategy to retired archive and save pattern."
  (ensure-rank strategy :retired reason))

(defun save-failure-pattern (strategy &optional reason)
  "Save failed strategy parameters for learning (avoid same mistakes).
   V47.2/V48.2: Enhanced robustness for Nassim Taleb's safety concerns.
   Appends to data/memory/graveyard.sexp with fallback to emergency file."
  (let ((pattern (list :name (strategy-name strategy)
                       :timeframe (strategy-timeframe strategy)
                       :direction (strategy-direction strategy)
                       :symbol (strategy-symbol strategy)
                       :sl (strategy-sl strategy)
                       :tp (strategy-tp strategy)
                       :sharpe (strategy-sharpe strategy)
                       :profit-factor (strategy-profit-factor strategy)
                       :win-rate (strategy-win-rate strategy)
                       :max-dd (strategy-max-dd strategy)
                       :reason reason
                       :timestamp (get-universal-time))))
    (handler-case
        (progn
          (ensure-directories-exist "data/memory/")
          (with-open-file (stream "data/memory/graveyard.sexp"
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (write pattern :stream stream)
            (terpri stream)))
      (error (e)
        (format t "[GRAVEYARD] ⚠️ Primary save failed: ~a. Attempting EMERGENCY save.~%" e)
        (handler-case
            (with-open-file (stream "data/memory/graveyard.emergency.sexp"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
              (write pattern :stream stream)
              (terpri stream))
          (error (e2)
            (format t "[GRAVEYARD] ❌ EMERGENCY SAVE FAILED: ~a. Pattern lost for ~a!~%" 
                    e2 (strategy-name strategy))))))))

(defun save-retired-pattern (strategy &optional reason)
  "Save retired strategy parameters for low-weight learning."
  (let ((pattern (list :name (strategy-name strategy)
                       :timeframe (strategy-timeframe strategy)
                       :direction (strategy-direction strategy)
                       :symbol (strategy-symbol strategy)
                       :sl (strategy-sl strategy)
                       :tp (strategy-tp strategy)
                       :sharpe (strategy-sharpe strategy)
                       :profit-factor (strategy-profit-factor strategy)
                       :win-rate (strategy-win-rate strategy)
                       :max-dd (strategy-max-dd strategy)
                       :reason reason
                       :timestamp (get-universal-time)
                       :retired t)))
    (handler-case
        (progn
          (ensure-directories-exist "data/memory/")
          (with-open-file (stream *retired-file*
                                  :direction :output
                                  :if-exists :append
                                  :if-does-not-exist :create)
            (write pattern :stream stream)
            (terpri stream)))
      (error (e)
        (format t "[RETIRED] ⚠️ Save failed: ~a~%" e)))))

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
;;; B-RANK CULLING (100 strategies per category → Keep best, discard rest)
;;; V47.2: Category = TF × Direction × Symbol
;;; ---------------------------------------------------------------------------

(defparameter *supported-timeframes* '(5 15 60 240 1440 10080)
  "M5, M15, H1, H4, D1, W1")
(defparameter *supported-directions* '(:BUY :SELL :BOTH))
(defparameter *supported-symbols* '("EURUSD" "GBPUSD" "USDJPY"))

(defun normalize-oos-defaults ()
  "Convert legacy OOS defaults of 0.0 to NIL so they trigger fresh validation.
   Applies to B/A rank strategies only, to avoid touching higher ranks with confirmed OOS."
  (let ((count 0))
    (dolist (s *strategy-knowledge-base*)
      (when (and (member (strategy-rank s) '(:B :A))
                 (numberp (strategy-oos-sharpe s))
                 (<= (abs (strategy-oos-sharpe s)) 1e-6))
        (setf (strategy-oos-sharpe s) nil)
        (ignore-errors (upsert-strategy s))
        (incf count)))
    (when (> count 0)
      (format t "[RANK] 🧼 Normalized ~d strategies with default OOS=0.0 → nil~%" count))
    count))

(defun run-b-rank-culling-for-category (timeframe direction symbol)
  "Cull B-RANK strategies for a specific TF × Direction × Symbol category."
  (let* ((b-strategies (get-strategies-by-rank :B timeframe direction symbol))
         (count (length b-strategies)))
    
    (when (>= count *culling-threshold*)
      (format t "[RANK] 🗡️ CULLING B-RANK (TF=~a Dir=~a Sym=~a Count=~d)~%" 
              timeframe direction symbol count)
      
      ;; Sort by composite score (Sharpe + PF bonus)
      (let* ((sorted (sort (copy-list b-strategies) #'>
                           :key (lambda (s) 
                                  (+ (or (strategy-sharpe s) 0)
                                     (* 0.1 (or (strategy-profit-factor s) 0))))))
             (to-promote (subseq sorted 0 (min *a-rank-slots-per-tf* (length sorted))))
             (to-discard (nthcdr *a-rank-slots-per-tf* sorted)))
        
        ;; Instead of direct promotion, we now queue for OOS Validation
        (dolist (s to-promote)
          (format t "[RANK] 🔬 Queueing ~a for OOS Validation...~%" (strategy-name s))
          (handler-case
              (validate-for-a-rank-promotion s)
            (error (e) (format t "[RANK] ⚠️ OOS Failed for ~a: ~a~%" (strategy-name s) e))))
        
        ;; Discard rest
        (dolist (s to-discard)
          (send-to-graveyard s "Culling Loser"))))))

(defun collect-active-symbols ()
  "Collect all unique symbols present in the KB."
  (remove-duplicates (mapcar #'strategy-symbol *strategy-knowledge-base*) :test #'string=))

;;; ---------------------------------------------------------------------------
;;; A-RANK EVALUATION
;;; ---------------------------------------------------------------------------

(defparameter *a-rank-probation-tracker* (make-hash-table :test 'equal)
  "Tracks consecutive failure days for A-Rank strategies (Grace Period).")

(defparameter *a-rank-elite-score* 0.30
  "Composite score threshold for considering A-rank as elite (CPCV-ready signal).")
(defparameter *a-rank-probation-score* 0.20
  "Composite score threshold for A-rank probation.")

(defun evaluate-a-rank-strategy (strategy)
  "Evaluate A-RANK strategy with CPCV (2021-2026 OOS).
   If passes S criteria → S-RANK. If fails → B-RANK or Graveyard."
  (let* ((name (strategy-name strategy))
         (cpcv-ready (and (numberp (strategy-cpcv-pass-rate strategy))
                          (> (or (strategy-cpcv-pass-rate strategy) 0.0) 0.0)
                          (> (or (strategy-cpcv-median-pf strategy) 0.0) 0.0)
                          (> (or (strategy-cpcv-median-wr strategy) 0.0) 0.0)))
         (score (if (fboundp 'score-from-metrics)
                    (score-from-metrics
                     (if cpcv-ready
                         (list :sharpe (strategy-cpcv-median-sharpe strategy)
                               :profit-factor (strategy-cpcv-median-pf strategy)
                               :win-rate (strategy-cpcv-median-wr strategy)
                               :max-dd (strategy-cpcv-median-maxdd strategy))
                         (list :sharpe (strategy-sharpe strategy)
                               :profit-factor (strategy-profit-factor strategy)
                               :win-rate (strategy-win-rate strategy)
                               :max-dd (strategy-max-dd strategy))))
                    (or (strategy-sharpe strategy) 0.0))))
    (if (check-rank-criteria strategy :S)
        (progn
          (remhash name *a-rank-probation-tracker*)
          (promote-rank strategy :S "CPCV Validated - LIVE TRADING PERMITTED")
          :S)
        ;; V50.3: No more automatic A->S shortcuts. 
        ;; We just check if it's ready for CPCV dispatch.
        (progn
          (when (and (>= score *a-rank-elite-score*)
                     (fboundp 'run-a-rank-cpcv-batch))
            (format t "[RANK] 🧪 ~a is Elite. Awaiting CPCV validation (Score=~,2f).~%" name score))
          (if (check-rank-criteria strategy :B)
              (if (< score *a-rank-probation-score*)
                    (let ((fails (incf (gethash name *a-rank-probation-tracker* 0))))
                      (if (< fails 7)
                          (progn
                            (format t "[RANK] 🛡️ A-RANK PROBATION (~d/7): ~a (Score=~,2f < ~,2f)~%"
                                    fails name score *a-rank-probation-score*)
                            :A)
                          (progn
                            (remhash name *a-rank-probation-tracker*)
                            (demote-rank strategy :B (format nil "Grace Period Expired (~d failures)" fails))
                            :B)))
                    (progn (remhash name *a-rank-probation-tracker*) :A))
              (progn
                (remhash name *a-rank-probation-tracker*)
                (send-to-graveyard strategy "CPCV Critical Failure (< 0.1 Sharpe)")
                :graveyard))))))

;;; ---------------------------------------------------------------------------
;;; BREEDING HELPERS
;;; ---------------------------------------------------------------------------

(defun increment-breeding-count (strategy)
  "Increment breeding use count."
  (let ((count (1+ (or (strategy-breeding-count strategy) 0))))
    (setf (strategy-breeding-count strategy) count)
    (when (and (>= count *max-breeding-uses*)
               (not (eq (strategy-rank strategy) :legend)))
      (send-to-graveyard strategy (format nil "Breeding limit reached (~d uses)" count)))))

(defun run-b-rank-culling (&optional single-tf)
  "Run culling for all TF × Direction × Symbol categories.
   V49.3: Dynamic symbol detection to prevent zombie-accumulation in non-major pairs."
  (let ((timeframes (if single-tf (list single-tf) *supported-timeframes*))
        (symbols (collect-active-symbols))) ;; Dynamic Symbols
    (dolist (tf timeframes)
      (dolist (dir *supported-directions*)
        (dolist (sym symbols)
          (run-b-rank-culling-for-category tf dir sym))))
    
    ;; V48.7: Meritocratic Promotion (A-Rank) -> Now queues for OOS
    (dolist (s (get-strategies-by-rank :B))
      (when (and (strategy-sharpe s) (>= (strategy-sharpe s) 0.3))
        (validate-for-a-rank-promotion s)))))

(defun run-rank-evaluation ()
  "Main rank evaluation cycle.
   1. Evaluate new strategies (→ B or Graveyard)
   2. Cull B-RANK if threshold reached (Dynamic Categories)
   3. Validate A-RANK via CPCV (→ S or back)
   V49.3: Added missing A-Rank evaluation loop."
  (format t "[RANK] 🏛️ Starting Rank Evaluation Cycle (V49.3 Fixed)~%")
  (reset-oos-failure-stats)
  ;; Normalize legacy OOS defaults so validation is re-triggered
  (normalize-oos-defaults)
  
  ;; 1. Culling
  (run-b-rank-culling)
  
  ;; 2. A-Rank Promotion (V49.3: THE MISSING LINK)
  (let ((a-ranks (get-strategies-by-rank :A)))
    (format t "[RANK] 🧐 Evaluating ~d A-Rank strategies for promotion...~%" (length a-ranks))
    (dolist (s a-ranks)
      (evaluate-a-rank-strategy s)))
  
  ;; Report status
  (let ((b-count (length (get-strategies-by-rank :B)))
        (a-count (length (get-strategies-by-rank :A)))
        (s-count (length (get-strategies-by-rank :S)))
        (g-count (length (get-strategies-by-rank :graveyard)))
        (l-count (length (get-strategies-by-rank :legend))))
    
    (format t "[RANK] 📊 Status: B=~d A=~d S=~d Graveyard=~d Legend=~d~%"
            b-count a-count s-count g-count l-count)
            
    ;; Audit Alert
    (when (> b-count 5000)
      (format t "[RANK] ⚠️ B-Rank bloat detected (~d). Check culling logic.~%" b-count))
    
    ;; 4. Global Portfolio Construction (The Draft) - Moved to school-portfolio.lisp
    (construct-global-portfolio)))


;;; ---------------------------------------------------------------------------
;;; DIRECTION AUTO-DETECTION (V47.2)
;;; ---------------------------------------------------------------------------

(defun detect-direction-from-entry (entry-code)
  "Analyze entry code to detect trade direction.
   Returns :BUY, :SELL, or :BOTH.
   V47.2: Owner's Vision - Auto-detect direction from strategy logic."
  (let ((entry-str (format nil "~a" entry-code)))
    (cond
      ;; BUY-only patterns
      ((and (search "BUY" (string-upcase entry-str))
            (not (search "SELL" (string-upcase entry-str))))
       :BUY)
      ((and (search "LONG" (string-upcase entry-str))
            (not (search "SHORT" (string-upcase entry-str))))
       :BUY)
      ;; SELL-only patterns  
      ((and (search "SELL" (string-upcase entry-str))
            (not (search "BUY" (string-upcase entry-str))))
       :SELL)
      ((and (search "SHORT" (string-upcase entry-str))
            (not (search "LONG" (string-upcase entry-str))))
       :SELL)
      ;; Default: Both directions
      (t :BOTH))))

(defun auto-set-strategy-direction (strategy)
  "Automatically set strategy direction based on entry logic."
  (let ((detected (detect-direction-from-entry (strategy-entry strategy))))
    (setf (strategy-direction strategy) detected)
    (format t "[RANK] 🎯 Direction detected for ~a: ~a~%" 
            (strategy-name strategy) detected)
    detected))

;;; ---------------------------------------------------------------------------
;;; BREEDING HELPERS (V47.0)
;;; ---------------------------------------------------------------------------

(defun can-breed-p (strategy)
  "Check if strategy can be used for breeding.
   Returns T if under breeding limit or is Legend."
  (and strategy
       (not (and (slot-exists-p strategy 'revalidation-pending)
                 (strategy-revalidation-pending strategy)))
       (not (eq (strategy-status strategy) :killed))
       (not (eq (strategy-rank strategy) :legend-archive))
       ;; Sanity checks to avoid pathological SL/TP values
       (numberp (strategy-sl strategy))
       (< (abs (strategy-sl strategy)) 1000.0)
       (numberp (strategy-tp strategy))
       (< (abs (strategy-tp strategy)) 1000.0)
       (or (eq (strategy-rank strategy) :legend)
           (< (or (strategy-breeding-count strategy) 0) *max-breeding-uses*))))

(defun run-legend-breeding ()
  "Breed Legend strategies with random B-rank strategies.
   V47.0: Owner's Vision - Legends participate in periodic random breeding."
  (format t "[LEGEND] 👑 Starting Legend Breeding Cycle...~%")
  (let* ((legends (remove-if (lambda (s)
                               (or (eq (strategy-rank s) :legend-archive)
                                   (and (slot-exists-p s 'revalidation-pending)
                                        (strategy-revalidation-pending s))))
                             (get-strategies-by-rank :legend)))
         (b-ranks (remove-if (lambda (s)
                               (and (slot-exists-p s 'revalidation-pending)
                                    (strategy-revalidation-pending s)))
                             (get-strategies-by-rank :B)))
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

;;; Note: RL, Graveyard, Q-learning, and File Rotation functions moved to school-learning.lisp (V47.3)

(defun apply-backtest-result (name metrics)
  "Apply backtest metrics to a strategy and trigger rank evaluation if necessary."
  (let ((strat (find-strategy name)))
    (if strat
        (progn
          (setf (strategy-sharpe strat) (float (getf metrics :sharpe 0.0))
                (strategy-profit-factor strat) (float (getf metrics :profit-factor 0.0))
                (strategy-win-rate strat) (float (getf metrics :win-rate 0.0))
                (strategy-trades strat) (getf metrics :trades 0)
                (strategy-max-dd strat) (float (getf metrics :max-dd 0.0))
                (strategy-cpcv-median-sharpe strat) (float (getf metrics :cpcv-median 0.0))
                (strategy-cpcv-median-pf strat) (float (getf metrics :cpcv-median-pf 0.0))
                (strategy-cpcv-median-wr strat) (float (getf metrics :cpcv-median-wr 0.0))
                (strategy-cpcv-median-maxdd strat) (float (getf metrics :cpcv-median-maxdd 0.0))
                (strategy-cpcv-pass-rate strat) (float (getf metrics :cpcv-pass-rate 0.0)))
          (when (and (slot-exists-p strat 'revalidation-pending)
                     (strategy-revalidation-pending strat))
            (setf (strategy-revalidation-pending strat) nil))
          ;; DEBUG V50.5.1
          (let ((s (strategy-sharpe strat)))
            (when (zerop s)
               (format t "[DB] ⚠️ Appplying Metrics for ~a: Sharpe is zero! Metrics: ~a~%" name metrics)))
          (upsert-strategy strat)
          ;; Trigger Phase 1 Evaluation if currently unranked
          (when (null (strategy-rank strat))
            (evaluate-new-strategy strat))
          t)
        ;; Fallback: update DB even if in-memory strategy is missing
        (progn
          (let ((updated nil)
                (sexp-str (ignore-errors
                           (execute-single "SELECT data_sexp FROM strategies WHERE name = ?" name))))
            (when (and sexp-str (stringp sexp-str))
              (handler-case
                  (let ((obj (swimmy.core:safe-read-sexp sexp-str :package :swimmy.school)))
                    (when (strategy-p obj)
                      (setf (strategy-sharpe obj) (float (getf metrics :sharpe 0.0))
                            (strategy-profit-factor obj) (float (getf metrics :profit-factor 0.0))
                            (strategy-win-rate obj) (float (getf metrics :win-rate 0.0))
                            (strategy-trades obj) (getf metrics :trades 0)
                            (strategy-max-dd obj) (float (getf metrics :max-dd 0.0))
                            (strategy-oos-sharpe obj) (float (getf metrics :oos-sharpe 0.0))
                            (strategy-cpcv-median-sharpe obj) (float (getf metrics :cpcv-median 0.0))
                            (strategy-cpcv-median-pf obj) (float (getf metrics :cpcv-median-pf 0.0))
                            (strategy-cpcv-median-wr obj) (float (getf metrics :cpcv-median-wr 0.0))
                            (strategy-cpcv-median-maxdd obj) (float (getf metrics :cpcv-median-maxdd 0.0))
                            (strategy-cpcv-pass-rate obj) (float (getf metrics :cpcv-pass-rate 0.0)))
                      (when (fboundp '(setf strategy-revalidation-pending))
                        (setf (strategy-revalidation-pending obj) nil))
                      (upsert-strategy obj)
                      (execute-non-query
                       "UPDATE strategies SET last_bt_time=? WHERE name=?"
                       (get-universal-time)
                       name)
                      (setf updated t)))
                (error (e)
                  (format t "[DB] ⚠️ Failed to parse data_sexp for ~a: ~a~%" name e))))
            (unless updated
              (execute-non-query
               "UPDATE strategies SET sharpe=?, profit_factor=?, win_rate=?, trades=?, max_dd=?, last_bt_time=? WHERE name=?"
               (float (getf metrics :sharpe 0.0))
               (float (getf metrics :profit-factor 0.0))
               (float (getf metrics :win-rate 0.0))
               (getf metrics :trades 0)
               (float (getf metrics :max-dd 0.0))
               (get-universal-time)
               name)))
          nil))))
