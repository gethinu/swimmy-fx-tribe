;;; src/lisp/school/school-rank-system.lisp
;;; ============================================================================
;;; B/A/S RANK SYSTEM (V47.0 - Owner's Vision)
;;; ============================================================================
;;; Implements the new rank-based lifecycle per Expert Panel (2026-01-21).
;;;
;;; RANKS:
;;; 1. :B - Phase 1 Backtest passed (Sharpe>=0.15, PF>=1.05, WR>=35%, MaxDD<25%)
;;; 2. :A - OOS validated (Sharpe>=0.45, PF>=1.30, WR>=43%, MaxDD<16%, OOS>=0.35)
;;; 3. :S - Research elite rank (live deployment is decided by deployment_gate_status)
;;;          (Sharpe>=0.75, PF>=1.70, WR>=50%, MaxDD<10%, CPCV pass>=70%, CPCV median MaxDD<12%)
;;; 4. :graveyard - Failed strategies (learning data)
;;; 5. :retired - Max Age archive (low-weight learning)
;;; 6. :legend - Protected strategies (61 total, never discarded)
;;; ============================================================================

(in-package :swimmy.school)

;;; ---------------------------------------------------------------------------
;;; RANK CONSTANTS
;;; ---------------------------------------------------------------------------

(defparameter *rank-criteria*
  '((:B       :sharpe-min 0.15 :pf-min 1.05 :wr-min 0.35 :maxdd-max 0.25)
    (:A       :sharpe-min 0.45 :pf-min 1.30 :wr-min 0.43 :maxdd-max 0.16 :oos-min 0.35)
    (:S       :sharpe-min 0.75 :pf-min 1.70 :wr-min 0.50 :maxdd-max 0.10
              :cpcv-pass-min 0.70 :cpcv-maxdd-max 0.12))
  "Rank criteria thresholds. All conditions must be met (AND logic).")

(defparameter *culling-threshold* 20
  "Number of B-RANK strategies per TF before culling begins.")

(defparameter *culling-bootstrap-when-no-a* t
  "When T, allow A-promotion bootstrap below threshold if A-rank is empty.")

(defparameter *culling-bootstrap-min-count* 2
  "Minimum B count required for below-threshold bootstrap promotion.")

(defparameter *culling-bootstrap-max-a-count* 8
  "Allow below-threshold bootstrap while A-rank count is <= this value.")

(defparameter *a-rank-slots-per-tf* 2
  "Only top 2 strategies per TF can be promoted to A-RANK.")

(defparameter *max-breeding-uses* 30
  "Maximum breeding reuse count for non-Legend parents.")
(defparameter *breeder-min-parent-trades* 30
  "Minimum trade evidence required for non-Legend breeding parents.")
(defparameter *breeder-parent-quality-floor-enabled* t
  "When T, non-Legend breeding parents must satisfy Stage1 B quality floor.")
(defparameter *breeder-parent-oos-floor-enabled* t
  "When T, selected ranks require minimum OOS Sharpe to be eligible as breeding parents.")
(defparameter *breeder-parent-oos-required-ranks* '(:A :S)
  "Ranks that require OOS floor in can-breed-p.")
(defparameter *breeder-parent-min-oos-sharpe* 0.35
  "Minimum OOS Sharpe required for ranks in *breeder-parent-oos-required-ranks*.")
(defparameter *breeder-parent-rank-conformance-enabled* t
  "When T, A/S parents must satisfy their own rank criteria in can-breed-p.")

(defparameter *a-candidate-category-metrics* (make-hash-table :test 'equal)
  "Latest A-candidate funnel metrics per category key (timeframe direction symbol).")

(defparameter *s-rank-staged-pf-wr-enabled* t
  "When T, S-rank PF/WR gates use trade-evidence stages.")

(defparameter *s-rank-staged-pf-wr-spec*
  '((:min-trades 150 :pf-min 1.30 :wr-min 0.38)
    (:min-trades 100 :pf-min 1.40 :wr-min 0.42)
    (:min-trades 30 :pf-min 1.55 :wr-min 0.45))
  "Descending stage specs for S-rank PF/WR gates keyed by trade evidence.")

(defparameter *a-rank-min-trade-evidence* 50
  "Minimum trade evidence required before A-rank eligibility.")

(defparameter *s-rank-min-trade-evidence* 100
  "Minimum trade evidence required before S-rank eligibility.")

(defparameter *enforce-rank-trade-evidence-floors* t
  "When T, rank evaluation demotes existing A/S strategies that violate trade-evidence floors.")

(defparameter *enforce-s-rank-criteria-conformance* t
  "When T, rank evaluation demotes existing S strategies that no longer satisfy S criteria.")

(defparameter *enforce-a-b-rank-criteria-conformance* t
  "When T, rank evaluation enforces A/B criteria conformance on existing ranks.")

(defvar *trade-evidence-count-cache* nil
  "Optional preloaded hash-table of canonical strategy-name -> persisted backtest trade count.")

;;; ArmadaAC Core Profile (behavioral replica)
(defun armada-core-env-value (key)
  "Read env value from process env (and .env fallback when available)."
  (or (ignore-errors
        (when (fboundp 'swimmy.core::getenv-or-dotenv)
          (swimmy.core::getenv-or-dotenv key)))
      (ignore-errors (uiop:getenv key))))

(defun armada-core-env-bool-or (key default)
  "Parse boolean env KEY with DEFAULT fallback."
  (let ((raw (armada-core-env-value key)))
    (if (and (stringp raw) (> (length raw) 0))
        (not (null (member (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))
                           '("1" "true" "yes" "on" "y" "t")
                           :test #'string=)))
        default)))

(defun armada-core-env-float-or (key default)
  "Parse numeric env KEY as float with DEFAULT fallback."
  (let* ((raw (armada-core-env-value key))
         (num (and (stringp raw)
                   (> (length raw) 0)
                   (ignore-errors (safe-parse-number raw)))))
    (if (numberp num)
        (float num 1.0)
        (float default 1.0))))

(defun armada-core-env-int-or (key default)
  "Parse integer env KEY with DEFAULT fallback."
  (let* ((raw (armada-core-env-value key))
         (num (and (stringp raw)
                   (> (length raw) 0)
                   (ignore-errors (parse-integer raw :junk-allowed t)))))
    (if (integerp num) num default)))

(defun armada-core-parse-rank-token (token)
  "Parse rank token string into keyword rank."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) token))
         (up (string-upcase trimmed)))
    (cond
      ((string= up "A") :A)
      ((string= up "S") :S)
      ((string= up "B") :B)
      ((string= up "LEGEND") :LEGEND)
      (t nil))))

(defun armada-core-env-ranks-or (key default)
  "Parse comma/space-delimited rank list env KEY."
  (let ((raw (armada-core-env-value key)))
    (if (and (stringp raw) (> (length raw) 0))
        (let* ((tokens (uiop:split-string raw :separator '(#\, #\Space #\Tab #\Newline)))
               (ranks (remove nil (mapcar #'armada-core-parse-rank-token tokens))))
          (if ranks (remove-duplicates ranks :test #'eq) default))
        default)))

(defparameter *armada-core-canary-mode-enabled*
  (armada-core-env-bool-or "SWIMMY_ARMADA_CANARY_MODE" nil)
  "When T, force Armada canary preset (A-rank only).")

(defparameter *armada-core-profile-enabled*
  (armada-core-env-bool-or "SWIMMY_ARMADA_CORE_PROFILE_ENABLED" nil)
  "When T, apply Armada Core profile gate to selected rank checks.")
(defparameter *armada-core-apply-ranks*
  (armada-core-env-ranks-or "SWIMMY_ARMADA_CORE_APPLY_RANKS" '(:A :S))
  "Ranks where Armada Core profile gate is enforced.")
(defparameter *armada-core-pf-min*
  (armada-core-env-float-or "SWIMMY_ARMADA_CORE_PF_MIN" 1.60)
  "Armada Core minimum PF.")
(defparameter *armada-core-maxdd-max*
  (armada-core-env-float-or "SWIMMY_ARMADA_CORE_MAXDD_MAX" 0.12)
  "Armada Core maximum drawdown (ratio).")
(defparameter *armada-core-min-trades*
  (armada-core-env-int-or "SWIMMY_ARMADA_CORE_MIN_TRADES" 250)
  "Armada Core minimum trade evidence.")
(defparameter *armada-core-min-avg-hold-seconds*
  (armada-core-env-int-or "SWIMMY_ARMADA_CORE_MIN_AVG_HOLD_SECONDS" 10800)
  "Armada Core minimum average hold-time (3h).")
(defparameter *armada-core-max-avg-hold-seconds*
  (armada-core-env-int-or "SWIMMY_ARMADA_CORE_MAX_AVG_HOLD_SECONDS" 36000)
  "Armada Core maximum average hold-time (10h).")
(defparameter *armada-core-max-side-share*
  (armada-core-env-float-or "SWIMMY_ARMADA_CORE_MAX_SIDE_SHARE" 0.80)
  "Armada Core maximum directional side concentration.")
(defparameter *armada-core-require-trade-profile*
  (armada-core-env-bool-or "SWIMMY_ARMADA_CORE_REQUIRE_TRADE_PROFILE" nil)
  "When T, missing hold-time/side profile fails Armada gate.")

(defun enable-armada-core-canary-mode ()
  "Enable Armada Core canary preset (A-rank only)."
  (setf *armada-core-profile-enabled* t
        *armada-core-apply-ranks* '(:A)
        *armada-core-require-trade-profile* t)
  t)

(when *armada-core-canary-mode-enabled*
  (enable-armada-core-canary-mode))

;;; ---------------------------------------------------------------------------
;;; COMPOSITE SCORE (Multi-Metric)
;;; ---------------------------------------------------------------------------

(defparameter *score-weight-sharpe* 0.45)
(defparameter *score-weight-pf* 0.25)
(defparameter *score-weight-wr* 0.20)
(defparameter *score-weight-maxdd* 0.10)
(defparameter *score-trade-confidence-k* 120.0
  "Evidence scaling constant for Sharpe confidence shrinkage in composite score.")
(defparameter *score-trade-confidence-floor* 0.55
  "Minimum Sharpe confidence multiplier when trade evidence is sparse.")
(defparameter *culling-a-base-deficit-penalty-enabled* t
  "When T, B-rank culling penalizes candidates farther from A-base thresholds.")
(defparameter *culling-a-base-deficit-penalty-weight* 1.5
  "Penalty multiplier for A-base deficit in B-rank culling score.")
(defparameter *a-base-deficit-weight-sharpe* 1.0
  "Weight for sharpe shortfall in A-base deficit score.")
(defparameter *a-base-deficit-weight-pf* 1.7
  "Weight for PF shortfall in A-base deficit score (PF-priority).")
(defparameter *a-base-deficit-weight-wr* 1.0
  "Weight for WR shortfall in A-base deficit score.")
(defparameter *a-base-deficit-weight-maxdd* 1.0
  "Weight for maxdd excess in A-base deficit score.")
(defparameter *culling-pf-near-offset* 0.06
  "Width of PF near band below A-gate PF for B-rank culling priority.")

(defun %clamp (v lo hi)
  (min hi (max lo v)))

(defun %norm (v lo hi)
  (if (<= hi lo)
      0.0
      (let* ((v (float v 1.0))
             (lo (float lo 1.0))
             (hi (float hi 1.0)))
        (/ (- (%clamp v lo hi) lo) (- hi lo)))))

(defun trade-evidence-confidence-factor (trades)
  "Return confidence multiplier [floor,1] from trade evidence count.
Low trade counts are softly shrunk instead of hard-blocked."
  (let* ((n (if (numberp trades) (max 0.0 (float trades 1.0)) 0.0))
         (k (max 1.0 (float *score-trade-confidence-k* 1.0)))
         (floor (%clamp (float *score-trade-confidence-floor* 1.0) 0.0 1.0))
         (ratio (/ n (+ n k))))
    (+ floor (* (- 1.0 floor) ratio))))

(defun evidence-adjusted-sharpe (sharpe trades)
  "Shrink Sharpe by trade-evidence confidence to avoid sparse-sample overvaluation."
  (* (float (or sharpe 0.0) 1.0)
     (trade-evidence-confidence-factor trades)))

(defun score-from-metrics (metrics)
  "Compute composite score from metrics plist."
  (let* ((sharpe (or (getf metrics :sharpe) 0.0))
         (pf (or (getf metrics :profit-factor) 0.0))
         (wr (or (getf metrics :win-rate) 0.0))
         (trades (getf metrics :trades nil))
         (adj-sharpe (if (numberp trades)
                         (evidence-adjusted-sharpe sharpe trades)
                         (float sharpe 1.0)))
         (dd (or (getf metrics :max-dd) 1.0))
         (n-sharpe (%norm adj-sharpe 0.0 2.0))
         (n-pf (%norm pf 1.0 2.0))
         (n-wr (%norm wr 0.40 0.70))
         (n-dd (%norm dd 0.0 0.20)))
    (+ (* *score-weight-sharpe* n-sharpe)
       (* *score-weight-pf* n-pf)
       (* *score-weight-wr* n-wr)
       (* -1 *score-weight-maxdd* n-dd))))

(defun %safe-deficit-ratio (shortfall threshold)
  "Normalize metric shortfall by threshold."
  (if (<= threshold 1.0e-9)
      0.0
      (/ shortfall threshold)))

(defun a-base-deficit-score (strategy)
  "Return aggregate distance from A-base thresholds (0 = passes A-base)."
  (let* ((criteria (get-rank-criteria :A))
         (sharpe (float (or (strategy-sharpe strategy) 0.0) 1.0))
         (pf (float (or (strategy-profit-factor strategy) 0.0) 1.0))
         (wr (float (or (strategy-win-rate strategy) 0.0) 1.0))
         (maxdd (float (or (strategy-max-dd strategy) 1.0) 1.0))
         (sh-min (float (or (getf criteria :sharpe-min) 0.45) 1.0))
         (pf-min (float (or (getf criteria :pf-min) 1.30) 1.0))
         (wr-min (float (or (getf criteria :wr-min) 0.43) 1.0))
         (dd-max (float (or (getf criteria :maxdd-max) 0.16) 1.0))
         (sh-def (if (>= sharpe sh-min) 0.0
                     (%safe-deficit-ratio (- sh-min sharpe) sh-min)))
         (pf-def (if (>= pf pf-min) 0.0
                     (%safe-deficit-ratio (- pf-min pf) pf-min)))
         (wr-def (if (>= wr wr-min) 0.0
                     (%safe-deficit-ratio (- wr-min wr) wr-min)))
         (dd-def (if (< maxdd dd-max) 0.0
                     (%safe-deficit-ratio (- maxdd dd-max) dd-max))))
    (+ (* *a-base-deficit-weight-sharpe* sh-def)
       (* *a-base-deficit-weight-pf* pf-def)
       (* *a-base-deficit-weight-wr* wr-def)
       (* *a-base-deficit-weight-maxdd* dd-def))))

(defun strategy-culling-score (strategy)
  "Composite culling score with optional A-base deficit penalty."
  (let* ((base (score-from-metrics
                (list :sharpe (or (strategy-sharpe strategy) 0.0)
                      :profit-factor (or (strategy-profit-factor strategy) 0.0)
                      :win-rate (or (strategy-win-rate strategy) 0.0)
                      :trades (or (strategy-trades strategy) 0)
                      :max-dd (or (strategy-max-dd strategy) 1.0))))
         (penalty (if *culling-a-base-deficit-penalty-enabled*
                      (* *culling-a-base-deficit-penalty-weight*
                         (a-base-deficit-score strategy))
                      0.0)))
    (- base penalty)))

(defun strategy-culling-pf-priority-band (strategy)
  "Return PF priority band for B-rank culling keep order.
   2 = PF passes A-gate, 1 = PF near A-gate, 0 = others."
  (let* ((criteria (get-rank-criteria :A))
         (pf-min (float (or (getf criteria :pf-min) 1.30) 1.0))
         (pf-near-min (max 0.0 (- pf-min *culling-pf-near-offset*)))
         (pf (float (or (strategy-profit-factor strategy) 0.0) 1.0)))
    (cond
      ((>= pf pf-min) 2)
      ((>= pf pf-near-min) 1)
      (t 0))))

(defun strategy-culling-priority-score (strategy)
  "PF-prioritized keep score for B-rank culling.
   PF band precedence is hard (A-pass > near > others), then culling score."
  (let* ((band (strategy-culling-pf-priority-band strategy))
         (pf (float (or (strategy-profit-factor strategy) 0.0) 1.0))
         (base (strategy-culling-score strategy)))
    (+ (* 1000.0 (float band 1.0))
       (* 10.0 pf)
       base)))

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

(defun strategy-trade-evidence-count (strategy)
  "Best-effort trade evidence count for staged S-rank gates.
Combines backtest evidence with shadow-paper trade evidence."
  (let* ((history-count (length (remove-if-not #'numberp (or (strategy-pnl-history strategy) '()))))
         (trade-count (or (strategy-trades strategy) 0))
         (local-count (max history-count trade-count))
         ;; Query persisted backtest evidence only when local evidence is still sparse.
         (query-threshold (max *a-rank-min-trade-evidence* *s-rank-min-trade-evidence*))
         (name (strategy-name strategy))
         (canonical-name (if (and name (stringp name) (fboundp '%canonicalize-backtest-strategy-name))
                             (%canonicalize-backtest-strategy-name name)
                             name))
         (backtest-count
           (if (and canonical-name
                    (stringp canonical-name)
                    (< local-count query-threshold))
               (cond
                 ((hash-table-p *trade-evidence-count-cache*)
                  (or (gethash canonical-name *trade-evidence-count-cache* 0) 0))
                 ((fboundp 'count-backtest-trades-for-strategy)
                  (handler-case
                      (or (count-backtest-trades-for-strategy canonical-name) 0)
                    (error (e)
                      (when (fboundp 'swimmy.core::emit-telemetry-event)
                        (swimmy.core::emit-telemetry-event
                         "rank.trade_evidence_db_error"
                         :service "school"
                         :severity :warn
                         :data (list :strategy canonical-name
                                     :local-count local-count
                                     :error (format nil "~a" e))))
                      0)))
                 (t 0))
               0))
         (shadow-count
           (if (and canonical-name
                    (stringp canonical-name)
                    (< local-count query-threshold)
                    (fboundp 'count-trade-logs-for-strategy))
               (handler-case
                   (or (count-trade-logs-for-strategy canonical-name :modes '(:shadow)) 0)
                 (error (e)
                   (when (fboundp 'swimmy.core::emit-telemetry-event)
                     (swimmy.core::emit-telemetry-event
                      "rank.trade_evidence_shadow_error"
                      :service "school"
                      :severity :warn
                      :data (list :strategy canonical-name
                                  :local-count local-count
                                  :error (format nil "~a" e))))
                   0))
               0))
         (base-count (max local-count backtest-count)))
    (+ base-count shadow-count)))

(defun armada-core-profile-applies-p (rank)
  "Return T when Armada Core profile gate applies to RANK."
  (and *armada-core-profile-enabled*
       (member rank *armada-core-apply-ranks* :test #'eq)))

(defun %normalize-trade-direction-token (value)
  "Normalize direction text from DB rows into :BUY/:SELL/:UNKNOWN."
  (let ((txt (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         (format nil "~a" (or value ""))))))
    (cond
      ((or (string= txt "BUY") (string= txt ":BUY") (string= txt "LONG") (string= txt ":LONG")) :buy)
      ((or (string= txt "SELL") (string= txt ":SELL") (string= txt "SHORT") (string= txt ":SHORT")) :sell)
      (t :unknown))))

(defun armada-strategy-trade-profile (strategy &key oos-kind)
  "Return trade behavior profile plist for STRATEGY from persisted backtest logs.
Keys:
  :trade-count :buy-count :sell-count :hold-sample-count
  :avg-hold-seconds :max-side-share"
  (let ((name (and strategy (strategy-name strategy))))
    (when (and name (stringp name) (fboundp 'fetch-backtest-trades))
      (handler-case
          (let* ((rows (fetch-backtest-trades name :oos-kind oos-kind))
                 (trade-count (length rows))
                 (buy-count 0)
                 (sell-count 0)
                 (hold-sum 0.0d0)
                 (hold-count 0))
            (dolist (row rows)
              ;; fetch-backtest-trades row:
              ;; (request_id strategy_name timestamp pnl symbol direction ... hold_time ...)
              (let ((direction (nth 5 row))
                    (hold-time (nth 11 row)))
                (case (%normalize-trade-direction-token direction)
                  (:buy (incf buy-count))
                  (:sell (incf sell-count)))
                (when (numberp hold-time)
                  (incf hold-sum (float hold-time 1.0d0))
                  (incf hold-count))))
            (let* ((directional-total (+ buy-count sell-count))
                   (avg-hold-seconds (when (> hold-count 0)
                                       (/ hold-sum (float hold-count 1.0d0))))
                   (max-side-share (when (> directional-total 0)
                                     (/ (float (max buy-count sell-count) 1.0d0)
                                        (float directional-total 1.0d0)))))
              (list :trade-count trade-count
                    :buy-count buy-count
                    :sell-count sell-count
                    :hold-sample-count hold-count
                    :avg-hold-seconds avg-hold-seconds
                    :max-side-share max-side-share)))
        (error (e)
          (format t "[RANK] ⚠️ Armada profile lookup failed for ~a: ~a~%" name e)
          nil)))))

(defun armada-core-profile-passed-p (strategy &key trade-profile)
  "Return T when STRATEGY matches Armada Core behavioral profile."
  (let* ((pf (float (or (strategy-profit-factor strategy) 0.0) 1.0))
         (maxdd (float (or (strategy-max-dd strategy) 1.0) 1.0))
         (trade-evidence (strategy-trade-evidence-count strategy))
         (profile (or trade-profile
                      (armada-strategy-trade-profile strategy)))
         (avg-hold-seconds (and profile (getf profile :avg-hold-seconds)))
         (max-side-share (and profile (getf profile :max-side-share)))
         (base-pass (and (>= pf (float *armada-core-pf-min* 1.0))
                         (<= maxdd (float *armada-core-maxdd-max* 1.0))
                         (>= trade-evidence *armada-core-min-trades*)))
         (hold-pass (if (numberp avg-hold-seconds)
                        (and (>= avg-hold-seconds (float *armada-core-min-avg-hold-seconds* 1.0))
                             (<= avg-hold-seconds (float *armada-core-max-avg-hold-seconds* 1.0)))
                        (not *armada-core-require-trade-profile*)))
         (side-pass (if (numberp max-side-share)
                        (<= max-side-share (float *armada-core-max-side-share* 1.0))
                        (not *armada-core-require-trade-profile*))))
    (and base-pass hold-pass side-pass)))

(defun min-trade-evidence-for-rank (rank)
  "Return minimum trade evidence required for rank gate."
  (case rank
    (:A *a-rank-min-trade-evidence*)
    (:S *s-rank-min-trade-evidence*)
    (otherwise 0)))

(defun effective-s-rank-criteria (strategy)
  "Return effective S-rank criteria considering staged PF/WR thresholds.
   Values: criteria-plist, trade-evidence-count, matched-stage-spec-or-nil."
  (let* ((criteria (copy-list (get-rank-criteria :S)))
         (trade-evidence (strategy-trade-evidence-count strategy))
         (stage-spec (and *s-rank-staged-pf-wr-enabled*
                          (find-if (lambda (spec)
                                     (>= trade-evidence (getf spec :min-trades 0)))
                                   *s-rank-staged-pf-wr-spec*))))
    (when stage-spec
      (setf (getf criteria :pf-min) (getf stage-spec :pf-min (getf criteria :pf-min 1.70))
            (getf criteria :wr-min) (getf stage-spec :wr-min (getf criteria :wr-min 0.50))))
    (values criteria trade-evidence stage-spec)))

(defun check-rank-criteria (strategy target-rank &key (include-oos t) (include-cpcv t))
  "Check if strategy meets all criteria for target-rank.
   Returns T if all conditions pass, NIL otherwise.
   Optional gates: INCLUDE-OOS/INCLUDE-CPCV can be disabled for pre-validation checks."
  (let* ((criteria (if (eq target-rank :S)
                       (nth-value 0 (effective-s-rank-criteria strategy))
                       (get-rank-criteria target-rank)))
         (sharpe (or (strategy-sharpe strategy) 0.0))
         (pf (or (strategy-profit-factor strategy) 0.0))
         (wr (or (strategy-win-rate strategy) 0.0))
         (maxdd (or (strategy-max-dd strategy) 1.0))
         (raw-trades (or (strategy-trades strategy) 0))
         (trade-evidence (strategy-trade-evidence-count strategy))
         (min-trade-evidence (min-trade-evidence-for-rank target-rank))
         (armada-pass (or (not (armada-core-profile-applies-p target-rank))
                          (armada-core-profile-passed-p strategy)))
         ;; Founder Phase1 recovery candidates can retain B-rank below strict Sharpe floor.
         ;; This keeps V2 founder gate semantics consistent with later B conformance sweeps.
         (founder-b-recovery-pass
           (and (eq target-rank :B)
                (fboundp 'founder-phase1-recovery-passed-p)
                (ignore-errors
                  (founder-phase1-recovery-passed-p
                   strategy sharpe pf raw-trades maxdd)))))
    (cond
      ((eq target-rank :S)
       (and (>= trade-evidence min-trade-evidence)
            armada-pass
            (>= sharpe (getf criteria :sharpe-min 0))
            (>= pf (getf criteria :pf-min 0))
            (>= wr (getf criteria :wr-min 0))
            (< maxdd (getf criteria :maxdd-max 1.0))
            (or (not include-cpcv)
                ;; vNext: S stage2 gate uses CPCV pass-rate + median maxdd
                (and (>= (or (strategy-cpcv-pass-rate strategy) 0.0) (getf criteria :cpcv-pass-min 0))
                     (< (or (strategy-cpcv-median-maxdd strategy) 1.0) (getf criteria :cpcv-maxdd-max 1.0))))))
      (t
       (and (>= trade-evidence min-trade-evidence)
            armada-pass
            (or founder-b-recovery-pass
                (and (>= sharpe (getf criteria :sharpe-min 0))
                     (>= pf (getf criteria :pf-min 0))
                     (>= wr (getf criteria :wr-min 0))
                     (< maxdd (getf criteria :maxdd-max 1.0))))
            ;; V50.3: Gate Lockdown
            (cond
              ((eq target-rank :A)
               (or (not include-oos)
                   (>= (or (strategy-oos-sharpe strategy) 0.0) (getf criteria :oos-min 0))))
              (t t)))))))

(defun meets-rank-criteria-p (strategy target-rank)
  "Compatibility wrapper for legacy callers/tests.
   Uses pre-validation semantics (no OOS/CPCV hard requirements)."
  (check-rank-criteria strategy target-rank :include-oos nil :include-cpcv nil))

(defun %s-gate-label (gate)
  (case gate
    (:sharpe "sharpe")
    (:trade-evidence "trade-evidence")
    (:pf "pf")
    (:wr "wr")
    (:maxdd "maxdd")
    (:cpcv-pass-rate "cpcv-pass-rate")
    (:cpcv-maxdd "cpcv-maxdd")
    (:common-stage2 "common-stage2")
    (t (string-downcase (string gate)))))

(defun s-rank-block-diagnostics (strategy &key (include-common-stage2 t))
  "Collect S-gate diagnostics for blocked promotions."
  (multiple-value-bind (criteria trade-evidence stage-spec)
      (effective-s-rank-criteria strategy)
    (let* ((sharpe (or (strategy-sharpe strategy) 0.0))
           (pf (or (strategy-profit-factor strategy) 0.0))
           (wr (or (strategy-win-rate strategy) 0.0))
           (maxdd (or (strategy-max-dd strategy) 1.0))
           (min-trade-evidence (min-trade-evidence-for-rank :S))
           (pass-rate (or (strategy-cpcv-pass-rate strategy) 0.0))
           (cpcv-maxdd (or (strategy-cpcv-median-maxdd strategy) 1.0))
           (failed '())
           (common-stage2-message nil))
      (when (< trade-evidence min-trade-evidence) (push :trade-evidence failed))
      (when (< sharpe (getf criteria :sharpe-min 0.0)) (push :sharpe failed))
      (when (< pf (getf criteria :pf-min 0.0)) (push :pf failed))
      (when (< wr (getf criteria :wr-min 0.0)) (push :wr failed))
      (when (>= maxdd (getf criteria :maxdd-max 1.0)) (push :maxdd failed))
      (when (< pass-rate (getf criteria :cpcv-pass-min 0.0)) (push :cpcv-pass-rate failed))
      (when (>= cpcv-maxdd (getf criteria :cpcv-maxdd-max 1.0)) (push :cpcv-maxdd failed))
      (when (and include-common-stage2 (fboundp 'common-stage2-gates-passed-p))
        (multiple-value-bind (passed message) (common-stage2-gates-passed-p strategy)
          (unless passed
            (push :common-stage2 failed)
            (setf common-stage2-message message))))
      (list :failed-gates (nreverse failed)
            :trade-evidence trade-evidence
            :min-trade-evidence min-trade-evidence
            :stage-min-trades (and stage-spec (getf stage-spec :min-trades))
            :sharpe sharpe
            :pf pf
            :wr wr
            :maxdd maxdd
            :pf-min (getf criteria :pf-min)
            :wr-min (getf criteria :wr-min)
            :cpcv-pass-rate pass-rate
            :cpcv-maxdd cpcv-maxdd
            :common-stage2-message common-stage2-message))))

(defun %format-s-rank-failed-gates (failed-gates)
  (if failed-gates
      (format nil "~{~a~^, ~}" (mapcar #'%s-gate-label failed-gates))
      "unknown"))

(defun %a-candidate-category-key (timeframe direction symbol)
  (list timeframe direction symbol))

(defun reset-a-candidate-category-metrics ()
  "Clear in-memory category funnel metrics."
  (clrhash *a-candidate-category-metrics*))

	(defun record-a-candidate-category-metric (timeframe direction symbol
                                           &key
                                             (b-count 0)
                                             (a-base-count 0)
                                             (a-ready-count 0)
                                             (queued-count 0)
                                             (bootstrap-p nil)
                                             (culling-triggered-p nil))
  "Record per-category A-candidate funnel metrics."
  (let* ((snapshot (list :timeframe timeframe
                         :direction direction
                         :symbol symbol
                         :b-count b-count
                         :a-base-count a-base-count
                         :a-ready-count a-ready-count
                         :queued-count queued-count
                         :bootstrap-p bootstrap-p
                         :culling-triggered-p culling-triggered-p
                         :updated-at (get-universal-time)))
         (key (%a-candidate-category-key timeframe direction symbol)))
	    (setf (gethash key *a-candidate-category-metrics*) snapshot)
	    (when (fboundp 'swimmy.core::emit-telemetry-event)
	      (swimmy.core::emit-telemetry-event "rank.a_candidate_funnel"
	        :service "school"
	        :severity "info"
	        :correlation-id (format nil "~a|~a|~a"
	                                symbol
	                                direction
	                                (if (fboundp 'get-tf-string) (get-tf-string timeframe) (format nil "M~a" timeframe)))
	        :data snapshot))
	    snapshot))

(defun lookup-a-candidate-category-metric (timeframe direction symbol)
  "Lookup latest funnel metric snapshot for a category."
  (gethash (%a-candidate-category-key timeframe direction symbol)
           *a-candidate-category-metrics*))

(defun a-candidate-metrics-snippet (&key (limit 5))
  "Human-readable snippet for category-level A-candidate funnel metrics."
  (let ((rows '()))
    (maphash (lambda (_key snapshot)
               (declare (ignore _key))
               (push snapshot rows))
             *a-candidate-category-metrics*)
    (if (null rows)
        "A Candidate Funnel (latest): none"
        (with-output-to-string (s)
          (format s "A Candidate Funnel (latest):~%")
          (dolist (m (subseq (sort rows #'> :key (lambda (x) (getf x :a-ready-count 0)))
                             0
                             (min limit (length rows))))
            (let* ((tf (or (getf m :timeframe) 1))
                   (tf-str (if (fboundp 'get-tf-string) (get-tf-string tf) (format nil "M~a" tf))))
              (format s "- ~a/~a/~a b=~d base=~d ready=~d queued=~d~@[ bootstrap~]~%"
                      (or (getf m :symbol) "UNKNOWN")
                      (or (getf m :direction) :BOTH)
                      tf-str
                      (or (getf m :b-count) 0)
                      (or (getf m :a-base-count) 0)
                      (or (getf m :a-ready-count) 0)
                      (or (getf m :queued-count) 0)
                      (and (getf m :bootstrap-p) t))))))))

(defun get-strategies-by-rank (rank &optional timeframe direction symbol)
  "Get all strategies with a specific rank, optionally filtered by TF/Direction/Symbol.
   V47.2: Extended for full category filtering."
  (let ((candidates (remove-if-not 
                      (lambda (s) (eq (strategy-rank s) rank))
                      *strategy-knowledge-base*)))
    ;; Apply TF filter
    (when timeframe
      (let* ((tf-key (if (fboundp 'get-tf-bucket-minutes)
                         (get-tf-bucket-minutes timeframe)
                         timeframe)))
        (setf candidates
              (remove-if-not
               (lambda (s)
                 (let* ((raw (or (strategy-timeframe s) 1))
                        (k (if (fboundp 'get-tf-bucket-minutes)
                               (get-tf-bucket-minutes raw)
                               raw)))
                   (eql k tf-key)))
               candidates))))
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
  (flet ((rank-token (rank)
           (let* ((raw (cond
                         ((null rank) "")
                         ((symbolp rank) (symbol-name rank))
                         (t (format nil "~a" rank))))
                  (up (string-upcase (string-trim '(#\Space #\Tab #\Newline #\Return) raw))))
             (if (and (> (length up) 0)
                      (char= (char up 0) #\:))
                 (subseq up 1)
                 up))))
    (let* ((old-rank (strategy-rank strategy))
           (old-token (rank-token old-rank))
           (new-token (rank-token new-rank))
           (db-old-token
             (rank-token
              (ignore-errors
                (and (fboundp 'execute-single)
                     (execute-single "SELECT rank FROM strategies WHERE name = ?"
                                     (strategy-name strategy)))))))
      ;; LEGEND保護: レジェンドは墓場送りしない（子世代は別扱い）
      (when (and (string= old-token "LEGEND")
                 (string= new-token "GRAVEYARD"))
        (format t "[RANK] ⚠️ Legend protection: ~a remains :LEGEND (skip graveyard).~%" (strategy-name strategy))
        (return-from %ensure-rank-no-lock old-rank))
      (when (and (string= new-token "GRAVEYARD")
                 (oos-request-pending-p (strategy-name strategy)))
        (format t "[RANK] ⏳ OOS pending: skip graveyard for ~a~%" (strategy-name strategy))
        (return-from %ensure-rank-no-lock old-rank))
      (when (and (string= new-token "S")
                 (not (string= old-token "S"))
                 (not (check-rank-criteria strategy :S)))
        (let* ((diag (s-rank-block-diagnostics strategy :include-common-stage2 t))
               (failed-gates (getf diag :failed-gates))
               (common-msg (getf diag :common-stage2-message))
               (summary (%format-s-rank-failed-gates failed-gates)))
          (format t "[RANK] 🚫 Blocked S promotion for ~a (~a) [pf>=~,2f wr>=~,1f%% n=~d]~@[: ~a~].~%"
                  (strategy-name strategy)
                  summary
                  (or (getf diag :pf-min) 0.0)
                  (* 100 (or (getf diag :wr-min) 0.0))
                  (or (getf diag :trade-evidence) 0)
                  common-msg)
          (when (fboundp 'swimmy.core::emit-telemetry-event)
            (swimmy.core::emit-telemetry-event "rank.promotion.blocked"
              :service "school"
              :severity "warning"
              :correlation-id (strategy-name strategy)
              :data (list :strategy (strategy-name strategy)
                          :old-rank old-rank
                          :new-rank new-rank
                          :promotion-reason reason
                          :block (or (first failed-gates) :unknown)
                          :failed-gates failed-gates
                          :trade-evidence (getf diag :trade-evidence)
                          :s-stage-min-trades (getf diag :stage-min-trades)
                          :pf-min (getf diag :pf-min)
                          :wr-min (getf diag :wr-min)
                          :sharpe (getf diag :sharpe)
                          :pf (getf diag :pf)
                          :wr (getf diag :wr)
                          :maxdd (getf diag :maxdd)
                          :cpcv-pass-rate (getf diag :cpcv-pass-rate)
                          :cpcv-maxdd (getf diag :cpcv-maxdd)
                          :common-stage2-message common-msg))))
        (return-from %ensure-rank-no-lock old-rank))

      ;; No-op rank updates still need DB healing when stale archived rank exists.
      ;; Example: restore-legend path may call ensure-rank :LEGEND for an existing
      ;; in-memory legend while DB row is still :GRAVEYARD.
      (when (string= old-token new-token)
        (let ((resurrection-write-p
                (and (member new-token '("B" "A" "S" "LEGEND") :test #'string=)
                     (member db-old-token '("GRAVEYARD" "RETIRED") :test #'string=))))
          (when resurrection-write-p
            (let ((*allow-rank-regression-write* t)
                  (*allow-archived-rank-resurrection-write* t))
              (declare (special *allow-rank-regression-write*
                                *allow-archived-rank-resurrection-write*))
              (upsert-strategy strategy))
            (format t "[RANK] ♻️ No-op rank heal for ~a: db=~a -> ~a~%"
                    (strategy-name strategy) db-old-token new-rank)))
        (return-from %ensure-rank-no-lock old-rank))

      (when (not (string= old-token new-token))
        ;; Normal rank change. Global Portfolio selection handles S-RANK capacity.
        (setf (strategy-rank strategy) new-rank)
        (format t "[RANK] ~a: ~a → ~a~@[ (~a)~]~%"
                (strategy-name strategy) old-rank new-rank reason)
        
        ;; V49.9: Persist to SQL.
        ;; Allow explicit archived(:graveyard/:retired)->active resurrection writes from ensure-rank.
        ;; Also allow resurrection when in-memory rank is NIL/stale but DB rank is archived.
        (let* ((resurrection-write-p
                 (and (member new-token '("B" "A" "S" "LEGEND") :test #'string=)
                      (or (member old-token '("GRAVEYARD" "RETIRED") :test #'string=)
                          (member db-old-token '("GRAVEYARD" "RETIRED") :test #'string=))))
               (*allow-rank-regression-write* t)
               (*allow-archived-rank-resurrection-write* resurrection-write-p))
          (declare (special *allow-rank-regression-write*
                            *allow-archived-rank-resurrection-write*))
          (when (and resurrection-write-p
                     (not (member old-token '("GRAVEYARD" "RETIRED") :test #'string=))
                     (member db-old-token '("GRAVEYARD" "RETIRED") :test #'string=))
            (format t "[RANK] ♻️ DB-anchored archive resurrection for ~a: mem=~a db=~a -> ~a~%"
                    (strategy-name strategy) old-rank db-old-token new-rank))
          (upsert-strategy strategy))

        (let ((promotion-p (%promotion-p old-rank new-rank)))
          (when promotion-p
            (handler-case
                (notify-noncorrelated-promotion strategy new-rank :promotion-reason reason)
              (error (e)
                (format t "[RANK] ⚠️ Noncorrelation notify failed: ~a~%" e)))
            (when (fboundp 'maybe-sync-evolution-report-on-promotion)
              (handler-case
                  (maybe-sync-evolution-report-on-promotion :rank new-rank :reason reason)
                (error (e)
                  (format t "[RANK] ⚠️ Promotion report sync failed: ~a~%" e))))))
        
        ;; V48.2: If going to graveyard, DELETE physically from KB and pools immediately (Nassim Taleb: Survival)
        (when (string= new-token "GRAVEYARD")
          (ignore-errors (cancel-oos-request-for-strategy (strategy-name strategy) "graveyard"))
          (save-failure-pattern strategy reason)
          (setf *strategy-knowledge-base* 
                (remove strategy *strategy-knowledge-base* :test #'eq))
          ;; Also remove from category pools
          (let ((cat (categorize-strategy strategy)))
            (setf (gethash cat *category-pools*)
                  (remove strategy (gethash cat *category-pools*) :test #'eq)))
          (when (boundp '*regime-pools*)
            (let ((regime-class (if (fboundp 'strategy-regime-class)
                                    (strategy-regime-class strategy)
                                    (strategy-category strategy))))
              (setf (gethash regime-class *regime-pools*)
                    (remove strategy (gethash regime-class *regime-pools*) :test #'eq))))
          
          ;; P13: Synchronize with File System
          (handler-case
              (swimmy.persistence:move-strategy strategy :graveyard :from-rank old-rank)
            (error (e)
              (format t "[RANK] ⚠️ File move failed: ~a~%" e)))
          
          (format t "[RANK] 🪦 Physically DELETED from Knowledge Base.~%"))

        (when (string= new-token "RETIRED")
          (save-retired-pattern strategy reason)
          (setf *strategy-knowledge-base*
                (remove strategy *strategy-knowledge-base* :test #'eq))
          ;; Also remove from category pools
          (let ((cat (categorize-strategy strategy)))
            (setf (gethash cat *category-pools*)
                  (remove strategy (gethash cat *category-pools*) :test #'eq)))
          (when (boundp '*regime-pools*)
            (let ((regime-class (if (fboundp 'strategy-regime-class)
                                    (strategy-regime-class strategy)
                                    (strategy-category strategy))))
              (setf (gethash regime-class *regime-pools*)
                    (remove strategy (gethash regime-class *regime-pools*) :test #'eq))))
          ;; Persist to archive
          (handler-case
              (swimmy.persistence:move-strategy strategy :retired :from-rank old-rank)
            (error (e)
              (format t "[RANK] ⚠️ File move failed: ~a~%" e)))
          (format t "[RANK] 🧊 Retired and removed from Knowledge Base.~%")))
      new-rank)))

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

(defparameter *supported-timeframes* '(5 15 30 60 240 1440 10080 43200)
  "Category TF buckets (minutes): M5, M15, M30, H1, H4, D1, W1, MN")
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
         (count (length b-strategies))
         (a-count (length (get-strategies-by-rank :A)))
         (bootstrap-p (and *culling-bootstrap-when-no-a*
                           (< count *culling-threshold*)
                           (>= count *culling-bootstrap-min-count*)
                           (<= a-count *culling-bootstrap-max-a-count*)))
         (a-base-candidates (remove-if-not
                             (lambda (s)
                               (check-rank-criteria s :A :include-oos nil))
                             b-strategies))
         (a-ready-candidates (remove-if-not
                              (lambda (s)
                                (if (fboundp 'a-expectancy-gate-passed-p)
                                    (nth-value 0 (a-expectancy-gate-passed-p s))
                                    t))
                              a-base-candidates)))
    (record-a-candidate-category-metric timeframe direction symbol
                                        :b-count count
                                        :a-base-count (length a-base-candidates)
                                        :a-ready-count (length a-ready-candidates)
                                        :queued-count 0
                                        :bootstrap-p bootstrap-p
                                        :culling-triggered-p nil)
    
    (when (or (>= count *culling-threshold*) bootstrap-p)
      (format t "[RANK] 🗡️ CULLING B-RANK (TF=~a Dir=~a Sym=~a Count=~d~@[ BOOTSTRAP~])~%" 
              timeframe direction symbol count (and bootstrap-p t))
      (when bootstrap-p
        (format t "[RANK] 🚀 Bootstrap mode: A-rank low (A=~d <= ~d), promoting without graveyard prune.~%"
                a-count *culling-bootstrap-max-a-count*))
      (format t "[RANK] 🎯 A-base candidates: ~d/~d (expectancy-pass ~d)~%"
              (length a-base-candidates) count (length a-ready-candidates))
      
      ;; Promote only A-base candidates, then keep B baseline by deficit-aware score.
      (let* ((sorted-ready (sort (copy-list a-ready-candidates) #'>
                                 :key #'strategy-culling-priority-score))
             (to-promote (subseq sorted-ready 0 (min *a-rank-slots-per-tf* (length sorted-ready))))
             (to-discard (unless bootstrap-p
                           (let* ((sorted-b (sort (copy-list b-strategies) #'> :key #'strategy-culling-priority-score))
                                  (keep-target (min count
                                                    (max *culling-threshold*
                                                         (length to-promote))))
                                  (to-keep (copy-list to-promote)))
                             (dolist (s sorted-b)
                               (when (and (< (length to-keep) keep-target)
                                          (not (member s to-keep :test #'eq)))
                                 (push s to-keep)))
                             (remove-if (lambda (s) (member s to-keep :test #'eq))
                                        b-strategies)))))
        (record-a-candidate-category-metric timeframe direction symbol
                                            :b-count count
                                            :a-base-count (length a-base-candidates)
                                            :a-ready-count (length a-ready-candidates)
                                            :queued-count (length to-promote)
                                            :bootstrap-p bootstrap-p
                                            :culling-triggered-p t)
        
        ;; Instead of direct promotion, we now queue for OOS Validation
        (dolist (s to-promote)
          (format t "[RANK] 🔬 Queueing ~a for OOS Validation...~%" (strategy-name s))
          (handler-case
              (validate-for-a-rank-promotion s)
            (error (e) (format t "[RANK] ⚠️ OOS Failed for ~a: ~a~%" (strategy-name s) e))))
        
        ;; In regular mode, prune only surplus above the retained B baseline.
        (dolist (s to-discard)
          (send-to-graveyard s "Culling Loser"))))))

(defun collect-active-symbols ()
  "Collect all unique symbols present in the KB."
  (remove-duplicates (mapcar #'strategy-symbol *strategy-knowledge-base*) :test #'string=))

(defun collect-active-timeframes ()
  "Collect all unique timeframes present in the active KB."
  (remove nil
          (remove-duplicates
           (mapcar (lambda (s)
                     (let ((raw (or (strategy-timeframe s) 1)))
                       (if (fboundp 'get-tf-bucket-minutes)
                           (get-tf-bucket-minutes raw)
                           raw)))
                   *strategy-knowledge-base*)
           :test #'eql)))

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
         (trade-evidence (strategy-trade-evidence-count strategy))
         (a-min-trade-evidence (min-trade-evidence-for-rank :A))
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
                               :trades trade-evidence
                               :max-dd (strategy-cpcv-median-maxdd strategy))
                         (list :sharpe (strategy-sharpe strategy)
                               :profit-factor (strategy-profit-factor strategy)
                               :win-rate (strategy-win-rate strategy)
                               :trades trade-evidence
                               :max-dd (strategy-max-dd strategy))))
                    (or (strategy-sharpe strategy) 0.0))))
    (when (< trade-evidence a-min-trade-evidence)
      (demote-rank strategy :B
                   (format nil "Trade evidence floor failed (~d < ~d)"
                           trade-evidence
                           a-min-trade-evidence))
      (return-from evaluate-a-rank-strategy :B))
    (if (check-rank-criteria strategy :S)
        (multiple-value-bind (common-pass common-msg)
            (if (fboundp 'common-stage2-gates-passed-p)
                (common-stage2-gates-passed-p strategy)
                (values nil "Common Stage2 gate unavailable"))
          (if common-pass
              (progn
                (remhash name *a-rank-probation-tracker*)
                (promote-rank strategy :S "CPCV+CommonStage2 validated - LIVE TRADING PERMITTED")
                :S)
              (progn
                (format t "[RANK] ⛔ ~a blocked for S-RANK: ~a~%" name common-msg)
                :A)))
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
    (setf (strategy-breeding-count strategy) count)))

(defun enforce-rank-trade-evidence-floors ()
  "Demote existing S/A strategies that do not meet trade-evidence floors.
Returns plist summary:
  (:s-demoted N :a-demoted N :s-min N :a-min N)"
  (let* ((s-min (min-trade-evidence-for-rank :S))
         (a-min (min-trade-evidence-for-rank :A))
         (s-demoted 0)
         (a-demoted 0))
    ;; S floor:
    ;; - n >= A-min and < S-min => S -> A
    ;; - n < A-min => S -> B
    (dolist (s (copy-list (get-strategies-by-rank :S)))
      (let ((n (strategy-trade-evidence-count s)))
        (when (< n s-min)
          (let ((target (if (>= n a-min) :A :B)))
            (demote-rank s target
                         (format nil "S trade-evidence floor failed (~d < ~d)"
                                 n s-min))
            (incf s-demoted)))))
    ;; A floor:
    ;; - n < A-min => A -> B
    (dolist (s (copy-list (get-strategies-by-rank :A)))
      (let ((n (strategy-trade-evidence-count s)))
        (when (< n a-min)
          (demote-rank s :B
                       (format nil "A trade-evidence floor failed (~d < ~d)"
                               n a-min))
          (incf a-demoted))))
    (format t "[RANK] 📉 Trade-evidence floor sweep: S-demoted=~d (<~d) A-demoted=~d (<~d)~%"
            s-demoted s-min a-demoted a-min)
    (list :s-demoted s-demoted
          :a-demoted a-demoted
          :s-min s-min
          :a-min a-min)))

(defun enforce-s-rank-criteria-conformance ()
  "Demote existing S strategies that no longer satisfy S criteria.
Returns plist summary:
  (:s-demoted N)"
  (let ((s-demoted 0))
    (dolist (s (copy-list (get-strategies-by-rank :S)))
      (unless (check-rank-criteria s :S)
        (let ((target (if (check-rank-criteria s :A) :A :B)))
          (demote-rank s target
                       (format nil "S criteria conformance failed (target ~a)" target))
          (incf s-demoted))))
    (format t "[RANK] 📉 S-criteria conformance sweep: S-demoted=~d~%"
            s-demoted)
    (list :s-demoted s-demoted)))

(defun enforce-a-b-rank-criteria-conformance ()
  "Enforce A/B criteria conformance on existing ranks.
Returns plist summary:
  (:a-demoted N :b-graveyarded N)"
  (let ((a-demoted 0)
        (b-graveyarded 0))
    ;; A conformance:
    ;; - Existing A that fail A criteria => demote to B.
    (dolist (s (copy-list (get-strategies-by-rank :A)))
      (unless (check-rank-criteria s :A)
        (demote-rank s :B "A criteria conformance failed")
        (incf a-demoted)))
    ;; B conformance:
    ;; - Existing B that fail B criteria => graveyard.
    ;; Includes newly demoted A from the first sweep.
    (dolist (s (copy-list (get-strategies-by-rank :B)))
      (unless (check-rank-criteria s :B)
        (send-to-graveyard s "B criteria conformance failed")
        (incf b-graveyarded)))
    (format t "[RANK] 📉 A/B conformance sweep: A-demoted=~d B->Graveyard=~d~%"
            a-demoted b-graveyarded)
    (list :a-demoted a-demoted
          :b-graveyarded b-graveyarded)))

(defun run-b-rank-culling (&optional single-tf)
  "Run culling for all TF × Direction × Symbol categories.
   V49.3: Dynamic symbol detection to prevent zombie-accumulation in non-major pairs."
  (let* ((timeframes (if single-tf
                         (list single-tf)
                         (or (collect-active-timeframes) *supported-timeframes*)))
         (symbols (collect-active-symbols))) ;; Dynamic Symbols
    (dolist (tf timeframes)
      (dolist (dir *supported-directions*)
        (dolist (sym symbols)
          (run-b-rank-culling-for-category tf dir sym))))))

(defun run-rank-evaluation ()
  "Main rank evaluation cycle.
   1. Enforce A/S trade-evidence floors on existing ranks.
   2. Enforce existing S-rank criteria conformance.
   3. Enforce existing A/B criteria conformance.
   4. Cull B-RANK if threshold reached (Dynamic Categories)
   5. Validate A-RANK via CPCV (→ S or back)
   V49.3: Added missing A-Rank evaluation loop."
  (let ((*trade-evidence-count-cache*
          (if (fboundp 'fetch-backtest-trade-count-map)
              (handler-case
                  (fetch-backtest-trade-count-map)
                (error (e)
                  (when (fboundp 'swimmy.core::emit-telemetry-event)
                    (swimmy.core::emit-telemetry-event
                     "rank.trade_evidence_cache_error"
                     :service "school"
                     :severity :warn
                     :data (list :error (format nil "~a" e))))
                  nil))
              nil)))
    (format t "[RANK] 🏛️ Starting Rank Evaluation Cycle (V49.3 Fixed)~%")
    (reset-oos-failure-stats)
    ;; Normalize legacy OOS defaults so validation is re-triggered
    (normalize-oos-defaults)

    ;; 0. Enforce trade-evidence floors on existing A/S ranks
    (when *enforce-rank-trade-evidence-floors*
      (enforce-rank-trade-evidence-floors))

    ;; 0.5 Enforce S-rank criteria conformance on existing S ranks
    (when *enforce-s-rank-criteria-conformance*
      (enforce-s-rank-criteria-conformance))

    ;; 0.6 Enforce A/B criteria conformance on existing A/B ranks
    (when *enforce-a-b-rank-criteria-conformance*
      (enforce-a-b-rank-criteria-conformance))
    
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
      ;; NOTE: This draft can be CPU/DB heavy (N^2 correlation checks). During startup mode we
      ;; defer it so Brain can finish initialization and bind ZMQ ports quickly.
      (if (and (boundp '*startup-mode*) *startup-mode*)
          (format t "[PORTFOLIO] ⏳ Startup mode: deferring global draft.~%")
          (construct-global-portfolio)))))


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
  (labels ((parent-quality-floor-passed-p (s)
             (if (not *breeder-parent-quality-floor-enabled*)
                 t
                 (let ((criteria (get-rank-criteria :B)))
                   (if (null criteria)
                       t
                       (let ((sharpe (float (or (strategy-sharpe s) 0.0) 1.0))
                             (pf (float (or (strategy-profit-factor s) 0.0) 1.0))
                             (wr (float (or (strategy-win-rate s) 0.0) 1.0))
                             (maxdd (float (or (strategy-max-dd s) 1.0) 1.0)))
                         (and (>= sharpe (float (getf criteria :sharpe-min 0.0) 1.0))
                              (>= pf (float (getf criteria :pf-min 0.0) 1.0))
                              (>= wr (float (getf criteria :wr-min 0.0) 1.0))
                              (< maxdd (float (getf criteria :maxdd-max 1.0) 1.0))))))))
           (parent-oos-floor-passed-p (s rank)
             (if (not *breeder-parent-oos-floor-enabled*)
                 t
                 (if (member rank *breeder-parent-oos-required-ranks* :test #'eq)
                     (>= (float (or (strategy-oos-sharpe s) -1.0) 1.0)
                         (float *breeder-parent-min-oos-sharpe* 1.0))
                     t)))
           (parent-rank-floor-passed-p (s rank)
             (if (not *breeder-parent-rank-conformance-enabled*)
                 t
                 (case rank
                   (:A (check-rank-criteria s :A :include-oos t :include-cpcv nil))
                   (:S (check-rank-criteria s :S :include-oos t :include-cpcv t))
                   (otherwise t)))))
  (let ((rank (and strategy (strategy-rank strategy))))
    (and strategy
         (not (and (slot-exists-p strategy 'revalidation-pending)
                   (strategy-revalidation-pending strategy)))
         (not (eq (strategy-status strategy) :killed))
         (not (member rank '(:legend-archive :retired :graveyard :archived :archive :incubator) :test #'eq))
         ;; Sanity checks to avoid pathological SL/TP values
         (numberp (strategy-sl strategy))
         (< (abs (strategy-sl strategy)) 1000.0)
         (numberp (strategy-tp strategy))
         (< (abs (strategy-tp strategy)) 1000.0)
         (or (eq rank :legend)
             (and (>= (or (strategy-trades strategy) 0) *breeder-min-parent-trades*)
                  (< (or (strategy-breeding-count strategy) 0) *max-breeding-uses*)
                  (parent-quality-floor-passed-p strategy)
                  (parent-oos-floor-passed-p strategy rank)
                  (parent-rank-floor-passed-p strategy rank)))))))

(defun run-legend-breeding ()
  "Breed Legend strategies with random B-rank strategies.
   V47.0: Owner's Vision - Legends participate in periodic random breeding."
  (format t "[LEGEND] 👑 Starting Legend Breeding Cycle...~%")
  (let* ((legends (remove-if (lambda (s)
                               (or (eq (strategy-rank s) :legend-archive)
                                   (and (slot-exists-p s 'revalidation-pending)
                                        (strategy-revalidation-pending s))))
                             (get-strategies-by-rank :legend)))
         (b-ranks (remove-if-not #'can-breed-p
                                 (remove-if (lambda (s)
                                              (and (slot-exists-p s 'revalidation-pending)
                                                   (strategy-revalidation-pending s)))
                                            (get-strategies-by-rank :B))))
        (bred-count 0))
    
    (when (and legends b-ranks)
      ;; Pick random legend and random B-rank
      (let* ((legend (nth (random (length legends)) legends))
             (b-rank (nth (random (length b-ranks)) b-ranks)))
        
        (when (and legend b-rank)
          (format t "[LEGEND] 🏆 Breeding ~a (Legend) + ~a (B-Rank)~%"
                  (strategy-name legend) (strategy-name b-rank))
          
          ;; Create child using breed-strategies from school-breeder
          (when (and (fboundp 'breed-strategies)
                     (fboundp 'add-to-kb))
            (let ((child (breed-strategies legend b-rank)))
              ;; Mark child as having legendary heritage
              (setf (strategy-generation child) 
                    (1+ (max (strategy-generation legend) 
                             (strategy-generation b-rank))))
              ;; Route through unified breeder intake (Phase1 mandatory via add-to-kb).
              (multiple-value-bind (accepted status)
                  (add-to-kb child :breeder :require-bt t :notify nil)
                (when accepted
                  ;; Consume parent quota only when child admission is accepted.
                  (increment-breeding-count b-rank)
                  (incf bred-count)
                  (if (eq (or status :added) :queued-phase1)
                      (format t "[LEGEND] ⏳ Royal Child queued for Phase1: ~a~%" (strategy-name child))
                      (progn
                        (when (fboundp 'save-recruit-to-lisp)
                          (save-recruit-to-lisp child))
                        (format t "[LEGEND] 👶 Royal Child Born: ~a~%" (strategy-name child)))))))))))
    
    (format t "[LEGEND] 👑 Legend Breeding Complete: ~d children~%" bred-count)
    bred-count))

;;; Note: RL, Graveyard, Q-learning, and File Rotation functions moved to school-learning.lisp (V47.3)

(defun %normalize-rank-token-for-backtest (rank)
  "Normalize rank token to uppercase without leading colon."
  (let* ((raw (cond
                ((null rank) "")
                ((stringp rank) rank)
                ((symbolp rank) (symbol-name rank))
                (t (format nil "~a" rank))))
         (trimmed (string-upcase (string-trim '(#\Space #\Tab #\Newline) raw))))
    (if (and (> (length trimmed) 0)
             (char= (char trimmed 0) #\:))
        (subseq trimmed 1)
        trimmed)))

(defun %phase1-evaluation-needed-p (rank)
  "Return T when RANK should be treated as unranked/incubator for phase-1 evaluation."
  (let ((token (%normalize-rank-token-for-backtest rank)))
    (or (null rank)
        (string= token "")
        (string= token "NIL")
        (string= token "INCUBATOR"))))

(defun apply-backtest-result (name metrics)
  "Apply backtest metrics to a strategy and trigger rank evaluation if necessary."
  (let ((strat (find-strategy name)))
	(if strat
	    (progn
	      ;; NOTE: (getf ... default) does not treat explicit NIL as missing. Coerce NIL to 0.0 here
	      ;; to avoid type errors in FLOAT/comparisons when upstream metrics are present-but-NIL.
	      (setf (strategy-sharpe strat) (float (or (getf metrics :sharpe) 0.0) 0.0)
		    (strategy-profit-factor strat) (float (or (getf metrics :profit-factor) 0.0) 0.0)
		    (strategy-win-rate strat) (float (or (getf metrics :win-rate) 0.0) 0.0)
		    (strategy-trades strat) (or (getf metrics :trades) 0)
		    (strategy-max-dd strat) (float (or (getf metrics :max-dd) 0.0) 0.0)
		    (strategy-cpcv-median-sharpe strat) (float (or (getf metrics :cpcv-median) 0.0) 0.0)
		    (strategy-cpcv-median-pf strat) (float (or (getf metrics :cpcv-median-pf) 0.0) 0.0)
		    (strategy-cpcv-median-wr strat) (float (or (getf metrics :cpcv-median-wr) 0.0) 0.0)
		    (strategy-cpcv-median-maxdd strat) (float (or (getf metrics :cpcv-median-maxdd) 0.0) 0.0)
		    (strategy-cpcv-pass-rate strat) (float (or (getf metrics :cpcv-pass-rate) 0.0) 0.0))
          (when (and (slot-exists-p strat 'revalidation-pending)
                     (strategy-revalidation-pending strat))
            (setf (strategy-revalidation-pending strat) nil))
          ;; DEBUG V50.5.1
          (let ((s (strategy-sharpe strat)))
            (when (zerop s)
               (format t "[DB] ⚠️ Appplying Metrics for ~a: Sharpe is zero! Metrics: ~a~%" name metrics)))
          (upsert-strategy strat)
          ;; Trigger Phase 1 Evaluation for newly screened strategies.
          ;; :INCUBATOR needs one backtest result to transition to :B or :GRAVEYARD.
          (when (%phase1-evaluation-needed-p (strategy-rank strat))
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
			  (setf (strategy-sharpe obj) (float (or (getf metrics :sharpe) 0.0) 0.0)
				(strategy-profit-factor obj) (float (or (getf metrics :profit-factor) 0.0) 0.0)
				(strategy-win-rate obj) (float (or (getf metrics :win-rate) 0.0) 0.0)
				(strategy-trades obj) (or (getf metrics :trades) 0)
				(strategy-max-dd obj) (float (or (getf metrics :max-dd) 0.0) 0.0)
				(strategy-oos-sharpe obj) (float (or (getf metrics :oos-sharpe) 0.0) 0.0)
				(strategy-cpcv-median-sharpe obj) (float (or (getf metrics :cpcv-median) 0.0) 0.0)
				(strategy-cpcv-median-pf obj) (float (or (getf metrics :cpcv-median-pf) 0.0) 0.0)
				(strategy-cpcv-median-wr obj) (float (or (getf metrics :cpcv-median-wr) 0.0) 0.0)
				(strategy-cpcv-median-maxdd obj) (float (or (getf metrics :cpcv-median-maxdd) 0.0) 0.0)
				(strategy-cpcv-pass-rate obj) (float (or (getf metrics :cpcv-pass-rate) 0.0) 0.0))
                      (when (fboundp '(setf strategy-revalidation-pending))
                        (setf (strategy-revalidation-pending obj) nil))
                      (upsert-strategy obj)
                      ;; Keep phase-1 lifecycle active even when strategy is missing in-memory.
                      (when (%phase1-evaluation-needed-p (strategy-rank obj))
                        (evaluate-new-strategy obj))
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
		       (float (or (getf metrics :sharpe) 0.0) 0.0)
		       (float (or (getf metrics :profit-factor) 0.0) 0.0)
		       (float (or (getf metrics :win-rate) 0.0) 0.0)
		       (or (getf metrics :trades) 0)
		       (float (or (getf metrics :max-dd) 0.0) 0.0)
		       (get-universal-time)
		       name)))
		  nil))))
