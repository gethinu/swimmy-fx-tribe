;;; school-ranking.lisp
;;; Phase 20: Ranking Logic (Meritocracy)
;;; Implements the Ladder: Incubator -> Rank B -> Rank A -> Rank S

(in-package :swimmy.school)

;;; =========================================================
;;; RANK B: THE POOL (Capacity 100)
;;; =========================================================

(defparameter *rank-b-capacity* 100 "Max strategies in Rank B pool per timeframe.")

(defun promote-to-rank-b (strategy)
  "Promote strategy to Rank B (The Pool).
   Triggered after passing Phase 1 Screening."
  (format t "[RANK] 🎖️ Promoting ~a to Rank B (Screening Passed)~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    ;; 1. Update Rank
    (setf (strategy-rank strategy) :B)
    
    ;; 2. Update Persistance
    (upsert-strategy strategy)
    
    ;; 3. Check Pool Capacity (Culling)
    ;; Validating pool size for this timeframe/pair?
    ;; For now, global B-rank checks or per-category.
    (cull-rank-b-pool)))

(defun cull-rank-b-pool ()
  "Maintain Rank B pool size. Remove weakest if > Capacity."
  (let ((b-rankers (remove-if-not (lambda (s) (eq (strategy-rank s) :B)) *strategy-knowledge-base*)))
    (when (> (length b-rankers) *rank-b-capacity*)
      (format t "[RANK] ✂️ Culling Rank B Pool (~d > ~d)...~%" (length b-rankers) *rank-b-capacity*)
      
      ;; Sort by composite score (Ascending - Weakest first)
      (let ((sorted (sort (copy-list b-rankers) #'<
                          :key (lambda (s)
                                 (score-from-metrics
                                  (list :sharpe (strategy-sharpe s)
                                        :profit-factor (strategy-profit-factor s)
                                        :win-rate (strategy-win-rate s)
                                        :max-dd (strategy-max-dd s))))))
            (culled-count 0))
        
        ;; Remove bottom 30% (buffer)
        (let ((to-remove (subseq sorted 0 (floor (* (length sorted) 0.3)))))
          (dolist (s to-remove)
            (demote-to-graveyard s "Rank B Culling (Weakest)")
            (incf culled-count))
          (format t "[RANK] ✂️ Culled ~d strategies.~%" culled-count))))))

;;; =========================================================
;;; RANK A: THE CANDIDATES (OOS Validated)
;;; =========================================================

(defun promote-to-rank-a (strategy)
  "Promote to Rank A (Candidate).
   Triggered after passing Phase 2 Validation (OOS)."
  (format t "[RANK] 🌟 Promoting ~a to Rank A (Validation Passed)~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :A)
    (upsert-strategy strategy)
    
    ;; Trigger CPCV for S-Rank?
    ;; Or wait for manual trigger/schedule?
    ;; V50.2 Plan says "Gate to Rank S (CPCV)".
    ;; We can queue it.
    (format t "[RANK] ⏳ ~a queued for CPCV (S-Rank Gate).~%" (strategy-name strategy))))

;;; =========================================================
;;; RANK S: THE GLADIATORS (Live Trading)
;;; =========================================================

(defun promote-to-rank-s (strategy)
  "Promote to Rank S (Gladiator).
   Triggered after passing CPCV."
  (format t "[RANK] 👑 Promoting ~a to Rank S (The Gladiator)!~%" (strategy-name strategy))
  
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :S)
    (upsert-strategy strategy)
    
    ;; Notification
    (swimmy.core:notify-discord-alert 
     (format nil "👑 **NEW GLADIATOR**: `~a` Promoted to Rank S!" (strategy-name strategy))
     :color 16766720))) ;; Gold

;;; =========================================================
;;; DSR (DEFLATED SHARPE RATIO) LOGIC (Simons/Prado)
;;; =========================================================

(defun count-graveyard-trials ()
  "Count total strategies in the Graveyard (representing failed trials)."
  (count-if (lambda (s) (eq (strategy-rank s) :GRAVEYARD)) *strategy-knowledge-base*))

;; V-methodology (2026-08-11): the honest trial total N for the Deflated Sharpe.
;; The graveyard alone under-counts trials (every A/B/S/legend strategy is also a
;; trial that was selected on). Real DSR needs the TOTAL number of configurations
;; ever evaluated, so count the whole knowledge base.
(defun count-total-trials ()
  "Total strategies ever evaluated (all ranks). This is N in the Deflated Sharpe."
  (length *strategy-knowledge-base*))

(defun calculate-dsr-threshold ()
  "LEGACY name-only 'DSR' Sharpe threshold (kept for flag-OFF byte-parity).
   Phase 23 Directive: S-Rank Barrier rises with trial count.
   Formula: Base=3.0 + 0.5*log10(GraveyardCount / 1000).
   NOTE: this is a Sharpe FLOOR, not a Deflated Sharpe Ratio. The real DSR lives in
   DEFLATED-SHARPE-RATIO below and is only used when *ENABLE-REAL-DSR* is on."
  (let* ((trials (max 1 (count-graveyard-trials)))
         (log-factor (log (/ trials 1000.0) 10)))
    (if (> log-factor 0)
        (float (+ 3.0 (* 0.5 log-factor))) ;; 0.5 scaling factor to be slightly lenient? No, Simons said strict.
        3.0)))

;;; ---------------------------------------------------------------------------
;;; REAL Deflated Sharpe Ratio (Bailey & Lopez de Prado, 2014) — flag-gated
;;; ---------------------------------------------------------------------------
;;; Deflated Sharpe = P[ true SR > 0 | observed SR, skew, kurtosis, sample length,
;;; and the number of trials N over which the best config was selected ]. It corrects
;;; the naive Sharpe for (a) non-normal returns (skew/kurtosis), (b) short samples,
;;; and (c) selection under many trials (multiple-testing / backtest overfitting).
;;;
;;; DEFAULT OFF: with *ENABLE-REAL-DSR* nil, MEETS-S-RANK-DSR-P is byte-identical to
;;; the legacy Sharpe-floor gate above. Turning it on is reversible (one defparameter).

(defparameter *enable-real-dsr* nil
  "OFF by default. When T, MEETS-S-RANK-DSR-P uses the real Deflated Sharpe Ratio
   (Bailey-Lopez de Prado) instead of the legacy Sharpe-floor. Reversible.")

(defparameter *dsr-prob-threshold* 0.95
  "Minimum Deflated Sharpe probability required for the S-rank gate when real DSR is on.")

(defparameter *dsr-min-samples* 20
  "Minimum length of pnl-history required to estimate skew/kurtosis for real DSR.
   Below this the real gate ABSTAINS and falls back to the legacy Sharpe-floor.")

(defparameter *euler-mascheroni* 0.5772156649015329d0)

(defun %mean (xs)
  (/ (reduce #'+ xs :initial-value 0.0d0) (length xs)))

(defun %central-moment (xs mean k)
  "k-th central moment (population / biased)."
  (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) k)) xs) :initial-value 0.0d0)
     (length xs)))

(defun %poly (coeffs x)
  "Horner evaluation of a polynomial. COEFFS highest-degree first."
  (reduce (lambda (acc c) (+ (* acc x) c)) coeffs :initial-value 0.0d0))

(defun %norm-cdf (x)
  "Standard normal CDF via Abramowitz & Stegun 26.2.17 (|err| < 7.5e-8)."
  (let* ((xd (coerce x 'double-float))
         (neg (< xd 0.0d0))
         (z (abs xd))
         (tt (/ 1.0d0 (+ 1.0d0 (* 0.2316419d0 z))))
         (pdf (* (/ 1.0d0 (sqrt (* 2.0d0 pi))) (exp (* -0.5d0 z z))))
         ;; k1*tt + k2*tt^2 + ... + k5*tt^5  ==  tt * poly(tt)
         (poly (* tt (%poly '(1.330274429d0 -1.821255978d0 1.781477937d0
                              -0.356563782d0 0.319381530d0)
                            tt)))
         (upper (* pdf poly)))
    ;; upper ~ 1-CDF(z) for z>=0
    (if neg upper (- 1.0d0 upper))))

(defun %norm-ppf (p)
  "Inverse standard normal CDF (Acklam's rational approximation, |err| < 1.2e-9)."
  (let ((p (coerce p 'double-float)))
    (when (<= p 0.0d0) (return-from %norm-ppf sb-ext:double-float-negative-infinity))
    (when (>= p 1.0d0) (return-from %norm-ppf sb-ext:double-float-positive-infinity))
    (let* ((ca '(-7.784894002430293d-03 -3.223964580411365d-01 -2.400758277161838d+00
                 -2.549732539343734d+00 4.374664141464968d+00 2.938163982698783d+00))
           ;; tail denominators carry a trailing 1.0 so %poly emits (...*q + 1)
           (cd '(7.784695709041462d-03 3.224671290700398d-01 2.445134137142996d+00
                 3.754408661907416d+00 1.0d0))
           (aa '(-3.969683028665376d+01 2.209460984245205d+02 -2.759285104469687d+02
                 1.383577518672690d+02 -3.066479806614716d+01 2.506628277459239d+00))
           (bb '(-5.447609879822406d+01 1.615858368580409d+02 -1.556989798598866d+02
                 6.680131188771972d+01 -1.328068155288572d+01 1.0d0))
           (plow 0.02425d0)
           (phigh (- 1.0d0 0.02425d0)))
      (cond
        ((< p plow)
         (let ((q (sqrt (* -2.0d0 (log p)))))
           (/ (%poly ca q) (%poly cd q))))
        ((<= p phigh)
         (let* ((q (- p 0.5d0)) (r (* q q)))
           (/ (* (%poly aa r) q) (%poly bb r))))
        (t
         (let ((q (sqrt (* -2.0d0 (log (- 1.0d0 p))))))
           (- (/ (%poly ca q) (%poly cd q)))))))))

(defun strategy-per-obs-sharpe (strategy)
  "Per-observation (per-trade) Sharpe from pnl-history, or NIL if not estimable.
   This is the frequency-consistent SR the Deflated Sharpe needs (NOT the stored,
   annualized strategy-sharpe)."
  (let ((h (strategy-pnl-history strategy)))
    (when (and h (>= (length h) 2))
      (let* ((xs (mapcar (lambda (x) (coerce x 'double-float)) h))
             (m (%mean xs))
             (var (%central-moment xs m 2)))
        (when (> var 0.0d0)
          (/ m (sqrt var)))))))

(defun trial-sharpe-variance ()
  "Variance of per-observation Sharpe across all trials with usable pnl-history.
   NIL if fewer than 2 such trials (real DSR then abstains)."
  (let ((srs (remove nil (mapcar #'strategy-per-obs-sharpe *strategy-knowledge-base*))))
    (when (>= (length srs) 2)
      (let ((m (%mean srs)))
        (%central-moment srs m 2)))))

(defun deflated-sharpe-ratio (strategy)
  "Real Deflated Sharpe Ratio probability in [0,1] for STRATEGY, or NIL if inputs
   are insufficient (short history, zero variance, <2 comparable trials).
   Formula (Bailey & Lopez de Prado 2014):
     SR0 = sqrt(Var[SR_trials]) * ((1-g)*Z^-1(1-1/N) + g*Z^-1(1-1/(N e)))
     DSR = Z( (SR_hat - SR0) * sqrt(T-1)
              / sqrt(1 - skew*SR_hat + ((kurt-1)/4)*SR_hat^2) )
   where g = Euler-Mascheroni, N = trial count, T = sample length, Z = normal CDF."
  (let* ((h (strategy-pnl-history strategy))
         (n-trials (max 2 (count-total-trials))))
    (when (and h (>= (length h) *dsr-min-samples*))
      (let* ((xs (mapcar (lambda (x) (coerce x 'double-float)) h))
             (tt (length xs))
             (m (%mean xs))
             (var (%central-moment xs m 2)))
        (when (> var 0.0d0)
          (let* ((sd (sqrt var))
                 (sr-hat (/ m sd))
                 (skew (/ (%central-moment xs m 3) (expt sd 3)))
                 (kurt (/ (%central-moment xs m 4) (expt sd 4))) ; Pearson (normal=3)
                 (var-sr (trial-sharpe-variance)))
            (when (and var-sr (> var-sr 0.0d0))
              (let* ((sd-sr (sqrt var-sr))
                     (e (exp 1.0d0))
                     (sr0 (* sd-sr
                             (+ (* (- 1.0d0 *euler-mascheroni*)
                                   (%norm-ppf (- 1.0d0 (/ 1.0d0 n-trials))))
                                (* *euler-mascheroni*
                                   (%norm-ppf (- 1.0d0 (/ 1.0d0 (* n-trials e))))))))
                     (denom (sqrt (max 1.0d-12
                                       (+ (- 1.0d0 (* skew sr-hat))
                                          (* (/ (- kurt 1.0d0) 4.0d0) (* sr-hat sr-hat))))))
                     (z (/ (* (- sr-hat sr0) (sqrt (max 1.0d0 (- tt 1.0d0)))) denom)))
                (%norm-cdf z)))))))))

(defun meets-s-rank-dsr-p (strategy)
  "S-rank statistical gate.
   Flag OFF (default): legacy Sharpe-floor comparison — byte-identical to prior behaviour.
   Flag ON (*enable-real-dsr*): require real Deflated Sharpe >= *dsr-prob-threshold*;
   if the real DSR is not estimable (insufficient data) fall back to the legacy floor."
  (if *enable-real-dsr*
      (let ((dsr (deflated-sharpe-ratio strategy)))
        (if dsr
            (if (>= dsr *dsr-prob-threshold*)
                t
                (progn
                  (format t "[DSR] 🚫 ~a DeflatedSharpe (~,4f) < threshold (~,2f), N=~d~%"
                          (strategy-name strategy) dsr *dsr-prob-threshold* (count-total-trials))
                  nil))
            ;; Not estimable -> conservative fallback to the legacy floor.
            (meets-s-rank-dsr-legacy-p strategy)))
      (meets-s-rank-dsr-legacy-p strategy)))

(defun meets-s-rank-dsr-legacy-p (strategy)
  "The original Sharpe-floor gate, preserved verbatim for flag-OFF byte-parity."
  (let ((threshold (calculate-dsr-threshold))
        (sharpe (or (strategy-sharpe strategy) 0.0)))
    (if (>= sharpe threshold)
        t
        (progn
          (format t "[DSR] 🚫 Strategy ~a Sharpe (~,2f) < DSR Threshold (~,2f)~%"
                  (strategy-name strategy) sharpe threshold)
          nil))))

;;; =========================================================
;;; UTILS
;;; =========================================================

(defun demote-to-graveyard (strategy reason)
  "Move to Graveyard."
  (format t "[RANK] 🪦 Demoting ~a to Graveyard: ~a~%" (strategy-name strategy) reason)
  (bt:with-lock-held (*kb-lock*)
    (setf (strategy-rank strategy) :GRAVEYARD)
    (upsert-strategy strategy)
    ;; Ideally remove from *strategy-knowledge-base* to save memory?
    ;; Or keep for record? Current logic keeps but marks Rank.
    ;; Pruning logic handles physical removal later.
    ))
