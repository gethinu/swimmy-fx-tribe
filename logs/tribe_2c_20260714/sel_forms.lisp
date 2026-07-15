(defpackage :swimmy.school (:use :cl))
(in-package :swimmy.school)
(defvar *enable-primitive-diversity* nil)
;; stub of the real school-rank-system accessor (identity for the test)
(defun evidence-adjusted-sharpe (sharpe trades) (declare (ignore trades)) (float (or sharpe 0.0) 1.0))

(defstruct strategy name indicators entry exit (sl 0.0) (tp 0.0) (volume 0.01) 
            (sharpe 0.0) (profit-factor 0.0) (win-rate 0.0) (trades 0) (max-dd 0.0)
            (category :trend) (indicator-type "sma") (pnl-history nil) 
            (timeframe 1) (generation 0)
            (filter-enabled nil) (filter-tf "") (filter-period 0) (filter-logic "")
            (adapt-vector nil) (confidence-estimator "edge_ratio_v1")
            ;; Lifecycle Management (Phase 6)
            (consecutive-losses 0) (status :active) (status-reason "") (cooldown-until 0) (last-update 0)
            ;; The Proving Grounds Tiers: :graveyard, :incubator, :training, :battlefield
            (tier :incubator)
            ;; V47.0: B/A/S Rank System (:B, :A, :S, :legend, :graveyard)
            (rank nil)
            ;; V50.6: Legend再検証キューに入っている間の隔離フラグ
            (revalidation-pending nil)
            ;; V47.0: Breeding usage counter (max 3 before discard, Legend exempt)
            (breeding-count 0)
            ;; V49.2: Metadata tracking (Expert Panel)
            (regime-intent :unknown)
            ;; V17d: Multi-Currency Identity
            (symbol "USDJPY")
            ;; V47.2: Trade Direction (:BUY, :SELL, :BOTH)
            (direction :BOTH)
            ;; P10: Inactivity Pruning
            (last-signal-time 0)
            ;; P13: New Recruits Tracking
            (creation-time (get-universal-time))
            ;; V49.8: Stable Logic Hash for Graveyard Matching
            (hash nil)
            ;; V50.3: Validation Gates Metrics
            ;; P1 (Thread A): unvalidated OOS is NIL, not 0.0. 0.0 was indistinguishable
            ;; from "OOS ran and scored 0", which (a) let the honest gate/rank logic treat
            ;; a never-validated strategy as measured, and (b) made run-oos-validation treat
            ;; the default as a cached result and skip requesting a real OOS backtest.
            (oos-sharpe nil)
            (cpcv-median-sharpe 0.0)
            (cpcv-median-pf 0.0)
            (cpcv-median-wr 0.0)
            (cpcv-median-maxdd 0.0)
            (cpcv-pass-rate 0.0)
            ;; Phase 21: Breeding & Competition DNA (Survival of the Fittest)
            (age 0) (immortal nil) (parents nil)
            ;; V2c (2026-07-14): primitive-diversity genes. Defaults reproduce prior
            ;; behaviour exactly (band-mult 2.0 = the old hard-coded Bollinger dev; ATR
            ;; barriers 0.0 = disabled => absolute sl/tp). Only emitted / mutated when
            ;; *enable-primitive-diversity* is on, so the legacy engine is unaffected.
            (band-mult 2.0) (atr-period 14) (atr-barrier-sl 0.0) (atr-barrier-tp 0.0))

(defun fitness-sharing-denominator (strategy population)
  "Count population members sharing STRATEGY's (symbol, category) niche (>=1)."
  (if (null population)
      1
      (let ((sym (strategy-symbol strategy))
            (cat (strategy-category strategy))
            (n 0))
        (dolist (s population)
          (when (and (equal (strategy-symbol s) sym)
                     (eq (strategy-category s) cat))
            (incf n)))
        (max 1 n))))

(defun selection-fitness (strategy &optional population)
  "Selection score. Flag OFF => raw strategy-sharpe (legacy). Flag ON => evidence-adjusted
   sharpe / niche-crowding (fitness sharing forces diversity by division)."
  (let ((raw (or (strategy-sharpe strategy) 0.0)))
    (if *enable-primitive-diversity*
        (let* ((trades (or (strategy-trades strategy) 0))
               (adj (if (fboundp 'evidence-adjusted-sharpe)
                        (evidence-adjusted-sharpe raw trades)
                        raw))
               (share (fitness-sharing-denominator strategy population)))
          (/ adj (max 1.0 (float share 1.0))))
        raw)))

(defparameter *2c-cpcv-pregate-min* 0.60
  "Minimum CPCV pass-rate to enter the breeding pool when *enable-primitive-diversity*.")

(defun breeding-cpcv-eligible-p (strategy)
  "Flag OFF => always t (legacy, no pre-gate). Flag ON => require CPCV pass-rate >= the
   pre-gate minimum so only CPCV-robust individuals breed."
  (or (not *enable-primitive-diversity*)
      (>= (or (strategy-cpcv-pass-rate strategy) 0.0) *2c-cpcv-pregate-min*)))