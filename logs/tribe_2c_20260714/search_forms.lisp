(defpackage :swimmy.school (:use :cl))
(in-package :swimmy.school)
(defvar *enable-primitive-diversity* nil)

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

(defun extract-sma-params (indicators)
  "Extract short and long parameters from any indicator list for backtesting.
   Returns first two numeric params as (short, long) ordered ascending."
  (let ((all-params nil))
    ;; Collect all numeric parameters from all indicators
    (dolist (ind indicators)
      (when (listp ind)
        ;; Only collect INTEGER parameters (filter out floats like BB deviation 2.0)
        (let ((nums (remove-if-not (lambda (n) (and (numberp n) (integerp n))) (cdr ind))))
          (dolist (n nums)
            (push n all-params)))))
    ;; If we have at least 2 params, use them as short/long
    (cond
      ((>= (length all-params) 2)
       (let ((sorted (sort (remove-duplicates all-params) #'<)))
         (values (first sorted) (second sorted))))
      ((= (length all-params) 1)
       ;; Single param (e.g., RSI 14) - use it and a derived value
       (let ((p (first all-params)))
         (values (max 3 (floor p 2)) p)))
      (t
       ;; No params found - use defaults based on indicator type
       (let ((first-ind (car indicators)))
         (cond
           ((and (listp first-ind) (member (car first-ind) '(rsi RSI)))
            (values 7 14))
           ((and (listp first-ind) (member (car first-ind) '(bb BB)))
            (values 10 20))
           ((and (listp first-ind) (member (car first-ind) '(stoch STOCH)))
            (values 7 14))
           ((and (listp first-ind) (member (car first-ind) '(macd MACD)))
            (values 12 26))
           (t (values 5 20))))))))

(defun detect-indicator-type (indicators)
  "Detect primary indicator type from indicators list for Rust backtester.
   Returns a symbol or nil."
  (when (and indicators (listp indicators))
    (let* ((first-ind (if (listp (car indicators)) (caar indicators) (car indicators)))
           (indicator-name (cond
                             ((symbolp first-ind) (string-downcase (symbol-name first-ind)))
                             ((stringp first-ind) (string-downcase first-ind))
                             (t nil))))
      (cond
        ((and indicator-name
              (or (search "vwapvr" indicator-name)
                  (search "vwap-volume-ratio" indicator-name)))
         'vwapvr)
        ((and indicator-name
              (or (search "vpoc" indicator-name)
                  (search "volume-poc" indicator-name)))
         'vpoc)
        ((and indicator-name
              (or (search "volsma" indicator-name)
                  (search "volume-sma" indicator-name)))
         'volsma)
        ((and indicator-name (search "vwap" indicator-name)) 'vwap)
        ((and indicator-name (search "rsi" indicator-name)) 'rsi)
        ((and indicator-name
              (or (search "boland" indicator-name)
                  (search "bollinger" indicator-name)
                  (search "bb" indicator-name)))
         'bb)
        ((and indicator-name (search "macd" indicator-name)) 'macd)
        ((and indicator-name
              (or (search "stoch" indicator-name)
                  (search "stochastic" indicator-name)))
         'stoch)
        (t 'sma)))))

(defun detect-indicator-type-extended (indicators)
  "Detect primary indicator type incl. keltner/donchian. Falls back to detect-indicator-type."
  (when (and indicators (listp indicators))
    (let* ((first-ind (if (listp (car indicators)) (caar indicators) (car indicators)))
           (nm (cond
                 ((symbolp first-ind) (string-downcase (symbol-name first-ind)))
                 ((stringp first-ind) (string-downcase first-ind))
                 (t nil))))
      (cond
        ((and nm (search "keltner" nm)) 'keltner)
        ((and nm (search "donchian" nm)) 'donchian)
        (t (detect-indicator-type indicators))))))

(defun primitive-first-int (indicators)
  "First integer parameter of the first indicator (the period), or nil."
  (let ((first-ind (car indicators)))
    (when (listp first-ind)
      (find-if #'integerp (cdr first-ind)))))

(defun emit-short-long (strat)
  (let ((inds (strategy-indicators strat)))
    (if *enable-primitive-diversity*
        (let ((type (detect-indicator-type-extended inds))
              (p (primitive-first-int inds)))
          (if (and p (member type '(keltner bb rsi stoch donchian)))
              (values p (* 2 p))
              (extract-sma-params inds)))
        (extract-sma-params inds))))

(defun strategy-to-alist (strat &key (name-suffix ""))
  "Convert strategy struct to an alist for S-Expression communication with Guardian."
  (multiple-value-bind (sma-short sma-long)
      (emit-short-long strat)
    `((name . ,(format nil "~a~a" (strategy-name strat) name-suffix))
      (sma_short . ,(or sma-short 5))
      (sma_long . ,(or sma-long 20))
      (sl . ,(or (strategy-sl strat) 0.0))
      (tp . ,(or (strategy-tp strat) 0.0))
      (volume . ,(or (strategy-volume strat) 0.01))
      ;; V2c: extended detector (keltner/donchian) only when the diversity flag is on;
      ;; otherwise the legacy detector, so the emitted alist is byte-identical to before.
      ,@(let ((type (if *enable-primitive-diversity*
                        (detect-indicator-type-extended (strategy-indicators strat))
                        (detect-indicator-type (strategy-indicators strat)))))
          (if type `((indicator_type . ,type)) nil))
      (timeframe . ,(or (strategy-timeframe strat) 1))
      ,@(when (and (slot-exists-p strat 'filter-enabled) (strategy-filter-enabled strat))
          '((filter_enabled . t)))
      (filter_tf . ,(if (slot-exists-p strat 'filter-tf) (or (strategy-filter-tf strat) "") ""))
      (filter_period . ,(if (slot-exists-p strat 'filter-period) (or (strategy-filter-period strat) 0) 0))
      (filter_logic . ,(if (slot-exists-p strat 'filter-logic) (format nil "~a" (strategy-filter-logic strat)) ""))
      ;; V2c: primitive-diversity extension — Rust backtester reads these serde-default
      ;; fields (band_mult / atr_period / atr_barrier_sl / atr_barrier_tp). Emitted ONLY
      ;; when the flag is on; the OFF path never appends them (legacy contract unchanged).
      ,@(when *enable-primitive-diversity*
          `((band_mult . ,(float (or (strategy-band-mult strat) 2.0) 1.0))
            (atr_period . ,(or (strategy-atr-period strat) 14))
            (atr_barrier_sl . ,(float (or (strategy-atr-barrier-sl strat) 0.0) 1.0))
            (atr_barrier_tp . ,(float (or (strategy-atr-barrier-tp strat) 0.0) 1.0)))))))

(defun breeder-legacy-regime (category)
  "The legacy category->regime mapping (byte-identical to the old inline case)."
  (case category
    (:trend :trend)
    (:reversion :reversion)
    (:breakout :breakout)
    (:scalp :reversion) ; Scalp usually reversions/fast trends
    (t :trend)))

(defun breeder-mutation-regime (category)
  "Regime to draw a swap indicator from. Flag OFF => the legacy mapping. Flag ON =>
   with 40% probability CROSS regimes (biased to :reversion/:breakout, where BB/Keltner
   live), so a TREND lineage can finally acquire mean-reversion indicators. This is the
   structural escape from the USDJPY/TREND/SMA absorbing state (regen doc B-3)."
  (if (and *enable-primitive-diversity* (< (random 1.0) 0.4))
      (nth (random 3) '(:reversion :breakout :reversion)) ; bias toward reversion
      (breeder-legacy-regime category)))

(defun pick-diverse-regime ()
  "A non-trend regime for low-frequency regime mutation (flag ON)."
  (nth (random 3) '(:reversion :breakout :reversion)))

(defun make-diverse-primitive-genes (regime)
  "Fresh single-primitive genes for a regime-mutated diverse child, seeded in the
   FORWARD-ROBUST neighbourhood found offline (Keltner/BB mean-reversion, period ~40-64,
   band-mult 1.5-2.5, ATR-normalized barriers 2-3xATR). Returns a plist with
   :indicators/:band-mult/:atr-period/:atr-barrier-sl/:atr-barrier-tp."
  (let* ((period (+ 40 (random 25)))            ; 40..64
         (mult (+ 1.5 (* 0.5 (random 3))))      ; 1.5 / 2.0 / 2.5
         (atr (+ 2.0 (random 2))))              ; 2.0 or 3.0
    (list :indicators (list (list (if (eq regime :breakout) 'keltner 'bollinger) period))
          :band-mult (float mult 1.0)
          :atr-period 14
          :atr-barrier-sl (float atr 1.0)
          :atr-barrier-tp (float atr 1.0))))

(defun make-diverse-child (parent1 parent2 next-gen child-name dir sym base-sl base-tp)
  "Build a fresh diverse primitive child (flag ON regime mutation). Keltner/BB
   mean-reversion in the forward-robust neighbourhood at H4/H6, ATR-normalized barriers.
   Driven by indicator_type + genes on the Rust side, so entry/exit AST are nil. Bypasses
   the legacy AST-crossover / logic-recovery machinery (which targets SMA-cross lineages)."
  (let* ((regime (pick-diverse-regime))
         (genes (make-diverse-primitive-genes regime))
         (tf (nth (random 2) '(240 360)))) ; H4/H6 — where the offline edge lives
    (make-strategy
      :name child-name
      :category regime
      :timeframe tf
      :direction dir
      :symbol sym
      :generation next-gen
      :sl base-sl
      :tp base-tp
      :volume 0.01
      :indicators (getf genes :indicators)
      :entry nil
      :exit nil
      :band-mult (getf genes :band-mult)
      :atr-period (getf genes :atr-period)
      :atr-barrier-sl (getf genes :atr-barrier-sl)
      :atr-barrier-tp (getf genes :atr-barrier-tp)
      :rank :incubator
      :tier :incubator
      :status :active
      :regime-intent regime
      :parents (list (strategy-name parent1) (strategy-name parent2)))))