;;; src/lisp/school/school-seed.lisp
;;; ============================================================================
;;; V2d regen ESCAPE ROUTE — diversity SEED injection (bypass the breeding pre-gate ONLY)
;;; ============================================================================
;;;   (regen_engine_redesign_20260703.md §B; measured gap:
;;;    tribe_2d_regen_b3_niche_quota_20260718.md / tribe_2d_regen_b5_overflow_20260718.md)
;;;
;;; Today's B-3/B-4/B-5 daemon_verify runs PROVED the diversity machinery is correct but never
;;; ENGAGES on the USDJPY-TREND monoculture, because that pool is never bred: the §B-6 CPCV
;;; breeding pre-gate (breeding-cpcv-eligible-p, cpcv>=0.55) excludes the CPCV-poor mono and the
;;; B-4 clone-block (dist<0.02) forbids USDJPY-TREND × USDJPY-TREND pairs. So symbol mutation
;;; (§B-3(2)) never fires on a TREND parent → no non-USDJPY-TREND child is ever born → the TREND
;;; pool stays a single (symbol×regime) niche → the niche quota (§B-3(3) part-1) and the
;;; fitness-sharing overflow sort (§B-5) have no 2nd niche to bite on. The ONLY escape is to put
;;; a non-USDJPY-TREND member INTO the population by a path that does not require breeding.
;;;
;;; This module injects such members directly. It relaxes EXACTLY ONE thing — the breeding CPCV
;;; pre-gate (the ENTRY): a seed enters the population without first having been a CPCV>=0.55
;;; parent. It relaxes NOTHING about the honest verification floor. A seed carries NO
;;; performance metrics (cpcv-pass-rate / sharpe / pf / trades left at fresh make-strategy
;;; defaults = 0.0 / oos-sharpe nil), so to reach rank B / A / diverse-robust it MUST be
;;; honestly backtested (real OOS + CPCV on real LOCAL price data) and pass the SAME Phase-1
;;; screening + §4 live-edge floor as every other strategy. "Give the entry, never fabricate
;;; the verdict." If the honest gate culls every seed, the seed is COSMETIC and the heavy
;;; conclusion (there is no honest non-USDJPY-TREND edge) stands; if some survive, it is REAL.
;;;
;;; Seeds are TREND-FIRST (the regime the monoculture owns, and the regime the earlier
;;; EUR/GBP-REVERSION seed set never touched), spread across EURUSD/GBPUSD/EURJPY and pip/ATR
;;; barriers, plus a REVERSION minority (Keltner/BB mean-reversion — the sole prior honest
;;; forward-robust region, EURUSD-biased) and a BREAKOUT minority (Donchian) so the set is
;;; genuinely diverse, not a mini-monoculture. Symbols are restricted to those with real local
;;; M1 data (no fabricated-data seeds).
;;;
;;; Flag OFF (*enable-primitive-diversity* nil) => inject-diversity-seeds returns NIL (no seed,
;;; byte-identical legacy). Composes with B-4 (calculate-genetic-distance sees the seed's
;;; symbol/regime), B-5 (fitness-sharing overflow sort), and B-3 (niche quota + symbol
;;; mutation): once a seed survives the honest gate the TREND pool has a genuine 2nd niche and
;;; the previously-idle machinery engages.
;;;
;;; PURE module: depends only on the strategy struct (dsl.lisp), strategy-regime-class
;;; (school-strategy.lisp) and the flags — so tests/diversity_seed_offline_test.lisp exercises
;;; it FFI-free with sbcl --script (no sqlite/pzmq), matching genome_b4_distance_offline_test.
;;; ============================================================================

(in-package :swimmy.school)

(defparameter *enable-diversity-seed-injection* t
  "regen escape-route sub-flag, consulted ONLY when *enable-primitive-diversity* is on. When
   nil, inject-diversity-seeds is a no-op even under the diversity flag (lets the seed path be
   disabled independently of the rest of the 2c/2d machinery). Default t: whenever the owner
   turns the master diversity flag on, the escape route is available. The master switch
   (swimmy.core:*enable-primitive-diversity*, config default OFF) still gates everything — OFF
   => byte-identical legacy behaviour.")

(defparameter *diversity-seed-symbols-with-data* '("EURUSD" "GBPUSD" "EURJPY")
  "Non-USDJPY currencies that have real local M1 data under data/historical (verified present:
   EURUSD_M1.csv, GBPUSD_M1.csv, EURJPY_M1.csv). A seed is NEVER emitted for a symbol without
   real data — no fabricated-data seeds.")

(defparameter *diversity-seed-specs*
  ;; (regime symbol indicator period timeframe-min band-mult atr-barrier-sl atr-barrier-tp)
  ;; TREND-FIRST: fill the regime the USDJPY monoculture owns with non-USDJPY members, across
  ;; EURUSD/GBPUSD/EURJPY, several SMA-cross periods, and BOTH pip (atr 0) and ATR-normalized
  ;; barriers (2x/3x) so the honest gate sees a fair spread. Then a REVERSION minority
  ;; (Keltner/BB mean-reversion — the sole prior honest forward-robust region, EURUSD-biased)
  ;; and a BREAKOUT minority (Donchian channel). indicator symbols map 1:1 to primitive_scan's
  ;; honest primitives {sma,keltner,bollinger->bb,donchian}, so every seed is honestly scorable.
  '(;; --- TREND (SMA crossover) : the escape-route focus (12) ---
    (:trend "EURUSD" sma 20 240 2.0 0.0 0.0)
    (:trend "EURUSD" sma 30 240 2.0 2.0 3.0)
    (:trend "EURUSD" sma 40 240 2.0 2.0 2.0)
    (:trend "EURUSD" sma 50 360 2.0 0.0 0.0)
    (:trend "EURUSD" sma 60 360 2.0 3.0 3.0)
    (:trend "GBPUSD" sma 20 240 2.0 0.0 0.0)
    (:trend "GBPUSD" sma 30 240 2.0 2.0 3.0)
    (:trend "GBPUSD" sma 40 240 2.0 2.0 2.0)
    (:trend "GBPUSD" sma 50 360 2.0 0.0 0.0)
    (:trend "GBPUSD" sma 60 360 2.0 3.0 3.0)
    (:trend "EURJPY" sma 20 240 2.0 0.0 0.0)
    (:trend "EURJPY" sma 30 240 2.0 2.0 3.0)
    ;; --- REVERSION (Keltner / BB mean-reversion) : prior honest forward-robust region (4) ---
    (:reversion "EURUSD" keltner 55 240 2.0 2.0 2.0)
    (:reversion "EURUSD" keltner 58 240 2.0 3.0 3.0)
    (:reversion "EURUSD" bollinger 40 240 2.0 0.0 0.0)
    (:reversion "GBPUSD" keltner 50 240 2.0 2.0 3.0)
    ;; --- BREAKOUT (Donchian channel) (2) ---
    (:breakout "EURUSD" donchian 40 240 2.0 2.0 3.0)
    (:breakout "GBPUSD" donchian 55 360 2.0 2.0 3.0))
  "Diverse seed templates. 18 seeds: TREND-majority (12), 3 symbols, pip+ATR barriers — NOT the
   earlier EUR/GBP-REVERSION mini-monoculture. Every symbol has real local data.")

(defun %seed-base-template (population)
  "Pick a valid base strategy to clone (for a well-formed entry/exit AST + slot defaults).
   Prefers a USDJPY-TREND veteran from POPULATION; falls back to a fresh make-strategy."
  (or (find-if (lambda (s) (and (strategy-p s)
                                (equal (strategy-symbol s) "USDJPY")
                                (eq (or (ignore-errors (strategy-regime-class s))
                                        (strategy-category s)) :trend)))
               population)
      (find-if #'strategy-p population)
      (make-strategy :name "SEED-BASE" :indicators '((sma 20)) :category :trend)))

(defun build-diversity-seed (spec base idx)
  "Build ONE UNSCORED diversity-seed strategy from SPEC by cloning BASE (for a valid entry/exit
   AST) and re-pointing the symbol / regime / indicator genes. Performance metrics are
   deliberately left at fresh defaults (cpcv-pass-rate 0.0, sharpe 0.0, pf 0.0, trades 0,
   oos-sharpe nil) so the seed cannot be counted robust or breed until it passes the HONEST
   gate. Returns the strategy. This function NEVER writes a performance number."
  (destructuring-bind (regime symbol indicator period tf band atr-sl atr-tp) spec
    (let ((c (if (and base (strategy-p base)) (copy-strategy base)
                 (make-strategy :name "s"))))
      (setf (strategy-name c) (format nil "DVSEED-~a-~a-~a~d-~d" symbol
                                      (string-downcase (symbol-name regime))
                                      (string-downcase (symbol-name indicator)) period idx)
            (strategy-symbol c) symbol
            (strategy-category c) regime
            (strategy-regime-intent c) regime
            (strategy-indicators c) (list (list indicator period))
            (strategy-indicator-type c) (string-downcase (symbol-name indicator))
            (strategy-timeframe c) tf
            (strategy-band-mult c) (float band 1.0d0)
            (strategy-atr-period c) 14
            (strategy-atr-barrier-sl c) (float atr-sl 1.0d0)
            (strategy-atr-barrier-tp c) (float atr-tp 1.0d0)
            (strategy-sl c) 0.005d0
            (strategy-tp c) 0.005d0
            ;; HONEST: no fabricated performance — must earn these via the real gate.
            (strategy-sharpe c) 0.0d0
            (strategy-profit-factor c) 0.0d0
            (strategy-win-rate c) 0.0d0
            (strategy-max-dd c) 0.0d0
            (strategy-trades c) 0
            (strategy-oos-sharpe c) nil
            (strategy-cpcv-pass-rate c) 0.0d0
            (strategy-cpcv-median-sharpe c) 0.0d0
            (strategy-generation c) 0
            (strategy-breeding-count c) 0
            (strategy-age c) 0
            (strategy-immortal c) nil
            (strategy-status c) :active
            (strategy-status-reason c) "diversity-seed (unscored)"
            (strategy-revalidation-pending c) nil
            (strategy-rank c) nil
            (strategy-tier c) :incubator
            (strategy-parents c) nil
            (strategy-pnl-history c) nil
            (strategy-hash c) nil)
      c)))

(defun inject-diversity-seeds (population &key (limit nil))
  "regen escape route (flag-gated). Returns a list of UNSCORED non-USDJPY seed candidates, or
   NIL. OFF (swimmy.core:*enable-primitive-diversity* nil OR *enable-diversity-seed-injection*
   nil) => NIL (no-op, byte-identical legacy). ON => build the diverse TREND-first seed set from
   *diversity-seed-specs* by cloning a valid base from POPULATION, carrying NO fabricated
   performance metrics. The CALLER must route each returned seed through the honest scoring +
   Phase-1/§4 promotion path; this function never promotes, ranks, or fabricates a verdict. It
   bypasses ONLY the breeding CPCV pre-gate (the entry) — the verification floor is untouched.
   LIMIT (optional) truncates the spec list (used by the offline test)."
  (when (and *enable-primitive-diversity* *enable-diversity-seed-injection*)
    (let* ((base (%seed-base-template population))
           (specs (if limit (subseq *diversity-seed-specs*
                                    0 (min limit (length *diversity-seed-specs*)))
                      *diversity-seed-specs*))
           (seeds (loop for spec in specs for i from 0
                        collect (build-diversity-seed spec base i))))
      ;; Defensive: never emit a seed for a symbol without real local data, and never a
      ;; USDJPY seed (the escape route is strictly non-USDJPY).
      (remove-if-not (lambda (s) (and (not (equal (strategy-symbol s) "USDJPY"))
                                      (member (strategy-symbol s)
                                              *diversity-seed-symbols-with-data* :test #'equal)))
                     seeds))))
