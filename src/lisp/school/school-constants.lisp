(in-package :swimmy.school)

;;; ==========================================
;;; SCHOOL CONSTANTS & PARAMETERS (V50.2)
;;; ==========================================

;;; 1. UNIVERSE DEFINITION (Simons Scope)
(defparameter *timeframes* '("M5" "M15" "H1" "H4" "D1" "W1" "MN") "Multi-Timeframe Scope")
(defparameter *symbols* '("USDJPY" "EURUSD" "GBPUSD") "Primary Currency Pairs")

;;; 2. BACKTEST RANGE (Screening)
;;; Phase 1 (Screening): Learn from history (2011-2020)
(defparameter *backtest-range-1* '(:start "2011.01.01" :end "2020.12.31") "Screening Range (10y)")

;;; 3. EVOLUTION PARAMETERS (Musk Optimization)
(defparameter *b-rank-pool-size* 20 "Max strategies per pool. Speed is survival.")
(defparameter *s-rank-slots* 3 "Elite slots per category (Best of Best)")

;;; 3.1 SELECTION SCORING (Sharpe + PF + WR + MaxDD)
(defparameter *selection-score-sharpe-weight* 0.4)
(defparameter *selection-score-pf-weight* 0.25)
(defparameter *selection-score-wr-weight* 0.2)
(defparameter *selection-score-maxdd-weight* 0.15)

(defun strategy-selection-score (s)
  "Composite score for selection/voting: Sharpe + PF + WR + (1-MaxDD)."
  (let* ((sh (or (strategy-sharpe s) 0.0))
         (pf (or (strategy-profit-factor s) 0.0))
         (wr (or (strategy-win-rate s) 0.0))
         (dd (or (strategy-max-dd s) 1.0)))
    (+ (* *selection-score-sharpe-weight* sh)
       (* *selection-score-pf-weight* pf)
       (* *selection-score-wr-weight* wr)
       (* *selection-score-maxdd-weight* (- 1.0 dd)))))

;;; 4. EXECUTION PARAMETERS
(defparameter *default-sl-pips* 0.15 "Default SL (15 pips)")
(defparameter *default-tp-pips* 0.40 "Default TP (40 pips)")
(defparameter *max-spread-pips* 0.03 "Max Spread (3 pips)")

;;; 5. SCREENING THRESHOLDS
(defparameter *phase1-min-sharpe* 0.1 "Minimum Sharpe for Phase 1 (Screening)")
