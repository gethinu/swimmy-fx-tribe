(in-package :swimmy.school)

;;; ==========================================
;;; SCHOOL CONSTANTS & PARAMETERS (V50.2)
;;; ==========================================

;;; 1. UNIVERSE DEFINITION (Simons Scope)
(defparameter *timeframes* '("M5" "M15" "H1" "H4" "D1" "W1" "MN") "Multi-Timeframe Scope")
(defparameter *symbols* '("USDJPY" "EURUSD" "GBPUSD") "Primary Currency Pairs")

;;; 2. BACKTEST RANGES (2-Stage Validation)
;;; Phase 1 (Screening): Learn from history (2006-2020)
;;; Phase 2 (Validation): Survive the new world (2021-Present)
(defparameter *backtest-range-1* '(:start "2006.01.01" :end "2020.12.31") "Screening Range (15y)")
(defparameter *backtest-range-2* '(:start "2021.01.01" :end "2026.12.31") "Validation Range (OOS)")

;;; 3. EVOLUTION PARAMETERS (Musk Optimization)
(defparameter *b-rank-pool-size* 20 "Max strategies per pool. Speed is survival.")
(defparameter *s-rank-slots* 3 "Elite slots per category (Best of Best)")

;;; 4. EXECUTION PARAMETERS
(defparameter *default-sl-pips* 0.15 "Default SL (15 pips)")
(defparameter *default-tp-pips* 0.40 "Default TP (40 pips)")
(defparameter *max-spread-pips* 0.03 "Max Spread (3 pips)")

;;; 5. SCREENING THRESHOLDS
(defparameter *phase1-min-sharpe* 0.1 "Minimum Sharpe for Phase 1 (Screening)")
(defparameter *phase2-min-sharpe* 0.5 "Minimum Sharpe for Phase 2 (Validation)")

