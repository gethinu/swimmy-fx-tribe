;;; strategies-legendary.lisp
;;; ALPHA INJECTION: Classic "Legendary" Strategies to seed the gene pool.
;;; These serve as stable parents for the "Smart Breeding" program.

(in-package :swimmy.school)

;; 1. The Golden Cross (Trend)
;; Classic SMA Crossover. Validated over decades.
(defstrategy "Legendary-Golden-Cross"
  :category :trend
  :timeframe 240 ; 4 Hour
  :indicators ("SMA-50" "SMA-200")
  :entry "CROSS SMA 50 200"
  :exit "CROSS SMA 50 200"
  :sl 0.05
  :tp 0.15
  :regime-filter t)

;; 2026-03-07 audit note:
;; These external Legend drafts remain as legacy seed only.
;; Canonical definitions live in legends.lisp or data/library/LEGEND + DB rows.
;; Do not restore the old RSI/London drafts here without a fresh source audit.
;;
;; legacy seed only: Legend-RSI-Reversion-V1
;; - old draft: RSI(2) < 5, exit on close > SMA(5), D1, SL 0.088, TP 0.091
;;
;; legacy seed only: Legend-London-Breakout-V1
;; - old draft: session-high/session-low 08-16, time-exit 24, H1, SL 0.10, TP 0.20

;; 2. [DELETED] London Breakout (Fragile Time-Based)
;; 3. [DELETED] RSI Reversion M30 (Redundant with M5)
;; 4. [DELETED] MACD Trend (Redundant with Golden Cross)

;; 5. Bollinger Squeeze (Breakout)
;; Expansion after contraction.
(defstrategy "Legendary-Bollinger-Squeeze"
  :category :breakout
  :timeframe 1440 ; Daily
  :indicators ("BOLAND-20-2" "ATR-14")
  :entry "CROSS CLOSE BOLAND-UP" ; Simplified entry
  :exit "CROSS CLOSE BOLAND-MID"
  :sl 0.05
  :tp 0.15
  :regime-filter t)
