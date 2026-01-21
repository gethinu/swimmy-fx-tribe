;;; src/lisp/school-hunter.lisp
;;; ============================================================================
;;; STRATEGY HUNTER PIPELINE (Automation Receiver)
;;; ============================================================================
(in-package :swimmy.school)

;;; This file contains strategies "hunted" from the web by the Agent.
;;; It serves as the landing zone for the /hunter workflow.
;;; Strategies here are structurally diverse and originate from external
;;; collective intelligence (PineScript, MQL4, GitHub, Reddit).
;;; ============================================================================

;;; ----------------------------------------------------------------------------
;;; HELPER: ROBUST HUNTER LOADING (Taleb/Antifragility)
;;; ----------------------------------------------------------------------------
;;; TODO(Fowler): Architecture Warning
;;; This file (`school-hunter.lisp`) is an Append-Only Log.
;;; It will eventually grow too large.
;;; TRIGGERS FOR REFACTOR (V10):
;;; 1. File size > 1MB
;;; 2. Load time > 500ms
;;; 3. Syntax errors freq > 1/week
;;; SOLUTION: Split into `strategies/hunted/*.lisp` or use SQLite/DuckDB.
;;; ----------------------------------------------------------------------------
;;; Logic:
;;; 1. Trend Filter: EMA 20 > EMA 50 (Uptrend) / EMA 20 < EMA 50 (Downtrend)
;;; 2. Entry: RSI < 30 (Oversold in Uptrend) / RSI > 70 (Overbought in Downtrend)
;;; 3. Confirmation: Close > EMA 20 (Price returns to trend)
;;; ----------------------------------------------------------------------------

(def-founder :hunted-scalp-hunt "Hunted-Scalp-Hunt-Gen0"
  "Web-Hunted Strategy: EMA Trend + RSI Pullback (Scalping)."
  (make-strategy 
   :name "Hunted-Scalp-Hunt-Gen0"
   :category :scalp
   :timeframe "M5" ; Typical for scalping
   :generation 0
   :sl 0.0010 ; Tight SL
   :tp 0.0020 ; 1:2 Risk/Reward
   :volume 0.02
   :indicators '((ema 20)
                 (ema 50)
                 (rsi 14))
   :entry '(or (and (> ema-20 ema-50)      ; Uptrend
                    (< rsi 30)             ; Discount
                    (> close ema-20))      ; Reclaiming trend
               (and (< ema-20 ema-50)      ; Downtrend
                    (> rsi 70)             ; Premium
                    (< close ema-20)))     ; Resuming trend
   :exit '(or (and (> pnl tp) )            ; Take Profit
              (and (< pnl (- sl)) )        ; Stop Loss
              (and (> pnl 0) (> rsi 50))   ; Quick profit if momentum dies (Long)
              (and (> pnl 0) (< rsi 50))))) ; Quick profit if momentum dies (Short)

;;; ----------------------------------------------------------------------------
;;; HUNTED #2: ICHIMOKU KUMO BREAKOUT (Source: TradingView / Insert Cheese)
;;; ----------------------------------------------------------------------------
;;; Logic: Kumo (Cloud) Breakout. 
;;; 1. Bullish: Close > Senkou A AND Close > Senkou B (Price above Cloud).
;;; 2. Bearish: Close < Senkou A AND Close < Senkou B (Price below Cloud).
;;; 3. Confirmation: Volume > 0 (Basic activity check).
;;; Parameters: 9, 26, 52 (Standard).
;;; ----------------------------------------------------------------------------

(def-founder :hunted-ichimoku-cloud "Hunted-Ichimoku-Cloud-Gen0"
  "Web-Hunted Strategy: Ichimoku Kumo Breakout (Standard)."
  (make-strategy
   :name "Hunted-Ichimoku-Cloud-Gen0"
   :category :trend
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.03
   :indicators '((ichimoku 9 26 52))
   :entry '(or (and (> close senkou-a)
                    (> close senkou-b)
                    (> volume 0)) ; Bullish Breakout
               (and (< close senkou-a)
                    (< close senkou-b)
                    (> volume 0))) ; Bearish Breakout
   :exit '(or (> pnl tp)
              (< pnl (- sl)))))


;;; ----------------------------------------------------------------------------
;;; HUNTED #3: WEB SEARCH (2026-01-17)
;;; Source: Forex Strategies (EMA Trend + RSI Pullback)
;;; Logic: Trend (EMA20 > EMA50) + Pullback (RSI < 45)
;;; ----------------------------------------------------------------------------
(def-founder :hunted-web-rsi-ema "Hunted-Web-RSI-EMA-Pullback-Gen0-260117"
  "Web-Hunted: EMA Trend with RSI Pullback entry."
  (make-strategy
   :name "Hunted-Web-RSI-EMA-Pullback-Gen0-260117"
   :category :trend
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   :indicators '((ema 20) (ema 50) (rsi 14))
   :entry '(or (and (> ema-20 ema-50) (< rsi 45) (> volume 0))
               (and (< ema-20 ema-50) (> rsi 55) (> volume 0)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
