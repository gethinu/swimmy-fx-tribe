;;; src/lisp/school-hunter.lisp
;;; (Appending to existing file content - ensuring Full Content is preserved)
;;; ... [Previous content omitted for brevity in prompt, but I must write FULL file] ...
;;; Actually I will APPEND the new ones to the PREVIOUS FULL CONTENT from Step 1422.

(in-package :swimmy.school)

;;; [RE-INJECTING PREVIOUS CONTENT FROM STEP 1422 TO PRESERVE IT]

(def-founder :hunted-scalp-hunt "Hunted-Scalp-Hunt-Gen0"
  "Web-Hunted Strategy: EMA Trend + RSI Pullback (Scalping)."
  (make-strategy 
   :name "Hunted-Scalp-Hunt-Gen0"
   :category :scalp
   :timeframe "M5"
   :generation 0
   :sl 0.0010
   :tp 0.0020
   :volume 0.02
   :indicators '((ema 20) (ema 50) (rsi 14))
   :entry '(or (and (> ema-20 ema-50) (< rsi 30) (> close ema-20))
               (and (< ema-20 ema-50) (> rsi 70) (< close ema-20)))
   :exit '(or (> pnl tp) (< pnl (- sl)) (and (> pnl 0) (> rsi 50)))))

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
   :entry '(or (and (> close senkou-a) (> close senkou-b) (> volume 0))
               (and (< close senkou-a) (< close senkou-b) (> volume 0)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

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

(def-founder :hunted-bb-reversion "Hunted-BB-Reversion-Gen0-260124"
  "Web-Hunted: Bollinger Band Pierce & Close Reversion."
  (make-strategy
   :name "Hunted-BB-Reversion-Gen0-260124"
   :category :reversion
   :timeframe "M15"
   :generation 0
   :sl 0.0020
   :tp 0.0040
   :volume 0.01
   :indicators '((bollinger 20 2))
   :entry '(or (and (< low lower-band) (> close lower-band))
               (and (> high upper-band) (< close upper-band)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

(def-founder :hunted-macd-stoch "Hunted-MACD-Stoch-Gen0-260124"
  "Web-Hunted: MACD Crossover with Stochastic Confirmation."
  (make-strategy
   :name "Hunted-MACD-Stoch-Gen0-260124"
   :category :trend
   :timeframe "H1"
   :generation 0
   :sl 0.0040
   :tp 0.0080
   :volume 0.01
   :indicators '((macd 12 26 9) (stochastic 14 3 3))
   :entry '(or (and (> macd-main macd-signal) (< k 20) (> k d))
               (and (< macd-main macd-signal) (> k 80) (< k d)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

(def-founder :hunted-london-proxy "Hunted-ATR-Breakout-Gen0-260124"
  "Web-Hunted: Volatility Breakout (London Proxy)."
  (make-strategy
   :name "Hunted-ATR-Breakout-Gen0-260124"
   :category :breakout
   :timeframe "M15"
   :generation 0
   :sl 0.0030
   :tp 0.0060
   :volume 0.01
   :indicators '((atr 14) (rsi 14) (sma 50))
   :entry '(or (and (> close sma-50) (> rsi 60) (> atr 0.0010))
               (and (< close sma-50) (< rsi 40) (> atr 0.0010)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; ----------------------------------------------------------------------------
;;; PHASE 2: ADVANCED CONCEPTS (2026-01-24)
;;; ----------------------------------------------------------------------------

;;; 7. STOP HUNTER (Liquidity)
;;; Logic: New 20-bar High/Low made, but price reverses aggressively (RSI Divergence or Candle shape).
;;; Proxy: Price > Highest(20), but Close < Open (Bearish Candle).
(def-founder :hunted-stop-hunter "Hunted-Stop-Hunter-Gen0"
  "Web-Hunted: Liquidity Stop Run Fade."
  (make-strategy 
   :name "Hunted-Stop-Hunter-Gen0"
   :category :reversion
   :timeframe "M15"
   :generation 0
   :sl 0.0015
   :tp 0.0030 ; 1:2
   :volume 0.03
   :indicators '((donchian 20) (rsi 14))
   :entry '(or (and (> high upper-band) (< close open))      ; Fake Breakout High
               (and (< low lower-band) (> close open)))      ; Fake Breakout Low
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; 8. TREND PULLBACK (Value)
;;; Logic: Trend (SMA 200) is UP, but Price dips to Lower Bollinger Band (or RSI < 30).
;;; Buying value in an uptrend.
(def-founder :hunted-trend-pullback "Hunted-Trend-Pullback-Gen0"
  "Web-Hunted: Multi-Logical Trend Pullback."
  (make-strategy
   :name "Hunted-Trend-Pullback-Gen0"
   :category :trend
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.02
   :indicators '((sma 200) (rsi 14))
   :entry '(or (and (> close sma-200) (< rsi 30))  ; Buy Dip
               (and (< close sma-200) (> rsi 70))) ; Sell Rally
   :exit '(or (> pnl tp) (< pnl (- sl)))))

;;; 9. VOLATILITY SQUEEZE (Gamma)
;;; Logic: Bollinger Bandwidth is continuously low, then Price breaks out.
;;; Proxy: ATR is low (< threshold), then Price moves > ATR * N.
(def-founder :hunted-vol-squeeze "Hunted-Vol-Squeeze-Gen0"
  "Web-Hunted: Volatility Compression Breakout."
  (make-strategy
   :name "Hunted-Vol-Squeeze-Gen0"
   :category :breakout
   :timeframe "H4"
   :generation 0
   :sl 0.0040
   :tp 0.0120 ; 1:3 for big moves
   :volume 0.02
   :indicators '((bollinger 20 2) (atr 14))
   :entry '(or (and (> close upper-band) (< atr 0.0010)) ; Breakout from quiet logic
               (and (< close lower-band) (< atr 0.0010)))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251329
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema10-rsi6-gen0-2601251329 "Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251329"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251329"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251330
;;; Logic: MOMENTUM_SCALP_EMA(10)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema10-rsi6-gen0-2601251330 "Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA10-RSI6-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 9) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI16-Bollinger20-2.1Squeeze-Gen0-2601251330
;;; Logic: RSI(16) + Bollinger(20, 2.1) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi16-bollinger20-2.1squeeze-gen0-2601251330 "Scalp-RSI16-Bollinger20-2.1Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI16-Bollinger20-2.1Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 20 2.1))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330
;;; Logic: RSI(13) + Bollinger(18, 2.2) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi13-bollinger18-2.2squeeze-gen0-2601251330 "Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 13) (bb 18 2.2))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI16-Bollinger22-1.8Squeeze-Gen0-2601251330
;;; Logic: RSI(16) + Bollinger(22, 1.8) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi16-bollinger22-1.8squeeze-gen0-2601251330 "Scalp-RSI16-Bollinger22-1.8Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI16-Bollinger22-1.8Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 22 1.8))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI16-Bollinger18-2.1Squeeze-Gen0-2601251330
;;; Logic: RSI(16) + Bollinger(18, 2.1) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi16-bollinger18-2.1squeeze-gen0-2601251330 "Scalp-RSI16-Bollinger18-2.1Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI16-Bollinger18-2.1Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 18 2.1))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330
;;; Logic: RSI(13) + Bollinger(18, 2.2) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi13-bollinger18-2.2squeeze-gen0-2601251330 "Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI13-Bollinger18-2.2Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 13) (bb 18 2.2))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI12-Bollinger21-2.0Squeeze-Gen0-2601251330
;;; Logic: RSI(12) + Bollinger(21, 2.0) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi12-bollinger21-2.0squeeze-gen0-2601251330 "Scalp-RSI12-Bollinger21-2.0Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI12-Bollinger21-2.0Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 12) (bb 21 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI16-Bollinger22-1.9Squeeze-Gen0-2601251330
;;; Logic: RSI(16) + Bollinger(22, 1.9) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi16-bollinger22-1.9squeeze-gen0-2601251330 "Scalp-RSI16-Bollinger22-1.9Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI16-Bollinger22-1.9Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 16) (bb 22 1.9))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI13-Bollinger18-2.0Squeeze-Gen0-2601251330
;;; Logic: RSI(13) + Bollinger(18, 2.0) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi13-bollinger18-2.0squeeze-gen0-2601251330 "Scalp-RSI13-Bollinger18-2.0Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI13-Bollinger18-2.0Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 13) (bb 18 2.0))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI14-Bollinger21-1.9Squeeze-Gen0-2601251330
;;; Logic: RSI(14) + Bollinger(21, 1.9) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi14-bollinger21-1.9squeeze-gen0-2601251330 "Scalp-RSI14-Bollinger21-1.9Squeeze-Gen0-2601251330"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI14-Bollinger21-1.9Squeeze-Gen0-2601251330"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 14) (bb 21 1.9))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA8-RSI7-Gen0-2601251332
;;; Logic: MOMENTUM_SCALP_EMA(8)_RSI(7) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema8-rsi7-gen0-2601251332 "Scalp-MOMENTUM-SCALP-EMA8-RSI7-Gen0-2601251332"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA8-RSI7-Gen0-2601251332"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 8) (rsi 6))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-RSI13-Bollinger20-1.9Squeeze-Gen0-2601251333
;;; Logic: RSI(13) + Bollinger(20, 1.9) Squeeze (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-rsi13-bollinger20-1.9squeeze-gen0-2601251333 "Scalp-RSI13-Bollinger20-1.9Squeeze-Gen0-2601251333"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-RSI13-Bollinger20-1.9Squeeze-Gen0-2601251333"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((rsi 13) (bb 20 1.9))
   :entry '(and (< bb-width 0.0010) (> rsi 60) (> volume 0))
   :exit '(> pnl tp)
   :exit '(or (> pnl tp) (< pnl (- sl)))))
;;; ----------------------------------------------------------------------------
;;; AUTO-HUNTED: Scalp-MOMENTUM-SCALP-EMA9-RSI6-Gen0-2601251334
;;; Logic: MOMENTUM_SCALP_EMA(9)_RSI(6) (Generated via trigger_hunt.py)
;;; ----------------------------------------------------------------------------
(def-founder :scalp-momentum-scalp-ema9-rsi6-gen0-2601251334 "Scalp-MOMENTUM-SCALP-EMA9-RSI6-Gen0-2601251334"
  "Auto-Generated Strategy by Hunter Agent."
  (make-strategy
   :name "Scalp-MOMENTUM-SCALP-EMA9-RSI6-Gen0-2601251334"
   :category :scalp
   :timeframe "H1"
   :generation 0
   :sl 0.0050
   :tp 0.0100
   :volume 0.01
   
   :indicators '((ema 10) (rsi 8))
   :entry '(and (> close ema) (> rsi 70))
   :exit '(or (> pnl tp) (< rsi 50))
   :exit '(or (> pnl tp) (< pnl (- sl)))))
