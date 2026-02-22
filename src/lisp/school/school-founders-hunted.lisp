;;; school-founders-hunted.lisp
;;; Hunted Strategies (H4/D1/W1) - Batch Recruit
;;; ============================================================================

(in-package :swimmy.school)

;;; ----------------------------------------------------------------------------
;;; 6. HUNTED STRATEGIES (H4 SWING) - BATCH 1
;;; ----------------------------------------------------------------------------

(def-founder :hunted-h4-ma-cross "Hunted-H4-MA-Cross-20-50"
  "Classic H4 Swing Trend. EMA 20 crossing EMA 50."
  (make-strategy :name "Hunted-H4-MA-Cross-20-50" :category :trend :timeframe "H4" :generation 0
    :sl 0.0050 :tp 0.0100 :volume 0.02
    :indicators '((ema 20) (ema 50))
    :entry '(cross-above ema-20 ema-50) :exit '(cross-below ema-20 ema-50)))

(def-founder :hunted-h4-rsi-rev "Hunted-H4-RSI-Reversal"
  "H4 RSI Reversion. Buy dip in uptrend context."
  (make-strategy :name "Hunted-H4-RSI-Reversal" :category :reversion :timeframe "H4" :generation 0
    :sl 0.0040 :tp 0.0080 :volume 0.02
    :indicators '((rsi 14) (sma 50))
    :entry '(and (< rsi 30) (> close sma-50)) :exit '(> rsi 60)))

(def-founder :hunted-h4-bb-bounce "Hunted-H4-BB-Bounce"
  "H4 Bollinger Band Bounce."
  (make-strategy :name "Hunted-H4-BB-Bounce" :category :reversion :timeframe "H4" :generation 0
    :sl 0.0030 :tp 0.0060 :volume 0.02
    :indicators '((bb 20 2))
    :entry '(< close bb-lower) :exit '(> close bb-middle)))

(def-founder :hunted-h4-donchian "Hunted-H4-Breakout-Donchian"
  "H4 Donchian Breakout (20)."
  (make-strategy :name "Hunted-H4-Breakout-Donchian" :category :breakout :timeframe "H4" :generation 0
    :sl 0.0040 :tp 0.0080 :volume 0.02
    :indicators '((donchian 20))
    :entry '(> close donchian-upper) :exit '(< close donchian-mid)))

(def-founder :hunted-h4-macd "Hunted-H4-MACD-Trend"
  "H4 MACD Trend Following."
  (make-strategy :name "Hunted-H4-MACD-Trend" :category :trend :timeframe "H4" :generation 0
    :sl 0.0040 :tp 0.0100 :volume 0.02
    :indicators '((macd 12 26 9))
    :entry '(cross-above macd-line signal-line) :exit '(cross-below macd-line signal-line)))

(def-founder :hunted-h4-stoch "Hunted-H4-Stoch-Cross"
  "H4 Stochastic Crossover."
  (make-strategy :name "Hunted-H4-Stoch-Cross" :category :reversion :timeframe "H4" :generation 0
    :sl 0.0030 :tp 0.0060 :volume 0.02
    :indicators '((stoch 14 3 3))
    :entry '(cross-above stoch-k stoch-d) :exit '(cross-below stoch-k stoch-d)))

(def-founder :hunted-h4-triple-ema "Hunted-H4-Triple-EMA"
  "H4 Triple EMA Trend (10>20>50)."
  (make-strategy :name "Hunted-H4-Triple-EMA" :category :trend :timeframe "H4" :generation 0
    :sl 0.0050 :tp 0.0150 :volume 0.02
    :indicators '((ema 10) (ema 20) (ema 50))
    :entry '(and (> ema-10 ema-20) (> ema-20 ema-50)) :exit '(< ema-10 ema-20)))

(def-founder :hunted-h4-cci "Hunted-H4-CCI-Explosion"
  "H4 CCI Breakout."
  (make-strategy :name "Hunted-H4-CCI-Explosion" :category :breakout :timeframe "H4" :generation 0
    :sl 0.0040 :tp 0.0080 :volume 0.02
    :indicators '((cci 20))
    :entry '(> cci 100) :exit '(< cci 0)))

(def-founder :hunted-h4-atr "Hunted-H4-ATR-Trend"
  "H4 ATR Volatility Trend."
  (make-strategy :name "Hunted-H4-ATR-Trend" :category :trend :timeframe "H4" :generation 0
    :sl 0.0060 :tp 0.0120 :volume 0.02
    :indicators '((sma 20) (atr 14))
    :entry '(and (> close sma-20) (> atr 0.0010)) :exit '(< close sma-20)))

(def-founder :hunted-h4-sar "Hunted-H4-Parabolic"
  "H4 Parabolic SAR Trend."
  (make-strategy :name "Hunted-H4-Parabolic" :category :trend :timeframe "H4" :generation 0
    :sl 0.0050 :tp 0.0100 :volume 0.02
    :indicators '((sma 50)) ; Proxy as we wait for SAR implementation
    :entry '(cross-above close sma-50) :exit '(cross-below close sma-50)))


;;; ----------------------------------------------------------------------------
;;; 7. HUNTED STRATEGIES (D1 POSITION) - BATCH 2
;;; ----------------------------------------------------------------------------

(def-founder :hunted-d1-golden "Hunted-D1-Golden-Cross"
  "D1 Golden Cross (50/200 SMA)."
  (make-strategy :name "Hunted-D1-Golden-Cross" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0300 :volume 0.03
    :indicators '((sma 50) (sma 200))
    :entry '(cross-above sma-50 sma-200) :exit '(cross-below sma-50 sma-200)))

(def-founder :hunted-d1-rsi2 "Hunted-D1-Connors-RSI2"
  "D1 RSI(2) Mean Reversion."
  (make-strategy :name "Hunted-D1-Connors-RSI2" :category :reversion :timeframe "D1" :generation 0
    :sl 0.0080 :tp 0.0080 :volume 0.03
    :indicators '((rsi 2) (sma 200))
    :entry '(and (< rsi 10) (> close sma-200)) :exit '(> rsi 80)))

(def-founder :hunted-d1-turtle "Hunted-D1-Turtle-20"
  "D1 Turtle Trading Channel (20)."
  (make-strategy :name "Hunted-D1-Turtle-20" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0300 :volume 0.03
    :indicators '((donchian 20))
    :entry '(> close donchian-upper) :exit '(< close donchian-mid)))

(def-founder :hunted-d1-turtle-long "Hunted-D1-Turtle-55"
  "D1 Turtle Long Term (55)."
  (make-strategy :name "Hunted-D1-Turtle-55" :category :trend :timeframe "D1" :generation 0
    :sl 0.0150 :tp 0.0500 :volume 0.03
    :indicators '((donchian 55))
    :entry '(> close donchian-upper) :exit '(< close donchian-mid)))

(def-founder :hunted-d1-macd-zero "Hunted-D1-MACD-Zero"
  "D1 MACD Zero Line Cross."
  (make-strategy :name "Hunted-D1-MACD-Zero" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0200 :volume 0.03
    :indicators '((macd 12 26 9))
    :entry '(cross-above macd-line 0) :exit '(cross-below macd-line 0)))

(def-founder :hunted-d1-bb-walk "Hunted-D1-BB-Walk"
  "D1 Bollinger Band Walk."
  (make-strategy :name "Hunted-D1-BB-Walk" :category :breakout :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0300 :volume 0.03
    :indicators '((bb 20 2))
    :entry '(> close bb-upper) :exit '(< close bb-middle)))

(def-founder :hunted-d1-ema-ribbon "Hunted-D1-EMA-Ribbon"
  "D1 EMA Ribbon (20/50/100)."
  (make-strategy :name "Hunted-D1-EMA-Ribbon" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0300 :volume 0.03
    :indicators '((ema 20) (ema 50) (ema 100))
    :entry '(and (> ema-20 ema-50) (> ema-50 ema-100)) :exit '(< ema-20 ema-50)))

(def-founder :hunted-d1-adx "Hunted-D1-ADX-Trend-V2"
  "D1 ADX Strong Trend."
  (make-strategy :name "Hunted-D1-ADX-Trend-V2" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0300 :volume 0.03
    :indicators '((adx 14) (ema 50))
    :entry '(and (> adx 23) (> close ema-50))
    :exit '(or (< adx 18) (< close ema-50))))

(def-founder :hunted-d1-williams "Hunted-D1-Williams-R-V2"
  "D1 Williams %R Reversion."
  (make-strategy :name "Hunted-D1-Williams-R-V2" :category :reversion :timeframe "D1" :generation 0
    :sl 0.0080 :tp 0.0150 :volume 0.03
    :indicators '((williams 14) (sma 200))
    :entry '(and (> close sma-200) (cross-above williams -80))
    :exit '(or (< close sma-200) (> williams -20))))

(def-founder :hunted-d1-sar "Hunted-D1-SMA-Break"
  "D1 SMA 10 Breakout (Momentum)."
  (make-strategy :name "Hunted-D1-SMA-Break" :category :trend :timeframe "D1" :generation 0
    :sl 0.0100 :tp 0.0200 :volume 0.03
    :indicators '((sma 10))
    :entry '(cross-above close sma-10) :exit '(cross-below close sma-10)))

(def-founder :hunted-h12-vwapvr-50-150 "Hunted-H12-VWAPVR-50-150-USDJPY"
  "H12 VWAP Volume-Ratio breakout tuned for USDJPY (orange threshold 150)."
  (make-strategy :name "Hunted-H12-VWAPVR-50-150-USDJPY"
                 :symbol "USDJPY"
                 :category :trend :timeframe 720 :generation 0
                 :sl 1.6 :tp 6.0 :volume 0.03
                 :indicators '((vwapvr 50 150))
                 :entry '(and (< vwapvr-50-prev vwapvr-threshold)
                              (>= vwapvr-50 vwapvr-threshold))
                 :exit '(<= vwapvr-50 0)))

(def-founder :hunted-d1-vwapvr-50-220-eurusd "Hunted-D1-VWAPVR-50-220-EURUSD"
  "D1 VWAP Volume-Ratio breakout tuned for EURUSD (threshold 220)."
  (make-strategy :name "Hunted-D1-VWAPVR-50-220-EURUSD"
                 :symbol "EURUSD"
                 :category :trend :timeframe 1440 :generation 0
                 :sl 1.2 :tp 4.0 :volume 0.03
                 :indicators '((vwapvr 50 220))
                 :entry '(and (< vwapvr-50-prev vwapvr-threshold)
                              (>= vwapvr-50 vwapvr-threshold))
                 :exit '(<= vwapvr-50 0)))

(def-founder :hunted-d1-vwapvr-80-180-gbpusd "Hunted-D1-VWAPVR-80-180-GBPUSD"
  "D1 VWAP Volume-Ratio breakout tuned for GBPUSD (threshold 180)."
  (make-strategy :name "Hunted-D1-VWAPVR-80-180-GBPUSD"
                 :symbol "GBPUSD"
                 :category :trend :timeframe 1440 :generation 0
                 :sl 1.2 :tp 4.0 :volume 0.03
                 :indicators '((vwapvr 80 180))
                 :entry '(and (< vwapvr-80-prev vwapvr-threshold)
                              (>= vwapvr-80 vwapvr-threshold))
                 :exit '(<= vwapvr-80 0)))


;;; ----------------------------------------------------------------------------
;;; 8. HUNTED STRATEGIES (W1 LONG TERM) - BATCH 3
;;; ----------------------------------------------------------------------------

(def-founder :hunted-w1-weinstein "Hunted-W1-Weinstein"
  "W1 Stan Weinstein Stage 2 (SMA 30)."
  (make-strategy :name "Hunted-W1-Weinstein" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0600 :volume 0.05
    :indicators '((sma 30))
    :entry '(cross-above close sma-30) :exit '(cross-below close sma-30)))

(def-founder :hunted-w1-macd "Hunted-W1-MACD"
  "W1 MACD Long Term."
  (make-strategy :name "Hunted-W1-MACD" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0600 :volume 0.05
    :indicators '((macd 12 26 9))
    :entry '(cross-above macd-line signal-line) :exit '(cross-below macd-line signal-line)))

(def-founder :hunted-w1-rsi "Hunted-W1-RSI-Bull"
  "W1 RSI 50 Bull Market."
  (make-strategy :name "Hunted-W1-RSI-Bull" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0500 :volume 0.05
    :indicators '((rsi 14))
    :entry '(cross-above rsi 50) :exit '(< rsi 40)))

(def-founder :hunted-w1-donchian "Hunted-W1-Donchian-4W"
  "W1 4-Week Rule (Richard Dennis)."
  (make-strategy :name "Hunted-W1-Donchian-4W" :category :breakout :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0800 :volume 0.05
    :indicators '((donchian 4))
    :entry '(> close donchian-upper) :exit '(< close donchian-mid)))

(def-founder :hunted-w1-ema "Hunted-W1-EMA-Trend-V2"
  "W1 EMA 13/26 Trend."
  (make-strategy :name "Hunted-W1-EMA-Trend-V2" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0500 :volume 0.05
    :indicators '((ema 13) (ema 26) (ema 52))
    :entry '(and (cross-above ema-13 ema-26) (> ema-26 ema-52))
    :exit '(or (cross-below ema-13 ema-26) (< ema-26 ema-52))))

(def-founder :hunted-w1-bb "Hunted-W1-BB-Break"
  "W1 Bollinger Breakout."
  (make-strategy :name "Hunted-W1-BB-Break" :category :breakout :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0600 :volume 0.05
    :indicators '((bb 20 2))
    :entry '(> close bb-upper) :exit '(< close bb-middle)))

(def-founder :hunted-w1-stoch "Hunted-W1-Stoch-Long"
  "W1 Stochastic Oversold."
  (make-strategy :name "Hunted-W1-Stoch-Long" :category :reversion :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0400 :volume 0.05
    :indicators '((stoch 14 3 3))
    :entry '(< stoch-k 20) :exit '(> stoch-k 80)))

(def-founder :hunted-w1-pullback "Hunted-W1-Pullback-V2"
  "W1 Trend Pullback."
  (make-strategy :name "Hunted-W1-Pullback-V2" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0500 :volume 0.05
    :indicators '((sma 50) (sma 200) (rsi 14))
    :entry '(and (> close sma-50) (> sma-50 sma-200) (< rsi 40))
    :exit '(or (> rsi 60) (< close sma-50))))

(def-founder :hunted-w1-long-hold "Hunted-W1-Long-Hold"
  "W1 Long Term Hold (SMA 200)."
  (make-strategy :name "Hunted-W1-Long-Hold" :category :trend :timeframe "W1" :generation 0
    :sl 0.0300 :tp 0.1000 :volume 0.05
    :indicators '((sma 200))
    :entry '(cross-above close sma-200) :exit '(cross-below close sma-200)))

(def-founder :hunted-w1-volume "Hunted-W1-Volume-Trend-V2"
  "W1 Volume Trend."
  (make-strategy :name "Hunted-W1-Volume-Trend-V2" :category :trend :timeframe "W1" :generation 0
    :sl 0.0200 :tp 0.0600 :volume 0.05
    :indicators '((sma 20) (ema 50) (rsi 14))
    :entry '(and (> close sma-20) (> sma-20 ema-50) (> rsi 52))
    :exit '(or (< close sma-20) (< rsi 45))))

;;; ----------------------------------------------------------------------------
;;; 9. BATCH RECRUITMENT (Auto-execution on Load)
;;; ----------------------------------------------------------------------------

(defun recruit-hunted-batch ()
  "Recruits the 30 Hunted Strategies (H4, D1, W1) immediately."
  (format t "[HUNT] 🏹 Commencing Batch Recruitment of 30 Strategies...~%")
  
  ;; H4 Batch
  (recruit-founder :hunted-h4-ma-cross)
  (recruit-founder :hunted-h4-rsi-rev)
  (recruit-founder :hunted-h4-bb-bounce)
  (recruit-founder :hunted-h4-donchian)
  (recruit-founder :hunted-h4-macd)
  (recruit-founder :hunted-h4-stoch)
  (recruit-founder :hunted-h4-triple-ema)
  (recruit-founder :hunted-h4-cci)
  (recruit-founder :hunted-h4-atr)
  (recruit-founder :hunted-h4-sar)

  ;; D1 Batch
  (recruit-founder :hunted-d1-golden)
  (recruit-founder :hunted-d1-rsi2)
  (recruit-founder :hunted-d1-turtle)
  (recruit-founder :hunted-d1-turtle-long)
  (recruit-founder :hunted-d1-macd-zero)
  (recruit-founder :hunted-d1-bb-walk)
  (recruit-founder :hunted-d1-ema-ribbon)
  (recruit-founder :hunted-d1-adx)
  (recruit-founder :hunted-d1-williams)
  (recruit-founder :hunted-d1-sar)
  (recruit-founder :hunted-h12-vwapvr-50-150)
  (recruit-founder :hunted-d1-vwapvr-50-220-eurusd)
  (recruit-founder :hunted-d1-vwapvr-80-180-gbpusd)

  ;; W1 Batch
  (recruit-founder :hunted-w1-weinstein)
  (recruit-founder :hunted-w1-macd)
  (recruit-founder :hunted-w1-rsi)
  (recruit-founder :hunted-w1-donchian)
  (recruit-founder :hunted-w1-ema)
  (recruit-founder :hunted-w1-bb)
  (recruit-founder :hunted-w1-stoch)
  (recruit-founder :hunted-w1-pullback)
  (recruit-founder :hunted-w1-long-hold)
  (recruit-founder :hunted-w1-volume)
  
  (format t "[HUNT] ✅ Batch Recruitment Complete. Portfolio Size: ~d~%" (length *strategy-knowledge-base*)))

;; Execution Trigger: Only run if we are in a running system (not just compilation)
;; (when (boundp '*strategy-knowledge-base*)
;;   (recruit-hunted-batch))
