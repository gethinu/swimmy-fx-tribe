(in-package :swimmy.school)

(defun get-trend-strategies ()
  (list
      ;; 1.1 Moving Average Crossover
      (make-strategy :name "Golden-Cross"
        :indicators '((sma 50) (sma 200))
        :entry '(cross-above sma-50 sma-200)
        :exit '(cross-below sma-50 sma-200)
        :sl 0.50 :tp 1.00 :volume 0.01
        :timeframe 5) ; H1 Golden Cross
      (make-strategy :name "Medium-EMA-Cross-20-50"
        :indicators '((ema 20) (ema 50) (rsi 14))
        :entry '(and (cross-above ema-20 ema-50) (> rsi-14 50))
        :exit '(cross-below ema-20 ema-50)
        :sl 0.40 :tp 0.80 :volume 0.02
        :category :trend :timeframe 60) ; M15 for clearer trend
      (make-strategy :name "TF-10-50"
        :indicators '((sma 10) (sma 50))
        :entry '(cross-above sma-10 sma-50)
        :exit '(cross-below sma-10 sma-50)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 60) ; M15 Trend
      
      ;; 1.2 Triple EMA / Perfect Order
      (make-strategy :name "Triple-EMA-Trend-Follow"
        :indicators '((ema 10) (ema 20) (ema 50))
        :entry '(and (> ema-10 ema-20) (> ema-20 ema-50))
        :exit '(cross-below ema-10 ema-20)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 10080) ; M15 Triple EMA
      (make-strategy :name "Perfect-Order-SMA"
        :indicators '((sma 20) (sma 50) (sma 200))
        :entry '(and (> sma-20 sma-50) (> sma-50 sma-200)) ; Perfect Order
        :exit '(< sma-20 sma-50)
        :sl 0.50 :tp 1.00 :volume 0.02
        :category :trend :timeframe 30)      (make-strategy :name "Fibonacci-EMA-Scalp"
        :indicators '((ema 5) (ema 8) (ema 13))
        :entry '(and (> ema-5 ema-8) (> ema-8 ema-13))
        :exit '(cross-below ema-5 ema-8)
        :sl 0.10 :tp 0.20 :volume 0.01
        :timeframe 10080) ; M5 Scalp
      
      ;; 1.3 Trend with RSI Filter
      (make-strategy :name "Trend-Pullback-Entry"
        :indicators '((sma 200) (rsi 14))
        :entry '(and (> close sma-200) (< rsi-14 40))
        :exit '(> rsi-14 60)
        :sl 0.50 :tp 0.80 :volume 0.01
        :timeframe 15) ; M15 Pullback
      (make-strategy :name "Conservative-Trend"
        :indicators '((sma 200) (rsi 7))
        :entry '(and (> close sma-200) (cross-above rsi-7 30))
        :exit '(> rsi-7 70)
        :sl 0.40 :tp 0.80 :volume 0.01
        :timeframe 30) ; M15 Conservative
      
      ;; ===== 2. MOMENTUM (モメンタム) =====
      
      ;; 2.1 MACD Strategies
      (make-strategy :name "MACD-Zero-Cross-Long"
        :indicators '((macd 12 26 9))
        :entry '(cross-above macd-line 0)
        :exit '(cross-below macd-line 0)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 240) ; M15 MACD
      (make-strategy :name "MACD-Signal-Cross"
        :indicators '((macd 12 26 9))
        :entry '(cross-above macd-line signal-line)
        :exit '(cross-below macd-line signal-line)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 240) ; M15 MACD Signal
      (make-strategy :name "MACD-Above-Zero-Cross"
        :indicators '((macd 12 26 9))
        :entry '(and (> macd-line 0) (cross-above macd-line signal-line))
        :exit '(cross-below macd-line signal-line)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 240) ; M15 MACD Trend
      (make-strategy :name "MACD-Expansion"
        :indicators '((macd 12 26 9))
        :entry '(and (> macd-line 0) (> macd-line signal-line))
        :exit '(cross-below macd-line signal-line)
        :sl 0.30 :tp 0.70 :volume 0.01
        :timeframe 240) ; M15 MACD Exp
      
      ;; 2.2 RSI Momentum
      (make-strategy :name "RSI-Momentum-Break"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 50)
        :exit '(cross-below rsi-14 50)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 240) ; M5 RSI Break
      (make-strategy :name "RSI-Bull-Zone"
        :indicators '((rsi 14) (sma 50))
        :entry '(and (> rsi-14 60) (> close sma-50))
        :exit '(cross-below rsi-14 50)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 240))) ; M5 RSI Zone
