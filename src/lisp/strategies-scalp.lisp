(in-package :swimmy.school)

(defun get-scalp-strategies ()
  (list
      ;; ===== 5. HYBRID/ADVANCED (複合戦略) =====
      
      ;; 5.1 Multi-Indicator Confluence
      (make-strategy :name "Puria-Method-Proxy"
        :indicators '((ema 5) (sma 75) (macd 12 26 9))
        :entry '(and (cross-above ema-5 sma-75) (> macd-line 0))
        :exit '(cross-below ema-5 sma-75)
        :sl 0.30 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 Puria
      (make-strategy :name "Holy-Grail-Proxy"
        :indicators '((ema 20) (adx 14))
        :entry '(and (> adx-14 30) (cross-above close ema-20)) ; Pullback entry? No, breakout.
        :exit '(< close ema-20)
        :sl 0.30 :tp 0.60 :volume 0.02
        :category :trend :timeframe 5) ; Proven on M5
      (make-strategy :name "Triple-Screen-Proxy"
        :indicators '((ema 50) (rsi 14) (macd 12 26 9))
        :entry '(and (> close ema-50) (> macd-line 0) (< rsi-14 50))
        :exit '(> rsi-14 70)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 60) ; H1 Triple Screen
      (make-strategy :name "Trend-Scalp-1M"
        :indicators '((ema 9) (ema 21) (rsi 14))
        :entry '(and (cross-above ema-9 ema-21) (> rsi-14 50))
        :exit '(cross-below ema-9 ema-21)
        :sl 0.10 :tp 0.20 :volume 0.01
        :timeframe 5) ; M5 Scalp (was M1)
      (make-strategy :name "MACD-Zero-Reject"
        :indicators '((macd 12 26 9) (ema 50))
        :entry '(and (> close ema-50) (cross-above macd-line 0))
        :exit '(cross-below macd-line signal-line)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 15) ; M15 Reject
      (make-strategy :name "Crossover-Plus-MACD"
        :indicators '((ema 10) (ema 20) (macd 12 26 9))
        :entry '(and (cross-above ema-10 ema-20) (> macd-line signal-line))
        :exit '(cross-below ema-10 ema-20)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 Cross+MACD
      (make-strategy :name "Simple-Momentum-Sync"
        :indicators '((ema 50) (rsi 14))
        :entry '(and (> close ema-50) (> rsi-14 55))
        :exit '(< rsi-14 45)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 15) ; M15 Sync
      
      ;; 5.2 RSI-Stochastic Combo
      (make-strategy :name "RSI-Stoch-Reversal"
        :indicators '((rsi 14) (stoch 14 3 3))
        :entry '(and (< rsi-14 30) (< stoch-k 20))
        :exit '(or (> rsi-14 70) (> stoch-k 80))
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 RSI+Stoch
      (make-strategy :name "Aggressive-Reversal"
        :indicators '((bb 20 2) (stoch 5 3 3))
        :entry '(and (< close bb-lower) (cross-above stoch-k 20))
        :exit '(>= close bb-middle)
        :sl 0.20 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 for reversal stability
      
      ;; 5.3 Double Bollinger
      (make-strategy :name "Double-Bollinger-Trend"
        :indicators '((bb 20 1) (bb 20 2))
        :entry '(and (> close bb-upper-1) (< close bb-upper-2))
        :exit '(< close bb-middle-1)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 Double BB
      
      ;; ===== 6. SCALPING (スキャルピング) =====
      
      (make-strategy :name "MA-Ribbon-Scalp"
        :indicators '((ema 5) (ema 13) (rsi 14))
        :entry '(and (> ema-5 ema-13) (> rsi-14 55)) ; Trend + Momentum
        :exit '(cross-below ema-5 ema-13)
        :sl 0.15 :tp 0.25 :volume 0.01
        :timeframe 5) ; M5 Scalping
      (make-strategy :name "Stoch-Momentum-Cross"
        :indicators '((stoch 14 3 3))
        :entry '(and (cross-above stoch-k stoch-d) (> stoch-k 50))
        :exit '(cross-below stoch-k stoch-d)
        :sl 0.20 :tp 0.30 :volume 0.01
        :category :scalp :timeframe 5) ; M5 Stoch Cross
      (make-strategy :name "Stoch-Pop"
        :indicators '((stoch 14 3 3))
        :entry '(cross-above stoch-k 50)
        :exit '(cross-below stoch-k 50)
        :sl 0.15 :tp 0.25 :volume 0.01
        :category :scalp :timeframe 5) ; M5 Stoch Pop
      (make-strategy :name "RSI-Volatility-Break"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 60)
        :exit '(cross-below rsi-14 40)
        :sl 0.25 :tp 0.50 :volume 0.01
        :category :breakout :timeframe 5) ; M5 Volatility
      (make-strategy :name "Pullback-Breakout"
        :indicators '((ema 20) (rsi 14))
        :entry '(and (cross-above close ema-20) (> rsi-14 50))
        :exit '(cross-below close ema-20)
        :sl 0.30 :tp 0.60 :volume 0.01
        :category :breakout :timeframe 5) ; M5 Pullback
      
      ;; ===== 7. ADDITIONAL FROM GEMINI =====
      
      (make-strategy :name "Bladerunner"
        :indicators '((ema 20))
        :entry '(and (> close ema-20) (cross-above low ema-20))
        :exit '(< close ema-20)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 5) ; M5 Bladerunner
      (make-strategy :name "Sweet-Chariot-SMA-40"
        :indicators '((sma 40))
        :entry '(cross-above close sma-40)
        :exit '(cross-below close sma-40)
        :sl 0.30 :tp 0.90 :volume 0.01
        :timeframe 15) ; M15 Sweet Chariot
      (make-strategy :name "CCI-Trend-Breakout"
        :indicators '((cci 14))
        :entry '(cross-above cci-14 100)
        :exit '(cross-below cci-14 0)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 CCI Breakout
      
      ;; ===== 6. CONTEXTUAL & STRUCTURAL =====
      
      ;; 6.1 Gotobi (Time-Based) Strategy
      ;; Entry: Gotobi day AND 8:00 <= Hour < 10:00 AND Price > SMA-5 (Short-term trend OK)
      ;; Exit: Hour >= 10:00 (Nakane passed)
      (make-strategy :name "T-Nakane-Gotobi"
        :indicators '((sma 5))
        :entry '(and gotobi-p 
                     (>= hour 9) (< hour 10) ; Start at 9:00 (Tokyo Open) to avoid spread
                     (< minute 55)   ; Do not enter just before 9:55
                     (> close sma-5)) ; Basic trend filter
        :exit '(or (>= hour 10)      ; Time exit
                   (< close sma-5))  ; Trend broken
        :sl 0.20 :tp 0.25 :volume 0.05 ; Realistic TP for Nakane
        :category :time-based :timeframe 5) ; M5 Nakane
        
      ;; ===== 8. MTF VERIFICATION STRATEGIES =====
      
      ;; 8.1 Control Strategy (M5 - Expert Panel Adjusted)
      (make-strategy :name "SCALP-BASE"
        :indicators '((ema 5) (ema 12))
        :entry '(cross-above ema-5 ema-12)
        :exit '(cross-below ema-5 ema-12)
        :sl 0.10 :tp 0.15 :volume 0.01
        :category :scalp :timeframe 5)

      ;; 8.2 MTF Filtered Strategy (M5 with H1 Trend - Expert Panel Adjusted)
      (with-trend-filter ("H1" "PRICE_ABOVE_SMA" 50)
        (make-strategy :name "SCALP-MTF-MACRO"
          :indicators '((ema 5) (ema 12))
          :entry '(cross-above ema-5 ema-12)
          :exit '(cross-below ema-5 ema-12)
          :sl 0.10 :tp 0.15 :volume 0.01
          :category :scalp :timeframe 5))
          
      ;; 8.3 Breakout MTF (Bollinger Walk with H1 Trend)
      (with-trend-filter ("H1" "PRICE_ABOVE_SMA" 50)
        (make-strategy :name "BB-Walk-Trend"
          :indicators '((bb 20 1))
          :entry '(> close bb-upper)
          :exit '(< close bb-middle)
          :sl 0.30 :tp 1.00 :volume 0.01
          :category :breakout :timeframe 15))
          
      ;; 8.4 Reversion MTF (RSI Dip Buy in H1 Uptrend)
      (with-trend-filter ("H1" "PRICE_ABOVE_SMA" 50)
        (make-strategy :name "RSI-Dip-Trend"
          :indicators '((rsi 14))
          :entry '(cross-above rsi-14 30)
          :exit '(> rsi-14 70)
          :sl 0.30 :tp 0.50 :volume 0.01
          :category :reversion :timeframe 5))
          
      ;; 8.5 Trend MTF (MACD Cross aligned with H1 Trend)
      (with-trend-filter ("H1" "PRICE_ABOVE_SMA" 50)
        (make-strategy :name "MACD-Trend-Trend"
          :indicators '((macd 12 26 9))
          :entry '(cross-above macd-line signal-line)
          :exit '(cross-below macd-line signal-line)
          :sl 0.50 :tp 1.00 :volume 0.01
          :category :trend :timeframe 15))

      ;; 8.6 Advanced Reversion (M5: RSI + BB Confluence)
      (with-trend-filter ("H1" "PRICE_ABOVE_SMA" 50)
        (make-strategy :name "Advanced-M5-Reversion"
          :indicators '((rsi 14) (bb 20 2))
          :entry '(and (< rsi-14 30) (< close bb-lower))
          :exit '(or (> rsi-14 50) (> close bb-middle))
          :sl 0.20 :tp 0.40 :volume 0.01
          :category :reversion :timeframe 5))))
