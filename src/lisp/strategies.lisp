;; strategies.lisp - Historical Strategy Knowledge Base
;; 歴史的に有効な戦略パターンのナレッジベース（100+種類）

(in-package :swimmy.school)
;; Sources: NotebookLM, Gemini research

(defparameter *strategy-knowledge-base* nil)

(defun init-knowledge-base ()
  "Initialize with historically proven strategies"
  (setf *strategy-knowledge-base*
    (list
      ;; ===== 1. TREND FOLLOWING (トレンドフォロー) =====
      
      ;; 1.1 Moving Average Crossover
      (make-strategy :name "Golden-Cross"
        :indicators '((sma 50) (sma 200))
        :entry '(cross-above sma-50 sma-200)
        :exit '(cross-below sma-50 sma-200)
        :sl 0.50 :tp 1.00 :volume 0.01
        :timeframe 60) ; H1 Golden Cross
      ;; (make-strategy :name "Death-Cross-50-200"
      ;;   :indicators '((sma 50) (sma 200))
      ;;   :entry '(cross-below sma-50 sma-200)
      ;;   :exit '(cross-above sma-50 sma-200)
      ;;   :sl 0.50 :tp 1.00 :volume 0.01)
      ;; (make-strategy :name "Fast-EMA-Cross-9-21"
      ;;   :indicators '((ema 9) (ema 21))
      ;;   :entry '(cross-above ema-9 ema-21)
      ;;   :exit '(cross-below ema-9 ema-21)
      ;;   :sl 0.20 :tp 0.40 :volume 0.01)
      (make-strategy :name "Medium-EMA-Cross-20-50"
        :indicators '((ema 20) (ema 50) (rsi 14))
        :entry '(and (cross-above ema-20 ema-50) (> rsi-14 50))
        :exit '(cross-below ema-20 ema-50)
        :sl 0.40 :tp 0.80 :volume 0.02
        :category :trend :timeframe 15) ; M15 for clearer trend
      ;; (make-strategy :name "Silver-Cross-20-100"
      ;;   :indicators '((sma 20) (sma 100))
      ;;   :entry '(cross-above sma-20 sma-100)
      ;;   :exit '(cross-below sma-20 sma-100)
      ;;   :sl 0.40 :tp 0.80 :volume 0.01)
      ;; (make-strategy :name "Scalp-Cross-5-12"
      ;;   :indicators '((ema 5) (ema 12))
      ;;   :entry '(cross-above ema-5 ema-12)
      ;;   :exit '(cross-below ema-5 ema-12)
      ;;   :sl 0.10 :tp 0.15 :volume 0.01)
      ;; (make-strategy :name "TF-5-20"
      ;;   :indicators '((sma 5) (sma 20))
      ;;   :entry '(cross-above sma-5 sma-20)
      ;;   :exit '(cross-below sma-5 sma-20)
      ;;   :sl 0.15 :tp 0.30 :volume 0.01)
      (make-strategy :name "TF-10-50"
        :indicators '((sma 10) (sma 50))
        :entry '(cross-above sma-10 sma-50)
        :exit '(cross-below sma-10 sma-50)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 15) ; M15 Trend
      
      ;; 1.2 Triple EMA / Perfect Order
      (make-strategy :name "Triple-EMA-Trend-Follow"
        :indicators '((ema 10) (ema 20) (ema 50))
        :entry '(and (> ema-10 ema-20) (> ema-20 ema-50))
        :exit '(cross-below ema-10 ema-20)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 Triple EMA
      (make-strategy :name "Perfect-Order-SMA"
        :indicators '((sma 20) (sma 50) (sma 200))
        :entry '(and (> sma-20 sma-50) (> sma-50 sma-200)) ; Perfect Order
        :exit '(< sma-20 sma-50)
        :sl 0.50 :tp 1.00 :volume 0.02
        :category :trend :timeframe 60)      (make-strategy :name "Fibonacci-EMA-Scalp"
        :indicators '((ema 5) (ema 8) (ema 13))
        :entry '(and (> ema-5 ema-8) (> ema-8 ema-13))
        :exit '(cross-below ema-5 ema-8)
        :sl 0.10 :tp 0.20 :volume 0.01
        :timeframe 5) ; M5 Scalp
      
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
        :timeframe 15) ; M15 Conservative
      
      ;; ===== 2. MOMENTUM (モメンタム) =====
      
      ;; 2.1 MACD Strategies
      (make-strategy :name "MACD-Zero-Cross-Long"
        :indicators '((macd 12 26 9))
        :entry '(cross-above macd-line 0)
        :exit '(cross-below macd-line 0)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 15) ; M15 MACD
      (make-strategy :name "MACD-Signal-Cross"
        :indicators '((macd 12 26 9))
        :entry '(cross-above macd-line signal-line)
        :exit '(cross-below macd-line signal-line)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 MACD Signal
      (make-strategy :name "MACD-Above-Zero-Cross"
        :indicators '((macd 12 26 9))
        :entry '(and (> macd-line 0) (cross-above macd-line signal-line))
        :exit '(cross-below macd-line signal-line)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 15) ; M15 MACD Trend
      (make-strategy :name "MACD-Expansion"
        :indicators '((macd 12 26 9))
        :entry '(and (> macd-line 0) (> macd-line signal-line))
        :exit '(cross-below macd-line signal-line)
        :sl 0.30 :tp 0.70 :volume 0.01
        :timeframe 15) ; M15 MACD Exp
      
      ;; 2.2 RSI Momentum
      (make-strategy :name "RSI-Momentum-Break"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 50)
        :exit '(cross-below rsi-14 50)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 5) ; M5 RSI Break
      (make-strategy :name "RSI-Bull-Zone"
        :indicators '((rsi 14) (sma 50))
        :entry '(and (> rsi-14 60) (> close sma-50))
        :exit '(cross-below rsi-14 50)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 5) ; M5 RSI Zone
      ;; (make-strategy :name "RSI-Fast-Break"
      ;;   :indicators '((rsi 5))
      ;;   :entry '(cross-above rsi-5 70)
      ;;   :exit '(cross-below rsi-5 30)
      ;;   :sl 0.15 :tp 0.30 :volume 0.01)
      ;; (make-strategy :name "RSI-Trend-Shift"
      ;;   :indicators '((rsi 14))
      ;;   :entry '(cross-above rsi-14 50)
      ;;   :exit '(cross-below rsi-14 40)
      ;;   :sl 0.20 :tp 0.40 :volume 0.01)
      
      ;; 2.3 Combined Momentum
      ;; (make-strategy :name "MACD-RSI-Confluence"
      ;;   :indicators '((macd 12 26 9) (rsi 14))
      ;;   :entry '(and (> macd-line signal-line) (> rsi-14 50))
      ;;   :exit '(cross-below macd-line signal-line)
      ;;   :sl 0.30 :tp 0.60 :volume 0.01)
      ;; (make-strategy :name "Elder-Impulse-Simulated"
      ;;   :indicators '((ema 13) (macd 12 26 9))
      ;;   :entry '(and (> close ema-13) (> macd-line signal-line))
      ;;   :exit '(cross-below close ema-13)
      ;;   :sl 0.25 :tp 0.50 :volume 0.01)
      ;; (make-strategy :name "Momentum-Burst"
      ;;   :indicators '((ema 9) (rsi 7))
      ;;   :entry '(and (> close ema-9) (> rsi-7 70))
      ;;   :exit '(cross-below rsi-7 60)
      ;;   :sl 0.15 :tp 0.45 :volume 0.01)
      
      ;; ===== 3. MEAN REVERSION (逆張り) =====
      
      ;; 3.1 RSI Reversal
      (make-strategy :name "RSI-Oversold-Reversal"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 30)
        :exit '(> rsi-14 70)
        :sl 0.30 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 Reversal
      (make-strategy :name "RSI-Overbought-Reversal"
        :indicators '((rsi 14))
        :entry '(cross-below rsi-14 70)
        :exit '(< rsi-14 30)
        :sl 0.30 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 Reversal
      ;; (make-strategy :name "RSI-Short-Reversion"
      ;;   :indicators '((rsi 5))
      ;;   :entry '(cross-above rsi-5 20)
      ;;   :exit '(> rsi-5 80)
      ;;   :sl 0.15 :tp 0.20 :volume 0.01)
      ;; (make-strategy :name "RSI-2-Period-Connors"
      ;;   :indicators '((rsi 2) (sma 200))
      ;;   :entry '(and (> close sma-200) (< rsi-2 10))
      ;;   :exit '(> rsi-2 90)
      ;;   :sl 0.40 :tp 0.80 :volume 0.01)
      
      ;; 3.2 Bollinger Band Reversal
      (make-strategy :name "BB-Lower-Bounce"
        :indicators '((bb 20 2))
        :entry '(cross-above close bb-lower)
        :exit '(>= close bb-middle)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 BB Bounce
      (make-strategy :name "BB-Upper-Rejection"
        :indicators '((bb 20 2))
        :entry '(cross-below close bb-upper)
        :exit '(<= close bb-middle)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 BB Reject
      (make-strategy :name "BB-RSI-Reversion-Combo"
        :indicators '((bb 20 2) (rsi 14))
        :entry '(and (< close bb-lower) (< rsi-14 30))
        :exit '(>= close bb-middle)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 5) ; M5 BB+RSI
      (make-strategy :name "Extreme-Reversion-BB"
        :indicators '((bb 20 3))
        :entry '(cross-above close bb-lower)
        :exit '(>= close bb-middle)
        :sl 0.40 :tp 0.80 :volume 0.01
        :timeframe 5) ; M5 Extreme BB
      
      ;; 3.3 Stochastic Reversal
      (make-strategy :name "Stoch-Oversold-Entry"
        :indicators '((stoch 14 3 3))
        :entry '(and (< stoch-k 20) (cross-above stoch-k stoch-d))
        :exit '(> stoch-k 80)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 5) ; M5 Stoch
      (make-strategy :name "Stoch-Overbought-Entry"
        :indicators '((stoch 14 3 3))
        :entry '(and (> stoch-k 80) (cross-below stoch-k stoch-d))
        :exit '(< stoch-k 20)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 5) ; M5 Stoch
      (make-strategy :name "Stoch-Extreme-Dip"
        :indicators '((stoch 14 3 3))
        :entry '(cross-above stoch-k 10)
        :exit '(> stoch-k 90)
        :sl 0.15 :tp 0.30 :volume 0.01
        :timeframe 5) ; M5 Stoch
      
      ;; ===== 4. BREAKOUT (ブレイクアウト) =====
      
      ;; 4.1 Bollinger Breakout
      ;; (make-strategy :name "BB-Breakout-Upper"
      ;;   :indicators '((bb 20 2))
      ;;   :entry '(cross-above close bb-upper)
      ;;   :exit '(cross-below close bb-middle)
      ;;   :sl 0.30 :tp 0.90 :volume 0.01)
      ;; (make-strategy :name "BB-Breakout-Lower"
      ;;   :indicators '((bb 20 2))
      ;;   :entry '(cross-below close bb-lower)
      ;;   :exit '(cross-above close bb-middle)
      ;;   :sl 0.30 :tp 0.90 :volume 0.01)
      ;; (make-strategy :name "BB-Squeeze-Expansion"
      ;;   :indicators '((bb 20 2) (sma 20))
      ;;   :entry '(and (cross-above close bb-upper) (> close sma-20))
      ;;   :exit '(cross-below close bb-upper)
      ;;   :sl 0.20 :tp 0.60 :volume 0.01)
      (make-strategy :name "Bollinger-Band-Walk"
        :indicators '((bb 20 1))
        :entry '(> close bb-upper)
        :exit '(< close bb-middle)
        :sl 0.30 :tp 1.00 :volume 0.01
        :timeframe 15) ; M15 Walk
      
      ;; 4.2 ATR/Volatility Breakout
      ;; (make-strategy :name "Volatility-Trend-Follow"
      ;;   :indicators '((sma 20) (atr 14))
      ;;   :entry '(and (> close sma-20) (> atr-14 0.002))
      ;;   :exit '(cross-below close sma-20)
      ;;   :sl 0.40 :tp 0.80 :volume 0.01)
      ;; (make-strategy :name "ATR-Confirmed-Breakout"
      ;;   :indicators '((sma 50) (atr 14))
      ;;   :entry '(and (cross-above close sma-50) (> atr-14 0.001))
      ;;   :exit '(cross-below close sma-50)
      ;;   :sl 0.30 :tp 0.60 :volume 0.01)
      ;; (make-strategy :name "Session-Breakout-Proxy"
      ;;   :indicators '((sma 5) (atr 14))
      ;;   :entry '(and (cross-above close sma-5) (> atr-14 0.0015))
      ;;   :exit '(cross-below close sma-5)
      ;;   :sl 0.20 :tp 0.40 :volume 0.01)
      ;; (make-strategy :name "Low-Vol-Breakout"
      ;;   :indicators '((sma 20) (atr 14))
      ;;   :entry '(and (cross-above close sma-20) (> atr-14 0.0005))
      ;;   :exit '(cross-below close sma-20)
      ;;   :sl 0.20 :tp 0.60 :volume 0.01)
      
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
        :timeframe 5) ; M5 Stoch Cross
      (make-strategy :name "Stoch-Pop"
        :indicators '((stoch 14 3 3))
        :entry '(cross-above stoch-k 50)
        :exit '(cross-below stoch-k 50)
        :sl 0.15 :tp 0.25 :volume 0.01
        :timeframe 5) ; M5 Stoch Pop
      (make-strategy :name "RSI-Volatility-Break"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 60)
        :exit '(cross-below rsi-14 40)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 5) ; M5 Volatility
      (make-strategy :name "Pullback-Breakout"
        :indicators '((ema 20) (rsi 14))
        :entry '(and (cross-above close ema-20) (> rsi-14 50))
        :exit '(cross-below close ema-20)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 5) ; M5 Pullback
      
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
    ))  

  (format t "[L] 📚 Knowledge base loaded: ~d strategies~%" 
          (length *strategy-knowledge-base*)))

;; ===== Sharpe フィルター;; Thresholds
(defparameter *min-sharpe-threshold* 0.0 "Minimum Sharpe to be adopted/kept (Taleb's Rule: Block negative EV)")
(defparameter *min-win-rate-threshold* 0.4 "Minimum Win Rate")
(defparameter *approved-strategies* nil)

(defun filter-by-sharpe (strategies)
  "Filter strategies with Sharpe > threshold"
  (let ((approved (remove-if-not 
                    (lambda (s) (and (strategy-sharpe s) (> (strategy-sharpe s) *min-sharpe-threshold*)))
                    strategies)))
    (format t "[L] 🔍 Filtered: ~d/~d passed Sharpe > ~,1f~%"
            (length approved) (length strategies) *min-sharpe-threshold*)
    approved))

(defun batch-backtest-knowledge ()
  "Backtest all knowledge base strategies"
  (setf *backtest-results-buffer* nil)
  (setf *expected-backtest-count* (length *strategy-knowledge-base*))
  (format t "[L] 🧪 Batch testing ~d strategies...~%" *expected-backtest-count*)
  (dolist (strat *strategy-knowledge-base*)
    (when (and *candle-history* (> (length *candle-history*) 100))
      (request-backtest strat))))

(defun adopt-proven-strategies ()
  "Adopt only strategies that passed Sharpe filter"
  (setf *approved-strategies* (filter-by-sharpe *strategy-knowledge-base*))
  (when *approved-strategies*
    (format t "[L] ✅ Adopted ~d proven strategies~%" (length *approved-strategies*))
    (dolist (s *approved-strategies*)
      (unless (find (strategy-name s) *evolved-strategies* :key #'strategy-name :test #'string=)
        (push s *evolved-strategies*))))
  (when *evolved-strategies*
    (setf *evolved-strategies* 
          (sort *evolved-strategies* #'> 
                :key (lambda (s) (or (strategy-sharpe s) 0))))))

;; Auto-initialize
(init-knowledge-base)

;;; ==========================================
;;; V7.9++: INDICATOR TYPE INFERENCE (Sharpe=-3.75 Bug Fix)
;;; Infer indicator_type from strategy indicators for correct backtesting
;;; ==========================================

(defun infer-indicator-type (strategy)
  "Infer the primary indicator type from strategy indicators.
   This fixes the Sharpe=-3.75 bug where all strategies defaulted to SMA."
  (let* ((indicators (strategy-indicators strategy))
         (first-indicator (first indicators))
         (indicator-name (when first-indicator 
                           (string-downcase (symbol-name (first first-indicator))))))
    (cond
      ;; MACD strategies
      ((and indicator-name (search "macd" indicator-name)) "macd")
      ;; RSI strategies (including elder, momentum)
      ((and indicator-name (search "rsi" indicator-name)) "rsi")
      ;; Stochastic strategies
      ((and indicator-name (search "stoch" indicator-name)) "stoch")
      ;; Bollinger Band strategies
      ((and indicator-name (search "bb" indicator-name)) "bb")
      ;; EMA strategies
      ((and indicator-name (search "ema" indicator-name)) "ema")
      ;; Default to SMA
      (t "sma"))))

(defun apply-indicator-types ()
  "Apply inferred indicator types to all strategies in knowledge base."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (strat *strategy-knowledge-base*)
      (let ((ind-type (infer-indicator-type strat)))
        (setf (strategy-indicator-type strat) ind-type)
        (incf (gethash ind-type counts 0))))
    ;; Log the distribution
    (format t "[STRATEGIES] Indicator types assigned:~%")
    (maphash (lambda (k v) (format t "  ~a: ~a strategies~%" k v)) counts)))

;; Apply indicator types after initialization
(apply-indicator-types)

(format t "[STRATEGIES] ~d strategies loaded from Knowledge Base~%" 
        (length *strategy-knowledge-base*))

;;; ==========================================
;;; BENCH SYSTEM (V5.1 Restored)
;;; ==========================================

(defparameter *benched-strategies* (make-hash-table :test 'equal))

(defun strategy-benched-p (name)
  "Check if strategy is benched"
  (gethash name *benched-strategies*))

(defun bench-strategy (name reason)
  "Bench a strategy with reason"
  (setf (gethash name *benched-strategies*) reason)
  (format t "[L] 🚫 BENCHED: ~a (~a)~%" name reason))

(defun unbench-all ()
  "Unbench all strategies"
  (clrhash *benched-strategies*)
  (format t "[L] ♻️  All strategies unbenched for new week~%"))

(defun should-weekly-unbench-p ()
  "Check if it is Monday morning (before 9am) to reset bench"
  (multiple-value-bind (s m h d mo y dow) (decode-universal-time (get-universal-time))
    (declare (ignore s m d mo y))
    (and (= dow 0) (< h 9))))

(defun weekly-unbench-all ()
  "Weekly unbench wrapper"
  (unbench-all))

(defun current-trading-session ()
  "Determine current market session (JST based approximation)"
  (let ((h (nth 2 (multiple-value-list (decode-universal-time (get-universal-time))))))
    (cond 
      ((and (>= h 9) (< h 15)) :asian)
      ((and (>= h 16) (< h 21)) :london)
      ((or (>= h 22) (< h 6)) :ny)
      (t :mixed))))

;;; ==========================================
;;; STRATEGY PERFORMANCE EVALUATION
;;; ==========================================

(defun evaluate-strategy-performance (strat sharpe trades win-rate)
  "Adjust strategy parameters based on backtest performance"
  (let ((name (strategy-name strat)))
    (when (> trades 10)
      ;; CRITICAL FAILURE CHECK (Bench System)
      (when (and (> trades 30) (or (< sharpe -1.5) (< win-rate 30)))
        (bench-strategy name (format nil "Critical Failure: S=~,2f WR=~,0f%" sharpe win-rate))
        (return-from evaluate-strategy-performance))
      
      (cond
        ;; Poor performance: tighten SL, reduce volume
        ((or (< sharpe 0) (< win-rate 40))
         (when (strategy-sl strat)
           (setf (strategy-sl strat) (* 0.9 (strategy-sl strat))))
         (when (strategy-volume strat)
           (setf (strategy-volume strat) (max 0.01 (* 0.8 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📉 ~a: Tightening params (poor perf)~%" name))
        ;; Good performance: widen TP, increase volume (Aggressive V7.0)
        ((and (> sharpe 1.0) (> win-rate 55))
         (when (strategy-tp strat)
           (setf (strategy-tp strat) (* 1.1 (strategy-tp strat))))
         (when (strategy-volume strat)
           ;; Scale faster: 1.2x -> 1.5x, Cap 0.1 -> 0.3
           (setf (strategy-volume strat) (min 0.3 (* 1.5 (strategy-volume strat)))))
         (format t "[L] ⚙️ 📈 🚀 ~a: Aggressive Expansion (Good Perf)~%" name))
        ;; Average: adjust SL/TP ratio for better risk/reward
        ((and (> sharpe 0.5) (> win-rate 45))
         (when (and (strategy-sl strat) (strategy-tp strat))
           (let ((rr (/ (strategy-tp strat) (max 0.01 (strategy-sl strat)))))
             (when (< rr 2.0)
               (setf (strategy-tp strat) (* 1.05 (strategy-tp strat)))
               (format t "[L] ⚙️ 🎯 ~a: Improving R:R ratio~%" name)))))))))

