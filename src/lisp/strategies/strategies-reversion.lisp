(in-package :swimmy.school)

(defun get-reversion-strategies ()
  (list
      ;; ===== 3. MEAN REVERSION (逆張り) =====
      
      ;; 3.1 RSI Reversal
      (make-strategy :name "RSI-Oversold-Reversal"
        :indicators '((rsi 14))
        :entry '(cross-above rsi-14 30)
        :exit '(> rsi-14 70)
        :sl 0.30 :tp 0.50 :volume 0.01
        :timeframe 240) ; M5 Reversal
      (make-strategy :name "RSI-Overbought-Reversal"
        :indicators '((rsi 14))
        :entry '(cross-below rsi-14 70)
        :exit '(< rsi-14 30)
        :sl 0.30 :tp 0.50 :volume 0.01
        :timeframe 240) ; M5 Reversal
      
      ;; 3.2 Bollinger Band Reversal
      (make-strategy :name "BB-Lower-Bounce"
        :indicators '((bb 20 2))
        :entry '(cross-above close bb-lower)
        :exit '(>= close bb-middle)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 10080) ; M5 BB Bounce
      (make-strategy :name "BB-Upper-Rejection"
        :indicators '((bb 20 2))
        :entry '(cross-below close bb-upper)
        :exit '(<= close bb-middle)
        :sl 0.25 :tp 0.50 :volume 0.01
        :timeframe 10080) ; M5 BB Reject
      (make-strategy :name "BB-RSI-Reversion-Combo"
        :indicators '((bb 20 2) (rsi 14))
        :entry '(and (< close bb-lower) (< rsi-14 30))
        :exit '(>= close bb-middle)
        :sl 0.30 :tp 0.60 :volume 0.01
        :timeframe 10080) ; M5 BB+RSI
      (make-strategy :name "Extreme-Reversion-BB"
        :indicators '((bb 20 3))
        :entry '(cross-above close bb-lower)
        :exit '(>= close bb-middle)
        :sl 0.40 :tp 0.80 :volume 0.01
        :timeframe 10080) ; M5 Extreme BB
      
      ;; 3.3 Stochastic Reversal
      (make-strategy :name "Stoch-Oversold-Entry"
        :indicators '((stoch 14 3 3))
        :entry '(and (< stoch-k 20) (cross-above stoch-k stoch-d))
        :exit '(> stoch-k 80)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 1440) ; M5 Stoch
      (make-strategy :name "Stoch-Overbought-Entry"
        :indicators '((stoch 14 3 3))
        :entry '(and (> stoch-k 80) (cross-below stoch-k stoch-d))
        :exit '(< stoch-k 20)
        :sl 0.20 :tp 0.40 :volume 0.01
        :timeframe 1440) ; M5 Stoch
      (make-strategy :name "Stoch-Extreme-Dip"
        :indicators '((stoch 14 3 3))
        :entry '(cross-above stoch-k 10)
        :exit '(> stoch-k 90)
        :sl 0.15 :tp 0.30 :volume 0.01
        :timeframe 1440))) ; M5 Stoch
