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

      (make-strategy :name "Legend-RSI-Reversion-V1"
        :indicators '((rsi 2)) ;; 2-period RSI (Larry Connors)
        :entry '(< rsi-2 5) ;; Extreme oversold
        :exit '(> close sma-5) ;; Exit on break above 5-SMA
        :sl 0.088 :tp 0.091 :volume 0.05
        :category :reversion :timeframe 1440)
        
      (make-strategy :name "Legend-London-Breakout-V1"
        :indicators '((session-high 8 16) (session-low 8 16)) ;; 8:00-16:00 (London Open)
        :entry '(break-above session-high)
        :exit '(time-exit 24)
        :sl 0.10 :tp 0.20 :volume 0.10
        :category :breakout :timeframe 60)

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
