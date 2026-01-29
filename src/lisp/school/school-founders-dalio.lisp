;;; school-founders-dalio.lisp - The Holy Grail Team
;;; Phase 27: Uncorrelated Return Streams
;;;
;;; "Markets are driven by flows and liquidity, not just price."
;;; - Ray Dalio

(in-package :swimmy.school)

(defun register-dalio-team ()
  (format t "[DALIO] 🌐 Recruiting 'The Uncorrelated' Team...~%")

  ;; 1. Dalio-VIX-Hunter (Volatility Breakout)
  ;; Target: High Volatility Regimes (where Trend strategies fail or are late)
  (let ((vix-hunter 
         (make-strategy 
          :name "Dalio-VIX-Hunter-H4"
          :symbol "USDJPY"
          :timeframe 240 ; H4
          :direction :LONG
          :entry-logic '((> (atr 14) (* (sma (atr 14) 100) 1.5)) ; Volatility Spike > 1.5x Normal
                         (> close (high 20)))              ; Donchian Breakout
          :exit-logic  '((< close (low 10)))
          :indicators  '((atr 14) (sma 100)))))
    (add-to-kb vix-hunter "Dalio-Recruiter" :verify-logic t))

  ;; 2. Dalio-Counter-Punch (Mean Reversion Extreme)
  ;; Target: Range Expansion (Sold into strength)
  ;; Designed to profit when Trend strategies are getting stop-lossed.
  (let ((counter-punch 
         (make-strategy 
          :name "Dalio-Counter-Punch-M30"
          :symbol "EURUSD"
          :timeframe 30
          :direction :SHORT
          :entry-logic '((> rsi 85)          ; Extreme Overbought
                         (> close (bb-upper 20 2.5))) ; Outside 2.5 Sigma
          :exit-logic  '((< rsi 50))
          :indicators  '((rsi 14) (bb 20 2.5)))))
    (add-to-kb counter-punch "Dalio-Recruiter" :verify-logic t))

  ;; 3. Dalio-Time-Bandit (Calendar Anomaly)
  ;; Target: Specific Time execution (uncorrelated to price technicals)
  ;; Checks for "Friday Close" anomaly (Week End profit taking)
  (let ((time-bandit 
         (make-strategy 
          :name "Dalio-Time-Bandit-Friday"
          :symbol "GBPUSD"
          :timeframe 60
          :direction :SHORT
          :entry-logic '((= dow 5)           ; Friday
                         (>= hour 15)        ; NY Open / London Close
                         (> rsi 60))         ; Still bullish, trap late bulls
          :exit-logic  '((>= hour 20))       ; Close before market close
          :indicators  '((rsi 14)))))
    (add-to-kb time-bandit "Dalio-Recruiter" :verify-logic t)))

(export 'register-dalio-team)
