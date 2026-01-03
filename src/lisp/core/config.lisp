(in-package :cl-user)

;; core/config.lisp - Extracted Configuration (Naval's Strangler Fig)
;; V41.4: First step of brain.lisp refactoring

;;; ==========================================
;;; SYSTEM CONFIGURATION
;;; ==========================================

;; Supported trading symbols
(defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD")
  "Active trading symbols for multi-currency support")

;;; ==========================================
;;; DISCORD WEBHOOKS
;;; ==========================================

(defparameter *discord-webhook-url* (uiop:getenv "SWIMMY_DISCORD_WEBHOOK")
  "Main Discord webhook for notifications")

(defparameter *discord-emergency-url* (uiop:getenv "SWIMMY_DISCORD_EMERGENCY")
  "Emergency Discord webhook for High Council alerts")

;; Aliases for multi-channel support (unify variables)
(defparameter *discord-emergency-webhook* *discord-emergency-url*)
(defparameter *discord-daily-webhook* (uiop:getenv "SWIMMY_DISCORD_DAILY"))
(defparameter *discord-weekly-webhook* (uiop:getenv "SWIMMY_DISCORD_WEEKLY"))

;; Legacy/Specific webhooks (hardcoded preservation)
(defparameter *backtest-webhook-url* 
  "https://discord.com/api/webhooks/1455351646962979000/p9cWLthwfP8gB1TgvukeJixren_kgJvjjIq-oVQ-doAsX_C4chGBQyf05Eh_iDmLu1Dy")
(defparameter *status-webhook-url* 
  "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW")
(defparameter *alerts-webhook-url*
  "https://discord.com/api/webhooks/1455549266301812849/r5Rv8rQrwgVsppGS0qIDJPNyz2KphVIzwshu6vTPABC-E6OSFrS89tZ9xAGQJEzmRqBH")

;; Symbol-specific webhooks
(defparameter *symbol-webhooks* (make-hash-table :test 'equal)
  "Per-symbol Discord webhooks")

;; Initialize symbol webhooks
(setf (gethash "USDJPY" *symbol-webhooks*) 
      (or (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_USDJPY") (uiop:getenv "SWIMMY_DISCORD_WEBHOOK")))
(setf (gethash "EURUSD" *symbol-webhooks*) 
      (or (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_EURUSD") (uiop:getenv "SWIMMY_DISCORD_WEBHOOK")))
(setf (gethash "GBPUSD" *symbol-webhooks*) 
      (or (uiop:getenv "SWIMMY_DISCORD_WEBHOOK_GBPUSD") (uiop:getenv "SWIMMY_DISCORD_WEBHOOK")))

;;; ==========================================
;;; STATE & BUFFERS
;;; ==========================================

;; Candle State
(defparameter *candle-histories* (make-hash-table :test 'equal))  ; symbol -> history
(defparameter *current-candles* (make-hash-table :test 'equal))   ; symbol -> candle
(defparameter *current-minutes* (make-hash-table :test 'equal))   ; symbol -> minute

;; Legacy single-currency compatibility
(defparameter *candle-history* nil)
(defparameter *current-candle* nil)
(defparameter *current-minute* -1)

;; Backtest Buffering
(defparameter *backtest-results-buffer* nil)
(defparameter *expected-backtest-count* 0)
(defparameter *backtest-start-time* 0)

;; Status Reporting State
(defparameter *last-status-notification-time* (make-hash-table :test 'equal))
(defparameter *status-notification-interval* 3600) ; Default 1 hour
(defparameter *tribe-status* (make-hash-table :test 'eq))

;;; ==========================================
;;; GLOBAL PLACEHOLDERS
;;; ==========================================
(defparameter *tribal-dialect* (make-hash-table :test 'equal))
(defparameter *reputation-scores* (make-hash-table :test 'equal))
(defparameter *genome* nil)
(defparameter *arms* nil)
(defparameter *memory* nil)
(defparameter *portfolio-indices* nil)
(defparameter *arm-states* (make-hash-table))

;;; ==========================================
;;; API KEYS
;;; ==========================================

(defparameter *gemini-api-key* (uiop:getenv "SWIMMY_GEMINI_API_KEY")
  "Gemini AI API key for strategy generation")

;;; ==========================================
;;; RISK MANAGEMENT
;;; ==========================================

(defparameter *base-lot-size* 0.01
  "Default lot size for trades")

(defparameter *max-dd-percent* 20
  "Maximum drawdown percentage before warning")

(defparameter *daily-loss-limit* -500
  "Daily loss limit in yen")

(defparameter *max-streak-losses* 3
  "Max consecutive losses before caution")

(defparameter *max-portfolio-size* 3
  "Maximum concurrent positions")

(defparameter *peak-equity* 0.0)
(defparameter *max-drawdown* 0.0)
(defparameter *current-drawdown* 0.0)
(defparameter *current-equity* 0.0)

;;; ==========================================
;;; TRADING PARAMETERS
;;; ==========================================

(defparameter *monthly-goal* 100000
  "Monthly profit target in yen")

(defparameter *trading-days-in-month* 22
  "Approximate trading days per month")

(defparameter *risk-tolerance* :moderate
  "Risk level: :conservative, :moderate, :aggressive")

(defparameter *daily-risk-limit* nil)
(defparameter *daily-pnl* 0.0)
(defparameter *total-trades* 0)
(defparameter *benched-arms* nil)

(defparameter *last-guardian-heartbeat* 0)
(defparameter *all-time-win-rate* 50.0)
(defparameter *portfolio-sharpe* 0.0)

;;; ==========================================
;;; VOLATILITY & REGIME (Soros)
;;; ==========================================
(defparameter *symbol-volatility-states* (make-hash-table :test 'equal))
(defparameter *current-volatility-state* :normal)
(defparameter *market-regime* :ranging)

;;; ==========================================
;;; EXECUTION & INFRASTRUCTURE
;;; ==========================================
(defparameter *cmd-publisher* nil)  ; ZMQ publisher socket
(defparameter *last-heartbeat-sent* 0)

;;; ==========================================
;;; EVOLUTION & LEARNING STATE
;;; ==========================================
(defparameter *dream-cycle* 0)
(defparameter *initial-backtest-done* nil)
(defparameter *last-narrative-day* -1)

;;; ==========================================
;;; TRADE TRACKING STATE
;;; ==========================================
(defparameter *accumulated-pnl* 0.0 "Cumulative profit/loss")
(defparameter *consecutive-wins* 0 "Current winning streak")
(defparameter *consecutive-losses* 0 "Current losing streak")
(defparameter *success-count* 0 "Total successful trades")
(defparameter *tribe-direction* :hold "Current tribe consensus direction")
(defparameter *tribe-consensus* 0.0 "Tribe agreement level 0-1")
(defparameter *danger-level* 0 "Current danger level 0-5")
(defparameter *last-swarm-consensus* nil "Last swarm voting result")


;; School State
(defparameter *trade-history* (make-hash-table :test 'equal))
(defparameter *strategy-ranks* (make-hash-table :test 'equal))
(defparameter *category-positions* (make-hash-table :test 'eq))
(defparameter *pair-correlations* (make-hash-table :test 'equal))
(defparameter *symbol-exposure* (make-hash-table :test 'equal))
(defparameter *max-symbol-exposure* 3.0)
(defparameter *max-total-exposure* 6.0)
(defparameter *current-leader* nil)

;; Risk State
(defparameter *resignation-threshold* -2000.0)
(defparameter *danger-cooldown-until* 0)
(defparameter *has-resigned-today* nil)

;;; ==========================================
;;; NEURAL NETWORK STATE
;;; ==========================================

(defparameter *last-prediction* "HOLD")
(defparameter *last-confidence* 0.0)
(defparameter *nn-threshold* 0.6)

;;; ==========================================
;;; GOVERNANCE & HIGH COUNCIL
;;; ==========================================
(defparameter *council-log* nil)
(defparameter *council-decision-threshold* 0.70)
(defparameter *notify-chieftain-threshold* :critical)

;;; ==========================================
;;; CONSTITUTION
;;; ==========================================
(defparameter *constitution* nil)
(defparameter *constitution-version* "1.0")

;;; ==========================================
;;; PHILOSOPHY LOGGER
;;; ==========================================
(defparameter *philosophy-log* nil)
(defparameter *philosophy-log-max* 500)
(defparameter *philosophy-log-path* "/home/swimmy/swimmy/.opus/philosophy_log.md")



;;; ==========================================
;;; TIMING
;;; ==========================================

(defparameter *dream-interval* 300
  "Seconds between Gemini dreaming cycles")
(defparameter *last-dream-time* 0)

;;; ==========================================
;;; ROUND-ROBIN TICK OPTIMIZATION
;;; ==========================================

(defparameter *symbol-round-robin-index* 0
  "V41.4: Index for round-robin symbol processing")


(format t "[L] ⚙️ core/config.lisp loaded - System configuration~%")
