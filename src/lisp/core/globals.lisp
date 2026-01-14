;;; src/lisp/core/globals.lisp
;;; Global variable and structure definitions
;;; =========================================

(in-package :swimmy.globals)

;;; CORE STRUCTURES
(defstruct candle timestamp open high low close volume)
(defstruct arm-state position entry-price sl tp streak)

(defstruct elder
  name              ; Strategy name
  peak-pnl          ; Highest profit achieved
  era               ; When they were active
  speciality        ; What they were good at
  wisdom            ; Lessons learned
  vote-weight)      ; How much their vote counts

;;; SYSTEM STATE
(defvar *system-start-time* (get-universal-time) "Universal time when the system started.")

;;; STRATEGY STATE
(defvar *evolved-strategies* nil "List of evolved/generated strategies.")
(defvar *strategy-knowledge-base* nil "Master knowledge base of all strategies.")
(defvar *active-team* (make-hash-table :test 'equal) "Currently active trading team by category.")

;;; MARKET STATE
(defvar *current-regime* :unknown)
(defvar *volatility-regime* :normal)
(defvar *candle-history* nil)
(defvar *candle-histories* (make-hash-table :test 'equal))
(defvar *current-candles* (make-hash-table :test 'equal))
(defvar *current-minutes* (make-hash-table :test 'equal))

;;; TRADING STATE
(defparameter *trading-enabled* t)
(defvar *daily-trade-count* 0)
(defvar *accumulated-pnl* 0)
(defvar *locked-treasury* 0)
(defvar *last-regime* nil)
(defparameter *base-lot-size* 0.01)
(defvar *success-count* 0)
(defvar *total-trades* 0)

;;; RISK STATE
(defvar *danger-level* 0)
(defvar *consecutive-losses* 0)
(defparameter *risk-tolerance* :balanced)
(defparameter *daily-loss-limit* -5000)
(defvar *daily-pnl* 0.0)
(defvar *current-equity* 100000.0)
(defvar *peak-equity* 100000.0)
(defvar *max-drawdown* 0.0)
(defparameter *max-dd-percent* 5.0)

;;; AI/ML STATE
(defparameter *prediction-cache* (make-hash-table :test 'equal))
(defparameter *history-process-cache* (make-hash-table :test 'equal) "V15.8: Throttle HISTORY msg processing")
(defparameter *last-prediction* nil)
(defparameter *last-confidence* 0.5)
(defparameter *last-swarm-consensus* 0.5)
(defparameter *tribe-direction* nil)
(defparameter *tribe-consensus* 0.0)
(defparameter *learned-patterns* nil)
(defparameter *improvement-requests* nil)
;; Persistence additions
(defparameter *failure-history* nil)
(defparameter *failure-log* nil)
(defparameter *success-log* nil)

;;; COMMUNICATION STATE
(defparameter *discord-webhook-url* nil)
(defparameter *last-heartbeat-sent* 0)
(defparameter *publisher* nil)
(defparameter *subscriber* nil)
(defparameter *cmd-publisher* nil)
(defparameter *backtest-requester* nil)

;;; MISSING GLOBALS (Package Migration)
(defparameter *current-leader* nil)
(defparameter *current-volatility-state* :unknown)
(defparameter *backtest-results-buffer* nil)
(defparameter *expected-backtest-count* 0)
(defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
(defparameter *symbol-volatility-states* (make-hash-table :test 'equal))
(defparameter *market-regime* :ranging)

;;; MISSING GLOBALS (Cleanup)
(defparameter *backtest-webhook-url* nil)
(defparameter *last-status-notification-time* (make-hash-table :test 'equal))
(defparameter *status-notification-interval* 3600)
(defparameter *warrior-allocation* (make-hash-table :test 'equal))
(defparameter *tribe-status* (make-hash-table :test 'eq))
(defparameter *resignation-threshold* -10000) ; Default logic usually sets this

;;; MISSING GLOBALS (Discord)
(defparameter *alerts-webhook-url* nil)
(defparameter *status-webhook-url* nil)
(defparameter *discord-daily-webhook* nil)
(defparameter *discord-weekly-webhook* nil)
(defparameter *discord-emergency-url* nil)
(defparameter *symbol-webhooks* (make-hash-table :test 'equal))
(defparameter *discord-recruit-webhook* "https://discord.com/api/webhooks/1413195529680191538/OLcthUXpQr6fM32o8Vx-zlEJfgDTXfq14RPPSJdEKBJJZUUVBWJ9Hwq7ZPNFOMDkmQSW")

;;; MISSING GLOBALS (Engine/Manager)
(defparameter *benched-arms* nil)
(defparameter *max-streak-losses* 3)
(defparameter *max-portfolio-size* 5)
(defparameter *has-resigned-today* nil)
(defparameter *max-symbol-exposure* 0.15)

;;; MISSING GLOBALS (Metrics/Signals/Config)
(defparameter *consecutive-wins* 0)
(defparameter *nn-threshold* 0.6)
(defparameter *genome-path* "genome.lisp")
(defparameter *max-total-exposure* 5.0)
(defparameter *strategy-usage-stats* (make-hash-table :test 'equal))

(defparameter *monthly-goal* 100000.0)
(defparameter *last-narrative-day* -1)
(defparameter *daily-report-sent-today* nil)

;;; ADDITIONAL MISSING GLOBALS (Comprehensive Review)
(defparameter *danger-cooldown-until* nil)
(defparameter *symbol-round-robin-index* 0)
(defparameter *dream-cycle* 0)
(defparameter *dream-interval* 3600)
(defparameter *last-dream-time* 0)
(defparameter *last-guardian-heartbeat* 0)
(defparameter *last-account-info-time* 0)  ; V8.5: ACCOUNT_INFO monitoring (Expert Panel P1)
(defparameter *account-info-alert-sent* nil) ; V8.5: Prevent duplicate alerts
(defparameter *all-time-win-rate* 0.5)
(defparameter *portfolio-sharpe* 0.0)
(defparameter *initial-backtest-done* nil)
(defparameter *arms* (make-hash-table :test 'equal))
(defparameter *arm-states* (make-hash-table :test 'equal))
(defparameter *portfolio-indices* nil)
(defparameter *clans* nil)
(defparameter *market-data* nil)
(defparameter *last-tick-time* 0)
(defparameter *candle-histories* (make-hash-table :test 'equal))
(defparameter *candle-histories-tf* (make-hash-table :test 'equal)) ; V41.6: Nested hash table for multi-timeframe support

(format t "[GLOBALS] Global variables defined in SWIMMY.GLOBALS~%")
