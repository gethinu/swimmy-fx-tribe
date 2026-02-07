;;; src/lisp/core/globals.lisp
;;; Global variable and structure definitions
;;; =========================================

(in-package :swimmy.globals)

;;; CORE STRUCTURES
(defstruct candle timestamp open high low close volume)
(defstruct arm-state position entry-price sl tp streak size symbol)

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
(defvar *candle-histories* (make-hash-table :test 'equal) "Master candle history: symbol -> M1 candles")
(defvar *candle-histories-tf* (make-hash-table :test 'equal) "V41.6: Multi-timeframe data: symbol -> tf -> candles")
(defvar *current-candles* (make-hash-table :test 'equal))
(defvar *current-minutes* (make-hash-table :test 'equal))
(defvar *current-candle* nil)
(defvar *current-minute* -1)

;;; TRADING STATE
(defparameter *trading-enabled* t)
(defvar *daily-trade-count* 0)
(defvar *accumulated-pnl* 0.0 "Life-time PnL (accumulated across all sessions)")
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
(defvar *daily-pnl* 0.0 "Daily PnL (resets at 00:00)")
(defvar *current-equity* 100000.0)
(defvar *peak-equity* 100000.0 "Peak equity for all-time drawdown calculation")
(defvar *max-drawdown* 0.0 "Maximum observed drawdown percentage (all-time)")
(defvar *current-drawdown* 0.0 "Current drawdown percentage")
(defparameter *max-dd-percent* 5.0)

;; V44.5: Dynamic (Session) Drawdown Monitoring (Expert Panel P2)
(defvar *monitoring-peak-equity* 0.0 "Peak equity for this monitoring session (reset on restart)")
(defvar *monitoring-drawdown* 0.0 "Dynamic drawdown relative to monitoring peak")
(defvar *monitoring-alert-sent-20* nil "Toggle for 20% alert")

;;; AI/ML STATE
(defparameter *prediction-cache* (make-hash-table :test 'equal))
(defparameter *history-process-cache* (make-hash-table :test 'equal) "V15.8: Throttle HISTORY msg processing")
(defparameter *last-prediction* nil)
(defparameter *last-confidence* 0.5)
(defparameter *last-swarm-consensus* 0.5)
(defparameter *learned-patterns* nil)
(defparameter *improvement-requests* nil)
;; Persistence additions
(defparameter *failure-history* nil)
(defparameter *failure-log* nil)
(defparameter *success-log* nil)

;;; KNOWLEDGE STATE
(defparameter *reputation-scores* (make-hash-table :test 'equal))
(defparameter *genome* nil)
(defparameter *memory* nil)

;;; COMMUNICATION STATE
(defparameter *discord-webhook-url* nil)
(defvar *last-discord-notification-time* 0 "V48.2 Expert Panel: Last time ANY message was sent to Discord")
(defvar *last-zmq-success-time* 0 "V48.2: Last time a ZMQ message was successfully handed off to pub-socket")
(defparameter *last-heartbeat-sent* 0)
(defparameter *publisher* nil)
(defparameter *subscriber* nil)
(defparameter *cmd-publisher* nil)
(defparameter *backtest-requester* nil)
(defvar *pending-orders* (make-hash-table :test 'equal) "Phase 7: Map of UUID -> (timestamp retry-count message)")

;;; MISSING GLOBALS (Package Migration)
(defparameter *current-leader* nil)
(defparameter *current-volatility-state* :unknown)
;; V49.5: Decoupled Backtest Buffers (Expert Panel P1)
(defparameter *rr-backtest-results-buffer* nil "Buffer for Round-Robin KB backtests")
(defparameter *rr-expected-backtest-count* 0)
(defparameter *rr-backtest-start-time* 0)

(defparameter *qual-backtest-results-buffer* nil "Buffer for Qualification (Incubator/Scout) backtests")
(defparameter *qual-expected-backtest-count* 0)
(defparameter *qual-backtest-start-time* 0)
(defparameter *backtest-submit-last-id* nil "Last request_id submitted for backtests.")
(defparameter *backtest-submit-count* 0 "Total backtest requests submitted.")
(defparameter *backtest-max-pending* 500 "Max pending backtest requests before throttling.")
(defparameter *backtest-rate-limit-per-sec* 5 "Max backtest sends per second.")
(defparameter *backtest-last-send-ts* 0 "Last send timestamp (monotonic seconds).")

(defparameter *sent-data-ids* (make-hash-table :test 'equal) "V50.9: Track Data IDs sent to Guardian")
(defparameter *phase2-last-end-unix* 0 "Last Phase2 end_time (Unix seconds) used for backtests.")

(defparameter *backtest-results-buffer* nil "DEPRECATED (Legacy fallback)")
(defparameter *expected-backtest-count* 0 "DEPRECATED (Legacy fallback)")
(defparameter *supported-symbols* '("USDJPY" "EURUSD" "GBPUSD"))
(defparameter *symbol-volatility-states* (make-hash-table :test 'equal))
(defparameter *market-regime* :ranging)

;;; SCHOOL STATE
(defparameter *trade-history* (make-hash-table :test 'equal))
(defparameter *strategy-ranks* (make-hash-table :test 'equal))
(defparameter *category-positions* (make-hash-table :test 'eq))
(defparameter *pair-correlations* (make-hash-table :test 'equal))
(defparameter *symbol-exposure* (make-hash-table :test 'equal))

;;; MISSING GLOBALS (Cleanup)
(defparameter *backtest-webhook-url* nil)
(defparameter *last-status-notification-time* (make-hash-table :test 'equal))
(defparameter *status-notification-interval* 3600)
(defparameter *warrior-allocation* (make-hash-table :test 'equal))
(defparameter *resignation-threshold* -10000) ; Default logic usually sets this

;;; MISSING GLOBALS (Discord)
(defparameter *alerts-webhook-url* nil)
(defparameter *status-webhook-url* nil)
(defparameter *discord-daily-webhook* nil)
(defparameter *discord-weekly-webhook* nil)
(defparameter *discord-emergency-url* nil)
(defparameter *symbol-webhooks* (make-hash-table :test 'equal))
(defparameter *discord-recruit-webhook* nil)

;;; MISSING GLOBALS (Engine/Manager)
(defparameter *max-streak-losses* 3)
(defparameter *max-portfolio-size* 5)
(defparameter *has-resigned-today* nil)
(defparameter *max-symbol-exposure* 0.30)

;;; MISSING GLOBALS (Metrics/Signals/Config)
(defparameter *consecutive-wins* 0)
(defparameter *nn-threshold* 0.6)
(defparameter *genome-path* "genome.lisp")
(defparameter *max-total-exposure* 0.60)
(defparameter *strategy-usage-stats* (make-hash-table :test 'equal))

(defparameter *monthly-goal* 100000.0)
(defparameter *last-narrative-day* -1)
(defparameter *last-new-day* nil "Last processed YYYYMMDD for day rollover")
(defparameter *daily-report-last-date* nil "YYYYMMDD of last daily report sent")
(defparameter *daily-report-sent-today* nil)
(defparameter *daily-pnl-aggregation-sent-today* nil)

;;; GOVERNANCE STATE
(defparameter *council-log* nil)
(defparameter *constitution* nil)
(defparameter *philosophy-log* nil)

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

;; Tiered Risk Limits (Expert Panel 2026-01-14)
(defvar *daily-loss-limit-pct* -5.0)    ; Stop trading for the day
(defvar *weekly-loss-limit-pct* -10.0)  ; Stop trading for the week
(defvar *monthly-loss-limit-pct* -20.0) ; Stop trading for the month

;; Base capital for Tiered Risk Limits (V19.8)
(defvar *total-capital* 1000000.0 "Base capital for risk calculations")
(defvar *min-safe-capital* 100000.0 "Conservative fallback capital when equity is unknown")

;; Equity tracking - Uses defvar to preserve state on hot-reload
(defparameter *hard-deck-drawdown-pct* 50.0)  ; Permanent System Stop (Ruin Prevention)

;; Risk Metrics
(defparameter *weekly-pnl* 0.0)       ; Resets Monday 00:00
(defparameter *monthly-pnl* 0.0)      ; Resets 1st of Month 00:00
(defvar *processed-tickets* (make-hash-table :test 'equal) "V44.0: Deduplication for TRADE_CLOSED events")
(defparameter *portfolio-indices* nil)
(defparameter *market-data* nil)
(defparameter *last-tick-time* 0)

;; V49.5: System Management Globals
(defvar *system-state* :unknown "Current operational state: :trading, :warmup, :paused.")
(defvar *cooldown-tier* 0 "Current danger avoidance level.")
(defparameter *cooldown-durations* '(180 300 600 900 1800 2700 3600 7200 10800 14400 :eod)
  "Escalating pause durations for the danger system.")

;; V49.5: Batch Buffering (Decoupled)
(defparameter *cpcv-results-buffer* nil)
(defparameter *expected-cpcv-count* 0)
(defparameter *cpcv-start-time* 0)

(format t "[GLOBALS] Global variables defined in SWIMMY.GLOBALS~%")
