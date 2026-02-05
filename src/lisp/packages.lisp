;;; src/lisp/packages.lisp
;;; ============================================================================
;;; PACKAGE DEFINITIONS (Rich Hickey's Namespace Logic)
;;; ============================================================================

(in-package :cl-user)

;;; 1. GLOBALS (Base Layer) ------------------------------------------------------
(defpackage :swimmy.globals
  (:use :cl)
  (:export 
   ;; Structures (defined in globals.lisp)
   #:candle #:make-candle
   #:candle-timestamp #:candle-open #:candle-high #:candle-low #:candle-close #:candle-volume
   #:arm-state #:make-arm-state
   #:arm-state-position #:arm-state-entry-price #:arm-state-sl #:arm-state-tp #:arm-state-streak
   #:arm-state-size #:arm-state-symbol
   #:elder #:make-elder
   #:elder-name #:elder-peak-pnl #:elder-era #:elder-speciality #:elder-wisdom #:elder-vote-weight
   
   ;; Configuration
   #:*debug-mode*
   #:*discord-webhook-url*
   #:*gemini-api-key*
   #:*supported-symbols*
   #:*genome-path*
   #:*system-state*
   #:*cooldown-tier*
   #:*cooldown-durations*
   
   ;; ZMQ State
   #:*publisher*
   #:*subscriber*
   #:*cmd-publisher*
   #:*backtest-requester*
   #:*last-heartbeat-sent*
   #:*last-discord-notification-time*
   #:*last-zmq-success-time*
   #:*pending-orders* ;; Phase 7
   
   ;; Trading State
   #:*trading-enabled*
   #:*daily-trade-count*
   #:*accumulated-pnl*
   #:*daily-pnl*
   #:*monthly-goal*
   #:*total-trades*
   #:*base-lot-size*
   #:*locked-treasury*
   
   ;; Risk State
   #:*danger-level*
   #:*consecutive-losses*
   #:*consecutive-wins*
   #:*daily-loss-limit*
   #:*current-equity*
   #:*peak-equity*
   #:*max-drawdown*
   #:*current-drawdown*
   #:*max-dd-percent*
   #:*risk-tolerance*
   #:*max-streak-losses*
   #:*max-portfolio-size*
   #:*max-symbol-exposure*
   #:*max-total-exposure*
   #:*has-resigned-today*
   #:*resignation-threshold*
   #:*danger-cooldown-until*
   
   ;; Tiered Risk (V19.8)
   #:*weekly-pnl*
   #:*monthly-pnl*
   #:*daily-loss-limit-pct*
   #:*weekly-loss-limit-pct*
   #:*monthly-loss-limit-pct*
   #:*hard-deck-drawdown-pct*
   #:*min-safe-capital*
   
   ;; Market State
   #:*current-regime*
   #:*volatility-regime*
   #:*market-regime*
   #:*current-volatility-state*
   #:*candle-history*
   #:*candle-histories*
   #:*current-candles*
   #:*current-minutes*
   #:*current-candle*
   #:*current-minute*
   #:*last-regime*
   #:*symbol-volatility-states*
   #:*candle-histories-tf*  ;; V41.6: Nested hash for multi-timeframe data: symbol -> timeframe -> candles
   
   ;; Strategy State
   #:*strategy-knowledge-base*
   #:*evolved-strategies*
   #:*active-team*
   #:*strategy-usage-stats*
   #:*trade-history*
   #:*strategy-ranks*
   #:*category-positions*
   #:*pair-correlations*
   #:*symbol-exposure*
   #:*clans*
   
   ;; AI/ML State
   #:*last-prediction*
   #:*last-confidence*
   #:*last-swarm-consensus*
   #:*tribe-direction*
   #:*tribe-consensus*
   #:*tribe-status*
   #:*learned-patterns*
   #:*improvement-requests*
   #:*tribal-dialect*
   #:*reputation-scores*
   #:*nn-threshold*
   
   ;; Logging State
   #:*failure-history*
   #:*failure-log*
   #:*success-log*
   #:*success-count*
   
   ;; Engine State
   #:*arms*
   #:*arm-states*
   #:*portfolio-indices*
   #:*genome*
   #:*memory*
   #:*current-leader*
   #:*warrior-allocation*
   #:*symbol-round-robin-index*
   #:*symbol-webhooks*
   
   ;; Backtest State
   #:*backtest-results-buffer*
   #:*expected-backtest-count*
   #:*backtest-start-time*
   #:*rr-backtest-results-buffer*
   #:*rr-expected-backtest-count*
   #:*rr-backtest-start-time*
   #:*qual-backtest-results-buffer*
   #:*qual-expected-backtest-count*
   #:*qual-backtest-start-time*
   #:*backtest-submit-last-id*
   #:*cpcv-results-buffer*
   #:*expected-cpcv-count*
   #:*cpcv-start-time*
   #:*initial-backtest-done*
   #:*backtest-webhook-url*
   #:*sent-data-ids*  ;; V50.9: Track sent data IDs to prevent spam
   #:*phase2-last-end-unix*
   
   ;; Dream/Evolution State
   #:*dream-cycle*
   #:*dream-interval*
   #:*last-dream-time*
   
   ;; Misc State
   #:*council-log*
   #:*constitution*
   #:*philosophy-log*
   #:*daily-report-sent-today*
   #:*discord-recruit-webhook*
   #:*last-narrative-day*
   #:*last-guardian-heartbeat*
   #:*last-tick-time*
   #:*market-data*
   #:*all-time-win-rate*
   #:*portfolio-sharpe*
   #:*last-status-notification-time*
   #:*status-notification-interval*
   
   ;; Account Info (Expert Panel P1)
   #:*last-account-info-time*
   #:*account-info-alert-sent*
   
   ;; Discord Webhooks
   #:*alerts-webhook-url*
   #:*status-webhook-url*
   #:*discord-weekly-webhook*
   #:*discord-emergency-url*
   
   ;; New Prediction Cache
   #:*prediction-cache*
   #:*history-process-cache*  ;; V15.8: Throttle HISTORY processing
   
   ;; V44.5 Dynamic Drawdown
   #:*monitoring-peak-equity*
   #:*monitoring-drawdown*
   #:*monitoring-alert-sent-20*
   ))


;;; 2. CORE (Infrastructure) -----------------------------------------------------
(defpackage :swimmy.core
  (:use :cl :swimmy.globals)
  (:export
   ;; Structures
   #:candle #:make-candle
   #:candle-timestamp #:candle-open #:candle-high #:candle-low #:candle-close #:candle-volume
   #:arm-state #:make-arm-state
   #:arm-state-position #:arm-state-entry-price #:arm-state-sl #:arm-state-tp #:arm-state-streak
   
   ;; Utils
   #:get-jst-str
   #:get-jst-timestamp
   #:get-time-string
   #:get-date-string
   #:safe-read-sexp
   #:safe-parse-number
   #:write-sexp-atomic
   #:read-sexp-file
   
   ;; Logger
   #:log-info
   #:log-warn
   #:log-debug
   #:log-error
   #:setup-logger
   
   ;; Config
   #:load-config
   #:*zmq-host*
   #:*port-market*
   #:*port-exec*
   #:*port-sensory*
   #:*port-motor*
   #:*port-external*
   #:*port-data-keeper*
   #:*port-notifier*
   #:*port-backtest-req*
   #:*port-backtest-res*
   #:*backtest-service-enabled*
   #:zmq-connect-endpoint
   #:zmq-bind-endpoint
   #:*constitution-version*
   #:*council-decision-threshold*
   #:*notify-chieftain-threshold*
   #:*philosophy-log-max*
   #:*trading-days-in-month*
   
   ;; Re-export Globals
   #:*debug-mode*
   #:*balance*
   #:*publisher*
   
   ;; Discord (Infrastructure)
   #:notify-discord
   #:notify-discord-symbol
   #:notify-discord-recruit
   #:notify-discord-daily
   #:notify-backtest-summary
   #:notify-discord-alert  ;; Exported for Circuit Breaker
   #:notify-discord-status
   #:notify-discord-weekly
   #:notify-discord-emergency
   #:notify-discord-backtest
   #:notify-cpcv-result
   #:notify-cpcv-summary
   #:notify-apex            ;; V41.6: Apex webhook for system status
   #:queue-discord-notification ;; Exported for tick-handler.lisp
   #:flush-discord-queue
   #:check-timeout-flushes
   #:+color-recruit+
   #:+color-alert+
   #:+color-backtest+
   #:+color-success+
   #:+color-info+
   #:+color-emergency+
   #:+color-weekly+
   #:+color-status+
   #:*discord-daily-webhook* ;; Fix for school-narrative.lisp

   
   ;; Research Algorithms (Migrated from School)
   #:research-enhanced-analysis
   #:detect-regime-hmm
   #:dual-trend-signal
   #:select-optimal-model
   #:estimate-mean-reversion
   
   ;; Data Keeper Client (V8.0)
   #:init-data-keeper-client
   #:data-keeper-status
   #:get-history-from-keeper
   #:add-candle-to-keeper
   #:close-data-keeper-client
   #:check-data-gap
   
   ;; Risk Gateway Client (V8.0)
   #:request-trade-approval
   
   ;; Helper for Kalman
   #:ind-kalman
   #:ind-kalman-velocity
   #:ind-kalman-trend

   ;; Inference Client (V8.0)
   ;; Inference Client (V8.0)
   #:ask-ai
   
   ;; Execution Protocol (Phase 7)
   #:make-order-message
   #:make-heartbeat-message
   #:encode-sexp
   #:sexp-alist-get
   #:generate-uuid ;; Utility
   
   ;; Message Constants
   #:+MSG-HEARTBEAT+
   #:+MSG-ORDER-OPEN+
   #:+MSG-ORDER-FILL+
   #:+MSG-ORDER-ACK+
   #:+MSG-TRADE-CLOSED+
   #:+MSG-ACCOUNT-INFO+
    #:+MSG-HISTORY+
    #:+MSG-TICK+
    #:+MSG-SWAP-DATA+

   ;; V49.8: SQL Connection Management
   #:execute-non-query
   #:execute-to-list
   #:execute-single
   #:with-transaction
   #:close-db-connection
   ))



;;; 3. ENGINE (Logic Layer) ----------------------------------------------------
(defpackage :swimmy.engine
  (:use :cl :swimmy.globals :swimmy.core)
  (:export
   ;; Risk
   #:safe-order
   #:risk-check-all
   #:trading-allowed-p
   #:calculate-lot-size
   #:update-drawdown
   #:get-risk-summary
   #:check-risk-limits
   #:check-correlation-risk
   #:get-symbol-exposure
   #:apply-gotobi-adjustment
   #:apply-london-edge
   
   ;; Portfolio
   #:update-portfolio
   #:get-portfolio-summary
   
   ;; Metabolism
   #:run-metabolism
   #:prune-strategies
   
   ;; Heartbeat
   #:start-heartbeat
   #:heartbeat-now
   
   ;; Learning
   #:update-nn-threshold
   
   ;; Metrics
   #:get-daily-risk-limit
   #:get-performance-stats
   
   ;; Goals
   #:get-goal-progress
   #:get-daily-target
   
   ;; State Persistence
   #:save-state
   #:load-state
   #:*state-file-path*
   
   ;; Positions
   #:on-trade-opened
   #:on-trade-closed
   
   ;; Danger
   #:danger-cooldown-active-p
   #:has-resigned-p
   
   ;; Strategy Struct (if defined here)
   #:strategy-sharpe

   ;; Candle Struct (V10.0 Export)
   #:make-candle
   #:candle-open
   #:candle-high
   #:candle-low
   #:candle-close
   #:candle-timestamp
   
   ;; Webhooks (tick-handler)
   #:process-msg
   ))

;;; 4. SCHOOL and above moved to packages-school.lisp
