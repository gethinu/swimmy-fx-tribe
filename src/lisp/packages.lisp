;;; src/lisp/packages.lisp
;;; ============================================================================
;;; PACKAGE DEFINITIONS (Rich Hickey's Namespace Logic)
;;; ============================================================================

(in-package :cl-user)

;;; ----------------------------------------------------------------------------
;;; 1. GLOBALS (Base Layer)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.globals
  (:use :cl)
  (:export 
   ;; Structures (defined in globals.lisp)
   #:candle #:make-candle
   #:candle-timestamp #:candle-open #:candle-high #:candle-low #:candle-close #:candle-volume
   #:arm-state #:make-arm-state
   #:arm-state-position #:arm-state-entry-price #:arm-state-sl #:arm-state-tp #:arm-state-streak
   
   ;; Configuration
   #:*debug-mode*
   #:*discord-webhook-url*
   #:*gemini-api-key*
   #:*supported-symbols*
   #:*genome-path*
   
   ;; ZMQ State
   #:*publisher*
   #:*subscriber*
   #:*cmd-publisher*
   #:*backtest-requester*
   #:*last-heartbeat-sent*
   
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
   #:*max-dd-percent*
   #:*risk-tolerance*
   #:*max-streak-losses*
   #:*max-portfolio-size*
   #:*max-symbol-exposure*
   #:*max-total-exposure*
   #:*has-resigned-today*
   #:*resignation-threshold*
   #:*danger-cooldown-until*
   
   ;; Market State
   #:*current-regime*
   #:*volatility-regime*
   #:*market-regime*
   #:*current-volatility-state*
   #:*candle-history*
   #:*candle-histories*
   #:*current-candles*
   #:*current-minutes*
   #:*last-regime*
   #:*symbol-volatility-states*
   #:*candle-histories-tf*  ;; V41.6: Nested hash for multi-timeframe data: symbol -> timeframe -> candles
   
   ;; Strategy State
   #:*strategy-knowledge-base*
   #:*evolved-strategies*
   #:*active-team*
   #:*strategy-usage-stats*
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
   #:*benched-arms*
   #:*current-leader*
   #:*warrior-allocation*
   #:*symbol-round-robin-index*
   #:*symbol-webhooks*
   
   ;; Backtest State
   #:*backtest-results-buffer*
   #:*expected-backtest-count*
   #:*initial-backtest-done*
   #:*backtest-webhook-url*
   
   ;; Dream/Evolution State
   #:*dream-cycle*
   #:*dream-interval*
   #:*last-dream-time*
   
   ;; Misc State
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
   ))


;;; ----------------------------------------------------------------------------
;;; 2. CORE (Infrastructure)
;;; ----------------------------------------------------------------------------
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
   
   ;; Logger
   #:log-info
   #:log-warn
   #:log-debug
   #:log-error
   #:setup-logger
   
   ;; Config
   #:load-config
   
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
   #:notify-apex            ;; V41.6: Apex webhook for system status
   #:queue-discord-notification ;; Exported for tick-handler.lisp
   #:flush-discord-queue
   
   ;; Research Algorithms (Migrated from School)
   #:research-enhanced-analysis
   #:detect-regime-hmm
   #:dual-trend-signal
   #:select-optimal-model
   #:estimate-mean-reversion
   #:estimate-mean-reversion
   
   ;; Data Keeper Client (V8.0)
   #:init-data-keeper-client
   #:data-keeper-status
   #:get-history-from-keeper
   #:add-candle-to-keeper
   #:close-data-keeper-client
   
   ;; Risk Gateway Client (V8.0)
   #:request-trade-approval
   
   ;; Helper for Kalman
   #:ind-kalman
   #:ind-kalman-velocity
   #:ind-kalman-trend

   ;; Inference Client (V8.0)
   #:ask-ai
   ))



;;; ----------------------------------------------------------------------------
;;; 3. ENGINE (Logic Layer)
;;; ----------------------------------------------------------------------------
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
   
   ;; Learning
   #:train-neural
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
   
   ;; Webhooks (tick-handler)
   #:process-msg
   ))

;;; ----------------------------------------------------------------------------
;;; 4. SCHOOL (Strategy Layer)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.school
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine)
  (:export
   ;; Council
   #:convene-high-council
   #:run-school-iteration
   
   ;; Learning
   #:record-trade-outcome
   #:extract-learned-patterns
   #:learn-from-failure
   #:store-memory
   #:get-price-position
   
   ;; Research
   #:integrate-research-papers
   
   ;; Indicators (used in tick-handler)
   #:ind-rsi
   #:ind-sma
   #:ind-ema
   #:ind-bb
   #:ind-atr
   #:calculate-atr
   
   ;; Meta Learning
   #:get-best-strategy-for-regime
   #:update-best-strategy-for-regime
   #:extract-learned-patterns
   
   ;; Trading Session
   #:current-trading-session
   #:london-session-p
   #:gotobi-day-p
   
   ;; Strategy Management
   #:initialize-clan-treasury
   #:assemble-team
   #:morning-ritual
   #:request-prediction
   #:request-backtest
   #:maintain-ecosystem-balance
   #:get-population-health
   #:process-category-trades
   #:check-evolution
   #:evolve-population
   #:mutate-strategy
   #:batch-backtest-knowledge
   #:adopt-proven-strategies
   #:record-trade-result
   #:update-leader-stats
   
   ;; Special Forces
   #:force-recruit-strategy
   #:recruit-special-forces
   #:strategy-allowed-by-volatility-p
   #:get-volatility-multiplier
   #:*volatility-shift-threshold*
   
   ;; Constitution
   #:initialize-constitution
   #:evaluate-constitution
   #:*constitution*
   
   ;; Reputation
   #:get-reputation
   #:update-reputation
   
   ;; Strategy Struct Accessors
   #:make-strategy
   #:strategy-name
   #:strategy-indicators
   #:strategy-sl
   #:strategy-tp
   #:strategy-volume
   #:strategy-entry
   #:strategy-exit
   #:strategy-sharpe
   #:strategy-win-rate
   #:strategy-trades
   #:strategy-max-dd
   #:strategy-benched-p
   
   ;; Clan Struct Accessors
   #:make-clan
   #:clan-id
   #:clan-name
   #:clan-title
   #:clan-emoji
   #:clan-philosophy
   #:clan-persona
   #:clan-battle-cry
   #:get-clan
   #:get-clan-display
   
   ;; Leader Info Struct Accessors
   #:make-leader-info
   #:leader-info-strategy-name
   #:leader-info-sharpe
   #:leader-info-pnl-as-leader
   #:leader-info-trades-as-leader
   
   ;; Elder Struct Accessors
   #:elder-name
   #:elder-peak-pnl
   #:elder-vote
   
   ;; Core Value Struct Accessors
   #:core-value-name
   #:core-value-description
   #:core-value-priority
   
   ;; Reputation Struct Accessors
   #:reputation-trust-score
   #:reputation-reliability
   #:reputation-profit-score
   
   ;; Trade Prediction Struct
   #:make-trade-prediction
   #:trade-prediction-symbol
   #:trade-prediction-confidence
   
   ;; Danger/Risk
   #:danger-cooldown-active-p
   #:has-resigned-p
   #:reset-danger-state
   #:debug-reset-warriors
   #:debug-warrior-status
   
   ;; Hall of Fame
   #:*hall-of-fame*
   #:induct-to-hall-of-fame
   #:load-hall-of-fame
   #:save-hall-of-fame
   #:elder-vote
   
   ;; Genome/State
   #:save-genome
   #:save-meta-learning
   
   ;; Pattern Matching
   #:pattern-similarity
   #:time-decay-weight
   
   ;; Learned Pattern Struct
   #:learned-pattern-confidence
   #:learned-pattern-description
   
   ;; Improvement Request Struct
   #:improvement-request-category
   #:improvement-request-description
   #:improvement-request-status
   
   ;; Tribal Dialect
   #:*tribal-dialect*
   #:initialize-tribal-dialect
   
   ;; Walk Forward Validation
   #:*wfv-pending-strategies*
   #:start-walk-forward-validation
   #:process-wfv-result
   
   ;; Meta Learning
   #:get-best-strategy-for-regime
   #:update-best-strategy-for-regime
   #:on-trade-close-meta
   #:record-proof-trade
   
   ;; Continuous Learning
   #:continuous-learning-step
   #:evaluate-strategy-performance
   #:calculate-mutual-aid
   
   ;; Benching
   #:weekly-unbench-all
   #:should-weekly-unbench-p
   
   ;; Daily Report
   #:get-daily-risk-limit
   #:get-performance-stats
   ))

;;; ----------------------------------------------------------------------------
;;; 5. SHELL (Interface Layer)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.shell
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school)
  (:export
   ;; Notification
   #:broadcast-event
   #:notify-discord
   #:notify-discord-symbol
   #:initialize-tribal-dialect
   #:setup-symbol-webhooks
   ;; Briefing
   #:report-goal-status
   #:send-periodic-status-report
   #:save-live-status
   ;; Date/Time utilities
   #:get-date-string
   #:get-time-string
   ))

;;; ----------------------------------------------------------------------------
;;; 6. EXECUTOR (Execution Layer) NOT MAIN
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.executor
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school :swimmy.shell)
  (:export
   ;; Heartbeat & Status
   #:send-heartbeat
   #:send-heartbeat-to-guardian
   #:check-guardian-connection
   #:update-status-file
   #:pulse-check
   
   ;; Trade Processing
   #:process-trade-closed
   #:process-account-info
   
   ;; Metrics
   #:get-system-metrics
   ))

;;; ----------------------------------------------------------------------------
;;; 7. MAIN (Entry Point)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.main
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school :swimmy.shell :swimmy.executor)
  (:export 
   #:start-system
   #:start-brain
   #:stop-brain
   #:call-gemini
   ;; Tick Handler (moved from swimmy.engine)
   #:process-msg
   #:update-candle
   #:send-heartbeat
   #:check-scheduled-tasks
   ;; Utility functions
   #:candles-to-json
   ))

;;; ----------------------------------------------------------------------------
;;; 7. TESTS (Unit Tests)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.tests
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school :swimmy.shell)
  (:export #:run-all-tests #:run-integration-tests))

;;; ----------------------------------------------------------------------------
;;; COMPATIBILITY (CL-USER)
;;; ----------------------------------------------------------------------------
;; Import everything into CL-USER for REPL convenience and legacy support
(format t "[PACKAGES] Importing symbols into CL-USER...~%")

(use-package :swimmy.globals :cl-user)
(use-package :swimmy.core :cl-user)
(use-package :swimmy.engine :cl-user)
(use-package :swimmy.school :cl-user)
(use-package :swimmy.shell :cl-user)
(use-package :swimmy.main :cl-user)
(use-package :swimmy.tests :cl-user)
;; Note: We avoid use-package for conflicts, but we might need it for legacy files.
;; For now, let's keep CL-USER relatively clean or use specific packages.

(format t "[PACKAGES] Package Hierarchy Defined.~%")
