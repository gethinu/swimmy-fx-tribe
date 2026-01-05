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
   #:*last-narrative-day*
   #:*last-guardian-heartbeat*
   #:*last-tick-time*
   #:*market-data*
   #:*all-time-win-rate*
   #:*portfolio-sharpe*
   #:*last-status-notification-time*
   #:*status-notification-interval*
   
   ;; Discord Webhooks
   #:*alerts-webhook-url*
   #:*status-webhook-url*
   #:*discord-daily-webhook*
   #:*discord-weekly-webhook*
   #:*discord-emergency-url*
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
   #:get-time-string
   #:get-date-string
   
   ;; Logger
   #:log-info
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
   #:notify-discord-daily
   #:flush-discord-queue
   ))

;;; ----------------------------------------------------------------------------
;;; 3. ENGINE (Logic Layer)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.engine
  (:use :cl :swimmy.globals :swimmy.core)
  (:export
   ;; Risk
   #:safe-order
   #:get-risk-summary
   #:check-risk-limits
   
   ;; Portfolio
   #:update-portfolio
   #:get-portfolio-summary
   
   ;; Metabolism
   #:run-metabolism
   #:prune-strategies
   
   ;; Heartbeat
   #:start-heartbeat
   
   ;; Learning
   #:load-hall-of-fame
   
   ;; Goals
   #:get-goal-progress
   #:get-daily-target
   
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
   
   ;; Research
   ;; Research
   #:integrate-research-papers
   
   ;; Missing Exports
   #:initialize-clan-treasury
   #:assemble-team
   #:morning-ritual
   #:request-prediction
   #:maintain-ecosystem-balance
   #:get-population-health
   #:process-category-trades
   #:check-evolution
   #:evolve-population
   ))

;;; ----------------------------------------------------------------------------
;;; 5. SHELL (Interface Layer)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.shell
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school)
  (:export
   ;; Notification
   #:broadcast-event
   #:initialize-tribal-dialect
   #:setup-symbol-webhooks
   ;; Briefing
   #:report-goal-status
   #:send-periodic-status-report
   #:save-live-status
   ))

;;; ----------------------------------------------------------------------------
;;; 6. MAIN (Entry Point)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.main
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school :swimmy.shell)
  (:export 
   #:start-system
   #:start-brain
   #:stop-brain
   #:call-gemini
   ;; Tick Handler (moved from swimmy.engine)
   #:process-msg
   #:update-candle
   #:send-heartbeat
   #:check-daily-narrative
   ))

;;; ----------------------------------------------------------------------------
;;; 7. TESTS (Unit Tests)
;;; ----------------------------------------------------------------------------
(defpackage :swimmy.tests
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine :swimmy.school :swimmy.shell))

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
