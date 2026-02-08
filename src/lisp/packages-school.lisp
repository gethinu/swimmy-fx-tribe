;;; src/lisp/packages-school.lisp
;;; ============================================================================
;;; PACKAGE DEFINITIONS (School & Upper Layers)
;;; ============================================================================

(in-package :cl-user)

;;; 4. SCHOOL (Strategy Layer) ---------------------------------------------------
(defpackage :swimmy.school
  (:use :cl :swimmy.globals :swimmy.core :swimmy.engine)
  (:export
   ;; Council
   #:convene-high-council
   #:run-school-iteration
   
   ;; Monte-Carlo Validation (Phase 11)
   #:run-monte-carlo-simulation
   #:analyze-monte-carlo-results
   #:run-mc-validation
   #:internal-bootstrap-sample ;; Exposed for testing
   #:calculate-max-drawdown ;; Exposed for testing
   
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
   #:assemble-team
   #:morning-ritual
   #:request-prediction
   #:request-backtest
   #:maintain-ecosystem-balance
   #:get-population-health
   #:process-category-trades
   #:check-evolution
   #:get-regime-tactics ;; V49.2: Tactical Mapping
   #:*current-regime*
   #:*predicted-regime*
   #:*volatility-regime*
   #:evolve-population-via-mutation
   #:mutate-strategy
   #:batch-backtest-knowledge
   #:adopt-proven-strategies
   #:ensure-rank
   #:send-to-graveyard
   #:strategy-rank
   #:record-trade-result
   #:update-leader-stats
   #:update-global-stats ;; V44.3: Moved from Reporting
   #:lookup-strategy-by-magic  ;; V44.8: Resolving Unknown strategy names
   #:lookup-pair-id-by-magic
   #:generate-trade-result-narrative
   #:process-clone-check-result ;; V10: Exported for message-dispatcher
   #:process-wfv-result ;; V10: Exported for message-dispatcher
   
   ;; Persistence
   #:cache-backtest-result
   #:save-backtest-cache
   #:load-backtest-cache
   #:get-cached-backtest
   #:record-backtest-trades
   #:fetch-backtest-trades
   #:refresh-strategy-metrics-from-db
   #:fetch-candidate-strategies
   #:fetch-all-strategies-from-db
   #:collect-all-strategies-unpruned
   #:map-strategies-from-db
   #:get-top-carry-pairs
   #:fetch-swap-history
   
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
   #:strategy
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
   #:strategy-status
   #:strategy-status-reason
   #:strategy-revalidation-pending
   #:strategy-symbol
   #:strategy-direction
   #:strategy-timeframe
   #:strategy-profit-factor
   #:strategy-regime-intent ;; V49.2: Metadata
   #:strategy-oos-sharpe
   #:strategy-cpcv-median-sharpe
   #:strategy-cpcv-pass-rate
   
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
   
   ;; Rank Accessors (V46.1)
   #:get-strategy-rank
   #:get-strategies-by-rank
   #:strategy-rank-rank
   #:strategy-rank-trades
   #:check-promotion
   #:recruit-elite-strategy
   
   ;; V47.0: B/A/S Rank System
   #:run-rank-evaluation
   #:check-rank-criteria
   #:evaluate-new-strategy
   #:apply-backtest-result
    #:upsert-strategy
   #:get-db-rank-counts
   #:get-library-rank-counts
   #:report-source-drift
   #:run-b-rank-culling
   #:can-breed-p
   #:run-legend-breeding
   #:queue-legend-revalidation
   
   ;; P8: Single Entry Point for KB
   #:add-to-kb
   #:*startup-mode*
   #:end-startup-mode
   #:notify-recruit-unified
   #:validate-strategy-for-kb
   
   ;; Trade Prediction Struct
   #:make-trade-prediction
   #:trade-prediction-symbol
   #:trade-prediction-confidence
   #:summarize-status-prediction
   
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
   
   ;; Position Sync (V19)
   #:reconcile-with-mt5-positions
   #:request-mt5-positions
   #:report-active-positions
   
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
   
   ;; OOS Backtest
   #:enqueue-oos-request
   #:lookup-oos-request
   #:complete-oos-request
   #:record-oos-error
   #:handle-oos-backtest-result
   #:report-oos-metrics
   #:report-oos-failure-stats
   #:reset-oos-failure-stats
   #:oos-metrics-summary-line
   
   ;; Meta Learning
   #:get-best-strategy-for-regime
   #:update-best-strategy-for-regime
   #:on-trade-close-meta
   #:record-proof-trade
   
   ;; Continuous Learning
   #:continuous-learning-step
   #:evaluate-strategy-performance
   #:calculate-mutual-aid
   
   ;; Adaptation Engine (Phase 4)
   #:update-adaptation-weights
   #:get-adaptation-multiplier
   
   ;; Scoring Engine (Phase 5)
   #:estimate-confidence
   #:calculate-strategy-score
   #:get-market-state-vector

   ;; Lifecycle Management (Phase 6)
   #:manage-strategy-lifecycle
   #:perform-daily-lifecycle-review 

   ;; Killing & Pruning (P0/P1 Expert Panel 2026-01-16)
   #:kill-strategy
   #:prune-similar-strategies
   #:strategy-distance
   #:strategies-similar-p
   #:compete-for-slot
   
   ;; Daily Report
   #:get-daily-risk-limit
   #:get-performance-stats
   #:get-db-stats
   
   ;; Visualization
   #:print-lineage
   
   ;; Headhunting
   #:recruit-founder
   #:immigration-census
   #:list-available-founders
    
    ;; Genome Engine (V14.0)
    #:crossover-strategy
    #:extract-genome
    #:implant-genome
    #:perform-structural-mutation
    #:select-parent-tournament
    #:select-category-pair
    
    ;; Phase 13: Wisdom Native
    #:analyze-veterans
    
    ;; Reporting (V44.3)
    #:update-global-stats
    
    ;; Phase 12: The Connector
    #:start-evolution-service
    #:candles-to-json

    ;; V49.8: Cemetery & Tactical Avoidance
    #:calculate-strategy-hash
    #:get-graveyard-hashes
    #:prune-strategies-by-graveyard-hash
    #:cemetery-audit-db
    #:record-graveyard-pattern
    
    ;; Phase 28: FX Alchemy Screeners
    #:get-top-carry-pairs
    ;; Phase 29: Swap Data Persistence
    #:record-swap-data
    ;; Phase 30: Breakout/Tactician
    #:deploy-breakout-strategy
    ;; Phase 31: Backtest V2 Logic
    #:handle-v2-result
    ))

;;; 5. SHELL (Interface Layer) ---------------------------------------------------
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
   #:get-jst-time-string
   ))

;;; 6. EXECUTOR (Execution Layer) ------------------------------------------------
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
   
   ;; Allocation (Phase 12)
   #:calculate-kelly-lot
   ))

;;; 7. MAIN (Entry Point) --------------------------------------------------------
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
   #:save-knowledge-base
   ;; Utility functions
   ;; Utility functions
   ;; v1.0 Moved to swimmy.school
   ))

;;; 8. TESTS (Unit Tests) --------------------------------------------------------
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
