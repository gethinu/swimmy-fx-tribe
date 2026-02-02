
(asdf:defsystem "swimmy"
  :description "The Efficient Gardener"
  :version "41.5.0"
  :author "Antigravity Team"
  :license "Proprietary"
  :depends-on ("cl-ppcre" "pzmq" "sqlite" "jsown" "dexador" "local-time" "uiop" "bordeaux-threads" "ironclad" "postmodern")
  :serial t
  :components ((:file "src/lisp/packages")
               (:file "src/lisp/packages-school")
               (:file "src/lisp/school/school-state") ; Moved to top (Dependency Fix)
               (:file "src/lisp/school/school-agent") ; V7.9++: Agent Skills (Extracted)
               (:file "src/lisp/school/school-integrity") ; Phase 25: Integrity
               (:file "src/lisp/school/school-scribe") ; Phase 25: Isolation
               (:file "src/lisp/school/school-watchdog") ; Phase 25: Watchdog
               (:file "src/lisp/core/globals")
               (:file "src/lisp/core/config")
               (:file "src/lisp/core/safe-read")
               (:file "src/lisp/core/persistence") ; Phase 16: Sharded Library
               (:file "src/lisp/core/execution-protocol") ; P7
               (:file "src/lisp/core/db-adapter")  ; Phase 9: DB Adapter
               (:file "src/lisp/core/sqlite-manager") ; V49.8: SQL Connection Management
               (:file "src/lisp/core/data-client")  ; V8.0: Data Keeper client
               (:file "src/lisp/core/risk-client")  ; V8.0: Risk Gateway client
               (:file "src/lisp/core/inference-client") ; V8.0: Inference client
               (:file "src/lisp/core/profiling")
               
               ;; ENGINE
               (:file "src/lisp/risk-manager")  ; V6.5: Added - contains safe-order (Unified Authority)
               (:file "src/lisp/engine/goals")
               (:file "src/lisp/engine/metrics")
               (:file "src/lisp/engine/experiments")
               (:file "src/lisp/engine/signals")
               (:file "src/lisp/engine/positions")
               (:file "src/lisp/engine/portfolio")
               (:file "src/lisp/engine/metabolism")
               (:file "src/lisp/engine/ledger")
               (:file "src/lisp/engine/treasury")
               (:file "src/lisp/engine/failsafe")
               (:file "src/lisp/engine/heartbeat")
               (:file "src/lisp/core/discord")
               (:file "src/lisp/core/governance")
               (:file "src/lisp/core/evaluator")
               (:file "src/lisp/core/meta-learning")
               (:file "src/lisp/system/opus")
               (:file "src/lisp/core/research-algorithms") ; CORE (Foundation Algorithm Library)
               
               ;; SHELL
               (:file "src/lisp/shell/notifications")
               (:file "src/lisp/shell/briefing")
               (:file "src/lisp/shell/narrative")
               (:file "src/lisp/shell/interface")
               (:file "src/lisp/shell/handoff")
               
               ;; LEGACY / DSL
               (:file "src/lisp/dsl")
               (:file "src/lisp/strategies/strategies-trend")
               (:file "src/lisp/strategies/strategies-reversion")
               (:file "src/lisp/strategies/strategies-breakout")
               (:file "src/lisp/strategies/strategies-scalp")
                (:file "src/lisp/school/school-strategy")  ; V8.1: Strategy/Clan System
                (:file "src/lisp/school/school-rank-system") ; Rank System Overhaul V47.0
                (:file "src/lisp/strategies/strategies")
                (:file "src/lisp/strategies/strategies-recruits") ; P7: Recruited Strategies
                (:file "src/lisp/strategies/legends") ; P7: Legendary Warriors
                (:file "src/lisp/strategies/legend-61") ; V3.0 (61-Strategy Signal System) revival
                
                ;; SCHOOL
                (:file "src/lisp/school/school-db")
                (:file "src/lisp/school/school-ast") ; Phase 23: Native AST Protocol
                (:file "src/lisp/school/school-cemetery")
                (:file "src/lisp/school/school-constants") ; Phase 3: Magic Numbers
                (:file "src/lisp/school/school-constitution")
                (:file "src/lisp/school/school-resignation")
                (:file "src/lisp/school/school-optimized-params") ; Phase 4: Code Gen
               (:file "src/lisp/school/school-voting")    ; V8.1: Voting/Council System
               
               ;; REFACTORED DREAMER MIGRATION (Phase 11)
               (:file "src/lisp/school/school-analytics") ; Metrics & Analysis
                (:file "src/lisp/school/school-backtest-utils")
                (:file "src/lisp/school/school-backtest")  ; Backtesting & WFV
                (:file "src/lisp/school/school-backtest-v2") ; V20.0: 2-Stage Backtest Logic
                (:file "src/lisp/school/school-ranking")     ; V20.0: Ranking Ladder (B->A->S)
               (:file "src/lisp/school/school-validation") ; P9: OOS Validation for A-RANK
               (:file "src/lisp/school/school-pruning")    ; P10: KB Pruning & Optimization
               (:file "src/lisp/school/school-monte-carlo") ; Phase 11: Monte Carlo
               (:file "src/lisp/school/school-stress")    ; P7: Deep Validation
               (:file "src/lisp/school/school-templates") ; Template Generation
               (:file "src/lisp/school/school-memory")
               (:file "src/lisp/school/school-ecosystem")
               (:file "src/lisp/school/school-genome")    ; V14.0: The Genome Engine (Sexual Reproduction)
               (:file "src/lisp/school/school-evolution") ; Genetic & LLM Generation (Dreamer)
               
               (:file "src/lisp/school/school-adaptation") ; Adaptation Engine (Phase 4)
               (:file "src/lisp/school/school-scoring")    ; Scoring Engine (Phase 5)
               (:file "src/lisp/school/school-lifecycle")  ; Lifecycle Management (Phase 6)
               (:file "src/lisp/school/school-kb")        ; P8: Single KB Entry Point
                (:file "src/lisp/school/indicators-library")
               (:file "src/lisp/school/school-p3-learning") ; P3: Advanced Learning
                (:file "src/lisp/school/school-breeder")    ; Evolution Engine (Phase 2): Breeding
               (:file "src/lisp/school/school-connector")
               (:file "src/lisp/school/school-learning")
                (:file "src/lisp/school/school-portfolio")   ; V49.6: Portfolio & Global Draft
               (:file "src/lisp/school/school-calendar")  ; V15.2: Contains market-open-p, required by school-execution
               (:file "src/lisp/school/school-volatility")
               
               (:file "src/lisp/school/school-founders") ; Headhunting Protocol
               (:file "src/lisp/school/school-founders-hunted") ; Hunted Strategies
               (:file "src/lisp/school/school-founders-dalio") ; Phase 27: Uncorrelated Return Streams
               (:file "src/lisp/school/school-telemetry") ; V9.5: Gene Kim Telemetry
               ;; (:file "src/lisp/school-hunter") ; V9.2: Loaded dynamically for safety (safely-load-hunter-strategies)
               (:file "src/lisp/school/prediction") ; RENAMED: from school-research.lisp
               (:file "src/lisp/school/school-risk")      ; SRP Refactor
               (:file "src/lisp/school/school-danger")    ; Found during Audit
               (:file "src/lisp/school/school-macro")     ; Phase 23: Dynamic Correlation
               (:file "src/lisp/school/school-dalio")     ; Phase 27: Holy Grail (Dalio)
               (:file "src/lisp/school/school-market")    ; SRP Refactor
               (:file "src/lisp/school/school-alchemy")   ; Phase 28: FX Pivoting (Carry/Vol)
               (:file "src/lisp/school/school-founders-alchemy") ; Phase 28: Hybrid Strategy
               (:file "src/lisp/school/school-founders-breakout") ; Phase 30: Staircase Breakout
               (:file "src/lisp/school/school-narrative") ; SRP Refactor
               (:file "src/lisp/school/school-allocation") ; SRP Refactor V19
               (:file "src/lisp/school/school-guards")
               (:file "src/lisp/school/school-evaluation") ; SRP Refactor
               (:file "src/lisp/school/school-pip-audit") ; Phase 28: Live Trade Audit
               (:file "src/lisp/school/school-execution") ; SRP Refactor
               (:file "src/lisp/school")                  ; Orchestrator
               (:file "src/lisp/school/school-fortress") ; Added
               (:file "src/lisp/transfer-learning") ; Added V7.0
               (:file "src/lisp/school/advisors")
               
               ;; SYSTEM & UTILS
               (:file "src/lisp/logger")
               (:file "src/lisp/core/rituals")
               (:file "src/lisp/core/comomentum") ;; V48.0: Replaced mixseek (4-clan removal)
               ;; (:file "src/lisp/research") MOVED to CORE
               (:file "src/lisp/llm-integration")
               (:file "src/lisp/evolution")
               (:file "src/lisp/error-handling")
               (:file "src/lisp/quality")
               (:file "src/lisp/repl")
               
               ;; TICK HANDLER (loads last to access all packages)
               (:file "src/lisp/core/scheduler")    ; SRP Refactor: Extracted from tick-handler
               (:file "src/lisp/core/narrative")    ; SRP Refactor: Extracted from tick-handler
               (:file "src/lisp/core/executor")
               (:file "src/lisp/core/message-dispatcher") ; SRP Refactor: New Module
               (:file "src/lisp/core/tick-handler")
               
               ;; TESTS
               (:file "src/lisp/tests")
               (:file "src/lisp/tests/backtest-db-tests")
               (:file "src/lisp/tests/backtest-payload-tests")
               
               (:file "src/lisp/tests/school-split-tests")
                (:file "src/lisp/tests/stress-test-kb")
               (:file "src/lisp/tests/school-mc-tests")   ; Phase 11: MC Tests
               (:file "src/lisp/tests/school-mismatch-tests")
               (:file "src/lisp/tests/evolution-tests")
               (:file "src/lisp/tests/integration-tests")
               
               ;; RUNNER & MAIN
               (:file "src/lisp/system/runner")
               (:file "src/lisp/main")))
