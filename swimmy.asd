
(asdf:defsystem "swimmy"
  :description "The Efficient Gardener"
  :version "41.5.0"
  :author "Antigravity Team"
  :license "Proprietary"
  :depends-on ("cl-ppcre" "pzmq" "jsown" "dexador" "local-time" "uiop" "bordeaux-threads")
  :serial t
  :components ((:file "src/lisp/packages")
               (:file "src/lisp/core/globals")
               (:file "src/lisp/core/config")
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
               (:file "src/lisp/strategies/strategies")
               
               ;; SCHOOL
               (:file "src/lisp/school/school-state")
               (:file "src/lisp/school/school-danger")
               (:file "src/lisp/school/school-constitution")
               (:file "src/lisp/school/school-resignation")
               (:file "src/lisp/school/school-strategy")  ; V8.1: Strategy/Clan System
               (:file "src/lisp/school/school-voting")    ; V8.1: Voting/Council System
               
               ;; REFACTORED DREAMER MIGRATION (Phase 11)
               (:file "src/lisp/school/school-analytics") ; Metrics & Analysis
               (:file "src/lisp/school/school-backtest")  ; Backtesting & WFV
               (:file "src/lisp/school/school-templates") ; Template Generation
               (:file "src/lisp/school/school-memory")
               (:file "src/lisp/school/school-ecosystem")
               (:file "src/lisp/school/school-genome")    ; V14.0: The Genome Engine (Sexual Reproduction)
               (:file "src/lisp/school/school-evolution") ; Genetic & LLM Generation (Dreamer)

               (:file "src/lisp/school/school-learning")
               (:file "src/lisp/school/school-calendar")  ; V15.2: Contains market-open-p, required by school-execution
               (:file "src/lisp/school/school-volatility")

               (:file "src/lisp/school/school-founders") ; Headhunting Protocol
               (:file "src/lisp/school/school-founders-hunted") ; Hunted Strategies
               (:file "src/lisp/school/school-telemetry") ; V9.5: Gene Kim Telemetry
               ;; (:file "src/lisp/school-hunter") ; V9.2: Loaded dynamically for safety (safely-load-hunter-strategies)
               (:file "src/lisp/school/prediction") ; RENAMED: from school-research.lisp
               (:file "src/lisp/school/school-risk")      ; SRP Refactor
               (:file "src/lisp/school/school-market")    ; SRP Refactor
               (:file "src/lisp/school/school-narrative") ; SRP Refactor
               (:file "src/lisp/school/school-allocation") ; SRP Refactor V19
               (:file "src/lisp/school/school-execution") ; SRP Refactor
               (:file "src/lisp/school")                  ; Orchestrator
               (:file "src/lisp/school/school-fortress") ; Added
               (:file "src/lisp/transfer-learning") ; Added V7.0
               (:file "src/lisp/school/advisors")
               
               ;; SYSTEM & UTILS
               (:file "src/lisp/logger")
               (:file "src/lisp/core/rituals")
               (:file "src/lisp/mixseek")
               ;; (:file "src/lisp/research") MOVED to CORE
               (:file "src/lisp/llm-integration")
               (:file "src/lisp/evolution")
               (:file "src/lisp/error-handling")
               (:file "src/lisp/quality")
               (:file "src/lisp/repl")
               
               ;; TICK HANDLER (loads last to access all packages)
               (:file "src/lisp/core/executor")
               (:file "src/lisp/core/tick-handler")
               
               ;; TESTS
               (:file "src/lisp/tests")

               (:file "src/lisp/tests/school-split-tests")
               (:file "src/lisp/tests/evolution-tests")
               (:file "src/lisp/tests/integration-tests")
               
               ;; RUNNER & MAIN
               (:file "src/lisp/system/runner")
               (:file "src/lisp/main")))
