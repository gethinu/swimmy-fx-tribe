
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
               (:file "src/lisp/dreamer2")
               (:file "src/lisp/strategies")
               
               ;; SCHOOL
               (:file "src/lisp/school-state")
               (:file "src/lisp/school-danger")
               (:file "src/lisp/school-constitution")
               (:file "src/lisp/school-resignation")
               (:file "src/lisp/school-strategy")  ; V8.1: Strategy/Clan System
               (:file "src/lisp/school-voting")    ; V8.1: Voting/Council System
               (:file "src/lisp/school-evolution") ; V8.0: Genetic Mutation
               (:file "src/lisp/school-learning")
               (:file "src/lisp/school-volatility")
               (:file "src/lisp/school/prediction") ; RENAMED: from school-research.lisp
               (:file "src/lisp/school")
               (:file "src/lisp/school-fortress") ; Added
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
