.PHONY: test run clean quality-gate setup

setup:
	@echo "📦 Installing pre-commit hook..."
	@cp hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Pre-commit hook installed"

run:
	@echo "🧹 Cleaning up old processes (Hard Kill)..."
	@-pkill -9 -f "brain.lisp" 2>/dev/null || true
	@-pkill -9 -f "guardian" 2>/dev/null || true
	@-pkill -9 -f "data_keeper.py" 2>/dev/null || true
	@-pkill -9 -f "notifier.py" 2>/dev/null || true
	@-pkill -9 -f "risk_gateway.py" 2>/dev/null || true
	@-pkill -9 -f "backtest_service.py" 2>/dev/null || true
	@-pkill -9 -f "inference_worker.py" 2>/dev/null || true
	@-fuser -k 5555/tcp 2>/dev/null || true
	@-fuser -k 5556/tcp 2>/dev/null || true
	@-fuser -k 5557/tcp 2>/dev/null || true
	@-fuser -k 5559/tcp 2>/dev/null || true
	@-fuser -k 5560/tcp 2>/dev/null || true
	@-fuser -k 5561/tcp 2>/dev/null || true
	@-fuser -k 5562/tcp 2>/dev/null || true
	@-fuser -k 5563/tcp 2>/dev/null || true
	@-fuser -k 5564/tcp 2>/dev/null || true
	@-fuser -k 5580/tcp 2>/dev/null || true
	@echo "⏳ Waiting for ports to clear..."
	@sleep 2
	@echo "🚀 Starting Guardian..."
	@nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
	@echo "🚀 Starting Data Keeper..."
	@nohup .venv/bin/python3 tools/data_keeper.py > /tmp/data_keeper.log 2>&1 &
	@echo "⏳ Waiting for Data Keeper to load..."
	@sleep 5
	@echo "🚀 Starting Brain..."
	@/bin/bash -c "source config/.env && sbcl --noinform --load brain.lisp > /tmp/brain.log 2>&1 & PID=\$$!; echo \"Brain PID: \$$PID\"; tail -f /tmp/brain.log --pid=\$$PID"

test:
	@echo "🧪 Running Swimmy Tests..."
	sbcl --script test_runner.lisp
	@echo "🦀 Running Rust Tests..."
	cd guardian && cargo test --release

# Quality Gate: Must pass before any deployment (memo3.txt Section 5)
quality-gate: test
	@echo "✅ Quality Gate PASSED - Ready for deployment"

# Integration Tests (Naval Modularization)
integration-test:
	@echo "🔗 Running Integration Tests..."
	@sbcl --noinform --load swimmy.asd --eval '(asdf:load-system :swimmy)' \
		--eval '(swimmy.tests:run-integration-tests)' --quit

clean:
	rm -rf ~/.cache/common-lisp/ || true
	rm -f *.fasl

# Benchmarks
run-benchmarks:
	@echo "🧹 Cleaning up old processes..."
	@-pkill -9 sbcl 2>/dev/null || true
	@-pkill -9 guardian 2>/dev/null || true
	@-fuser -k 5555/tcp 2>/dev/null || true
	@-fuser -k 5556/tcp 2>/dev/null || true
	@-fuser -k 5557/tcp 2>/dev/null || true
	@-fuser -k 5560/tcp 2>/dev/null || true
	@truncate -s 0 /tmp/guardian.log
	@echo "🚀 Starting Guardian (Release)..."
	@nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
	@sleep 2
	@echo "📊 Running Benchmarks..."
	@./ci-test.sh
	@echo "🧪 Running Lisp Benchmarks..."
	@sbcl --noinform --load brain.lisp --load src/lisp/benchmark.lisp --eval '(swimmy.school:run-all-benchmarks)' --quit
