.PHONY: test run clean quality-gate setup

setup:
	@echo "📦 Installing pre-commit hook..."
	@cp hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Pre-commit hook installed"

run:
	@echo "🧹 Cleaning up old processes..."
	@-pkill -9 sbcl 2>/dev/null || true
	@-pkill -9 guardian 2>/dev/null || true
	@-fuser -k 5555/tcp 2>/dev/null || true
	@-fuser -k 5556/tcp 2>/dev/null || true
	@-fuser -k 5557/tcp 2>/dev/null || true
	@-fuser -k 5560/tcp 2>/dev/null || true
	@echo "🚀 Starting Guardian..."
	@nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
	@sleep 2
	@echo "🚀 Starting Discord Bot..."
	@nohup /bin/bash -c "source config/.env && .venv/bin/python3 src/python/discord_bot.py" > /tmp/discord_bot.log 2>&1 &
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
