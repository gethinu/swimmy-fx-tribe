.PHONY: test run clean quality-gate setup

setup:
	@echo "📦 Installing pre-commit hook..."
	@cp hooks/pre-commit .git/hooks/pre-commit
	@chmod +x .git/hooks/pre-commit
	@echo "✅ Pre-commit hook installed"

# Kill all swimmy-related processes to prevent zombies
kill-zombies:
	@echo "🧟 Killing zombie processes..."
	@-pkill -9 -u $(USER) -f "sbcl" 2>/dev/null || true
	@-pkill -9 -u $(USER) -f "guardian" 2>/dev/null || true
	@-pkill -9 -u $(USER) -f "data_keeper.py" 2>/dev/null || true
	@-pkill -9 -u $(USER) -f "notifier.py" 2>/dev/null || true
	@-pkill -9 -u $(USER) -f "evolution_daemon.py" 2>/dev/null || true
	@systemctl --user reset-failed

# Systemd Management
# Run = Restart to ensure changes are applied
run: kill-zombies
	@echo "🔄 Restarting Swimmy System (via systemd)..."
	@systemctl --user daemon-reload
	@systemctl --user restart swimmy-brain swimmy-guardian swimmy-notifier swimmy-data-keeper strategy_hunter swimmy-evolution
	@echo "✅ System restarted. Use 'make status' to check health."

stop:
	@echo "🛑 Stopping Swimmy System..."
	@systemctl --user stop swimmy-brain swimmy-guardian swimmy-notifier swimmy-data-keeper
	@echo "✅ System stopped."

status:
	@systemctl --user status swimmy-brain swimmy-guardian swimmy-notifier swimmy-data-keeper

# CLI Dashboard (Expert Panel 2026-01-10)
dashboard:
	@python3 tools/dashboard.py

# Logs (Tail all logs)
logs:
	@echo "📜 Tailing logs (Ctrl+C to exit)..."
	@journalctl --user -f -u swimmy-brain -u swimmy-guardian -u swimmy-notifier

test:
	@echo "🧪 Running Swimmy Tests..."
	sbcl --script test_runner.lisp
	@echo "🦀 Running Rust Tests..."
	cd guardian && cargo test --release

# System Integrity Check (Quality Gate 2.0)
integrity:
	@echo "🛡️ Running System Integrity Checks..."
	@./tools/check_integrity.sh

# Quality Gate: Must pass before any deployment (memo3.txt Section 5)
quality-gate: test integrity
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
	@-fuser -k 5557/tcp 2>/dev/null || true
	@-fuser -k 5560/tcp 2>/dev/null || true
	@-pkill -9 -f "strategy_hunter.py" 2>/dev/null || true
	@-pkill -9 -f "pending_manager.py" 2>/dev/null || true
	@truncate -s 0 /tmp/guardian.log
	@echo "🚀 Starting Guardian (Release)..."
	@nohup ./guardian/target/release/guardian > /tmp/guardian.log 2>&1 &
	@sleep 2
	@echo "📊 Running Benchmarks..."
	@./ci-test.sh
	@echo "🧪 Running Lisp Benchmarks..."
	@sbcl --noinform --load brain.lisp --load src/lisp/benchmark.lisp --eval '(swimmy.school:run-all-benchmarks)' --quit

# Strategies Lineage (Genealogy)
lineage:
	@sbcl --noinform --load tools/show_lineage.lisp

status:
	@python3 tools/report_status.py
