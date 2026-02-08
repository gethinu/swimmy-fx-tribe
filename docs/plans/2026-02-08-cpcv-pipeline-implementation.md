# CPCV Pipeline Recovery Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore CPCV validation end-to-end and harden strategy loading, with a success-case CPCV unit test and verified pipeline restart.

**Architecture:** Guardian handles `CPCV_VALIDATE` and emits `CPCV_RESULT` from a shared payload builder. Lisp tolerates unknown strategy keys and skips invalid entries with logging. Integration uses cherry-picks from existing fix branches. Runtime verification confirms CPCV queue activity and results.

**Tech Stack:** Rust (guardian), Common Lisp (core), SQLite, systemd, shell tooling

### Task 1: Guardian CPCV Success-Payload Test (TDD)

**Files:**
- Modify: `guardian/src/main.rs`

**Step 1: Write the failing test**

Add below the existing CPCV test in the `#[cfg(test)]` module:

```rust
    #[test]
    fn test_cpcv_payload_success() {
        let agg = cpcv::CpcvAggregateResult {
            median_sharpe: 0.75,
            median_pf: 1.3,
            median_wr: 55.0,
            median_maxdd: 0.12,
            std_sharpe: 0.2,
            path_count: 10,
            passed_count: 6,
        };

        let payload = cpcv_payload_from_aggregate("UT-CPCV-SUCCESS", &agg);

        assert_eq!(payload.strategy_name, "UT-CPCV-SUCCESS");
        assert_eq!(payload.path_count, 10);
        assert_eq!(payload.passed_count, 6);
        assert_eq!(payload.failed_count, 4);
        assert!((payload.pass_rate - 0.6).abs() < 1e-9);
        assert!(payload.is_passed);
        assert!(payload.error.is_none());
    }
```

**Step 2: Run test to verify it fails**

Run:
```bash
cd /home/swimmy/swimmy/guardian
cargo test --release test_cpcv_payload_success -- --nocapture
```
Expected: FAIL with missing function `cpcv_payload_from_aggregate`.

**Step 3: Write minimal implementation**

Add this helper near `build_cpcv_result`:

```rust
fn cpcv_payload_from_aggregate(
    strategy_name: &str,
    agg: &cpcv::CpcvAggregateResult,
) -> CpcvResultPayload {
    let path_count = agg.path_count;
    let passed_count = agg.passed_count;
    let failed_count = path_count.saturating_sub(passed_count);
    let pass_rate = if path_count > 0 {
        passed_count as f64 / path_count as f64
    } else {
        0.0
    };
    let is_passed = agg.median_sharpe >= 0.5 && pass_rate >= 0.5;

    CpcvResultPayload {
        strategy_name: strategy_name.to_string(),
        median_sharpe: agg.median_sharpe,
        path_count,
        passed_count,
        failed_count,
        pass_rate,
        is_passed,
        error: None,
    }
}
```

**Step 4: Wire helper into existing flow**

Replace the `Ok(agg)` branch in `build_cpcv_result` with:

```rust
        Ok(agg) => cpcv_payload_from_aggregate(&_req.strategy_name, &agg),
```

**Step 5: Run test to verify it passes**

Run:
```bash
cargo test --release test_cpcv_payload_success -- --nocapture
```
Expected: PASS.

**Step 6: Run CPCV test group**

Run:
```bash
cargo test --release test_cpcv_validate_handler_error_for_missing_file -- --nocapture
```
Expected: PASS.

**Step 7: Commit**

```bash
git add guardian/src/main.rs
git commit -m "test: add CPCV success payload helper"
```

### Task 2: Integrate Guardian Fixes to Default Branch

**Files:**
- Modify: `guardian/src/main.rs`

**Step 1: Identify default branch**

```bash
cd /home/swimmy/swimmy/guardian
git branch --show-current
```
If not on default branch, switch to it (usually `master` or `main`).

**Step 2: Cherry-pick CPCV commits**

Cherry-pick the existing CPCV handler commit plus the new test commit from Task 1:

```bash
git cherry-pick b5ab70c <NEW_TEST_COMMIT_SHA>
```
Expected: clean cherry-pick.

**Step 3: Verify build**

```bash
cargo test --release test_cpcv_validate_handler_error_for_missing_file -- --nocapture
cargo test --release test_cpcv_payload_success -- --nocapture
```
Expected: both PASS.

### Task 3: Integrate Lisp Fixes to Swimmy Main Repo

**Files:**
- Modify: `src/lisp/core/persistence.lisp`
- Modify: `src/lisp/strategies/strategies.lisp`
- Modify: `src/lisp/tests.lisp`

**Step 1: Cherry-pick Lisp fixes from worktree branch**

```bash
cd /home/swimmy/swimmy
git cherry-pick 90f5d46 02a3c30
```
Expected: clean cherry-pick, no conflicts with local LEGEND changes.

**Step 2: Verify Lisp tests**

```bash
scripts/ci-test.sh
```
Expected: `✅ ALL TESTS PASSED` (164 tests).

### Task 4: Restart and Verify CPCV Pipeline

**Files:**
- No code changes

**Step 1: Confirm running processes and ports**

```bash
ps aux | rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"
ss -tulnp | rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"
```
Expected: identify any existing guardian/brain processes to avoid double-starts.

**Step 2: Restart services (choose one path)**

If using systemd system services:
```bash
sudo systemctl restart swimmy-guardian swimmy-evolution
```

If using user services:
```bash
systemctl --user restart swimmy-guardian swimmy-evolution
```

**Step 3: Verify CPCV activity**

```bash
cd /home/swimmy/swimmy
cat data/reports/evolution_factory_report.txt | rg -n "CPCV Status|queued|received"
rg -n "CPCV" logs/guardian.log | tail -40
```
Expected: CPCV queued/received counts increase and guardian logs show CPCV handling.
