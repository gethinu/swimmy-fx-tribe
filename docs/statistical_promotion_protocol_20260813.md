# Statistical promotion protocol

**Effective configuration:** systemd `swimmy-brain` and `swimmy-guardian`
services set the following values.

```text
SWIMMY_STATISTICAL_PROMOTION_GATES=1
SWIMMY_ENABLE_REAL_DSR=1
SWIMMY_S_RANK_PBO_MAX=0.25
SWIMMY_CPCV_REFIT=1
SWIMMY_CPCV_PBO=1
SWIMMY_BOOTSTRAP_MOVING_BLOCK=1
```

## S-rank decision contract

A candidate must pass the existing numerical S criteria and Common Stage 2.
It must then also prove all of the following:

1. A real Deflated Sharpe Ratio is estimable from per-trade PnL and is at least
   `0.95`.  Insufficient history fails closed; it does not use the legacy
   Sharpe-floor fallback.
2. Its latest CPCV result was produced with `cpcv_refit=true`.  This confirms
   every path selected parameters only on purged/embargoed train ranges before
   scoring the held-out test range.
3. CSCV PBO is present and no greater than `0.25`.

All S entry points—async CPCV result handling, scheduled rank evaluation,
manual/direct `ensure-rank`, and S-rank conformance—use the same predicate.
The requirement is deliberately S-only: it never enables an order or replaces
the independent `deployment_gate_status` decision.

## Persistence and audit

The `strategies` table stores `cpcv_pbo` and `cpcv_refit`.  A new CPCV result
that lacks either proof clears it, so an older favourable result cannot certify
a later legacy rerun. `data/logs/cpcv_statistical_history.csv` records the PBO
and refit provenance of every received CPCV response; the original
`cpcv_history.csv` schema is unchanged.

## Applying on the Linux host

After deploying this checkout and rebuilding Guardian, reload the units and
restart both processes:

```bash
sudo systemctl daemon-reload
sudo systemctl restart swimmy-guardian swimmy-brain
```

Use `journalctl -u swimmy-guardian -u swimmy-brain -n 200` to confirm CPCV
logs show per-fold refit/PBO and that S blocks include an actionable `pbo`,
`cpcv-refit`, or `deflated-sharpe` reason when applicable.
