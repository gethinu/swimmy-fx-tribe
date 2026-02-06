# Noncorrelated Strategy Discord Notification Design

Date: 2026-02-06

## Goal
Send a Discord notification when a strategy is promoted to A/S rank, including a Dalio-style noncorrelation score computed from live (trade_logs) PnL.

## Non-Goals
- Real-time streaming correlation updates.
- Backtest-based correlation scoring.
- Changing rank criteria or promotion logic.

## Decisions (Confirmed)
- Trigger: A/S rank promotion.
- Discord webhook: `SWIMMY_DISCORD_REPORTS`.
- Message content: standard fields (strategy name, new rank, correlation score or N/A, threshold, portfolio size, timestamp).
- Data source: live trade logs (SQLite `trade_logs`).
- PnL aggregation: daily totals, 30 most recent trading days (days with trades).
- Insufficient data: notify with `N/A` (do not block promotion).
- Approach: daily batch aggregation + promotion-time lookup.
- Batch timing: 00:10 JST daily.

## Architecture Overview
1) **Daily PnL batch**
- New SQLite table `strategy_daily_pnl`.
- Daily job aggregates `trade_logs` into per-strategy daily PnL.
- Idempotent upsert by `(strategy_name, trade_date)`.

2) **Correlation scoring on promotion**
- When `ensure-rank` (or rank system) promotes B->A or A->S, compute correlation score.
- Use `strategy_daily_pnl` for the promoted strategy and the current portfolio set.
- Compute Pearson correlations and score = (uncorrelated pairs / total pairs), using abs(corr) < 0.2.
- If data insufficient, score is `N/A` and include reason.

3) **Discord notification**
- Send via existing Notifier (`queue-discord-notification`) to `SWIMMY_DISCORD_REPORTS`.
- Payload fields:
  - Strategy name
  - New rank (A or S)
  - Noncorrelation score (or N/A)
  - Threshold (0.2)
  - Portfolio size
  - Timestamp (JST)

## Data Model
New table in `swimmy.db`:
```
strategy_daily_pnl (
  strategy_name TEXT NOT NULL,
  trade_date TEXT NOT NULL,         -- YYYY-MM-DD (JST)
  pnl_sum REAL NOT NULL,
  trade_count INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (strategy_name, trade_date)
)
```

## Data Flow
- **Batch**: `trade_logs` -> daily aggregation -> `strategy_daily_pnl`.
  - `trade_logs.timestamp` is converted to JST date.
  - Upsert ensures reruns remain correct.
- **Promotion**:
  - Collect promoted strategy + portfolio strategies.
  - Load last 30 trading days PnL vectors.
  - Compute correlation score or `N/A` if missing.
  - Send Discord notification.

## Error Handling
- Batch failures (DB lock, schema missing, SQL errors) log and exit; next run will retry.
- Promotion scoring failures do not block promotion; notify with `N/A` and reason.

## Testing
1) **Daily aggregation unit test**: validate daily rollup and upsert behavior.
2) **Correlation unit test**: known vectors yield expected correlations and score.
3) **Promotion integration**: promotion triggers exactly one notification with required fields.

## Operational Notes
- Log batch start/end and row counts to `logs/brain.out` (minimal verbosity).
- Ensure daily job is scheduled (core scheduler or equivalent) at 00:10 JST.
