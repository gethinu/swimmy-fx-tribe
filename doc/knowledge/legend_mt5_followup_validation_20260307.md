# MT5 Followup Validation 2026-03-07

## Scope

This note extends the earlier short-window MT5 inventory run with:

- yearly validation for `2024` and `2025`
- fixed OOS half-year windows across `2024H1`, `2024H2`, `2025H1`, `2025H2`
- parameter/behavior audit for the two weakest Legend ports:
  - `legend-london-breakout-v1`
  - `legend-rsi-reversion-v1`

Artifacts used:

- short-window baseline: `data/reports/mt5/inventory_tester/run_20260307_064816`
- 24-month rerun: `data/reports/mt5/inventory_tester/run_20260307_070330`
- yearly windows:
  - `data/reports/mt5/inventory_tester/run_20260307_071613`
  - `data/reports/mt5/inventory_tester/run_20260307_071724`
- fixed OOS half-year windows:
  - `data/reports/mt5/inventory_tester/run_20260307_071842`
  - `data/reports/mt5/inventory_tester/run_20260307_071952`
  - `data/reports/mt5/inventory_tester/run_20260307_072101`
  - `data/reports/mt5/inventory_tester/run_20260307_072157`
- latest-window rerun:
  - `data/reports/mt5/inventory_tester/run_20260307_101456`

Coverage note:

- `run_20260307_064816` is the baseline isolated tester run for all `22` active jobs
  (`9` legend + `13` historical S).
- This followup note is not the first-pass execution record. It only deepens validation for the
  shortlist that survived the baseline screen or needed behavioral audit.
- `run_20260307_101456` adds a fresh latest window from `2026.01.01` to `2026.03.07` for the
  three remaining shortlist candidates.

## Yearly Validation

The earlier run that looked like "3 months" was actually about 2 months: `2025.01.01` to `2025.03.01`.

The four short-window winners were rerun over full calendar years:

| Job | 2024 net | 2024 PF | 2024 trades | 2025 net | 2025 PF | 2025 trades |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `historical-s-bred940-trend-core` | `109.54` | `1.22` | `145` | `-41.45` | `0.91` | `111` |
| `legend-macd-above-zero-cross` | `31.73` | `1.76` | `49` | `10.97` | `1.26` | `41` |
| `legend-macd-signal-cross` | `1.77` | `1.01` | `123` | `-12.20` | `0.91` | `119` |
| `legend-pullback-breakout` | `-10.18` | `0.97` | `421` | `80.97` | `1.22` | `425` |

Yearly conclusion:

- `legend-macd-above-zero-cross` is the only candidate that stayed positive in both years.
- `historical-s-bred940-trend-core` is a strong `2024` candidate that decays in `2025`.
- `legend-pullback-breakout` is the opposite regime story: weak in `2024`, strong in `2025`.
- `legend-macd-signal-cross` is too thin to treat as stable.

## Fixed OOS Half-Year Windows

The same four candidates were tested over four non-overlapping six-month windows:

- `2024H1`: `2024.01.01` to `2024.06.30`
- `2024H2`: `2024.07.01` to `2024.12.31`
- `2025H1`: `2025.01.01` to `2025.06.30`
- `2025H2`: `2025.07.01` to `2025.12.31`

Half-year results:

| Job | 2024H1 | 2024H2 | 2025H1 | 2025H2 |
| --- | ---: | ---: | ---: | ---: |
| `historical-s-bred940-trend-core` | `30.02` | `83.17` | `-34.22` | `-12.47` |
| `legend-macd-above-zero-cross` | `14.51` | `17.22` | `5.77` | `6.95` |
| `legend-macd-signal-cross` | `-2.24` | `4.01` | `-17.81` | `11.40` |
| `legend-pullback-breakout` | `-13.91` | `3.73` | `61.09` | `22.37` |

Stability ranking over fixed OOS windows:

| Rank | Job | Positive windows | Total net | Median net | Mean PF | Total trades |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | `legend-macd-above-zero-cross` | `4/4` | `44.45` | `10.73` | `1.56` | `89` |
| 2 | `legend-pullback-breakout` | `3/4` | `73.28` | `13.05` | `1.09` | `842` |
| 3 | `historical-s-bred940-trend-core` | `2/4` | `66.50` | `8.77` | `1.06` | `259` |
| 4 | `legend-macd-signal-cross` | `2/4` | `-4.64` | `0.88` | `1.01` | `240` |

OOS conclusion:

- `legend-macd-above-zero-cross` is the cleanest survivor. It won in every half-year window.
- `legend-pullback-breakout` has the largest gross upside after `legend-macd-above-zero-cross`, but that upside is dominated by `2025`.
- `historical-s-bred940-trend-core` is not robust enough to survive a stability-first screen.
- `legend-macd-signal-cross` should not be treated as a stable winner.

## Latest Window: `2026.01.01` to `2026.03.07`

The shortlist was rerun once more on the most recent available window:

| Job | Net | PF | Sharpe | Trades |
| --- | ---: | ---: | ---: | ---: |
| `historical-s-bred940-trend-core` | `57.35` | `2.30` | `3.01` | `22` |
| `legend-macd-above-zero-cross` | `-1.67` | `0.66` | `-0.31` | `4` |
| `legend-pullback-breakout` | `-20.70` | `0.72` | `-4.30` | `82` |

Latest-window conclusion:

- `legend-macd-above-zero-cross` stays the primary stability-first survivor overall, but the
  latest window only produced `4` trades, so this rerun is too thin to overrule the stronger
  2024/2025 fixed-window evidence.
- `legend-pullback-breakout` stayed negative in early `2026`, which reinforces the earlier
  conclusion that it is regime-dependent rather than a stable always-on winner.
- `historical-s-bred940-trend-core` recovered sharply in early `2026`, so it should be treated as
  a live regime candidate, not as a dead line item. That said, one positive latest window still
  does not erase its `2025` decay or promote it to the top stability-first slot.

## Parameter Audit: `legend-london-breakout-v1`

Code contract:

- timeframe `H1`
- symbol defaults to `USDJPY`
- range window `08:00-09:00`
- breakout only after the range closes
- one trade per day
- force-close at `16:00`
- fixed stop/target:
  - `SL = 0.20`
  - `TP = 0.60`

Reference:

- `src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5`

Observed tester behavior from the short-window report:

- entries: `38`
- average entries per active day: `1.00`
- max entries per day: `1`
- entry hours: concentrated in `10:00-15:00`
- exit reasons:
  - `sl`: `26`
  - `manual`: `10`
  - `tp`: `2`

Audit conclusion:

- The MT5 port is behaving according to the intended time gating and one-trade-per-day contract.
- The strategy is not failing because of obvious port corruption.
- The weakness is the strategy itself in the tested regime: too many breakouts resolve to stop-outs, with only two take-profit exits in the short-window baseline.

## Parameter Audit: `legend-rsi-reversion-v1`

Code contract:

- timeframe `M5`
- symbol defaults to `USDJPY`
- `RSI(2)`
- entry thresholds:
  - long when `RSI < 10`
  - short when `RSI > 90`
- exit thresholds are symmetric
- fixed stop/target:
  - `SL = 0.10`
  - `TP = 0.10`

Reference:

- `src/mt5/legend_batch3/Legend_RSIReversionV1.mq5`

Observed tester behavior from the short-window report:

- entries: `1623`
- average entries per active day: `38.64`
- max entries per day: `52`
- entries distributed over all `24` hours
- side count:
  - long entries: `827`
  - short entries: `796`
- exit reasons:
  - `sl`: `793`
  - `tp`: `618`
  - `manual`: `192`
  - `end`: `1`

Audit conclusion:

- The MT5 port is directionally symmetric. There is no evidence that one side is broken or missing.
- The strategy is simply too active for the tested market and parameter set.
- With `RSI(2)` on `M5` and fixed price exits, the edge is too thin relative to turnover and trading friction.

## Decision

If the ranking emphasizes stability over raw short-window upside:

1. Keep `legend-macd-above-zero-cross` as the primary validated survivor, with the caveat that
   the fresh `2026.01.01-2026.03.07` rerun is sample-thin.
2. Treat `legend-pullback-breakout` as a regime-dependent secondary candidate.
3. Treat `historical-s-bred940-trend-core` as a regime candidate that recovered in early `2026`,
   but is still not globally stable enough to outrank the primary Legend survivor.
4. Drop `legend-macd-signal-cross` from any stability-first shortlist.

If the objective changes from stability-first to regime rotation, `legend-pullback-breakout` and `historical-s-bred940-trend-core` remain interesting, but they should not be promoted as unconditional winners.

## Remaining Risks

- Baseline isolated tester execution and the `timeframe=3600 -> H1` reproduction contract are
  already covered. The open risk is robustness, not first-pass portability.
- These are fixed-window and latest-window reruns, but not full walk-forward optimization cycles.
- No parameter retuning was performed between windows.
- The fresh `2026.01.01-2026.03.07` window is especially thin for `legend-macd-above-zero-cross`
  (`4` trades), so that rerun alone cannot carry a final deployment decision.
- The conclusions are based on MT5 tester results for the current portable environment and symbol
  setup, not broker-by-broker deployment variance.
