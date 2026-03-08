# MT5 Followup Validation 2026-03-07

## Scope

This note extends the earlier short-window MT5 inventory run with:

- yearly validation for `2024` and `2025`
- fixed OOS half-year windows across `2024H1`, `2024H2`, `2025H1`, `2025H2`
- rolling `180d` windows stepped by `90d` across `2024-2025`
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
- rolling `180d` windows stepped by `90d`:
  - `data/reports/mt5/inventory_tester/run_20260307_105536`
  - `data/reports/mt5/inventory_tester/run_20260307_105621`
  - `data/reports/mt5/inventory_tester/run_20260307_105702`
  - `data/reports/mt5/inventory_tester/run_20260307_105741`
  - `data/reports/mt5/inventory_tester/run_20260307_105823`
  - `data/reports/mt5/inventory_tester/run_20260307_105907`
  - `data/reports/mt5/inventory_tester/run_20260307_105948`
- broker-variance probe:
  - `data/reports/mt5/inventory_tester_spreadprobe/run_20260307_110931`
  - `data/reports/mt5/inventory_tester_spreadprobe/run_20260307_110958`
- latest-window rerun:
  - `data/reports/mt5/inventory_tester/run_20260307_101456`
- full walk-forward optimization:
  - `data/reports/mt5/legend_walkforward/run_20260307_113707`

Coverage note:

- `run_20260307_064816` is the baseline isolated tester run for all `22` active jobs
  (`9` legend + `13` historical S).
- This followup note is not the first-pass execution record. It only deepens validation for the
  shortlist that survived the baseline screen or needed behavioral audit.
- `run_20260307_101456` adds a fresh latest window from `2026.01.01` to `2026.03.07` for the
  three remaining shortlist candidates.
- `run_20260307_113707` adds a minimum `3`-fold MT5 walk-forward optimization cycle for the same
  three survivors, using MT5 built-in `ForwardMode=2` (`1/3` OOS) plus narrow bounded parameter
  ranges per strategy.

## Progress Map

This document is no longer trying to answer "can these strategies be ported to MT5?".
That question is already closed.

The current stage is:

1. preserve the MT5 evidence chain
2. lock the shortlist ordering
3. close only the remaining robustness gaps

What is already done:

- named Legend shortlist and historical `:S` batch have been ported to MQ5
- surviving artifacts compile successfully in MT5
- baseline isolated tester runs are complete
- yearly / half-year / rolling-window reruns are complete
- walk-forward reruns are complete
- broader-retune follow-up is complete for the surviving shortlist, except for a true full-wide
  `historical-s-bred940-trend-core` sweep that hit the current runtime ceiling

What is already fixed as the working conclusion:

- primary stability-first line: `legend-macd-above-zero-cross`
- second adaptive contender: `historical-s-bred940-trend-core`
- third adaptive contender: `legend-pullback-breakout`

What is not the objective anymore:

- adding more MQ5 ports
- reopening the top slot with fresh ad hoc candidate churn
- treating fixed-parameter short-window winners as sufficient by themselves

What remains only as residual risk or optional follow-up:

- true broker-variance replay on an actually independent source
- optional full-wide `historical-s-bred940-trend-core` sweep if runtime budget is explicitly approved
  (`run_20260307_132228_b940_medium` is the freeze-era canonical fallback, so this is not needed to close the batch)
- cleanup / commit-boundary decisions are already documented in
  `doc/knowledge/legend_mt5_cleanup_boundary_20260307.md`

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

## Rolling `180d` Windows (step `90d`)

To reduce dependence on calendar boundaries, the three remaining shortlist candidates were rerun on
seven overlapping `180d` windows stepped every `90d` from `2024.01.01` through `2025.12.20`.

Rolling-window ranking:

| Rank | Job | Positive windows | Total net | Median net | Mean PF | Total trades |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | `legend-macd-above-zero-cross` | `6/7` | `85.17` | `14.26` | `1.65` | `162` |
| 2 | `historical-s-bred940-trend-core` | `5/7` | `177.93` | `31.11` | `1.11` | `466` |
| 3 | `legend-pullback-breakout` | `5/7` | `145.86` | `6.65` | `1.10` | `1486` |

Rolling-window conclusion:

- `legend-macd-above-zero-cross` remains the cleanest stability-first survivor. It stayed positive
  in `6/7` windows and kept the strongest mean PF by a wide margin.
- `legend-pullback-breakout` still looks regime-dependent. It won `5/7` windows, but most of its
  gross upside came from a narrower `2025` cluster, and the fresh early-`2026` rerun stayed
  negative.
- `historical-s-bred940-trend-core` remains a regime candidate rather than a primary stable winner.
  The rolling windows confirm real upside, but they also show a two-window `2025` drawdown pocket
  that the latest `2026` recovery alone does not erase.

## Full Walk-Forward Optimization (`3` folds, `Forward=1/3`)

The shortlist was then rerun through a minimum `3`-fold MT5 walk-forward cycle:

1. `2024.01.01` to `2024.12.31`
2. `2024.07.01` to `2025.06.30`
3. `2025.03.01` to `2026.03.07`

Execution contract:

- MT5 built-in forward optimization:
  - `Optimization=2`
  - `ForwardMode=2`
  - `Model=4`
- artifact root:
  - `data/reports/mt5/legend_walkforward/run_20260307_113707`
- optimized inputs were deliberately narrow and human-auditable:
  - `legend-macd-above-zero-cross`: stop/target + MACD periods
  - `legend-pullback-breakout`: stop/target + EMA period + RSI thresholds
  - `historical-s-bred940-trend-core`: stop/target + MA periods + RSI thresholds

One execution note matters for reproducibility: the `fold2` MACD launch triggered an MT5 build
update and automatic relaunch. Completion was therefore judged by artifact presence
(`.xml` + `.forward.xml`), not by the first terminal exit alone.

Walk-forward ranking by quality-first metrics:

| Rank | Job | Passing folds | Total forward profit | Median forward profit | Mean PF | Mean Sharpe | Total forward trades |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | `legend-macd-above-zero-cross` | `3/3` | `53.84` | `19.22` | `2.27` | `4.07` | `47` |
| 2 | `historical-s-bred940-trend-core` | `3/3` | `272.31` | `72.60` | `1.75` | `2.89` | `149` |
| 3 | `legend-pullback-breakout` | `3/3` | `131.17` | `41.88` | `1.46` | `4.76` | `346` |

Walk-forward conclusion:

- All three survivors passed all `3` forward folds under bounded retuning. That means the
  fixed-parameter weak windows are not sufficient to eliminate any of them once per-fold
  re-optimization is allowed.
- `legend-macd-above-zero-cross` still looks like the cleanest stability-first line. It kept the
  strongest mean PF and never produced a weak fold, even though its trade count stayed thin.
- `historical-s-bred940-trend-core` upgraded materially. It is no longer just an early-`2026`
  recovery story; under walk-forward retuning it produced the strongest forward profit total by a
  wide margin.
- `legend-pullback-breakout` also passed `3/3`, so the strict "regime-dependent only" label is too
  harsh once walk-forward retuning is allowed. The better statement is that it is an adaptive
  contender with more turnover and lower quality metrics than the other two.
- The fixed-parameter and retuned conclusions are intentionally different:
  - fixed-parameter reruns still favor `legend-macd-above-zero-cross`
  - retuned walk-forward reruns keep all three names alive, with `historical-s-bred940-trend-core`
    showing the most forward profit and `legend-macd-above-zero-cross` the cleanest quality profile

## Broader Retune Follow-Up (`2026-03-07`)

To test whether the narrow-bounded walk-forward ranges were hiding better parameter regions, a
second pass was run with broader search ranges:

- artifact roots:
  - `data/reports/mt5/legend_walkforward/run_20260307_123443_wide`
  - `data/reports/mt5/legend_walkforward/run_20260307_132228_b940_medium`
- contract:
  - same `3` folds
  - same MT5 built-in `ForwardMode=2` / `1/3` OOS split
- profile note:
  - `legend-macd-above-zero-cross` and `legend-pullback-breakout` completed a true wider retune
  - `historical-s-bred940-trend-core` was started on the same wide grid, but `fold1` only reached
    `20%` after roughly `10` minutes, which projected about `50` minutes per fold in the current
    portable environment; that sweep was aborted and rerun with a medium profile so the comparison
    could be closed

Broader-retune comparison versus the narrow walk-forward baseline:

| Job | Follow-up profile | Passing folds | Total forward profit | Delta vs narrow | Mean PF | Delta vs narrow | Total forward trades |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `legend-macd-above-zero-cross` | `wide` | `3/3` | `116.34` | `+62.50` | `2.9664` | `+0.6972` | `46` |
| `legend-pullback-breakout` | `wide` | `3/3` | `213.03` | `+81.86` | `1.6812` | `+0.2234` | `389` |
| `historical-s-bred940-trend-core` | `medium` | `3/3` | `237.99` | `-34.32` | `1.8208` | `+0.0682` | `141` |

Broader-retune conclusion:

- `legend-macd-above-zero-cross` strengthened its lead. The wider sweep improved both forward
  profit and mean PF while keeping trade count effectively unchanged.
- `legend-pullback-breakout` also improved materially under the wider search, but it still carried
  the highest turnover and its quality metrics remained below the top two lines.
- `historical-s-bred940-trend-core` stayed alive on all `3` folds under the medium fallback, but
  the broader search did not unlock more forward profit than the original narrow-bounded run.
  That means the narrow profile was already close to its profitable neighborhood; the open question
  is now runtime cost, not obvious under-search.
- For freeze closeout, `run_20260307_132228_b940_medium` is the canonical artifact for
  `historical-s-bred940-trend-core`. A true full-wide replay remains optional follow-up work, not a
  requirement to lock the shortlist ordering.
- The broader follow-up therefore does not reopen the top slot. It reinforces
  `legend-macd-above-zero-cross` as the quality-first leader, keeps
  `historical-s-bred940-trend-core` as the second adaptive contender, and leaves
  `legend-pullback-breakout` as the higher-turnover third line.

## Broker-Variance Probe

The remaining open question was whether the shortlist ranking survives a broker/source change. In
the current local environment, only one authenticated MT5 server is available: `MetaQuotes-Demo`.
That means there is no true second broker snapshot to replay directly.

Two proxy probes were attempted:

1. An alternate portable root injected `bases/Default/History/USDJPY` into
   `bases/MetaQuotes-Demo/history/USDJPY`.
2. Fixed-spread overrides were attempted through Strategy Tester INI files.

Observed behavior:

- The alternate portable root still authenticated on `MetaQuotes-Demo` and immediately started
  preliminary `M1` history download, so it cannot be treated as a clean second-source replay.
- The fixed-spread probe did not move results, even with extreme values:

| Probe | Window | Net | PF | Trades | Baseline status |
| --- | --- | ---: | ---: | ---: | --- |
| `legend-macd-above-zero-cross`, spread `25` | `2025` | `10.97` | `1.26` | `41` | identical to baseline |
| `legend-pullback-breakout`, spread `300` | `2025` | `80.97` | `1.22` | `425` | identical to baseline |

Broker-variance conclusion:

- Broker-by-broker variance is still unresolved in this environment.
- For freeze closeout, this is treated as a deferred residual risk rather than a blocker.
- The attempted proxies were useful as diagnostics, but they are not strong enough to close the
  risk:
  - history injection is contaminated by fresh `MetaQuotes-Demo` synchronization
  - fixed `Spread=` overrides do not affect the current `Model=4` tester pipeline in practice
- To close this properly, the next artifact must be either:
  - a second broker/account snapshot with independent history
  - a custom offline bar/tick export path that preserves source labels outside MT5's live sync

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

1. Keep `legend-macd-above-zero-cross` as the primary stability-first survivor. The fixed-window,
   rolling-window, narrow walk-forward, and broader-retune evidence all keep it alive, and the
   broader follow-up strengthened both its forward profit and quality metrics.
2. Keep `historical-s-bred940-trend-core` as the second adaptive contender. Its fixed-parameter
   `2025` decay is still real, but the `3/3` walk-forward pass survives both the narrow and medium
   retune profiles, and the broader follow-up did not show that the narrow profile was materially
   understating its edge.
3. Keep `legend-pullback-breakout` on the shortlist, but behind the top two. The wider search
   improved its forward profit materially, yet its turnover stayed the highest and its mean PF
   still trailed both `legend-macd-above-zero-cross` and `historical-s-bred940-trend-core`.
4. Drop `legend-macd-signal-cross` from any stability-first shortlist.

If the objective changes from fixed-parameter stability to retuned walk-forward adaptation, the
shortlist is still genuinely three-deep, but the broader follow-up now supports a clearer ordering:
`legend-macd-above-zero-cross` -> `historical-s-bred940-trend-core` -> `legend-pullback-breakout`.

## Operational Objective After Freeze

As of `2026-03-07 JST`, the practical objective is no longer "port more MQ5 strategies".
That phase is already complete for the current named Legend shortlist and historical `:S` batch.

The live question is now narrower:

1. lock the stability-first primary line for offline / freeze-era comparison
2. preserve the evidence chain that justifies that ranking
3. keep only the smallest unresolved robustness follow-ups alive

Under that objective, the operational stance is:

- fix `legend-macd-above-zero-cross` as the primary stability-first line
- keep `historical-s-bred940-trend-core` as the second adaptive line, not as an unresolved top-slot challenger
- keep `legend-pullback-breakout` as a third higher-turnover adaptive line
- keep broker-variance replay as deferred residual risk rather than as a closeout requirement

## Freeze Closeout Status

The MT5 freeze follow-up is complete for the current local environment.

The medium fallback for `historical-s-bred940-trend-core` is treated as canonical closeout evidence
instead of as an unresolved issue. The full-wide rerun remains optional future research, not a
requirement for closing this batch.

The repo cleanup boundary has already been documented in
`doc/knowledge/legend_mt5_cleanup_boundary_20260307.md` and is therefore not kept on the
validation-risk list.

## Residual Risks

- Baseline isolated tester execution and the `timeframe=3600 -> H1` reproduction contract are
  already covered. The open risk is robustness, not first-pass portability.
- Full walk-forward is now covered with narrow bounded ranges plus a broader follow-up (`wide` for
  `legend-macd-above-zero-cross` / `legend-pullback-breakout`, `medium` fallback for
  `historical-s-bred940-trend-core`). The medium fallback is accepted as the freeze-era closeout
  artifact, so the aborted true full-wide rerun is no longer treated as an unresolved blocker.
- Broker variance remains unresolved. The 2026-03-07 proxy probe showed that the current
  environment cannot produce a clean second-broker replay, and fixed-spread overrides do not
  materially alter the present `Model=4` tester results. Freeze closeout accepts this as deferred
  residual risk rather than keeping the batch open.
- The fresh fixed-parameter `2026.01.01-2026.03.07` window is especially thin for
  `legend-macd-above-zero-cross` (`4` trades), so static-set deployment decisions should not rely
  on that rerun alone even though the walk-forward fold stayed positive.
- Repo cleanup is no longer a validation blocker. The current split decision is already recorded in
  `doc/knowledge/legend_mt5_cleanup_boundary_20260307.md`.
- The conclusions are based on MT5 tester results for the current portable environment and symbol
  setup, not broker-by-broker deployment variance.
