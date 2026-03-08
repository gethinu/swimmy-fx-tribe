# MT5 Freeze Cleanup Boundary 2026-03-07

日付: 2026-03-07 JST

## 目的

freeze 後の MT5 / Legend 作業を commit 単位で切り分けるために、
現在の dirty worktree を `keep / separate / drop` で分類する。

ここでの `drop` は「この MT5 cleanup commit には含めない」という意味であり、
勝手に削除・revert する判断ではない。

## KEEP

MT5 freeze / validation の canonical 成果物として同じ commit に残す。

- knowledge docs
  - `doc/knowledge/legend_s_rank_inventory_20260307.md`
  - `doc/knowledge/legend_mt5_tester_automation_20260307.md`
  - `doc/knowledge/legend_mt5_followup_validation_20260307.md`
- MT5 tester artifacts
  - `data/reports/mt5/inventory_tester/run_20260307_064816/`
  - `data/reports/mt5/inventory_tester/run_20260307_070330/`
  - `data/reports/mt5/inventory_tester/run_20260307_071613/`
  - `data/reports/mt5/inventory_tester/run_20260307_071724/`
  - `data/reports/mt5/inventory_tester/run_20260307_071842/`
  - `data/reports/mt5/inventory_tester/run_20260307_071952/`
  - `data/reports/mt5/inventory_tester/run_20260307_072101/`
  - `data/reports/mt5/inventory_tester/run_20260307_072157/`
  - `data/reports/mt5/inventory_tester/run_20260307_101456/`
  - `data/reports/mt5/inventory_tester_spreadprobe/run_20260307_110931/`
  - `data/reports/mt5/inventory_tester_spreadprobe/run_20260307_110958/`
  - `data/reports/mt5/legend_walkforward/run_20260307_113707/`
  - `data/reports/mt5/legend_walkforward/run_20260307_123443_wide/`
  - `data/reports/mt5/legend_walkforward/run_20260307_132228_b940_medium/`

理由:

- この範囲だけで「何を port し、何を compile し、どう validation し、なぜ
  `legend-macd-above-zero-cross` を top slot に固定したか」が再現できる。
- 2026-03-07 時点の freeze-aware な判断材料として十分。

## SEPARATE

同じ repo には残り得るが、MT5 freeze cleanup と同じ commit に混ぜない。

- runtime / SLTP override 実装
  - `src/lisp/core/message-dispatcher.lisp`
  - `src/lisp/school/school-execution.lisp`
  - `src/lisp/tests.lisp`
- library churn
  - `data/library/B/*.lisp` の削除群
  - `data/library/LEGEND/*.lisp` の更新群
- unrelated config / planning
  - `tools/configs/xau_autobot.tuned_auto_gc_m5_60d.json`
  - `docs/plans/2026-03-07-*.md`

理由:

- `src/lisp/*` 差分は symbol SL/TP override と shadow/live 実行ロジックの変更であり、
  MT5 offline validation とは別テーマ。
- `data/library/LEGEND/*.lisp` の差分は実質 `:AGE` の経時更新で、validation 結論の根拠ではない。
- `data/library/B/*.lisp` の bulk deletion は影響範囲が大きく、MT5 doc sync と一緒に入れるべきではない。
- plan docs は監査用には有用だが、canonical knowledge docs の必須要素ではない。

## DROP

この MT5 cleanup commit からは外す。必要なら別途 keep/separate で扱う。

- generated trend / model artifacts
  - `data/trend_arbitrage/latest_run.json`
  - `data/trend_arbitrage/runs/*.json`
  - `data/patterns/models/ensemble_weight.json`

理由:

- MT5 / Legend validation の入力でも出力でもない。
- timestamp や ranking の自然更新が中心で、今回の作業目的に対してノイズが大きい。

## 推奨コミット境界

1. `docs: sync MT5 freeze validation state`
2. `data: preserve MT5 validation artifacts` が必要なら別 commit
3. `fix: symbol SLTP override runtime flow` は別トピックとして独立
4. `chore: library cleanup` は削除意図を確認してから別で扱う

## 現時点の運用決定

- `docs/plans/2026-03-07-*.md` は audit trail として repo に残してよいが、MT5 freeze cleanup commit には混ぜない。
- `data/library/B/*.lisp` の削除群は、この cleanup 範囲には含めず、意図確認が取れるまで separate 扱いを維持する。
- `data/library/LEGEND/*.lisp` の `:AGE` 更新も、この cleanup 範囲には含めず separate 扱いを維持する。
