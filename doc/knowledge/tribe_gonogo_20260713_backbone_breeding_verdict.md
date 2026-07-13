# tribe 本丸 go/no-go — 背骨修正後の育種 honest OOS/CPCV 判定（2026-07-13）

**日付**: 2026-07-13 JST ／ **モード**: オフライン・paper のみ（ライブ発注なし・ZMQ/Guardian 不使用）
**関連**: [`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md) §0.5,
[`KILL_CRITERIA_20260703.md`](KILL_CRITERIA_20260703.md) §4,
[`thread_a_p3_oos_cpcv_backfill_20260711.md`](thread_a_p3_oos_cpcv_backfill_20260711.md)（2026-07-08 §4 実験）
**証跡（ローカル・gitignore 下）**: `logs/tribe_gonogo_20260713/`（scripts はリポジトリに追跡・raw JSON は再現可能）

---

## TL;DR

- **honest PASS 件数 = 60**（`honest_gate` 無改造・実コスト 2pip・López de Prado purge/embargo CPCV 込み）。
  実パイプライン `python -m tools.tribe.export`（SQL 正本）で二重確認（PASS=60 / PROVISIONAL=1 / REJECT=428）。
- **事前登録ルール（0→畳む / ≥1→継続）→ 継続**。「PASS=0 なら畳む」の決定的シグナルは**発火せず**。
  背骨修正は実証済み（pre-fix の feed PASS は配線断絶で構造的 0 → post-fix で 60）。
- **但し KILL_CRITERIA §4 の決定的テスト（多様性＋頑健性）は依然 FAIL**（diverse=0・CPCV median 負・robust 実質 1 件）。
  2026-07-08 の §4 と**同一結論**。個体群は **USDJPY・TREND・SMA クロスの単一栽培**のまま。
- したがって **「継続」は多様性獲得を条件**とする。同一単一栽培の増産では §4 は永久に FAIL。実弾昇格は不可。

## 母集団（現行の育種出力）

`data/memory/swimmy.db` = 18,506 行。うち 18,006 が `:SCOUT`（検証前プール・feed 既定除外）。
**feed 対象（rank ∉ {SCOUT,GRAVEYARD}）= 489 件**（A=44 / B=380 / LEGEND-ARCHIVE=64 / RETIRED=1）。
- symbol: **USDJPY 487** / EURUSD 1 / GBPUSD 1（非 USDJPY 2 件は価格データ不在で honest 評価不能）
- category: **TREND 484** / SCALP 2 / BREAKOUT 2 / REVERSION 1
- §4 autopsy 時（168 件）から 3 倍に増えたが**単一栽培は不変**。

## 方法（2026-07-08 §4 実験の忠実な拡張）

- エンジン: `guardian/src/bin/kill_oos_cpcv.rs`（Guardian 実 BACKTEST 意味論 = entry/exit AST 非送信 →
  SMA(short/long) ゴールデン/デッドクロス＋SL/TP を再現）。
- manifest: `build_manifest.py`（`school-backtest.lisp:extract-sma-params` を忠実移植。§4 の 61 件と 61/61 一致で検証）。
- コスト: 往復 2pip 保守（slippage=0.01 片側 × entry/exit）。penalized(Taleb) sharpe。
- OOS=2021–2024 ／ CPCV=2015–2024 を 10 purged block（purge/embargo は TF スケール warmup で付与・fold 間漏洩なし）。
- **honest_gate 無改造**（min_trades=200 / min_pf=1.10 / min_sharpe=0.10）。floor は一切緩めていない。

### 再現コマンド
```
python logs/tribe_gonogo_20260713/build_manifest.py --out logs/tribe_gonogo_20260713/manifest_full.json
target/release/kill_oos_cpcv.exe --data data/historical/USDJPY_M1.csv \
    --manifest logs/tribe_gonogo_20260713/manifest_full.json \
    --out logs/tribe_gonogo_20260713/results_full.json
python logs/tribe_gonogo_20260713/score_gate.py --results .../results_full.json --out .../gate_scored.json
python logs/tribe_gonogo_20260713/backfill_and_export.py   # export.py on a COPY of swimmy.db (live DB untouched)
```

## 結果（実コスト 2pip・purge/embargo、n=489）

判定ラダー（**どの定義でも 0 にはならない** = fold トリガー不発）:

| 指標 | 件数 |
|---|---:|
| **PROD honest_gate PASS**（IS floor＋実 OOS sharpe>0。feed が publish）| **60** |
| ALL-REAL forward PASS（実 OOS 窓が全 floor 通過）| 28 |
| real-edge PASS（実 OOS PF≥1.10 かつ CPCV median>0）| 12 |
| robust PASS（実 OOS PF≥1.10 かつ CPCV pass_rate≥0.6）| 2 |
| 完全 §4 バー（OOS-qualified＋CPCV-ok）| 1（`Bred-Bred--293-Gen39`）|
| **diverse（非USDJPY or 非TREND）** | **0** |

母集団分布: **OOS PF 中央値=1.035**（コスト後ほぼ損益分岐、PF≥1.10 は 198/489）、
**CPCV median sharpe 中央値=−0.085**（中央の戦略は多期間 risk-adjusted で負、>0 は 194/489）。
→ §4（61 件で −0.13）と同じ薄さ。2021–24 は USDJPY +52% 一方向で TREND clone に**有利な窓**、それでもこの結果。

最強の生存候補（robust 2 件・いずれも USDJPY/TREND）:
- `Bred-Bred--293-Gen39` tf=180m sma=79/84: OOS t=200 PF=1.348 sh=0.68 / CPCV pr=0.60 med=0.522（§4 の唯一生存と同一）
- `Bred-Bred--689-Gen61` tf=360m sma=87/89: OOS t=**106**(<200) PF=1.534 sh=0.696 / CPCV pr=0.60 med=0.441

## 副産物: §4 bin の多様性判定バグを修正

`kill_oos_cpcv.rs` の `diversity_ok` は `m.category != ":TREND"` で比較していたが、SQL 由来 manifest の
`category` は `"TREND"`（コロン無し）。→ 全 USDJPY/TREND clone を誤って "diverse" 判定し、
**偽の §4 SURVIVE** を出していた。`trim_start_matches(':')` で正規化して修正。修正後の bin は
**diverse=false / §4 PASS=false → KILL per §5** を正しく出力（=2026-07-08 と一致）。
（2026-07-08 の 61 件走は ":TREND" manifest だったため当時は露見せず。）

## go/no-go（結論）

- **事前登録ルール適用**: honest PASS=60（最厳定義でも 1）→ **fold しない・継続**。背骨修正の価値は実証。
- **条件**: 継続は **多様性の獲得**（非 USDJPY / 非トレンドの独立エッジ＋その symbol の価格データ）を必須とする。
  category ラベルは Guardian 実経路が全個体を同一 SMA クロスで実行するため**コスメティック**で、多様性にならない。
- **実弾昇格は不可**（robust 実質 1 件・境界・単一相関）。deployment_gate 前提を維持。
