# 🧬 tribe 2d — B-4/R4 行動距離の実装（距離関数が銘柄/TF/行動を見るようになった）

**日付**: 2026-07-18 JST ／ **ブランチ**: `claude/2d-behavioral-distance`
**モード**: オフライン・**ライブ発注なし・live `swimmy.db` は無改変（read-only COPY のみ使用）・フラグ本番 ON はしていない**。
**設計正本**: [`regen_engine_redesign_20260703.md`](regen_engine_redesign_20260703.md) §B-4（行動距離）・§A-1(3)、
[`tribe_diversity_engine_rebuild_20260713.md`](tribe_diversity_engine_rebuild_20260713.md) §R4。
**前段**: [`tribe_2c_wsl_verification_20260715.md`](tribe_2c_wsl_verification_20260715.md)（2c 本体・fitness-sharing/selection-fitness は master 済）。

---

## 0. TL;DR

- **根本ギャップ**: `calculate-genetic-distance` が **銘柄・timeframe・行動を一切見ず** indicators Jaccard＋category＋SL＋TP だけで測っていた（regen §A-1(3), §R4）。このため周期違いの USDJPY-TREND SMA クローン2体が距離 **0.408**（0.02 clone-floor 超・0.2–0.8 sweet-spot 内）に散らばり、clone 検知も fitness-sharing も**原理的に**単一栽培を見られなかった。
- **実装（純Lisp・flag-gated）**: `*enable-primitive-diversity*` ON で距離を **B-4 合成 niche/行動距離**に作り直した。behaviour(1−|corr|, データ有時 PRIMARY) ＋ symbol ＋ category/regime ＋ timeframe ＋ indicator-family ＋ entry-tree ＋ sl/tp。**flag OFF は旧式（legacy）と byte 一致**（`legacy-genetic-distance` に旧本体を逐語保存し dispatch）。
- **実証（実 monoculture・read-only COPY・18,507 個体）**: 同一 (symbol,regime,tf) の真クローンは B-4 で **中央値 0.0010／96.4% が 0.02 clone-floor 未満／100% が sweet-spot 未満**に収束。非USDJPY(EURUSD/GBPUSD)は **100% が 0.20 超（中央値 0.56）**、cross-regime も **100% が 0.20 超**。→ **クローンは距離ゼロ近傍、diverse は遠距離**（要件どおり）。
- **ガードレール順守**: honest_gate の検証 floor（min_trades/min_pf/min_sharpe）は**一切未改変**。live daemon 未起動・live DB 無改変・フラグ本番 ON なし。多様性だけを増やし検証は捏造していない。

---

## 1. 変更ファイル

| ファイル | 変更 |
|---|---|
| `src/lisp/school/school-genome.lisp` | `calculate-genetic-distance` を flag dispatch 化。新規: `b4-genetic-distance`, `legacy-genetic-distance`（旧本体逐語）, ヘルパ `genome-indicator-family` / `genome-timeframe-distance` / `genome-tree-distance` / `%sexp-node-count` / `genome-behavior-distance` / `%b4-symbol-key`, 重み `defparameter *b4-w-*` 群。`extract-genome` に `:symbol`・`:behavior`(pnl-history) を第一級搬送。`crossover-genes`/`implant-genome` に **flag-gated** symbol 伝播（OFF は byte 一致）。 |
| `src/lisp/tests/evolution-tests.lisp` | `deftest test-b4-*` ×5（genome 搬送・flag OFF legacy 一致・clone 近ゼロ・diverse 遠・behaviour hook）。 |
| `src/lisp/tests.lisp` | 上記5 test を `run-all-tests` の dolist に登録。 |
| `tests/genome_b4_distance_offline_test.lisp` | **FFI 不要のオフライン回帰**（native Windows は sqlite/pzmq DLL 不在で `asdf:load-system` 不可のため、pure な school-genome.lisp だけを最小 struct にロードして検証）。`sbcl --script tests/genome_b4_distance_offline_test.lisp`。 |

## 2. B-4 距離の設計（flag ON）

合成距離 ∈ [0,1]。**データのある項だけで重みを正規化**（legacy の conditional SL/TP と同じ発想）:

```
behaviour 1-|corr|  w=0.48  ← PRIMARY。両者に return 系列がある時のみ。無ければ重みは構造項へ再配分
symbol mismatch      w=0.20  ← 単一栽培が崩れる軸
category/regime      w=0.12
timeframe bucket      w=0.08  ← log2 距離 / 11.0
indicator family      w=0.06  ← sma/rsi/bb… 先頭 family（周期は無視＝R2「behaviour は family で決まる」）
entry tree edit       w=0.03  ← node 数差＋root-op 不一致（本格 tree-edit は後日）
sl/tp param           w=0.03  ← 行動的に軽微
```

- **クローン（同一 niche・周期/sl/tp のみ差）** → 構造項ほぼ 0 → 距離 ~0.001。**0.02 clone-floor 未満**で `strategies-correlation-ok-p`／`genetic-compatibility-p` が clone を弾ける。
- **behaviour 項は本物のフック**: `strategy_daily_pnl` は現状 **空（0 行）**＝ live DB に return 系列が無い。だから corr は**休眠**し構造項が clone 検知を担う。データが入れば `extract-genome` の `:behavior` gene 経由で自動的に PRIMARY 化。**データが無いのに corr を捏造していない**（regen §C の正直さ規律）。

## 3. 実証コマンド（再現）

```bash
# オフライン回帰（FFI 不要・native Windows で可）
sbcl --script tests/genome_b4_distance_offline_test.lisp          # 13/13 PASS

# 実 monoculture 実証: swimmy.db を COPY して read-only 抽出→距離統計（scripts は logs/ 追跡）
#   [1b] 同一(sym,regime,tf)クローン B-4: median=0.0010, 96.4%<0.02, 100%<0.20
#   [2]  USDJPY-TREND vs 非USDJPY(VWAPVR): 100% >0.20 (median 0.56)
#   [3]  vs REVERSION/BREAKOUT: 100% >0.20
```

（full FFI suite の `run-all-tests`（test-b4-* 含む）は native Windows の sqlite/pzmq DLL 不在で未実走＝CI/WSL 側で。deftest 5件は本物の deftest/assert マクロを通して native 検証済。）

## 4. 残课題（引き継ぎ）

1. **behaviour corr の実データ配線**: `strategy_daily_pnl` を埋める or backtest の equity/trade 系列を `strategy-pnl-history`（=`:behavior` gene）に流す。入れば corr が PRIMARY 化し「遺伝子違い・収益同一」の真の behavioural clone を検知（regen B-4 の本来の主指標）。
2. **B-3 生成ループ**: 銘柄/regime **変異オペレータ**（吸収状態を破る脱出路）と **niche クォータ**は未実装。本 2d は「距離が niche を見る」ところまで。fitness-sharing(2c 済) × B-4 距離(本 2d) が揃ったので、次は変異オペレータで実際に非USDJPY を注入。
3. **A: daemon 隔離検証**: flag-ON で B-4 距離が live 採点経路（add-to-kb→phase1→CPCV）を通って多世代で単一栽培を薄めるかは未実測（[[tribe-2c-wsl-verification]] の (A) と同じ、Guardian backtest-only 隔離が必要）。
4. **重みチューニング**: `*b4-w-*` は妥当な初期値。behaviour データ投入後に再校正推奨。

**本番投入はオーナー判断**（flag は既定 OFF＝完全可逆）。
