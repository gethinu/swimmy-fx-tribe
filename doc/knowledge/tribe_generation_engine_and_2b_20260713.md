> ⚠️ **訂正 (2026-07-14)**: 本doc §2 の 2b 決定実験は**採点ハーネスのバグで汚染**されていた。
> `kill_oos_cpcv.rs:build_strategy` が `indicator_type` を `"ema"` 以外すべて `Sma` に潰しており、
> `bb`/`rsi`/`stoch`/`macd` の全 config が**実際には SMA として走った**。よって §2 の中核主張
> **「BB 平均回帰は USDJPY H1 で CPCV 頑健エッジを生む」は誤り（実は SMA(30/60)）→ 撤回**。
> honest 再測定では頑健&diverse は **EURUSD の BB H4（period 40–50）に 2 件（境界）**。
> 正しい結論・数値は [`tribe_2b_correction_honest_primitives_20260714.md`](tribe_2b_correction_honest_primitives_20260714.md) を正とする。
> （§1 の生成/育種エンジン機構 file:line マップは無改変・有効。）

# 🏭 tribe 生成/育種エンジンの実機構 ＋ 2b 決定実験（非トレンド・プリミティブ）

**日付**: 2026-07-13 JST ／ **モード**: オフライン・read-only 診断 ＋ ハーネス実験（ライブ発注なし・live `swimmy.db` 無改変・honest_gate/§4 floor 不変）
**位置づけ**: [`tribe_design_grounding_20260713.md`](tribe_design_grounding_20260713.md) の Phase 0 を受けた「生成の実機構の具体化（file:line）」＋ロードマップ 2b の実行結果。
**姉妹**: [`tribe_diversity_engine_rebuild_20260713.md`](tribe_diversity_engine_rebuild_20260713.md)（2a）。

---

## 0. TL;DR

- **生成の実機構（コード実物で確定）**: ライブの genesis は **`run-breeding-cycle`（同一 category・同一 symbol 内の交叉/変異）＋`run-legend-breeding` のみ**。多様性を注入し得る経路は**全て dead/未配線**（phase-5-recruit 削除、`recruit-special-forces` 多シンボル創業株=未呼出、`school-evolution-orchestration` の AST 交叉/構造変異=未呼出、`select-category-pair` cross-regime=デッド）。
- **変異の実体**: 子は **symbol/regime/entry-exit AST を親から丸ごと継承**。実際に変異するのは **TF・indicator（同一 regime ライブラリ内で swap）・SL/TP のみ**。→ 「TREND-USDJPY プールの近親交配」に構造的に閉じている。
- **2b 決定実験（実データ・断定）**: 非トレンド・プリミティブ（RSI/BB/Stoch 平均回帰）を実コスト＋purge/embargo CPCV で評価。
  **結論＝条件付き YES**: **BB 平均回帰は USDJPY H1 で CPCV 頑健なエッジ候補を安定的に生む**（§4 decisive PASS on USDJPY 2b、robust×2、近傍も同傾向）。**SMA は全銘柄で robust 0** だったので**プリミティブが効くことを実証**。ただし**独立ペア（EURUSD/GBPUSD）は SMA も MR も robust 0**。
- **順序律への回答**: 2b は「（非TREND 頑健エッジが）在る」→ **2c 続行が確定**。方向＝**プリミティブ多様性（2c: エンジンに真のプリミティブ/AST を実行させる）を銘柄多様性（2e）より先に**。

---

## 1. 生成/育種エンジンの実機構（file:line・推測なし）

### 1-A. メインループ（`school-connector.lisp:start-evolution-service`）
実行順（`:293-320`）: `phase-1-validation`（IS backtest 選別）→ `phase-3-qualify`（WFV/OOS）→ `phase-3-5-cpcv-validate`（A→S 昇格のみ, `:99-105`）→ `phase-4-purge`（純 IS 淘汰）→ **`phase-5-recruit` はコメントアウト（`:311`）／「P8: DELETED」（`:86`）** → `phase-6-breeding`（`:88-97`）→ phase-7。
- **phase-6-breeding** = `run-breeding-cycle`（`:94`）＋ `run-legend-breeding`（`:96-97`, fboundp 時）。
- → **ライブの新個体 genesis はこの 2 つだけ。** 種の外部注入・テンプレ生成・ランダム創出は**走っていない**。

### 1-B. 育種サイクル（`school-breeder.lisp:run-breeding-cycle:1280-1358`）
- `(let ((categories '(:trend :reversion :breakout :scalp)) (max-pairs-per-category 20))`（`:1288-1289`）。
- 各 category につき、その **regime-class の warrior だけ**を抽出（`:1293-1300`）→ `strategy-breeding-priority-score` で降順ソート（`:1302-1303`）→ `cull-pool-overflow`（`:1306`）。
- p1 に対し `find-diverse-breeding-partner`（0.02 距離ゲート）で p2 を選び（`:1316-1319`）、`breed-strategies p1 p2`（`:1328`）→ `add-to-kb child :breeder :require-bt t`（`:1336`, Phase1 backtest 必須→`:queued-phase1`）→ `save-recruit-to-lisp`（`:1351`, ライブラリ保存＝RECRUIT-RND-*.lisp の出所）。
- **含意**: 育種は **category 内で閉じる**。個体群は TREND 484 / SCALP 2 / BREAKOUT 2 / REVERSION 1（gonogo 実測）＝**非 TREND category は親が枯渇し繁殖不能**。実質 **TREND だけが増殖**する。

### 1-C. 交配＝子の生成（`school-breeder.lisp:breed-strategies:1106-1218`）
子が親から受け取るもの:
| 遺伝子 | 由来 | 変異? |
|---|---|---|
| **symbol** | `(or (strategy-symbol parent1) "USDJPY")`（`:1119`）| **なし**（親1固定）|
| **category/regime** | `parent-regime-class`＝親1（`:1204,1134-1136`）| **なし** |
| **entry AST** | `higher-wr-parent` 由来を丸ごと（`:1121,1123-1126,:1214`）| **構造変異なし** |
| **exit AST** | `higher-pf-parent` 由来を丸ごと（`:1122,1127-1130,:1215`）| **構造変異なし** |
| **indicators** | `mutate-indicators-with-library(mutate-indicator-params(crossover-indicators p1 p2)) regime`（`:1138-1140`）| **あり**（同一 regime ライブラリ内 swap ＋数値変異）|
| **timeframe** | `select-breeder-child-timeframe`（`:1148-1150`）| **あり**（crossover/mutation モード）|
| **SL/TP** | 親平均＋`mutate-value 0.3`＋PF/WR bias＋Q-guided＋clamp（`:1132-1200`）| **あり** |

### 1-D. 指標変異が regime を跨がない（単一栽培の鍵）
`mutate-indicators-with-library`（`:31-50`）: 30% で 1 指標を `get-random-indicator-for-regime regime` に swap。regime は `case category`（`:39-44`）で **`:trend→:trend` / `:reversion→:reversion` / `:breakout→:breakout` / `:scalp→:reversion` / `(t :trend)`**。→ **TREND 系統は TREND ライブラリの指標しか得られない。** regime も継承（1-C）ゆえ、**TREND→REVERSION への系統転換オペレータは存在しない**。非 TREND 挙動は**創業株 seed でしか入らず**、生 sharpe 選択で淘汰された。

### 1-E. 選択＝生 sharpe（regen §A-2 を現行コードで再確認）
親トーナメント（`school-genome.lisp:210`）、親優先度（`school-breeder.lisp:696,745`）、溢れ淘汰（`:1384,1402`）、デスマッチ（`:1577`）、Stagnant/Weak（`:1651,1666`）、Wisdom（`:1524,1551`）が**生 `strategy-sharpe`**。penalized/CI/evidence は計算後に**未参照**。novelty/fitness-sharing **なし**。

### 1-F. 評価＝AST を捨てた汎用シグナル（実は分岐は在る）
- Lisp 送出 `strategy-to-alist`（`school-backtest.lisp:90-107`）は **entry/exit AST を送らない**（sma_short/long＝先頭2整数、indicator_type＝先頭指標、sl/tp/vol/tf のみ）。
- Rust は indicator_type で**実際に分岐**（`backtester.rs:818-843`）: `sma/ema→トレンド`, `rsi→平均回帰(30/70)`, `bb→平均回帰(下バンド反発/中央exit)`, `macd→モメンタム`, `stoch→平均回帰(20/80)`, `vwap→トレンド`。AST があれば eval も可（`:767,811`）だが**送られない**。
- → 挙動は「先頭指標 type ＋先頭2整数」に縮約。個体群が TREND(先頭 SMA/EMA) ゆえ実挙動は SMA クロスに均質化。**category ラベルはコスメティック**（＝生成側の帰結であってエンジン制約ではない）。

### 1-G. regen §B 機構の実装状況（対応表）
| regen §B | 実機構での状態 | file:line |
|---|---|---|
| B-1 symbol/regime = 変異遺伝子 | **欠落**（親1固定・変異オペレータ無）| `breeder:1119,1204` |
| B-2 単一 selection-fitness＋sharing | **欠落**（生 sharpe 比較子 8 箇所）| §1-E |
| B-3 niche クォータ＋変異＋cross-niche | **欠落/デッド**（category 内で閉じる、`select-category-pair` 未呼出）| `breeder:1288-1300`, `genome:212` |
| B-4 行動距離 | **欠落**（距離は indicators/cat/SL/TP、0.02 フロア）| `genome:18-51`, `breeder:213` |
| B-6 CPCV 前段ゲート | **欠落**（純 IS 淘汰でプール形成）| `connector:293-314` |
| （AST 実行）| **劣化**（送出せず縮約）；ただし Rust 分岐は健在 | `backtest:90-107`, `backtester.rs:818-843` |
| （多シンボル genesis）| **未配線**（`recruit-special-forces` 存在するが未呼出）| `execution:1981-2038` |
| （AST 交叉/構造変異）| **未配線**（`school-evolution-orchestration` 未呼出）| `genome:152-196`, orchestration |

---

## 2. 2b 決定実験 — 非トレンド・プリミティブに CPCV 頑健エッジは在るか

### 2-A. 方法
- ハーネス `kill_oos_cpcv.rs`（indicator_type を実ジェネレータに dispatch）を使い、**RSI/BB/Stoch 平均回帰＋MACD モメンタム**の grid を、各銘柄の実データで実コスト 2pip（pip 補正 slippage）＋ purge/embargo CPCV（10 fold, 2015-2024）評価。
- grid: 各 primitive × period × tf{15,60,240} × pip 補正 SL/TP、計 108 config/銘柄。honest_gate/§4 floor **不変**。
- score_gate の `diverse` は symbol≠USDJPY **or** category≠TREND。**MR は engine が実際に平均回帰を実行**するので、非 TREND ラベルは**挙動として正当**（gonogo で問題化したコスメティック非 TREND とは異なる）。

### 2-B. 結果（§4 バー: OOS trades≥200 & PF≥1.10 & pen≥0.3、CPCV pr≥0.6 & med<2.0）

| 銘柄(slippage) | OOS 適格 | **CPCV 頑健** | §4 decisive | 備考 |
|---|---:|---:|:--:|---|
| **USDJPY(0.01)** | 8 | **2** | **PASS** | robust は **BB 平均回帰・H1・period30**（下記）|
| EURJPY(0.01) | 2 | 0 | FAIL | RSI/Stoch H4、CPCV pr=0.3 |
| EURUSD(0.0001) | 0 | 0 | FAIL | 全 primitive で頑健エッジ皆無 |
| GBPUSD(0.0001) | 0 | 0 | FAIL | 同上 |

**BB 確認 grid（period×tf×barrier を細分）**: USDJPY は再び OOS 適格 4 / **CPCV 頑健 2**、安定領域を確認:
```
BBc-USDJPY-p30-tf60-sl50tp50  OOS(t=423,pf=1.165,pen=0.581) CPCV(pr=0.7,med=+0.443,pf=1.124)  ROBUST
BBc-USDJPY-p30-tf60-sl30tp60  OOS(t=423,pf=1.115,pen=0.397) CPCV(pr=0.6,med=+0.398,pf=1.112)  ROBUST
BBc-USDJPY-p25-tf60-sl30tp60  CPCV pr=0.4 (fail, 近傍)
BBc-USDJPY-p30-tf60-sl80tp80  CPCV pr=0.4 (fail, 近傍)
```
EURUSD/GBPUSD の BB 細分 grid = **OOS 適格 0 / robust 0**（独立ペアは MR でも不発）。

### 2-C. 断定（正直な限界つき）
1. **プリミティブが効く**: SMA は全銘柄 robust 0。**BB 平均回帰は USDJPY H1 で CPCV 頑健エッジを安定的に生む**（近傍込みで領域を成す＝単発 fluke でない）。→ 「エンジンに真のプリミティブを実行させれば §4-③（頑健性）に手が届く」ことの**実データ実証**。
2. **§4-② の達成ルート**: **非 TREND（挙動）ルートは達成**（USDJPY-BB-MR、engine が実平均回帰）。**非 USDJPY（銘柄）ルートは未達**（EURUSD/GBPUSD は SMA も MR も robust 0）。
3. **限界（過大主張しない）**: エッジは**控えめ**（OOS PF≈1.12–1.17、CPCV median sharpe≈0.4、median PF≈1.12）で**バー際どい**。**多重検定**（108+48 config）下の発見であり、**forward/holdout での確認前は実弾不可**。独立性（BB-MR-USDJPY と SMA-TREND 個体群の収益相関）は未計測（挙動上は別物・要定量）。
4. **honest_gate/§4 floor は全工程で不変。**

---

## 3. ロードマップ更新（順序律への回答）

> **2b = 「在る」（非 TREND 頑健エッジが実在）→ 2c 続行が確定。** 方向はデータで精密化: **プリミティブ多様性（2c）を銘柄多様性（2e）より先に。**

- **2c（次・要オーナー承認＝破壊的）**: **エンジンに真のプリミティブ/entry-exit AST を実行させる。** 具体化:
  - (a) 育種が **indicator_type/regime を実際に探索・保持**できるようにする（現状 TREND 系統は MR 指標を得られない・§1-D）。まず **MR/breakout プリミティブを育種の探索空間に正式投入**。
  - (b) `strategy-to-alist` に **entry/exit AST を含め**、Rust の AST eval 経路（`backtester.rs:767,811`）を実採点で有効化 → 縮約解消。
- **2d**: 単一 selection-fitness ＋ fitness sharing（行動距離）— BB-MR のような非 TREND を生 sharpe 淘汰から守り、多様性を除算強制。
- **2e**: symbol/regime を遺伝子化＋niche クォータ＋変異＋**sl/tp を pip/ATR 正規化**。※ 2b が示すとおり銘柄多様性は「独立ペアに現行プリミティブの頑健エッジが無い」ため**単独では §4 を通さない**→ 2c/2d の後。
- **2f**: CPCV 前段ゲート（単一窓 fit を種にしない）。

**次アクション（オーナー判断待ち）**: 2c は破壊的（`school-backtest` 送出／Guardian 契約／`school-breeder` 探索空間）。着手アプローチ（AST 送出を先にするか、育種の regime/primitive 探索を先にするか、ブランチ戦略）をオーナー確認後に実装。

---

## 4. 再現コマンド
```bash
D=logs/tribe_diversity_20260713
python3 $D/generate_2b_grid.py --symbol USDJPY --pip 0.01   --out $D/grid2b_USDJPY.json
target/release/kill_oos_cpcv.exe --data data/historical/USDJPY_M1.csv \
    --manifest $D/grid2b_USDJPY.json --slippage 0.01 --out $D/grid2b_results_USDJPY.json
python3 logs/tribe_gonogo_20260713/score_gate.py --results $D/grid2b_results_USDJPY.json --out $D/grid2b_gate_USDJPY.json
# EURUSD/GBPUSD は --pip 0.0001 / --slippage 0.0001
```
