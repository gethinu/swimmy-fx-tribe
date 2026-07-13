# 🧬 tribe 多様性エンジン再建 — 設計 grounding・単一栽培ルート原因・多シンボル第一ピース

**日付**: 2026-07-13 JST ／ **モード**: オフライン・paper のみ（ライブ発注なし・ZMQ/Guardian 実発注経路 不使用）
**方針**: tribe は畳まない。設計ドキュメントの「最強の仕組み（多様性/生成エンジン）」を実現して作り変える。
**関連**:
[`regen_engine_redesign_20260703.md`](regen_engine_redesign_20260703.md)（多様性エンジンの設計正本）、
[`KILL_CRITERIA_20260703.md`](../KILL_CRITERIA_20260703.md) §4、
[`tribe_gonogo_20260713_backbone_breeding_verdict.md`](tribe_gonogo_20260713_backbone_breeding_verdict.md)（honest PASS=60 / §4 FAIL）、
[`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md)。
**証跡**: `logs/tribe_diversity_20260713/`（スクリプト追跡・大容量 CSV/JSON は gitignore）。

---

## 0. TL;DR

- **設計正本 = `regen_engine_redesign_20260703.md`。** 多様性は「構造で強制、過剰最適化はループ内で罰する、
  適応度アクセサを1本化」する設計。銘柄・regime を第一級遺伝子に昇格し、niche クォータ＋fitness sharing＋
  行動距離＋CPCV 前段ゲートで「61 clone を構造的に不能化」するのが本来の意図。
- **ルート原因（現行コードで再検証・断定）**: 単一栽培は 4 つの構造欠陥の合成による**吸収状態**。
  regen doc §A の主張は 2026-07-13 の現行コードでも**全て健在**（P1/P2/P3 は検証配線・ランク衛生のみで
  生成器は無改変）。加えて実データで 2 点を確定: (i) `data/historical/` は **USDJPY_M1.csv 単一**、
  (ii) 実採点経路は entry/exit AST を送らず **indicator_type + 先頭2整数**の汎用シグナルに縮約。
- **第一ピース（実装＋検証済）**: 多シンボル価格データ供給パイプラインを構築し、**§4 ハーネスを 4 銘柄で実走**。
  結論は決定的かつ正直: **現行プリミティブ（SMA クロス）はどの銘柄でも CPCV 頑健なエッジを 1 つも生まない。**
  非 JPY 独立ペア（EURUSD/GBPUSD）では OOS 適格すら 0。個体群の「エッジ」の正体は
  **2021–2024 の JPY 安トレンドという単一マクロ因子**であり、多様な独立エッジではない。
- **含意（ロードマップの向き）**: 多シンボルデータ＋銘柄遺伝子は**必要だが不十分**。真のレバレッジは
  **非トレンド・プリミティブ（平均回帰/ブレイク/キャリー/クロスペア相対値）＋銘柄別育種＋CPCV 前段ゲート**。

---

## 1. 設計が意図した多様性エンジン（regen doc §B の要点抽出）

| 機構 | 設計意図 | §4 のどの条項に効くか |
|---|---|---|
| **銘柄・regime を第一級遺伝子に** (B-1) | `genome := (:symbol :regime :timeframe :indicators :entry :exit :sl :tp :volume)`。現状 breed で親1固定・変異なし → 変異可能遺伝子へ昇格。 | ②非USDJPY/非TREND |
| **単一 selection-fitness アクセサ** (B-2,B-5) | 生 sharpe を読む 5+ 箇所を `selection-fitness` 1本に置換。`edge=min(haircut, CI下限)×evidence`、norm cap 2.0→1.5。 | ①過剰最適化を種にしない |
| **fitness sharing（除算で多様性強制）** (B-2) | `selection_fitness = base / (1+Σ kernel(distance))`。同一 niche が 61 個いれば 61 番目の分母 ~61 → 選ばれない。**重み調整で消せない**のが要点。 | ②61 clone を構造的に不能化 |
| **niche クォータ＋銘柄/regime 変異** (B-3) | `dolist (cat)` を `dolist (niche)` に。空 niche に探索ボーナス。銘柄/regime 変異オペレータ追加（**吸収状態を破る唯一の脱出路**）。 | ② |
| **行動距離を主指標に** (B-4) | `1 − |corr(behavior(a),behavior(b))|`。遺伝子が違っても収益が同一なら clone 判定。距離に symbol/timeframe/木編集距離を追加。 | ②相関 clone 検知 |
| **CPCV 前段ゲート** (B-6) | phase-1(IS) と phase-4(淘汰) の間に purged-CPCV ゲート。CPCV 通過を**育種プール入りの条件**に。 | ③頑健性・単一窓 fit 排除 |

**正直な限界（regen doc §C-2、本実験が定量裏付け）**: 発生器の改良は**データに無いエッジを生まない**。
多様性強制は「USDJPY-TREND の過剰最適化」を「6通貨×4regime の過剰最適化」に置換する危険を内包する。
防波堤は B-2 の CI 下限と B-6 の CPCV ゲート**のみ**。

---

## 2. 単一栽培のルート原因（現行コードで再検証・断定）

489 個体（feed 対象）が USDJPY 487 / TREND 484 に収束するのは、以下 4 欠陥の合成による**吸収状態**。

### R1. 銘柄が遺伝子でない・変異しない ＋ 非USDJPY 価格データが存在しない【確定】
- `extract-genome`（`school-genome.lisp:62-71`）に **`:symbol` なし**。遺伝子は indicators/entry/exit/sl/tp/volume/timeframe/category のみ。
- `breed-strategies`：`(sym (or (strategy-symbol parent1) "USDJPY"))`（`school-breeder.lisp:1119`）→ 子は親1銘柄固定（`:symbol sym`, `:1207`）。**銘柄変異オペレータ不在**＝数学的吸収状態。
- 実データ: `data/historical/` = **`USDJPY_M1.csv` 単一**（204MB）。非USDJPY 生存 2 件（EURUSD/GBPUSD）は価格データ不在で**honest 評価不能**。`strategy-to-alist` は symbol を emit すらしない。

### R2. 実採点経路が entry/exit AST を捨て、汎用シグナルに縮約【確定】
- `strategy-to-alist`（`school-backtest.lisp:90-107`）が emit するのは `sma_short/sma_long`（= `extract-sma-params`= 全 indicator 先頭2整数）＋ `indicator_type`（= `detect-indicator-type`= 先頭 indicator）＋ sl/tp/vol/tf のみ。**entry/exit の S 式（RSI/Stoch 等の実ロジック）は送られない。**
- Rust エンジンは indicator_type で分岐（`backtester.rs:818-843`: SMA/EMA=トレンド, RSI=平均回帰, BB, MACD, Stoch, VWAP…）し、AST があれば eval もする（`:767,811`）。→ **エンジンは「常に SMA」ではない**。だが Lisp 側が AST を送らないため、精緻な多条件ロジックは**先頭 indicator＋先頭2整数の汎用シグナルに縮約**される。個体群は 484/489 が TREND（先頭 SMA/EMA）→ 実挙動は SMA クロスに均質化。**category ラベルはコスメティック。**

### R3. 適応度が多様性を一切報酬しない・選択は生 sharpe【確定】
- 親優先度（`school-breeder.lisp:696,745`）と ~8 箇所の選択/淘汰比較子（`school-genome.lisp:210`; `school-breeder.lisp:1384,1402,1524,1551,1577,1651,1666`）が**生 `strategy-sharpe`** を読む。novelty / fitness-sharing / 銘柄・regime 分散項ゼロ。
- → 正のフィードバック: 創業 USDJPY-TREND クラスタが毎サイクル親を独占 → 子孫がまた最強 → 61→484 変奏。penalized/CI/evidence は計算後に**捨てられている**（表示専用）。

### R4. 遺伝的距離・相関ゲートが銘柄/TF/行動を見ない【確定】
- `calculate-genetic-distance`（`school-genome.lisp:18-51`）= indicators Jaccard(2)＋category(1)＋SL(1)＋TP(1) のみ。**銘柄・timeframe・entry/exit 木構造なし。**強制ゲートは 0.02 フロア。→ 484 USDJPY clone を**原理的に**検知不能。

> **断定**: 生成器は「seed の多様性を深掘りするだけの niche 崩壊アンプ」。seed＝USDJPY 優位＋単一 CSV＋生 sharpe 選択 → USDJPY×TREND への収束は不可避。多様性は「低報酬」ではなく**構造的にゼロ表現**（遺伝子でも距離でも適応度でもない）。

---

## 3. 第一ピース: 多シンボル価格データ供給＋銘柄別 honest 採点（実装＋検証）

### 3.1 実装
- **データ源（ローカル・課金なし）**: `mt5_Bundle-of-edges/.../data/ohlc_m1/`（10 銘柄 audjpy/audusd/eurjpy/eurusd/gbpjpy/gbpusd/nzdusd/usdcad/usdchf/usdjpy、各 ~19 年ファイル 2006–2024）。
- **変換器** `convert_bundle_m1.py`: `datetime,ohlc`（naive UTC）→ `timestamp(epoch),ohlc,volume=1.0`。
  **検証**: usdjpy 2015–2024 を変換 → 既存 `data/historical/USDJPY_M1.csv` と **3,721,229 行が byte 一致**（変換パイプライン忠実性を証明）。
- EURUSD/GBPUSD/EURJPY を同一変換で `data/historical/<SYM>_M1.csv` 生成（各 ~3.72M 行）。
- **ハーネス改修（再現性保存）**: `kill_oos_cpcv.rs` に**任意 `--slippage` 引数**を追加（既定 = `COST_2PIP=0.01` で **2026-07-08 §4 の 28 適格/1 CPCV-ok を byte 再現**）。sl/tp・slippage は**絶対価格距離**で、USDJPY pip=0.01 に固定校正されていた → 非 JPY majors（pip=0.0001）では 100 倍過大コスト・過大バリアで**採点が無効**だった（重要な副次バグ）。honest_gate floor は**一切未改変**。

### 3.2 実験と結果（実コスト 2pip・purge/embargo CPCV・n は下表）
**A. 転移テスト**（489 実 evolved config を各銘柄へ・非JPY は pip 補正 sl/tp×0.01・slippage 0.0001）:

| 銘柄 | honest PASS(prod) | ALL-REAL PASS | §4 OOS 適格 | CPCV 頑健 | diverse | 最良 OOS PF |
|---|---:|---:|---:|---:|---:|---:|
| EURUSD | 52 | 1 | 1 | **0** | ✔(ラベル) | 2.85 |
| GBPUSD | 54 | 1 | 1 | **0** | ✔ | 4.04 |
| EURJPY | 60 | 2 | 2 | **0** | ✔ | — |

- 最良候補はどの銘柄でも **同一 config `Bred-Bred--293-Gen39`（tf=180m sma=79/84）**＝ USDJPY §4 唯一生存と同一。全銘柄で CPCV pass_rate 0.3–0.6（<0.6）→ **単一窓 fit**。
- pip 未補正時は EURUSD 最良 PF=0.656（純損）→ コスト単位バグが「エッジ皆無」の**偽陰性**を生んでいた。補正後も CPCV 頑健は 0。

**B. 銘柄別 grid サーチ**（96 の新規 SMA config を各銘柄自身のデータで・pip 補正・USDJPY は陽性対照）:

| 銘柄 | §4 OOS 適格 | CPCV 頑健 | 解釈 |
|---|---:|---:|---|
| USDJPY（対照） | 6 | **0** | JPY 安トレンドは OOS に出るが CPCV 非頑健（home でも）|
| EURJPY | 5 | **0** | 同上（JPY 因子）|
| EURUSD | **0** | **0** | 非JPY 独立ペアは OOS 適格すら皆無 |
| GBPUSD | **0** | **0** | 同上 |

**C. 独立性の定量**（日次リターン相関 2021–2024, 1039 日）:

```
        USDJPY  EURUSD  GBPUSD  EURJPY
USDJPY    1.00   -0.45   -0.42    0.69
EURUSD   -0.45    1.00    0.77    0.34
GBPUSD   -0.42    1.00            0.18
EURJPY    0.69    0.34    0.18    1.00
累積対数リターン 2021-24: USDJPY +0.421 / EURJPY +0.253 / EURUSD -0.169 / GBPUSD -0.081
```

### 3.3 断定（第一ピースの決定的所見）
1. **現行プリミティブ（SMA クロス）は 4 銘柄いずれでも CPCV 頑健なエッジをゼロ生成。** USDJPY の home でも 0。
2. **非 JPY 独立ペア（EURUSD/GBPUSD, 対 USDJPY 相関 −0.4x）では OOS 適格すら 0。** SMA が拾えるのは大トレンドのある銘柄だけ。
3. 個体群の「エッジ」の正体は **2021–2024 JPY 安（USDJPY +42% / EURJPY +25%）という単一マクロ因子**。多様な独立エッジではない。相関 clone。
4. ゆえに**多シンボルデータ＋銘柄遺伝子だけでは §4-③（頑健性）を通せない**。SMA のまま EURUSD/GBPUSD を育種宇宙に足しても survivor 0 → 銘柄多様性は JPY へ即崩壊（吸収状態はデータ的にも正当化される）。

---

## 4. 実装ロードマップ（設計実現・最高レバレッジ順）

> 順序原則（regen doc §C / KILL_CRITERIA 準拠）: **安価で決定的な実験を先に**。honest_gate/§4 floor は不変。CPCV 前提。

**Phase 2a（済・本doc）**: 多シンボルデータ供給＋銘柄別 honest 採点＋ハーネス pip 補正。→ 「SMA×多銘柄では独立頑健エッジ無し」を確定。

**Phase 2b — 非トレンド・プリミティブの実装（★最高レバレッジ★）**
- エンジンは既に RSI/BB/MACD/Stoch/VWAP ジェネレータを持つ（`backtester.rs:818-843`）が、育種は探索しない。まず**銘柄別 grid を平均回帰系（RSI/BB）へ拡張**し、EURUSD/GBPUSD/レンジ銘柄で CPCV 頑健エッジが**存在するか**をハーネスで先に判定（安価・決定的）。存在すれば発生器改修が正当化。
- さらに **entry/exit AST を実採点経路へ通す**（`strategy-to-alist` に AST を含め、Rust の AST eval 経路 `:767,811` を有効化）→ 多条件ロジックの縮約を解消。

**Phase 2c — selection-fitness 1本化＋ fitness sharing（regen B-2/B-4/B-5）**
- 生 sharpe 比較子 8 箇所を `selection-fitness` に置換。行動距離（`1−|corr|`）で fitness sharing。過剰最適化を CI 下限で沈める。

**Phase 2d — 銘柄・regime を遺伝子化＋ niche クォータ＋変異（regen B-1/B-3）**
- `extract-genome`/`implant-genome`/距離関数に symbol/timeframe を追加。`breed-strategies` に低頻度 `mutate-symbol`/`mutate-regime`。`run-breeding-cycle` を niche ループ化。**sl/tp を pip/ATR 正規化**（本実験で判明: 絶対価格距離は銘柄間で転移しない）。

**Phase 2e — CPCV 前段ゲート（regen B-6）**
- phase-1(IS) と phase-4(淘汰) の間に purged-CPCV ゲート。CPCV 通過を育種プール入りの条件に。単一窓 fit（本実験の PF2.85/CPCV0.3 型）を種にしない。

**横断規律**: ライブ発注経路（ZMQ/Guardian）不使用。live `swimmy.db` 無改変（実験はコピー/読取専用）。honest_gate/§4 floor 不変。段階コミット・push はオーナー確認後。

---

## 5. 再現コマンド

```bash
# データ変換（検証込み）
python3 logs/tribe_diversity_20260713/convert_bundle_m1.py --symbol usdjpy \
    --out data/tmp/x.csv --validate-against data/historical/USDJPY_M1.csv   # -> VALIDATION PASS
python3 logs/tribe_diversity_20260713/convert_bundle_m1.py --symbol eurusd --out data/historical/EURUSD_M1.csv

# 再現性（引数なし＝§4 byte 再現: 28 適格 / 1 CPCV-ok）
target/release/kill_oos_cpcv.exe --data data/historical/USDJPY_M1.csv \
    --manifest logs/tribe_gonogo_20260713/manifest_full.json --out data/tmp/repro.json

# 銘柄別 grid（pip 補正）
python3 logs/tribe_diversity_20260713/generate_grid_manifest.py --symbol EURUSD --pip 0.0001 \
    --out logs/tribe_diversity_20260713/grid_EURUSD.json
target/release/kill_oos_cpcv.exe --data data/historical/EURUSD_M1.csv \
    --manifest logs/tribe_diversity_20260713/grid_EURUSD.json --slippage 0.0001 \
    --out logs/tribe_diversity_20260713/grid_results_EURUSD.json
python3 logs/tribe_gonogo_20260713/score_gate.py --results <results> --out <gate>
```
