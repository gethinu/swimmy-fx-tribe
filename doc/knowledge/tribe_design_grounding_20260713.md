# 🧭 tribe 多様性エンジン — Phase 0 設計 grounding（審査用）

**日付**: 2026-07-13 JST
**目的**: 実装に着手する前に、「設計ドキュメントが意図した最強の仕組み」を**本文忠実・引用付き**で構造化し、
現行コードとの乖離・単一栽培のルート原因・実装ロードマップを**レビュー可能な形**で提示する。
**方針**: tribe は畳まない。設計の多様性エンジンを実現して作り変える。honest_gate/§4 floor は改変しない。
**この文書のステータス**: **審査待ち**。Phase 1（read-only 診断）は完了。Phase 2（本格実装・破壊的変更）は**本文書のオーナー承認後**に着手する。
**姉妹文書**: 実データ実験の結果は [`tribe_diversity_engine_rebuild_20260713.md`](tribe_diversity_engine_rebuild_20260713.md)。

> **凡例**: 【doc】= 設計文書の本文に明記（引用元を付す）／【code】= 現行コードで確認（file:line）／
> 【実測】= 本タスクの実データ実験／【解釈】= 上記からの私の構造化（推測は明示）。

---

## 0. 一次ソース（何を読んだか）

| ソース | 役割 |
|---|---|
| `doc/knowledge/regen_engine_redesign_20260703.md` | **多様性/生成エンジンの再設計 正本**（§A 原因・§B 再設計・§C KILL 接続）|
| `doc/KILL_CRITERIA_20260703.md` | §4「本物基準」＝多様性・頑健性の合格定義 |
| `doc/knowledge/rebuild_design_fable_P1-P7_20260703.md` | 背骨（検証配線）再構築 P1〜P7 |
| `doc/knowledge/tribe_gonogo_20260713_backbone_breeding_verdict.md` | 背骨修正後 go/no-go（honest PASS=60 / §4 FAIL）|
| `doc/FACTORY.md` | 工場（蠱毒）思想＋feed 契約＋honest gate 意味論 |
| `tools/tribe/honest_gate.py` | 昇格ゲートの実装（floor・DD 正規化）|
| `src/lisp/school/school-founders-*.lisp` | 創業株（seed pool）＝**意図された**シンボル群・regime |

---

## 1. 設計が意図した「最強の仕組み」（多様性/生成エンジン）

### 1-A. 工場思想（何が資産か）【doc: FACTORY.md】
- 価値は**個別戦略ではなく「生成し続け淘汰し続ける機械（蠱毒の壺）」**そのもの（`FACTORY.md:3-8,19-24`）。
- bundle-of-edge（有限・手検証エッジ）と相補: swimmy は**継続生成する工場**。弱点は明記されて「**検証より速く生成する**」（`FACTORY.md:14-18`）。
- 出力は**正直にラベル付けされた feed**（PASS/PROVISIONAL/REJECT）で下流に渡す（`FACTORY.md:23-24,93-102`）。
- 【解釈】設計思想の核＝「多産＋正直な淘汰」。多様性はこの「多産」が**独立した**候補を生むことで初めて価値になる。

### 1-B. honest_gate の設計意図【doc/code: honest_gate.py, FACTORY.md §3】
2 つの原則を強制（`honest_gate.py:10-22`）:
1. **リスク正規化**: `budgeted_monthly = headline × (dd_budget/dd_observed)`（`:77-92`）。見出しリターンではなく**DD バジェット（12%）に換算した値**で採点。
2. **運（luck）でなく証拠（evidence）**: 少数トレード・OOS/CPCV 欠落は flag。IS-only の限界株は**絶対に PASS にせず PROVISIONAL 止まり**（`:148-156,180-185`）。
- floor（`GateConfig`, `:37-45`）: `dd_budget=12% / kpi=3%/mo / min_trades=200 / min_forward_trades=300 / min_pf=1.10 / min_sharpe=0.10`。
- 判定: **PASS**=floor 通過＋OOS/CPCV 検証あり／**PROVISIONAL**=floor 通過だが warn（thin/IS-only）／**REJECT**=hard floor 割れ（`:180-185`）。
- 【解釈】ゲートは「多様性」自体は測らない。多様性は**生成側**の責務で、ゲートは各候補の**正直さ**のみ担保する分業。

### 1-C. §4「本物基準」＝多様性・頑健性の合格定義【doc: KILL_CRITERIA §4】
以下を**すべて**満たすこと（`KILL_CRITERIA_20260703.md:47-52`）:
1. OOS で `trades≥200 ∧ PF≥1.10 ∧ penalized_sharpe≥0.3` を **3 件以上**、
2. かつ **USDJPY 以外 or :TREND 以外が最低 1 件**（＝相関 clone でない多様性の証明）、
3. かつ CPCV `pass_rate≥0.6` で `median_sharpe<2.0`（＝過剰最適化署名でない頑健性）。
- 生 sharpe でなく **penalized/haircut 版**でゲート、実コストは**往復 2pip 以上**（`:54`）。
- 【解釈】§4 は「多様性エンジンの合格証明」の operational な定義。①=証拠量、②=**独立性/多様性**、③=**頑健性**。

### 1-D. 多様性エンジンの設計原則【doc: regen_engine_redesign §B】
正本の設計原則（`regen_engine_redesign_20260703.md:85`）:
> **「多様性は構造で強制、過剰最適化はループ内で罰する。単一の適応度アクセサに一本化する。」**

| # | 機構 | 本文の意図（引用元） |
|---|---|---|
| B-1 | **銘柄・regime を第一級の *変異可能* 遺伝子に** | `genome := (:symbol :regime :timeframe :indicators :entry :exit :sl :tp :volume)`（`§B-1:90-102`）。現状 breed で親1固定→変異昇格。 |
| B-2 | **単一 selection-fitness＋fitness sharing** | `edge=min(haircut, CI下限)×evidence`、norm cap 2.0→1.5、`selection_fitness = base / (1+Σ kernel(distance))`。「同一 niche が 61 個いれば 61 番目の分母 ~61 → 選ばれない」「**除算で強制するのが要点（重み調整で消せない）**」（`§B-2:107-128`）。 |
| B-3 | **niche クォータ＋銘柄/regime 変異＋cross-niche 交配** | `dolist(cat)`→`dolist(niche)`、空 niche に探索ボーナス、`mutate-symbol`/`mutate-regime` 追加＝「**吸収状態を破る唯一の脱出路**」（`§B-3:130-135`）。 |
| B-4 | **行動距離を主指標に** | `1 − |corr(behavior(a),behavior(b))|`、距離に symbol/TF/木編集距離を追加。「遺伝子が違っても収益が同一なら clone 判定」（`§B-4:137-141`）。 |
| B-6 | **purged-CPCV を淘汰の手前にゲート化** | phase-1(IS) と phase-4(淘汰) の間に CPCV ゲート、通過を**育種プール入りの条件**に（`§B-6:151-154`）。 |

### 1-E. 意図されたシンボル群・regime（seed pool）【code: school-founders-*.lisp】
創業株は**多様性を明示的に seed していた**:
- **シンボル（6 通貨ペア）**: USDJPY, GBPUSD, EURUSD, GBPJPY, EURJPY, AUDUSD（`school-founders-*.lisp` 実測 grep）。
- **regime/アーキタイプ（ファイル冒頭コメント）**:
  - dalio = 「**Uncorrelated Return Streams**（Ray Dalio Holy Grail）」＋Volatility Breakout（`school-founders-dalio.lisp:1-6`）
  - breakout = Momentum Breakout（Koroush AK staircase）（`school-founders-breakout.lisp:1-2`）
  - alchemy = 「Carry（swap yield）＋Reversion＋Volatility のハイブリッド」（`school-founders-alchemy.lisp:1-6`）
  - hunted = H4/D1/W1 スイング（`school-founders-hunted.lisp:1-3`）
- 【解釈・重要】**設計は §4 が要求する多様性そのもの（無相関ストリーム・多通貨・多 regime・多 TF）を最初から意図していた。** 現状の単一栽培は「意図の欠如」ではなく**意図からの劣化**である。

### 1-F. データ供給の設計意図【doc: regen §A-1(5), FACTORY.md §2】
- backtest は**銘柄別 CSV** を組む設計（`school-backtest-v2.lisp:56` `data/historical/~a_M1.csv`）（regen `§A-1(5):43-47` が引用）。
- `data/historical/` は**ホスト側 SSD に置き git 管理外**（`FACTORY.md:57-59`）。→ リポジトリだけ見ると価格データが「無い」ように見える。
- 【doc は「🔎推測・要 ops 確認」と明記】単一 CSV override で全銘柄が同一価格採点される疑い（regen `§A-1(5):42-48`、「本番 `SWIMMY_BACKTEST_CSV_OVERRIDE` と `data/historical/` 実在ファイルの確認が必要」）。→ **本タスクで確認済**（§3 R1）。

---

## 2. 設計 ↔ 現行コード 乖離マップ

| 設計要素（意図） | 現行コードの実態（file:line）| 判定 |
|---|---|---|
| **B-1** symbol/regime = 変異可能遺伝子 | `extract-genome`（`school-genome.lisp:62-71`）に **`:symbol` 無し**。`breed-strategies`：`(sym (or (strategy-symbol parent1) "USDJPY"))`（`school-breeder.lisp:1119`）→ 子は親1銘柄固定（`:symbol sym`, `:1207`）。regime も親1継承（`:1204`）。銘柄/regime 変異オペレータ**不在**。 | **未実装** |
| **B-2** 単一 selection-fitness＋sharing | 選択/淘汰の比較子が**生 `strategy-sharpe`**：トーナメント（`school-genome.lisp:210`）、親優先度（`school-breeder.lisp:696,745`）、溢れ淘汰（`:1384,1402`）、デスマッチ（`:1577`）、Stagnant/Weak（`:1651,1666`）、Wisdom（`:1524,1551`）。penalized/CI/evidence は計算後に**未参照**。fitness sharing **無し**。 | **未実装** |
| **B-3** niche クォータ＋変異＋cross-niche | `run-breeding-cycle` は `dolist (cat categories)`（`school-breeder.lisp:1292`）で category 内配偶→`breed-strategies`（`:1328`）。cross-category の `select-category-pair`/`crossover-strategy`（`school-genome.lisp:212,128`）は**主経路から未呼び出し＝デッドコード**。 | **未実装** |
| **B-4** 行動距離を主指標 | `calculate-genetic-distance`（`school-genome.lisp:18-51`）= indicators Jaccard(2)＋category(1)＋SL(1)＋TP(1) のみ。**symbol/TF/entry木/行動なし**。強制ゲートは 0.02 フロア（`school-breeder.lisp:213`）。「sweet spot 0.2–0.8」（`school-genome.lisp:53-56`）は**この経路で未強制**。 | **未実装/劣化** |
| **B-6** CPCV を淘汰の手前に | フロー: phase-1(IS)→phase-4-purge(**純 IS 淘汰**)→phase-6-breeding（`school-connector.lisp:290-314`）。CPCV は A→S 昇格候補のみ（phase-3-5）。**育種プール入りの手前に CPCV ゲート無し**。 | **未実装** |
| **実採点が entry/exit AST を実行** | `strategy-to-alist`（`school-backtest.lisp:90-107`）は sma_short/sma_long/sl/tp/vol/indicator_type/tf のみ emit、**entry/exit AST を送らない**。Rust は AST があれば eval 可（`backtester.rs:767,811`）だが送られないため未実行。 | **劣化（縮約）** |
| **多銘柄データ供給** | `data/historical/` = **`USDJPY_M1.csv` 単一**（実測）。§4 ハーネスも単一 CSV に対して採点。 | **欠落** |
| **多様な seed（§1-E）** | 創業株は 6 通貨・多 regime を seed（`school-founders-*.lisp`）。だが淘汰で EURUSD/GBPUSD 等が消え、feed 対象 489 は USDJPY487/TREND484（gonogo 実測）。 | **seed は健全・下流で崩壊** |
| honest_gate（正直ラベル）| `honest_gate.py` 実装済・floor 健全。背骨修正（P1）で feed へ配線済（gonogo で PASS=60 実証）。 | **実装済（健全）** |
| §4 判定ハーネス | `guardian/src/bin/kill_oos_cpcv.rs`＋`build_manifest.py`＋`score_gate.py` 実在・実走可。 | **実装済** |

**要約**: honest_gate・§4 ハーネス・多様な seed は**在る**。欠落しているのは **regen §B の生成側 5 機構（B-1〜B-4,B-6）と多銘柄データ供給**。単一栽培はこの欠落の帰結。

---

## 3. 単一栽培のルート原因（仮説 → 根拠）

489 個体が USDJPY487/TREND484 に収束する = **4 欠陥の合成による吸収状態（absorbing state）**。regen §A の主張を現行コードで再検証（全て健在）＋実データで 2 点確定。

### R1. 銘柄が遺伝子でない・変異しない ＋ 非USDJPY 価格データ皆無
- **仮説**【doc: regen §A-1(1),(5)】: 銘柄が親1固定・変異なし → 初期 USDJPY 優位が不可逆吸収。単一 CSV 採点の疑い。
- **根拠**【code】: `extract-genome` に `:symbol` 無し（`school-genome.lisp:62-71`）、`breed-strategies:1119,1207` で親1固定。
- **根拠**【実測】: `data/historical/` = `USDJPY_M1.csv` 単一。非USDJPY 生存 2 件は「価格データ不在で honest 評価不能」（gonogo 実測）。→ **doc の🔎推測を確認（YES: 非USDJPY は採点不能）**。
- **判定**: 確定。

### R2. 実採点経路が entry/exit AST を捨て汎用シグナルに縮約
- **仮説**【doc/memory】: 実 BACKTEST は AST を送らず SMA クロスに縮約 → category はコスメティック。
- **根拠**【code】: `strategy-to-alist:90-107` が AST を emit しない。Rust は indicator_type で分岐（`backtester.rs:818-843`: RSI 平均回帰/BB/MACD/Stoch/VWAP…）し AST eval も可（`:767,811`）だが、Lisp が送らないので**先頭 indicator＋先頭 2 整数の汎用シグナル**に縮約。
- **判定**: 確定。**「常に SMA」ではなく「AST 未送出＋個体群が SMA/EMA-TREND に均質化した結果」**。

### R3. 適応度が多様性を一切報酬しない・選択は生 sharpe
- **仮説**【doc: regen §A-1(4),A-2】: 生 sharpe 比較子のみ、novelty/sharing ゼロ → 正のフィードバックで clone 増殖。penalized/CI は配線されず無力。
- **根拠**【code】: §2 の 8 箇所が生 `strategy-sharpe`。penalized/CI/evidence は別スロットに格納され選択系が未参照。
- **判定**: 確定。

### R4. 遺伝的距離・相関ゲートが銘柄/TF/行動を見ない
- **仮説**【doc: regen §A-1(3)】: 距離が indicators/category/SL/TP のみ → 単一栽培を原理的に検知不能。
- **根拠**【code】: `calculate-genetic-distance:18-51`、強制ゲート 0.02 フロア（`school-breeder.lisp:213`）。
- **判定**: 確定。

### R5.（実データが加えた新根拠）現行プリミティブは多銘柄でも頑健エッジを生まない
- **根拠**【実測・§sister doc】: 銘柄別 SMA grid（pip 補正・2pip・CPCV）→ **CPCV 頑健エッジ = 全銘柄 0（USDJPY home でも 0）**。非JPY 独立ペア（対 USDJPY 相関 −0.45/−0.42）は **OOS 適格すら 0**。個体群の「エッジ」= 2021–24 JPY 安の**単一マクロ因子**。
- **含意**: R1〜R4 を全部直しても、**プリミティブが SMA のままだと多様性強制は「6 通貨×4 regime の過剰最適化」を生むだけ**（regen §C-2 の警告が実測で裏付け）。→ ロードマップの優先順位を規定。

> **断定**: 単一栽培は「意図の欠如」ではなく、**多様な seed（§1-E）に対し、生成側 5 機構の欠落（R1〜R4）＋単一データ（R1）が働いた構造的劣化**。加えて **現行プリミティブ自体が独立頑健エッジを持たない（R5）**ため、多様性の実現には**データ/遺伝子だけでなくプリミティブと頑健性ゲート**が要る。

---

## 4. 実装ロードマップ（フェーズ分割・審査対象）

> 順序原則【doc: regen §C, KILL_CRITERIA】: **安価で決定的な実験を先に**。floor 不変・実コスト・purge/embargo CPCV 前提。ライブ発注経路（ZMQ/Guardian 実発注）不使用。live `swimmy.db` 無改変。

| Phase | 内容 | 触るもの | 破壊度 | 依存 |
|---|---|---|---|---|
| **2a（済）** | 多シンボルデータ供給＋銘柄別 honest 採点＋ハーネス pip 補正 | 新スクリプト、`kill_oos_cpcv.rs`（任意 `--slippage` 追加・既定で §4 byte 再現）| 低（追加のみ）| — |
| **2b（次・★最高レバレッジ／決定的実験★）** | grid ハーネスを**非トレンド・プリミティブ（RSI/BB 平均回帰・breakout）**へ拡張し、EURUSD/レンジ銘柄に **CPCV 頑健エッジが存在するか**を先判定 | ハーネス（read-mostly な実験コード）| 低 | 2a |
| **2c** | `strategy-to-alist` に entry/exit AST を含め、Rust の AST eval 経路を実採点で有効化（縮約解消）| `school-backtest.lisp`, Guardian 契約 | **中〜高** | 2b |
| **2d** | 単一 `selection-fitness` 化＋fitness sharing（行動距離）＝生 sharpe 比較子 8 箇所を置換（regen B-2/B-4/B-5）| `school-genome.lisp`, `school-breeder.lisp` | **高**（育種中核）| 2b |
| **2e** | symbol/regime を遺伝子化＋niche クォータ＋変異、**sl/tp を pip/ATR 正規化**（regen B-1/B-3）| `school-genome.lisp`, `school-breeder.lisp` | **高** | 2c,2d |
| **2f** | CPCV 前段ゲート（育種プール入り条件）（regen B-6）| `school-connector.lisp` フロー | **高** | 2d,2e |

**2b を次にやる根拠**: regen §C の順序律（生成器の作り込みより先に「データにエッジが在るか」を安価に確定）に忠実。かつ R5 が示すとおり、**プリミティブに頑健エッジが無ければ B-1〜B-6 は過剰最適化の高速化になる**。2b で「非トレンドの独立頑健エッジが実在する」ことを確認できて初めて、2c 以降の育種中核改修（破壊的）が正当化される。

---

## 5. 審査ポイント（オーナーに確認したいこと）

1. **設計解釈の妥当性**: §1（意図した仕組み）の引用・要約に、doc 本文との齟齬はないか。特に §1-E（seed は多様性を意図）と §1-D（fitness sharing を「除算で強制」）の読み。
2. **ルート原因の断定**: §3 R1〜R5 で、コード/実データ根拠に反論はあるか。R5（プリミティブ自体に頑健エッジ無し）を受け入れるか。
3. **順序**: 「2b（非トレンド・プリミティブの決定的実験）を次に」の優先順位に同意か。それとも 2e（銘柄遺伝子化）や 2c（AST 実行）を先に見たいか。
4. **破壊的改修の範囲**: 2c 以降は育種中核（`school-breeder`/`school-genome`）と Guardian 契約に触れる。ブランチ戦略・段階コミット・検証観点の希望。

**現時点で停止し、本審査の結果を待つ。**
