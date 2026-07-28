# tribe-2d ⑬ — 二つの未踏角度: (1) ポートフォリオ結合 (2) 複数日barrier × 暦シーズナリティ (measured 2026-07-28)

**Question (owner).** ⑨〜⑫ で「6 primitive × FX × per-regime CPCV」の網羅空間は honest に潰し切った。
再スキャンは禁止。まだ試していない **2 つの角度**だけを突く:

- **角度1（本命・新しい問い）= ポートフォリオ結合.** これまで全段が「**1 本で** deploy 基準を越えるエッジ」を
  探してきた。だが今日までに forward-robust で見つかった**無相関の点を "束ねて" deploy 基準を越えるか**は
  一度も検証していない。個別エッジではなく**スリーブ**として採点する。
- **角度2（副）= 未踏の「低頻度・厚い」新 primitive を 1 つ.** ⑩ が「壁は COST、レバーは低頻度・厚い/trade」と
  示した。未実装の方向を 1 つ: **複数日 barrier（1 トレードを厚く）× event/seasonality ゲート（稀に確信で入る）**。

**Verdict up front（実測 — どちらも "越えない"、ただし理由は互いに違い、両方とも新情報）:**

- **角度1: 分散の利得は「本物だが、教科書どおりの算術ちょうど」で、deploy 基準には届かない。**
  無相関 2 点（MR-EURUSD H4 ＋ UK100-MR H2、日次 corr **+0.005**）を equal-risk で束ねると
  SEL Sharpe **0.571/0.492 → 0.757**、maxDD 1.18/2.35 → **1.25 vol-year**、Calmar 0.48/0.21 → **0.60**。
  GBPUSD-MR を足した 3 本では SEL **0.805** / HOL **0.742**、CPCV 0.80/0.70 で**ポートフォリオとして 2 窓 honest floor を PASS**
  （構成員 3 本のうち 2 本は単体では 2 窓 floor を落とす）。**しかし事前登録した deploy-grade 基準
  （Sharpe ≥ 1.00 かつ Calmar ≥ 0.50 を両窓で）には届かない。**
  さらに決定的に: **実測 = `Σ Sh_i / √(1'R1)` の予測値と ±0.007 で一致**（4 ケース全て）。
  つまり利得は**相関構造の算術そのもの**であり、それ以上でも以下でもない。⇒ Sharpe 0.5 級の構成員から 1.0 に
  届くには**完全無相関のスリーブが 4 本**必要。realistic コストで手元にあるのは **3 グループ（うち 2 つは +0.22 相関）**。
  **算術的に足りない。**
- **角度1 の、より重い発見: 上の "PASS" は後知恵であって、実時間では取れない。**
  2021 年 1 月時点の情報だけ（holdout 2015-21 で robust）でメンバーを選び、手つかずの 2021-25 で走らせる
  **真の OOS プロトコル P-B は、どの構成規則でもマイナス〜ゼロ**: 全 117 本 group-equal **−0.442**、
  全 117 本 member-equal **+0.090**、"instrument ごとにベスト 1 本" 5 本束 **−0.237**。
  逆向き（2021-25 で選び 2015-21 で採点）も 0.225 で floor 落ち。
  **摩擦ゼロなら同じ P-B が +0.714（floor PASS, 8 instrument）**になる ⇒ ポートフォリオという発想自体は健全で、
  実コストが**事前候補集合を 8 instrument → 5 に削り、生き残りをマイナスに変える**。壁は再び **COST**。
- **角度2: realistic 2-pip で 0。** 2,160 config 中、2 窓 forward-robust は **1 本だけ**で、それは
  既知の `keltner-EURUSD-MR-H4`（**新軸を両方 OFF にした config**）。暦ゲート付き **0**、load-bearing **0**、
  既知家系の外 **0**。
- **ただし角度2 は ⑩ の DENSITY 壁を設計で解いた上で、摩擦ゼロで 7 本の "load-bearing かつ既知家系の外" を出した。**
  M15 ベース × 20 日 barrier × 四半期末月ゲートで trades 380–1138（密度クリア）。
  **⑧ の網羅 census で摩擦ゼロでも 0 だった EURJPY と、USDJPY-REVERSION（diverse）に新機序が出る**。
  そして**全部 2-pip で死ぬ**（PF −0.036〜−0.187、CPCV pass-rate 0.70-0.90 → 0.30-0.50）。
  コスト感度: 3 本が **0.5 pip RT では生存**、**1.0 pip で全滅**。
- **暦効果（四半期末月）は「小さいが本物」であることも測った。** 勝者だけでなく全格子 410-428 の
  matched cell で pf(QEND) − pf(NONQ) = 平均 **+0.021〜+0.051**、勝率 **52.7-57.5%**（4 条件すべて同符号）。
  **本物のシーズナリティ。だがその大きさは、越えねばならない摩擦（−0.08〜−0.19 PF）の 2〜4 分の 1。**

**総合: 単一栽培は deployment gate で依然不破。「分散で越えた」は作れなかった — 数字で越えない。**

Guardrails honored: redirect ハーネスのみ（standalone `primitive_scan.exe`）、repo ルートで `run-all-tests` は
未実行、ライブ `swimmy.db` **未オープン**（本 run は DB パイプライン・daemon を一切使わない）、honest floor 定数
（`200 / 1.10 / 0.30 / 0.60 / 2.0`）は**読むだけ・diff に floor 行 0**（確認済）、新軸は **flag-gated で OFF は byte-identical**
（新キー無し manifest で `--out` SHA256 一致: `feac84e7…` before==after）、genome gene 未投入・フラグ未 flip、
持ち込みデータ commit なし（`logs/` は gitignore）、無関係 dirty（LEGEND / guardian runtime json / RECRUIT）未接触。

---

## 1. 角度1 — ポートフォリオ結合

### 1.1 なぜこれが本当に未踏か

⑧〜⑫ は毎回「1 本で box の内点になるか」を問うた。box の内点は 2 点（MR-EURUSD H4/H6・UK100-MR H2）、
どちらも sub-deploy。⑫ は両者の return 相関が ≈0 であることまで測った**が、束ねた場合の合成指標
（ポートフォリオ Sharpe / maxDD / 2 窓 robust）は一度も計算していない**。本節がそれ。

### 1.2 方法（都合の良い選択が入り得る箇所を全て明示）

| 項目 | 決定 | なぜそれが honest か |
|---|---|---|
| リターン列 | **無改変**スコアラー（⑫ の verification-only `--dump-daily`）で **2015-2025 全期間を一続き**に出力し、オフラインで SEL/HOL に切る | 窓ごとに backtest を再起動しない＝実運用の連続履歴。窓別 gate 数値は別途の 2 窓スキャンから取る |
| 日次規約 | エンジンと同一（`start_day..=end_day`、無取引日は 0） | ポートフォリオ Sharpe が per-config Sharpe と直接比較可能 |
| equal-risk | **因果的**。t 日の重み = TARGET / σ_t、σ_t は**厳密に t より前**の 252 暦日で推定 | in-sample vol scaling は look-ahead で Sharpe を水増しする。使わない |
| 重み付け | member 等リスク と **group（＝instrument）等リスク**の両方 | EURUSD keltner の 17 変種はほぼクローン。1 本の UK100 を票数で潰させない |
| 感度 | ① 因果 rolling-252d ② HOL 期間だけで推定した**定数**スカラー（SEL 評価に対して因果） ③ 全期間定数（**覗く。明示**） | ③ は「相関効果」だけを推定器ノイズから切り離すため。覗くことを隠さない |
| ポートフォリオ gate | honest floor を**保守側に**適応: trades=構成員合計、PF=**日次**集計、pen_sharpe の trade 数は**活動日数**（実 trade 数以下＝罰が重い）、CPCV=日次 10 ブロック | 適応は全て「見栄えを悪くする方向」。floor 定数自体は不変 |
| deploy-grade 基準 | **走らせる前に**スクリプトへ事前登録: 両窓で Sharpe ≥ 1.00 かつ Calmar ≥ 0.50（どちらも**レバレッジ不変**） | 事後に基準を動かせない。構成員は個別 Sharpe ~0.4-0.7 なので「分散が倍にするか」を直接問う |
| メンバー選択 | **P-A** = 2 窓 robust（後知恵・比較用）／**P-B** = **holdout だけで選び SEL で採点（真の OOS）**／**P-C** = 逆向き | チェリーピックの対照。P-B が「2021 年 1 月に実際に組めた束」 |

**ハーネス検証（VALIDATION）:** 全期間 dump を窓で切った系列は 2 窓スキャンの gate 数値を再現する —
`keltner-EURUSD-MR-H4` SEL sharpe **0.571 vs gate 0.569**、HOL **0.433 vs 0.433**。
UK100 だけ dump 側が低く出る（SEL 0.492 vs 0.530）が、これはエンジンが `dp/eq` の**複利**で測るのに対し
本 dump は非複利だから（UK100 は PnL が eq=10000 に対し大きい）。**保守側のズレ**なので採用する。

**候補プール（新規の単体エッジ探索ではない）:** ⑫ と同じ 186-config 汎用格子を 10 instrument に、
さらに ⑧ が名指しした Keltner/BB-MR EUR/USD 近傍だけ 420-config で再導出（**メンバーのリターン列が必要**なため。
既知領域の再導出であって網羅再スキャンではない）。realistic で 2 窓 forward-robust = **21 本**
（EURUSD 18 / GBPUSD 2 / UK100 1）＝ ⑧ の「15 本・1 家系」と整合（barrier 変種の分だけ密）。

### 1.3 結果 — 名指しの束（realistic 2-pip、per-member 等リスク）

`N1` = `keltner-EURUSD-MR p50 d2.0 H4` ／ `N2` = `keltner-UK100-MR p50 d3.0 H2` ／ `N3` = N1+N2 ／ `N4` = N3 + best GBPUSD-MR

**日次リターン相関 corr(N1,N2) = +0.0053**（共通 2017-2025）— ⑫ の +0.019 と整合、実質無相関。

*(a) 定数スカラー = HOL 期間推定（SEL 評価に対して因果）:*

| 束 | SEL sharpe | SEL maxDD(vol-yr) | SEL Calmar | HOL sharpe | HOL Calmar | 2 窓 floor | deploy-grade |
|---|---:|---:|---:|---:|---:|---|---|
| N1 単体 | 0.571 | 1.18 | 0.48 | 0.433 | 0.20 | FAIL | FAIL |
| N2 単体 | 0.492 | 2.35 | 0.21 | 0.385 | 0.17 | FAIL | FAIL |
| **N3 = N1+N2** | **0.757** | **1.25** | **0.60** | **0.570** | 0.25 | **PASS** | **FAIL** |
| **N4 = N3+GBPUSD** | **0.805** | **1.00** | **0.80** | **0.742** | 0.47 | **PASS** | **FAIL** |

*(b) 因果 rolling-252d（両窓で因果。推定器ノイズを含む）:* N3 SEL **0.785**（Calmar 0.57・floor PASS）／
HOL **0.387**（floor FAIL）。N1 単体は HOL で 0.433 → **0.170** に落ちる。
⇒ **vol-targeting オーバーレイ自体がリスク**（推定器ノイズが薄いエッジを削る）。正直に両方載せる。

*(c) 全期間定数（覗く・開示）:* (a) とほぼ同値（N3 SEL 0.756 / HOL 0.571）。
⇒ (a) の因果推定は安定していて、覗きによる差はほぼ無い。

### 1.4 利得は「算術ちょうど」か — 自分を騙していないかの検算

k 本 equal-risk・相関行列 R なら合成 Sharpe = `Σ Sh_i / √(1'R1)`。

| 窓 | 束 | 構成員 Sharpe | 1'R1 | **予測** | **実測** | 差 |
|---|---|---|---:|---:|---:|---:|
| SEL | N3 | 0.571, 0.492 | 1.983 | 0.755 | **0.757** | +0.002 |
| SEL | N4 | 0.571, 0.492, 0.472 | 3.616 | 0.807 | **0.805** | −0.002 |
| HOL | N3 | 0.433, 0.385 | 2.058 | 0.570 | **0.570** | −0.000 |
| HOL | N4 | 0.433, 0.385, 0.547 | 3.393 | 0.741 | **0.742** | +0.001 |

**利得は純粋に相関構造の算術。** 魔法はない代わりに、**必要本数も算術で決まる**:
個別 Sharpe ≈0.5 から deploy 基準 1.00 に届くには **完全無相関のスリーブ k = (1.00/0.5)² = 4 本**。
realistic コストで手元にあるのは **3 グループ**、しかも EURUSD-GBPUSD は **+0.217** 相関（UK100 は ±0.01 で無相関）。
**「分散で越える」は、現在の在庫では算術的に不可能** — 越えるには *realistic-cost で 2 窓 robust な無相関スリーブ*
をあと 1〜2 本**新規に見つける**必要がある。これは ⑨〜⑫ が失敗し続けた、まさにその問題に戻る。

### 1.5 P-B — 真の OOS（2021 年 1 月に組めた束か）

**holdout（2015-21）だけで robust だったものを選び、手つかずの 2021-25 で採点:**

| 構成規則 | メンバー | SEL sharpe | 日次PF | CPCV | floor |
|---|---:|---:|---:|---:|---|
| 全 holdout-robust, group 等リスク | 117 (5 instr) | **−0.442** | 0.907 | 0.00 | FAIL |
| 全 holdout-robust, member 等リスク | 117 | **+0.090** | 1.024 | 0.30 | FAIL |
| **instrument ごとベスト 1 本（事前規則）** | **5** | **−0.237** | 0.947 | 0.20 | FAIL |
| （参考）その中で SEL を生き延びた唯一 = UK100 単体 | 1 | +0.523 | 1.200 | 0.60 | PASS |

**P-C（逆向き: 2021-25 で選び 2015-21 で採点）** も 4 グループ束で **+0.225 / floor FAIL**。

**摩擦ゼロなら同じ P-B は成立する:** instrument ごとベスト 1 本 × **8 instrument** で SEL **+0.714 / floor PASS**
（構成員は個別に 0.09〜0.65 でほとんどが単体 floor 落ち — まさに分散の教科書どおりの働き）。
全 205 本 member-equal でも +0.425 / floor PASS。

⇒ **ポートフォリオという発想は健全（摩擦ゼロで実証）。実コストが事前候補集合を 8 instrument → 5 に削り、
残りをマイナスに変える。壁は COST。** そして **P-A の PASS は後知恵**である（2 窓 robust は事後にしか分からない）。

---

## 2. 角度2 — 複数日 barrier × 暦シーズナリティ（新 primitive、flag-gated）

### 2.1 実装 — 「本当に足りない自由度」だけを 1 つ足す

⑩ の shortlist の (a) seasonality は「⑨ と同じ entry-gate 族だから」と見送られたが、**厚さと組む形では未測定**。
そして重要な事実: **⑨ の clock gate は hour-of-day ＋ day-of-**WEEK** だけで、暦（day-of-month / month-of-year）を
表現できない**。turn-of-month のリバランス・フローや月次シーズナリティは**現状表現不能**。厚さ側
（`hold_mode="barrier"` ＋ wide ATR tp ＋ multi-day `max_hold`）は ⑩ で既に存在する。
⇒ **新規コードは暦ゲートのみ**（最小増分）。

`guardian/src/bin/primitive_scan.rs` に flag-gated で 3 フィールド:

| field | default | 意味 |
|---|---|---|
| `dom_lo` / `dom_hi` | 0 / 0 | day-of-month 窓（両端含む、1..31）。`lo>hi` は月境界をまたぐ（`25..5` = turn-of-month） |
| `month_mask` | 0 | 月 1..12 のビットマスク。0 = 全月 |
| `calendar` | `""` | 人間可読ラベルのみ。backtest ロジックは読まない |

3 つ全部が既定値なら `in_calendar()` は 1 行目で `true` に短絡 ⇒ **新キー無し manifest は pre-calendar エンジンと byte-identical**。

**byte-identity 証明**（60-config identity manifest、新キー一切なし）:
```
id_OLD.json sha256 = feac84e75cf5abefc19042bf5647d870de1360b97b60d23da3711ae255064ac2
id_NEW.json sha256 = feac84e75cf5abefc19042bf5647d870de1360b97b60d23da3711ae255064ac2   (IDENTICAL)
```

**ゲートが動く／不活性でないことの検証**（`keltner-EURUSD-H4`、全期間 620 trades を基準）:

| gate | 期待通過率 | 実測 trades | 実測比 |
|---|---:|---:|---:|
| キー無し | 100% | 620 | — |
| `NONE`（明示ゼロ） | 100% | **620** | **キー無しと完全一致** |
| `TOM` (dom 25..5) | ~39% | 288 | 46% |
| `MID` (dom 8..22) | ~48% | 324 | 52% |
| `QEND` (3,6,9,12月) | 33.3% | 227 | 37% |
| `JAN`（1月のみ） | **8.3%** | 50 | **8.1%** |

1 月のみが 8.1% と理論値 8.3% にほぼ一致 ⇒ **暦演算（days→civil）は正しい**。

**honest scope の明示:** これは "event/seasonality" の**暦（calendar）側**である。
真のマクロ**イベント**ゲート（NFP / FOMC / CPI）は**この repo に経済カレンダーが存在しない**（`data/macro/` は空）ため
**実装できず、価格プロキシで偽装もしない — 未検証のギャップとして明示的に残す**（§4）。

### 2.2 格子 — DENSITY を設計で解く

⑩ は barrier-config の **62% が <200 trades** で死んだ。今回はそれを設計で潰す:
**高頻度の base core ＋ TF を M15 から始める**ので、暦が 1/3〜1/2 を落としても 200 を超えられる。
厚さは barrier ＋ wide ATR tp ＋ multi-day time-stop が担う。

12 cores × 3 TF (M15/H1/H4) × 3 exits (`SIG` 対照 / `B5d` sl2.5-tp8-5日 / `B20d` sl3-tp6-20日)
× 5 gates (`NONE` 対照 / `TOM` / `MID` / `QEND` / `NONQ`) = **540/銘柄 × 4 銘柄 = 2,160 config**。
（多重検定面は ⑩ の 768 の約 3 倍。**隠さず開示**する。規律は 2 窓 forward gate と下記 load-bearing 対照。）

**LOAD-BEARING 対照（ラベル運の排除）:** 各ヒットについて
`gate-load-bearing` = 同じ (core,TF,exit) の **`NONE` 兄弟が robust でない**、
`hold-load-bearing` = 同じ (core,TF,gate) の **`SIG` 兄弟が robust でない**。
兄弟も robust なら「既知エッジが新しいラベルを着ただけ」。

### 2.3 結果

**realistic 2-pip（deploy バー）:**

| | 値 |
|---|---:|
| 2 窓 forward-robust | **1** |
| うち暦ゲート付き | **0** |
| うち gate-load-bearing | **0** |
| うち既知 keltner-MR EUR/GBP-USD 家系の外 | **0** |
| **両方満たす（新軸由来の新家系）** | **0** |

唯一の生存は `keltner-EURUSD-REVERSION-p50-d2.0-H4-**SIG**-**NONE**` = **新軸を両方 OFF にした既知の点**。

**摩擦ゼロ上限（機序は存在するか）: 10 本 forward-robust、うち 9 本が暦ゲート付き・9 本 gate-load-bearing・
7 本が既知家系の外。**

| 銘柄 | config | SEL (t, pf, pr) | HOL (t, pf, pr) |
|---|---|---|---|
| EURUSD | `stoch-MR-M15-B20d-QEND` | 380, 1.231, 0.90 | 550, 1.298, 0.80 |
| EURUSD | `stoch-MR-M15-B5d-QEND` | 373, 1.183, 0.60 | 547, 1.203, 0.70 |
| **EURJPY** | `bb-MR-M15-B20d-QEND` | 495, 1.128, 0.60 | 644, 1.185, 0.80 |
| **USDJPY** | `keltner-MR-M15-B20d-QEND` | 495, 1.238, 0.70 | 595, 1.194, 0.80 |
| **USDJPY** | `bb-MR-M15-B20d-QEND` | 524, 1.220, 0.70 | 608, 1.129, 0.60 |
| **USDJPY** | `bb-MR-M15-B5d-QEND` | 469, 1.179, 0.70 | 594, 1.218, 0.60 |
| **USDJPY** | `stoch-MR-M15-B20d-QEND` | 447, 1.156, 0.70 | 555, 1.163, 0.70 |

**EURJPY は ⑧ の網羅 census で "摩擦ゼロでも 0"** だった銘柄で、そこに初めて 2 窓 robust な機序が出た。
USDJPY-**REVERSION** は `diversity_ok`（symbol≠USDJPY **or** regime≠TREND）を満たす**多様**エッジ。
trades は 380〜1138 で **DENSITY は完全にクリア** — ⑩ の壁は設計で解けた。

**そして全部 2-pip で死ぬ。壁は COST。**

| config | fric PF (SEL/HOL) | 2pip PF (SEL/HOL) | PF gap | CPCV pr の落ち方 |
|---|---|---|---:|---|
| `stoch-EURUSD-M15-B20d-QEND` | 1.231 / 1.298 | 1.058 / 1.145 | −0.173 / −0.154 | 0.90→0.50, 0.80→0.50 |
| `keltner-USDJPY-M15-B20d-QEND` | 1.238 / 1.194 | 1.066 / 1.053 | −0.173 / −0.141 | 0.70→0.40, 0.80→0.30 |
| `bb-EURJPY-M15-B20d-QEND` | 1.128 / 1.185 | 1.019 / 1.100 | −0.109 / −0.085 | 0.60→0.50, 0.80→0.60 |
| `stoch-USDJPY-M15-B20d-QEND` | 1.156 / 1.163 | 1.120 / 1.046 | −0.036 / −0.117 | 0.70→0.40, 0.70→0.30 |

**コスト感度（RT）:** 0.5 pip で **3 本生存**（`keltner-USDJPY-M15-B20d-QEND` を含む — diverse!）、
**1.0 pip で全滅**、2.0 pip で 0。⑨ の薄い M15 は 2-pip で PF < 1.0 に崩壊したが、ここは 1.02–1.14 で
**厚さは効いている**。だが足りない。

### 2.4 暦効果は本物か、それとも選択の産物か

QEND が勝者 9 本中 7 本を占める。勝者だけを見て「四半期末シーズナリティ発見」と言うのは
過去に最も高くついた種類の誤り。**全格子の matched cell（同じ symbol/core/TF/exit で gate だけ違う組）**で検定した:

| cost | 窓 | matched cells | pf(QEND)−pf(NONQ) 平均 | 中央値 | QEND 勝率 | pf(QEND)−pf(NONE) 平均 |
|---|---|---:|---:|---:|---:|---:|
| fric | SEL | 410 | +0.0234 | +0.0054 | 52.7% | +0.0217 |
| fric | HOL | 428 | +0.0488 | +0.0328 | 55.6% | +0.0386 |
| real | SEL | 410 | +0.0209 | +0.0105 | 52.9% | +0.0199 |
| real | HOL | 428 | +0.0508 | +0.0328 | 57.5% | +0.0396 |

**4 条件すべてで同符号・勝率 >50% ⇒ 四半期末月の平均回帰チルトは「小さいが系統的」＝本物。**
**しかしその大きさ（+0.02〜+0.05 PF）は、越えねばならない摩擦（−0.08〜−0.19 PF）の 2〜4 分の 1。**
これが「壁は COST」の最も精密な言い方であり、**近傍を掘っても越えない**理由でもある
（シグナル自体が摩擦より一桁近く小さい）。

---

## 3. 正直な総合判定

| 問い | 実測 |
|---|---|
| 無相関の sub-deploy 点を束ねると合成 Sharpe は上がるか | **YES** — N3 SEL 0.757（最良単体 0.571 比 +32%）、maxDD 改善、Calmar 0.48→0.60 |
| 束ねると deploy 基準（Sharpe≥1.0 & Calmar≥0.5 を両窓）を越えるか | **NO** — 最良でも SEL 0.805 / HOL 0.742 |
| 利得は算術以上のものか | **NO** — `ΣSh/√(1'R1)` の予測と ±0.007 で一致。**必要本数 = 4 本無相関、在庫 3 グループ（うち 2 つ +0.22 相関）** |
| 束は少なくとも honest floor を 2 窓で通るか | **YES（後知恵の P-A で）** — 構成員 3 本中 2 本が単体では落ちる 2 窓 floor を、束は PASS |
| その PASS は実時間で取れるか | **NO** — 真の OOS（P-B）は −0.442 / +0.090 / −0.237。逆向き P-C も floor FAIL |
| ポートフォリオという発想自体は健全か | **YES、ただし摩擦ゼロで** — P-B 摩擦ゼロは +0.714 / floor PASS（8 instrument） |
| 角度2 の新 primitive は realistic で家系の外に点を出したか | **NO — 0**（2,160 config、暦ゲート 0・load-bearing 0） |
| 角度2 は何かを前進させたか | **YES** — ⑩ の DENSITY 壁を設計で解き、摩擦ゼロで **7 本の load-bearing な家系外**（EURJPY・USDJPY-REVERSION 含む、⑧ census で 0 だった領域）。四半期末シーズナリティが**小さいが本物**であることも定量化 |
| 角度2 の壁は何か | **COST**（⑨ と同じ）。density はクリア、robustness もクリア、0.5pip 生存・1.0pip 全滅 |
| 「無い」のか「探索が浅い」のか | **無い（現在の在庫・現在のコストでは）**。角度1 は算術で本数不足が確定、角度2 はシグナル/摩擦比が 1/2〜1/4 で確定。どちらも "もっと密な格子" では動かない構造的理由 |

**壁の三巡:** ⑨ *fewer* → COST。⑩ *fatter* → DENSITY。**⑬b *fewer × fatter* → density は解けた、COST に戻った。**
そして **⑬a portfolio → COST が事前候補集合を削り、後知恵でしか PASS しない。**
joint box `{trades≥200 ∧ realistic-cost-survivable ∧ 2-window-robust}` の内点は依然
**MR-EUR/USD H4/H6 ＋ native UK100-MR H2 の 2 点（ともに MR・ともに sub-deploy）**。

**Deploy NO。フラグ OFF。** 暦軸のコードは shippable（OFF は byte-identical、floor 中立）。
本番 ON は測定上正当化されない。

---

## 4. 次の一手（オーナー判断）

1. **打ち止めに近い、が「打ち止め」ではない — 残ったのは 1 本の、明確に条件付きのレバー。**
   角度1 が数量化した必要条件は具体的だ: **realistic コストで 2 窓 robust、かつ既存 3 グループと無相関な
   スリーブがあと 1〜2 本**。それが揃えば束の Sharpe は算術で 0.95〜1.10 に届く（現状 0.805 + 1 本無相関 0.5 級 → √ 効果）。
   ⑫ が示したとおり、その供給源として唯一実績があるのは **native な別アセットクラス**。
   ⑫ で **rates（金利先物）だけが「イントラデイ・データが存在しない」ため未検証**のまま残っている。
   これは「試して失敗した」のではなく「**試していない**」唯一の箱。無料（Dukascopy public feed）だが
   symbol 文字列と価格スケールが**未検証の推測**で、phantom-data リスクがある ⇒
   **scratch への小さな probe fetch ＋ schema/scale 検証を先にやる**なら筋が通る。オーナー go/no-go。
2. **経済カレンダー（NFP/FOMC/CPI）ゲートも "未検証の箱"。** 本 run は暦（構造）側だけを実装した。
   真のイベントゲートには外部カレンダー・データが要る（repo に無い）。
   ただし §2.4 の測定（シグナル +0.02〜0.05 PF ≪ 摩擦 0.08〜0.19 PF）は、
   **entry-gate 族全般に対する強い事前予測**を与える: イベントゲートも同じ比率で負ける公算が高い。
   やるなら「厚さ」と組んだ上で、**M15 ではなく H4 以上の厚い base** で。優先度は 1. より低い。
3. **やってはいけないこと（明示）:** 0.5 pip 生存を根拠にコスト仮定を下げること、
   density/CPCV floor を下げて near-miss を通すこと、P-A の後知恵 PASS を deploy 根拠に使うこと。
   いずれも過去に最も高くついた形の自己欺瞞で、本 run はそれを避けるために P-B と算術検算を先に組んだ。
4. **両軸ともコードは shippable（flag OFF・byte-identical・floor 中立）。** マージは安全。
   本番 flag-ON は測定上正当化されない（オーナー判断・未実施）。

---

### Reproduce

**角度1** `logs/tribe_2d_portfolio/`（gitignored）: `gen_native_grid.py`（⑫ 由来 186-config プール格子）、
`gen_mr_neighbourhood.py`（⑧ 既知 MR 近傍 420-config の再導出）、`run_pool.sh` / `run_fx_pool.sh`（2 窓 ×2 コスト）、
`gen_member_manifests.py`（robust 候補だけ dump 対象に絞る）、`run_dump.sh`（全期間 `--dump-daily`）、
`portfolio.py`（因果 equal-risk・適応 gate・指標）、`run_portfolio.py`（P-A/P-B/P-C・名指し束・算術検算）、
`VERDICT_{real,fric}.txt`、`PORTFOLIO_{real,fric}.json`。

**角度2** `logs/tribe_2d_calendar/`（gitignored）: `gen_cal_grid.py`（540/銘柄）、`run_cal.sh`、
`score_cal.py`（2 窓 join ＋ NONE/SIG 兄弟 load-bearing 判定）、`diagnose_cal.py`（QEND matched-cell 検定 ＋ cost gap）、
`VERDICT_{real,fric}.txt`、`DIAGNOSE.txt`、`CAL_SUMMARY_{real,fric}.json`、
`fire_check.json`（ゲート発火検証）、`identity_check.json` / `id_OLD.json` / `id_NEW.json`（byte-identity）、
`primitive_scan_OLD.exe`（改変前バイナリ）。

Binary: `guardian/src/bin/primitive_scan.rs` — 角度1 は**無改変**（`--dump-daily` は ⑫ 由来）、
角度2 は `dom_lo` / `dom_hi` / `month_mask` / `calendar` の 4 フィールドと `in_calendar()` / `civil_from_days()` を追加。
byte-identity は「新キーを一切含まない manifest」で `--out` の SHA256 を改変前後で比較（上記 `feac84e7…`）。
大きい格子 JSON / 日次 JSONL は scratch（gitignore）に置き、生成器から再現可能。
