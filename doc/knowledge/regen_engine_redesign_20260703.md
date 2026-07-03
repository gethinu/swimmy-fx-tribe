# 🧬 生成エンジン根本再設計 — 収束と過剰最適化の原因特定＋再発防止設計

**作成日:** 2026-07-03 JST
**ステータス:** 設計提案（未commit・未実装）。実コード根拠に file:line、推測は「🔎推測」で明示。
**上位判定:** [`KILL_CRITERIA_20260703.md`](../KILL_CRITERIA_20260703.md)（本再設計はこの判定に従属する）
**関連:** [`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md)

> **結論を先に**: autopsy が指摘した2欠陥は、いずれも**コードで確定的に再現できる構造欠陥**である。
> ①「USDJPY×TREND への収束」＝ *銘柄と regime が遺伝子でなく、変異せず、育種が同一 niche 内で回り、適応度が多様性を一切報酬しない* ための **吸収状態（absorbing state）**。
> ②「少数サンプルの過剰最適化が生き延びる」＝ *ハンディキャップ（penalized_sharpe / evidence shrink / CI 下限）は実装済みだが、生成・淘汰ループの比較子が全て生 `strategy-sharpe` を読む* ための **選択圧の抜け**。
> ただし本再設計は**発生器の改良であって、データに無いエッジを生み出さない**。KILL_CRITERIA の期限実験（61 PROVISIONAL の実コスト OOS）より前に着手してはならない（§C）。

---

## A. 根本原因（コード根拠）

### A-1. なぜ USDJPY × TREND に収束するのか

#### (1) 銘柄が遺伝子でない・変異しない ＝ 吸収状態【確定】
- `extract-genome` に `:symbol` が無い。遺伝子は indicators/entry/exit/sl/tp/volume/timeframe/category のみ（`school-genome.lisp:62-71`）。
- 交叉 `crossover-genes` も銘柄を扱わない（`school-genome.lisp:89-126`）。構造変異 `mutate-sexp-tree` / `perform-structural-mutation` も entry ロジックのみ（`school-genome.lisp:152-196`）。
- 実際に使われる `breed-strategies` は **`sym = (or (strategy-symbol parent1) "USDJPY")`**（`school-breeder.lisp:1119`）で親1の銘柄をそのまま継承し、`:symbol sym`（`:1207`）で子に固定。**銘柄変異オペレータが存在しない。**
- → 子の銘柄は常に親1の銘柄。初期集団が USDJPY 優位なら、**銘柄は不可逆に USDJPY へ吸収される**（数学的には吸収状態）。166/168 が USDJPY という実測はこの直接の帰結。

#### (2) regime も親1継承・育種は同一 category 内で閉じる【確定】
- `breed-strategies` は `:category parent-regime-class`（親1の regime、`school-breeder.lisp:1204`）。
- `run-breeding-cycle` は `(dolist (cat categories) …)` で category ごとに `all-warriors` を当該 cat に絞ってから配偶（`school-breeder.lisp:1288-1300`）。**category 間の遺伝子流入が主経路に無い。**
- category を混ぜる関数 `select-category-pair`・`crossover-strategy`（`school-genome.lisp:128,212-230`）は **`run-breeding-cycle` から呼ばれない**（`school-breeder.lisp:1280-1358` は `breed-strategies` を呼ぶ）＝実質デッドコード。
- → TREND clone は TREND としか交配せず、regime も固定される。

#### (3) 遺伝的距離が銘柄・TF・ロジック木を見ない → 相関ゲートが単一栽培を検知できない【確定】
- `calculate-genetic-distance` の重みは indicators Jaccard(2) + category一致(1) + SL差(1) + TP差(1) の計5（`school-genome.lisp:18-51`）。**銘柄・timeframe・entry/exit 木構造が距離に入っていない。**
- 実際に効くゲートは `strategies-correlation-ok-p`（`school-breeder.lisp:1506-1517`）で、閾値は **`*breeder-min-genetic-distance*`=0.02**（`school-breeder.lisp:213`、complement では 0.01 まで緩和 `:215-217`）。上限なし。
- コメント/`genetic-compatibility-p` が謳う「sweet spot 0.2–0.8」（`school-genome.lisp:53-56`）は**この経路で強制されていない**（0.02 フロアのみ）。
- → 「61個の USDJPY-TREND clone」は距離 0.02 以上でありさえすれば自由に交配。相関ゲートは銘柄単一栽培を**原理的に**見られない。

#### (4) 適応度が多様性をゼロも報酬しない（正のフィードバック）【確定】
- 親優先度 `strategy-breeding-priority-score` = rank + culling-score + generation + cpcv + oos − threshold-penalty（`school-breeder.lisp:734-763`）。**novelty / fitness-sharing / 銘柄・regime 分散項が一切無い。**
- トーナメント選択 `select-parent-tournament` は **生 `strategy-sharpe` で降順ソート**（`school-genome.lisp:202-210`）。
- → 初期に最強のクラスタ（USDJPY-TREND 創業株）が毎サイクル親を独占 → その子孫がまた最強 → **正のフィードバックで61変奏に収束**。多様性には負の圧すら無く、ゼロ。

#### (5) 「多銘柄」が実質1銘柄の疑い【🔎推測・要ops確認】
- backtest は銘柄別 CSV を組む設計（`school-backtest-v2.lisp:56` `data/historical/~a_M1.csv`）だが、
  - `*backtest-csv-override*`（`config.lisp:135`, 環境変数 `SWIMMY_BACKTEST_CSV_OVERRIDE`）が設定されていれば**全銘柄が同一 CSV で採点**される。
  - 正準 data_id はコメント上 `USDJPY_M1` を前提（`school-backtest-v2.lisp:50,66`）。リポジトリに `*_M1.csv` は未コミット（ops box 側）。
- 創業株には EURUSD/GBPUSD が存在する（`school-founders-dalio.lisp:32,47`, `school-founders-hunted.lisp:172,183`）にもかかわらず淘汰で消えている。
- **🔎推測**: override が USDJPY を指す、または USDJPY_M1.csv しか存在しないため、EURUSD/GBPUSD ラベルの株も USDJPY 価格で採点され、真の銘柄エッジを発現できず生 sharpe 競争で負ける。**要確認**: 本番 `SWIMMY_BACKTEST_CSV_OVERRIDE` の値と `data/historical/` の実在ファイル。
- ※ この点が仮に否でも、(1)〜(4) だけで収束は成立する。銘柄採点が本物でも、変異しない銘柄遺伝子＋多様性無報酬なら吸収状態は変わらない。

### A-2. なぜ少数サンプルの過剰最適化が淘汰を生き延びるのか

#### (1) ハンディキャップは実装済み、しかし生成/淘汰ループが読まない【確定】
存在するペナルティ:
- Rust `calculate_penalized_sharpe(sharpe, trades)`：Sharpe>5 で50%、>3で20% haircut ＋ <50trade で最大30% shrink（`guardian/src/backtester.rs:180-199`）。結果は `adjusted_sharpe` として返る（`:1041`）。
- CI 下限 `sharpe_ci_lower`（bootstrap 5%タイル, `backtester.rs:1042`）。
- Lisp `evidence-adjusted-sharpe`＝`sharpe × trades/(trades+k)` 型 shrink（`school-rank-system.lisp:225-237`）。

しかし**選択・淘汰の比較子は全て生 `strategy-sharpe`**:
| 箇所 | file:line | 使う指標 |
|---|---|---|
| 親トーナメント | `school-genome.lisp:210` | 生 sharpe |
| プール溢れ淘汰 | `school-breeder.lisp:1400-1406` | `score-from-metrics` を **`:trades` 無しで呼ぶ**→ evidence shrink が effゼロ |
| 親子デスマッチ | `school-breeder.lisp:1577-1601` | 生 sharpe |
| Stagnant/Weak 淘汰 | `school-breeder.lisp:1651-1669` | 生 sharpe 閾値 |
| Wisdom elite 抽出 | `school-breeder.lisp:1552, 1521-1525` | 生 sharpe > 0.1 |

- `strategy-sharpe` は backtest 受信時に**生値**が入る（`message-dispatcher.lisp:530`）。`adjusted_sharpe`/`sharpe_ci_lower` は別スロット `:adjusted-sharpe`/`:sharpe-ci-lower` に格納されるだけ（`:531-532,553-554`）で、上記ループはどれも参照しない。
- → **haircut は計算されてから捨てられている**。ゲート表示専用の飾りで、選択圧になっていない。35trade/Sharpe17 が毎回トーナメントと溢れ淘汰を勝ち抜く。

#### (2) evidence 経路すら「罰さず・報酬もしない」で飽和【確定】
- `score-from-metrics` は `:trades` 有り時のみ shrink（`school-rank-system.lisp:244-247`）だが、`(%norm adj-sharpe 0.0 2.0)` で **Sharpe を 2.0 で頭打ち正規化**（`:249`）。PF も `(%norm pf 1.0 2.0)`（`:250`）で 2.0 cap。
- → Sharpe17/PF9.8 の過剰最適化は n-sharpe=1.0, n-pf=1.0 に飽和し、**本物の Sharpe2/PF1.5 と culling-score 上ほぼ同点**になるだけ。過剰最適化を*相対的に下げる*負の圧が無い。
- しかも溢れ淘汰は `:trades` 無し呼び（前項）＝ shrink すら効かず、生 Sharpe17 が上位で保護される。

#### (3) 親資格フロアは 30trade で、35 は素通り【確定】
- `*breeder-min-parent-trades*`=30（`school-rank-system.lisp:47`）。35>30 で親資格を得る。かつこれは*親資格*の門であって*生存ランク*の圧ではない。
- 外部 gate の `min_trades=200`（`honest_gate.py`、autopsy §1）と**二重基準**。生成側は 30、判定側は 200。生成は 35trade 過剰最適化を「勝者」として増殖させ続ける。

**A-2 総括**: 過剰最適化は「たまたま生き残る」のではなく、**生 sharpe を読む5つの比較子によって能動的に選好される attractor**。既存の penalized/shrink/CI は配線されていないため無力。

---

## B. 再設計（根本的作り直し）

設計原則: **「多様性は構造で強制、過剰最適化はループ内で罰する。単一の適応度アクセサに一本化する。」**

### B-1. データモデル変更

```
;; 遺伝子に銘柄・regime を第一級で持たせ、niche を定義する
genome := (:symbol   SYMBOL          ; 変異可能な遺伝子に昇格（現状 breed で親1固定）
           :regime   REGIME          ; trend/reversion/breakout/scalp（同上）
           :timeframe TF
           :indicators (...) :entry (...) :exit (...)
           :sl X :tp Y :volume V)

;; niche = 多様性を測る座標。クォータ・fitness-sharing の単位。
niche(g) := (list (:symbol g) (:regime g) (tf-bucket (:timeframe g)))

;; behavioral signature = 遺伝子が違っても「同じ賭け」を検知する行動指紋
behavior(s) := 正規化トレード収益ベクトル（またはエクイティ曲線の月次リターン）
             ← Guardian が既に trade_list / equity_curve を返す（backtester.rs:1037,1043）
```

戦略 struct に `selection-fitness`（キャッシュ）と `behavior-vector` スロットを追加。`strategy-sharpe`（生値）は**表示専用に降格**し、選択系は一切読まない。

### B-2. 適応度式（ループ内・単一の真実）

```
edge(s)      = min( adjusted_sharpe(s),           ; Rust haircut（backtester.rs:180）
                    sharpe_ci_lower(s) )           ; bootstrap 5%タイル（同:1042）
                × evidence_confidence(trades)       ; 追加 shrink（rank-system:225）

base(s)      = w_edge · norm(edge, 0, 1.5)          ; ← cap を 2.0→1.5 に下げ過剰最適化を飽和させない
             + w_cpcv · cpcv_pass_rate(s)
             − w_dd   · norm(maxdd, 0, 0.20)

;; ハード・フロア（選択専用。学習用にレコードは残す）
IF trades(s) < N_floor(=200 に統一)  →  selection_fitness := −∞   ; 生成・淘汰から除外
IF edge(s)   ≤ 0                      →  selection_fitness := −∞

;; fitness sharing（同一 niche の密集を割り算で罰する）
share(s)     = 1 + Σ_{t≠s} kernel( distance(s,t) )   ; distance は遺伝的×行動の合成（B-4）
selection_fitness(s) = base(s) / share(s)            ; ← ここが「61 clone 不能」の構造装置
```

- **過剰最適化の生成段階排除**: `edge = min(haircut, CI下限)` で少数サンプルの幸運（広い CI）は CI 下限が潰す。`N_floor=200` を*生成側にも*効かせ、内部30/外部200 の二重基準を撤廃。norm cap を 1.5 にして「Sharpe17 も Sharpe2 も同点」の飽和を止め、CI 下限経由で 17 は 2 未満へ沈む。
- **多様性の構造的強制**: fitness sharing により、USDJPY-TREND niche が 61 個いれば 61 番目の分母が ~61 になり選択されない。novelty pressure を別項で足すのではなく**除算で強制**するのが要点（重み調整で消せない）。

### B-3. 生成ループの構造変更（`run-breeding-cycle` の作り直し）

1. **niche クォータ**: `dolist (cat categories)` を `dolist (niche active-niches)` に置換。各 niche に *最小占有目標* と *上限* を課す。空/薄い niche には**探索ボーナス**（未踏 niche から強制サンプリング）。
2. **銘柄・regime 変異オペレータを追加**（現状ゼロ）: `breed-strategies` に低頻度の `mutate-symbol` / `mutate-regime` を追加（timeframe 変異 `select-breeder-child-timeframe:386` が既にあるので同じ枠組み）。これが吸収状態を破る唯一の脱出路。
3. **プール上限を (symbol×regime) 単位に**: `cull-pool-overflow`（`school-breeder.lisp:1360-1439`）は現状 category 単位。niche 単位に変え、1 niche が 61 個を占められないようにする。溢れ淘汰の sort を `score-from-metrics`（:trades 無し, `:1402`）から `selection_fitness` に差し替え。
4. **cross-niche 交配の復活**: デッドコードの `select-category-pair`（`school-genome.lisp:212`）を niche 版に一般化し、主経路に接続。hybrid vigor を実際に効かせる。

### B-4. 距離関数の作り直し（`calculate-genetic-distance`）

- 距離に **symbol（不一致=大）・timeframe・entry/exit 木の編集距離** を追加（現状は indicators/category/sl/tp のみ, `school-genome.lisp:18-51`）。
- **行動距離**を主指標に格上げ: `1 − |corr(behavior(a), behavior(b))|`。遺伝子が違っても収益が同一なら clone と判定。fitness sharing の kernel はこの合成距離を使う。
- 「sweet spot 0.2–0.8」を*行動距離*で実際に強制（`strategies-correlation-ok-p:1506` の 0.02 フロアを置換）。

### B-5. 生 sharpe 比較子の一掃（5箇所）
以下を全て `selection-fitness` アクセサ1本に置換:
- `school-genome.lisp:210`（トーナメント）
- `school-breeder.lisp:1400-1406`（溢れ淘汰）
- `school-breeder.lisp:1577-1601`（親子デスマッチ）
- `school-breeder.lisp:1651-1669`（Stagnant/Weak 淘汰）
- `school-breeder.lisp:1552`（Wisdom elite）

### B-6. walk-forward / purged CPCV を淘汰の手前に（是非→**是**）
- 現状フロー: `phase-1(IS backtest)` → `phase-4-purge(純IS淘汰)` → `phase-6-breeding`（`school-connector.lisp:290-314`）。**IS だけで淘汰して育種プールを作る** → IS 過剰最適化が種になる。
- **提案**: phase-1 と phase-4 の間に *軽量 purged-CPCV ゲート* を挿入。CPCV 通過（pass_rate 下限）を**育種プール入りの条件**にする。既存 `guardian/src/cpcv.rs`（López de Prado 実装）を流用。これは KILL_CRITERIA が言う「検証ループを閉じる」を生成段階で実現する。
- コスト: 全 PROVISIONAL に CPCV を回す容量が要る（P3 と同じ制約）。段階導入（まず新規子のみ→既存 PROVISIONAL 61件）。

### B-7. P1–P7 との噛み合わせ（矛盾チェック）
| 本再設計 | P1–P7 | 関係 |
|---|---|---|
| B-6 CPCV を淘汰前に | **P1**（検証結果の配線修復）＋**P3**（検証母集団拡大 PROVISIONAL 全件） | **一致・前提**。P1 で結果が書き戻らねば selection_fitness が CPCV を読めない。P1→本再設計の順。 |
| B-2 penalized/CI をループ内 | **P2**（ランク健全化＝gate.verdict 一次化） | **整合**。ランクも selection_fitness と同じ penalized 基準に寄せる。 |
| B-1 銘柄=遺伝子・実採点 | **P3**（Python5580/Rust5556 二重系統・単一CSV疑い） | **依存**。A-1(5) が真なら銘柄多様性は P3 の CSV/エンジン統合が前提。 |
| B-3 生成ループ改造 | autopsy §3「育種を停止せよ」 | **時系列の衝突に注意**。停止期間中は実装のみ、再起動は期限実験の後（§C）。 |

矛盾は無い。**本再設計は P1+P3 が数字を出した後に載せる上物**であり、P1+P3 の代替ではない。

---

## C. KILL_CRITERIA（2026-07-31）への接続と判定

### C-1. §4 本物基準への写像
| §4 条項 | 本再設計で効く機構 |
|---|---|
| ①`trades≥200 & PF≥1.10 & penalized_sharpe≥0.3` を3件 | B-2 の `N_floor=200` ＋ `edge=min(haircut,CI下限)` をループ内強制。過剰最適化を種にしない。 |
| ②USDJPY 以外 or TREND 以外を1件（多様性の証明） | B-1 銘柄遺伝子＋B-2 fitness sharing＋B-3 niche クォータ＋銘柄/regime 変異。**61 clone を構造的に不能化**。 |
| ③CPCV pass≥0.6 & median_sharpe<2.0 | B-6 purged-CPCV を淘汰前ゲート化＋B-2 の cap=1.5 で Sharpe 飽和署名を排除。 |

構造的には**3条項すべてに直接対応する**。設計上は「§4 を満たす候補を生む見込み」はある。

### C-2. しかし——見込みの前提（正直な限界）
- 本再設計は**発生器（generator）の改良**であり、**データに存在しないエッジは生成できない**。
- 実測の base rate は絶望的: PROVISIONAL の PF 中央値 1.13 / Sharpe 中央値 0.27（楽観 1pip コスト, autopsy §1）、真 OOS 生存者は**歴史上ゼロ**（`FACTORY.md:124` "0 verified"）。
- より良い GA を*エッジの無いデータ*に回すことは、**より速く・より多様な過剰最適化を生む**だけになりうる。多様性の強制は「USDJPY-TREND の過剰最適化」を「6通貨×4regime の過剰最適化」に置き換える危険を内包する（fitness sharing は clone を防ぐが、各 niche 内の curve-fit までは防がない。防ぐのは B-2 の CI 下限と B-6 の CPCV ゲート）。

### C-3. 判定（明言）
> **本再設計は「継続する場合には必要だが、それ単独では十分でない」。かつ、KILL_CRITERIA の期限実験より前に着手してはならない。**

- **順序の固定**: autopsy §3（育種停止）→ §4 の決定的実験（61 PROVISIONAL の実コスト OOS, 往復2pip 以上）→ その数字を見てから本再設計。先に生成器を作り込むのは KILL_CRITERIA §5「また配管修復・生成・インフラ磨きに逃げた」に**該当する行為**。
- **期限実験が §4 を1件も出さなかった場合** → 本再設計を実装せず、**(C) 生成エンジンごと畳むのが正しい**。エッジの無いデータに対する高機能 GA は過剰最適化の高速化にすぎない。これは明言してよい結論。
- **期限実験が §4 を≥1件出した場合** → その時**初めて**本再設計が正当化される。理由: 1件→3件へ、単一 niche→複数 niche へスケールさせる局面では、多様性強制と過剰最適化のループ内排除（B-2/B-3/B-6）が**律速**になるから。ここで本設計は決定的な価値を持つ。

**要するに**: 作る価値のある設計だが、**作る資格は期限実験の結果が与える**。順序を守れば整合、破れば KILL_CRITERIA 違反。

---

## Appendix. 確定/推測の区別
- **確定（コード直読）**: A-1(1)(2)(3)(4), A-2(1)(2)(3), B の全変更対象 file:line。
- **🔎推測（要確認）**: A-1(5) 単一CSV採点（`SWIMMY_BACKTEST_CSV_OVERRIDE` の本番値・`data/historical/` 実在ファイルの確認が必要）。
- **未検証**: fitness sharing / CPCV 前段ゲートを載せた後の実効スループット（現ハード容量, P3/P4 と連動）。
</content>
</invoke>
