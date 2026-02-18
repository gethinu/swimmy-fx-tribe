# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-17  
**Leader:** Elon Musk  
**Mode:** critique  
**Trigger:** 「トレード数35って信用に値するか？」

## 結論（先出し）
- **現状の「35トレード高Sharpe」は、単独では信用に値しない。**
- 理由は「サンプル数の少なさ」だけでなく、**Stage2証拠（DryRun/MC運用証跡）と表示・運用の整合が崩れている**ため。

---

## 🏛️ 常設顧問の意見

### Taleb
- 破滅回避の観点では、`trades=35` は「推定誤差に極端に弱い領域」。  
- 実データ上、上位ランク（B/A/S）1866件のうち **1699件が30-39トレード**で、密集しすぎている（2026-02-17時点DB集計）。
- Sharpe計算で0リターン日を含める修正は正しいが（`guardian/src/backtester.rs:229`）、それでも**少数取引での過信は解決しない**。

### Graham
- いまの成長阻害はアルゴ探索力ではなく、**評価契約の不整合**。  
- レポート文言はA/Sに「+MC/DryRun」と書くが（`data/reports/evolution_factory_report.txt:9`, `data/reports/evolution_factory_report.txt:12`）、実際のAゲート設定は `*a-rank-require-dryrun* nil` / `*a-rank-require-mc* nil`（`src/lisp/school/school-validation.lisp:64`, `src/lisp/school/school-validation.lisp:67`）。

### Naval
- 「自動化」は**ルールの実施証明**まで自動化して初めてレバレッジになる。  
- `dryrun_slippage_samples` が0件、`strategy_daily_pnl` も0件（2026-02-17 DB集計）で、運用証拠が欠落している状態は、スケール時に事故率を上げる。

### Jim Simons
- 定量視点では、問題はTF自由化ではなく**統計的有効性の閾値管理**。  
- 現在、S=93中、`trades=35` が36件、A=1634中、`trades=35` が1112件（2026-02-17 DB集計）。  
- `check-rank-criteria` は依然としてraw Sharpe等を直接判定（`src/lisp/school/school-rank-system.lisp:246`）。

---

## 💻 技術パネルの意見

### Fowler
- ドメインルールは良いが、**実運用での整合性テストが不足**。  
- 例: S昇格はCommon Stage2必須（`src/lisp/school/school-rank-system.lisp:795`）だが、実DBの証拠テーブルは空（2026-02-17 DB集計）。  
- これは「仕様正しい/データ不整合」か「昇格経路の抜け穴」のどちらかで、どちらでも重大。

### Hickey
- 状態が二重化して壊れている兆候あり。  
- `KB active mismatch (DB=21316 KB=2)`（`data/reports/evolution_factory_report.txt:27`）。  
- 評価状態が分裂すると、35トレード議論以前に「何を信じるか」が崩れる。

### Uncle Bob
- 「Sに上げる前にDryRun>=20」等の不変条件が、テストで守られていない。  
- `common-stage2-gates-passed-p` のDryRun分岐（`src/lisp/school/school-validation.lisp:244`）と、実DB状態（サンプル0）が矛盾するケースをCIで落とすべき。

---

## 🚀 ビジョナリーパネルの意見

### Ng
- モデル探索（任意TF）は継続すべき。ただし採用条件は**探索自由・採用厳格**に分離する。  
- 採用判定に `CI下限` と `sample-aware` 指標を主軸化し、ランキング表示も同一指標に統一が必要。

### López de Prado
- 35トレードでSharpe>10が大量発生しているなら、優先疑義は「過学習/リーケージ/評価設計」。  
- 特に `S_core_count=12335` に対し `trades 30-39 = 11727`（2026-02-17 DB集計）は、閾値設計としては危険サイン。

### Gene Kim
- これは研究問題ではなく運用問題。  
- まず監視対象を「成績」から「証拠の充足率」に変えるべき。  
- 例: `dryrun sample coverage`, `MC evidence coverage`, `S revalidation failure rate` を日次で可視化。

---

## 🚀 Musk's Decision (Final)
> 「35トレードは“候補としては有効”、だが“採用根拠としては不十分”。  
> 探索は自由に継続する。採用は証拠契約を満たしたものだけ。  
> まず評価パイプラインの一貫性を直し、既存S/Aを再審査する。」

## Actionable Items
1. **S/A再審査ジョブを即時実行**: 現行`common-stage2-gates-passed-p`基準で全S/Aを再評価し、不通過は降格（`src/lisp/school/school-rank-system.lisp:795`, `src/lisp/school/school-validation.lisp:205`）。
2. **Aゲート文言と実設定を一致**: `*a-rank-require-dryrun*` / `*a-rank-require-mc*` が `nil` の間はレポートから「+MC/DryRun」を外すか、設定を `t` に上げる（`src/lisp/school/school-validation.lisp:64`, `src/lisp/school/school-validation.lisp:67`, `data/reports/evolution_factory_report.txt:12`）。
3. **35トレードを“候補帯”へ格下げ**: 昇格判定をraw Sharpe中心から、`CI下限 + evidence-adjusted`中心へ移行（`guardian/src/backtester.rs:905`, `src/lisp/school/school-rank-system.lisp:108`）。
4. **証拠KPIを監視追加**: `dryrun_slippage_samples` 件数、`backtest_trade_logs` 由来MC算出率、`strategy_daily_pnl` 埋まり率を日次アラート化。
5. **TF探索は継続**: 8TFはデフォルト母集団として維持しつつ任意TFを探索。ただし採用は上記証拠契約を満たした戦略のみ。

---

# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-17  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「戦略のtfの関係でトレードが足りないなら回数増やすためにバックテスト期間伸ばす？それか、ｂ案の合算で良いと思う。いずれにしろ、35件じゃ少ない。」
**Purpose:** 35トレード問題に対して、`期間延長` と `証拠合算（B案）` のどちらを採るべきか意思決定する。  
**Constraints:** `A>=50 / S>=100` のTradeEvidence floorは維持（`docs/llm/SPEC.md:142`, `docs/llm/SPEC.md:143`, `src/lisp/school/school-rank-system.lisp:61`, `src/lisp/school/school-rank-system.lisp:64`）。  
**Success Criteria:** 低頻度TFでも「過大評価を増やさず」100トレード相当の証拠を確保し、昇格判断の再現性を上げる。  
**Assumptions:** 現在の35件群は主にBランクかつTF=3600（`data/memory/swimmy.db`集計）。  
**Uncertainties:** OOS実装が想定どおり分割評価されているかに疑義あり（`-OOS`/`_OOS`不一致）。

## 事実ベース（2026-02-17 / `data/memory/swimmy.db`）
- `trades=35` は `:B=17`, `:GRAVEYARD=6090`。`A/S`に35件は0。  
- `:B`かつ35件は全件 `timeframe=3600`。  
- `:B`かつ35件の `cpcv_pass_rate` は平均0.178、`>=0.70`は0件。  
- `backtest_trade_logs` は `OOS=78472`, `BACKTEST=1526` と偏在。35件B群に紐づくログは `OOS` のみ確認。  

## 🏛️ 常設顧問の意見
### Taleb
- 35件は「候補」には使えるが「採用根拠」には弱い。破滅回避の観点で、単純合算は危険。  
- 選択肢:
  - 1. 期間延長（推奨）: 同一ルールで標本数だけ増やす。歪みが小さい。  
  - 2. 合算: OOS/CPCV/BACKTESTの混合は leakage を起こしやすい。採るなら重複排除と重み付け必須。  

### Graham
- まず契約不整合を潰すべき。OOS要求が `-OOS` なのに、別経路は `_OOS` 想定（`src/lisp/school/school-validation.lisp:614`, `src/lisp/school/school-backtest.lisp:374`, `src/lisp/school/school-backtest.lisp:494`）。  
- ここが曖昧なまま「合算で100達成」は数字作りに見える。

### Naval
- 手作業判断では再発する。  
- `低頻度TF向けロングホライズン再検証` を自動ジョブ化し、100未満は自動再試験に回すべき。

### Jim Simons
- 計算上、現行デフォルトが `M1 100000本`（`src/mt5/SwimmyBridge.mq5:711`）かつ OOS比率30%（`src/lisp/school/school-validation.lisp:12`）なら、OOS窓は約20.8日。  
- 35件は約1.68件/日。100件には約59.5日OOSが必要で、総履歴は約198日（約286k本M1）相当。  
- 結論: 先に期間延長で母数を増やすのが統計的に自然。

## 💻 技術パネルの意見
### Fowler
- `guardian`側BACKTESTは suffixでOOS分割していない（`guardian/src/main.rs:1701`, `guardian/src/main.rs:1749`）。  
- まず「OOSとは何か」をコードで一意化しないと、35件議論の土台が崩れる。

### Hickey
- 命名規約の分裂（`-OOS` vs `_OOS`）は状態バグの温床。  
- OOS専用フィールド（`start_time/end_time` or range_id）で明示し、suffix依存をやめるべき。

### Uncle Bob
- 追加テストが必須。  
- 具体:
  - `OOS dispatch -> 実際にOOS窓のみ評価` を検証するテスト。  
  - `trades<100` のS昇格不可能テスト（既存は0件だが回帰防止）。

## 🚀 ビジョナリーの意見
### Ng
- 探索自由は維持。ただし採用は厳格。  
- 合算は「候補優先度スコア」まで、ランク昇格の必須件数には使わない二層設計が妥当。

### López de Prado
- 反証: 「低頻度だから合算して100にする」は最適化バイアスを増やす。  
- 推奨は `期間延長 + CPCV継続`。合算は上限付き補助指標（例:公式件数へ最大30%まで寄与）に制限。

### Gene Kim
- 運用KPI化するべき。  
- `tf別 median trades`, `>=100到達率`, `OOS/BACKTEST比率`, `同一戦略の sharpe vs oos_sharpe 差分` を日次監視に追加。

## 🚀 Musk's Decision (Final)
> 「結論はハイブリッドだ。  
> 1) 公式昇格件数は期間延長で稼ぐ。  
> 2) 合算は候補選抜の補助に限定する。  
> 3) その前にOOS実装の意味をコードで統一する。  
> 35件は“使ってよい候補”だが“信頼して賭ける本命”ではない。」

## Actionable Items
1. **OOS意味の統一を先行修正**: `-OOS/_OOS`混在を廃止し、`request-backtest`にOOSレンジを明示パラメータ化する（`src/lisp/school/school-validation.lisp:614`, `src/lisp/school/school-backtest.lisp:374`, `src/lisp/school/school-backtest.lisp:494`, `guardian/src/main.rs:1701`）。  
2. **低頻度TF専用の長期バックテスト枠を追加**: `timeframe>=3600` かつ `trades<100` は約286k M1相当まで自動延長して再計測する。  
3. **合算(B案)は補助スコア限定**: `official_trade_evidence`（昇格判定）と `composite_trade_evidence`（候補優先）を分離。昇格は既存floorを維持。  
4. **昇格ゲートの透明化**: レポートに `official/composite` を別表示して、100件到達の根拠を明示する。  
5. **回帰テスト追加**: OOS分割実体、昇格floor、合算スコアがrank判定へ侵入しないことをCIで固定する。  
