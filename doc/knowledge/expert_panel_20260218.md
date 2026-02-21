# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-18  
**Leader:** Elon Musk  
**Mode:** critique  
**Trigger:** 「`5308e3e` の実装で問題なし？」

## 結論（先出し）
- **「問題なし」ではない。**
- 実装は方向性として正しい（legacy suffix吸収・テスト追加）が、**運用負荷と整合性の2点で未解決リスク**が残る。

---

## 🏛️ 常設顧問の意見

### Taleb
- 取引証拠件数が昇格判定に直結する設計で、`record-backtest-trades` が名前正規化を黙って適用するのは、**同名衝突時にサイレント破壊**になり得る。  
  参照: `src/lisp/school/school-db.lisp:653`, `src/lisp/school/school-db.lisp:667`, `src/lisp/school/school-db.lisp:695`
- ただし、現DB実測（2026-02-18）では `backtest_trade_logs` の suffix名残存は `0/93541` 行で、即時破滅リスクは低い。

### Graham
- 価値は「legacy吸収」であり、そこは実現できている。  
  参照: `src/lisp/school/school-db.lisp:754`, `src/lisp/school/school-db.lisp:738`, `src/lisp/tests/backtest-db-tests.lisp:949`, `src/lisp/tests/backtest-db-tests.lisp:979`
- ただし `Top Candidates` は依然として `strategy_name` 完全一致 JOIN。**表示層だけ古い定義**のままで、運用者判断を誤らせる。  
  参照: `src/lisp/school/school-narrative.lisp:223`, `src/lisp/school/school-narrative.lisp:232`

### Naval
- `strategy-trade-evidence-count` でDBを都度参照する実装は、戦略数拡大時に**レバレッジではなく負債**になる。  
  参照: `src/lisp/school/school-rank-system.lisp:225`, `src/lisp/school/school-rank-system.lisp:233`, `src/lisp/school/school-rank-system.lisp:897`, `src/lisp/school/school-rank-system.lisp:907`
- インデックス追加は良いが（`idx_backtest_trade_strategy*`）、N+1問題自体は消えていない。  
  参照: `src/lisp/school/school-db.lisp:343`, `src/lisp/school/school-db.lisp:346`

### Jim Simons
- 統計処理の一貫性が部分的。ランク判定は alias合算に寄ったが、レポート集計が非対称。**同一データに対し意思決定関数が複数ある状態**。  
  参照: `src/lisp/school/school-rank-system.lisp:225`, `src/lisp/school/school-narrative.lisp:220`
- 定量的には「判定の一貫性 > 新しい閾値」。いまは一貫性が未完成。

---

## 💻 技術パネルの意見

### Fowler
- `message-dispatcher` 側ですでに `name` 正規化済みで `record-backtest-trades` を呼んでおり、DB層でも再正規化している。**責務重複**。  
  参照: `src/lisp/core/message-dispatcher.lisp:551`, `src/lisp/core/message-dispatcher.lisp:556`, `src/lisp/school/school-db.lisp:695`
- 正規化責務は1箇所に寄せるべき（dispatcher or DBのどちらか）。

### Hickey
- `ignore-errors` でDB失敗を握り潰すため、証拠件数の劣化が静かに起こる。  
  参照: `src/lisp/school/school-rank-system.lisp:237`
- 「SimpleではなくConvenient」。最低限ログ計測を入れて障害可視化が必要。

### Uncle Bob
- テストは良い追加だが、**性能回帰テストと表示整合テストがない**。  
  参照: `src/lisp/tests/backtest-db-tests.lisp:949`, `src/lisp/tests/backtest-db-tests.lisp:979`
- 追加すべきテスト:
  - rank評価1サイクルでのDB query数上限
  - `build-top-candidates-snippet-from-db` が alias合算と一致すること

---

## 🚀 ビジョナリーパネルの意見

### Ng
- 今回の実装は「データ救済」として妥当。次段階は**evidence feature store化**（事前集計）で推論コストを定数化すべき。

### López de Prado
- サンプル数を増やすロジックで重複計上は致命的。現DBで重複は未観測でも、将来の再送・DLQ再実行に備えた**一意制約設計**が必要。  
  参照: `src/lisp/school/school-db.lisp:263`（`backtest_trade_logs` 定義に unique制約なし）

### Gene Kim
- 可観測性不足。`count-backtest-trades-for-strategy` の fallback発生率、DB失敗率、rank評価時間をメトリクス化すべき。  
  参照: `src/lisp/school/school-rank-system.lisp:225`

---

## 🚀 Musk's Decision (Final)
> 「この実装は“方向は正しいが完成ではない”。本番で使ってよいが、現状は暫定版。  
> 次にやるべきは、判定・表示・運用コストの一貫化だ。  
> つまり、N+1排除、集計定義統一、失敗可視化。この3つをやる。逆に、ここを飛ばして閾値いじりはやらない。」

## Actionable Items
1. **N+1解消**: rank評価前に `strategy_name -> composite_trade_count` を一括取得してキャッシュ利用に変更。`strategy-trade-evidence-count` から都度SQLを外す。  
   参照: `src/lisp/school/school-rank-system.lisp:225`
2. **表示と判定の定義統一**: `build-top-candidates-snippet-from-db` の `composite_trades` を alias合算ロジックに置き換える。  
   参照: `src/lisp/school/school-narrative.lisp:223`
3. **正規化責務の単一化**: suffix正規化は dispatcher か DB のどちらか一方に寄せる（重複責務を廃止）。  
   参照: `src/lisp/core/message-dispatcher.lisp:556`, `src/lisp/school/school-db.lisp:695`
4. **障害可視化**: `ignore-errors` fallback時に telemetry/event を記録し、evidence件数の劣化を監視対象に追加。  
   参照: `src/lisp/school/school-rank-system.lisp:237`
5. **重複耐性の強化**: `backtest_trade_logs` に再送耐性のユニーク戦略（例: `request_id+timestamp+strategy_name`）を導入し、重複時は upsert/ignore。  
   参照: `src/lisp/school/school-db.lisp:263`

---

# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-18  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「これ逆じゃない？tfが正でしょ？意味カテゴリは昔の実装で今は形骸化してるはず。」  
**Purpose:** `strategy-category` の正本定義（TFキー vs 意味カテゴリ）を再決定し、トレード停止リスクを減らす。  
**Constraints:** 本番稼働中。既存DB/Library戦略を破壊しない。`Scanning 0/X` を再発させない。  
**Success Criteria:** カテゴリ定義が1つに定まり、プール選抜・レジーム選抜・進化系で同時に整合する。  
**Assumptions:** Owner’s Vision は TF-Bucket×Direction×Symbol をカテゴリ正本としている。  
**Uncertainties:** 現在のDB内 `strategy-category` 実データ分布（semantic/TF混在率）を未計測。

## 現状診断（要点）
- 仕様正本は TF キー寄り。  
  参照: `doc/knowledge/implementation_plan_v50.6.md:322`, `doc/knowledge/implementation_plan_v50.6.md:335`
- 実装は二重化して衝突。  
  TFキー生成: `src/lisp/school/school-strategy.lisp:197`, `src/lisp/school/school-strategy.lisp:208`, `src/lisp/school/school-strategy.lisp:220`  
  semantic前提選抜: `src/lisp/school/school-strategy.lisp:283`, `src/lisp/school/school-strategy.lisp:305`, `src/lisp/school/school-evaluation.lisp:478`  
  semantic前提の生態系: `src/lisp/school/school-ecosystem.lisp:33`, `src/lisp/school/school-ecosystem.lisp:113`
- 直近変更は semantic 側へ寄せており、Vision と逆方向。  
  参照: `src/lisp/school/school-strategy.lisp:154`, `src/lisp/school/school-strategy.lisp:186`, `src/lisp/school.lisp:59`, `src/lisp/school/school-kb.lisp:325`, `src/lisp/strategies/strategies.lisp:74`

## 🏛️ 常設顧問の意見

### Taleb
- 単一スロットに「市場マイクロ構造軸（TF/Dir/Symbol）」と「戦術意味軸（trend/reversion）」を混在させるのは、静かな破局の温床。  
  参照: `src/lisp/dsl.lisp:169`, `src/lisp/school/school-strategy.lisp:208`, `src/lisp/school/school-evaluation.lisp:494`
- **選択肢**
  1. TF正本の単一化（semanticは都度推論）: シンプルだが推論ゆらぎが増える。
  2. 二軸分離（TF正本 + semanticタグ別スロット）: 破局回避に最も強い。
  3. semantic正本へ回帰: Vision逸脱、再び0件選抜リスク。

### Graham
- 事業価値は「トレード継続」。現在はドメイン語彙が壊れている。`category` という名前で2概念を運ぶのをやめるべき。  
  参照: `src/lisp/school/school-strategy.lisp:208`, `src/lisp/school/school-strategy.lisp:289`
- 推奨は Option 2。名前を分ければレビュー・オンボーディング・障害解析が速くなる。

### Naval
- レバレッジ視点では「推論を毎回行う設計」は負債。タグは計算して保存し、実行時は読むだけにする。  
  参照: `src/lisp/school/school-evaluation.lisp:476`, `src/lisp/school/school-learning.lisp:290`
- 反証: 「保存タグは陳腐化する」→ だから再計算ジョブを定期実行すればよい。

### Jim Simons
- 統計軸（TF/Dir/Symbol）と戦術軸（regime適合）は別確率空間。1変数へ射影するのは情報落ち。  
  参照: `src/lisp/school/school-evolution-orchestration.lisp:35`, `src/lisp/school/school-breeder.lisp:704`
- Option 2 が最も情報保存量が高い。

## 💻 技術パネルの意見

### Fowler
- これはモデリング不一致。`strategy-category` を domain term のまま多義化したのが原因。  
  参照: `src/lisp/dsl.lisp:162`, `src/lisp/dsl.lisp:168`, `src/lisp/school/school-strategy.lisp:154`
- `strategy-scope-key`（TF/Dir/Symbol）と `strategy-regime-class`（semantic）に分離すべき。

### Hickey
- 「Simple」ではなく「Complected」。1つのデータに2意味を絡めた結果、関数境界で if/fallback が増殖している。  
  参照: `src/lisp/school/school-evaluation.lisp:494`, `src/lisp/school/school-strategy.lisp:177`
- データモデルを先に直せ。ロジックで救済するな。

### Uncle Bob
- 現在の新規テストは“移行動作”は守るが、“設計契約”を守っていない。  
  参照: `src/lisp/tests.lisp:9738`, `src/lisp/tests.lisp:9758`
- 追加必須テスト:
  1. scope-key/semantic-tag の両方が必須である契約テスト
  2. `build-category-pools` と `assemble-team` のキー空間一致テスト
  3. `collect-strategy-signals` で `Scanning 0/X` を再現防止する回帰

## 🚀 ビジョナリーパネルの意見

### Ng
- 推論タグは ML feature として有効だが、推論結果を主キーにしてはいけない。主キーは観測可能な market microstructure に置くべき。  
  参照: `src/lisp/school/school-strategy.lisp:197`, `src/lisp/school/school-strategy.lisp:289`

### López de Prado
- レジーム選択とカテゴリ淘汰を同一キーで扱うと、分散投資が形だけになる。TF/Dir/Symbol 分散は保持し続ける必要がある。  
  参照: `doc/knowledge/implementation_plan_v50.6.md:320`, `src/lisp/school/school-breeder.lisp:996`

### Gene Kim
- まず可観測化。`Scanning 0/X`、pool key cardinality、semantic-tag欠損率をメトリクス化しないと議論が宗教化する。  
  参照: `src/lisp/school/school-evaluation.lisp:515`, `src/lisp/school/school-evolution-orchestration.lisp:35`

## 🚀 Musk's Decision (Final)
> 「君の指摘は正しい。TFが正本だ。だが“TFだけ”でも足りない。  
> やるべきことは、TF/Dir/Symbolを正本キーに据えた上で、レジーム意味タグを別軸として持つこと。  
> 逆に、`strategy-category` をどちらか片方に寄せるパッチはもうやらない。  
> データモデルを二軸化し、選抜系を段階的に差し替える。」

## Actionable Items
1. `strategy` 構造体を二軸化する。  
`strategy-scope-key`（list: TF-bucket/dir/symbol）と `strategy-regime-class`（keyword）を追加し、`strategy-category` は互換レイヤーへ縮退。  
対象: `src/lisp/dsl.lisp:169`
2. プールを分離する。  
`*category-pools*`（scope用）と `*regime-pools*`（semantic用）を分離し、`assemble-team` / `select-strategies-for-regime` の参照先を統一。  
対象: `src/lisp/school/school-strategy.lisp:220`, `src/lisp/school/school-strategy.lisp:283`, `src/lisp/school/school-evaluation.lisp:476`
3. 直近の semantic 側移行パッチをロールバックし、暫定的には「scope正本 + semanticは明示推論関数のみ」に戻す。  
対象: `src/lisp/school/school-strategy.lisp:154`, `src/lisp/school.lisp:59`, `src/lisp/school/school-kb.lisp:325`, `src/lisp/strategies/strategies.lisp:74`
4. 観測を追加する。  
`pool_scope_cardinality`, `pool_regime_cardinality`, `regime_scan_candidates`, `semantic_tag_missing_rate` を telemetry 出力。  
対象: `src/lisp/school/school-evaluation.lisp:515`
5. 契約テストを追加する。  
「キー空間不一致でfail」「両軸があると選抜が0にならない」をCI必須にする。  
対象: `src/lisp/tests.lisp:9738`, `src/lisp/tests.lisp:9758`
