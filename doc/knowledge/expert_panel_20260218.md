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
