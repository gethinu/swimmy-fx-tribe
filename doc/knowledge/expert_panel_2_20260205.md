# Expert Panel 2 (Reliability & Resilience) — 2026-02-05

## Scope
ペア戦略（Pair-Composite）導入における信頼性・可用性・耐障害性レビュー。対象は「Lisp内オーバーレイ」「バックテスト結果にトレード一覧を含める」「SQLiteの履歴保存」。

### 参照コンテキスト
- ZMQ送信: `src/lisp/school/school-backtest-utils.lisp:46-69`
- Backtest結果スキーマ: `docs/llm/INTERFACES.md:220-228`
- trade_logs: `src/lisp/school/school-db.lisp:60-70`
- PnL相関算出: `src/lisp/school/school-portfolio.lisp:56-66`

---

## System Deep Analysis (Observations)

1. **巨大ZMQメッセージのリスク**
   - Backtest結果に「全トレード一覧」を含めると、ZMQメッセージのサイズが肥大化し、送信のブロック/遅延/メモリ枯渇が発生しやすい。`send-zmq-msg` はサイズ上限や分割送信の概念がない（`school-backtest-utils.lisp:46-69`）。
   - Backtest Service/Guardian の一時バッファ溢れが再発する恐れ（既存の送信スロットリングは件数ベースでサイズ非考慮）。

2. **SQLite肥大とクエリ負荷**
   - `trade_logs` に `pair_id` を追加し、さらに `backtest_trade_logs` を全件保持する設計は容量増大が不可避。索引設計が無いとペア評価の計算が遅延する。
   - 既存 `trade_logs` には `strategy_name` インデックスのみ（`school-db.lisp:93-95`）。`pair_id` や `request_id` への索引が必要。

3. **永続化失敗時の挙動**
   - ディスク満杯やSQLiteロック競合時に、ペア評価に必要なPnL系列が欠落する。
   - 「N<100なら除外」ルールは**欠損が連鎖するとペア候補が枯れる**。

4. **整合性/再現性の欠如**
   - Backtest結果にトレード一覧を追加すると、仕様変更により既存の結果再利用が難しくなる。`data/backtest_cache.sexp` は現状メトリクスのみを想定（`school-backtest-utils.lisp:71-113`）。

5. **単一戦略パイプラインへの影響**
   - ペア層は独立する設計だが、評価結果のDB書き込みが単一戦略の評価タイミングと競合する可能性。

---

## Extreme Feedback (Murphy Scenarios)
- **Backtest結果が10万トレードを返す** → ZMQ送信が詰まり、Brainが受信不能。全体が停止。  
- **SQLiteがロック競合** → ペア履歴の書き込みが失敗し、再評価で候補が枯れる。  
- **ディスク満杯** → backtest_trade_logs保存失敗。救済モード多発、無意味なペア評価。  
- **不完全データ** → 100件未満で除外が連続し、ペア枠が常に空になる。  
- **Backtest Service停止** → トレード一覧が取れず、相関計算が完全停止。

---

## Panel Feedback

### 🏗️ Werner Vogels (Infrastructure)
- **批判**: 「全件返す」はシステムのスケール限界を超える。**メッセージサイズは運用リスク**。
- **提案**: トレード一覧は別ストレージにスプールし、結果には参照IDだけを返す。

### 🛡️ Joe Armstrong (Fault Tolerance)
- **批判**: 失敗時の復旧パスがない。`backtest_trade_logs` 書き込み失敗時にどう復旧するか未定。
- **提案**: 書き込みは「別プロセスで非同期」にし、失敗時はDLQへ。

### 🚀 Margaret Hamilton (Safety)
- **批判**: 「N<100なら除外」は安全設計だが、**除外ばかりでペア枠が空**になるケースの対処が未定。
- **提案**: 「安全停止（ペア枠0）」時の通知・監視指標を定義せよ。

### 📊 W. Edwards Deming (Quality)
- **批判**: 品質指標がない。ペア戦略の有効性・データ欠損率・失敗率を**測定する設計**が必要。
- **提案**: ペア評価の成功率、欠損率、平均処理時間をメトリクスに追加。

### ⚡ Brendan Gregg (Performance)
- **批判**: SQLiteへの大量INSERTがボトルネックになる。
- **提案**: バルク挿入と専用インデックス設計、ZMQ受信遅延を可視化。

---

## Action Items (Reliability First)
1. **ZMQ大型メッセージ対策**: トレード一覧は「別保存＋参照ID返却」または「分割送信」へ。
2. **DB索引追加**: `backtest_trade_logs(request_id)`, `backtest_trade_logs(strategy_name)`, `trade_logs(pair_id)` の索引を必須化。
3. **失敗時の挙動定義**: 書き込み失敗はDLQ/リトライへ。ペア候補枯渇時の通知を追加。
4. **監視メトリクス追加**: ペア評価の失敗率、欠損率、平均処理時間、DBサイズ増加速度を計測。
5. **バックテストキャッシュ整合性**: `data/backtest_cache.sexp` と新テーブルの整合ルールを設計。

---

## Summary
ペア戦略は「独立レーン」で安全設計だが、**Backtest結果に全トレードを含める**設計は可用性リスクが高い。最優先は**メッセージサイズの分離**と**DB索引・監視**。これを入れない限り、ペア戦略は本番で壊れる。
