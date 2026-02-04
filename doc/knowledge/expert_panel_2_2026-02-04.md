# Expert Panel 2 (Reliability & Resilience) — 2026-02-04

## Scope
Discord通知パイプラインの信頼性・可用性・耐障害性レビュー。
対象: `src/lisp/core/discord.lisp`, `tools/notifier.py`, `src/lisp/shell/notifications.lisp`, `src/python/discord_bot.py`, `src/python/sexp_utils.py`, `tools/report_system_status.py`, `tools/report_backtest_summary.py`

---

## System Deep Analysis (Observations)

1. **ZMQ送信の安全性**
   - `src/lisp/core/discord.lisp` の `queue-discord-notification` は `pzmq:send` を同期実行。Notifier停止/遅延時に送信がブロックし、トレーディングループへ影響する可能性がある。
   - 送信成功判定は「ZMQへ送信できたか」で、Discord到達は担保されない。`*last-zmq-success-time*` が実配達に紐付かず、**誤った健全性**を作る。

2. **Notifierの可用性と永続性**
   - `tools/notifier.py` は **単一プロセス + 非永続キュー**（`deque`）。プロセス再起動やクラッシュで未送信メッセージは消失。
   - キューが**無制限**で、Discord障害やWebhook障害時にメモリ膨張の危険。
   - 429時は再送するが、**遅延・滞留の可視化がない**。

3. **更新時刻の整合性**
   - `src/lisp/shell/notifications.lisp` は `.opus/live_status.sexp` を60秒間隔で原子的に保存。
   - `src/python/discord_bot.py` は `Updated: {datetime.now()}` を表示するため、**ファイルが古くても最新に見える**。
   - `live_status.sexp` に `last_updated` があるが未使用。**ステール検知欠如**。

4. **統合ソースのズレ**
   - `live_status.sexp` / `system_metrics.sexp` / `backtest_cache.sexp` の更新周期が異なるため、**合成表示にタイムスキューが混入**する。
   - 現状は「どの時刻の値か」が通知に出ないため、誤判断を招く。

5. **エラーモードの見える化不足**
   - Webhook 4xx/5xx は `notifier.py` の標準出力にのみ残る。**監視ダッシュボードやDiscord通知に反映されない**。
   - 失敗件数・ドロップ件数・キュー長など**信頼性指標が存在しない**。

6. **S式パース耐性**
   - `src/python/sexp_utils.py` は最低限のパーサで、破損時は `SexpParseError`。
   - `discord_bot.py` は例外時に古いキャッシュを維持するが、**ユーザーにステールを示さない**。

---

## Extreme Feedback (Murphy Scenarios)

- **Notifierが停止**: ZMQ送信が詰まり、Lisp側がブロックする可能性。通知は「送れた風」に見える。
- **Discordが障害/429連発**: メッセージが大量に遅延し、キューが肥大化。復旧後に古い通知が雪崩。
- **ディスク満杯**: `live_status.sexp` 保存が失敗しても、Discordは“今の時刻”で更新済に見える。
- **Webhook誤設定/期限切れ(404)**: `notifier.py` のログのみ。運用者が気づかず通知機能が長時間停止。
- **プロセス再起動**: 未送信通知が全損。復旧後に状態が復元できない。

---

## Panel Feedback

### 🏗️ Werner Vogels (Infrastructure)
- 「全部が壊れる前提で設計せよ」。ZMQ送信は**非同期化 / バックプレッシャー制御**が必要。
- **HWM（High Water Mark）**設定とドロップ時の計測が不可欠。

### 🛡️ Joe Armstrong (Fault Tolerance)
- **“Let it crash”** の前に、クラッシュしても失われないキューが必要。
- `notifier.py` が単一プロセス依存で**SPOF**。最低でもディスクスプールや永続化が必要。

### 🚀 Margaret Hamilton (Safety)
- 失敗の**見える化**が不足。ステールデータや通知失敗は**ユーザーに明示**すべき。
- 「Updated時刻が嘘」は**安全性リスク**。`last_updated` を正として表示すべき。

### 📊 W. Edwards Deming (Quality)
- 測定がないと改善不能。**キュー長、ドロップ数、リトライ回数、成功率**を可視化せよ。
- `SWIMMY_DISCORD_*` 依存の構成は**監査ログ**として残すべき。

### ⚡ Brendan Gregg (Performance)
- 遅延の見える化が無い。**通知遅延（生成→配達）**のSLAを測定すべき。
- `requests.post` が逐次実行でレイテンシが蓄積。セッション/バッチ化を検討。

---

## Action Items (Reliability First)

### P0 — すぐやるべき
1. **ステール検知**: `discord_bot.py` で `live_status.sexp` の `last_updated` を表示し、一定時間以上古ければ `STALE` を明示。
2. **ZMQ送信の非ブロック化**: `pzmq:send` を `dontwait` 対応にし、詰まり時はドロップ + カウンタ加算。
3. **Notifier健全性指標**: `notifier.py` に `queue_len / drops / errors` を出力し、`system_metrics` に反映。

### P1 — 1週間内
4. **永続キューの導入**: JSONL / SQLite へのスプールで再起動後に再送。
5. **Webhook監査**: 404/401 を検出したら **別チャネルに警告**。
6. **送信SLA可視化**: 生成時刻→配達時刻を計測し、遅延が閾値超過なら通知。

### P2 — 中期
7. **統合ステータスの時刻整合**: `live_status + system_metrics + backtest_cache` を合成する場合、各ソースの取得時刻を明示。
8. **スキーマバージョン検証**: `schema_version` 不一致時に安全なフォールバックとアラート。

---

## Summary
現在の通知経路は**「送れたかどうか分からない」**という構造的欠陥を抱えている。特に `Updated` 表示が実データの更新と無関係な点は、運用上の誤認を招く。最優先は **ステール検知** と **送信失敗の可視化**。次に **永続キュー** と **SLA計測** を入れ、Discord通知の信頼性を実運用レベルに引き上げるべき。
