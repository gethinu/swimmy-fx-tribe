# 🧭 Expert Panel 2 Report (Reliability & Resilience)

**Date:** 2026-02-02  
**Panel:** Werner Vogels, Joe Armstrong, Margaret Hamilton, W. Edwards Deming, Brendan Gregg  
**Focus:** OOS自動審査フローを含む信頼性・可用性・耐障害性の洗い出し

## パネル所見

### Werner Vogels (Scalability/Infra)
- OOS要求の状態管理がメモリ依存 (`*oos-pending*` in `src/lisp/school/school-validation.lisp:18-33`)。プロセス再起動でペンディング情報が蒸発し、バックテストサービスへの送信・重複送信が乱れる。永続ストアへキューを移し、idempotentな request-id を付与すべき。
- `request-backtest` 呼び出し（`school-validation.lisp:24-33`）にタイムアウト・リトライ・バックプレッシャーが無い。ZMQ接続一時断で静かに落ちる。送信フェーズに再試行と指数バックオフを入れ、送信失敗をメトリクス化せよ。

### Joe Armstrong (Fault Tolerance)
- `src/lisp/core/message-dispatcher.lisp:230-309` は `handler-case` で例外を潰すが、失敗時に再処理キューを持たず、失敗した BACKTEST_RESULT が消える。「Let it crash」の前に、隔離されたワーカー+デッドレターキューを用意せよ。
- OOS結果の処理 (`school-validation.lisp:35-48`) で DB書き込み失敗時のリカバリが無い。`upsert-strategy` 失敗時にリトライ/隔離行きのパスを持つべき。

### Margaret Hamilton (Safety)
- 入力データ検証が甘い。`run-oos-validation` は `probe-file` だけを見ており、空ファイルや欠損ヘッダを許容する (`school-validation.lisp:53-69`)。最低限、ヘッダ検証・行数>0チェック・数値パース失敗の早期アボートを行い、「想定外入力」を体系的に捕捉せよ。
- `school-db.lisp:133-161` の INSERT OR REPLACE はジャーナル/同期設定が不明で、電源断時の部分書き込みが起き得る。OOSやランク更新はトランザクションで囲み、`PRAGMA journal_mode=WAL` と `synchronous=NORMAL` など明示設定を推奨。

### W. Edwards Deming (Process/Quality)
- OOSフローの運用指標がゼロ。送信件数、キュー滞留時間、結果成功率、再送回数といったKPIが無く、改善サイクルを回せない。`school-validation.lisp` と `message-dispatcher.lisp` に計測ポイントを入れ、Prometheus/Discord へ定期レポートせよ。
- 定期評価ループ (`school-rank-system.lisp:243-269, 275-314`) で正常系ログのみ。失敗パス（送信失敗、データ欠損、DBエラー）を統計として蓄積し、日次で品質レビューするプロセスを持つべき。

### Brendan Gregg (Observability/Performance)
- ZMQ経路のレイテンシ計測が無い。`send-zmq-msg` から `handle-oos-backtest-result` までの end-to-end 分布が取れず、輻輳/スパイクを検知できない。トレースID付与とタイムスタンプログを入れ、p99 を監視せよ。
- `refresh-strategy-metrics-from-db` (`school-db.lisp:183-209`) は60秒毎に全件走査し、巨大KBでスパイクを作る。インクリメンタル更新か、変更検知型のpullに置き換えると負荷が安定する。

## Actionable Items (優先度順)
1. **OOSキューの永続化と再送**: `*oos-pending*` をDBテーブルに置き換え、request-id付きで送信・ACKまでトランザクション化。再起動後も再送/重複抑止ができるようにする。
2. **データ品質ゲート**: `run-oos-validation` 前にCSVヘッダ/行数/数値妥当性を検証し、失敗をDiscordへ即時通知するガードを追加。
3. **E2Eメトリクスとトレース**: OOS送信→結果受信までのレイテンシと成功率を計測し、p50/p95/p99 をダッシュボード表示。送信失敗とデッドレターキュー件数も公開。
4. **メッセージ処理のデッドレター化**: `message-dispatcher.lisp` の BACKTEST_RESULT 処理に失敗時リトライ＋デッドレターキューを導入。例外を潰さずに隔離・再処理できる構造に。
5. **DBの安全設定**: SQLiteにWAL+journal/synchronous設定を明示し、OOS/ランク更新はトランザクションでバッチ化。部分書き込みを防ぐ。
6. **評価ループの失敗統計化**: `run-rank-evaluation` で OOS送信失敗・データ欠損・DBエラーをカウントし、日次サマリを出す。Demingサイクルを回せる形にする。
7. **負荷平準化**: `refresh-strategy-metrics-from-db` を差分更新orバッチサイズ制御に変更し、KB拡大時のGC・IOスパイクを抑制。

