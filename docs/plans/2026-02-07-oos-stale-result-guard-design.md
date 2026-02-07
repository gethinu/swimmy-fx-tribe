# OOS Stale Result Guard Design

**Goal:** OOS再試行時に古いrequest_idの結果が最新のOOS結果と衝突しないようにし、再試行は必ず新しいrequest_idを使う。

## Architecture
- OOSリクエストは`oos_queue`に最新のrequest_idのみ保持する。
- 結果受信は`handle-oos-backtest-result`で`lookup-oos-request`の最新request_idと照合する。
- 一致しない結果はstaleとして無視し、状態は更新しない。

## Data Flow
1. `maybe-request-oos-backtest`が新しいrequest_idを生成してenqueue。
2. RustからOOS結果を受信。
3. `handle-oos-backtest-result`で最新request_idと一致確認。
4. 一致時のみ`complete-oos-request`と`strategy-oos-sharpe`更新。
5. 不一致時はstaleログ出力のみ。

## Error Handling
- request_id欠落や一致なしはstale扱いで無視。
- staleは失敗カウンタに含めない（監視ログのみ）。

## Testing
- stale結果が到着した場合に`oos_sharpe`が更新されないこと。
- stale結果で`oos_queue`がクリアされないこと。
- リトライ時に新しいrequest_idが生成されること（既存テスト維持）。
