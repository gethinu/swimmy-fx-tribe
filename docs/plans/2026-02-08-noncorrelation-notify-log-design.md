# Noncorrelation Notification Log Design

Date: 2026-02-08

## Goal
- 非相関 昇格通知 の本文を `logs/swimmy.log` に残し、Discord 依存を減らす。

## Decisions
- 変更箇所は `notify-noncorrelated-promotion` のみ。
- ログには **通知本文そのまま** を残す。
- 他通知の一元ログ化は行わない（ノイズ・機密リスクを回避）。

## Architecture
- `notify-noncorrelated-promotion` で `msg` を生成した直後に
  `swimmy.core:log-info` を呼び、本文を `swimmy.log` に書き出す。
- Discord 送信の成否に関係なく、ログは必ず残す。
- 送信経路は既存の `queue-discord-notification` を保持。

## Error Handling
- `log-info` 内部の例外は既存の処理で吸収されるため、通知本処理に影響しない。

## Testing
- `notify-noncorrelated-promotion` 実行時に `log-info` が呼ばれ、
  本文に `非相関スコア` と `データ不足: X/30日` が含まれることを確認。

## Non-Goals
- 全通知のログ化
- JSONテレメトリ形式の変更
