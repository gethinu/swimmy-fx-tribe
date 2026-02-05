# Aux Services S-Expression Design

**Goal:** Data Keeper / Risk Gateway / Notifier のZMQプロトコルを S-expression（alist）に統一し、外部API境界（Discord/HTTP/MCP）はJSONのまま維持する。

**Architecture:** 補助サービスへのZMQメッセージは `type` と `schema_version` を必須とする alist で表現する。Data Keeper は `type=DATA_KEEPER` + `action` で操作を識別し、結果は `type=DATA_KEEPER_RESULT` で返す。Risk Gateway は `type=CHECK_RISK` で依頼し、`type=RISK_DECISION` で応答する。Notifier は `type=NOTIFY` の一方向メッセージのみを受け付け、Discord HTTP 送信は内部でJSONに変換する。

**Data Flow:** Lisp/Tools は `encode-sexp` で S式文字列を送信し、Python側は `sexp_utils.parse_sexp_alist` で受信する。応答は Python 側が alist を生成し、S-expression として返送する。`schema_version` は現行 `1` とし、将来の拡張時に互換性判断に使う。

**Error Handling:** 解析失敗（S式パースエラー/必須フィールド欠落）はサービス側で明示的に `status=error` と `error` を返す（Notifierはログ化のみ）。JSON文字列の埋め込みは禁止し、データはS式構造として保持する。

**Testing:** Data Keeper / Risk / Notifier それぞれにS式契約テストを追加し、最低限のラウンドトリップ検証（S式送信→S式応答 or 正常な受理）を自動化する。
