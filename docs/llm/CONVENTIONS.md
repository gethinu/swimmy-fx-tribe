# Coding Conventions

## 共通
- **言語**: 変数名・コメントは英語推奨（ただし日本人チーム向けに日本語コメントも許容）。
- **文字コード**: UTF-8 (BOMなし)。MT5のみ環境依存の可能性に注意。
- **改行コード**: LF (Unix style)。
- **設定**: 環境変数 (`.env`) または定数ファイルに集約。ハードコード禁止。
- **機密情報**: Webhook URL, API Keyは `.env` 必須。リポジトリ混入禁止。

## MQL5規約
- **命名**: `PascalCase` (関数), `snake_case` (変数), `g_` (グローバル)。
- **安全性**: `ZMQ_DONTWAIT` を使用し、MT5のGUIスレッドをブロックしない。
- **配列**: `ArraySetAsSeries(true)` を原則とする。
- **ログ**: `Print()` は最低限に。大量ログはパフォーマンスを落とす。

## Rust規約 (Guardian)
- **Style**: `rustfmt` 準拠。
- **Lint**: `clippy` の警告を可能な限り解消する。
- **安全性**: `unwrap()` は初期化時のみ許容。メインループ内では `Result` をハンドリングし、決してパニックさせない。
- **Concurrency**: `Mutex` ロックの粒度に注意（デッドロック防止）。
- **JSON**: `serde_json` を使用し、型安全にパースする。

## Lisp規約 (Brain)
- **Style**: 標準的なCommon Lispスタイル（インデント、命名）。
- **Package**: `:swimmy.core`, `:swimmy.school` 等のパッケージで適切に名前空間を分ける。
- **Error Handling**: `handler-case` でメインループを保護し、エラーでプロセスを落とさない。
- **Log**: `format t` ではなくロガー関数を通す（タイムスタンプ付与のため）。

## テスト規約

### 統合テスト (Connecting the dots)
- **疎通確認**: `tools/test_heartbeat.sh` 等のスクリプトを使用し、各コンポーネントが生きているか確認する。
- **Risk Gateテスト**: `guardian` を `--test` モードまたはUnit Testで起動し、制限値超えの注文が拒否されるか確認する。

### MT5ストラテジーテスター
- 原則、SwimmyBridge自体のバックテストは（通信相手がいないため）機能しない。
- 戦略ロジックの検証は **Rust製バックテスター** または **Lisp製シミュレーター** で行う。
