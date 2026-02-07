# Tribe/Clan Removal Design (Category-Only)

Date: 2026-02-07

## Goal
- 部族/クランの語彙と構造を完全撤去し、カテゴリ（TF×Direction×Symbol）を唯一の上位概念として統一する。
- 破壊的変更として `live_status` スキーマを更新し、監視側と契約を明示する。
- カテゴリ別の取引間隔制御は維持し、名称のみカテゴリへ統一する。

## Non-Goals
- カテゴリ配分/枠/選抜ロジックの再設計は行わない。
- 取引ロジックの性能改善や新機能追加は行わない。
- 互換フィールドの残置やマッピングは行わない。

## Scope
- 表示/通知: 日次レポート、live_status、起動ログ
- 実行ロジック: 取引間隔制御、危険時ゲート
- 状態/永続化: ledger state、globals
- 対話/ドキュメント: REPLコマンド、SYSTEM_ARCHITECTURE
- 削除対象: clan/tribe 構造体、DSLマクロ、儀式、ナラティブ、テスト

## Architecture
- 体系は「カテゴリのみ」。tribe/clan 由来の構造体・状態・表示・ナラティブは削除する。
- 危険時ゲートは `swarm-consensus` のみで判定する。
- `live_status` は `schema_version=2` とし、tribe系フィールドを削除する。
- ledger state から `:tribe-*` を削除し、復元も撤去する。

## Component Changes
- `src/lisp/core/narrative.lisp`: 日次レポートから Tribe 表記と `*tribe-*` 参照を削除。
- `src/lisp/shell/notifications.lisp`: `tribes` と `tribe_consensus` を削除し `schema_version=2` に更新。
- `src/lisp/school/school-execution.lisp`: `*last-clan-trade-time*` を `*last-category-trade-time*` に改名し、関連関数をカテゴリ名へ統一。
- `src/lisp/school/school-voting.lisp`: 危険時ゲートの `tribe-consensus` 依存を撤去。
- `src/lisp/engine/ledger.lisp`: `:tribe-*` を保存/復元から削除。
- `src/lisp/core/globals.lisp` と `src/lisp/packages.lisp`: `*tribe-*` と `*clans*` を削除。
- `src/lisp/school/school-strategy.lisp`: clan 構造体と narrative/treasury など clan 依存の削除。
- `src/lisp/school/school-narrative.lisp`: clan 表示/positions summary の削除またはカテゴリ化。
- `src/lisp/core/rituals.lisp`: clan 依存の儀式描写を削除。
- `src/lisp/dsl.lisp`: `with-tribe-context` マクロを削除。
- `src/lisp/repl.lisp`: `:clans` / `:clan` コマンドを削除。
- `doc/SYSTEM_ARCHITECTURE.md` と `src/lisp/system/runner.lisp`: clan 表記を削除しカテゴリ中心に更新。
- `src/lisp/tests.lisp`: clan テスト削除。カテゴリ制御と危険時ゲートの代替テスト追加。

## Data Contract
- `live_status`:
  - `schema_version` を 2 に変更。
  - `tribes` と `tribe_consensus` を削除。
- `ledger state`:
  - `:tribe-consensus` と `:tribe-direction` を削除。

## Behavior Changes
- 日次レポートの合意率は `Swarm` のみ表示。
- 危険時ゲートの承認条件は `swarm-consensus` のみで判定。
- カテゴリ別の取引間隔制御は維持し、名称のみ変更。

## Migration Plan (Immediate Removal)
1. 通知/レポートの tribe/clan 参照削除と `schema_version=2`。
2. 実行ロジックのカテゴリ名統一と危険時ゲート簡素化。
3. 永続化/グローバル/DSL/REPL/テストの clan/tribe 撤去。
4. ドキュメントと起動ログの更新。

## Testing
- 既存の clan テストを削除。
- 新規テスト:
  - カテゴリ別の取引間隔制御が動作すること。
  - 危険時ゲートが `swarm-consensus` のみで動作すること。
- 手動確認:
  - 日次レポートの Tribe 表記が消えていること。
  - `live_status` の `schema_version=2` と tribe フィールド削除。

## Risks
- 監視/分析側のスキーマ不一致。
- REPLコマンド削除による運用影響。
- ドキュメントの齟齬。

## Success Criteria
- すべての tribe/clan 参照がコードと出力から消えていること。
- `live_status` と ledger state が新スキーマで安定動作すること。
- カテゴリ別の取引間隔制御が維持されること。
