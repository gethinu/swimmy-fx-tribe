# 🦅 Expert Panel Report

**Date:** 2026-02-02  
**Leader:** Elon Musk  
**Trigger:** /expert-panel「ZMQ通信＋ローカル保存までS式化(B案)で進めたい。意見が欲しい」

## 🏛️ 常設顧問の意見
### Taleb: “S式全域化は攻撃面の拡大。安全なリーダ無しは自滅”
- 受信側の中心は`message-dispatcher`でS式を読んでいる。全ZMQをS式にすると、**入力面が全方位化**する。安全なリーダ/検証なしで進めれば破滅。`src/lisp/core/message-dispatcher.lisp:121-129`
- 仕様がJSON前提のままなのにS式に進めば、運用監査の真実が崩れる。**まず仕様の一枚岩化**が先。`docs/llm/INTERFACES.md:3-16`

### Graham: “仕様の正義が割れているまま移行するな”
- インターフェースは**JSONと明記**され、Backtestだけ例外的にS式。B案はこの分裂を全域に拡散する。`docs/llm/INTERFACES.md:3-126`
- SPECはData Keeperや各コンポーネントを前提に書かれているが、S式化の責務は未定義。**まずSPEC/ARCH/STATEの整合が必要**。`docs/llm/SPEC.md:7-53`, `docs/llm/ARCHITECTURE.md:35-56`

### Naval: “保守性を上げるどころか、全言語に痛みを配る”
- Lisp内だけでもJSON依存が広範囲。全域S式は**MQL5/Rust/Pythonまで改修**が必須で、保守性が逆に低下する。`src/lisp/system/runner.lisp:72-103`, `src/lisp/core/data-client.lisp:60-122`
- ZMQだけでなくローカル保存もS式化するなら、既存のJSONL監査/ログ運用が壊れる。**保守性を理由に運用の基盤を壊すのは本末転倒**。`src/lisp/core/db-adapter.lisp:14-54`, `src/lisp/logger.lisp:7-35`

### Jim Simons: “検証基盤が切断される”
- 統合テストはJSON入力/出力に依存。全域S式化は**テストの再設計**が必須。`src/lisp/tests/integration-tests.lisp:13-31`
- 監査/統計はJSONL前提で集計されている。S式化で過去比較が不可能になる。`src/lisp/core/db-adapter.lisp:46-54`

## 💻 技術パネルの意見
### Fowler: “境界の崩壊が最大の技術負債”
- `internal-process-msg`はS式/JSONの二重分岐。全域S式化は**境界の共通化（変換層）**なしでは破滅的。`src/lisp/core/message-dispatcher.lisp:121-343`
- Backtest ServiceはS式入力でも**辞書→JSON返却が残る**。仕様が揺れたまま。`tools/backtest_service.py:520-528`

### Hickey: “Lispの都合で全員を巻き込むな”
- S式はLisp内部では最強だが、**他言語での正規化設計がない**。全域S式化は複雑性を増す。`docs/llm/INTERFACES.md:3-16`
- ローカル保存S式化は、JSON前提の可観測性（logger/telemetry/live_status）と衝突する。`src/lisp/logger.lisp:7-35`, `src/lisp/school/school-telemetry.lisp:7-34`, `src/lisp/shell/notifications.lisp:123-167`

### Uncle Bob: “テストと移行計画なしの統一は事故”
- JSON→S式の**移行テストが存在しない**。BacktestだけでなくZMQ/保存の全域を変えるなら、失敗時のロールバック/互換モードが必須。`src/lisp/tests/integration-tests.lisp:13-31`
- Notifier/DiscordはJSON送信前提。全域S式化で壊れる箇所が明確。`src/lisp/core/discord.lisp:40-68`

## 🚀 ビジョナリーの意見
### Ng: “観測とAI運用の互換性が落ちる”
- テレメトリやログはJSON前提で可視化されている。S式化は**監視の再構築**が必要。`src/lisp/school/school-telemetry.lisp:7-58`, `src/lisp/logger.lisp:7-35`

### López de Prado: “履歴の連続性が断裂する”
- JSONL監査/ログをS式に変えると過去比較が途切れる。統計の前提が崩れる。`src/lisp/core/db-adapter.lisp:46-54`

### Gene Kim: “運用は壊れやすくなる”
- `live_status.json`やDiscord通知は運用の生命線。S式化は運用ツールの互換性を壊す。`src/lisp/shell/notifications.lisp:123-167`, `src/lisp/core/discord.lisp:40-68`

## 🚀 Musk's Decision (Final)
> 「B案は“やるなら徹底的に設計してから”だ。  
>  まず**SPEC/INTERFACES/STATEの全面改訂**で正義を一本化しろ。  
>  その上で段階移行（互換モード→S式強制）を設計し、テストとロールバックを用意する。  
>  いきなり全域S式はやらない。設計と検証の準備が整ったら実装に入れ。」

## Actionable Items
1. **ドキュメント正義の一本化**：`docs/llm/INTERFACES.md` のEncoding/Backtest記述をB案に合わせ全面更新。`docs/llm/SPEC.md`, `docs/llm/ARCHITECTURE.md`, `docs/llm/STATE.md`も同時更新。`docs/llm/INTERFACES.md:3-126`, `docs/llm/SPEC.md:44-53`
2. **境界設計**：ZMQ全ポートのS式スキーマを定義し、JSONとの互換モード/移行期限を明記。`docs/llm/INTERFACES.md:3-16`
3. **Backtest Serviceの返却統一**：S式入力時にJSON返却する箇所を廃止し、結果もS式で返す。`tools/backtest_service.py:520-528`
4. **ローカル保存の優先順位**：JSONL/telemetry/live_status/backtest_cacheのどれをS式に変えるかを優先順で決め、移行スクリプトを用意。`src/lisp/core/db-adapter.lisp:14-54`, `src/lisp/school/school-telemetry.lisp:7-34`, `src/lisp/shell/notifications.lisp:123-167`, `src/lisp/school/school-backtest-utils.lisp:9-80`
5. **テスト再設計**：JSON前提の統合テストをS式対応に改修し、互換モード/強制モード両方を検証する。`src/lisp/tests/integration-tests.lisp:13-31`
