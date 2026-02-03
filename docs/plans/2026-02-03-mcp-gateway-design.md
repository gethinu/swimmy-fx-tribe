# MCP Gateway Design (WSL2)

**Date:** 2026-02-03  
**Scope:** MCPゲートウェイ（C: 将来のライブ拡張口まで設計、実装は封印）

## 背景 / 目的
MT5/Rust/Lispの既存ZMQ契約を崩さず、外部からの自動化（バックテスト/状態取得）を可能にする。  
“C”設計として将来のライブ拡張口を定義するが、初期実装はread-only/backtest-execに限定し、trade-capableは無効化する。

## 目標
- 既存ZMQポートを変更しない（5559のみ使用）。
- JSON境界はMCPゲートウェイ1箇所に限定、内部はS式正本。
- IDE依存を排除し、CLI/バッチ/CIからの呼び出しを前提にする。

## 非目標
- MT5命令(5560)の直接操作。
- Brain/Rustの内部改造（最小変更）。
- trade-capableの即時解放。

## アーキテクチャ概要
```
MCP Client -> MCP Gateway (WSL2, systemd) -> ZMQ PUB -> 5559 (Guardian SUB)
                                          -> status/log reader -> MCP response
```
- WSL2内に常駐し、localhostバインドを既定。
- 外部公開は明示フラグで許可（デフォルト非公開）。

## コンポーネント
1. **MCP Gateway Core**
   - 入力検証、権限判定、JSON→S式変換、ZMQ送信。
2. **ZMQ Adapter**
   - `tcp://localhost:5559` へPUB送信（エンドポイントは設定可能）。
3. **Observer**
   - `/tmp/swimmy_status`, `data/reports/backtest_status.txt` から状態取得。
4. **CLI**
   - MCPクライアント経由の一括実行/バッチ化を想定。

## インターフェース（MCP Tools）
- `health.ping`（read-only）
- `system.status` / `system.metrics`（read-only）
- `backtest.submit` / `backtest.status`（backtest-exec）
- `trade.submit`（trade-capable / 初期は常に 403）

## データフロー
### backtest.submit
1. 入力検証（symbol/期間/TF/データソース）
2. S式生成（INTERFACES.md準拠）
3. ZMQ送信（5559）
4. `request_id` を返す
5. `backtest.status` で観測（結果は既存ファイルから読む）

### system.status
- `/tmp/swimmy_status` と `backtest_status.txt` の読み取りのみ（副作用無し）。

## 権限レイヤー
- **read-only**: 状態取得のみ
- **backtest-exec**: BACKTESTのみ許可
- **trade-capable**: 403固定（将来の拡張口）

## セキュリティ
- APIキー必須（環境変数）
- localhostバインドがデフォルト
- 外部公開は明示フラグで許可

## エラーハンドリング
- 4xx: 入力検証エラー
- 5xx: ZMQ送信失敗 / 観測失敗
- 失敗時はJSONLログに記録（request_id, action, duration, result）

## テスト戦略
1. **契約テスト**: S式のgolden fixtures化、JSON拒否（Iron Rule）を明示。
2. **統合スモーク**: ZMQエンドポイント差し替え可能にし、本番5559を叩かない。
3. **運用リハ**: systemd起動/停止、APIキー拒否、ネットワーク遮断時の挙動。

## 運用 / 監視
- systemdで常駐管理、healthチェックを用意。
- 緊急停止は systemd stop + bind遮断。
- runbookに復旧手順と開放条件を明記。

## ロールアウト方針
1. read-only / backtest-exec のみ有効化
2. `request_id` 相関と観測点整備
3. trade-capableはCPCV品質と監視が揃うまで封印

## リスクと対策
- **本番ポート誤打ち**: エンドポイント差し替えと環境分離
- **データ揺れ**: M1正本/リサンプル規約を固定
- **権限誤用**: 層分離 + 初期403固定

## 未決事項
- request_id を `backtest_status.txt` に反映する拡張の詳細
- 外部公開時のFW/ネットワーク方針
