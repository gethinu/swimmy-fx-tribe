# Swimmy Port Map

## Core Ports (5555-5565)

| Port | Process | Direction | Purpose |
|------|---------|-----------|---------|
| 5555 | Brain (sbcl) | PULL/bind | Sensory Nerve - 全てのinput受信 (Guardian→Brain, Python tools→Brain) |
| 5556 | Brain (sbcl) | PUB/bind | Motor Nerve - コマンド出力 (Brain→Guardian) |
| 5557 | Guardian (rust) | PUB/bind | Market Data - マーケットデータ配信 (Guardian→Brain) |
| 5559 | Guardian (rust) | SUB/bind | External Command - 外部コマンド受付 (Legacy) |
| 5560 | Guardian (rust) | Unknown | 未使用？要調査 |
| 5561 | data_keeper.py | REP/bind | Data Keeper - ヒストリカルデータ提供 |
| 5562 | notifier.py | PULL/bind | Notifier - Discord通知キュー |
| 5563 | risk_gateway.py | REP/bind | Risk Gateway - リスクチェック |
| 5564 | pattern_similarity_service.py | REP/bind | Pattern Similarity - チャートパターン類似度（S式） |
| 5565 | inference_worker.py | REP/bind | Inference Worker - AI推論（S式） |

## エフェメラルポート (32xxx, 33xxx, 34xxx)

これらは**一時的な発信ポート**で、Discord Webhook 呼び出しなどの HTTP リクエスト時に自動割り当てされます。**正常な動作**であり、接続終了後に自動解放されます。

## 注意

- VS Code の「97ポート」表示は、エフェメラルポートを含んでいます
- 実際に必要なのは上記の **9ポートのみ**
- ポート 5560 は現在未使用の可能性あり（要調査）
