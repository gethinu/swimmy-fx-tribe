---
description: 信頼性・可用性・耐障害性に特化した専門家パネル（Vogels, Armstrong, Hamilton等）への諮問
---

# /expert-panel-2 - インフラ・信頼性専門家パネル

このワークフローは、システムの可用性、耐久性、保守性、耐障害性に特化した専門家グループに諮問し、アーキテクチャの欠陥を極限まで洗い出します。

## パネル構成
- **Infrastructure**: Werner Vogels (Amazon CTO), Kelsey Hightower (Kubernetes)
- **Fault Tolerance**: Joe Armstrong (Erlang開発者), Leslie Lamport (分散システム)
- **Safety/Precision**: Margaret Hamilton (Apollo計画), NASA Software Assurance
- **Quality/Performance**: W. Edwards Deming (品質管理), Brendan Gregg (パフォーマンス)

## 実行ステップ

1. **システム状態の深層分析**
   - ZMQ通信、メッセージディスパッチ、永続化（WAL未実装等）のコードを精査。
   - 並行処理の設計（スレッド競合、ブロッキングI/O）を特定。

2. **「極限」のフィードバック収集**
   - 「0.1%の確率で何が起きるか？」「ディスクが一杯になったらどうなるか？」といった極限状態の欠陥を指摘。

3. **Muskの決断 (統合)**
   - 信頼性向上のための具体的なアクションアイテムを決定。

4. **成果物の保存**
   - `expert_panel_2_[DATE].md` としてレポートを作成。

## 出力形式
各専門家による辛辣な批評と、最終的な「レジリエンス・ロードマップ」。
