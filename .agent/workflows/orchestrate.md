# /orchestrate - Antigravity複雑タスクオーケストレーション

Antigravity専用の複雑タスク管理ワークフロー。PDCAサイクルで段階的に実行。

## 発動条件

以下のいずれかに該当:
- 推定作業時間 **30分以上**
- 変更対象 **5ファイル以上**
- ユーザー明示指定 `/orchestrate`

## Antigravity のツールセット

| ツール | 用途 |
|--------|------|
| `task_boundary` | フェーズ/進捗管理 |
| `notify_user` | ユーザー承認ポイント |
| `browser_subagent` | UI/ブラウザテスト |
| `run_command` | シェルコマンド実行 |
| `*_file_content` | コード編集 |

**注意**: 真の「subagent」はない。全てのコード実装はこのエージェント自身が行う。

---

## Phase 1: PLAN (計画) - PLANNING モード

// turbo
1. `task_boundary(Mode=PLANNING)` で開始
2. 要件分析 → `implementation_plan.md` に書き出し
3. タスク分解 → `task.md` に **5-10分単位** で細分化
4. `notify_user(BlockedOnUser=true)` で計画承認を取得

**承認前に実装しない**

---

## Phase 2: DO (実行) - EXECUTION モード

各タスクに対して:

// turbo
1. `task_boundary(Mode=EXECUTION, TaskStatus="タスク名")` でタスク開始
2. 必要なツールで実装（並列実行可能なものは並列で）
3. `task.md` のチェックボックスを `[x]` に更新
4. 5ツール毎に `task_boundary` でサマリー更新

**エラー発生時**: 即座に `notify_user` でユーザーに報告

---

## Phase 3: CHECK (検証) - VERIFICATION モード

// turbo
1. `task_boundary(Mode=VERIFICATION)` に切り替え
2. `/quality-check` ワークフローを実行
3. UIテストが必要な場合は `browser_subagent` を使用
4. テスト失敗 → ACT へ、成功 → 完了

---

## Phase 4: ACT (改善)

**テスト失敗時**:
1. 失敗原因を分析
2. 修正タスクを `task.md` に追加
3. Phase 2 に戻る（Modeは EXECUTION に戻す）

**成功時**:
1. `walkthrough.md` に成果を記録
2. `notify_user(BlockedOnUser=false)` で完了報告

---

## 連携ワークフロー

| ワークフロー | 使用タイミング |
|--------------|----------------|
| `/quality-check` | Phase 3 (CHECK) |
| `/expert-panel` | 設計レビュー時 |
| `/deploy` | 本番デプロイ時 |

---

## 出力アーティファクト

- `task.md` - 進捗チェックリスト
- `implementation_plan.md` - 設計書（レビュー用）
- `walkthrough.md` - 完了報告
