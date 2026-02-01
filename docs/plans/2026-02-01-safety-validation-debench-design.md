# Safety & Validation & Debench Design

**Date:** 2026-02-01
**Owner:** Swimmy Core

## Goal
入力安全・検証整合・運用単純化を同時に達成する。
- 外部入力の安全な読み取りを保証する
- Backtest V2 の検証パイプラインを正しいデータフローに統一する
- ベンチ概念を廃止し、二重系統の状態機械を解消する

## Scope
**In scope**
- `read-from-string` の入力境界化（ZMQ/LLM/永続化ロード）
- Backtest V2 payload 修正と Phase2 昇格ロジック実装
- ベンチ廃止（戦略ベンチ/アームベンチ/表示/分岐）
- systemd install fail-fast 化
- 監視スクリプト修正＋簡易スモーク
- config/state 分離（重複グローバル解消）

**Out of scope**
- 新規機能追加や大規模リファクタリング
- UI/ダッシュボード大幅改修

## Decisions
1) **評価フェーズのベンチは廃止し、基準未満は即墓場/kill**
- Sharpe/PF/WR基準を満たさない戦略は棚上げせず淘汰

2) **実行フェーズにベンチ無し**
- Thompson Sampling と既存リスク管理に委ね、`*benched-arms*` を廃止

3) **外部入力は安全リーダで隔離**
- `read-from-string` の直接評価を禁止し、ホワイトリスト読取に統一

4) **Backtest V2 は `strategy-alist` を唯一の真実に**
- 送信 payload は alist のみ、境界で JSON 化

## Architecture
### Input Safety Layer
- 新しい安全読み取り関数（例: `safe-read-sexp`）を core に配置
- `*read-eval*` 無効化と許可トークンの限定
- 不正入力は拒否し、ログを出して処理を継続

### Backtest V2 Integrity
- `strategy-alist` → payload の単一経路
- Phase1/Phase2 を同じ handler で処理し、Phase2 合格は Rank A/S へ昇格
- 結果が不正/欠損の場合は昇格も墓場も行わずログで検知

### Debench Simplification
- 戦略ベンチ: `strategy-status :benched` の使用を廃止
- アームベンチ: `*benched-arms*` と `arm-benched-p`/`bench-arm` を撤廃
- 心拍（Heartbeat）表示から benched カウントを削除

## Data Flow (Summary)
1. ZMQ/LLM/DB からの入力 → 安全リーダ → スキーマ検証 → 既存処理
2. Backtest V2: `strategy-alist` → payload → Guardian → results → handler → Rank 更新
3. 評価: 基準未満は墓場/kill、基準以上は存続
4. 実行: Thompson Sampling + risk guard による自然選択

## Error Handling
- 安全リーダで失敗した入力は破棄し、エラーログを出す
- Backtest V2 結果の欠損/異常値は無視し、プロモーション停止
- systemd install 失敗は即終了（成功偽装を禁止）

## Testing Strategy (TDD)
- 安全リーダ: 不正トークン拒否/正当入力許可
- Backtest V2: payload 正常化・Phase2 昇格・異常値無視
- Debench: 評価分岐の削除、実行で arm ベンチが無い状態でも動作
- 監視: `report_system_status.py` の import 依存不足回避＋簡易実行

## Rollout / Risk
- 安全リーダ導入は初期フラグで切替可能にし、拒否ログで調整
- ベンチ廃止は「評価淘汰の強化」と「実行の単純化」を同時に行う
- 既存のベンチ依存ログは更新し、過去ログとの混同を避ける

