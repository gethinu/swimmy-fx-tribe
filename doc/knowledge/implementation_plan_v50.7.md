# 🧭 Strategy Edge Reinforcement Plan V50.7

**更新日:** 2026-02-20 JST  
**ステータス:** Draft（KPI-first）

---

## 0. 運用監視KPI（最優先）

> 新規機能の前に、まず「今どこで負けているか」を定点で可視化する。

### KPI-0: Live Edge Guard 準拠率（既存正本の遵守）
- 定義: `deployment_gate_status=LIVE_READY` かつ実行時 Live Edge Guard（PF/WR/net_pnl/loss_streak）を通過した発注割合
- 目的: ルール上は許可されるが実績劣化で fail-closed された案件を把握
- 基準: 既存 `STATE/SPEC` の hard gate をそのまま使用（新閾値は増やさない）

### KPI-1: 実運用PnL健全性（7日/30日）
- 定義: `trade_logs` から `net_pnl`, `profit_factor`, `win_rate`, `max_loss_streak` を 7日/30日で集計
- 目的: 「一時的に勝つ」ではなく連続稼働での edge 減衰を検知
- 基準: hard fail は既存 Live Edge Guard へ委譲。ここでは trend（悪化方向）を監視

### KPI-2: Rank Conformance ドリフト
- 定義: `tools/check_rank_conformance.py` の `violations.total` と `transitions`（前日比）
- 目的: low-trade 過大評価やランク残留ドリフトの再発を日次で監視
- 基準: `RANK_CONF_MAX_VIOLATIONS` は運用値として明示管理（既定0）

### KPI-3: Breeder Parent 品質率
- 定義: 交配候補のうち `can-breed-p` を通過した割合（rank別）
- 目的: 親候補の枯渇/質低下を可視化し、交配ロジックを感覚で調整しない
- 基準: まず観測を固定化（閾値はV50.7内でデータを見て決める）

---

## 1. 実装タスク（KPI固定化）

- [x] **V50.7-P0 日次 Edge Scorecard を生成**（2026-02-20 完了）
  - 追加: `tools/edge_scorecard.py`（JSON出力）
  - 出力: `data/reports/edge_scorecard_latest.json` + 履歴 `data/reports/edge_scorecard/`
  - 必須項目: KPI-0〜KPI-3
  - 完了条件: 欠損時も fail-open せず `status=degraded` で可観測化
  - 実装:
    - `build_edge_scorecard` / `run_edge_scorecard` を実装（latest + history）
    - 既存 `check_rank_conformance` を参照して KPI-2/KPI-3 を算出
    - 回帰テスト `tools/tests/test_edge_scorecard.py` を追加

- [ ] **V50.7-P1 system_audit へ Edge Scorecard 統合**
  - 追加先: `tools/system_audit.sh`
  - 方針: WARN ステップとして実行、summary をログへ1行出力
  - 完了条件: `swimmy-system-audit.timer` 日次実行で定点出力される

- [ ] **V50.7-P2 Discord運用通知（要約のみ）**
  - 追加: scorecard の `degraded/critical` 時だけ通知
  - 目的: ノイズ通知ではなく、対応が必要な劣化だけを通知
  - 完了条件: 通常日は無通知、異常日のみ要約通知

- [ ] **V50.7-P3 KPIドキュメント固定化**
  - 反映先: `docs/llm/STATE.md`（契約）/ 必要なら `SPEC.md`
  - 内容: 指標定義、算出窓、データソース、fail条件
  - 完了条件: 実装とドキュメントの差分が無い

---

## 2. 検証方針

- 単体:
  - `tools/tests/test_check_rank_conformance.py`
  - `tools/tests/test_edge_scorecard.py`（新規）
- 結合:
  - `tools/system_audit.sh` 実行で KPI 出力を確認
- 回帰:
  - `SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp`

---

## 3. 非ゴール（V50.7ではやらない）

- 交配アルゴリズムの大規模再設計（まず計測を固定）
- 新規ランク定義の追加（B/A/S/Legend体系は維持）
- Live Gate hard threshold の独断変更（既存正本準拠）

---

## 4. 完了判定

- KPI-0〜KPI-3 が日次で自動出力される
- 異常時に運用が「どの層で崩れたか」を1回で特定できる
- STATE/SPEC と実装の契約差分が解消されている
