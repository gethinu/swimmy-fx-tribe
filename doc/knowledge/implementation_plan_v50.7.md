# 🧭 Strategy Edge Reinforcement Plan V50.7

**更新日:** 2026-02-20 JST  
**ステータス:** Draft（KPI-first）

---

## 2026-02-22 運用追補: XAU AutoBot Trial V2（期間固定GO/NO-GO）

- 方針:
  - live劣化時の自動ガードに依存せず、**期間固定で判定して不合格は即ボツ**にする。
  - 判定基準は先に固定し、期間中に閾値を動かさない。
- 設定:
  - trial config: `tools/configs/xau_autobot.trial_v2_20260222.json`
  - 分離キー: `magic=560072`, `comment=xau_autobot_trial_v2_20260222`
  - guard既定: `XAU_AUTOBOT_LIVE_GUARD_ENABLED=0`（明示有効化時のみ動作）
- 運用コマンド:
  - 実行（trial configでloop, guard無効固定）:
    - `tools/xau_autobot_trial_v2_start.sh`
  - 評価（live report + GO/NO-GO 判定）:
    - `tools/xau_autobot_trial_v2_eval.sh`
- 判定コマンド:
  - `./.venv/bin/python tools/xau_autobot_trial_judge.py --reports-dir data/reports --min-days 14 --min-closed-positions 30 --min-profit-factor 1.10 --min-win-rate 0.42 --min-net-profit 0 --fail-on-no-go`
- 2026-02-22 判定結果（既存live report基準）:
  - 出力: `data/reports/xau_autobot_trial_judge.json`
  - verdict: `NO_GO`
  - failed_checks: `closed_positions`, `profit_factor`, `win_rate`, `net_profit`
- 2026-02-22 実装追補（trial成立性の明示）:
  - `tools/xau_autobot_trial_judge.py`
    - verdict を `GO/NO_GO/INVALID_TRIAL` の3値化
    - `diagnostics.after_magic_filter` / `after_comment_prefix_filter` が 0 の場合は `INVALID_TRIAL`
    - 出力項目を追加: `trial_valid`, `invalid_reasons`
  - `tools/xau_autobot_trial_v2_start.sh`
    - 起動前 preflight を追加（`xau_autobot.py` / `xau_autobot_live_loop.ps1` 既存稼働を検知して fail-fast）
    - `flock` ベースの排他ロックを追加（fallback: `mkdir` ロック）
    - 強制実行フラグ: `XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES=1`

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

- [x] **V50.7-P1 system_audit へ Edge Scorecard 統合**（2026-02-21 完了）
  - 追加先: `tools/system_audit.sh`
  - 方針: WARN ステップとして実行、summary をログへ1行出力
  - 完了条件: `swimmy-system-audit.timer` 日次実行で定点出力される
  - 実装:
    - `tools/system_audit.sh` に `run_edge_scorecard_audit` を追加
    - 監査フローに `run_warn "Edge scorecard"` ステップを統合
    - help usage に `EDGE_SCORECARD_*` 環境変数を追加
    - `tools/test_system_audit.sh` に usage/step/summary の回帰チェックを追加

- [x] **V50.7-P2 Discord運用通知（要約のみ）**（2026-02-21 完了）
  - 追加: scorecard の `degraded/critical` 時だけ通知
  - 目的: ノイズ通知ではなく、対応が必要な劣化だけを通知
  - 完了条件: 通常日は無通知、異常日のみ要約通知
  - 実装:
    - `tools/edge_scorecard.py` に通知ポリシー判定 / webhook解決 / notifier経由queueを実装
    - `tools/edge_scorecard.py` に `send_discord_notification` を追加（要約 `content` + embed）
    - `tools/system_audit.sh` から `EDGE_SCORECARD_DISCORD_*` を引き渡して日次監査に統合
    - `tools/tests/test_edge_scorecard.py` で通知判定・送信・env webhook解決を回帰テスト化

- [x] **V50.7-P3 KPIドキュメント固定化**（2026-02-21 完了）
  - 反映先: `docs/llm/STATE.md`（契約）/ 必要なら `SPEC.md`
  - 内容: 指標定義、算出窓、データソース、fail条件
  - 完了条件: 実装とドキュメントの差分が無い
  - 実装:
    - `docs/llm/STATE.md` に `Edge Scorecard KPI 定義契約（V50.7-P3）` を追記
    - KPI-0..3 の data source / degraded条件 / overall集約を明文化

- [x] **V50.7-P4 Edge Scorecard 専用timer/service 追加**（2026-02-21 完了）
  - 追加先: `systemd/` + `tools/`
  - 方針: `system_audit` とは独立に scorecard を日次実行可能にする
  - 完了条件: `dry-run` で unit install 経路が検証できる
  - 実装:
    - `systemd/swimmy-edge-scorecard.service`
    - `systemd/swimmy-edge-scorecard.timer`
    - `tools/edge_scorecard_runner.sh`
    - `tools/install_edge_scorecard_service.sh`
    - `tools/test_install_edge_scorecard_service.sh`
    - `install_edge_scorecard_service.sh` に `SWIMMY_SYSTEMD_SCOPE=system|user` を追加（sudo不可環境は `user` scope で有効化）
    - user scope 実行時は installer が配置先 service unit から `User=`/`Group=` を除去する（`status=216/GROUP` 回避）

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

---

## 5. Armada運用投入トラック（V50.6連携）

> V50.7のKPI運用を維持したまま、Armada core5 を投入レベルまで引き上げる。

### 5.1 現状（2026-02-22）

- B2R（pandajiro/yumimin, volsma）は完了:
  - `seed={11,23,47,83,131}` で `top3 (oos_ok && cpcv_ok)=3/3` を維持
  - 参照: `data/reports/armada_b2_volsma_seed_sweep_20260221_summary.json`
- C1（core5投入判定）は保留:
  - strict/proxy ともに `deploy_decision=保留`
  - 参照:
    - `data/reports/armada_deploy_readiness_20260222_b1rrefresh.json`
    - `data/reports/armada_deploy_readiness_20260222_proxy_b1rrefresh.json`
- B1R（taiki/kojirin再現性）は完走したが未達:
  - 参照: `data/reports/armada_b1_seed_sweep_20260222_summary.json`
  - 集計: `taiki=2/5`, `kojirin=1/5`, `both_players_pass=0/5`
  - 判定: `b1r_completed=false`（完了条件 `>=4/5` を満たさず）
- A1（paper投入監視パック）は完了:
  - 生成: `data/reports/armada_paper_readiness_20260222.json`
  - 判定: `decision=HOLD`（`paper_trade_count=0/20`, `slippage_sample_count=0/20`）

### 5.2 実行タスク（次バッチ）

- [x] **V50.7-A0 B1R共通探索（taiki/kojirin同時）実測**
  - 目的: 共通条件での seed 再現性の上限を把握する。
  - 成果物: `data/reports/armada_b1_seed_sweep_20260222_summary.json`
  - 結果: `player_pass_counts={taiki:2, kojirin:1}`, `both_players_pass_count=0/5`
  - 判定: 共通探索は完了したが、プレイヤー別の癖を吸収できず未達。

- [ ] **V50.7-A0T taiki専用 B1Rトラック**
  - 目的: `taiki` の pass rate を `>=4/5` まで引き上げる（kojirin と分離して最適化）。
  - 完了条件: 5 seed 中 `4/5` 以上で `top3 oos_ok>=1`。
  - 実行方針:
    - `--players taiki` の単独 sweep で実行。
    - taiki で優勢だった指標を優先（`rsi/volsma/vwapvr` を段階探索）。
  - 成果物:
    - `data/reports/armada_b1_seed_sweep_YYYYMMDD_taiki_summary.json`

- [ ] **V50.7-A0K kojirin専用 B1Rトラック**
  - 目的: `kojirin` の pass rate を `>=4/5` まで引き上げる（taiki と分離して最適化）。
  - 完了条件: 5 seed 中 `4/5` 以上で `top3 oos_ok>=1`。
  - 実行方針:
    - `--players kojirin` の単独 sweep で実行。
    - kojirin で相性の良い指標を優先（`vwapvr/volsma/rsi` を段階探索）。
  - 成果物:
    - `data/reports/armada_b1_seed_sweep_YYYYMMDD_kojirin_summary.json`

- [x] **V50.7-A1 C2 paper投入監視パック作成**
  - 目的: L3条件（paper 20 trades）を機械判定できる状態にする。
  - 完了条件: DD/スリッページ/連敗/実現PnLの警戒値を日次評価し、`GO/HOLD` を出力。
  - 成果物: `data/reports/armada_paper_readiness_YYYYMMDD.json`
  - 2026-02-22 実行メモ:
    - 生成: `data/reports/armada_paper_readiness_20260222.json`
    - 判定: `summary.decision=HOLD`
    - 根拠:
      - `paper_evidence_shortage: 0/20`
      - `slippage_samples_shortage: 0/20`
    - 閾値ソース（正本準拠）:
      - `paper_min_trades=20`
      - `runtime_guard: net_pnl>=0 / latest_loss_streak<=3`
      - `drawdown_guard: hard_dd<=12% / weekly_dd<=4%`
      - `slippage_guard: p95_abs_pips<=3.0 (min samples=20)`

- [x] **V50.7-A2 C1 refresh（B1R反映版）**
  - 目的: B1R完了後の最新入力で core5 の投入判定を更新する。
  - 完了条件: strict/proxy の両ビューで `投入可/保留/再探索` を再分類。
  - 成果物: `data/reports/armada_deploy_readiness_YYYYMMDD_refresh.json`
  - 2026-02-22 実行メモ:
    - strict refresh: `data/reports/armada_deploy_readiness_20260222_b1rrefresh.json`
    - proxy refresh: `data/reports/armada_deploy_readiness_20260222_proxy_b1rrefresh.json`
    - 反映点:
      - `b1_seed_sweep_summary`（共通探索の実測）を入力追加
      - `paper_readiness`（A1: HOLD）を入力追加
      - taiki/kojirin に `b1_seed_pass_count` / `b1_seed_repro_gate_pass` を追記
    - 判定:
      - strict: `投入可=0, 保留=4, 再探索=1`（維持）
      - proxy: `投入可=0, 保留=5, 再探索=0`（維持）
      - `deploy_decision=保留`（B1R未達 + paper証拠不足）

- [x] **V50.7-A3 nami strict R&Dトラック継続**
  - 目的: `BT PF>=1.30` 未達のボトルネックを分離し、運用トラックと切り離して検証する。
  - 完了条件: strict到達性の改善有無を monthly で判定（到達不能なら探索空間見直し）。
  - 成果物: `data/reports/armada_nami_strict_reachability_YYYYMMDD.json`
  - 2026-02-22 実行メモ:
    - 生成: `data/reports/armada_nami_strict_reachability_20260222.json`
    - 参照: `data/reports/armada_nami_strict_reachability_summary_20260222_refresh.json`
    - 判定: `max_bt_pf_observed=1.2514`, `strict_pf130_unreachable_in_scanned_space=true`
    - 月次判定: `UNREACHED`（R&Dトラック継続）

- [x] **V50.7-A4 B1R是正探索（indicator drift抑制 / 第1波）**
  - 目的: B1R未達の主因（`rsi/volsma` 偏重）を抑制し、`taiki/kojirin` の OOS再現性を改善する。
  - 完了条件: 是正条件で `seed={11,23,47,83,131}` を再実行し、`both_players_pass_count>=4/5` を達成。
  - 成果物:
    - `data/reports/armada_b1r_failure_analysis_20260222.json`
    - `data/reports/armada_player_replica_YYYYMMDD_b1r_fix_taiki_seed*_*.json`
    - `data/reports/armada_player_replica_YYYYMMDD_b1r_fix_kojirin_seed*_*.json`
    - `data/reports/armada_b1_seed_sweep_YYYYMMDD_fix_summary.json`
  - 2026-02-22 事前分析:
    - `data/reports/armada_b1r_failure_analysis_20260222.json`
    - 集計: `taiki_seed_pass_rate=0.4`, `kojirin_seed_pass_rate=0.2`, `both_players_pass_count=0/5`
    - 補足: `strict_bt_ok_total_top3={taiki:0, kojirin:0}`
    - 次run方針: player分割 + indicator制約（`vwapvr`中心）+ `candidates_per_player<=120` で drift を抑える
  - 2026-02-22 実行メモ（第1波）:
    - 実行条件:
      - player分割（`taiki` / `kojirin` 別run）
      - `indicators=vwapvr`（drift抑制）
      - `candidates_per_player=120`, `top_per_player=3`
      - `cpcv_folds=5`, `cpcv_require_for_core=true`
      - `oos_min_trades_abs=50`, `oos_trade_ratio_floor=0.35`
    - 生成:
      - `data/reports/armada_player_replica_20260222_b1r_fix_taiki_seed*_c120_vwapvr_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix_kojirin_seed*_c120_vwapvr_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix_taiki_kojirin_seed*_c120_vwapvr_top3.json`
      - `data/reports/armada_b1_seed_sweep_20260222_fix_summary.json`
      - `data/reports/armada_b1r_fix_evaluation_20260222.json`
    - 結果:
      - `player_pass_counts={taiki:3, kojirin:1}`
      - `both_players_pass_count=0/5`
      - `b1r_completed=false`
      - baseline比: `taiki +1`, `kojirin ±0`, `both ±0`
    - 判定: drift抑制は taiki 側のみ改善。`>=4/5` ゲートは未達のため次波へ継続。

- [ ] **V50.7-A5 B1R是正探索（player別ミックス / 第2波）**
  - 目的: `kojirin` 側の OOS不安定を是正し、A4未達（`both_players_pass_count=0/5`）を突破する。
  - 完了条件: `seed={11,23,47,83,131}` で `both_players_pass_count>=4/5`。
  - 実行方針:
    - `taiki`: `indicators=vwapvr,vwap`（A4で改善した `vwapvr` を軸に僅かに探索拡張）
    - `kojirin`: `indicators=vwapvr,volsma`（`vwapvr` 軸 + 既存適性のある volume 系を限定復帰）
    - 追加比較軸: `kojirin` のみ `disable_hold_tf_filter` ON/OFF 比較（TF120固定の偏りを緩和）
  - 成果物:
    - `data/reports/armada_player_replica_YYYYMMDD_b1r_fix2_taiki_seed*_*.json`
    - `data/reports/armada_player_replica_YYYYMMDD_b1r_fix2_kojirin_seed*_*.json`
    - `data/reports/armada_b1_seed_sweep_YYYYMMDD_fix2_summary.json`
    - `data/reports/armada_b1r_fix2_evaluation_YYYYMMDD.json`
