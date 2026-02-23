# 🧭 Strategy Edge Reinforcement Plan V50.7

**更新日:** 2026-02-22 JST  
**ステータス:** Draft（KPI-first）

---

## 2026-02-22 運用追補: Institutional Hunter EA 最適化（MT5）

- 実行1（完了）:
  - 実行ID: `ih_opt_full_20260221_163522`
  - 条件:
    - `Expert=InstitutionalHunterEA.ex5`
    - `ExpertParameters=InstitutionalHunterEA_OptimizeCore_XAU_FX4.set`
    - `Symbol=XAUUSD`, `Period=M15`, `Model=Every tick based on real ticks`
    - `Optimization=Genetic`, `Forward=1/3`
    - `Back: 2024-08-21 00:00 -> 2025-08-21 00:00`
    - `Forward: 2025-08-21 00:00 -> 2026-02-20 00:00`
  - 完了ログ（2026-02-22 13:25 JST）:
    - `forward optimization finished, total passes 4158`
    - `optimization done in 20 hours 51 minutes 39 seconds`
  - 成果物:
    - `C:\Users\stair\AppData\Roaming\MetaQuotes\Terminal\D0E8209F77C8CF37AD8BF550E51FF075\ih_opt_full_20260221_163522.xml`
    - `C:\Users\stair\AppData\Roaming\MetaQuotes\Terminal\D0E8209F77C8CF37AD8BF550E51FF075\ih_opt_full_20260221_163522.forward.xml`
  - 要点:
    - Back最上位: `Result=10063.08`, `PF=1.641448`, `DD%=0.8682`, `Trades=5`
    - Forward最上位: `Forward Result=10016.94`, `DD%=0.0913`, `Trades=1`
    - Forward分布: `4158件中 trades=0 が 3627件 / trades=1 が 517件 / trades=2 が 14件`
    - 判定メモ: このrun単体では forward 側の取引件数が少なく、運用採用には追加検証が必要

- 実行中（継続監視）:
  - 開始: 2026-02-22 18:18 JST
  - 条件:
    - `Back: 2023-02-20 00:00 -> 2025-02-19 00:00`
    - `Forward: 2025-02-19 00:00 -> 2026-02-20 00:00`
  - 進捗（2026-02-23 12:14 JST時点）:
    - ログ最終更新は `2026-02-22 22:59 JST` の `Best result 10935.76 produced at generation 0. Next generation 2`
    - `metatester64` CPU合計は増加継続（例: `573882.45 -> 573959.41` / 6秒）
  - 備考:
    - `terminal64` と `metatester64 x16` の稼働を確認済み（計算継続中）
    - 監視コマンド: `tools/mt5_watch_optimization.sh --report-prefix ih_opt_full_rerun_20260222_181920`

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
  - run_id を明示した再実行例:
    - `XAU_AUTOBOT_TRIAL_RUN_ID=trial_v2_20260223_070000 tools/xau_autobot_trial_v2_start.sh`
    - `XAU_AUTOBOT_TRIAL_RUN_ID=trial_v2_20260223_070000 tools/xau_autobot_trial_v2_eval.sh`
- 判定コマンド:
  - `./.venv/bin/python tools/xau_autobot_trial_judge.py --reports-dir data/reports --min-days 14 --min-closed-positions 30 --min-profit-factor 1.10 --min-win-rate 0.42 --min-net-profit 0 --fail-on-no-go`
- 2026-02-22 判定結果（既存live report基準）:
  - 出力: `data/reports/xau_autobot_trial_judge.json`
  - verdict: `NO_GO`
  - failed_checks: `closed_positions`, `profit_factor`, `win_rate`, `net_profit`
- 2026-02-22 Trial V2 実運用開始（run_id固定）:
  - run_id: `trial_v2_20260222_143932`
  - 開始: `2026-02-22T14:39:32.973910+00:00`（`2026-02-22 23:39:32 JST`）
  - trial config: `tools/configs/xau_autobot.trial_v2_20260222.json`
  - 実行中プロセス: `xau_autobot_live_loop.ps1 ... xau_autobot.trial_v2_20260222.json ... -Live`
  - 即時評価（疎通確認）: `INVALID_TRIAL`（`after_magic_filter=0`, `after_comment_prefix_filter=0`）
  - 最終評価予定: `2026-03-08T14:39:32+00:00`（`2026-03-08 23:39:32 JST`）以降に同一 run_id で `tools/xau_autobot_trial_v2_eval.sh`
- 2026-02-22 実装追補（trial成立性の明示）:
  - `tools/xau_autobot_trial_judge.py`
    - verdict を `GO/NO_GO/INVALID_TRIAL` の3値化
    - `diagnostics.after_magic_filter` / `after_comment_prefix_filter` が 0 の場合は `INVALID_TRIAL`
    - 出力項目を追加: `trial_valid`, `invalid_reasons`
  - `tools/xau_autobot_trial_v2_start.sh`
    - 起動前 preflight を追加（`xau_autobot.py` / `xau_autobot_live_loop.ps1` 既存稼働を検知して fail-fast）
    - `flock` ベースの排他ロックを追加（fallback: `mkdir` ロック）
    - 強制実行フラグ: `XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES=1`
  - `tools/xau_autobot_promote_best.py`
    - live report 同定を `magic/comment_prefix` 一致必須へ強化
    - period config 群から期待 identity を推定し、不一致レポートは除外
    - 一致レポートが0件の場合は fail-closed（promotion中止）
  - `tools/xau_autobot_cycle_compare.py` / `tools/xau_autobot_cycle_runner.sh`
    - `market_closed` 通知の連投抑止（既定24時間cooldown）を追加
    - notify state を `data/reports/xau_autobot_cycle_compare_notify_state.json` に保存
    - runner から `XAU_AUTOBOT_SKIP_NOTIFY_*` 環境変数で cooldown/state path を制御可能化
  - `tools/xau_autobot_trial_v2_start.sh` / `tools/xau_autobot_trial_v2_eval.sh`
    - `run_id` ベース成果物保存を追加（`xau_autobot_live_report_<run_id>.json`, `xau_autobot_trial_judge_<run_id>.json`）
    - start が `data/reports/xau_autobot_trial_v2_current_run.json` へ run metadata を保存（初回 start 実行後に生成）
    - eval は metadata から run_id を解決し、`*_latest.json` へもコピー更新
    - `NO_GO/INVALID_TRIAL`（exit!=0）でも `*_latest.json` は更新した上で終了コードを返す（2026-02-22運用修正）
    - eval の観測窓は `run_meta.started_at_utc` 起点で固定（`start=started_at_utc`, `end=min(start+14d, now)`）。満了前の途中評価は `window_days` 未達で失敗しうる（2026-02-22運用修正）

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
- A5（B1R第2波 / player別ミックス）は完走したが未達:
  - 参照:
    - `data/reports/armada_b1_seed_sweep_20260222_fix2_summary.json`（ON）
    - `data/reports/armada_b1_seed_sweep_20260222_fix2_holdoff_summary.json`（kojirin holdoff）
    - `data/reports/armada_b1r_fix2_hold_mode_evaluation_20260222.json`
  - 集計:
    - ON: `taiki=2/5`, `kojirin=1/5`, `both_players_pass=1/5`
    - holdoff/nohold: `taiki=2/5`, `kojirin=4/5`, `both_players_pass=1/5`
  - 判定: `kojirin` は改善したが、両者同時passは `1/5` のままで `b1r_completed=false`。
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
  - 2026-02-22 実行メモ:
    - 実行済み（hold filter ON, `indicators=vwapvr,vwap`, `candidates_per_player=120`）:
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed11_c120_vwapvr_vwap_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed23_c120_vwapvr_vwap_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed47_c120_vwapvr_vwap_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed83_c120_vwapvr_vwap_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed131_c120_vwapvr_vwap_top3.json`
    - 集計:
      - `data/reports/armada_b1_seed_sweep_20260222_taiki_summary.json`
      - `player_pass_counts={taiki:2}`, `both_players_pass_count=2/5`, `b1r_completed=false`
    - 観測:
      - `seed=23` で `top3_oos_ok=2/3`, `seed=131` で `1/3`、その他 seed は `0/3`
      - top3 指標は全seedで `vwapvr` に固定（探索分散が不足）
  - 2026-02-23 追加実行メモ（hold_tf_filter OFF, 進行中）:
    - 実行:
      - `taiki`: `--indicators vwapvr,vwap --disable-hold-tf-filter`
      - 共通: `candidates_per_player=120`, `top_per_player=3`, `seed={11,23,47,83,131}`
    - 出力先:
      - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_seed*_c120_vwapvr_vwap_holdoff_top3.json`
      - `logs/armada_b1r_fix3_taiki_seed*_c120_vwapvr_vwap_holdoff_20260223.log`
    - 状況（2026-02-23 01:44時点）:
      - `seed11` 完了
      - `seed23` 実行中（`seed47/83/131` 待機）

- [x] **V50.7-A0K kojirin専用 B1Rトラック**
  - 目的: `kojirin` の pass rate を `>=4/5` まで引き上げる（taiki と分離して最適化）。
  - 完了条件: 5 seed 中 `4/5` 以上で `top3 oos_ok>=1`。
  - 実行方針:
    - `--players kojirin` の単独 sweep で実行。
    - kojirin で相性の良い指標を優先（`vwapvr/volsma/rsi` を段階探索）。
  - 成果物:
    - `data/reports/armada_b1_seed_sweep_YYYYMMDD_kojirin_summary.json`
  - 2026-02-22 実行メモ（monitor回収）:
    - 実行済み（hold filter ON, `indicators=vwapvr,volsma`, `candidates_per_player=120`）:
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed11_c120_vwapvr_volsma_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed23_c120_vwapvr_volsma_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed47_c120_vwapvr_volsma_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed83_c120_vwapvr_volsma_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed131_c120_vwapvr_volsma_top3.json`
    - 集計:
      - `data/reports/armada_b1_seed_sweep_20260222_kojirin_summary.json`
      - `player_pass_counts={kojirin:1}`, `both_players_pass_count=1/5`, `b1r_completed=false`
    - 観測:
      - `seed=131` のみ `top3_oos_ok=1/3`、他 seed は `0/3`
      - top3 指標は大半が `volsma` に偏り、`vwapvr` の上位残存が限定的
  - 2026-02-22 追加実行メモ（hold_tf_filter OFF）:
    - 生成:
      - `data/reports/armada_b1_fix2_kojirin_seed_sweep_20260222_holdoff_summary.json`
      - `data/reports/armada_b1_fix2_kojirin_seed_sweep_20260222_nohold_summary.json`
    - 結果:
      - `player_pass_counts={kojirin:4}`, `both_players_pass_count=4/5`, `b1r_completed=true`
    - 判定: A0K完了（kojirin単独トラックの `>=4/5` 条件を達成）。

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
  - 2026-02-22 実行メモ（A5-1 / hold_tf_filter=ON）:
    - 実行:
      - `taiki`: `indicators=vwapvr,vwap`
      - `kojirin`: `indicators=vwapvr,volsma`
      - 共通: `candidates_per_player=120`, `top_per_player=3`, `cpcv_folds=5`, `oos_min_trades_abs=50`
    - 生成:
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_seed*_c120_vwapvr_vwap_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed*_c120_vwapvr_volsma_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_taiki_kojirin_seed*_c120_mix_top3.json`
      - `data/reports/armada_b1_seed_sweep_20260222_fix2_summary.json`
      - `data/reports/armada_b1r_fix2_evaluation_20260222.json`
    - 結果:
      - `player_pass_counts={taiki:2, kojirin:1}`
      - `both_players_pass_count=1/5`（A4比 `+1`）
      - `b1r_completed=false`
    - 判定: 両者同時passは改善したが、目標 `>=4/5` には未達。A5-2（`kojirin disable_hold_tf_filter=OFF` 比較）へ継続。
    - 補完実行:
      - `kojirin seed=131` を同条件で再実行し、`data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed131_c120_vwapvr_volsma_top3.json` を生成（`logs/armada_b1r_fix2_kojirin_seed131_c120_vwapvr_volsma_20260222.log`）。
  - 2026-02-22 実行メモ（A5-2 / hold_tf_filter=OFF 比較, 完了）:
    - 実行:
      - `kojirin`: `--indicators vwapvr,volsma --disable-hold-tf-filter`
      - 共通: `candidates_per_player=120`, `top_per_player=3`, `seed={11,23,47,83,131}`
      - 比較系列: `holdoff` / `nohold` の2系列で5-seed完走
    - 生成:
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed*_c120_vwapvr_volsma_holdoff_top3.json`
      - `data/reports/armada_player_replica_20260222_b1r_fix2_kojirin_seed*_c120_vwapvr_volsma_nohold_top3.json`
      - `data/reports/armada_b1_fix2_kojirin_seed_sweep_20260222_holdoff_summary.json`
      - `data/reports/armada_b1_fix2_kojirin_seed_sweep_20260222_nohold_summary.json`
      - `data/reports/armada_b1_fix2_kojirin_holdoff_comparison_20260222.json`
      - `data/reports/armada_b1_fix2_kojirin_holdoff_vs_nohold_comparison_20260222.json`
      - `data/reports/armada_b1_seed_sweep_20260222_fix2_holdoff_summary.json`
      - `data/reports/armada_b1_seed_sweep_20260222_fix2_nohold_summary.json`
      - `data/reports/armada_b1r_fix2_hold_mode_evaluation_20260222.json`
    - 結果:
      - `kojirin`単体:
        - ON(`fix2`): `1/5`
        - OFF(`holdoff`): `4/5`
        - OFF(`nohold`): `4/5`
        - 改善量: `+3` seed（ON比）
      - OFF系列同士比較（`holdoff` vs `nohold`）:
        - `completed_seed_count=5`
        - `holdoff_better=0`, `nohold_better=0`, `same=5`
        - gate pass数も同一（`4/5`）
      - 両者同時pass（`taiki+kojirin`）:
        - ON: `1/5`
        - OFF(`holdoff`): `1/5`
        - OFF(`nohold`): `1/5`
      - seed重なり:
        - `taiki gate_pass`: `{23,131}`
        - `kojirin gate_pass (OFF)`: `{11,23,47,83}`
        - overlap: `{23}` のみ
    - 判定:
      - A5-2は完走したが、A5完了条件（`both_players_pass_count>=4/5`）は未達。
      - ボトルネックは `kojirin` 単体の弱さではなく、`taiki` と `kojirin` の seed重なり不足。
  - 2026-02-23 実行メモ（A5-3 / taiki holdoff refresh）:
    - 実行:
      - `taiki`: `--indicators vwapvr,vwap --disable-hold-tf-filter`
      - 共通: `candidates_per_player=120`, `top_per_player=3`, `seed={11,23,47,83,131}`
    - 生成:
      - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_seed*_c120_vwapvr_vwap_holdoff_top3.json`
      - `data/reports/armada_b1_seed_sweep_20260223_taiki_fix3_holdoff_summary.json`
      - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_kojirin_seed*_c120_holdoff_mix_top3.json`
      - `data/reports/armada_b1_seed_sweep_20260223_fix3_holdoff_summary.json`
      - `data/reports/armada_b1r_fix3_taiki_holdoff_evaluation_20260223.json`
    - 結果:
      - `taiki gate_pass=3/5`（`11,83,131`）
      - `kojirin gate_pass=4/5`（`11,23,47,83`、A5-2 holdoff固定）
      - `both_players_pass_count=2/5`（`11,83`）
      - baseline（`data/reports/armada_b1_seed_sweep_20260222_fix2_holdoff_summary.json`）比:
        - `delta={taiki:+1, kojirin:+0, both_players_pass_count:+1}`
    - 判定:
      - 改善は確認できたが、A5完了条件（`>=4/5`）には未達。
      - 次の主対象は `seed={23,47}` の `taiki` 側ボトルネック解消。

- [x] **V50.7-A6 Seed重なり是正（taiki holdoff / 第3波）**
  - 目的: A5で露呈した seed重なり不足（`taiki={23,131}` vs `kojirin={11,23,47,83}`）を是正し、`both_players_pass_count` を引き上げる。
  - 完了条件:
    - `taiki` 側 holdoff sweep（`seed={11,23,47,83,131}`）を完走し、pass seed集合を確定。
    - `kojirin holdoff`（A5-2）との seed overlap を再計算し、`both_players_pass_count` を再評価。
  - 実行方針:
    - `taiki`: `indicators=vwapvr,vwap`, `disable_hold_tf_filter=true`, `candidates_per_player=120`。
    - まず taiki 側だけを再評価し、重なり不足が継続する場合のみ indicator 軸を追加する（`volsma` 等）。
  - 成果物:
    - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_seed*_c120_vwapvr_vwap_holdoff_top3.json`
    - `data/reports/armada_b1_fix3_taiki_seed_sweep_20260223_holdoff_summary.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_kojirin_seed*_c120_mix_holdoff_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix3_taiki_kojirin_seed*_c120_mix_nohold_top3.json`
    - `data/reports/armada_b1_seed_sweep_20260223_fix3_holdoff_summary.json`
    - `data/reports/armada_b1_seed_sweep_20260223_fix3_nohold_summary.json`
    - `data/reports/armada_b1r_fix3_hold_mode_evaluation_20260223.json`
    - `data/reports/armada_b1_seed_sweep_20260223_fix3_taiki_holdoff_interim_summary.json`
    - `data/reports/armada_b1r_fix3_overlap_evaluation_20260223_interim.json`
    - `data/reports/armada_b1r_fix3_feasibility_cutoff_20260223.json`
  - 2026-02-23 実行メモ（完了）:
    - 実行コマンド:
      - `python3 tools/ops/armada_player_replica.py --players taiki --indicators vwapvr,vwap --candidates-per-player 120 --top-per-player 3 --seed {11,23,47,83,131} --oos-min-trades-abs 50 --oos-trade-ratio-floor 0.35 --cpcv-folds 5 --cpcv-require-for-core --disable-hold-tf-filter --output data/reports/armada_player_replica_20260223_b1r_fix3_taiki_seed*_c120_vwapvr_vwap_holdoff_top3.json`
    - 補完メモ:
      - 並列実行中に `seed=83/131` が欠損したため、再実行で回収。
      - 参照ログ:
        - `logs/armada_b1r_fix3_taiki_seed83_c120_vwapvr_vwap_holdoff_20260223_rerun.log`
        - `logs/armada_b1r_fix3_taiki_seed131_c120_vwapvr_vwap_holdoff_20260223_rerun.log`
    - 観測:
      - `taiki_pass_seeds={11,83,131}`（`3/5`）
      - `kojirin_holdoff_pass_seeds={11,23,47,83}`（`4/5`）
      - `overlap={11,83}`（`both_players_pass_count=2/5`）
      - hold mode別の両者同時pass:
        - `fix2_holdoff=1/5`
        - `fix3_holdoff=2/5`
        - `fix3_nohold=2/5`
    - 判定:
      - A6は完走し、`both_players_pass_count` を `1/5 -> 2/5` へ改善。
      - ただし完了条件（`>=4/5`）は未達のため、A7を継続。

- [ ] **V50.7-A7 taiki条件切替（overlap到達可能性の再構成）**
  - 目的: `taiki` の pass seed を `kojirin_holdoff_pass={11,23,47,83}` に重ね、`both_players_pass_count>=4/5` の到達可能性を復元する。
  - 完了条件:
    - 新条件で `taiki` を `seed={11,23,47,83,131}` 再探索し、`taiki_pass_seeds` と overlap 上限を再評価。
    - `max_possible_overlap >= 4` を満たす条件を1本以上確保する。
  - 実行方針:
    - 指標軸を拡張（`vwapvr,vwap,volsma` と `vwapvr,rsi`）して `taiki` 単独 sweep。
    - まず `seed={23,47,83}` の targeted run で overlap 改善余地を先に確認し、成立条件のみ 5seed へ展開。
  - 成果物:
    - `data/reports/armada_player_replica_20260223_b1r_fix4a_taiki_seed*_c120_vwapvr_vwap_volsma_holdoff_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix4b_taiki_seed*_c120_vwapvr_rsi_holdoff_top3.json`
    - `data/reports/armada_b1r_fix4_overlap_feasibility_20260223.json`
  - 2026-02-23 実行メモ（targeted完了）:
    - fix4a（axis1: `vwapvr,vwap,volsma`）:
      - `seed={23,47,83}` を実行
      - `taiki_pass_tested_seeds={23}`
      - `taiki_fail_tested_seeds={47,83}`
    - fix4b（axis2: `vwapvr,rsi`）:
      - `seed={23,47,83}` を実行
      - `taiki_pass_tested_seeds={23,83}`
      - `taiki_fail_tested_seeds={47}`
    - 到達可能性評価:
      - `data/reports/armada_b1r_fix4_overlap_feasibility_20260223.json`
      - `kojirin_holdoff_pass_seeds={11,23,47,83}` を固定した場合、fix4a/fix4bともに `seed=47` がfail。
      - overlap上限は `max_possible_overlap=3/5` で、完了条件 `>=4/5` は未到達。
    - 判定:
      - targeted 2軸とも到達可能性を回復できず、A7完了条件は未達。
      - 次段は「taiki生成軸の再設計」または「kojirin側seed集合の再構成」を伴う追加設計が必要。
  - 2026-02-23 追加検証（fix4b full 5seed）:
    - 追加実行:
      - `data/reports/armada_player_replica_20260223_b1r_fix4b_taiki_seed11_c120_vwapvr_rsi_holdoff_top3.json`
      - `data/reports/armada_player_replica_20260223_b1r_fix4b_taiki_seed131_c120_vwapvr_rsi_holdoff_top3.json`
      - （既存targeted分）`seed=23,47,83` を合算して5seed化
    - 集計:
      - `data/reports/armada_b1_fix4b_taiki_seed_sweep_20260223_holdoff_summary.json`
      - `data/reports/armada_b1_seed_sweep_20260223_fix4b_mix_holdoff_summary.json`
      - `data/reports/armada_b1r_fix4b_overlap_evaluation_20260223.json`
    - 結果:
      - `taiki_pass_seeds={23,83}`（`2/5`）
      - `kojirin_holdoff_pass_seeds={11,23,47,83}`（`4/5`）
      - `overlap={23,83}`（`2/5`）
      - `delta_fix4b_vs_fix3_both_players_pass_count=0`
    - 判定:
      - full5seedでも `both_players_pass_count=2/5` で不変。
      - A7完了条件（`>=4/5`）は継続未達。

- [ ] **V50.7-A8 kojirin側 seed集合再構成（131回復軸）**
  - 目的: `kojirin` の gate pass seed を `131` へ拡張し、`taiki` 側で通る seed との overlap 上限を引き上げる。
  - 完了条件:
    - `kojirin` の再探索条件で `seed=131` を gate pass 化する。
    - かつ `seed={11,23,83}` の pass を極力維持し、overlap上限を `>=4/5` に戻す見込みを確認する。
  - 実行方針:
    - holdoff維持（`disable_hold_tf_filter=true`）のまま indicator 軸を再探索:
      - axis1: `vwapvr,volsma,rsi`
      - axis2: `vwapvr,vwap,volsma`
    - まず targeted（`seed={131,23,83}`）で可否確認し、成立軸のみ 5seed 展開。
  - 成果物（予定）:
    - `data/reports/armada_player_replica_20260223_b1r_fix5a_kojirin_seed*_*.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed*_*.json`
    - `data/reports/armada_b1r_fix5_overlap_feasibility_20260223.json`
  - 2026-02-23 実行メモ（完了）:
    - fix5a（axis1: `vwapvr,volsma,rsi`, targeted `seed={131,23,83}`）:
      - 実行:
        - `data/reports/armada_player_replica_20260223_b1r_fix5a_kojirin_seed131_c120_vwapvr_volsma_rsi_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5a_kojirin_seed23_c120_vwapvr_volsma_rsi_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5a_kojirin_seed83_c120_vwapvr_volsma_rsi_holdoff_top3.json`
      - 結果:
        - `seed131: gate_pass=false`
        - `seed23: gate_pass=true`
        - `seed83: gate_pass=false`
      - 判定: `131`回復に失敗（axis1は不採用）。
    - fix5b（axis2: `vwapvr,vwap,volsma`）:
      - 5seed完走:
        - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed11_c120_vwapvr_vwap_volsma_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed23_c120_vwapvr_vwap_volsma_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed47_c120_vwapvr_vwap_volsma_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed83_c120_vwapvr_vwap_volsma_holdoff_top3.json`
        - `data/reports/armada_player_replica_20260223_b1r_fix5b_kojirin_seed131_c120_vwapvr_vwap_volsma_holdoff_top3.json`
      - 結果:
        - `kojirin_fix5b_pass_seeds={47,83,131}`（`3/5`）
        - `seed131` は回復したが `seed11/23` はfail
    - 集計:
      - `data/reports/armada_b1_fix5b_kojirin_seed_sweep_20260223_holdoff_summary.json`
      - `data/reports/armada_b1r_fix5b_overlap_evaluation_20260223.json`
      - `data/reports/armada_b1r_fix5_overlap_feasibility_20260223.json`
      - `taiki_pass_seeds={11,83,131}`（A6/fix3）
      - overlapは `{83,131}`（`both_players_pass_count=2/5`）
      - `max_possible_overlap=3/5`（目標 `>=4/5` に未達）
      - `required_anchor={11,23,83}` の維持は `{83}` のみ
    - 判定:
      - A8の主眼だった `seed131` 回復は達成。
      - ただし `seed11/23` を維持できず、重なり上限が `3/5` のため A8完了条件は未達。

- [ ] **V50.7-A9 taiki seed47 直撃探索（高探索量）**
  - 目的: A7/A8で残存した主ボトルネック `taiki seed47` の gate fail を単独で崩す。
  - 完了条件:
    - `seed47` で `top3_oos_ok>=1` を達成する条件を1本以上確保。
  - 実行方針:
    - `players=taiki`, `seed=47`, `disable_hold_tf_filter=true` 固定。
    - indicator 制約を外し（all indicators）、`candidates_per_player=480` へ拡張して探索密度を上げる。
  - 成果物:
    - `data/reports/armada_player_replica_20260223_b1r_fix6a_taiki_seed47_c480_allind_holdoff_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix6b_taiki_seed47_c3216_allind_holdoff_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix6c_taiki_seed47_c804_allind_holdon_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix6d_taiki_seed47_c804_allind_holdoff_selrelaxed_top3.json`
    - `data/reports/armada_player_replica_20260223_b1r_fix7a_taiki_seed47_c240_vwapvr_rsi_ema_holdoff_top3.json`
  - 2026-02-23 実行メモ（進行中）:
    - fix6a（`candidates_per_player=480`）:
      - `data/reports/armada_player_replica_20260223_b1r_fix6a_taiki_seed47_c480_allind_holdoff_top3.json`
      - 結果: `top3_oos_ok=0`（gate未達）
    - fix6b（`candidates_per_player=3216` / 全候補）:
      - 実行中（再開済み）
      - 出力先: `data/reports/armada_player_replica_20260223_b1r_fix6b_taiki_seed47_c3216_allind_holdoff_top3.json`
      - 監視ログ: `logs/armada_b1r_fix6b_taiki_seed47_c3216_allind_holdoff_20260223.log`
    - fix6c（`candidates_per_player=804` / all indicators, holdon）:
      - 完了
      - 出力先: `data/reports/armada_player_replica_20260223_b1r_fix6c_taiki_seed47_c804_allind_holdon_top3.json`
      - 結果: `top3_oos_ok=0`（gate未達）
    - fix7a（`candidates_per_player=240` / `indicators=vwapvr,rsi,ema`, holdoff）:
      - 完了
      - 出力先: `data/reports/armada_player_replica_20260223_b1r_fix7a_taiki_seed47_c240_vwapvr_rsi_ema_holdoff_top3.json`
      - 結果: `top3_oos_ok=0`（gate未達）
    - fix6d（`candidates_per_player=804` / all indicators, holdoff, selection-relaxed）:
      - 実行中（2026-02-23 13:21 JST 開始）
      - 出力先: `data/reports/armada_player_replica_20260223_b1r_fix6d_taiki_seed47_c804_allind_holdoff_selrelaxed_top3.json`
      - 監視ログ: `logs/armada_b1r_fix6d_taiki_seed47_c804_allind_holdoff_selrelaxed_20260223.log`
    - 2026-02-23 13:23 JST 進捗スナップショット:
      - fix6b: `output未生成`, `log_bytes=259303`（増加継続）
      - fix6d: `output未生成`, `log_bytes=4391`（増加継続）

---

## 2026-02-23 運用追補: 3通貨 Founder（Hunted VWAPVR）安定化

- 対象 founder:
  - `Hunted-H12-VWAPVR-50-150-USDJPY`
  - `Hunted-D1-VWAPVR-50-220-EURUSD`
  - `Hunted-D1-VWAPVR-80-180-GBPUSD`

- 事象:
  - `GBPUSD` founder が `Phase1 Screening Passed` 後に一時 `:B` へ昇格しても、DB が `:GRAVEYARD` に戻るケースが再発。
  - `A/B conformance` 側では `B->Graveyard=0` のままでも rank 後退が起きるため、明示demoteではなく stale object upsert の上書きを疑う。

- 根因（確定）:
  - `upsert-strategy` の rank回帰ガードは `active -> lower active`（例: `:A -> :B`）は防いでいたが、
    `active -> archive`（例: `:B -> :GRAVEYARD`）を stale in-memory object から上書きできる穴が残っていた。

- 実装修正:
  - `src/lisp/school/school-db.lisp`
    - `upsert-strategy` の回帰ガードを拡張し、`db-level(active)` かつ `incoming-archive` の場合も
      `*allow-rank-regression-write*` が `nil` なら DB rank を保持するよう修正。

- テスト追加:
  - `src/lisp/tests/backtest-db-tests.lisp`
    - `test-upsert-preserves-active-rank-when-incoming-archive`
      - stale archive upsert で active rank が落ちないことを検証。
    - `test-upsert-allows-explicit-active-to-archive-regression`
      - `*allow-rank-regression-write*=t` の明示経路では active->archive を許可することを検証。

- 検証結果（2026-02-23）:
  - rank回帰系テスト 7件:
    - `passed=7 failed=0`
  - 実運用ログ:
    - `Hunted-D1-VWAPVR-80-180-GBPUSD` が `GRAVEYARD -> B (Phase1 Screening Passed (V2))` を記録。
    - 以後 `A/B conformance sweep: ... B->Graveyard=0` 継続。
  - DBスナップショット:
    - 3 founder 全て `:B` を確認。

- 運用判定:
  - 3通貨 founder の「昇格後即落ち」症状は解消。
  - 次運用は `B=3 / Graveyard=0` の継続監視（短期）を実施し、再発がなければ本修正を確定扱いとする。

- 2026-02-23 短期監視ジョブ起動（JST 12:13）:
  - systemd unit:
    - `founder_rank_watch_20260223_121322.service`
  - 監視条件:
    - 監視対象: 上記3 founder 固定
    - 5分間隔 (`interval_sec=300`) x 288サンプル（24時間）
    - 逸脱条件: `count_b != 3` または `count_graveyard != 0` または founder missing
  - 出力:
    - JSONL: `data/reports/founder_rank_watch_20260223_121322.jsonl`
    - runtime log: `data/runtime/founder_rank_watch_20260223_121322.out`
    - current unit marker: `data/runtime/founder_rank_watch_current.unit`
  - 初回サンプル:
    - `sample 1/288: b=3 gy=0 drift=False missing=0`
  - 参照/停止コマンド:
    - `systemctl --user status founder_rank_watch_20260223_121322.service --no-pager`
    - `tail -f data/reports/founder_rank_watch_20260223_121322.jsonl`
    - `systemctl --user stop founder_rank_watch_20260223_121322.service`
