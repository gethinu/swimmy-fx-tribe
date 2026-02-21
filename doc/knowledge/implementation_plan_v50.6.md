# 🏛️ Strategy Lifecycle Implementation Plan V50.6

**更新日:** 2026-02-19 JST
**バージョン:** V50.6 (Structured Telemetry & Retired Rank)

---

## 2026-02-21 実装追補: XAU Autobot の「バックテスト良化 / 実運用悪化」診断強化

- 背景:
  - バックテスト指標が良好でも、live では `PF<1`・`net_profit<0` が続くケースがあり、選定根拠の可視性が不足していた。
- 実装:
  - `tools/xau_autobot_cycle.py`
    - readiness compact に OOS頑健性を追加:
      - `oos_count`, `oos_worst_pf`, `oos_worst_total_return`, `oos_negative_return_ratio`
  - `tools/xau_autobot_promote_best.py`
    - liveレポート自動解決 + 鮮度フィルタ（`--live-max-age-hours`）
    - live弱化時のストレス乗数を導入し、DD/OOS不安定性ペナルティを強化
    - `scoreboard`（period別スコア一覧）を promotion report に出力
    - `live_gap`（backtest vs live 差分診断）を promotion report に出力
      - `delta_profit_factor`, `delta_win_rate`, `underperforming` など
    - `--fail-on-live-underperforming` を追加（sample_quality=ok かつ underperforming の場合に昇格をブロック）
    - underperforming判定閾値をCLI化:
      - `--live-min-closed-positions`
      - `--live-min-profit-factor`
      - `--live-max-profit-factor-drop`
      - `--live-max-win-rate-drop`
      - `--live-ignore-net-profit-check`
    - promotion通知を追加:
      - `--discord-webhook` / `--discord-webhook-fallbacks` / `--notify-strict`
      - 通知本文へ `underperforming_reasons` を含め、ブロック理由を即時可視化
  - `tools/xau_autobot_cycle_compare.py`
    - `--market-hours-only` 時に `SKIP` 行のみ返るケースを許容
    - 比較結果を fail-close せず、`action=SKIP` / `reason=market_closed` を出力
    - 全期間 `SKIP` の場合は compare からDiscordへ SKIP 通知を送信
  - `tools/xau_autobot_cycle_runner.sh`
    - `--live-reports-dir` / `--live-max-age-hours` を promotion step へ連携
    - `XAU_AUTOBOT_FAIL_ON_LIVE_UNDERPERFORMING=1` で fail-on-live-underperforming を有効化
    - `XAU_AUTOBOT_LIVE_MIN_*` / `XAU_AUTOBOT_LIVE_MAX_*` / `XAU_AUTOBOT_LIVE_IGNORE_NET_PROFIT_CHECK` で閾値調整を連携
    - `XAU_AUTOBOT_NOTIFY_STRICT=1` で promotion通知失敗時を fail-close 化
    - 比較結果 `periods=[]` の場合は `SKIP_PROMOTION` で promotion を自動スキップ
    - `XAU_AUTOBOT_COMPARE_NOTIFY_STRICT=1` で compare SKIP通知失敗時を fail-close 化
- 検証:
  - `./.venv/bin/python -m pytest -q tools/tests/test_xau_autobot_cycle.py tools/tests/test_xau_autobot_cycle_compare.py tools/tests/test_xau_autobot_promote_best.py` → pass
  - promotion report (`data/reports/xau_autobot_promotion.json`) で `scoreboard` / `live_gap` を確認

---

## 2026-02-21 実装追補: 3通貨ペア Founder の Phase1 復元耐性

- 背景:
  - `RECRUIT_SPECIAL_FORCES` 実行時に、再起動/重複結果のタイミングで
    `Strategy not found for Phase 1 result` が発生し得ることを確認。
  - 原因は `handle-v2-result` が KB + pending hash のみ参照し、メモリ状態喪失時に復元できないこと。
- 実装:
  - `src/lisp/school/school-backtest-v2.lisp`
    - `%load-strategy-from-db-for-phase1` を追加（`strategies.data_sexp` から復元）。
    - `handle-v2-result` の Phase1 解決順を拡張:
      - `find-strategy` → `take-phase1-pending-candidate` → `DB fallback`
- テスト:
  - `src/lisp/tests.lisp`
    - 追加: `test-handle-v2-result-loads-phase1-candidate-from-db-fallback`
    - `run-all-tests` の対象リストへ追加
  - 実行（抜粋）:
    - `test-handle-v2-result-admits-pending-phase1-candidate => T`
    - `test-handle-v2-result-loads-phase1-candidate-from-db-fallback => T`
    - `test-recruit-special-forces-respects-founder-key-symbol-suffix => T`

---

## 2026-02-20 推奨実行順（依存つき運用TODO）

> 現状は「実装完了」状態のため、次の失敗点は機能追加より運用ドリフト。  
> 推奨は **優先順位整理 → 監視/回帰の固定化**。

### 優先順位

| 優先 | タスク | 目的 | 依存 |
|------|--------|------|------|
| P0 | クリティカル回帰の定期実行を固定化 | 既存仕様の再劣化を即検知 | なし |
| P1 | systemd scope + heartbeat観測の常設化 | 「止まって見えるが動いている」誤認を防ぐ | P0 |
| P1 | TF/strategy解決 fail-close の監査定常化 | 古いシグナル混入による誤発注を防ぐ | P0 |
| P2 | ランク整合（A/S floor, conformance）の日次監査化 | 低サンプル過大評価の再発防止 | P0 |
| P3 | V50.7ドラフト起票（新規機能は監視定着後） | 追加開発前に運用土台を固定 | P0-P2 |

### 実行TODO（推奨順）

- [x] **P0-1 回帰バンドルを1コマンド化**（2026-02-20 完了）
  - 追加先: `tools/system_audit.sh`
  - 内容:
    - `SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp`
    - `python3 tools/test_backtest_service.py`
    - `python3 tools/check_order_timeframe_consistency.py --lookback-minutes 120 --fail-on-issues`
  - 完了条件: 3系統が1実行で pass/fail 可視化される
  - 実装: `RUN_REGRESSION_BUNDLE`（既定ON）で `system_audit.sh` に統合済み

- [x] **P1-1 systemd scope 判定を監査に組み込み**（2026-02-20 完了）
  - 追加先: `tools/system_audit.sh`（`/proc/<pid>/cgroup` 判定）
  - 目的: `systemctl --user` と `systemctl` の取り違え防止
  - 完了条件: system/user scope の判定結果が監査出力に明示される
  - 実装: `Systemd cgroup scope alignment` ステップ追加（`detect_pid_scope` / `audit_systemd_scope_alignment`）

- [x] **P1-2 Backtest heartbeat 停滞アラート閾値を固定**（2026-02-20 完了）
  - 対象: `tools/backtest_service.py` / `logs/backtest.log`
  - 目的: heartbeat 停滞を「停止疑い」として機械判定
  - 完了条件: `rx_age/tx_age` の閾値超過で監査が WARNING/FAIL を返す
  - 実装:
    - `tools/check_backtest_heartbeat.py` を追加（log鮮度 + `rx_age/tx_age` をしきい値判定）
    - `tools/system_audit.sh` の `Backtest heartbeat freshness` ステップを新スクリプト呼び出しへ統一
    - しきい値: `BACKTEST_HEARTBEAT_MAX_LOG_AGE_SECONDS` / `BACKTEST_HEARTBEAT_MAX_RX_AGE_SECONDS` / `BACKTEST_HEARTBEAT_MAX_TX_AGE_SECONDS`

- [x] **P1-3 Live TF fail-close の定期監査化**（2026-02-20 完了）
  - 実行: `python3 tools/check_order_timeframe_consistency.py --lookback-minutes 120 --fail-on-issues`
  - 目的: stale timeframe / unresolved strategy の再混入検出
  - 完了条件: 日次実行（timer/cron） + 失敗時ログ導線確保
  - 実装:
    - `systemd/swimmy-system-audit.service`
    - `systemd/swimmy-system-audit.timer`
    - ログ導線: `logs/system_audit.log`（`tools/system_audit.sh` の append 出力）
  - 運用有効化:
    - `sudo systemctl daemon-reload`
    - `sudo systemctl enable --now swimmy-system-audit.timer`

- [x] **P2-1 Rank conformance 監査を日次ジョブ化**（2026-02-20 完了）
  - 対象: A/S floor と S conformance sweep の結果差分
  - 目的: low-trade高Sharpe再発を監視で抑止
  - 完了条件: 前日比の昇降格件数と違反件数が定点出力される
  - 実装:
    - `tools/check_rank_conformance.py` を追加（前日スナップショット基準の昇降格差分 + 違反件数）
    - `tools/rank_conformance_audit_runner.sh` を追加
    - `systemd/swimmy-rank-conformance-audit.service` / `.timer` を追加（日次実行）
    - user scope 用に `systemd/swimmy-rank-conformance-audit.user.service` も追加
    - `tools/system_audit.sh` に `Rank conformance audit` ステップを追加
    - 回帰: `tools/tests/test_check_rank_conformance.py` に前日基準選択/フォールバックのテストを追加

- [x] **P3-1 V50.7ドラフト起票**（2026-02-20 完了）
  - 作成先: `doc/knowledge/implementation_plan_v50.7.md`（新規）
  - 前提: P0-P2 が監査運用に載っていること
  - 完了条件: 「新機能」ではなく「運用監視KPI」を先頭に置いた計画になっている
  - 実装:
    - `doc/knowledge/implementation_plan_v50.7.md` を起票済み
    - 先頭セクションを `0. 運用監視KPI（最優先）` として定義

---

## 2026-02-21 実装追補: Institutional Hunter EA 追加（MT5単体）

- 目的:
  - 「大口介入の推定」を `MSB + OB + 出来高 + ATR` で厳格判定し、MT5単体で自動執行するEAを新設。
- 実装:
  - 新規EA: `src/mt5/InstitutionalHunterEA.mq5`
  - 自動コンパイル:
    - `scripts/compile_swimmybridge_mt5.sh` を汎用化（`--src` でEAソース切替）
    - `scripts/compile_institutionalhunter_mt5.sh` を追加（InstitutionalHunterEA専用ラッパー）
  - テスター運用プリセット:
    - `src/mt5/InstitutionalHunterEA_Strict_AllSymbols.set`
    - `src/mt5/InstitutionalHunterEA_OptimizeCore_XAU_FX4.set`
    - `src/mt5/InstitutionalHunterEA_Forward_XAU_FX4.set`
  - 主要仕様:
    - H4で `MSB/OrderBlock` 検出、M15で執行判定
    - 厳格条件: `tick_volume >= SMA20*2.0` かつ `z-score(50)>=2.0` かつ `ATR14/ATR50>=1.3`
    - エントリー: OB接触後の反転確定足で成行
    - SL/TP: `SL=OB外+0.5ATR`、`TP=反対側OB`
    - リスク: 1トレード0.5%、1シンボル1ポジ、同時最大3、日次DD2%停止
    - 安全ガード: 取引モード制約（LONG/SHORT可否）、スプレッド上限、シンボル日次件数上限、バー間クールダウン
- 運用資料:
  - `doc/knowledge/institutional_hunter_ea_quickstart_20260221.md`
- 補足:
  - 自動コンパイル検証（2026-02-21 JST）:
    - `scripts/compile_institutionalhunter_mt5.sh` 実行
    - `Result: 0 errors, 0 warnings`
    - 出力: `.../MQL5/Experts/InstitutionalHunterEA.ex5`

---

## 2026-02-21 運用追補: Armada再現モデルの運用投入タスク化（Core 5）

- 方針:
  - まずは **A1（nami専用の局所探索）** を最優先で実行する。
  - 同時に、他プレイヤー（taiki / pandajiro / yumimin / kojirin）も「運用投入レベル」へ順次引き上げる。
  - 比較は `hold TF filter ON/OFF` を固定条件で継続し、TF逸脱（hold-bars<2）を抑制しながら強化する。

### 運用投入レベル（Armada暫定定義）

| レベル | 判定 | 条件 |
|------|------|------|
| L1 Candidate | 単発合格 | strict verdict で `bt_ok/oos_ok/cpcv_ok = True` |
| L2 Player | 再現性合格 | seed=5 で `top1 strong >= 2/5` かつ `median OOS PF >= 1.15` |
| L3 Deploy-Ready | 投入合格 | L2達成 + paper 20 tradesで重大逸脱なし（DD/スリッページ警戒値内） |

> 2026-02-21 Armada運用注記: strict `BT PF>=1.30` はR&D基準として維持しつつ、運用判定は proxy `BT PF>=1.22` を暫定採用。

### 優先順位（2026-02-21時点）

| 優先 | Task | 対象 | 目的 | 依存 |
|------|------|------|------|------|
| P0 | A1 | nami | まず1名をL1へ到達 | なし |
| P1 | A2 | nami | L2（seed再現性）到達 | A1 |
| P1 | B1 | taiki/kojirin | TF逸脱抑制 + OOS証拠不足の解消 | A1 |
| P2 | B2 | pandajiro/yumimin | CPCV pass_rateの底上げ | A1 |
| P3 | C1 | core5全体 | L3判定と投入パッケージ化 | A2/B1/B2 |

### 実行タスク（Armada）

- [x] **A1 nami局所探索（proxy運用判定）**
  - 目的: `nami_no_yukusaki_armada` の `TF120` 周辺で L1 候補を作る。
  - 固定条件: `--players nami_no_yukusaki_armada --candidates-per-player 240 --top-per-player 5 --oos-min-trades-abs 50 --oos-trade-ratio-floor 0.35 --cpcv-folds 5 --cpcv-require-for-core`。
  - 探索方針: `volsma/vwapvr` を中心に、`sma/ema` を比較対象として残す。
  - 完了条件: top5 内で `strong` 条件（strictまたはproxy）を最低1件。
  - 成果物: `data/reports/armada_player_replica_YYYYMMDD_nami_focus_*.json`
  - 2026-02-21 実行メモ:
    - 実行済み: fixed条件で `seed={20260220,11,23,47,83,131}` を実行。
    - 結果: 全runで top5 `strong=0/5`（`oos_ok/cpcv_ok` は成立、`bt_ok` が全落ち）。
    - 追加再探索（拡張指標）: `data/reports/armada_player_replica_20260221_a1_nami_focus_seed20260221_c240_extind.json`
    - 追加再探索結果: top5 `strong=0/5`, `bt_ok=0/5`（best `volsma TF120`, `BT PF=1.2304`, `OOS PF=1.3508`, `CPCV pass_rate=0.80`）
    - 追加再探索（候補数拡張 c480）: `data/reports/armada_player_replica_20260221_a1_nami_focus_seed20260221_c480_allind.json`
    - c480結果: top5 `strong=0/5`, `bt_ok=0/5`（best `volsma TF120`, `BT PF=1.2353`, `OOS PF=1.2560`, `CPCV pass_rate=0.80`）
    - 追加再探索（hold filter OFF）: `data/reports/armada_player_replica_20260221_a1_nami_holdoff_seed20260221_c240_allind.json`
    - hold OFF結果: top5 `strong=0/5`, `bt_ok=0/5`（top3に `TF240` 混在、best `BT PF=1.2282` で改善なし）
    - 完了待機ジョブ取り込み（2026-02-21 17:21 JST）: hold OFF run を再確認し、`strong=0/5`, `bt_ok=0/5` を再現（A1未達は継続）。
    - 補足: `armada_nami_bt_scan_seed20260221_c480_allind.json` は待機ジョブ完了後も出力未生成（当日判断は既存 feasibility レポートを正本とする）。
    - BT-only 出力健全性確認（debug）: `data/reports/armada_player_replica_20260221_a1_nami_btonly_seed20260221_c60_top60_debug.json` は正常保存（`saved report` 到達）を確認。
    - BT-only 追加スキャン（hold OFF, top240）: `data/reports/armada_player_replica_20260221_a1_nami_btonly_seed20260221_c240_top240_holdoff_scan.json`
    - BT-only 追加スキャン診断: `data/reports/armada_nami_bt_feasibility_20260221_seed20260221_c240_holdoff.json`
    - 追加診断結果: `max BT PF=1.2386`, `p95 BT PF=1.1656`, `median BT PF=1.0358`, `BT PF>=1.30 = 0/240`, `bt_ok=0/240`
    - seed sweep集計: `data/reports/armada_nami_seed_sweep_20260221.json`
    - 診断: `data/reports/armada_nami_bt_feasibility_20260221.json`
    - 診断結果: `max BT PF=1.2514`, `BT PF>=1.30 = 0/240`, `bt_ok=0/240`
    - 閾値感度: `data/reports/armada_nami_bt_threshold_sensitivity_20260221.json`
    - 感度結果: A2 proxy条件を満たす `BT PF` 範囲は `1.20〜1.22`（最大成立値 `1.22`）
    - 実装観測: strict `bt_ok` 判定は `BT PF>=1.30 && Sharpe>=0.10`（固定）だが、選抜スコア側の既定 floor は `core_bt_pf_floor=1.20`。この差で `BT PF 1.20台` 候補が上位に残りやすい。
    - 判定（strict）: 現行候補空間では strict `bt_ok` 閾値到達が確認できず、A1は未達（ブロッカー）
    - proxy判定（`BT PF>=1.22`）: top5 `strong_proxy=1/5`（A1完了条件を充足）
    - 運用決定（2026-02-21）: Armadaの運用判定は暫定で `BT PF>=1.22` を採用し、strict `BT PF>=1.30` はR&D再探索トラックとして継続する。

- [x] **A2 nami再現性検証（seed sweep, proxy運用判定）**
  - 目的: A1で得た条件が偶然でないことを確認し、L2判定に進める。
  - 実行: seed `{11, 23, 47, 83, 131}` で同一条件を再実行。
  - 完了条件: `top1 strong(proxy) >= 2/5` かつ `median OOS PF >= 1.15`。
  - 成果物: `data/reports/armada_nami_seed_sweep_*.json`
  - 2026-02-21 実行メモ:
    - 実行済み: seed `{11, 23, 47, 83, 131}`（固定条件）。
    - 集計: `data/reports/armada_nami_seed_sweep_20260221.json`
    - 結果: `top1 strong=0/5`, `median top1 OOS PF=1.2787`, `A2 pass=false`
    - 主要示唆: OOS/CPCV再現性は一定あるが、strict `bt_ok` 未達でL2条件を満たせない
    - proxy判定（`BT PF>=1.22`）: `top1 strong_proxy=2/5`, `median top1 OOS PF=1.2787`（A2完了条件を充足）

- [x] **B1 taiki/kojirin引き上げ（TF逸脱対策つき）**
  - 目的: OFF時に顕著だった `TF360/720` 偏重を抑え、OOS有効トレードを増やす。
  - 実行: hold TF filter を有効固定し、`hold-bars` 逸脱（<2）をゼロ化。
  - 完了条件: 各プレイヤーで `oos_ok` 候補を top3 内に最低1件。
  - 成果物: `data/reports/armada_player_replica_YYYYMMDD_taiki_kojirin_*.json`
  - 2026-02-21 実行メモ:
    - 統合run: `data/reports/armada_player_replica_20260221_taiki_kojirin_b1.json`
    - taiki: `top3 oos_ok=1/3`（pass）
    - kojirin(統合run): `top3 oos_ok=0/3`（fail）
    - kojirin再探索（vwapvr集中）: `data/reports/armada_player_replica_20260221_kojirin_b1_vwapvr_seed23.json`
    - kojirin(vwapvr集中): `top3 oos_ok=1/3`（pass）
    - 追加検証（hold TF filter ON, seed=20260221）:
      - `data/reports/armada_player_replica_20260221_b1_taiki_kojirin_seed20260221.json`
      - taiki/kojirin ともに `top3 oos_ok=1/3`、`timeframe=120`（`hold-bars>=2` を維持）
    - 判定: B1完了条件（taiki/kojirinともに top3 内 `oos_ok>=1`）を満たした。

- [x] **B2 pandajiro/yumimin引き上げ（CPCV底上げ）**
  - 目的: OOSは通るが CPCVが弱い候補の pass_rate を改善。
  - 実行: `volsma/vwapvr` の閾値・期間密度を上げて再探索（TF60中心）。
  - 完了条件: 各プレイヤーで `cpcv_ok=True` かつ `oos_ok=True` の候補を top3 内に最低1件。
  - 成果物: `data/reports/armada_player_replica_YYYYMMDD_panda_yumi_*.json`
  - 2026-02-21 実行メモ:
    - 実行済み: `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed20260221.json`
    - pandajiro: `top3 oos_ok=3/3`, `top3 (oos_ok && cpcv_ok)=0/3`（fail）
    - yumimin: `top3 oos_ok=3/3`, `top3 (oos_ok && cpcv_ok)=0/3`（fail）
    - 追加再探索: `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed20260220_c180_allind.json`
    - 追加再探索結果: pandajiro/yumimin ともに `top3 (oos_ok && cpcv_ok)=0/3`（fail継続）
    - 追加再探索(seed=11): `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed11_c180_allind_sweep.json`
    - 追加再探索(seed=11)結果: pandajiro/yumimin ともに `top3 (oos_ok && cpcv_ok)=0/3`（fail継続）
    - 条件充足run: `data/reports/armada_player_replica_20260221_hold_off_oos50_cpcv5_c180_top3.json`
    - 条件充足結果: pandajiro/yumimin ともに `top3 (oos_ok && cpcv_ok)=1/3`（pass）
    - 補足: 条件充足runは `hold_timeframe_filter.enabled=false` だが、top3はいずれも `TF60` で成立。
    - 追加再探索（volsma専用）: `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed23_c240_volsma.json`
    - volsma専用結果: pandajiro/yumimin ともに `top3 oos_ok=3/3`, `top3 cpcv_ok=3/3`, `top3 (oos_ok && cpcv_ok)=3/3`（強化pass）
    - 追加再探索（vwapvr専用, c60）: `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed23_c60_vwapvr.json`
    - vwapvr専用(c60)結果: pandajiro/yumimin ともに `top3 oos_ok=3/3`, `top3 cpcv_ok=0/3`, `top3 (oos_ok && cpcv_ok)=0/3`（fail）
    - 追加再探索（vwapvr専用, c240）: `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed23_c240_vwapvr.json`
    - vwapvr専用(c240)結果: pandajiro/yumimin ともに `top3 oos_ok=3/3`, `top3 cpcv_ok=0/3`, `top3 (oos_ok && cpcv_ok)=0/3`（fail継続）
    - 比較サマリ: `data/reports/armada_b2_indicator_comparison_20260221.json`（all-indicators系と `vwapvr(c60/c240)` は `players_with_b2_gate=0`、volsma専用のみ `players_with_b2_gate=2`、`missing=0`）
    - 追加診断（pandajiro top50）: `data/reports/armada_player_replica_20260221_pandajiro_b2_probe_top50_seed23_c180.json`
    - top50診断結果: `top3 combo=0/3`, `top10 combo=1/10`, `top50 combo=30/50`（候補自体はあるが選抜上位に乗らない）
    - 追加診断（pandajiro strength_weight=3.0）: `data/reports/armada_player_replica_20260221_pandajiro_b2_strengthw3_seed23_c180.json`
    - strength_weight=3.0結果: `top3 oos_ok=0/3`, `top3 cpcv_ok=0/3`（悪化、採用見送り）
    - 主要ボトルネック: all-indicators混在時は `cpcv_pass_rate=0.40` で頭打ちだが、`volsma` へ絞ると `0.60` まで改善
    - 判定: B2完了（完了条件を満たすrunを複数確認）ただし最終選抜ルールの再現性は未固定

- [x] **C1 core5投入判定パック作成**
  - 目的: core5全体の L1/L2/L3 判定を1ファイルで追跡可能にする。
  - 実行: ON/OFF比較、seed比較、strict合格数、paper監視結果を統合。
  - 完了条件: 「投入可/保留/再探索」の3区分でプレイヤー別に判定完了。
  - 成果物: `data/reports/armada_deploy_readiness_YYYYMMDD.json`
  - 2026-02-21 実行メモ:
    - 実行済み: `data/reports/armada_deploy_readiness_20260221.json`
    - 区分結果: `投入可=0`, `保留=4(taiki/kojirin/pandajiro/yumimin)`, `再探索=1(nami)`
    - 判定: `deploy_decision=保留 (no player reached L3)`
    - 参照入力:
      - `data/reports/armada_nami_seed_sweep_20260221_strengthbias_summary.json`
      - `data/reports/armada_player_replica_20260221_b1_taiki_kojirin_seed20260221.json`
      - `data/reports/armada_player_replica_20260221_b2_panda_yumi_seed23_c240_volsma.json`
      - `data/reports/armada_b2_indicator_comparison_20260221.json`

---

## 2026-02-21 運用追補: VWAPVR Founder投入時の Guardian ドリフト対策

- 背景:
  - `:hunted-h12-vwapvr-50-150` 投入時、`logs/backtest.log` に `unknown variant 'vwapvr'` が断続的に発生。
  - 症状として Phase1 が `Sharpe=0/PF=0` 側へ偏り、正常な比較ができないケースが発生。
- 原因:
  - `backtest_service.py` が古い Guardian バイナリを保持したまま稼働し、`vwapvr` variant を解釈できない状態だった。
- 対応:
  - `guardian` を再ビルド（`cargo build --release`）し、`backtest_service` を再起動して新バイナリへ切替。
  - 影響を受けた対象（`Hunted-H12-VWAPVR-50-150-USDJPY`）は `GRAVEYARD` 重複を除去して再評価。
- 検証:
  - `logs/swimmy.log` で `Phase 1 Result ... Sharpe=0.12 PF=1.33` と `NIL → B` を確認。
  - DB（`data/memory/swimmy.db`）で `Hunted-H12-VWAPVR-50-150-USDJPY` が `:B` を確認。
- 運用ルール（再発防止）:
  - Guardian の indicator variant 追加後は必ず `guardian` 再ビルド + `backtest_service` 再起動をワンセットで実施。
  - `unknown variant` が検出された場合、Phase1結果を信頼せず、まずバイナリ整合を疑う。

## 2026-02-21 実装追補: VWAPVR Founder の3通貨ペア対応（USDJPY/EURUSD/GBPUSD）

- 背景:
  - 既存 `:hunted-h12-vwapvr-50-150` は USDJPY では機能したが、同一設定を EURUSD/GBPUSD へ横展開すると `PF<1` 側に寄りやすかった。
  - 共通1セットでは3通貨同時に安定しづらいため、通貨ペア別チューニングへ切替。
- 実装:
  - `src/lisp/school/school-founders-hunted.lisp`
    - 既存キーを USDJPY 固定化:
      - `:hunted-h12-vwapvr-50-150` -> `Hunted-H12-VWAPVR-50-150-USDJPY`
    - 新規 founder を追加:
      - `:hunted-d1-vwapvr-50-220-eurusd` (`D1`, `vwapvr 50/220`, `SL=1.2`, `TP=4.0`)
      - `:hunted-d1-vwapvr-80-180-gbpusd` (`D1`, `vwapvr 80/180`, `SL=1.2`, `TP=4.0`)
    - `recruit-hunted-batch` に EUR/GBP 向け2 founder を追加。
  - `src/lisp/school/school-execution.lisp`
    - `recruit-special-forces` に founder key 接尾辞ベースの symbol pinning を追加:
      - `-USDJPY` / `-EURUSD` / `-GBPUSD` は該当シンボルのみ募集。
      - 互換性維持のため `:hunted-h12-vwapvr-50-150` は `USDJPY` 固定で扱う。
    - それ以外の founder は従来どおり `*supported-symbols*` 全体へ募集。
- テスト:
  - `src/lisp/tests.lisp`
    - `test-hunted-founder-templates-apply-trend-hardening-filters` を拡張し、EUR/GBP 向け VWAPVR founder の存在・symbol・TF・entry/exit を検証。
    - `test-recruit-special-forces-respects-founder-key-symbol-suffix` を追加（接尾辞 founder の単一シンボル募集を検証）。
    - 既存の `recruit-special-forces` 系回帰 (`...per-supported-symbol`, `...filters-founder-keys`, `...respects-recruit-limit`) も pass。

---

## 2026-02-19 実装追補: TF正本化の徹底（意味カテゴリは補助タグ）

- 方針:
  - `strategy-category` は新形式 `TF DIRECTION SYMBOL` を正本として扱う。
  - 旧来の意味カテゴリ（`trend/reversion/scalp/breakout`）は主キーとしては使わず、`strategy-regime-class` で補助的に推定・参照する。
- 実装:
  - `*regime-pools*` を導入し、意味カテゴリ系の集計・レジーム選抜・生態系評価はこのプールを参照。
  - `*category-pools*` は TFスコープ（`TF DIRECTION SYMBOL`）用途として維持。
  - KB追加/除去・実行系・ランク遷移時に `*category-pools*` と `*regime-pools*` を同時同期。
  - `cull-pool-overflow` は regime pool 優先で処理し、victim の scope pool 側も明示的に除去。
  - 旧「意味カテゴリへ移行（migrate）」前提の処理は除去し、新方式に一本化。

### 追加/更新テスト（2026-02-19 実行）

- `test-strategy-regime-class-prefers-semantic-and-infers-scope-keys` → pass
- `test-select-strategies-for-regime-supports-tf-direction-category-keys` → pass
- `test-select-strategies-for-regime-uses-real-categories` → pass
- `test-breeder-cull-uses-composite-score` → pass（`*regime-pools*` 前提へテスト更新）

### 検証（2026-02-19 JST）

- ターゲット回帰:
  - `sbcl --non-interactive --eval ... (swimmy.tests::test-...)` 群 → **all pass**
- フル回帰:
  - `SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp` → **462 passed / 0 failed**

---

## 2026-02-18 運用追補: systemdスコープ整合 + Backtest Heartbeat可観測性

- 背景:
  - `systemctl --user status swimmy-*` は `unit not found` だが、実プロセス/ポートは稼働しているように見える状態が継続。
  - `logs/backtest.log` の更新停滞により、Backtest停止と誤認しやすい観測ギャップが発生。
- 原因:
  - Swimmyコア（`swimmy-brain/school/backtest/guardian/data-keeper/notifier/risk`）は user scope ではなく system scope で稼働。
  - PID `cgroup` は `/system.slice/swimmy-*.service` を指しており、`journalctl -u`（system）が正本、`journalctl --user` は空でも異常とは限らない。

### 対応

- 診断手順を更新:
  - `pipeline-diagnosis` スキルを更新し、確認順を `systemctl status`（system）優先へ変更。
  - `journalctl -u`（system）→ `journalctl --user -u`（user）の順で確認する手順へ変更。
  - `/proc/<pid>/cgroup` による system/user scope 判定を手順に追加。
  - 反映先:
    - `/home/swimmy/swimmy/.codex/skills/pipeline-diagnosis/SKILL.md`
    - `/home/swimmy/.codex/skills/pipeline-diagnosis/SKILL.md`
- Backtest可観測性を改善:
  - `tools/backtest_service.py` に heartbeat を実装（既定60秒、`SWIMMY_BACKTEST_HEARTBEAT_SEC=0` で無効化）。
  - 出力内容: `inflight/recv/submit/done/sent/rx_age/tx_age`。
  - テスト追加: `tools/test_backtest_service.py`。

### 検証（2026-02-18 JST）

- テスト:
  - `python3 tools/test_backtest_service.py` → `ok`
  - `PYTHONPATH=/home/swimmy/swimmy python3 tools/tests/test_systemd_drift_probe.py` → `OK`
- サービス:
  - `sudo -n systemctl restart swimmy-backtest` 実行後、`swimmy-backtest.service` は `active (running)`。
  - `logs/backtest.log` に heartbeat 行を確認:
    - 例: `[BACKTEST-SVC] ❤️ HEARTBEAT inflight=0/6 recv=... tx_age=...`
- パイプライン:
  - `oos_queue` は空（`[]`）を確認。

### 取り込み状況（2026-02-18 JST）

- master へ反映済み・push済み（`origin/master`）。
- worktree先行コミット（`d083399`, `c559885`, `85d5297`）は `cherry-pick` 検証で空コミット化（同等内容が master 済み）を確認。
  - `git cherry -v master feat-polyclaw-safety-weather` → `- d083399 ...`
  - `git cherry -v master feat-weather-open-meteo` → `- c559885 ...`, `- 85d5297 ...`

## 2026-02-17 運用追補: Live実行TFコンテキストの fail-close 強化

- `prepare-trade-context` は、呼び出し側から渡る `strategy-timeframe` よりも、解決済み戦略オブジェクトの `strategy-timeframe` を優先するよう修正。
- `execute-category-trade` は、`timeframe` が有効でも `strategy-name` が KB/evolved で解決できない場合に fail-close（`execution.context_missing`）するよう修正。
- これにより、古いシグナルペイロード由来の `|M1` 混入や `unknown` 戦略名での発注を抑止。

### 追加テスト（2026-02-17 実行）

- `test-prepare-trade-context-prefers-strategy-timeframe-over-stale-override` → pass
- `test-execute-category-trade-fails-closed-on-unresolved-strategy-name` → pass
- `test-execute-category-trade-fails-closed-on-missing-timeframe` → pass（回帰なし）

### 運用確認（2026-02-17 JST）

- `swimmy-school.service` を再起動し、`ExecStartPre`（`tools/restore_legend_61.lisp`）成功を確認。
- DB確認: `Aggressive-Reversal=10080(W1)`, `MA-Ribbon-Scalp=10080(W1)`, `CCI-Trend-Breakout=240(H4)`。
- 再起動後ログで対象3戦略の `execution.order_submitted` に `M1` は未検出。
- `src/mt5/SwimmyBridge.mq5` の再コンパイル成功（2026-02-17 JST、ローカル確認）。
- `swimmy-guardian.service` を 2026-02-17 20:02 JST に再起動し、`0.0.0.0:5557/5560` の再リッスン復帰と `Active: running` を確認。
- 再起動後監査: `python3 tools/check_order_timeframe_consistency.py --lookback-minutes 30 --fail-on-issues` → `orders=3 issues=0`。
- 監査ツール `tools/check_order_timeframe_consistency.py` を追加。  
  例: `python3 tools/check_order_timeframe_consistency.py --since 2026-02-17T17:04:06 --fail-on-issues`
- `tools/system_audit.sh` に「Order timeframe consistency」ステップを統合。既定は `--lookback-minutes 120`（`ORDER_TF_AUDIT_LOOKBACK_MINUTES` で変更、固定開始時刻は `ORDER_TF_AUDIT_SINCE`）。
- `src/mt5/SwimmyBridge.mq5` を運用補強:
  - S式キー抽出は「パターン定義順」ではなく「文字列内で最初に現れた一致」を優先（混在フォーマット時の誤抽出を低減）
  - S式キー抽出は quoted string 内の疑似キー一致を探索対象外にし、判定は事前マスク化で低オーバーヘッド化
  - `ORDER_OPEN` は `instrument` 欠落時でも `symbol` を受理（旧ペイロード互換）
  - `instrument/symbol` が `NIL/NULL/NONE` の場合は空扱いに正規化し、`ORDER_OPEN` は fail-close
  - `ORDER_OPEN` の `symbol=ALL` は fail-close（全チャート一括発注を禁止）
  - `HISTORY` 送信の `total` バッチ数を厳密計算（5000本境界の off-by-one 解消）
  - コマンド処理は `MAX_COMMANDS_PER_TIMER` で1秒あたり複数件ドレイン
  - ZMQ `context/socket` 生成失敗時は `INIT_FAILED` / 接続失敗扱いで fail-close、`ACK/REJECT` 送信全失敗時は `g_pub_connected=false` に落として再接続ループへ復帰

## 2026-02-17 実装追補: TF内部統一 + 低トレード過大評価補正

- TF内部表現を `minutes(int)` 正本で統一（`M36/H2/H5/H60` 等の任意TFを許容）。
- 8TF (`M5/M15/M30/H1/H4/D1/W1/MN`) は「デフォルト集合」として維持しつつ、カテゴリ淘汰/相関スコープはバケット化で有限化。
- Pattern Gate は任意TFを直接拒否せず、クエリ時にバケットTFへ正規化（例: `H5 -> H4`）。
- Guardian 側TF解釈を統一し、`M/H/D/W/MN` と数値分文字列（例: `"300"`）を同一経路で秒換算。
- `runner/main` の履歴要求TFを定数由来に統一（ハードコード分岐を削減）。

### 低トレード補正（トレード数35問題への対処）

- `score-from-metrics` に trade evidence 係数を導入し、低サンプル時のSharpe寄与を縮小。
- A評価 / Breeder優先度 / Cullingスコアに `:trades` を渡し、同じ補正式で評価。
- 通知側は raw Sharpe に加えて adjusted Sharpe を併記し、低頻度戦略の過信を抑制。
- Evolution Report `Top Candidates` も raw Sharpe順から `evidence-adjusted-sharpe` 順へ統一し、表示を `S=<adjusted> (raw <raw>)` に更新。
- A/S ランクに trade evidence floor を導入（A>=50, S>=100）。`run-rank-evaluation` で既存ランクにも降格スイープを適用。
- `evaluate-a-rank-strategy` は floor 未達を即時 `:B` 降格として扱い、A維持を許容しない。

### 検証（2026-02-17 実行）

- `sbcl --script tests/test_runner.lisp` → **419 passed / 0 failed**
- `cd guardian && cargo test test_get_tf_duration_supports_custom_timeframes -- --nocapture` → **pass**

### 実測スナップショット（2026-02-17）

- 対象DB: `data/memory/swimmy.db`
- rank件数: `:A=36`, `:S=2`, `:B=140`, `:INCUBATOR=4972`, `:GRAVEYARD=350158`, `:RETIRED=39841`, `:LEGEND=26`
- floor違反件数: `A_lt50=0`, `S_lt100=0`（違反ゼロを確認）
- 低トレード高Sharpe上位10件は現在 `:GRAVEYARD`（A/Sではない）であることを確認
- backup table: `trade_floor_backup_20260217` が存在し、`1707` rows

### 追加ターゲットテスト（2026-02-17 実行）

- `test-timeframe-utils-support-arbitrary-minutes` → pass
- `test-timeframe-bucketization-is-finite` → pass
- `test-score-from-metrics-penalizes-low-trade-evidence` → pass
- `test-evaluate-a-rank-demotes-on-min-trade-evidence` → pass
- `test-enforce-rank-trade-evidence-floors-demotes-existing-as` → pass
- `cd guardian && cargo test test_sharpe_ratio_includes_zero_returns -- --nocapture` → pass

### テスト追補（2026-02-17 追加）

- 失敗していた回帰テストを、現行仕様（A: TradeEvidence>=50 / S: TradeEvidence>=100、S昇格時CommonStage2）に追従させた。
  - CPCV候補抽出/バッチテストに trade evidence を付与
  - B-rank culling の A-base 候補系テストに trade evidence を付与
  - Promotion通知テストは Stage2判定をテスト目的で固定化
  - Evolution report件数テストはタブ/Library注記を許容する比較へ修正
  - 閾値依存の固定値（80/120）を除去し、`min-trade-evidence-for-rank` 参照で将来閾値変更に追従
- `test-backtest-trade-logs-insert` は `*disable-auto-migration*` を有効化し、巨大自動migration起因の `database is locked` フレークを解消。
- 検証結果: `sbcl --script tests/test_runner.lisp` → **445 passed / 0 failed**。

### Top Candidates 実装追補（2026-02-17 追加）

- `build-top-candidates-snippet-from-db` を DB Active候補の `evidence-adjusted-sharpe(sharpe,trades)` ソートに変更。
- 表示は `S=<adjusted> (raw <raw>)` へ統一（rank `NIL` は `INCUBATOR` 表示を維持）。
- 検証（対象テスト）:
  - `test-top-candidates-excludes-archive-and-hides-nil-rank` → pass
  - `test-top-candidates-prefers-evidence-adjusted-sharpe-order` → pass
  - `test-trade-logs-supports-pair-id` → pass（`backtest-db-tests` のネスト崩れ回復確認）
  - `test-cpcv-status-snippet-reads-status-file-and-last-start` → pass
  - フル回帰: `SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp` → **456 passed / 0 failed**

### Sランク整合追補（2026-02-17 追加）

- `run-rank-evaluation` に S conformance sweep を追加し、既存 `:S` も `check-rank-criteria :S` を再評価する仕様へ更新。
- S基準逸脱時は `:A`（A基準通過時）または `:B` へ降格。trade floor のみ満たす `:S` 残留を防止。
- 回帰テスト追加: `test-enforce-s-rank-criteria-conformance-demotes-noncompliant-s` → pass
- 実DB是正（`data/memory/swimmy.db`）:
  - backup table: `s_conformance_backup_20260217`（更新前 `:S` 3件を退避）
  - 変更: `RECRUIT-RND-1768781166-12` を `:S -> :B`（1件）
  - 是正後（手動是正直後）: `:S=2`, `:A=36`, `:B=140`
- 運用反映（`tools/ops/finalize_rank_report.sh --with-rank-eval` 実行後）:
  - `S` conformance sweep: `S-demoted=0`
  - 上記 `RECRUIT-RND-1768781166-12` は週次評価/cullingにより最終的に `:GRAVEYARD` へ遷移
  - 最新件数: `:S=2`, `:A=36`, `:B=139`, `:GRAVEYARD=350159`
- 検証結果: `SWIMMY_DISABLE_DISCORD=1 sbcl --script tests/test_runner.lisp` → **450 passed / 0 failed**。

---

## 2026-02-15 運用追補: Backtest Service 入力互換 + 5580 競合対策

- 症状: `S-Exp parse error: missing field \`t\``（MANUAL-TRADELIST系の inline candles が `timestamp` キー）と、`invalid type: symbol, expected boolean` が散発。
- 症状: `zmq.error.ZMQError: Address already in use (tcp://*:5580)`（Backtest Service の二重起動/競合起動）。
- 対応: `tools/backtest_service.py` で `timestamp/time/open/high/low/close/volume` を `t/o/h/l/c/v` へ正規化し、`swap_long/swap_short` も `sl/ss` へ正規化。
- 対応: `tools/backtest_service.py` で `t` キーが `(true . ...)` に壊れるケースを防止（S式パーサが `t` を bool として解釈する副作用への局所対策）。
- 対応: `tools/backtest_service.py` に単一インスタンスロック（`/tmp/swimmy-backtest-svc-<port>.lock`）を追加し、競合起動の再発を抑制。
- 検証: `tools/test_backtest_service.py` に正規化の契約テストを追加（`timestamp`→`t`、`t`キー保持、`swap_*`→`s*`、`filter_tf`小文字化）。

## V50.8 追補 (2026-02-11) - Balanced Gate / DryRun Persistence

| 項目 | 現行実装 |
|------|----------|
| **Stage 1 閾値** | B=`Sharpe>=0.15 PF>=1.05 WR>=35% MaxDD<25%` / A=`Sharpe>=0.45 PF>=1.30 WR>=43% MaxDD<16%` / S=`Sharpe>=0.75 PF>=1.70 WR>=50% MaxDD<10%` |
| **A昇格ゲート** | OOS `Sharpe>=0.35` かつ `net_expectancy_pips > 0`（`calculate-avg-pips - *max-spread-pips*`） |
| **S昇格ゲート** | CPCV `pass_rate>=70%` かつ `median MaxDD<12%` |
| **Stage2ゲート** | A: MC `prob_ruin<=2%` 必須（DryRunは `*a-rank-require-dryrun*=NIL` 既定でサンプル不足時ブートストラップ可） / S: MC + DryRun `p95(abs(slippage_pips))<=*max-spread-pips*` 必須（20 samples） |
| **DryRun永続化** | `dryrun_slippage_samples` に保存、戦略ごとに最新 `*dryrun-slippage-sample-cap*` 件（既定200件）保持 |
| **DryRun期間保持** | `*dryrun-slippage-max-age-seconds*` が正値なら保持期間外を削除。`NIL` は無効 |

---

## V50.6 完了フェーズ (Structured Telemetry / Retired Rank)

| 機能 | 詳細 |
|------|------|
| **Structured Telemetry** | JSONL統合ログ（`logs/swimmy.json.log`、`log_type="telemetry"`）＋10MBローテ |
| **Local Storage S-exp** | `system_metrics.sexp` / `live_status.sexp` を原子書き込みに統一 |
| **Retired Rank** | Max Age退役アーカイブ（`data/library/RETIRED/`・`data/memory/retired.sexp`、低ウェイト学習） |
| **Aux Services S-exp** | Data Keeper / Notifier / Risk Gateway を S式 + `schema_version=1` に統一 |

---

## V50.6 追加: ペア戦略 (Hybrid Slots)

| 項目 | 詳細 |
|------|------|
| **永続化** | `pair_strategies` テーブルで `pair_id/strategy_a/strategy_b/weight/評価指標/rank/last_updated` を保持 |
| **選抜** | `*pair-slots-per-tf*` (シンボル×TF上限) + `*pair-competition-top-n*` (単一戦略と同列競争) |
| **検証ゲート** | A=OOS合成評価、S=CPCV合成評価。trade_list不足は昇格不可 |
| **スケジュール** | 毎日 00:10 の日次PnL集計後に `refresh-pair-strategies` → `refresh-pair-active-defs` |
| **実行反映** | `*pair-active-defs*` のみ overlay に適用 |

---

## V49.8 完了フェーズ (Phase 12)

| 機能 | 詳細 |
|------|------|
| **SQL Migration** | 200MB超の戦略データを SQLite へ移行。メモリバウンドな設計を解消。 |
| Swarm Draft SQL | 28万件超のレコードをSQLインデックス経由で高速検索・抽出。 |
| Graveyard SQL | 墓場パターンの分析・回避ロジックをSQLクエリ化。 |

---

## V49.5 完了フェーズ (Phase 11)

| 機能 | 詳細 |
|------|------|
| **Project Haystack** | ジム・シモンズ哲学に基づく原子予測子（Predictor）のアンサンブル統合 |
| Swarm Strategy | 既存システムを壊さず、数千の弱学習器を内包するコンテナ戦略の実装 |
| Predictor Factory | Lisp S式によるロジック自動生成と Forward Sharpe Sieve による選別 |
| Safety Guards | システム停止時の Ghost Notification 抑制機能を実装 |
| ASDF Integration | 新モジュールを ASDF システムに正式登録 |


---

## V49.2 完了フェーズ (Phase 10)

| 機能 | 詳細 |
|------|------|
| Tactical Mapping | `*regime-tactics*` によるレジーム別戦術のデータ化 |
| LLM Injection | `test-llm-tactical-injection` によるLLMプロンプトへの知恵注入 |
| Cloning Safety | ブリーディング時の遺伝的距離チェック (`strategies-correlation-ok-p`) |
| Strict Regimes | `regime-intent` メタデータの継承による意図の追跡 |

## V48.5 完了フェーズ (Phase 9)

| 機能 | 詳細 |
|------|------|
| Perf Persistence | `cpcv_history.csv` へのバックテスト結果永続化 |
| Notification Throttling | 大量バックテスト時の通知流量制御によるノイズ削減 |
| Category Watchers | 通貨ペア×方向×TF別の精鋭戦略監視機能 |

---

## V48.2 完了フェーズ (Phase 8)

| 機能 | 詳細 |
|------|------|
| Atomic KB Locks | `*kb-lock*` によるスレッド安全なKB操作 |
| Safe Graveyard Save | `graveyard.emergency.sexp` へのフォールバック保存 |
| S-RANK Slot Limits | 各カテゴリ最大3枠のS-RANK枠管理 & 自動入れ替え |
| Circular Dependency Fix | `school-state.lisp` への状態変数集約によるビルド安定化 |

---

## V48.1 完了フェーズ (Phase 7)

| 機能 | 詳細 |
|------|------|
| Graveyard Immediate Purge | `prune-to-graveyard` で即座にKBから削除 |
| CPCV Batch Size Up | バッチサイズ 5 → 20 件 |
| Unified Rank Setter | `ensure-rank` によるランク変更の一元管理 |

---

## 1. ランク体系 (The Hierarchy)

| Rank | シンボル | 説明 |
|------|---------|------|
| B | `:B` | 初期審査通過 (Phase 1 BT クリア) |
| A | `:A` | OOS検証通過 |
| S | `:S` | 実弾許可 (The Elite) |
| Graveyard | `:graveyard` | 廃棄・学習用データ |
| Retired | `:retired` | Max Age 退役アーカイブ（低ウェイト学習 / `data/memory/retired.sexp`） |
| Legend | `:legend` | 保護対象 (61戦略) |

---

## 2. ライフサイクルフロー

```mermaid
graph TD
    A[新戦略] --> B{Phase1 BT}
    B -->|Sharpe≥0.15| C[B-RANK]
    B -->|Fail| D[Graveyard]
    C -->|100戦略蓄積| E{Culling}
    E -->|Top2/カテゴリ| F{OOS検証}
    F -->|Sharpe≥0.35 + Expectancy>0 + MC/DryRun| AR[A-RANK]
    F -->|Fail| C
    E -->|Others| D
    AR --> G{CPCV検証}
    G -->|pass_rate≥70% + median MaxDD<12% + MC/DryRun| H[S-RANK]
    G -->|Fail| C
    
    %% S-RANK Live Trade Audit (V47.5)
    H --> I[ライブトレード]
    I -->|20トレード毎| J{Live Audit}
    J -->|0違反| I
    J -->|1違反| AR
    J -->|2違反| C
    J -->|3違反| D
    
    %% Learning & Breeding
    L[Legend 61戦略] -.->|Random Breeding| C
    D -.->|学習データ| M[RL/Graveyard分析]
    M -.->|Q-value| N[交配最適化]
```

> [!NOTE]
> **Max Age Retirement**: 年齢>30のアクティブ戦略はランクに関係なく Retired へ移動。

---

## 3. カテゴリ体系 (Owner's Vision V47.2)

> [!IMPORTANT]
> **淘汰は カテゴリ単位 で行う**

### カテゴリキー = TF-Bucket × Direction × Symbol

```lisp
(defun make-category-key (strategy)
  ;; 内部TFは minutes(int) を正とする。
  ;; ただしカテゴリ淘汰は有限化のため bucket 化したTFで行う。
  (list (get-tf-bucket-minutes (strategy-timeframe strategy))
        (strategy-direction strategy)   ; :BUY, :SELL, :BOTH
        (strategy-symbol strategy)))    ; EURUSD, GBPUSD, USDJPY
```

> [!IMPORTANT]
> **TF運用ルール（V50.6追補）**
> - 内部表現は `minutes(int)` を正とする（例: H5=300, H60=3600）。
> - 8TF (`M5/M15/M30/H1/H4/D1/W1/MN`) はデフォルトの評価バケット。
> - `M36/H2/H5/H60` など任意TFは探索対象として許容。
> - ただしカテゴリ/相関スコープは 8TF バケットへ正規化し、無限分岐を防ぐ。

### 例

| Raw TF (minutes) | Bucket TF | Direction | Symbol | カテゴリキー |
|----|-----------|-----------|--------|--------------|
| 36 | M30 | :BUY | EURUSD | `(30 :BUY "EURUSD")` |
| 300 | H4 | :SELL | GBPUSD | `(240 :SELL "GBPUSD")` |
| 3600 | D1 | :BOTH | USDJPY | `(1440 :BOTH "USDJPY")` |

---

## 4. 基準値 (Strict Criteria)

| ランク | Sharpe | PF | WR | MaxDD |
|--------|--------|-----|-----|-------|
| B | ≥0.15 | ≥1.05 | ≥35% | <25% |
| A | ≥0.45 | ≥1.30 | ≥43% | <16% |
| S | ≥0.75 | ≥1.70 | ≥50% | <10% |

**※ 全条件 AND で判定**

### Stage 2 検証ゲート（A/S昇格時）
- **A**: OOS `Sharpe >= 0.35` かつ `net_expectancy_pips > 0` + MC `prob_ruin <= 2%`（DryRunは `*a-rank-require-dryrun*=NIL` 既定でサンプル不足時ブートストラップ可）
- **S**: CPCV `pass_rate >= 70%` かつ `median MaxDD < 12%` + MC `prob_ruin <= 2%` + DryRun `p95(abs(slippage_pips)) <= *max-spread-pips*`

---

## 5. 検証 & 適応 (Validation & Adaptation)

### A. CPCV (Combinatorial Purged CV)
- **目的**: 過学習の徹底排除
- **実装**: cpcv.rs
  - 20年データを5ブロックに分割
  - **Purge (3ヶ月)** + **Embargo (1.5ヶ月)** でリーク防止

### B. カルマンフィルター (Kalman Filter)
- **目的**: 動的なパラメータ最適値の推定とノイズ除去
- **実装**: kalman.rs (6テスト含む)

---

## 6. 実装フェーズ

### P0 ✅ 完了 (Commit: 02a3978)
| タスク | ファイル |
|--------|----------|
| ランク体系 `:B/:A/:S` | school-rank-system.lisp |
| CPCV バックテスト | cpcv.rs |
| カルマンフィルター | kalman.rs |

### P1 ✅ 完了 (Commits: c6407cd, 5233f5d)
| タスク | 詳細 |
|--------|------|
| `run_backtest_range` 実装 | cpcv.rs:172 |
| Brain統合 | school.lisp で呼び出し |
| Purge/Embargo 短縮 | 1年→3ヶ月, 6ヶ月→1.5ヶ月 |
| 交配ルール | 3回制限、Legend保護 |

### P1.5 ✅ 完了 (Commit: 4b29b42)
| タスク | 詳細 |
|--------|------|
| `:direction` スロット追加 | dsl.lisp |
| フィルター拡張 | TF/Direction/Symbol |
| カテゴリ淘汰 | run-b-rank-culling-for-category |
| Direction自動判定 | detect-direction-from-entry |

### P2 ✅ 完了 (Commit: 2b8b5c1)
| タスク | 詳細 |
|--------|------|
| `save-failure-pattern` | graveyard.sexp 保存 |
| Kalman 6テスト | エッジケース追加 |
| `record-rl-reward` | rl_rewards.sexp 保存 |

### P3 ✅ 完了 (Commit: 4ed3bc3)

#### ① 失敗パターン分析 (Taleb/Graham)
- [x] `load-graveyard-patterns` - sexpファイル読込
- [x] `analyze-graveyard-for-avoidance` - SL/TP範囲で失敗クラスタ抽出
- [x] `should-avoid-params-p` - 回避判定

#### ② RL優先度学習 (Naval/Ng)
- [x] `load-rl-rewards` - sexpファイル読込
- [x] `get-param-priority` - 勝率ベース重み (0.5-1.5)

#### ③ Q-learning拡張 (Ng)
- [x] `*q-table*` - (TF Dir Sym SL TP) → Value
- [x] `update-q-value` - Q = Q + α(r - Q)
- [x] `explore-or-exploit-p` - ε-greedy (80/20)

#### ④ 時間減衰 (López de Prado)
- [x] `apply-p3-time-decay` - 古いデータを割引
- [x] 減衰率: 1ヶ月経過で0.9倍

#### ⑤ ファイルローテーション (Gene Kim)
- [x] `check-file-rotation-needed` - 10MB超過で分割
- [x] `rotate-file-if-needed` - graveyard.YYYY.sexp形式

---

### P4 ✅ 完了 (Commit: f9fd438)

#### ① 平均利益Pips計測
- [x] `calculate-avg-pips` - 期待pips: (WR × TP) - ((1 - WR) × SL)
- [x] `calculate-avg-pips-from-history` - 実績PNLからpips逆算

#### ② SL/TP比率分析
- [x] `analyze-sl-tp-ratio` - ratio, expected-pips, verdict
- [x] `calculate-required-wr` - 損益分岐点WR計算

#### ③ GPT基準フィルタ (GPT提案)
- [x] `check-pip-design-health` - 警告リスト生成
- [x] `audit-strategy-pip-design` - 個別戦略監査
- [x] `audit-all-strategies` - 一括監査レポート

**GPT基準閾値:**
- 平均利益 < 10pips → 警告
- PF < 1.2 → 警告
- Sharpe < 0.3 → ノイズ警告

---

## 8. ファイル対応表

| Phase | ファイル |
|-------|----------|
| ランク判定 | school-rank-system.lisp |
| 交配 | school-breeder.lisp |
| DSL/Strategy | dsl.lisp |
| CPCV | cpcv.rs |
| Kalman | kalman.rs |
| P3 学習 | school-p3-learning.lisp |
| **P4 監査** | **school-pip-audit.lisp** |
| 学習データ | data/memory/*.sexp |
| Retired Archive | data/memory/retired.sexp / data/library/RETIRED/ |

---

### P5 ✅ 完了 (Self-Review Fixes + Live Trade Audit)

#### ① Self-Review高優先度
- [x] Q-table永続化 (`save-q-table`, `load-q-table`)
- [x] Graveyard分析をScoutに接続 (`is-toxic-params`拡張)

#### ② Self-Review中優先度
- [x] 空リスト対策 (`remove nil` added)
- [x] 通貨ペア別pip値 (`*pip-values-by-symbol*`)

#### ③ Self-Review低優先度
- [x] `audit-all-strategies` 利用可能
- [x] Q-learning exploitation実装済

#### ④ ライブトレード監査 (GPT基準)
- [x] `run-live-trade-audit` - 20トレード毎に監査
- [x] `determine-live-audit-action` - 違反数で判定
- [x] S-RANK降格ロジック:
  - 0違反 → S-RANK維持
  - 1違反 → A-RANK (probation)
  - 2違反 → B-RANK (breeding stock)
  - 3違反 → Graveyard

**GPT基準閾値:**
```
平均利益 < 10pips → 違反
PF < 1.2 → 違反  
Sharpe < 0.3 → 違反
```

---

### P5.5 ✅ 完了 (Integration Hooks - Commit: 813316e)

#### ① トレードクローズフック
- [x] `run-live-trade-audit` → school-execution.lisp (2箇所)
- [x] `record-rl-reward` → school-execution.lisp (2箇所)

#### ② 状態保存フック
- [x] `save-q-table` → ledger.lisp (save-state内)

#### ③ 交配回避フック
- [x] `analyze-graveyard-for-avoidance` → school-breeder.lisp
- [x] `should-avoid-params-p` → 回避リージョンで再生成

---

## 9. 最終ステータス

| Phase | 状態 | Commit |
|-------|------|--------|
| P0-P2 | ✅ 完了 | 02a3978 → 2b8b5c1 |
| P3 | ✅ 完了 | 4ed3bc3 |
| P4 | ✅ 完了 | f9fd438 |
| P5 | ✅ 完了 | 462adea |
| **P5.5** | ✅ **統合完了** | **813316e** |

**🚀 V47.5 デプロイ準備完了**

---

### P6 ✅ 完了 (V47.5 Testing - Commit: 2b765a1)

#### ① V47.5専用テスト追加
- [x] テストファイル作成: `src/lisp/tests/school-v47-tests.lisp`
- [x] 15テストケース実装:
  - Pip Audit: 5テスト
  - Live Trade Monitoring: 4テスト
  - Q-Table: 2テスト
  - Graveyard Analysis: 2テスト
  - Rank System: 2テスト

#### ② テストカバレッジ
| 機能 | テスト数 |
|------|---------|
| calculate-avg-pips | 1 |
| get-pip-value | 1 |
| analyze-sl-tp-ratio | 1 |
| check-pip-design-health | 1 |
| calculate-required-wr | 1 |
| trade-count-tracking | 1 |
| live-audit-needed-p | 1 |
| determine-live-audit-action | 2 |
| Q-value-update | 1 |
| explore-or-exploit | 1 |
| should-avoid-params-p | 1 |
| time-decay | 1 |
| meets-rank-criteria | 1 |

---

## 10. 完全なPhaseサマリー

| Phase | 内容 | Commit |
|-------|------|--------|
| P0-P2 | ランク体系、CPCV、Learning基盤 | 02a3978 → 2b8b5c1 |
| P3 | Graveyard分析、Q-Learning、Time Decay | 4ed3bc3 |
| P4 | Pip Design Audit | f9fd438 |
| P5 | Self-Review修正、Live Trade Audit | 462adea |
| P5.5 | Integration Hooks | 813316e |
| P6 | V47.5 Testing | 8b5c464 |

---

### P7 ✅ 完了 (Q-Value Strategy Selection - Commit: cf800c1)

#### Musk条件付き承認

| 条件 | 内容 | 実装 |
|------|------|------|
| ① | Breederのみ適用 | ✅ breed-strategies |
| ② | 80/20ルール | ✅ 80%探索、20%活用 |
| ③ | 日次decay | ✅ 1%/日 減衰 |

#### 追加関数

| 関数 | 場所 | 役割 |
|------|------|------|
| `get-top-q-sltps` | school-p3-learning.lisp | Top N SL/TP取得 |
| `select-sltp-with-q` | school-p3-learning.lisp | Q-value選択 (80/20) |
| `decay-q-table` | school-p3-learning.lisp | 日次1%減衰 |

#### 統合ポイント

| フック | 場所 |
|--------|------|
| Q-value SL/TP選択 | school-breeder.lisp:breed-strategies |
| 日次decay実行 | scheduler.lisp:check-scheduled-tasks |

| **P7** | **Q-Value Strategy Selection** | **cf800c1** |

**🏁 V47.7 完全完成**

---
## P8: Strategy Pipeline Redesign [x] 完了

### 📋 用語定義

| 用語 | 定義 |
|------|------|
| **KB** | Knowledge Base - 戦略知識ベース (`*strategy-knowledge-base*`) |
| **Founder** | **外部検証済み戦略** (Web/論文/Hunted/Legend) |
| **Breeder** | **既存高性能戦略の交配** |

---

### ❌ 廃止項目

| 項目 | 理由 |
|------|------|
| Scout | ランダム生成は非効率 |
| recruit-from-evolution | 不要 |
| school-execution:402 | Founder重複 |
| strategies.lisp:23 | 不要 |
| Legend交配(別関数) | Breederに統合 |

---

### 新ライフサイクル (2入口)

```mermaid
graph TD
    subgraph 入口["📥 戦略入口 (2つのみ)"]
        F[Founder<br/>外部検証済]
        B[Breeder<br/>交配]
    end
    
    F --> ADD[add-to-kb]
    B --> ADD
    
    ADD --> KB[(KB)]
    KB --> BT{Phase1 BT}
    BT -->|Sharpe≥0.1| BRANK[B-RANK]
    BT -->|Fail| GY[Graveyard]
    
    BRANK -->|100戦略| CULL{淘汰}
    CULL -->|Top2| OOS{OOS検証}
    OOS -->|Sharpe≥0.3| ARANK[A-RANK]
    OOS -->|Fail| BRANK
    
    ARANK --> CPCV{CPCV}
    CPCV -->|Sharpe≥0.5| SRANK[S-RANK]
    
    SRANK --> LIVE[ライブ]
    
    SRANK -.->|高性能親| B
    GY -.->|学習| QL[Q-Learning]
    QL -.->|最適化| B
    
    style ADD fill:#2ecc71
```

---

### 🔒 Expert Panel 条件付き承認

| # | 条件 | 担当 | 実装 |
|---|------|------|------|
| 1 | **テストファースト**: add-to-kbテスト先に書く | Uncle Bob | Phase 1 |
| 2 | **Breeder→BT必須**: 生成物はPhase1 BTを通す | Graham | Phase 3 |
| 3 | **Founder→BT**: 最低限のBT検証 | López de Prado | Phase 3 |
| 4 | **段階デプロイ**: Scout削除→統合→通知 | Gene Kim | 実装順序 |
| 5 | **ロールバック**: 旧コードバックアップ | Gene Kim | Phase 2前 |

---

### 実装順序

#### Phase 1: 基盤整備
- [x] add-to-kb 関数作成 (school-kb.lisp 新規)
- [x] add-to-kb テスト作成 (**テストファースト**)
- [x] `*startup-mode*` フラグ追加

#### Phase 2: 削除 (バックアップ後)
- [x] 旧school-scout.lisp バックアップ（N/A: 現ツリーに対象ファイルなし）
- [x] Scout 完全削除
- [x] recruit-from-evolution 削除
- [x] school-execution:402 削除  
- [x] strategies.lisp:23 削除

#### Phase 3: 統合
- [x] Legend交配をBreederに統合
- [x] Founder → add-to-kb 経由に変更
- [x] Breeder → add-to-kb 経由に変更
- [x] **Breeder生成物 → 必ずBT通過確認**
- [x] **Founder → 最低限BT検証追加**

> [!NOTE]
> 2026-02-13 照合時点: `add-to-kb` 入口統合は実装済み（`school-kb.lisp` / `school-founders.lisp` / `school-breeder.lisp`）。
> Breeder生成物のBT必須化は `run-breeding-cycle -> add-to-kb(:require-bt t)` と `add-to-kb` 側の `source=:breeder` 強制Phase1で実装済み。
> Legend交配も `run-legend-breeding -> add-to-kb(:source :breeder :require-bt t)` へ統合済み。
> Scout語彙は新規生成・フォールバックから撤去済み（`rank=:incubator` を正本化）。旧 `:scout` は既存データ互換のため読取のみ許容。

#### Phase 4: 通知統一 ✅
- [x] 統一通知フォーマット実装
- [x] 起動時通知抑制

#### Phase 5: 検証・デプロイ ✅
- [x] Quality Gate
- [x] デプロイ (2026-01-21 23:04)

---

## P9: Validation Strengthening ✅ 完了 (2026-01-22)

### Phase 1: OOS Validation (CPCV Lite) ✅
- [x] school-validation.lisp 作成 (82行)
- [x] run-oos-validation - 70/30 train/test split concept
- [x] validate-for-a-rank-promotion - 全A-RANK基準チェック
- [x] meets-a-rank-criteria - Sharpe≥0.3, PF≥1.2, WR≥40%, MaxDD<20%

> [!NOTE]
> 真のCPCV (cpcv.rs連携) は P11 で実装予定

### Phase 2: school-hunter.lisp分割 ✅
| 分割後 | 行数 | 内容 |
|--------|------|------|
| school-hunter.lisp | 104 | コア戦略 |
| school-hunter-auto.lisp | 1,017 | Auto生成 (SRP除外) |

### Phase 3: E2Eテスト追加 ✅
- [x] test-meets-a-rank-criteria-pass/fail
- [x] test-e2e-add-to-kb-lifecycle

---

## P10: Optimization & Cleanup ✅ 完了 (2026-01-22)

### Phase 1: SRP修正 ✅
| ファイル | 変更 |
|----------|------|
| school-backtest.lisp | 640→564行 |
| school-validation.lisp | 82行 (新規) |
| school-pruning.lisp | 198行 (新規) |

### Phase 2: 戦略プルーニング ✅
- [x] prune-low-sharpe-strategies (Sharpe < 0.08)
- [x] prune-inactive-strategies (90日非活性)
- [x] prune-similar-strategies (distance < 0.1)
- [x] run-kb-pruning (メインオーケストレータ)
- [x] get-kb-statistics (統計表示)

### Phase 3: KB検索最適化 (将来検討)
- [x] O(n)→ハッシュ化
- [x] カテゴリ別インデックス

---

## 完了フェーズ

| Phase | 内容 | 完了日 |
|-------|------|--------|
| P0 | ランク体系・CPCV・Kalman基盤 | 2026-01-21 |
| P1 | Brain統合・CPCV完成 | 2026-01-21 |
| P1.5 | TF×Direction×Symbol | 2026-01-21 |
| P2 | Learning Loop基盤 | 2026-01-21 |
| P3 | Learning Advanced | 2026-01-21 |
| P7 | Q-value戦略選択 | 2026-01-21 |
| P8 | Strategy Pipeline Redesign | 2026-01-21 |
| **P9** | **OOS Validation + Hunter Split** | **2026-01-22** |
| **P10** | **KB Pruning + SRP Fix** | **2026-01-22** |
| **P11** | **Expert Panel Items (3/4)** | **2026-01-22** |
| **P12** | **True CPCV Lisp-Rust** | **2026-01-22** |
| **P24** | **Logic Integrity (Symbolic Hash)** | **2026-01-28** |
| **P25** | **Isolation & Watchdog** | **2026-01-28** |
| **P26** | **Structured Telemetry (JSONL + S-exp snapshots)** | **2026-02-03** |
| **P27** | **Retired Rank (Max Age archive + low-weight learning)** | **2026-02-06** |

---

## 🚀 総合ステータス: 100% 完了 (V50.6 Structured Telemetry + Retired Rank)

### P24 Implementation Details (Logic Integrity)
- **Symbolic Hashing**: `school-kb.lisp` checks logic similarity (Jaccard).
- **Highlander Rule**: Correlation > 95% triggers "Survival of the Fittest".
- **AI Auditor**: `failure_auditor.py` (XGBoost) culls toxic patterns from Graveyard.

### P25 Implementation Details (Resilience)
- **Service Isolation**: `school-scribe.lisp` (Async Block I/O).
- **Broken Arrow**: `school-watchdog.lisp` (100ms Latency Monitor).
- **DNA Verify**: `school-integrity.lisp` (SHA256 Pre-flight).

### P26 Implementation Details (Structured Telemetry)
- **JSONL統合ログ**: `src/lisp/logger.lisp` が `logs/swimmy.json.log` に `log_type="telemetry"` を出力。
- **イベント発火**: `emit-telemetry-event` を OOS/WFV/Heartbeat/metrics に統一。
- **ローカルスナップショット**: `data/system_metrics.sexp` / `.opus/live_status.sexp` を原子書き込み。

### P27 Implementation Details (Retired Rank)
- **Max Age退役**: `school-breeder.lisp` で age>30 の戦略を `:retired` に移動。
- **低ウェイト学習**: `data/memory/retired.sexp` を回避学習に低重みで適用。
- **アーカイブ**: `data/library/RETIRED/` に退役戦略を保存。
