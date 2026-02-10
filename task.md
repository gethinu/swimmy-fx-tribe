# Tasks

## Expert Panel 2026-01-31
- [ ] 入力の`read-from-string`経路を封鎖（`*read-eval*`無効化＋安全パーサ導入）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`, `src/lisp/core/persistence.lisp`
- [ ] Backtest V2のpayload修正とPhase2昇格ロジック実装＋テスト: `src/lisp/school/school-backtest-v2.lisp`
- [ ] 監視スクリプト修正（`import re`）＋実行テスト: `tools/report_system_status.py`
- [ ] 起動シーケンスの責務分割と重複削除: `src/lisp/main.lisp`
- [ ] ドキュメントの版本整合更新: `doc/owners_guide.md`, `doc/SYSTEM_ARCHITECTURE.md`
- [ ] systemdユニット配置の正本化とスクリプトのfail-fast化: `tools/install_services.sh`, `scripts/install-service.sh`

## Expert Panel 2026-02-01
- [ ] 入力の`read-from-string`経路を封鎖（`*read-eval*`無効化＋安全リーダ＋スキーマ検証）: `src/lisp/core/message-dispatcher.lisp`, `src/lisp/school/school-evolution.lisp`
- [ ] Backtest V2のpayload修正（`strategy-alist`統一）＋Phase2昇格ロジック実装＋テスト: `src/lisp/school/school-backtest-v2.lisp`
- [ ] systemdインストールのfail-fast化（コピー失敗で即停止＋ユニット存在検証）: `tools/install_services.sh`
- [ ] 監視レポート修正（`import re`）＋スモークテスト追加: `tools/report_system_status.py`
- [ ] 起動シーケンスの責務分割と重複削除（`load-hall-of-fame`重複排除）: `src/lisp/main.lisp`
- [ ] 設定と状態の分離（`config`からランタイム状態排除、`globals`に一本化）: `src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`
- [ ] ベンチ廃止の系統整理（戦略/アーム/通知）: `src/lisp/strategies/strategies.lisp`, `src/lisp/school/school-lifecycle.lisp`, `src/lisp/engine/positions.lisp`, `src/lisp/engine/heartbeat.lisp`

## Expert Panel 2026-02-01 (Benching Decision)
- [ ] 評価フェーズのベンチを墓場/killへ置換（Sharpe/PF/WR条件）: `src/lisp/strategies/strategies.lisp`
- [ ] 実行フェーズのアームベンチ廃止（`arm-benched-p`/`bench-arm`/`*benched-arms*`削除）: `src/lisp/engine/positions.lisp`, `src/lisp/engine/portfolio.lisp`, `src/lisp/core/config.lisp`, `src/lisp/core/globals.lisp`
- [ ] シグナル評価のbenched分岐削除: `src/lisp/school/school-evaluation.lisp`
- [ ] Heartbeatのbenched表示削除: `src/lisp/engine/heartbeat.lisp`
- [ ] 廃止後の期待動作テスト追加: `src/lisp/tests.lisp` or `tests/`

## Expert Panel 2026-02-03 (Backtest Policy)
- [ ] Backtest期間の正を決定し、`docs/llm/SPEC.md` と `docs/llm/STATE.md` に明記する。
- [ ] Graveyardのバックテスト方針（デフォルト除外/明示フラグ）を `docs/llm/STATE.md` / `doc/owners_guide.md` / runbook に反映する。
- [ ] `tools/ops/all_strategy_backtest.lisp` にスコープ指定（rank/期間/銘柄）と高速モード（sleep/ログ抑制）を追加する。
- [ ] Backtestレポートに「期間」「対象スコープ」を出力する（`data/reports/backtest_all_start.txt` 等の拡張）。

## Expert Panel 2026-02-03 (Thresholds & Ranges)
- [ ] `*phase2-min-sharpe*` を 0.3 に統一し、Phase2判定と Rank 条件を一致させる（`src/lisp/school/school-constants.lisp`, `src/lisp/school/school-backtest-v2.lisp`, `src/lisp/school/school-rank-system.lisp`）。
- [ ] Phase1/Phase2 の期間を `2011–2020` / `2021–現在(ローリング)` に更新し、年号ベタ書きの結果ルーティングを廃止する（`src/lisp/school/school-constants.lisp`, `src/lisp/school/school-backtest-v2.lisp`）。
- [ ] Phase2 の `end_time` を現在時刻で動的に埋める（固定 2026.12.31 を廃止）（`src/lisp/school/school-backtest-v2.lisp`）。
- [ ] Phase2 の実使用 `end_time` をログ/レポートに出力する（`src/lisp/school/school-backtest-v2.lisp`, `src/lisp/school/school-narrative.lisp`）。
- [ ] Phase2期間のユニットテストを追加する（`src/lisp/school/school-backtest-v2.lisp`）。
- [ ] Project Haystack/Swarm/Predictor Factory を planned/inactive として owners_guide/plan を修正する。
- [ ] CPCV の train/test split を実装し、purge/embargo を適用する（`guardian/src/cpcv.rs` + テスト追加）。
- [ ] Backtest Service が `start_time/end_time/data_id/aux_candles(_files)/swap_history` を Guardian へ転送するよう修正し、契約テストを追加する（`tools/backtest_service.py`, `tools/test_backtest_service.py`）。
- [ ] Phase識別を suffix 依存から `phase`/`range_id` の明示フィールドへ移行する（`src/lisp/school/school-backtest-v2.lisp`）。

## Expert Panel 2026-02-03 (MCP/MT5 Integration)
- [ ] MCPゲートウェイ（MCP ↔ ZMQ変換）の設計/実装を追加し、S式正本で運用する（`docs/llm/INTERFACES.md`）。
- [ ] 5559外部コマンドの認証/制限を導入し、バックテスト専用であることを明文化する（`guardian/src/main.rs`, `docs/llm/INTERFACES.md`）。
- [ ] `doc/SYSTEM_ARCHITECTURE.md` と `docs/llm/ARCHITECTURE.md` を統合し、MCPゲートウェイ位置を正本として記載する。
- [ ] MCPゲートウェイの統合テストを追加し、ライブ経路とは分離した検証ルートを定義する。
- [ ] MCPゲートウェイをsystemd常駐サービスとして起動できるようにし、運用手順をrunbookへ追加する。
- [ ] IDE非依存のCLI/バッチ実行ルート（バックテスト/状態取得）を用意し、自動化ジョブ化できる形にする。
- [ ] MCPゲートウェイ配置をWSL2内に固定し、正本ドキュメントへ明記する（`docs/llm/ARCHITECTURE.md`, `doc/SYSTEM_ARCHITECTURE.md`）。
- [ ] MCPゲートウェイのバインド/公開方針（localhostデフォルト、許可制）を決め、設定項目を追加する（`src/lisp/core/config.lisp`）。
- [ ] WSL IP手動設定の復旧手順をrunbookへ追記する（`docs/llm/STATE.md`）。
- [ ] MCPゲートウェイの権限レイヤー（read-only / trade-capable）を設計し、初期はread-onlyのみ有効化する。
- [ ] ライブ命令の拡張口はインターフェースのみ定義し、実装は無効化しておく。
- [ ] ライブ開放条件（検証/監視/緊急停止）をrunbookに明文化する。
- [ ] CPCV品質が担保されるまでライブ経路を解放しない方針をSTATEに追記する。
- [ ] MCPゲートウェイのS式契約をgolden fixtures化し、INTERFACES.mdと一致することを契約テストで保証する。
- [ ] 統合スモークはZMQエンドポイント差し替え可能にし、本番5559を叩かないテスト構成にする。
- [ ] JSON拒否（Iron Rule）をゲートウェイ契約テストに明示的に追加する。
- [ ] request_id相関をゲートウェイで導入し、backtest_status.txtと結びつけて検証可能にする。
- [ ] systemd起動/停止とAPIキー拒否の運用リハをスクリプト化し、runbookとセットでテストする。

## Expert Panel 2026-02-09
- [ ] CPCVバッチのゲート失敗理由（PF/WR/MaxDD含む）をログ/レポートへ出力: `src/lisp/school/school-validation.lisp`
- [ ] CPCV_RESULTのキー正規化/`request_id`/`is_passed`型整合を実装 + 回帰テスト: `guardian/src/main.rs`, `src/lisp/core/message-dispatcher.lisp`
- [ ] CPCV対象抽出をSQLite正本で実装、またはKBリフレッシュを明示: `src/lisp/school/school-validation.lisp`
- [ ] CPCVのqueued/sent/received/failedメトリクス永続化とreport整合: `src/lisp/school/school-validation.lisp`, `data/reports/evolution_factory_report.txt`
- [ ] CPCV_VALIDATE要求スキーマをINTERFACESに明記（必須キー/例）: `docs/llm/INTERFACES.md`
- [ ] CPCV_RESULTにmedian_pf/median_wr/median_maxddを追加し、Lispで取り込む: `guardian/src/cpcv.rs`, `guardian/src/main.rs`, `src/lisp/core/message-dispatcher.lisp`, `docs/llm/INTERFACES.md`
- [x] PF/MaxDDへの最適化圧力を評価系へ組み込む（Sharpe偏重是正）: `src/lisp/school/school-voting.lisp`, `src/lisp/school/school-rank-system.lisp`
- [ ] `doc/SYSTEM_ARCHITECTURE.md` を `docs/llm/ARCHITECTURE.md` と統合/廃止して正本化する: `doc/SYSTEM_ARCHITECTURE.md`, `docs/llm/ARCHITECTURE.md`

## Expert Panel 2026-02-09 (Stagnant C-Rank Batch)
- [x] フラッシュをループ終端依存から分離（独立タイマー or 短周期フック）: `src/lisp/school/school-connector.lisp`, `src/lisp/core/scheduler.lisp`
- [x] Stagnant C-Rank の実行頻度を仕様化（週次なら通知/Docs明記、日次ならロジック変更）: `src/lisp/school/school-breeder.lisp`, `doc/owners_guide.md`
- [x] 理由の構造化（文字列search廃止、reasonコード導入）: `src/lisp/strategies/strategies.lisp`, `src/lisp/core/discord.lisp`
- [x] バッファ長/最古滞留時間のテレメトリ追加: `src/lisp/core/discord.lisp`
- [x] school側フラッシュ経路のテスト追加（長時間フェーズ耐性）: `src/lisp/tests/scheduler-tests.lisp` or new integration test

## Expert Panel 2026-02-09 (Swarm Consensus Practicality)
- [ ] `*last-swarm-consensus*` を正本1箇所に統一し、Swarm集計の更新経路を接続する: `src/lisp/core/globals.lisp`, `src/lisp/school/school-execution.lisp`, `src/lisp/school/school-voting.lisp`, `src/lisp/school/school-state.lisp`
- [ ] High Council の `proposal` 型を plist に統一し、型不整合の回帰テストを追加する: `src/lisp/school/school-voting.lisp`, `src/lisp/school/school-execution.lisp`, `src/lisp/tests.lisp`
- [ ] Swarm 記述と実装の整合（残すなら配線、外すなら記述削除）を行う: `doc/owners_guide.md`, `src/lisp/school/school-evaluation.lisp`
- [ ] Status 通知の webhook ルーティングを明示して運用事故を防ぐ: `src/lisp/core/config.lisp`, `src/lisp/core/discord.lisp`

## Expert Panel 2026-02-09 (Model Switch / Dual Trend)
- [ ] 通常ボラ時のモデル挙動を1つに固定し、TDDで仕様化する: `src/lisp/core/research-algorithms.lisp`
- [ ] `get-model-prediction` を使わないなら削除、使うなら1箇所に接続する: `src/lisp/core/research-algorithms.lisp`, `src/lisp/school/school-execution.lisp`
- [ ] dual-trend の固定窓/閾値のOOS/WFV検証を行い、失敗なら削除: `src/lisp/core/research-algorithms.lisp`

## Expert Panel 2026-02-09 (Dual Trend Strength)
- [ ] dual-trend を OOS/WFV（コスト込み）で短期検証し、IS負けなら撤去: `src/lisp/core/research-algorithms.lisp`
- [ ] MODEL GATE 抑制件数をテレメトリ/レポートに出す: `src/lisp/school/school-execution.lisp`
- [ ] dual-trend の挙動テストを追加して仕様を固定する: `src/lisp/tests.lisp`

## Expert Panel 2026-02-09 (Pattern Similarity Service)
- [ ] Pattern Similarity Service の追加を正本ドキュメントに反映（ポート/プロトコル/S式）: `docs/llm/SPEC.md`, `docs/llm/INTERFACES.md`, `docs/llm/STATE.md`
- [ ] Data Keeper の保存上限/分離ストレージ方針を決定し明文化: `docs/llm/SPEC.md`
- [ ] Lookahead/過学習対策（Purged CV/Embargo）を設計に明記し、検証経路を定義: `docs/llm/SPEC.md`, `docs/llm/STATE.md`
- [ ] レジーム統合方針（*predicted-regime* への統合など）を設計に反映: `src/lisp/school/school-strategy.lisp`
- [ ] ソフトゲートによるロット減衰の合成順序を仕様化し、テストを追加: `src/lisp/risk-manager.lisp`, `src/lisp/tests.lisp`
- [ ] ゲート判断の Structured Telemetry 出力を追加: `src/lisp/core/logger.lisp` or `src/lisp/school/*`, `docs/llm/STATE.md`

## Expert Panel 2026-02-09 (Pattern Similarity Query)
- [ ] Pattern Similarity QUERYの入力形式をS式OHLCVで正本化し、画像生成はサービス側に集約: `docs/llm/INTERFACES.md`
- [ ] ZMQメッセージサイズ上限と分割方針を仕様化: `docs/llm/INTERFACES.md`
- [ ] S式正本ルールに「バイナリ送信禁止」を明記: `docs/llm/SPEC.md`, `docs/llm/INTERFACES.md`

## Expert Panel 2026-02-09 (Model Gate Practicality)
- [x] `tools/legend_gate_compare.py` のSharpe/PF/MaxDD算出をGuardianの**日次リターン基準**に揃える or Guardian backtestに接続する: `tools/legend_gate_compare.py`, `guardian/src/backtester.rs`
- [ ] 比較ツールに**スプレッド/手数料/ショート**前提を追加し、実運用に近い条件で検証する: `tools/legend_gate_compare.py`
- [ ] モデル切替の固定閾値/窓/ブレンド係数を**設定化 or 廃止**し、説明できない常数を削減する: `src/lisp/core/research-algorithms.lisp`

## Expert Panel 2026-02-10 (Deep Learning Level)
- [ ] Pattern Similarity の学習パイプライン責務（Training Service/batch）をSPEC/STATEに追加: `docs/llm/SPEC.md`, `docs/llm/STATE.md`
- [ ] Purged CV/Embargo 前提のラベル生成・検証仕様を明文化: `docs/llm/SPEC.md`
- [ ] ティック欠損/VAP生成の品質保証方針を定義: `docs/llm/INTERFACES.md`, `docs/llm/STATE.md`
- [ ] 既存NNオンライン学習との責務境界を明確化: `src/lisp/core/executor.lisp`
- [ ] モデル版管理/ロールバック方針を運用に追記: `docs/llm/STATE.md`
