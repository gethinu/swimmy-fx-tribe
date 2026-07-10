# Thread A — P3: OOS/CPCV バックフィル拡大＋二重エンジン cross-check（状況整理）

**日付:** 2026-07-11 JST
**ブランチ:** `claude/thread-a-verification-wiring`
**関連:** [`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md) §2 P3、
[`thread_a_p1_progress_20260708.md`](thread_a_p1_progress_20260708.md)、[`../KILL_CRITERIA_20260703.md`](../KILL_CRITERIA_20260703.md)

---

## P3 の3要素と現状

| 要素 | 状態 | 根拠 |
|---|---|---|
| (1) 61 PROVISIONAL 全件へ実コスト OOS/CPCV **バックフィル** | **実走済**（2026-07-08） | `logs/kill_criteria_20260708/`（gitignore・ローカル保全）。standalone Rust bin `guardian/src/bin/kill_oos_cpcv.rs` が 61 件全て評価 |
| (2) honest gate へ PASS/REJECT を接続（KILL_CRITERIA §4） | **判定確定** | §4 FAIL → §5 (C) 畳む。KILL_CRITERIA doc に追記（本コミット） |
| (3) 二重エンジン（Python 5580 / Rust 5556）cross-check | **未実施**（本セッションでは着手せず・下記理由） | 両エンジン停止中。アーキテクチャ上 5580=Python dispatcher→guardian subprocess のため「独立2実装の突合」は自明でない |

## (1)(2) の実体（2026-07-08 の決定的実験）

- 対象: `feed/strategies_feed.json` の `gate.verdict==PROVISIONAL` = ちょうど 61 件（全 USDJPY・全 :TREND）。
- コスト: 往復2pip 保守（slippage=0.01 両側）、penalized(Taleb haircut) sharpe でゲート。
- データ: `data/historical/USDJPY_M1.csv`（mt5_Bundle 由来 2015–2024, 3,721,228 本, 検証済・gitignore）。
- 忠実再現: `strategy-to-alist` が entry/exit AST を送らない実挙動を再現（SMA(short/long) クロス+SL/TP）。
  抽出 IS trades 中央値 4773 が autopsy §1 と一致（fidelity 二重一致）。
- **結果:** OOS 素通り 23/61 だが CPCV 両立は **1件のみ**、多様性 0（61/61 USDJPY・:TREND）、
  CPCV median sharpe の母集団中央値 −0.13。→ §4 の3条件すべて不成立 → **(C) 畳む**。
- 詳細: `logs/kill_criteria_20260708/{00_PROVENANCE_AND_METHOD,01_RESULTS_AND_VERDICT}.md`。

## 本コミットで確定させるもの

- `guardian/src/bin/kill_oos_cpcv.rs` — 実験ツール本体（これまで未コミット）。※`cargo check` で compile 検証。
- `doc/KILL_CRITERIA_20260703.md` — §4 の実測・FAIL 判定・(C) 畳むを履歴表に追記（P3/§4 成果物）。
- 本ドキュメント。

証跡（`logs/kill_criteria_20260708/`）と価格データ（`data/historical/USDJPY_M1.csv`）は **gitignore**
（大容量・runtime）。パスと SHA 的事実は上記ドキュメント群に固定済み。

## (3) 二重エンジン cross-check — 未着手の理由と必要物

**目的:** kill 実験の standalone 再現（`kill_oos_cpcv.rs` in-process）が本番 guardian 経路と一致することの
独立検証（＝検証ツール自体の頑健性チェック。片方のエンジンのバグが KILL 判定を歪めていないことの担保）。

**現状の障壁:**
1. 両エンジン停止中（`netstat` に 5580/5556 なし）。起動が必要。
2. `tools/backtest_service.py`（5580）は独立 backtester ではなく **guardian サブプロセスへの dispatcher**。
   よって「Python 実装 vs Rust 実装」の素朴な二重化にはならない。意味ある cross-check は
   「`kill_oos_cpcv` standalone 結果 vs live guardian（5556 or 5580→guardian）BACKTEST 結果」の突合。
3. guardian の release/debug ビルドキャッシュが無く、cold build（zmq crate 含む）が必要。

**方針判断:** §4 は既に FAIL→畳む で確定しており、(3) は「判定の追加担保」であって判定を覆すものではない。
両 ZMQ サービス起動＋guardian cold build＋新規突合ハーネスは重く、fold 済みの工場に対する限界価値は小さい。
→ 本セッションでは (1)(2) の確定・コミットまでを P3 の成果とし、(3) は**要否をオーナー判断**とする
（実施する場合の最小手順は上記「必要物」）。**ライブ発注なし・オフライン検証のみ**は全工程で厳守。
