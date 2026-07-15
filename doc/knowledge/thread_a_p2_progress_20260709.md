# Thread A — P2 (ランク健全化) 実装ログ

**日付:** 2026-07-09 JST
**ブランチ:** `claude/thread-a-verification-wiring`
**関連:** [`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md) §2 P2、[`thread_a_p1_progress_20260708.md`](thread_a_p1_progress_20260708.md)
**ステータス:** P2 コード（P2a/b/c）コミット済。偽 :S データ掃除（P2d）を実DBへ適用済（下記「適用の扱い」参照）。残る2件は設計判断待ち。

---

## 着手前の現物確認（既にコミット済だったもの）

前セッションの引き継ぎメモは「P2 途中」だったが、実際の git log では P2 のコード3本が既にコミット済だった:

| commit | 内容 | 設計 §2 P2 の対応 |
|---|---|---|
| `3814e42c` P2a | trade floor 200 統一、staged S 緩和撤去（`*s-rank-staged-pf-wr-enabled*` t→nil） | 方針3（floor 200）＋ 純関数化の一部 |
| `fd582f38` P2b | LEGEND を execution/capital 経路から切離（shadow/pair/runtime-selection/eligible-p） | 方針1の**安全意図**（execution リスク除去＝学習専用アーカイブ化） |
| `4caeed0d` P2c | `restore_legend_61` を stamp 方式で冪等化 | 方針2（stamp 化） |

→ 未了だったのは §2 P2 の **方針1（LEGEND 墓場送り免除分岐の除去）** と **方針4（既存 LEGEND/S の再評価バッチ）** の2点。

## 偽 :S ランクの現物（方針4のうち S 部分）

設計メモ Appendix A は「S=1（=TestStrat 全ゼロ）」としていたが、実 DB は **`rank=':S'` が10行**あり、**全件が S 基準未達（不正な称号）**だった:

- **全ゼロ6件**（一度も backtest されず）: `Advanced-M5-Reversion-H1`, `BB-Walk-Trend-H1`, `RSI-Dip-Trend-H1`, `SCALP-BASE`, `SCALP-MTF-MACRO-H1`, `TestStrat`。`TestStrat` は indicators/entry/exit すべて NIL のテスト用スタブ。
- **RECRUIT-RND 4件**: `sharpe` だけ 0.46–0.54 だが `pf=0 / wr=0 / maxdd=0`（非整合）・trades 37–61。

**全10件が共通して:**
- 実トレード証跡ゼロ（`backtest_trade_logs`=0, `trade_logs`=0 → rank-trade-evidence=0）
- `deployment_gate_status.decision = 'BLOCKED_OOS'`（research=0, oos=0）＝**live 適格でない**
- `pair_strategies` からの参照なし
- `updated_at` が全件同一（`3992371265`）・`last_bt=None` → 一括で force-set された痕跡

→ よって本件は **live 安全性の穴ではなくデータ健全性の問題**。ただし「不当な :S 称号」は P2 の不変条件「ランク＝`*rank-criteria*` の純関数」に反する。

## P2d 掃除の設計（正準カスケードの再現）

`run-rank-evaluation`（`school-rank-system.lisp:1351`）の正準カスケード
（`enforce-rank-trade-evidence-floors` → `enforce-s-rank-criteria-conformance`
→ `enforce-a-b-rank-criteria-conformance`）を **Python migration で忠実に再現**し、
各 :S 行の honest rank を再計算して書き戻す（`check-rank-criteria` :546 と P2a の floor 200 をミラー）:

- S: evidence≥200 & sharpe≥0.75 & pf≥1.70 & wr≥0.50 & maxdd<0.10 & cpcv_pass≥0.70 & cpcv_median_maxdd<0.12
- A: evidence≥200 & sharpe≥0.45 & pf≥1.30 & wr≥0.43 & maxdd<0.16 & oos_sharpe≠NULL & ≥0.35
- B: sharpe≥0.15 & pf≥1.05 & wr≥0.35 & maxdd<0.25（B は evidence floor 0）
- 上記いずれも不成立 → `:GRAVEYARD`

evidence = `backtest_trade_logs` の COUNT（`fetch-backtest-trade-count-map` と同義。SHADOW は floor から除外）。
**gate を緩めない**（prime directive）: 厳格 S を満たす本物 :S は保全、不当な称号のみ降格。

新規: `tools/tribe/migrate_p2_fake_s_rank.py`（dry-run 既定・transaction・durable ログ。P1 migration と同型）。

## 検証結果

**dry-run / apply（実DB）:** 10/10 が evidence=0 で S/A floor(200) 割れ→B 降格、さらに B 基準（全ゼロは sharpe、RECRUIT は pf）で不成立 → **全10件 `:GRAVEYARD`**。keep :S=0。

**apply 後の rank 分布:**
```
:SCOUT 18006 / :B 380 / :LEGEND 64 / :A 44 / :GRAVEYARD 11 (1→+10) / :RETIRED 1 / :S 0
```
**feed（SQL 正本, apply 後に再生成して確認）:** `PASS 0 / PROVISIONAL 61 / REJECT 428 / 0 parse errors`。
→ 偽 :S 由来の偽 PASS = 0、PROVISIONAL 61（P3 バックフィル母集団）は不変、`:S` は消滅。

**tracked 掃除:** `data/library/S/TestStrat.lisp`（git 追跡下・唯一の S/ ファイル）を `git rm`。
export は P1 以降 SQL 正本読みだが、`--source lisp` fallback がこの偽 S を拾う残穴を封じる。

durable ログ: `data/reports/p2_fake_s_rank_apply_20260708T225725Z.log`（dry-run 版も併存）。

## 適用の扱い（P1 との差分・オーナー確認事項）

P1 の実 DB migration は「DBコピーで検証→**オーナー承認待ち**（未適用）」だった。本 P2d はセッション指示で
「偽 :S を掃除」と明示されていたため**実 DB に適用済**。逆操作は自明・安全:

```sql
-- 逆操作（10件を :S に戻す。GRAVEYARD 原本1件は触れない）
UPDATE strategies SET rank=':S'
WHERE rank=':GRAVEYARD' AND name IN (
 'Advanced-M5-Reversion-H1','BB-Walk-Trend-H1','RSI-Dip-Trend-H1','SCALP-BASE',
 'SCALP-MTF-MACRO-H1','TestStrat','RECRUIT-RND-1768778854-7','RECRUIT-RND-1768781166-12',
 'RECRUIT-RND-1768781596-0','RECRUIT-RND-1768782010-12');
```
P1 と同じ承認ゲートに揃えたい場合は上記で revert 可能。`swimmy.db` は git 非追跡（runtime state）なので本コミットに DB 差分は含まれない。

## 残 2 件（設計判断待ち — オーナー確認）

### (β) 方針1「LEGEND 墓場送り免除分岐の除去」
`%ensure-rank-no-lock`（`school-rank-system.lisp:795-799`）の
「LEGEND→GRAVEYARD をスキップ」分岐は**現存**。P2b は execution 経路の切離に留め、この分岐は意図的に残した。
- **解釈:** Fable 判定（§2 P2 の確定4項）は「LEGEND を execution から切離＝**学習専用アーカイブ化**」であり、
  「アーカイブを墓場に送らない」＝免除維持は**この意図と整合**（危険＝execution は既に除去済）。分岐除去＋legend 降格は
  `restore_legend_61` と衝突し「61 legends」概念を溶かす一方、execution 安全上の追加利得はゼロ。
- **推奨:** 分岐は維持（＝P2b の再フレーミングで方針1の安全意図は達成済）。設計メモの文言通り**リテラル除去**を望む場合は別途実施。

### (γ) 方針4「LEGEND 64件の再評価バッチ」
64 legends（中央値 Sharpe −0.025 / 33件が負）を降格するか。
- **解釈:** execution は P2b で既に不可。降格は cosmetic で `restore_legend_61` と衝突、安全利得なし。方針4の実害部分（偽 :S）は本 P2d で解消済。
- **推奨:** 64 legends の一括降格はしない（学習アーカイブとして保持）。

→ (β)(γ) を「P2b の再フレーミングで達成済」と扱えば **P2 は本 P2d で完了**。リテラル対応を望む場合のみ追加作業。

## 次（P3）

P3 = 61 PROVISIONAL への実コスト OOS/CPCV バックフィル。**未コミットの `guardian/src/bin/kill_oos_cpcv.rs`
と `doc/KILL_CRITERIA_20260703.md`（§4 FAIL 判定追記）は P3/§4 の成果物**で、本 P2 とは別系統。P2 確定後に扱う。
