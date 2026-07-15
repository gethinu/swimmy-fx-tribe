# Thread A — P2e/P2f: LEGEND を LEGEND-ARCHIVE へ退役（P2 締め）

**日付:** 2026-07-11 JST
**ブランチ:** `claude/thread-a-verification-wiring`
**前提:** [`thread_a_p2_progress_20260709.md`](thread_a_p2_progress_20260709.md) の「残 2 件（設計判断待ち）」= (β) LEGEND 墓場送り免除分岐、(γ) LEGEND 64件の再評価。
**ステータス:** P2e コード実装・検証済（テスト 582 passed / 11 failed = ベースライン同一・回帰ゼロ）。P2f データ移行は別記。

---

## オーナー決定（β/γ の確定）

前セッションが AskUserQuestion（β/γ の扱い）で停止。オーナー判断で確定:

- **LEGEND を execution 経路から完全に切り離す**（P2b で `*execution-ineligible-ranks*` に `:legend` 済）。
- **hard-delete しない**。quarantine＝件数保存・監査用に残す（no-hard-delete 方針＋永続化設計）。
- 具体: LEGEND を「**学習専用アーカイブ・資金配分ゼロ・execution 不可**」へ**降格**。
  `restore_legend_61` の毎起動復元で損失戦略が不死化する経路を断つ。
- **ランク＝`*rank-criteria*` の純関数化**（trade floor 200 統一・段階緩和撤去は P2a 済）。
- 327 の未追跡ライブラリファイルは未追跡のまま残置（コミットに入れない・hard-delete しない）。

## 退役先＝既存の `:LEGEND-ARCHIVE` ランク

コードには既に「アーカイブ・execution 不可・保全」専用のランク `:LEGEND-ARCHIVE` が実在:

- `*execution-ineligible-ranks*`（school-execution.lisp:897）に含まれる → 資金配分ゼロ・スロット不可。
- 永続化/ライブラリ層で GRAVEYARD/RETIRED/ARCHIVE と同群の**アーカイブ**扱い（persistence.lisp:27,54 ほか）。
  ただし *legend* の出自を保持 → `:GRAVEYARD`（失敗リクルートと混同）とは別。**「61 legends」の監査痕跡（件数）を保存**。
- アクティブ・ランクのカスケード / 育種 / revalidation queue から除外（school-rank-system.lisp:1495,1513；legend-61.lisp:184）。

→ これが決定の「学習専用アーカイブ・資金配分ゼロ・execution 不可」の**そのままの受け皿**。よって
**クラス退役 = 64 LEGEND → LEGEND-ARCHIVE**（純関数ラダーへの個別再ランクではなく、レガシー LEGEND クラスの一括退役）。
KILL_CRITERIA §4 FAIL →（C）畳む、と整合。

## 不死化の3本柱と、それを断つ変更

| 柱 | 実体 | 対応 |
|---|---|---|
| 1. `restore-legend-61` | 61件を `:legend`＋`immortal=t` で強制復元 | **P2c** で stamp 冪等化（毎起動→署名変化時のみ）＋ **P2e** で `:LEGEND-ARCHIVE` 行は復元スキップ（sticky） |
| 2. `%ensure-rank-no-lock` β分岐 | `LEGEND→GRAVEYARD` を無条件ブロック＝不滅の :LEGEND | **P2e** で `LEGEND→LEGEND-ARCHIVE` に**リダイレクト**（純関数・保全・execution 不可） |
| 3. `handle-v2-result` | legend revalidation は rank 不変（metrics only） | 変更不要。退役後は `:LEGEND-ARCHIVE` を維持するのが正（アクティブ revalidation は queue が :legend のみで到達せず） |

## P2e 実装（コミット対象）

1. **`src/lisp/school/school-rank-system.lisp`**（`%ensure-rank-no-lock`）
   β分岐を「ブロック（:LEGEND 据置）」から「`:LEGEND-ARCHIVE` へ退役」に変更。以降の GRAVEYARD 物理削除・
   failure-pattern 保存・graveyard ディレクトリ移動（:911-934）は new-token≠"GRAVEYARD" となり**バイパス**
   （＝KB から物理削除されず保全）。`*allow-rank-regression-write*` により降格書込は許可。
2. **`src/lisp/strategies/legend-61.lisp`**（`restore-legend-61`）
   ループ先頭で権威 DB ランクを照会（新ヘルパ `%legend-current-db-rank-token`）。`LEGEND-ARCHIVE` なら
   **復元スキップ**（sticky archive）。意図的退役が restore で不死化する経路を封鎖。
3. **`tools/restore_legend_61.lisp`**
   `*legend-restore-stamp-version*` 1→2（ロジック変更の契約に従い、次回ブート時に一度だけ再照合）。
4. **`src/lisp/tests.lisp`** 追加2件（run-all-tests に登録）:
   - `test-ensure-rank-legend-graveyard-routes-to-legend-archive`：:legend→:graveyard 要求が :legend-archive に退役（:graveyard でも :legend 据置でもない）。
   - `test-restore-legend-61-skips-sticky-legend-archive`：restore-legend-61 が :legend-archive 行を復元しない。
   既存の heal 系（`...resurrects-db-archived-rank...` / `...noop-heals-archived-db-rank`）は stale rank に `:graveyard`
   を使うため無影響（`:legend-archive` 分岐は発火せず）。

## 検証

- ベースライン（未改変・HEAD）: **580 passed / 11 failed**。
- P2e 後: **582 passed / 11 failed**（+2 = 新規テスト、両方 PASS）。**失敗集合はベースラインと完全一致**
  （deployment-gate / archive-reconcile / source-drift / telemetry-fallback の既知の環境依存 11 件）。→ 回帰ゼロ。
- 実行環境: 本ネイティブ Windows で `%SWIMMY_HOME%\lib`（sqlite3.dll ほか）を PATH に載せ、
  `~/quicklisp/setup.lisp` をロードして `(swimmy.tests:run-all-tests)`。

## gate は緩めない（prime directive）

本変更はランクの**正直化**のみ。S/A/B の floor・criteria は不変（P2a の 200 統一を含む）。LEGEND を
「不滅の称号」から「正直なアーカイブ」に落とすだけで、execution 安全は P2b で既に確立済。

---

## P2f: 実 DB 移行（64 :LEGEND → :LEGEND-ARCHIVE）

新規ツール `tools/tribe/migrate_p2_legend_archive.py`（P2d と同型：dry-run 既定・単一トランザクション・
durable ログ・逆操作明記）。**一括退役**（純関数ラダーへの個別再ランクではない）。

**現物（apply 前）:** `:LEGEND` 64件（うち negative-sharpe 31）、`:LEGEND-ARCHIVE` 0件。
64件のうち 61 は canonical 署名戦略、残りは過去セッションのテスト汚染（例 `TEST-REFRESH-ACTIVE-SEXP-SYNC`
＝trades=0 の不当 :LEGEND）を含む。不当な称号こそ純関数原則で退役対象なので一括退役に含める。

**apply 後の rank 分布:**
```
:SCOUT 18006 / :B 380 / :LEGEND-ARCHIVE 64 (0→+64) / :A 44 / :GRAVEYARD 11 / :RETIRED 1 / :LEGEND 0
```
不変条件: remaining `:LEGEND` = 0、`:LEGEND-ARCHIVE` の増分 = 64（＝退役件数）。冪等（再実行で 0件対象）。

durable ログ: `data/reports/p2_legend_archive_dryrun_20260710T232031Z.log` /
`..._apply_20260710T232129Z.log`。

**逆操作（承認ゲートに揃える場合）:** apply 前に `:LEGEND-ARCHIVE` 0件だったため厳密可逆:
```sql
UPDATE strategies SET rank=':LEGEND' WHERE rank=':LEGEND-ARCHIVE';
```
`swimmy.db` は git 非追跡（runtime state）なので本コミットに DB 差分は含まれない。

**restore との整合:** stamp を v2 に上げたため次回ブートで restore-legend-61 が一度だけ再走 →
64件全て `:LEGEND-ARCHIVE` 検出でスキップ（復元されない）→ v2 stamp 書込で確定。以降は署名不変なら
skip。不死化ループは完全に断たれる。

## 検証（P2f 後）＋ テスト密閉性の修正

実 DB 移行**直後**のフル再走は **580 passed / 13 failed**。差分の2件は
`test-restore-legend-61-updates-existing-timeframe-from-optimized-map` と
`...-applies-optimized-timeframe-when-adding`。両者は canonical 名 `Aggressive-Reversal` で
tmp-db を張らず、P2e で追加した `%legend-current-db-rank-token` の DB 照会が **live-DB** を読み、
移行後の `:LEGEND-ARCHIVE` を検出して skip したため（＝ambient live-DB 依存だった旧テストの露呈）。

**修正:** 両テストで `%legend-current-db-rank-token` を nil にモック（他の副作用同様に密閉化）。archive-skip
自体は専用の `test-restore-legend-61-skips-sticky-legend-archive`（tmp-db + :legend-archive）が担保。

**再検証:** フル再走 → **582 passed / 11 failed**、失敗集合はベースライン同一（回帰ゼロ）。
live-DB のランク書換がどの通過テストも壊さないことを確認。
