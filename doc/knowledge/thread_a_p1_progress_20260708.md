# Thread A — P1 (検証結果の配線修復) 実装ログ

**日付:** 2026-07-08 JST
**ブランチ:** `claude/thread-a-verification-wiring`
**関連:** [`rebuild_design_fable_P1-P7_20260703.md`](rebuild_design_fable_P1-P7_20260703.md) §2 P1、[`KILL_CRITERIA_20260703.md`](../KILL_CRITERIA_20260703.md)
**ステータス:** P1 完了（コード＋migration＋オフライン検証済）。実DB migration と実feed再生成はオーナー承認待ち。

---

## 着手前の現物確認（設計メモとの差分）

- **この開発箱は `data/library/*` が全空**、`data/memory/swimmy.db`（27.6MB, 当日更新）だけが正本。
  → 「正本 SQL 一本化」は推奨ではなく必然。旧 export.py（.lisp walk）はこの箱では空フィードになる。
- **SQL 実態: 18,503 行** = `:SCOUT` 18,006（育種スカウト雑音）/ `:B` 379 / `:LEGEND` 64 / `:A` 43 / `:S` 10 / `:GRAVEYARD` 1。
  survivor（SCOUT/GRAVEYARD 除く）= **496 件**。
- `oos_sharpe`: **0.0 が 18,496 行**（nil を 0.0 に潰していた）、`>0` は **7 行**、うち **3 行が IS ミラー**（`oos==sharpe`、例 `17.264853` は設計メモの実例と一致）。
- `cpcv_pass_rate>0` は **0 行**（CPCV は一度も正の pass を出していない）。
- 根本原因は2つ:
  1. `strategy` struct が `oos-sharpe` を **0.0 で初期化**（`dsl.lisp:237,248`）→「未検証」と「OOS=0」が区別不能。さらに `run-oos-validation`（`school-validation.lisp:1082`）が 0.0 を「キャッシュ済み結果」とみなして**実OOSを要求しない**副作用。
  2. `upsert-strategy`（`school-db.lisp`）が sharpe/pf/trades 等は DB 値を保全するのに **`oos_sharpe` だけ保全せず**、nil/0.0 の in-memory で上書き＝**実OOSの静かな喪失**。INSERT が `(or oos 0.0)` で nil を 0.0 に強制。

## 変更（コミット単位）

| ファイル | 変更 | 目的 |
|---|---|---|
| `src/lisp/dsl.lisp:237,248` | `(oos-sharpe 0.0)` → `(oos-sharpe nil)`（defstruct と defstrategy 既定） | 未検証＝nil。育種 child は make-strategy 既定で nil 誕生（親 metrics を複製しない: `school-breeder.lisp:1202` は metrics 未設定を確認） |
| `src/lisp/school/school-db.lisp` upsert-strategy | 保全 SELECT に `oos_sharpe` 追加、`cur-oos` 導出（実DB値を保全、未検証は nil）、INSERT 引数を `cur-oos` に | 実OOSのクロバー防止＋nil→SQL NULL |
| `src/lisp/school/school-validation.lisp` handle-oos-backtest-result | OOS結果が IS Sharpe と**完全一致**なら mirror として拒否（保存せず nil のまま、telemetry `oos.mirror_rejected`） | 将来のミラー再発を封じる（purge/embargo 症状の防御ネット） |
| `tools/tribe/export.py` | `--source sql`（既定）追加。SQL columns を正本に、logic は `data_sexp` から。SCOUT/GRAVEYARD 除外 | 正本 SQL 読みに切替。.lisp は表示用派生に降格 |
| `tools/tribe/migrate_p1_oos_nil.py`（新規） | mirror（`oos==sharpe`）＋ 0.0 を NULL 化する one-shot migration（dry-run 既定、transaction、durable ログ） | SQL 正本のOOSを honest 化 |

cl-sqlite の `bind-parameter` は Lisp `nil` を `sqlite3_bind_null` にディスパッチ（`sqlite.lisp:384`）→ `cur-oos=nil` は SQL NULL として書かれることを確認。

## 検証結果（DB コピー上）

**migration（コピーに apply）:** mirror→NULL=3, zero→NULL=18,496 → post: NULL=18,499 / `>0`=4 / mirror 残 0。

**SQL export（migration 済みコピー）:**
```
parsed 496 survivors (PASS 0 / PROVISIONAL 61 / REJECT 435; 0 parse errors)
rank dist: A=43 B=379 LEGEND=64 S=10
oos_sharpe>0 rows: 4 → 全て trades≈35 → 全て REJECT（under-sampled）
mirror rows: oos=None（migration 済）→ REJECT（偽PASS 0）
```
- **PROVISIONAL=61 が設計メモの「PROVISIONAL 61」と一致**、LEGEND=64 も一致 → フィードが正しく survivor 母集団を反映。
- 設計成功判定を満たす: `{OOS>0 かつ trades≥200}` = 0 = feed PASS。**ミラー由来の偽 PASS = 0。**

**回帰:** `tools/tribe/tests`（8件）pass。`export.py`/migration byte-compile OK。**Lisp フルシステム compile+load OK**（sbcl exit 0、`lib/` を PATH に。残る Undefined note `*SYMBOL-SLTP-OVERRIDES-PATH*` は本変更と無関係の既存 forward-ref）。

## 正しい解釈（KILL_CRITERIA 向け）

現在 **PASS=0 は「工場 KILL 確定」ではない**。OOS はこれまで**極小サンプル(35 trades)のA昇格候補にしか走っておらず、trades≥200 の 61 PROVISIONAL には一度も走っていない**。P1 は「今の正直な姿（PASS=0、61 が実OOS 未実施）」を可視化しただけ。
**decisive experiment は P3: この 61 に実コスト OOS/CPCV をバックフィルして PASS が出るか**。出なければ §4 と合わせ KILL 支持、出れば背骨の残りを仕上げる。

## オーナー承認待ちの適用作業（未実行）

1. 実DBへの migration: `python -m tools.tribe.migrate_p1_oos_nil --apply`（先に dry-run 推奨）。
2. 実 feed 再生成: `python -m tools.tribe.export`（既定で SQL 読み）→ `feed/strategies_feed.json` 更新。
3. push（オーナー確認後）。
