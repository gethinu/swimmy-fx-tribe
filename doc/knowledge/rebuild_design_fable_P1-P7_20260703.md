# 🏗️ Swimmy 再構築設計メモ — 背骨だけ部分再構築（P1〜P7）

**作成日:** 2026-07-03 JST
**ステータス:** Draft（実装フェーズ着手前のハンドオフ資料）
**著者:** Claude（Opus 4.8）による docs/コード根拠レビュー
**対象コミット時点:** `master` 系列（feed 生成 2026-06-24、library 168 survivors）

> [!IMPORTANT]
> **本ドキュメントの性質**: 以下は Claude によるリポジトリ実コード＋docs 根拠の分析と、
> それを Fable に精査させるための設計指針である。**Fable による深掘り精査はまだ実行して
> いない**。各節末尾の「🔬 Fable 要確認」は、Fable（または実装担当）が実コードを追って
> **確定させるべき未決事項**であり、確定済みの結論ではない。確定事実には file:line を付す。
> P1〜P7 に対応する Fable 用の自己完結プロンプトは本メモとセットで別途保持している。

---

## 0. 総括判定

**「現状延命 vs 全面再構築」は誤った二択。正解は "背骨（検証パイプラインの一貫性）だけを作り直す" 部分再構築。**

- **全面 rewrite は損**: 健全な資産（Guardian の Rust アリーナ約9.4k行、`honest_gate.py`、DSL/育種ループ）を巻き添えにする。
- **現状延命も不可**: 「ランクが損益と無関係（LEGEND が損失戦略を不死化）」「フィード PASS が構造的に 0」を温存し、工場の出力が永久に無価値のまま。
- **致命傷はアーキテクチャ欠陥ではなく "配線の断絶" と "規律の欠如"**。したがって背骨（検証結果の書き戻し → ランク健全化 → 検証スループット）を作り直せば、既存資産の上で工場が初めて「正直に PASS>0」を出せる。

### 作業の束ね方
| スレッド | 内容 | 依存 |
|---|---|---|
| **A（P1→P2→P3）** | 検証結果の配線修復 → ランク健全化 → 検証スループット | 連鎖。順に実施 |
| **B（P4/P5）** | 信頼性（クラッシュループ/メモリ/ヒープ）＋永続化の破損耐性 | A と独立・並行可 |
| **C（P6/P7）** | 契約ドキュメント一元化＋低リスク一括クリーンアップ | 独立・並行可 |

---

## 1. 現状の構造的評価

### 1-A. 強み（残す資産）
| 資産 | 根拠 | 価値 |
|---|---|---|
| 正直な gate と feed 契約 | `tools/tribe/honest_gate.py:36-45,149-185`, `doc/FACTORY.md` | PASS=OOS/CPCV検証済、PROVISIONAL=IS-only、REJECT=floor割れ。DDバジェット正規化 `budgeted = headline × dd_budget/dd_observed`。**最も誠実な部分。** |
| Guardian Rust アリーナ | `guardian/src/{backtester.rs(79KB), cpcv.rs(24KB), lstm,ppo,neural,mcts,ensemble,kalman,tournament}.rs`, `main.rs(133KB)` | CPCV(López de Prado)含む本物の OOS 検証エンジン。subtree で in-repo 化済（`doc/OPERATIONS.md:64-113`）。 |
| ポート契約の実装整合 | `docs/llm/INTERFACES.md:7-19` ↔ `config.lisp:103-107` ↔ `guardian/src/main.rs:24-29` | 5557/5560/5555/5556/5559/5562 が3面一致。**実装は揃っている**（ドキュメントのみ drift）。 |
| 育種ループの実在 | `school-connector.lisp:269-341`(1s周期), `school-breeder.lisp`, `school-genome.lisp` | 交叉/トーナメント/変異、遺伝的距離0.2-0.8制御。RPではなく実GA。 |
| ナラティブ層は薄い | rituals/narrative/constitution 合計 約1.7k LOC（全66k の約2.6%） | 俗説と逆で**問題ではない**。削る対象ではない。 |

### 1-B. 致命的欠陥（実測で確認済み）
| # | 欠陥 | 根拠 |
|---|---|---|
| ① | **フィード PASS=0 は配線断絶**: OOS/CPCV結果が SQL には書かれるが `data/library/*.lisp` に書き戻らない。export は .lisp を読むので常に `OOS-SHARPE 0.0`。 | `school-validation.lisp:995-1027`(`upsert-strategy` のみ, `save-strategy` 無し), `export.py:68-101` |
| ② | **ランクが損益と乖離**: `LEGEND` 64件は中央値 Sharpe −0.025、33/64 が負、63/64 が <1.0（定義は >2.0）。しかも墓場送り免除で不死化。 | 実測（下記 Appendix A）, `school-rank-system.lisp:23-28,786-794`, `systemd/swimmy-school.service:11` |
| ③ | **クラッシュループ無防備**: school/brain は `Restart=always`＋`StartLimit` 無し。school-daemon は未捕捉例外で即 exit 1。 | `systemd/swimmy-school.service`, `swimmy-brain.service`, `school-daemon.lisp:10-17` |
| ④ | **graveyard 書き込みにアトミック保証なし**: 中断で `data_sexp` 破損 → safe-read が黙って読み飛ばし、失敗パターンが静かに消失。 | `school-cemetery.lisp:67-73`, `school-kb.lisp:66-118` |
| ⑤ | **規律負債**: 平文DBパスワード、.fasl 4件コミット、契約docの stale、v1/v2重複。 | `db-adapter.lisp:17`, `git ls-files`, `HANDOFF.md`, `REFERENCE.md` |

---

## 2. スレッドA（P1→P2→P3）＝背骨の作り直し

### P1. 検証結果の配線修復（PASS=0 の根治）

**A. 根本原因（確定）**
- gate の PASS 条件: hard floor 通過 **かつ** `oos_sharpe>0 or cpcv_pass>0 or cpcv_sharpe>0`（`honest_gate.py:149-156`。未検証は `warn=True`→PROVISIONAL 止まり `:180-185`）。
- export は各 `data/library/<rank>/<name>.lisp` を読む（`export.py:68-101` の `_gate_candidate` が `m.get("oos_sharpe")` 等）。
- OOS結果ハンドラ `handle-oos-backtest-result`（`school-validation.lisp:1016-1027`）は `(setf (strategy-oos-sharpe strat) sharpe)` → `(upsert-strategy strat)` で **SQL のみ**更新。`swimmy.persistence:save-strategy`（.lisp 再書き出し、`persistence.lisp:69-100`、temp→rename でアトミック）は**呼ばれない**。CPCV も同様（`message-dispatcher.lisp:726-732` 付近で `strategy-cpcv-*` を setf→`upsert-strategy`）。
- 結果、library .lisp の `OOS-SHARPE` は 0.0 のまま（168件中133件が 0.0）。
- **二重の詰み**: 非ゼロ OOS を持つ 35件は、値が in-sample SHARPE の**完全ミラー**（例 `SHARPE 17.264853 / OOS-SHARPE 17.264853 / TRADES 35`）で、独立 OOS ではなく育種時の複製。しかも trades=35 < `min_trades=200` で REJECT。→ 配線を直しても sample-size で落ちる母集団。

**B. 再設計（single source of truth）**
- 正本を **SQL `strategies` に一本化**し、`export.py` を「SQL 読み」に変更する案を第一候補とする（マルチプロセス整合が最も素直）。対抗案は「OOS/CPCV確定時に必ず `save-strategy` を呼び .lisp を正本のまま最新化」。
- OOS 未実施は `0.0` ではなく **未検証(nil)** で表現（gate 側の `oos_sharpe is not None` 判定と整合）。育種時の OOS ミラー複製を禁止。

**C. パッチ方針（最小変更順）**
1. OOS/CPCV 確定パス（`school-validation.lisp:1026`, CPCVハンドラ）に `save-strategy` 追加、または export の読み口を SQL に切替。
2. 既存 35件のミラー OOS を一括 nil 化する one-shot migration。
3. `python3 -m tools.tribe.export` 再生成。
- 成功判定: 「trades≥200 かつ本物 OOS>0」の件数が feed の PASS に一致して増える。ミラー由来の偽 PASS が 0。

**D. リスク**: 配線修復で見かけ上 PASS が急増し誤って本番投入される危険。gate の floor（trades200/PF1.10/Sharpe0.10/DD12%）は**緩めない**前提を固定し、PASS→live の間に別ゲート（deployment_gate_status）を必須化。

**🔬 Fable 要確認**
- 正本を SQL に寄せるか .lisp 最新化かの最終判断（`tools/ops/reconcile_archive_db.py` の前提と衝突しないか）。
- SQL↔.lisp の既存ドリフト（どちらが新しいか不定）を解消する一回きり migration の設計。
- 35件ミラーの混入箇所（育種のどの関数が OOS=SHARPE を複製しているか）の特定。

---

### P2. ランク健全化（損益と乖離した称号の廃止）

**A. 原因（確定）**
- メトリクス連動の基準は B/A/S のみ（`school-rank-system.lisp:23-28`）。**LEGEND は `*rank-criteria*` に存在しない**＝検証されないランク。
- LEGEND は墓場送り免除（`:786-790` の「Legend protection」分岐で `GRAVEYARD` 遷移を握り潰す）。加えて school 起動毎に `ExecStartPre=... tools/restore_legend_61.lisp`（`swimmy-school.service:11`）で 61件を復元。ヘッダコメント `:legend - Protected strategies (61 total, never discarded)`。
- 内部 trade floor（`*a-rank-min-trade-evidence*=50`, `*s-rank-min-trade-evidence*=100`, S段階緩和は 30 trades まで `:66-76`）と外部 gate（`min_trades=200`）が**不整合**＝二重基準。

**B. 再設計**
- ランク＝`gate.verdict`（P1のPASS/PROV/REJECT）または `*rank-criteria*` の**純関数**にする。称号ではなく検証結果を一次表示に。
- LEGEND を **execution 経路から切り離す**（「歴史的アーカイブ＝execution不可・資金配分ゼロ・学習専用」への降格）。損失戦略の不死化を廃止。
- trade floor を **200 に統一**（段階緩和は overfit を通す穴として要精査）。

**C. パッチ方針**
1. `:786-790` の LEGEND保護分岐を除去 or アーカイブ化。
2. `restore_legend_61.lisp` の毎起動実行を停止/改修（P4 の起動負荷とも連動）。
3. trade-evidence floor を 200 に統一。
4. 既存 LEGEND 64件・S 1件（=`TestStrat` 全ゼロプレースホルダ）を再評価するバッチ。

**D. 安全確認**: 変更で実弾 execution 対象が変わる。有望株誤殺/損失株延命を防ぐ検証ゲートを migration 前に通す。

**🔬 Fable 要確認**
- LEGEND が実際に execution 許可・資金配分に効いているか（`REFERENCE.md` は LEGEND=Execution Allowed）。効いていれば最大リスク。
- 完全廃止 vs アーカイブ降格の判断。
- S段階緩和（30 trades で S）を残すか。

---

### P3. 検証スループット（誰をいつ検証に回すか）

**A. 実データフロー（確定＋要確認）**
- メインループ（`school-connector.lisp:269-341`, 1s周期）: `phase-1-validation`（IS backtest）→ `phase-3-qualify`（WFV/OOS）→ `phase-3-5-cpcv-validate`（A→S のみ）→ `phase-4-purge`（**純IS淘汰**）→ `phase-6-breeding`。`phase-5-recruit` はコメントアウト。
- backtest リクエストは **Python Backtest Service（ZMQ 5580）** へ（`school-backtest-v2.lisp:75-82` の `send-zmq-msg ... :target :backtest`）。単一CSV `data/historical/USDJPY_M1.csv` を丸読み、train/test は `start_time/end_time` 任意指定。
- 一方 Guardian(Rust) は **5556** で `BACKTEST`/`CPCV_VALIDATE` を**自前処理**（`main.rs:1967-1990`、`serde_lexpr`/`serde_json` dual-parse）。→ **通常backtest=Python(5580)、CPCV=Rust(5556) の二重系統の疑い**。

**B. 再設計**
- OOS/CPCV は現状「昇格候補のみ」＝母集団が狭い。P1 の書き戻しを直しても、検証にエントリーする母集団が狭ければ PASS は増えない。**PROVISIONAL 全件を検証に回す**スループット・アーキテクチャ（キュー・優先度・容量計画・バックプレッシャ）。
- 二重エンジンを統合 or 明確分離（コストモデル/スプレッド/スワップの一致を保証）。
- `phase-4-purge` の純IS淘汰の前に軽量 OOS ゲートを挟む是非。

**C. パッチ方針**: 検証母集団を「昇格候補限定 → PROVISIONAL 全件」へ拡大し、必要容量・レート（`SWIMMY_BACKTEST_WORKERS`/`*_MAX_INFLIGHT`）を再設定。段階導入。

**D. A/B/C 統合確認**: P1（書き戻し）＋P2（ランク健全化）＋P3（母集団拡大）を通して初めて「PASS>0 が正直に出る」ことのシナリオ検証。

**🔬 Fable 要確認**
- Python(5580) と Rust(5556) の二重 backtest が意図的か事故か。コストモデル一致の検証。どちらが「正」か。
- 単一CSV丸読み＋任意 start/end で walk-forward の purge/embargo（López de Prado）が正しく効いているか。
- 全 survivor を検証しきる容量が現ハードで足りるか（P4 のメモリ制約と連動）。

---

## 3. スレッドB（P4/P5）＝信頼性・永続化

### P4. クラッシュループ／メモリ／ヒープ

**A. 障害モード（確定）**
| ユニット | Restart | StartLimit | 備考 |
|---|---|---|---|
| swimmy-school | always / 10s | **無し** | `school-daemon.lisp:10-17` 未捕捉例外で即 exit 1。ExecStartPre で毎回 restore_legend_61 |
| swimmy-brain | always / 10s | **無し** | `Environment=SWIMMY_SBCL_DYNAMIC_SPACE_MB=4096` |
| swimmy-backtest | on-failure / 10s | **300s/5**（正しい） | 参照実装 |
| swimmy-data-keeper | always / 10s | `[Unit]` に 300s/5 あり | だが `Restart=always` で burst 到達後の恒久停止と on-failure の使い分けが未整理。運用メモは「Unknown key」を報告（実害要判定） |

- カスケード仮説: school 無限再起動 → 起動毎に restore_legend_61＋initialize-system でメモリ再確保 → 他プロセスを swap へ → OOM 連鎖。実測 VmmemWSL ~6.8GB / 物理14GiB / swap 1.8GB 使用（`expert_panel_20260213.md:5-9`）。

**B. ヒープ整合（確定）**
- `run.sh:69,71`（現物 Ver 41.5）は `SWIMMY_SBCL_DYNAMIC_SPACE_MB:-6144` で **env 尊重・既定6144MB**。
- systemd(brain/school) が `Environment=...=4096` で上書き＝**本番実効 4096MB**。
- 運用メモ `expert_panel_20260213.md` の「run.sh:64 が4096固定でenv無視」という批判は**現物と不一致（stale）**。ドキュメント修正対象。
- `STATE.md:2166`: `load-graveyard-cache` が既定ヒープで枯渇しうる（診断時 ≥2048 推奨）。

**C. 再設計／パッチ方針**
- systemd 復旧ポリシー統一規格: **脳系＝on-failure＋burst＋通知＋degraded運転**、補助系＝always＋burst。brain/school に `StartLimitIntervalSec/Burst` を**正しいセクション（`[Unit]`）**で付与。
- ヒープ/ワーカーのメモリ配分を物理14GiB前提で数値設計（brain/school/data-keeper/guardian の合計が swap を叩かない）。
- graveyard-cache 枯渇の緩和（TTL/インクリメンタル）。
- 検証: 「毒データ1件で無限再起動しない」「burst到達で通知」「swap 定常0付近」。

**🔬 Fable 要確認**
- data-keeper の StartLimit 実害（`[Unit]` 配置だが `Restart=always`）の最終判定。
- crash-loop ガード付与時の「取引脳が落ちたまま」リスクと degraded 運転設計。
- restore_legend_61 を毎起動で走らせる是非（P2 と連動）。

---

### P5. 永続化の破損耐性

**A. 破損耐性マップ（確定）**
| 層 | 読み | 書き |
|---|---|---|
| library `.lisp` | safe-read | **アトミック（temp→rename, `persistence.lisp:69-100`）＝安全** |
| SQL `strategies`（graveyard含む, `data_sexp`列） | safe-read | **INSERT OR REPLACE 単発（`upsert-strategy`）。トランザクション境界/耐久設定が要点** |
| `data/memory/graveyard.sexp` | `read`＋`(error()nil)` スキップ | **追記形式・アトミック保証なし** |

- **非対称の穴**: safe-read が破損を隠蔽するため、書き込み破損によるデータ欠損に**気付けない**（クラッシュしないが失敗パターンが静かに消失）。`cemetery-audit-db` が batch 毎 `(sb-ext:gc :full t)`＝メモリ逼迫の兆候。

**B/C. 再設計／パッチ方針（低リスク順）**
1. SQLite PRAGMA（WAL＋`synchronous=NORMAL`）でクラッシュ耐性向上。
2. `save-strategy` の `<name>.tmp` 固定名を PID/uuid で一意化（並行書き込み衝突回避）。
3. 欠損検知監査（graveyard 件数の単調性、hash `:INVALID` 比率）。
4. `data_sexp` 肥大対策（正規化 or サイズ上限）。

**🔬 Fable 要確認**
- SQLite の現行 journal/synchronous 設定と `upsert-strategy`/`cemetery-audit-db` のトランザクション境界。
- マルチプロセス（brain/school 別プロセス）での `upsert-strategy` 排他（コメントに「should ideally use lock if running in parallel」）。
- 既存の `:INVALID` hash 行の移行方針。

---

## 4. スレッドC（P6/P7）＝契約一元化・クリーンアップ

### P6. 契約ドキュメントとコードの真実一元化

**A. 差分表（確定）**
| 項目 | 宣言 | 実装 | 判定 |
|---|---|---|---|
| Brain↔Guardian ポート | HANDOFF.md: 5558/5559 | main.rs:25-26 は 5555/5556 | **HANDOFF stale（5558は不在）** |
| トレードコマンド形式 | REFERENCE.md: JSON 例 | 実配線は S式（`cmd_route.rs:88-111`） | **REFERENCE 誤解を招く** |
| ZMQ エンコーディング | INTERFACES.md:5「S式のみ・JSON非受理」 | main.rs:1957-1981 は 5556/5559 で **dual-parse（S式+JSON）** | **正本の記述が実装と不一致** |
| ポート実体 5557/5560/5555/5556/5559/5562 | INTERFACES.md:7-19 | main.rs:24-29, config.lisp:103-107 | **一致** |

**B/C. 一元化設計／パッチ方針**
- single-source を **INTERFACES.md** に固定、他は派生（リンク/自動生成）。
- dual-parse を「締める（JSONパス削除）」か「正本を実装に合わせる」かを判断。
- HANDOFF.md に deprecated 明記、REFERENCE.md の JSON例→S式化、INTERFACES.md の「JSON非受理」記述を実装整合。
- 回帰防止: `cmd_route.rs` の type/action 集合と INTERFACES.md の突合テストを CI に。

**🔬 Fable 要確認**
- dual-parse（JSON受理）が後方互換の遺物か意図的か。締めるコスト/リスク。
- MT5(MQL5)側は別リポで本文に無い。5557/5560 境界フォーマットは要確認。

---

### P7. 低リスク一括クリーンアップ

**A. チェックリスト（確定）**
| 項目 | 根拠 | 分類 | リスク |
|---|---|---|---|
| 平文DBパスワード `"swimmy_password"` | `db-adapter.lisp:17`（ただし既定 `*db-mode* :jsonl` で postgres時のみ使用） | 即対応 | 低（履歴残存が主） |
| .fasl 4件コミット | `discord.fasl`, `message-dispatcher.fasl`, `school.fasl`, `evolution-tests.fasl` | 即対応 | 低 |
| backtest v1/v2 併存 | `school-backtest.lisp`(867行) と `-v2.lisp` | 計画 | 中（呼出元トレース要） |
| no-op/stub 3件 | `prediction.lisp:336`, `school-lifecycle.lisp:60`, `school-evolution-llm.lisp:13` | 計画 | 低 |
| tests.lisp 17k行 | 手書きで正当、setup重複 | 放置可（共通化候補） | 低 |

**B/C. パッチ方針**
- DBパスワード: `*db-conn-params*` を `getenv-or-dotenv` 化（jsonl既定を壊さない）。
- .fasl: `.gitignore` 追記＋`git rm --cached`、再生成確認。
- backtest v1/v2: 呼出元 grep→段階廃止。
- 再混入防止: `.fasl`・平文secret の pre-commit フック。

**🔬 Fable 要確認**
- v1 のみが持つ機能が無いか（安全な廃止可否）。
- git 履歴の平文secret 除去（filter-repo）は破壊的＝別オペレーションとして切り出し。

---

## Appendix A. 実測値（本レビューで直接確認）

- feed（`feed/strategies_feed.json`, 生成 2026-06-24）: `total=168, PASS=0, PROVISIONAL=61, REJECT=107`。gate_config: `dd_budget=12%, kpi=3%/mo, min_trades=200, min_pf=1.10, min_sharpe=0.10`。
- library 分布: S=1（=`TestStrat` 全ゼロ）, A=9, B=93, LEGEND=64, RETIRED=1。
- LEGEND 64件 Sharpe: 中央値 −0.025 / 負 33件 / <1.0 が 63件 / 最小 −5.60 / 最大 1.4。例 `LEGEND/Aggressive-Reversal.lisp`: `SHARPE -0.058, PF 0.87, WR 0.4, TRADES 130`。
- 非ゼロ OOS 35件（A7/B26/LEGEND2）は `OOS-SHARPE == SHARPE` のミラー（例 trades=35 で Sharpe 17.26）。
- 規模: Lisp 66,519行 / 168ファイル（school 79ファイル）、Guardian Rust 約9.4k行 / 15ファイル。systemd 35 service＋17 timer。doc/docs 259ファイル、STATE.md 2619行、expert_panel 26本。

## Appendix B. 実装フェーズへの落とし込み順序（推奨）

1. **A-P1**（書き戻し修復）→ feed に本物 PASS が出る土台。
2. **A-P2**（ランク健全化）→ LEGEND execution リスク除去。
3. **A-P3**（検証母集団拡大）→ PASS を実数として増やす。
4. **B-P4/P5**（信頼性・永続化）→ 並行。長期運転の前提。
5. **C-P6/P7**（契約・掃除）→ 並行。事故率と負債の低減。

各 P には Fable 用の自己完結プロンプト（背景＋実コード抜粋＋検証観点＋期待アウトプット A/B/C/D）を別途用意済み。実装着手時はプロンプト→Fable 精査→本メモの「🔬 要確認」確定→パッチ、の順で進める。
