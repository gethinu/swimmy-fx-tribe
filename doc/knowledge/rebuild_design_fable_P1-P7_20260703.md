# 🏗️ Swimmy 再構築設計メモ — 背骨だけ部分再構築（P1〜P7）

**作成日:** 2026-07-03 JST
**更新日:** 2026-07-03 JST（Fable 精査結果を反映・実装優先順位を再編）
**ステータス:** Fable 精査反映済 / 実装フェーズ着手可
**著者:** Claude（Opus 4.8）レビュー ＋ Fable 深掘り精査（P1〜P7、オーナー実施）
**関連:** [`kill_criteria_20260703.md`](kill_criteria_20260703.md)（viability autopsy / KILL_CRITERIA。※別途コミット。未コミットの場合は追加要）

> [!WARNING]
> **生成（育種）は停止する。** viability autopsy の結論に基づき、本設計の完了までは
> `phase-6-breeding` を含む戦略「生成」を止める。現在の 168 survivors は生成過剰・検証不足
> の産物であり、これ以上増やすことは検証負債を増やすだけ。**今やるのは "増やす" ではなく
> "既にいる 61 の PROVISIONAL を実コストで検証しきる" こと。** 詳細な撤退基準は上記
> KILL_CRITERIA doc を正とする。

> [!IMPORTANT]
> **本ドキュメントの性質**: Claude によるリポジトリ実コード＋docs 根拠の分析を土台に、
> **Fable による P1〜P7 深掘り精査（オーナーが別途実施）の確定結論を反映済**。各節末尾は
> 🔬「Fable 判定」に更新し、Fable でも机上で決めきれず**現物コード確認が要る項目のみ
> 「未確認」**として残した。確定事実には file:line を付す。

---

## 0. 総括判定

**「現状延命 vs 全面再構築」は誤った二択。正解は "背骨（検証パイプラインの一貫性）だけを作り直す" 部分再構築。**

- **全面 rewrite は損**: 健全な資産（Guardian の Rust アリーナ約9.4k行、`honest_gate.py`、DSL/育種ループ）を巻き添えにする。
- **現状延命も不可**: 「ランクが損益と無関係（LEGEND が損失戦略を不死化）」「フィード PASS が構造的に 0」を温存し、工場の出力が永久に無価値のまま。
- **致命傷はアーキテクチャ欠陥ではなく "配線の断絶" と "規律の欠如"**。背骨を作り直せば、既存資産の上で工場が初めて「正直に PASS>0」を出せる。

---

## 0.5 実装優先順位（viability autopsy 反映 — ★最重要★）

**目的を「工場の全面再建」から「4週間で決定的実験を回せる状態にする」に絞る。**

| 優先度 | 項目 | ゴール |
|---|---|---|
| **🥇 最優先（4週間の決定的実験）** | **P1（配線修復）＋ P3 の一部（61件 PROVISIONAL への実コスト OOS 実走）** | 生成を止めた上で、既存 61 survivors に対し**実コスト（スプレッド/スワップ/スリッページ込み）で本物の OOS/CPCV を実走**し、`honest_gate` に正直な PASS/REJECT を出させる。これが「工場に価値があるか」を殺すか活かすかの決定的実験。KILL_CRITERIA に接続。 |
| 🥈 二次 | **P2（ランク健全化）** | LEGEND の execution リスク除去。実験結果が出る前でも実弾経路の安全化は並行してよい。 |
| 🥉 三次 | **P4/P5（信頼性・永続化）** | 4週間の実験を落とさず回すための最低限。 |
| 補助 | **P6/P7（契約・掃除）** | 事故率・負債低減。実験の妨げにならない範囲で並行。 |

> **判断基準**: P1＋P3 の実走で 61件から「実コストでも PASS」が出るか。出なければ工場（蠱毒）の thesis は棄却＝ KILL_CRITERIA 発動。出れば背骨の残り（P2/P4-P7）を仕上げて本運用へ。**P2/P4-P7 に先に時間を使わない**（実験の結論が先）。

### スレッドの束ね方（従来）
| スレッド | 内容 | 依存 |
|---|---|---|
| **A（P1→P2→P3）** | 検証結果の配線修復 → ランク健全化 → 検証スループット | 連鎖。ただし上記のとおり **P1＋P3(バックフィル) を先行**、P2 は二次 |
| **B（P4/P5）** | 信頼性＋永続化 | A と独立・並行可 |
| **C（P6/P7）** | 契約一元化＋掃除 | 独立・並行可 |

---

## 1. 現状の構造的評価

### 1-A. 強み（残す資産）
| 資産 | 根拠 | 価値 |
|---|---|---|
| 正直な gate と feed 契約 | `tools/tribe/honest_gate.py:36-45,149-185`, `doc/FACTORY.md` | PASS=OOS/CPCV検証済、PROVISIONAL=IS-only、REJECT=floor割れ。DDバジェット正規化 `budgeted = headline × dd_budget/dd_observed`。**最も誠実な部分。** |
| Guardian Rust アリーナ | `guardian/src/{backtester.rs(79KB), cpcv.rs(24KB), lstm,ppo,neural,mcts,ensemble,kalman,tournament}.rs`, `main.rs(133KB)` | CPCV(López de Prado)含む本物の OOS 検証エンジン。subtree で in-repo 化済（`doc/OPERATIONS.md:64-113`）。 |
| ポート契約の実装整合 | `docs/llm/INTERFACES.md:7-19` ↔ `config.lisp:103-107` ↔ `guardian/src/main.rs:24-29` | 5557/5560/5555/5556/5559/5562 が3面一致。**実装は揃っている**（ドキュメントのみ drift）。 |
| 育種ループの実在 | `school-connector.lisp:269-341`(1s周期), `school-breeder.lisp`, `school-genome.lisp` | 交叉/トーナメント/変異、遺伝的距離0.2-0.8制御。RPではなく実GA。**※ただし当面は停止（§0.5）。** |
| ナラティブ層は薄い | rituals/narrative/constitution 合計 約1.7k LOC（全66k の約2.6%） | 俗説と逆で**問題ではない**。削る対象ではない。 |

### 1-B. 致命的欠陥（実測で確認済み）
| # | 欠陥 | 根拠 |
|---|---|---|
| ① | **フィード PASS=0 は配線断絶**: OOS/CPCV結果が SQL には書かれるが `data/library/*.lisp` に書き戻らない。export は .lisp を読むので常に `OOS-SHARPE 0.0`。 | `school-validation.lisp:995-1027`(`upsert-strategy` のみ, `save-strategy` 無し), `export.py:68-101` |
| ② | **ランクが損益と乖離**: `LEGEND` 64件は中央値 Sharpe −0.025、33/64 が負、63/64 が <1.0（定義は >2.0）。しかも墓場送り免除で不死化。 | 実測（Appendix A）, `school-rank-system.lisp:23-28,786-794`, `systemd/swimmy-school.service:11` |
| ③ | **クラッシュループ無防備**: school/brain は `Restart=always`＋`StartLimit` 無し。school-daemon は未捕捉例外で即 exit 1。 | `systemd/swimmy-school.service`, `swimmy-brain.service`, `school-daemon.lisp:10-17` |
| ④ | **graveyard 書き込みにアトミック保証なし**: 中断で `data_sexp` 破損 → safe-read が黙って読み飛ばし、失敗パターンが静かに消失。 | `school-cemetery.lisp:67-73`, `school-kb.lisp:66-118` |
| ⑤ | **規律負債**: 平文DBパスワード、.fasl 4件コミット、契約docの stale、v1/v2重複。 | `db-adapter.lisp:17`, `git ls-files`, `HANDOFF.md`, `REFERENCE.md` |

---

## 2. スレッドA（P1→P2→P3）＝背骨の作り直し

### P1. 検証結果の配線修復（PASS=0 の根治）★最優先★

**A. 根本原因（確定）**
- gate の PASS 条件: hard floor 通過 **かつ** `oos_sharpe>0 or cpcv_pass>0 or cpcv_sharpe>0`（`honest_gate.py:149-156`。未検証は `warn=True`→PROVISIONAL 止まり `:180-185`）。
- export は各 `data/library/<rank>/<name>.lisp` を読む（`export.py:68-101` の `_gate_candidate` が `m.get("oos_sharpe")` 等）。
- OOS結果ハンドラ `handle-oos-backtest-result`（`school-validation.lisp:1016-1027`）は `(setf (strategy-oos-sharpe strat) sharpe)` → `(upsert-strategy strat)` で **SQL のみ**更新。`save-strategy`（`persistence.lisp:69-100`、temp→rename でアトミック）は**呼ばれない**。CPCV も同様。
- 結果、library .lisp の `OOS-SHARPE` は 0.0（168件中133件）。非ゼロ35件は in-sample SHARPE の**ミラー**（例 `SHARPE 17.264853 / OOS-SHARPE 17.264853 / TRADES 35`）で偽検証、かつ trades<200 で REJECT。

**B. 再設計（single source of truth）**
- **正本を SQL `strategies` に一本化し、`export.py` を「SQL 読み」に変更**する。
- OOS 未実施は `0.0` ではなく **未検証(nil)** で表現。育種時の OOS ミラー複製を**禁止**。

**C. パッチ方針**
1. `export.py` の読み口を library .lisp → SQL に切替（正本一本化）。
2. 既存35件のミラー OOS を一括 nil 化する one-shot migration。
3. SQL↔.lisp ドリフト解消 migration（正本SQL基準）。
4. `python3 -m tools.tribe.export` 再生成。
- 成功判定: 「実コスト OOS>0 かつ trades≥200」の件数が feed の PASS に一致。ミラー由来の偽 PASS が 0。

**D. リスク**: 見かけ上 PASS 急増→誤本番投入。gate の floor は緩めず、PASS→live に deployment_gate を必須化。

**🔬 Fable 判定（確定）**
- ✅ 正本＝**SQL 一本化**、`export.py` を SQL 読みに変更（推奨として確定）。`.lisp` は表示用の派生に降格。
- ✅ OOS ミラー禁止・未検証は **nil 表現**。
- ✅ SQL↔.lisp ドリフトは **one-shot migration** で解消（正本SQL基準）。
- ⚠️ 未確認: OOS ミラーを複製している育種側の該当関数の特定は、実装時にコードを追って pin する（机上では場所未特定）。

---

### P2. ランク健全化（損益と乖離した称号の廃止）※二次

**A. 原因（確定）**
- 基準は B/A/S のみ（`school-rank-system.lisp:23-28`）。**LEGEND は `*rank-criteria*` に不在**＝非検証ランク。
- LEGEND 墓場送り免除（`:786-790`）＋ school 起動毎 `ExecStartPre=... restore_legend_61.lisp`（`swimmy-school.service:11`）で 61件復元。
- 内部 trade floor（A=50/S=100、S段階緩和は30まで `:66-76`）と外部 gate（`min_trades=200`）の**二重基準**。

**B. 再設計**
- ランク＝`gate.verdict`／`*rank-criteria*` の**純関数**化。
- LEGEND を **execution 経路から切離し、学習専用アーカイブ化**（資金配分ゼロ・execution不可）。
- trade floor を **200 に統一**。

**C. パッチ方針**
1. `:786-790` LEGEND保護分岐の除去→アーカイブ化。
2. `restore_legend_61.lisp` を **stamp 方式**化（毎起動フル復元をやめ、初回のみ/冪等スタンプで判定）。
3. trade-evidence floor を 200 に統一。
4. 既存 LEGEND 64件・S 1件（=`TestStrat` 全ゼロ）の再評価バッチ。

**D. 安全確認**: 変更で実弾 execution 対象が変わる。有望株誤殺/損失株延命を防ぐ検証ゲートを migration 前に。

**🔬 Fable 判定（確定）**
- ✅ ランク＝**gate.verdict/純関数化**。
- ✅ LEGEND を **execution 経路から切離＝学習専用アーカイブ化**（実弾リスク除去）。
- ✅ **trade floor 200 統一**。
- ✅ `restore_legend_61` は **stamp 方式**化（P4 の起動負荷是正と共通）。

---

### P3. 検証スループット（誰をいつ検証に回すか）★61件バックフィルが最優先★

**A. 実データフロー（確定）**
- メインループ（`school-connector.lisp:269-341`）: `phase-1-validation`（IS）→ `phase-3-qualify`（WFV/OOS）→ `phase-3-5-cpcv-validate`（**A→S のみ**）→ `phase-4-purge`（**純IS淘汰**）→ `phase-6-breeding`。
- backtest は **Python Backtest Service（5580）** へ（`school-backtest-v2.lisp:75-82` `:target :backtest`）。単一CSV丸読み、train/test は任意指定。
- Guardian(Rust) は **5556** で `BACKTEST`/`CPCV_VALIDATE` を自前処理（`main.rs:1967-1990`, dual-parse）。→ **二重エンジン**。

**B. 再設計**
- 検証母集団を「昇格候補限定 → **PROVISIONAL 全件 OOS バックフィル**」へ拡大（＝§0.5 の4週間実験の実体）。
- Python(5580)/Rust(5556) 二重エンジンを **cross-check CI** 化（同一入力で結果一致を検証）。

**C. パッチ方針**: 61件 PROVISIONAL に対し実コスト OOS/CPCV を一括実走するバックフィル・ジョブを新設。容量・レート（`SWIMMY_BACKTEST_WORKERS`/`*_MAX_INFLIGHT`）を実験用に設定。

**D. 統合確認**: P1（書き戻し）＋P3（バックフィル）で「実コストでも PASS が出るか」を4週間で判定 → KILL_CRITERIA。

**🔬 Fable 判定（確定＋要確認）**
- ✅ 母集団を **昇格候補限定→PROVISIONAL 全件 OOS バックフィル**へ拡大。
- ✅ Python(5580)/Rust(5556) 二重エンジンは **cross-check CI 化**（統合ではなく一致検証で担保）。
- ⚠️ **未確認（最優先の現物確認）**: **purge/embargo（López de Prado）の実装が正しいか**。単一CSV丸読み＋任意 start/end で walk-forward の purge/embargo が効いていないと、バックフィルの OOS 自体が汚染される。**バックフィル実走の前に、この実装検証を最優先で行う**（実験の信頼性の前提）。

---

## 3. スレッドB（P4/P5）＝信頼性・永続化 ※三次

### P4. クラッシュループ／メモリ／ヒープ

**A. 障害モード（確定）**
| ユニット | Restart | StartLimit | 備考 |
|---|---|---|---|
| swimmy-school | always / 10s | **無し** | `school-daemon.lisp:10-17` 未捕捉例外で即 exit 1。ExecStartPre で毎回 restore_legend_61 |
| swimmy-brain | always / 10s | **無し** | `Environment=SWIMMY_SBCL_DYNAMIC_SPACE_MB=4096` |
| swimmy-backtest | on-failure / 10s | **300s/5**（正しい） | 参照実装 |
| swimmy-data-keeper | always / 10s | `[Unit]` に 300s/5 | 実害は限定（下記 Fable 判定で stale 訂正） |

- ヒープ: `run.sh:69,71`（Ver 41.5）は `SWIMMY_SBCL_DYNAMIC_SPACE_MB:-6144` で **env 尊重・既定6144**。systemd が 4096 上書き＝実効4096。運用メモの「run.sh:64 が4096固定」は **stale（訂正対象）**。`STATE.md:2166`: graveyard-cache が既定ヒープで枯渇しうる。

**B/C. 再設計／パッチ方針**
- 脳系 systemd を **`Restart=on-failure` ＋ `StartLimitIntervalSec=600`/`StartLimitBurst=5` ＋ `OnFailure=` 通知**に。
- **brain 停止時は guardian が degraded 運転**（新規発注停止・既存管理のみ）。
- ヒープを **`/etc/swimmy/heap.env` に一元化**（`brain=3072` / `school=4096`、**変数名を分離**して一括置換事故を防止）。
- `restore_legend_61` を **stamp 方式**化（P2 と共通）。

**🔬 Fable 判定（確定）**
- ✅ 脳系: **on-failure ＋ StartLimit(600/5) ＋ OnFailure 通知**、**brain 停止時 guardian degraded 運転**。
- ✅ ヒープ **`/etc/swimmy/heap.env` 一元化（brain 3072 / school 4096、変数名分離）**。
- ✅ `restore_legend_61` **stamp 化**。
- ✅ stale 2件（run.sh:64 の4096固定説／data-keeper StartLimit 実害説）は **訂正済＝実害なし**。

---

### P5. 永続化の破損耐性

**A. 破損耐性マップ（確定）**
| 層 | 読み | 書き |
|---|---|---|
| library `.lisp` | safe-read | **アトミック（temp→rename, `persistence.lisp:69-100`）＝安全** |
| SQL `strategies`（graveyard含む, `data_sexp`列） | safe-read | INSERT OR REPLACE 単発。トランザクション/耐久設定が要点 |
| `data/memory/graveyard.sexp` | `read`＋`(error()nil)` | 追記形式・アトミック保証なし |

- 非対称の穴: safe-read が破損を隠蔽→書き込み破損に気付けない。`cemetery-audit-db` は batch 毎 full GC＝逼迫兆候。

**B/C. 再設計／パッチ方針**
- SQLite **WAL ＋ `synchronous=NORMAL` ＋ `busy_timeout`**。
- graveyard 書き込みを **トランザクション化**。
- `data_sexp` に **sha256 checksum ＋ サイズ上限**。
- tmp 名を **一意化（pid＋epoch）**。
- **欠損検知監査**（graveyard 件数の**単調非減少 invariant**）。
- **破損行は削除せず quarantine**（隔離テーブルへ退避、静かな消失を防ぐ）。

**🔬 Fable 判定（確定＋要確認）**
- ✅ **WAL＋synchronous=NORMAL＋busy_timeout**、graveyard **トランザクション化**、`data_sexp` **sha256＋サイズ上限**、tmp **pid+epoch 一意化**、**単調非減少 invariant 監査**、破損行は **quarantine（削除しない）**。
- ⚠️ 未確認: `save-strategy` の temp-path が **path と同一FS上か**（rename のアトミック性は同一FS前提）。実環境で要確認。

---

## 4. スレッドC（P6/P7）＝契約一元化・掃除 ※補助

### P6. 契約ドキュメントとコードの真実一元化

**A. 差分表（確定）**
| 項目 | 宣言 | 実装 | 判定 |
|---|---|---|---|
| Brain↔Guardian ポート | HANDOFF.md: 5558/5559 | main.rs:25-26 は 5555/5556 | **HANDOFF stale（5558不在）** |
| トレードコマンド形式 | REFERENCE.md: JSON 例 | 実配線は S式（`cmd_route.rs:88-111`） | **REFERENCE 誤解を招く** |
| ZMQ エンコーディング | INTERFACES.md:5「S式のみ・JSON非受理」 | main.rs:1957-1981 は 5556/5559 で **dual-parse** | **正本の記述が実装と不一致** |
| ポート実体 | INTERFACES.md:7-19 | main.rs:24-29, config.lisp:103-107 | **一致** |

**B/C. 一元化設計／パッチ方針**
- single-source を **`contract.toml`** に集約し、docs は派生（生成/リンク）。
- HANDOFF.md deprecated 明記、REFERENCE.md の JSON例→S式化、INTERFACES.md の「JSON非受理」を実装整合。
- dual-parse は **2段階廃止（WARN観測 → ゼロ確認後に削除）**。
- 回帰防止: `cmd_route.rs` の type/action 集合と契約の突合 CI。

**🔬 Fable 判定（確定＋要確認）**
- ✅ **stale 5件確定**（HANDOFF 5558非実在 等）。
- ✅ **`contract.toml` を single-source 化**、docs は派生。
- ✅ dual-parse は **WARN観測→ゼロ確認後削除の2段階**（いきなり削除しない）。
- ⚠️ 未確認: **MT5境界（5557/5560）のワイヤフォーマット**は別リポ（MQL5）で本文に無い。要現物確認。

---

### P7. 低リスク一括クリーンアップ

**A. チェックリスト（確定）**
| 項目 | 根拠 | 分類 | リスク |
|---|---|---|---|
| 平文DBパスワード `"swimmy_password"` | `db-adapter.lisp:17`（既定 `:jsonl`、postgres時のみ使用） | **即対応＋露出済ローテート** | 高（露出済） |
| .fasl 4件コミット | `discord.fasl`, `message-dispatcher.fasl`, `school.fasl`, `evolution-tests.fasl` | 即対応 | 低 |
| backtest v1/v2 併存 | `school-backtest.lisp`(867行) と `-v2.lisp` | 計画（段階廃止） | 中 |
| no-op/stub 3件 | `prediction.lisp:336`, `school-lifecycle.lisp:60`, `school-evolution-llm.lisp:13` | 計画 | 低 |
| tests.lisp 17k行 | 手書きで正当、setup重複 | 放置可（共通化候補） | 低 |

**B/C. パッチ方針**
- DBパスワード: `getenv-or-dotenv` 化（jsonl既定を壊さない）＋**露出済みパスワードのローテートを最優先**。
- .fasl: `.gitignore` 追記＋`git rm --cached`。
- backtest v1/v2: 呼出元 grep→段階廃止。
- 再混入防止: **gitleaks pre-commit**。

**🔬 Fable 判定（確定）**
- ✅ DBパスワード **env 化＋露出済みローテート最優先**。
- ✅ **.fasl 追放**（.gitignore＋rm --cached）。
- ✅ **V1/V2 段階廃止**。
- ✅ **gitleaks pre-commit** で secret 再混入防止。

---

## Appendix A. 実測値（本レビューで直接確認）

- feed（`feed/strategies_feed.json`, 生成 2026-06-24）: `total=168, PASS=0, PROVISIONAL=61, REJECT=107`。gate_config: `dd_budget=12%, kpi=3%/mo, min_trades=200, min_pf=1.10, min_sharpe=0.10`。
- library 分布: S=1（=`TestStrat` 全ゼロ）, A=9, B=93, LEGEND=64, RETIRED=1。
- LEGEND 64件 Sharpe: 中央値 −0.025 / 負 33件 / <1.0 が 63件 / 最小 −5.60 / 最大 1.4。例 `LEGEND/Aggressive-Reversal.lisp`: `SHARPE -0.058, PF 0.87, WR 0.4, TRADES 130`。
- 非ゼロ OOS 35件（A7/B26/LEGEND2）は `OOS-SHARPE == SHARPE` のミラー（例 trades=35 で Sharpe 17.26）。
- 規模: Lisp 66,519行 / 168ファイル（school 79ファイル）、Guardian Rust 約9.4k行 / 15ファイル。systemd 35 service＋17 timer。doc/docs 259ファイル、STATE.md 2619行、expert_panel 26本。

## Appendix B. 実装フェーズの着手順序（viability autopsy 反映・改訂）

1. **【最優先 / 4週間の決定的実験】**
   a. **P1**: 配線修復（export→SQL 一本化、OOS ミラー nil 化、ドリフト migration）。
   b. **P3 前提**: **purge/embargo 実装検証**（未確認・最優先の現物確認）。
   c. **P3 本体**: 61件 PROVISIONAL への**実コスト OOS/CPCV バックフィル**実走 → 正直な PASS/REJECT。
   d. 結果を **KILL_CRITERIA** に接続（工場 thesis を活かすか殺すか判定）。
   e. ※この間 **生成（育種）は停止**。
2. **【二次】P2**: ランク健全化（LEGEND execution 切離、trade floor 200、restore_legend_61 stamp 化）。実弾安全化は並行可。
3. **【三次】P4/P5**: 信頼性・永続化（4週間の実験を落とさない最低限）。
4. **【補助】P6/P7**: 契約一元化・掃除（DBパスワード ローテートのみ即時、他は妨げにならない範囲）。

各 P には Fable 用の自己完結プロンプト（背景＋実コード抜粋＋検証観点＋期待アウトプット A/B/C/D）を別途保持。実装は「プロンプト→Fable 精査（済）→本メモの🔬判定→パッチ」の順。**残る「未確認」は P3(purge/embargo)、P5(temp-path 同一FS)、P6(MT5 ワイヤフォーマット) の3点のみ。**
