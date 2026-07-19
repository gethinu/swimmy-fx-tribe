> ⚠️ **後続訂正 (2026-07-19) — 本 run の希釈% と diverse-robust 数は cosmetic seed のアーティファクト。**
> 本検証は 20 体の REVERSION クローンを `cpcv=0.7 / sharpe=0.7` pre-set・`:require-bt nil`（honest gate 未通過）で
> seed した。よって §0/§1 の **「USDJPY 99.6%→72.9%/77.9% 希釈」「diverse-robust 0→~20–21」は水増し**。honest に
> 採点し直すと（seed-escape `fed9bf7b` / inside-out `6ee78541`）**希釈 floor ≈ 84–85% / diverse-robust plateau = 2–3**。
> **機序的結論はすべて有効**（実パイプライン flag-ON 完走・希釈主因は §4 floor・per-cycle cull は raw score=B-5 gap・
> fitness-sharing 自己制限・隔離厳守）。判定 "CONDITIONAL GO" は「検証ギャップが閉じた」意で有効だが、**デプロイ含意は
> deploy NO で上書き**。集約と対照表: [`tribe_2d_diversity_investigation_summary_20260719.md`](tribe_2d_diversity_investigation_summary_20260719.md) §1。

# 🧪 2a Daemon-Isolation Verification — flag-ON breeding pipeline promotes diverse & dilutes monoculture (measured, real machinery)

**日付:** 2026-07-18 JST ／ **ブランチ:** `claude/2d-behavioral-distance` (base `60edd7d1`, = master 2c + 2d/B-4 距離)
**環境:** WSL **Ubuntu-22.04**（`stair` ユーザ, 非ライブ distro・swimmy fleet 無し）。userspace SBCL 2.1.11 + vendored libzmq、**copy DB のみ**。
**モード:** オフライン・**ライブ発注ゼロ・ライブ swimmy.db 未接触・本番フラグ既定 OFF のまま**。Guardian のライブ発注ループ非起動（[[guardian-closes-live-trade-loop]]）。honest_gate/§4 floor 不変。

> **これは何か:** [`tribe_2c_wsl_verification_20260715.md`](tribe_2c_wsl_verification_20260715.md) §3 が残した唯一の未検証リンク **(A) daemon 隔離**＝「flag-ON で diverse child が**実**育種パイプライン（add-to-kb→phase1→A-rank→cull）を実際に昇格し、多世代で単一栽培が希釈されるか」を、**実コードの実関数**で copy DB 上に実測した記録。

---

## 0. TL;DR

- **実パイプラインが flag-ON で例外なく完走**：copy DB（実 monoculture: **USDJPY 486 / 99.6%・TREND 488 / 100%**）を種に、`add-to-kb :breeder :require-bt t`（実 dedup/correlation/graveyard ゲート＋defer＋pending DB upsert）→ **`handle-v2-result`（実 Phase-1 昇格：`ensure-rank :B` / graveyard ＋ upsert）** → `run-rank-evaluation`（実 A-rank）→ `cull-pool-overflow`（実 20-or-die）を 6 世代。
- **(a) diverse child は実パイプラインを実際に昇格**：毎世代 **10/10 児が diverse**（2f ゲートが mono 育種を枯らす）、うち **3–6 児/世代が実 Phase-1 を通って B 昇格し copy DB に永続**（例 `Bred-SEED-850` EURUSD BREAKOUT, cpcv 0.6, 270 trades）。6 世代で ~30 diverse 昇格。
- **(b) 単一栽培は希釈**：**USDJPY 99.6% → 72.9%（fitness-sharing 有）／77.9%（無）**、**TREND 100% → 72.9%／79.8%**。category は TREND 単一 → TREND/REVERSION/BREAKOUT に分散。
- **(c) forward-robust 非USDJPY/非TREND が KB に**：**0 → 20–21**（cpcv≥0.6, active）。
- **罠対策が効く**：diverse Keltner **20 体 seed ＋ 2f pre-gate 0.60→0.55** で毎世代 10 pair 育種・**ゲート rejected=0**。これが無いと mono プールは CPCV 枯渇で育種停止（既知診断）。
- **2d fitness-sharing 装置が発火・自己制限**：daily stagnant/weak cull（selection-fitness＝evidence-adjusted-sharpe ÷ niche 密度）が **GEN1 で密な USDJPY niche を 81→62（21 体除去）**、以降は niche が小さくなり selection-fitness≥0.6 で **均衡**（＝「61 clone 不能」の除算装置が設計どおり働く）。
- **隔離厳守を実測確認**：repo `data/library` 汚染 **0**、**ソース dev swimmy.db mtime 不変**、全 library 書込は scratch（`/mnt/c/tmp/verify_library`）へ redirect。

---

## 1. 実測（Q への回答）

### Q(a). flag-ON で diverse child が add-to-kb→phase1→A-rank→cull を昇格するか → **YES**

毎世代、育種児 10/10 が diverse（EURUSD/GBPUSD Keltner/BB-MR）。各児は
`add-to-kb :breeder :require-bt t` で実ゲート通過→**defer（pending, rank nil, DB upsert）**、
→ `handle-v2-result "<name>_P1"`（実 `phase1-screening-passed-p`: sharpe≥`*phase1-min-sharpe*` & pf≥1.0）
→ pass なら **`ensure-rank :B` ＋ active-pools 追加 ＋ upsert**、fail なら `send-to-graveyard`。
**3–6/世代が B 昇格して copy DB に永続**。これは「offline sim が top-N cull＋primitive_scan で代替した部分」を**実 add-to-kb/実 Phase-1 昇格/実 DB 永続**で置換した初の実測。

### Q(b). 多世代で単一栽培が希釈されるか → **YES（ただし主因は §4 honest floor、詳細は §3）**

**Run A（per-cycle cull のみ: run-rank-evaluation + cull-pool-overflow）**

| Gen | active | USDJPY% | TREND% | diverse-robust | bred(div) | promoted→B(div) |
|---:|---:|---:|---:|---:|---:|---:|
| 0 | 488 | 99.6 | 100.0 | 0 | – | – |
| 0b(seed) | 508 | 95.7 | 96.1 | 20 | – | – |
| 1 | 103 | 78.6 | 80.6 | 20 | 10(10) | 4(4) |
| 2–3 | 103 | 78.6 | 80.6 | 20 | 10(10) | 4(4) |
| 4 | 104 | 77.9 | 79.8 | 21 | 10(10) | 4(4) |
| 5 | 104 | 77.9 | 79.8 | 21 | 10(10) | 6(6) |
| 6 | 104 | **77.9** | **79.8** | 21 | 10(10) | 3(3) |

最終 symbol=USDJPY 81 / EURUSD 16 / GBPUSD 7、category=TREND 83 / REVERSION 20 / BREAKOUT 1。

**Run B（＋ daily 2d fitness-sharing cull: cull-stagnant-crank-daily / cull-weak-strategies）**

| Gen | active | USDJPY% | TREND% | promoted→B(div) | fit-share cull |
|---:|---:|---:|---:|---:|---:|
| 0 | 488 | 99.6 | 100.0 | – | – |
| 0b(seed) | 508 | 95.7 | 96.1 | – | – |
| 1 | 82 | 75.6 | 75.6 | 5(5) | **killed 21**(81→62) |
| 2 | 82 | 75.6 | 75.6 | 5(5) | 0（均衡） |
| 3 | 83 | 74.7 | 74.7 | 5(5) | 0 |
| 4 | 84 | 73.8 | 73.8 | 3(3) | 0 |
| 5 | 84 | 73.8 | 73.8 | 6(6) | 0 |
| 6 | 85 | **72.9** | **72.9** | 3(3) | 0 |

### Q(c). forward-robust 非USDJPY/非TREND が KB に入ったか → **YES: 0 → 20–21**（cpcv≥0.6, active）

seed 由来が大半。育種児は B 昇格するが cpcv≥0.6（forward-robust）到達は希（歩留まり薄、[[tribe-diversity-engine-rebuild]] の ~2% と整合）→ diverse-robust は ~20 で頭打ち。

---

## 2. 罠対策の効き（診断どおり）

flag だけ ON では mono プールに CPCV≥0.55 がほぼ無く**育種が停止**する。本検証は 2 点セットを投入し、これが必要十分な bootstrap であることを実証：

1. **diverse Keltner/BB-MR を 20 体 seed**（実 breedable 個体を clone→遺伝的に分散させた非USDJPY MR。`add-to-kb :require-bt nil` で実ゲート通過・DB 永続）。
2. **2f pre-gate `*2c-cpcv-pregate-min*` 0.60→0.55**（前段ゲートのみ。**honest_gate の検証 floor（min_trades=200/min_pf/min_sharpe）は一切不変**）。

結果：毎世代 10 pair が育種され、add-to-kb の **ゲート rejected=0**（breeder correlation-variant 経路で受理）。

---

## 3. 正直な限界（デプロイ判断の材料）

1. **希釈の主因は 2c/2d ではなく §4 honest floor。** GEN0→GEN1 の大幅減（active 488→~85–104, USDJPY% 99.6→~78）は **`run-rank-evaluation` が trade-evidence floor（≥200 trades）＋ conformance で脆弱な 35-trade 級 mono を graveyard 化**した結果。これは honest gate が正しく働いた証拠（望ましい）だが、「単一栽培%が落ちた主因は多様性注入ではなく脆弱 mono の淘汰」である。
2. **fitness-sharing は per-cycle overflow cull に配線されていない（コード事実）。** `cull-pool-overflow`（school-breeder.lisp:1499-1505）は **raw `score-from-metrics`**（sharpe/pf/wr/maxdd）でソートし、**`selection-fitness` を使わない**。除算による多様性強制が効くのは **daily の `cull-stagnant-crank-daily`/`cull-weak-strategies`（:1749/:1766）と親トーナメント/deathmatch/wisdom-elite**（:1627/1682/1624）だけ。→ offline sim の強い単調希釈（86%→67%, [[tribe-diversity-engine-rebuild]]）は sim が cull に selection-fitness を使ったため。**実 daemon の per-cycle cull は raw score** なので、希釈は Run A で頭打ち（~78%）、Run B の daily cull を足して初めて追加で ~5pt 進む（72.9%）。regen §B-3(3)/B-5「溢れ淘汰の sort を selection_fitness に差し替え」は**未実装**。
3. **diverse fraction は 6 世代で少数派（~25–27%）で均衡**、多数派化しない。要因：(i) forward-robust diverse 歩留まりが薄い（diverse-robust ~20 で flat）、(ii) 上記 per-cycle cull が raw score。多数派化には ~15–20 世代 or 歩留まり改善 or B-5 配線が要る。
4. **backtest transport は primitive_scan（guardian と同一 backtester/CPCV engine）で同期代替**。実 async ZMQ→guardian ではない。理由：本検証を回した**非ライブ distro（Ubuntu-22.04, sudo 無し）で guardian の Rust rebuild が zmq-sys の libzmq ソースビルド（要 g++）に阻まれた**（apt 権限制約で g++/pkg-config 依存鎖を userspace で解けず）。ただし **async ZMQ の各リンクは §3d smoke（`5b658752`）で個別検証済み**（emit→add-to-kb 受理→backtest_service.py→guardian real Keltner 956,228 trades≠SMA→cpcv.rs keltner-aware）。**残る未走は「literal `start-evolution-service` の async 無限ループを一気通貫で回す」1 点のみ**。
5. Run B の age/seed ポリシー（veteran mono を cull-eligible=age15、fresh diverse lineage を age0 保護）はモデリング選択。実 daemon では diverse も加齢するが、育種変異で遺伝的に分散していれば selection-fitness が保たれ生存する想定。

---

## 4. 判定：**CONDITIONAL GO**（(A) ギャップは実質解消）

- **実パイプラインは flag-ON で完走**し、**diverse child を実 Phase-1 昇格＋DB 永続**、**単一栽培を 99.6%→73–78% に希釈**、**2d 除算装置が発火・自己制限**、**ライブ発注面ゼロ・ソース DB 不変**を copy DB 上で実測。
- **本番投入前の残タスク（可逆・段階、[`tribe_2c_wsl_verification_20260715.md`](tribe_2c_wsl_verification_20260715.md) §6 手順）**：
  - **(残1) literal async daemon の一気通貫 smoke**：ライブホスト（WSL Ubuntu, guardian ビルド済）で `start-evolution-service` ＋ `guardian --backtest-only`（発注 OFF・`SWIMMY_ALLOW_EXTERNAL_ORDER_OPEN` unset・MT5 非接続）を数サイクル。各リンクは §3d 検証済のため、これは最終の統合 smoke。
  - **(残2) seed/閾値 bootstrap の本番投入**（forward-robust EURUSD Keltner p50-60 dev2 H4 ATR2x2 を ~20 体 ＋ `*2c-cpcv-pregate-min*` 初期 0.55、env 化推奨）。
  - **(残3) 歩留まり ~2% ・希釈が緩慢（少数派均衡）を許容前提として受容。** 単一栽培の"入替"ではなく"希釈"である。
  - honest_gate/§4 floor 不変・**flag flip はオーナー判断**（不可逆なライブ進化変更・未実施）。
- **推奨**：deploy 手順の (残1) を最終 smoke として実施 → 問題なければ seed 投入 → `SWIMMY_PRIMITIVE_DIVERSITY=1` → 監視（例外・昇格・分布）→ 異常時は env を外して即 revert。

---

## 5. 手法・再現

- **隔離ハーネス**: `logs/tribe_2c_wsl/daemon_verify.lisp`（本検証本体）＋ `logs/tribe_2c_wsl/wsl_env_setup.sh`（Ubuntu-22.04 userspace SBCL+libzmq bootstrap）＋ `logs/tribe_2c_wsl/run_harness.lisp`（quickload→load ハーネス）。
- **DB**: `data/memory/swimmy.db` を `C:\tmp\swimmy_daemon_verify.db` に**コピー**（sidecar -wal/-shm を除去してから）。ハーネスは `swimmy.core::*db-path-default*` を copy に rebind、`swimmy.persistence::*library-path*` を `/mnt/c/tmp/verify_library/` に redirect（repo 書込禁止・起動時アサート）。
- **scoring**: 既存 Windows `target/release/primitive_scan.exe` を WSL interop で呼出（OOS 2pip＋purged CPCV、guardian と同一 engine）。相対パス（cwd=repo root）で Windows バイナリが解決。
- **flag**: `SWIMMY_PRIMITIVE_DIVERSITY` 未使用。ハーネス内で `(setf swimmy.core:*enable-primitive-diversity* t)` を**プロセス内のみ**。config.lisp:124 の既定は **nil のまま不変**。
- **安全実測**: repo `data/library` 汚染 0・ソース dev DB mtime 不変・全 library 書込は scratch。ライブ daemon/motor/guardian order path/MT5 は一切起動せず。

## 6. コミット
`claude/2d-behavioral-distance`。ハーネス＋本レポートは flag-gated（既定 OFF）・可逆。
関連: [[tribe-diversity-engine-rebuild]] [[tribe-2d-behavioral-distance]] [[guardian-backtest-reproduction]] [[kill-criteria-s4-verdict]] [[native-windows-sbcl-blockers]]
