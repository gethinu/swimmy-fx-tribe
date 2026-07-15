# 🧬 2c 本体 — プリミティブ多様性育種エンジン（flag-gated・破壊的背骨改修）

**日付**: 2026-07-14 JST ／ **ブランチ**: `claude/2c-primitive-diversity-breeding`
**モード**: オフライン実装。**flag OFF がデフォルト＝ライブ育種は無改変（可逆）。** ライブ発注なし・live `swimmy.db` 無改変・honest_gate/§4 floor 不変。
**前提**: [`tribe_expanded_primitive_forward_20260714.md`](tribe_expanded_primitive_forward_20260714.md)（forward-robust な Keltner-MR エッジ＝EURUSD H4）。
**制約遵守（オーナー指定）**: プリミティブ単独配線は禁止 → **2f（CPCV 前段ゲート）＋2d（fitness sharing）を同時実装**。

---

## 0. TL;DR

- **マスターフラグ** `swimmy.core:*enable-primitive-diversity*`（env `SWIMMY_PRIMITIVE_DIVERSITY`, 既定 **nil=OFF**）。OFF では下記すべてが legacy とバイト同一。
- **Stage 1（emit 縮約解消）**: `strategy-to-alist` が flag ON で keltner/donchian type＋`band_mult`/`atr_period`/`atr_barrier_sl/tp` を送出。period 破損を直す `emit-short-long`。struct に多様性遺伝子を additive 追加。
- **Stage 2（探索空間）**: `breeder-mutation-regime`（cross-regime swap）＋`breed-strategies` の 15% regime-mutation short-circuit が forward-robust 近傍の Keltner/BB-MR 個体（H4/H6・ATR バリア）を生成。単一栽培の吸収状態を破る唯一のオペレータ。
- **Stage 3（2d＋2f）**: `selection-fitness`＝evidence-adjusted sharpe ÷ niche 混雑数（fitness sharing）を 4 つの生 sharpe 比較子に配線＋`breeding-cpcv-eligible-p`（CPCV pass-rate≥0.60 を育種プール入りの条件）。
- **検証（native-Windows SBCL・`--no-userinit`）**: 全 flag-OFF テストで legacy バイト同一を確認。実抽出ソースでの isolated test＋Rust round-trip＋selection-dynamics シミュレーションが通過。**フル育種ループ（sqlite/zmq/signals）は WSL 限定＝ここでは未検証**（§4 の runbook）。

---

## 1. 実装差分（file / flag-gated）

### Stage 1 — flag ＋ 遺伝子 ＋ emit
| 変更 | file | 内容 |
|---|---|---|
| フラグ定義 | `src/lisp/core/config.lisp` | `*enable-primitive-diversity*`（env-bool-or, 既定 nil）|
| フラグ export | `src/lisp/packages.lisp` | `#:*enable-primitive-diversity*`（swimmy.core）|
| 遺伝子 | `src/lisp/dsl.lisp` | struct に `band-mult`(2.0)/`atr-period`(14)/`atr-barrier-sl`(0.0)/`atr-barrier-tp`(0.0)。既定＝legacy |
| emit | `src/lisp/school/school-backtest.lisp` | `detect-indicator-type-extended`(keltner/donchian)・`primitive-first-int`・`emit-short-long`・`strategy-to-alist` に flag-gated 拡張フィールド。OFF は inline `,@(when *flag* ...)`＝空 → バイト同一 |

### Stage 2 — 探索空間
| 変更 | file:fn | 内容 |
|---|---|---|
| cross-regime swap | `school-breeder.lisp:breeder-mutation-regime` | flag ON 40% で :reversion/:breakout を跨いで指標選択。OFF は `breeder-legacy-regime`＝旧 case と同一 |
| regime mutation | `school-breeder.lisp:breed-strategies` | flag ON 15% で `make-diverse-child` early-return＝Keltner/BB-MR 新個体（period40-64・mult1.5-2.5・ATR2-3x・H4/H6）。legacy AST-crossover 機構を迂回 |

### Stage 3 — 2d fitness sharing ＋ 2f CPCV 前段ゲート
| 変更 | file:fn | 内容 |
|---|---|---|
| selection-fitness | `school-genome.lisp:selection-fitness` / `fitness-sharing-denominator` | OFF=生 sharpe。ON=evidence-adjusted sharpe ÷ (symbol,category) niche 混雑数。第 k clone は ÷k → 484 monoculture は構造的に選択されない |
| 比較子配線（4 箇所）| `school-genome.lisp:210`（tournament）／`school-breeder.lisp` elite-cap sort／parent-child duel／2 age-cull | 生 sharpe → `selection-fitness`。OFF バイト同一（population snapshot で sort 中の混雑数読取を回避）|
| 2f CPCV 前段ゲート | `school-breeder.lisp:breeding-cpcv-eligible-p` ＋ `run-breeding-cycle` の all-warriors | flag ON で cpcv-pass-rate≥`*2c-cpcv-pregate-min*`(0.60) を育種プール入りの条件。OFF は no-op |

---

## 2. 検証（ここで実行可能な範囲・すべて通過）

native-Windows で SBCL 2.6.4 は `--no-sysinit --no-userinit` で pure Lisp 実行可（起動時の sqlite prime クラッシュを回避）。**実ファイルから関数を抽出**して isolated test：

| test | 確認内容 | 結果 |
|---|---|---|
| paren balance | 全編集 file の括弧均衡 | OK |
| `emit_test` | OFF: keltner→sma・拡張フィールドなし（legacy 同一）／ON: keltner・sma_short=period50・band_mult・atr barriers | PASS |
| Rust round-trip | ON-emit の Keltner alist を Rust backtester が消費し実 Keltner backtest（EURUSD p50 H4 → OOS t=219 pf=1.069）| PASS |
| `search_test` | OFF regime==legacy 全 category／diverse gene が forward-robust 近傍／diverse child が keltner/bb を period＋genes 付きで emit（H4/H6）| PASS |
| `sel_test` | OFF=生 sharpe／ON=monoculture を ÷6・lone niche は満額／2f が cpcv<0.6 を遮断 | PASS |
| `dynamics_test` | **OFF: 20-clone TREND monoculture が選択トップ（生 sharpe 1.0）／ON: fitness-sharing で lone diverse(0.7/1)＞monoculture(1.0/20=0.05) → diverse が選択トップ** | PASS |
| Rust 全 test | backtester/kill_oos_cpcv（Stage 前の additive generator）| 85+ green |

`dynamics_test` が 2d の核心（monoculture の選択支配を構造的に破る）を実証。

---

## 3. flag-OFF live-neutrality（可逆性の担保）

- 全ての新挙動は `*enable-primitive-diversity*` で gate。OFF では:
  - emit: inline `,@(when *flag* ...)` が空・type は legacy detector → **alist バイト同一**。
  - 探索: `breeder-mutation-regime`＝`breeder-legacy-regime`、regime-mutation `when` は false。
  - 選択: `selection-fitness`＝生 sharpe、2f は no-op。
- struct 遺伝子は既定値＝legacy（band-mult 2.0・atr-barrier 0.0）。`make-strategy` の既存呼出は無改変（keyword 既定）。
- `IndicatorType::random()`（Rust）に Keltner を足していない＝ライブ育種は Keltner を emit しない。

---

## 4. ⚠️ 未検証（要 WSL）＋ runbook

**ここで実行不能**: フル育種ループ（`start-evolution-service`）は sqlite（swimmy.db）／zmq（Guardian backtest/CPCV）／POSIX signals に依存。native Windows では [[native-windows-sbcl-blockers]] の 3 gate で走らない。よって **flag-ON 育種で「diverse honest-PASS 個体が実際に生まれ、単一栽培が薄まるか」の実測は WSL 限定・未検証**。

**WSL runbook（オーナー実行 or 次セッション）**:
```bash
# 1. swimmy.db の COPY を使う（live 無改変）。flag ON。
export SWIMMY_PRIMITIVE_DIVERSITY=1
# 2. N サイクル育種を回す（paper・ライブ発注経路 OFF）。
#    - 期待: 15% regime-mutation が Keltner/BB-MR (H4/H6) を注入
#    - 2f: cpcv-pass-rate>=0.6 の seed のみ育種 → 単一窓 fit を種にしない
#    - 2d: fitness-sharing が USDJPY/TREND monoculture の選択を ÷niche
# 3. 計測（honest_gate 無改造・実コスト 2pip・purge/embargo CPCV）:
#    - diverse honest-PASS 件数（非USDJPY or 非TREND, 挙動 Keltner/BB-MR）
#    - 個体群の symbol/category 分布が USDJPY487/TREND484 から動いたか
#    - Keltner-MR 系統の forward-robust 生存（2015-20 holdout 再確認）
```
**ブートストラップ注意**: flag ON で 2f が pool を CPCV≥0.6 に絞るため、多様性は「既に CPCV-robust な seed の 15% regime-mutation」から注入される。もし CPCV≥0.6 の seed が皆無だと pool が枯れ得る → `*2c-cpcv-pregate-min*` を下げるか、diverse Keltner 個体を数体 seed する（`save-recruit-to-lisp`）。gonogo では robust≈2（TREND, pr=0.60）なので最低限のブートストラップは可能。

**未検証の具体リスト**:
1. フル育種 1 サイクルが flag ON で例外なく完走するか（Lisp compile＋実行、要 WSL）。
2. regime-mutant child が add-to-kb→phase1→A-rank→CPCV を通過するか（Guardian 実 backtest 経路、要 WSL）。
3. 2f gate 下でのブートストラップ実挙動（pool 枯渇の有無）。
4. 単一栽培が実測で薄まる件数。

---

## 5. コミット列（`claude/2c-primitive-diversity-breeding`）
```
e993fd14 feat(tribe-2c): 2d fitness-sharing selection + 2f CPCV pre-gate (Stage 3)
11e40e0e feat(tribe-2c): search-space regime mutation + cross-regime primitives (Stage 2)
14613a03 feat(tribe-2c): flag + primitive-diversity genes + flag-gated emit (Stage 1)
```
（土台 master 側: 43a47612 harness訂正 / 4ebaac4e forward kill-switch / 6171b2e6 expanded search / c8b6e626 additive Rust generators / 58388be1 walk-forward）
