# ⚠️ 2b 訂正 — 非トレンド・プリミティブの honest 再測定（harness 汚染バグの修正）

**日付**: 2026-07-14 JST ／ **モード**: オフライン・read-only 診断＋ハーネス実験（ライブ発注なし・live `swimmy.db` 無改変・honest_gate/§4 floor 不変）
**位置づけ**: [`tribe_generation_engine_and_2b_20260713.md`](tribe_generation_engine_and_2b_20260713.md) §2（2b 決定実験）を**訂正**する。姉妹: [`tribe_diversity_engine_rebuild_20260713.md`](tribe_diversity_engine_rebuild_20260713.md)（2a）。
**証跡**: `logs/tribe_diversity_20260714/`（scripts 追跡・大容量 JSON は gitignore）。

---

## 0. TL;DR（何が間違っていて、正しくは何か）

- **バグ**: 2b の採点ハーネス `guardian/src/bin/kill_oos_cpcv.rs` の `build_strategy`（旧 `:143-146`）が
  **`indicator_type` を `"ema"` 以外すべて `Sma` に潰していた**（`_ => IndicatorType::Sma`）。
  → 2b grid の `bb`/`rsi`/`stoch`/`macd` 全 config が**実際には SMA クロスとして走った**。
  Rust backtester 側の dispatch（`backtester.rs:818-841`）は正しいが、**ハーネスがそこに正しい variant を渡していなかった**。
- **実証（同一 param の probe）**:
  | probe | 修正前 | 修正後 |
  |---|---|---|
  | `bb` p30 tf60 sl50tp50 | IS t=683 pf=1.201 / OOS pf=1.165 / CPCV pr=0.70 **QUALIFIED** | IS t=1408 pf=0.840 / OOS pf=0.796 / CPCV pr=0.00 |
  | `sma` 30/60 tf60 sl50tp50 | 同上（一致）| IS t=683 pf=1.201 / OOS pf=1.165 / CPCV pr=0.70 **QUALIFIED** |

  修正前は `bb` と `sma` が**バイト一致** → 2b の「robust BB-USDJPY-H1-p30」は**実は SMA(30/60)** だった。
  修正後、**真の BB 平均回帰は USDJPY H1 で不適格**（pf=0.796, CPCV pr=0.00）。
- **よって 2b の中核主張「BB 平均回帰は USDJPY H1 で CPCV 頑健エッジを安定的に生む」は誤り。** 撤回する。
- **honest 再測定の結論（下記 §2）**: 頑健エッジは**在るが薄く、場所が違う**。
  **USDJPY ではなく EURUSD の BB 平均回帰（H4・period 40–50）**に、CPCV 頑健 config が**2 件**（境界・pr=0.60 ちょうど）。
  そして**真の MR プリミティブは、SMA が OOS 適格を 1 つも出せなかった独立ペア（EURUSD/GBPUSD）で広い OOS 適格領域を開く**
  （EURUSD 58/144・GBPUSD 66/144 OOS 適格）。→ **「プリミティブが効く」自体は honest に成立**、ただし CPCV 頑健は薄い。
- **順序律への回答（訂正版）**: 2c（プリミティブ多様性）の**動機は honest に維持**（プリミティブは独立ペアで SMA に無いエッジを開く）。
  ただし**汚染前提が崩れた以上、ライブ背骨の破壊的改修は「最も可逆」ではない**。CPCV 頑健が薄い（境界 2 件）ため、
  **2c を単独で撃つと薄いエッジへの過剰最適化**になる。**CPCV 前段ゲート（2f）と fitness sharing（2d）を安全装置として同時に要する。**
  → 破壊的ライブ改修は**オーナー green-light 待ち**。本セッションは可逆な部分（ハーネス honest 化＋回帰テスト＋再測定＋訂正）を確定。

---

## 1. バグの詳細と修正

### 1-A. 症状
- `build_strategy`（`kill_oos_cpcv.rs`）:
  ```rust
  // 旧（汚染）
  let it = match m.indicator_type.to_lowercase().as_str() {
      "ema" => IndicatorType::Ema,
      _ => IndicatorType::Sma,     // ← bb/rsi/stoch/macd が全部ここ
  };
  ```
- backtester の signal dispatch は健在（`backtester.rs:818-841` が `strategy.indicator_type` で分岐）。
  → ハーネスが常に `Sma`/`Ema` しか渡さないので、MR ジェネレータ（`generate_bb_signals` 等）は**一度も呼ばれなかった**。
- 影響範囲: **2b のみ**。2a・gonogo は `indicator_type` が `sma`/`ema` の manifest だったため汚染なし（元から SMA）。

### 1-B. 修正
- 全 variant を dispatch（`sma/ema/rsi/bb/macd/stoch/vwap/volsma/vpoc/vwapvr`、未知は WARN 付き Sma）。
- **回帰テスト追加** `tests::build_strategy_dispatches_every_primitive`（`cargo test --release --bin kill_oos_cpcv` → 22 passed）:
  非 SMA ラベルが二度と黙って Sma に潰れないことを pin。

---

## 2. honest 再測定（真の MR/momentum プリミティブ・実コスト 2pip・purge/embargo CPCV）

### 2-A. 2b grid（108 config/銘柄: rsi36 / bb36 / stoch24 / macd12。JPY pip=0.01 / 非JPY pip=0.0001）

§4 バー: OOS trades≥200 & PF≥1.10 & pen≥0.3（=OOS適格）、CPCV pr≥0.6 & med<2.0（=CPCV-ok）、diverse=非USDJPY or 非TREND。

| 銘柄 | OOS 適格 | **CPCV 頑健** | robust&diverse | 適格の主プリミティブ |
|---|---:|---:|---:|---|
| USDJPY | 4 | **0** | 0 | rsi×3, macd×1（全 tf240）|
| EURJPY | 0 | **0** | 0 | — |
| **EURUSD** | 13 | **1** | **1** | bb×10, rsi×3（全 tf240）|
| GBPUSD | 8 | **0** | 0 | bb×7, rsi×1（tf60/240）|

**唯一の頑健&diverse**: `2B-EURUSD-bb-p50-tf240-sl50tp100` — OOS(t=211, pf=1.189, pen=0.487) CPCV(pr=0.60, med=0.374)。
非USDJPY **かつ** 非TREND（実 BB 平均回帰）。**汚染 2b が USDJPY に誤帰属した頑健性は、honest には EURUSD にある。**

### 2-B. BB 確認 grid（144 config/銘柄: period{20,25,30,40,50,60}×tf{120,180,240,360}×barrier 6・BB のみ）

「唯一の頑健 hit は領域か fluke か」を判定:

| 銘柄 | OOS 適格 | **CPCV 頑健(pr≥0.6)** | 近傍(pr∈[0.5,0.6)) |
|---|---:|---:|---:|
| **EURUSD** | 58/144 | **2** | 8 |
| GBPUSD | 66/144 | **0** | 8 |

EURUSD 頑健 2 件 = `bb-p40-tf240-sl50tp50`（pr=0.60, med=0.378）＋`bb-p50-tf240-sl50tp100`（pr=0.60, med=0.374）。
**隣接 period（40/50）・同 tf240** → **単発 fluke ではなく小さな一貫領域**。ただし pr は**フロアちょうど 0.60**・median≈0.37 で**境界**。
GBPUSD は OOS 適格が最広（66/144）だが CPCV 頑健 0（近傍 8）→ **OOS 陽性だが単一窓寄り**。

### 2-C. 断定（honest・過大主張しない）

1. **「プリミティブが効く」は honest に成立**: 真の BB/RSI 平均回帰は、**SMA が OOS 適格 0 だった独立ペア（EURUSD/GBPUSD, 対USDJPY 相関 −0.4x）で広い OOS 適格領域**を開く（EURUSD 58・GBPUSD 66 / 144）。これは 2a の「SMA は非JPY で OOS 適格すら 0」と対比して**プリミティブの実効性を実証**。
2. **CPCV 頑健は薄い・場所は EURUSD の BB H4（period 40–50）**: 全 grid（2b 432 config＋BB confirm 288 config）で頑健&diverse は**計 2 config**、いずれも境界（pr=0.60・med≈0.37）。多重検定下の発見であり、**forward/holdout 確認前は実弾不可**。
3. **USDJPY の真の MR は頑健 0**（汚染 2b の主張と正反対）。EURJPY も 0。
4. **honest_gate/§4 floor は全工程で不変。** live DB 無改変・ライブ発注なし。

---

## 3. ロードマップへの含意（訂正）

> **2c の動機は維持されるが、前提は「薄い・EURUSD BB H4」に更新。破壊的ライブ改修は最も可逆ではない → green-light 待ち。**

- **2c（プリミティブ多様性・破壊的・要オーナー承認）の位置づけ**: honest データは「真の MR プリミティブが独立ペアでエッジを開く」を支持するが、**CPCV 頑健は境界 2 件**。
  ライブ育種にプリミティブを注入するだけ（2c 単独）だと**薄いエッジへ収束→過剰最適化**の危険。→ **CPCV 前段ゲート（2f）と fitness sharing（2d）を安全装置として同時に要求**。
- **本セッションで確定した可逆成果**:
  - ハーネス honest 化（`build_strategy` 全 dispatch）＋回帰テスト（汚染再発防止）。
  - honest 再測定（2b grid ×4 銘柄＋BB confirm ×2 銘柄）。
  - 訂正の durable 記録（本doc＋汚染 doc へ訂正バナー）。
- **保留した破壊的成果（green-light 待ち）**: ライブ `school-breeder.lisp`（cross-regime 指標変異＋regime 変異）／`school-backtest.lisp:strategy-to-alist`（AST/primitive 送出）の改修。
  理由: (i) 前提が汚染で崩れ、honest エッジは薄く場所が違う → 破壊的背骨改修は**最も可逆ではない**。
  (ii) ネイティブ Windows SBCL 制約でライブ Lisp 経路を本環境で end-to-end 検証できない → 未検証の背骨改修 commit は「green を壊さない」原則に反する。
  → §4 に**実装計画**（feature-flag・段階・file:line）を用意し、オーナー承認後に着手。

## 4. 次アクション（実装計画・オーナー green-light 待ち）

**優先度順（安価・可逆・決定的を先に）:**

1. **[可逆・本セッション済]** ハーネス honest 化＋回帰テスト＋honest 再測定。
2. **[推奨・次・可逆]** EURUSD BB H4 の**forward/holdout 確認**（2025 データ or 2021-24 を holdout 分割）で境界 2 件が forward で残るか。残らなければ 2c 破壊的改修は正当化されない（安価な足切り）。
3. **[破壊的・要 green-light]** 2c-(b): `strategy-to-alist` に entry/exit AST＋`indicator_type` を送出（`backtester.rs:767,811` の AST eval 経路を実採点で有効化）。**feature-flag デフォルト OFF**でライブ挙動不変を保証。
4. **[破壊的・要 green-light]** 2c-(a): `mutate-indicators-with-library`（`school-breeder.lisp:31-50`）に cross-regime swap 確率＋regime 変異オペレータ。同上 flag ガード。
5. **[破壊的・2c と同時必須]** 2f: phase-1(IS) と phase-4(淘汰) 間の CPCV 前段ゲート（薄いエッジへの過剰最適化防波堤）。

**2e（銘柄多様性）への判断**: honest データは「独立ペアに OOS 適格領域は在る（EURUSD/GBPUSD BB-MR）」を示す
→ **銘柄多様性は 2c（真プリミティブ）と一体で意味を持つ**（SMA のままでは 2a の通り即 JPY 崩壊）。単独 2e は不可、2c/2f 後。

---

## 5. 再現コマンド
```bash
D=logs/tribe_diversity_20260714 ; G=logs/tribe_gonogo_20260713
# honest ハーネス（build_strategy 全 dispatch）で再ビルド
cargo build --release --bin kill_oos_cpcv
cargo test  --release --bin kill_oos_cpcv     # -> 22 passed (回帰テスト含む)
# 2b grid（真プリミティブ）
for S in USDJPY EURJPY ; do python logs/tribe_diversity_20260713/generate_2b_grid.py --symbol $S --pip 0.01   --out $D/grid2b_$S.json ; done
for S in EURUSD GBPUSD ; do python logs/tribe_diversity_20260713/generate_2b_grid.py --symbol $S --pip 0.0001 --out $D/grid2b_$S.json ; done
# USDJPY/EURJPY は --slippage 0.01、EURUSD/GBPUSD は 0.0001
target/release/kill_oos_cpcv.exe --data data/historical/EURUSD_M1.csv --manifest $D/grid2b_EURUSD.json --slippage 0.0001 --out $D/grid2b_results_EURUSD.json
python $G/score_gate.py --results $D/grid2b_results_EURUSD.json --out $D/grid2b_gate_EURUSD.json
# BB 確認 grid
python $D/gen_bb_confirm.py --symbol EURUSD --pip 0.0001 --out $D/bbconf_EURUSD.json
target/release/kill_oos_cpcv.exe --data data/historical/EURUSD_M1.csv --manifest $D/bbconf_EURUSD.json --slippage 0.0001 --out $D/bbconf_results_EURUSD.json
python $G/score_gate.py --results $D/bbconf_results_EURUSD.json --out $D/bbconf_gate_EURUSD.json
```
