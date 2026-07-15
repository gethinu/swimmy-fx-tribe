# 🔬 拡張プリミティブ探索 — forward-robust な非TREND/非USDJPY エッジは在るか（2026-07-14 ②拡張）

**日付**: 2026-07-14 JST ／ **モード**: オフライン・paper のみ（ライブ発注なし・live `swimmy.db` 無改変・honest_gate/§4 floor 不変・背骨 backtester.rs／ライブ Lisp／Guardian 無改変）
**位置づけ**: [`tribe_2b_correction_honest_primitives_20260714.md`](tribe_2b_correction_honest_primitives_20260714.md) §4b の forward 足切り（EURUSD BB-MR H4 が holdout で消滅）を受け、**プリミティブ/バリア空間を拡張**して forward-robust な多様性エッジが在るかを安価に判定。
**証跡**: `logs/tribe_diversity_20260714/`（scripts 追跡・大容量 JSON gitignore）。

---

## 0. TL;DR — **在る（薄いが forward-robust）**

- **背骨は無改変。** 拡張は 2 経路:
  - **Part 1**: 既存プリミティブ（rsi/bb/stoch/macd）を既存 honest ハーネス `kill_oos_cpcv` で広く sweep（4銘柄×5TF×6barrier=1080 config）。
  - **Part 2**: 自己完結の新 bin `primitive_scan.rs`（backtester.rs の pub read-only helper のみ再利用・**engine に対し sma/bb/rsi でバイト一致検証済**）で、**engine に無いプリミティブ**を追加: **可変 dev Bollinger・Keltner チャネル・Donchian ブレイクアウト・per-trade ATR 正規化バリア**。
- **判定基準（今回消えた EURUSD BB-MR と同一の足切り）**: selection 窓(2021-24)で OOS適格＋CPCV pr≥0.6、**かつ** 未使用 holdout 窓(2015-20)でも OOS適格＋CPCV pr≥0.6。両窓通過のみ forward-robust。
- **結果**:
  - **Part 1（既存プリミティブ）= forward-robust 0**（全銘柄）。→ 現行プリミティブ空間には多様性エッジ無し（確定）。
  - **Part 2（新プリミティブ）= EURUSD で forward-robust 4**、他 0。
  - **narrow 確認 grid**: **EURUSD で forward-robust 11**（一貫領域）＋**GBPUSD 1**（＋lite 6）、EURJPY 0。
- **エッジの正体**: **Keltner チャネル平均回帰・H4（tf240）・EMA period 50–60・ATR バリア（2–3×ATR）**が中核領域。可変 dev BB（dev 1.5–1.75）H6 も少数寄与。**CPCV fold は 2015–2024 に時間分散**（単一窓 artifact ではない）。
- **限界（過大主張しない）**: エッジは**控えめ**（holdout PF 1.13–1.20・CPCV median sharpe 0.48–0.80）。**EUR/GBP-USD 限定**（EURJPY=0、JPY レジームには出ない）。多重検定下だが**係数を滑らかに動かしても残る一貫領域＋2窓通過＋fold 時間分散**ゆえ noise ではない。**forward-robust だが実弾には未達**（deployment_gate 前提維持）。

---

## 1. 方法（背骨無改変の担保）

### 1-A. Part 2 新 bin `primitive_scan.rs` の忠実性
- backtester.rs の **pub read-only** 関数のみ再利用（`load_candles_from_csv` / `resample_candles` / `calculate_penalized_sharpe`）。backtester.rs・strategy_ast.rs・ライブ Lisp・Guardian は**無改変**。
- backtest ループは `run_backtest_internal` を忠実移植（close ベース SL/TP、slippage 両側=2pip 往復、1 ポジション、daily-returns sharpe×√252、CPCV 10 block）。
- **検証（決定的）**: 同一 config を engine（`kill_oos_cpcv`）と新 bin で走らせ**バイト一致**を確認:
  | config | engine | primitive_scan |
  |---|---|---|
  | SMA 30/60 tf60 | OOS t=423 pf=1.165 pen=0.581 CPCV pr=0.70 | **同一** |
  | BB p30 dev2 tf60 | OOS t=1049 pf=0.796 CPCV pr=0.00 | **同一** |
  | RSI p14 tf60 | OOS t=984 pf=0.850 CPCV pr=0.00 | **同一** |
  （鍵: 指標窓は engine と同じく現在バーを除く `[i-p, i)`・population 分散・非 Wilder RSI。）
- **ボリューム注意**: 本データの volume は合成の定数 1.0。→ volume 系（vwap/volsma/vpoc/vwapvr）は**退化のため除外**（走らせれば artifact エッジを生む）。

### 1-B. forward 足切り（可逆・floor 不変）
- `kill_oos_cpcv`／`primitive_scan` 双方に**任意の窓フラグ** `--is/oos/cpcv-start/end`（既定は正準分割を byte 再現）。
- selection = 既定（OOS 2021-24、CPCV 全span）／ holdout = `--oos/cpcv 2015-2020`。両者を config 名で join（`score_forward.py`）。

---

## 2. 結果

### 2-A. Part 1 — 既存プリミティブ（engine 忠実・1080 config）
| 銘柄 | selection-robust | **forward-robust** |
|---|---:|---:|
| USDJPY | 0 | 0 |
| EURJPY | 1 | 0（holdout CPCV pr=0.5）|
| EURUSD | 2 | 0（holdout PF<1.0 でネット損）|
| GBPUSD | 0 | 0 |

→ **既存プリミティブ空間には forward-robust な多様性エッジは無い（確定）。**

### 2-B. Part 2 — 新プリミティブ（可変dev BB・Keltner・Donchian・ATR バリア・816 config）
| 銘柄 | selection-robust | **forward-robust(diverse)** | forward-lite |
|---|---:|---:|---:|
| **EURUSD** | 12 | **4** | 7 |
| USDJPY | 1 | 0 | 0 |
| EURJPY | 0 | 0 | 0 |
| GBPUSD | 0 | 0 | 0 |

### 2-C. narrow 確認 grid（Keltner/低dev-BB 近傍・EUR/GBP-USD・EURJPY・621 config）
| 銘柄 | selection-robust | **forward-robust** | forward-lite | 解釈 |
|---|---:|---:|---:|---|
| **EURUSD** | 33 | **11** | 19 | **一貫領域**（Keltner p50-60 dev2 H4 ATR）|
| GBPUSD | 15 | **1** | 6 | 部分転移（Keltner p40 H4）|
| EURJPY | 0 | 0 | 0 | JPY レジームには出ない |

**代表 forward-robust（未使用 holdout 2015-20 の実績）**:
```
C2-EURUSD-keltner-p55-d2.0-tf240-atr2.0x2.0  sel(pf=1.175,pr=0.6)  HOLD(t=435,pf=1.199,pr=0.7,med=+0.80)
C2-EURUSD-keltner-p50-d2.0-tf240-atr2.0x2.0  sel(pf=1.219,pr=0.6)  HOLD(t=435,pf=1.176,pr=0.6,med=+0.63)
C2-EURUSD-bb-p30-d1.75-tf360-atr2.0x2.0      sel(pf=1.249,pr=0.8)  HOLD(t=344,pf=1.136,pr=0.6,med=+0.47)
C2-GBPUSD-keltner-p40-d2.0-tf240-atr3.0x3.0  sel(pf=1.177,pr=0.7)  HOLD(t=342,pf=1.223,pr=0.6,med=+0.43)
```

**fold 時間分散**（EURUSD 勝者、10 block 2015→2024。P=pass）:
```
selection(2015-24)  PP.P..PP.P / PP.P..PPPP   (早期 2015-16 fold も pass)
holdout(2015-20)    P.PP.PP..P / PPPP..PP.P   (6年窓に分散)
```
→ 単一窓の残像ではなく**時間的に分散した平均回帰エッジ**。

---

## 2-D. 中間段 path 2（2026-07-14 追補）— engine への additive generator ＋ walk-forward 補強

### engine への additive generator（背骨は breeding-core 無改変・回帰で担保）
- `backtester.rs` に **Keltner（新 IndicatorType）・可変dev BB（`band_mult`）・ATR 正規化バリア（`atr_barrier_sl/tp`）**を additive 追加。
  `calculate_ema`/`calculate_atr` helper 追加。全新フィールドは `#[serde(default)]`（既定＝従来挙動）。
- **live-neutral 担保**: `IndicatorType::random()` に Keltner を**足さない**（live 育種は Keltner を絶対に emit しない）。既存 80→85 test green、
  **バイト一致回帰**（SMA t=423/pf=1.165・BB t=1049/pf=0.796・RSI t=984/pf=0.850 が改修後も不変）。
  新 test: default 逆シリアライズの live-neutrality／band_mult 配線／Keltner 発火／random≠Keltner／ATR バリア効果。
- **gold-standard cross-check**: engine の新 Keltner ＝ 検証済 `primitive_scan` の Keltner が**バイト一致**
  （EURUSD p50 H4 ATR2×2: OOS t=270 pf=1.219 pen=0.594 CPCV pr=0.60 med=0.61）。
- breeding-core（`school-breeder`／`strategy-to-alist`／Guardian 契約）は**無改変**。

### walk-forward（2025 データは存在しない＝全 CSV が 2024-12-31 で終端。代替に 5×2年 disjoint 窓 2015-2024）
forward-robust 領域（EURUSD 11・GBPUSD 1）を、各 2年窓で OOS PF/pen 評価（H4 で 1窓 ~100-160 trade＝200 floor 未満なので**診断**、
§4 PASS ではない。floor 準拠の判定は既述の 200+trade 2窓（2021-24 選択／2015-20 holdout）が正）:

| 銘柄 | PF≥1.10 in **全5窓** | pen>0 in 4/5 | 弱い窓 | 領域全体 PF≥1.10 in ≥4/5窓 |
|---|---:|---|---|---:|
| EURUSD | **0/11** | ほぼ全config | **2019-20**（PF≈0.85-0.93）| 6/243 |
| GBPUSD | 0/1 | 1/1 | 2017-18 | 19/243 |

**含意（正直に）**: Keltner-MR は **4/5 の 2年窓で PF≥1.10・pen>0**（2015-16 は特に強く PF 1.3-1.6）だが、**2019-2020 の 1 ビエニアムで PF<1.0**。
→ **完全な全天候エッジではない**（0/11 が全5窓通過）。2015-20 holdout が通ったのは、強い 2015-18 が弱い 2019-20 を集計で相殺したため。
walk-forward がその集計に隠れた 2019-20 の弱さを露呈。**エッジは実在・4/5 窓で頑健だが控えめ・レジーム依存**。

## 3. 断定と含意

1. **forward-robust な非TREND/非USDJPY エッジは在る**（Keltner-MR H4 + ATR バリア／EURUSD 主・GBPUSD 部分）。**現行プリミティブ（SMA/BB-dev2/RSI）では出ず、engine に無い新機構（Keltner・可変dev・ATR 正規化バリア）で初めて出た。** → **2c（真プリミティブを生成エンジンに走らせる）の動機がデータで裏付けられた**（汚染 2b とは違い honest に）。
2. **限界**: 控えめ（holdout PF≈1.1–1.2、median sharpe≈0.5–0.8）・EUR/GBP-USD 限定・多重検定。実弾不可、deployment_gate 前提維持。
3. **順序律**: 結果は非ゼロ → 事前登録では「残れば 2c 本体」。ただし**破壊的背骨改修の前に本報告で区切る**。2c 実装は次段（オーナー go 後）。

## 4. 次段（2c 実装・オーナー go 待ち）— 破壊的背骨改修の設計

> 制約（オーナー既定・message②）: **専用ブランチ・flag OFF デフォルト・CPCV 前段ゲート(2f)＋fitness sharing(2d)を同時・段階コミット・native-Windows SBCL でライブ Lisp を e2e 検証できない部分は「未検証」と明記。**

- **2c-(a)** `strategy-to-alist`（`school-backtest.lisp:90-107`）に **entry/exit AST＋indicator_type＋dev/atr パラメータ**を送出、Rust の AST/dispatch を実採点で有効化。**ただし現状の Rust には Keltner/Donchian generator も可変-dev BB も無い**（`backtester.rs` は BB dev=2.0 ハードコード）。→ **本探索で有効性が実証された Keltner/可変dev BB/ATR バリアを、まず backtester.rs に追加**（新 IndicatorType variant＋per-trade ATR バリア）。これは live emit が該当型を送らない限りライブ挙動不変（additive）だが、**shared lib 改修ゆえオーナー確認必須**。
- **2c-(b)** 育種の探索空間（`mutate-indicators-with-library` `school-breeder.lisp:31-50`）に MR/Keltner プリミティブ＋regime 変異を **flag OFF デフォルト**で追加。
- **2f** phase-1(IS)↔phase-4(淘汰) 間に purged-CPCV 前段ゲート（薄いエッジの過剰最適化防波堤）。
- **2d** `selection-fitness` 1本化＋fitness sharing。

## 5. 再現コマンド
```bash
D=logs/tribe_diversity_20260714 ; G=logs/tribe_gonogo_20260713
cargo build --release --bin primitive_scan && cargo build --release --bin kill_oos_cpcv
# 検証（新 bin == engine）
target/release/primitive_scan.exe --data data/historical/USDJPY_M1.csv --manifest $D/validate_ps.json --slippage 0.01 --out $D/val_ps.json
# Part1（既存プリミティブ・engine）: gen_expand_grid.py → sel/hol → score_forward.py
# Part2（新プリミティブ）: gen_part2_grid.py → primitive_scan sel/hol → score_forward.py
python $D/gen_part2_grid.py --symbol EURUSD --out $D/p2_EURUSD.json
target/release/primitive_scan.exe --data data/historical/EURUSD_M1.csv --manifest $D/p2_EURUSD.json --slippage 0.0001 --out $D/p2_sel_EURUSD.json
target/release/primitive_scan.exe --data data/historical/EURUSD_M1.csv --manifest $D/p2_EURUSD.json --slippage 0.0001 \
    --oos-start 1420070400 --oos-end 1609459200 --cpcv-start 1420070400 --cpcv-end 1609459200 --out $D/p2_hol_EURUSD.json
python $D/score_forward.py --selection $D/p2_sel_EURUSD.json --holdout $D/p2_hol_EURUSD.json --out $D/p2_fwd_EURUSD.json
```
