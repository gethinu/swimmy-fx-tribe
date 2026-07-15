# 🐧 2c 本体 WSL 検証 — flag-ON フル育種の実測（native Windows で不可だった部分）

**日付**: 2026-07-15 JST ／ **ブランチ**: `claude/2c-primitive-diversity-breeding`
**環境**: WSL Ubuntu 24.04（`swimmy` ユーザ）・SBCL 2.2.9・quicklisp。repo は `/mnt/c/Repos/swimmy-fx-tribe`。
**モード**: オフライン・**ライブ発注なし・live `swimmy.db` は一度も open せず**（population は Library の .lisp から read-only ロード。DB copy は `/c/tmp/swimmy_2c_copy.db` に保全）。honest_gate/§4 floor 不変・実コスト2pip・purge/embargo CPCV。**Guardian のライブ発注ループは起動していない**（[[guardian-closes-live-trade-loop]]）。

---

## 0. TL;DR

- **コンパイル**: `asdf:load-system :swimmy`（168 file）が flag-gated 2c 込みで**クリーン通過**（2c 記号への warning 0・全体 warning 3=既存ノイズ）。フラグ `swimmy.core:*enable-primitive-diversity*` は EXTERNAL export・既定 nil、`selection-fitness` fbound を確認。
- **flag-ON 育種の生成コア完走**: 実 population（493・**USDJPY 491 / EURUSD 1 / GBPUSD 1** の単一栽培）から `breed-strategies`＋実ペアリングで **352 pair をエラーなく育種**。
- **多様性注入が実働**: 352 children 中 **55（15.6%）が diverse プリミティブ（Keltner/BB-MR）**、うち **52 が非USDJPY・非TREND**（EURUSD 29 / GBPUSD 23）。＝15% regime-mutation が設計どおり単一栽培を破って EURUSD/GBPUSD の Keltner/BB-MR 個体を注入。
- **diverse honest-PASS（実件数）**: 52 diverse children を standalone honest gate（selection 2021-24＋holdout 2015-20・CPCV pr≥0.6 両窓）で採点 → **forward-robust 1 件**：`Bred-…Gen59 EURUSD keltner p58 H4`（HOLD t=432, pf=1.189, CPCV pr=0.70, med=+0.70）。**非USDJPY・非TREND の forward-robust 個体が実際に育種で生まれた**（オフライン探索が指したエッジ型と一致）。
- **⚠️ 2f プール枯渇（重要）**: 実 population で **CPCV pass-rate≥0.60 は 1 件のみ**（Library 由来。DB には gonogo で robust≈2）。→ **flag-ON＋厳格 2f は初期プールをほぼ枯渇させる**。ブートストラップ要（下記 §3）。

---

## 1. 実測（4 問への回答）

### Q1. flag-ON でフル育種が例外なく完走するか
- **生成コア（breed-strategies＋pairing＋emit）＝YES**：352 pair をエラー 0 で完走。コンパイルも全 168 file クリーン。
- **フル daemon ループ（`start-evolution-service`：add-to-kb→DB→zmq→CPCV dispatch→persistence）＝未実行**。理由：Guardian のライブ発注ループ起動リスク回避（本測定は生成コアを純粋に回す設計）。→ **daemon 完走は未検証**（要 Guardian backtest service のみ起動する隔離、次段）。

### Q2. regime-mutant child が add-to-kb→phase1→A-rank→CPCV を通過するか
- child は**正しく生成**され、standalone 採点で **forward-robust**（上記 Gen59）。
- ライブ昇格パイプライン（phase1 backtest→A-rank→CPCV）通過は **未検証**（daemon＋Guardian 必要）。

### Q3. 2f gate 下でプール枯渇するか → **枯渇する**
- 実 population 493 中 **cpcv-pass-rate≥0.60 = 1 件**（trend）。→ flag-ON＋2f だと**育種親が実質 1 体**＝ほぼ枯渇。
- CPCV 実データは swimmy.db 側（Library .lisp には未格納）。DB でも gonogo 実測 robust≈2（pr=0.60）→ いずれにせよ**極めて少数**。

### Q4. diverse honest-PASS 何件・単一栽培は薄まるか
- **diverse honest-PASS（forward-robust）= 1 件**：EURUSD keltner p58 H4（HOLD pf=1.189, pr=0.70, med=+0.70）。GBPUSD 0（オフラインでも GBPUSD は弱く 1/207）。
- **生成レベルの希釈**：1 育種バッチで children の **15.6% が diverse（非USDJPY/非TREND）**＝単一栽培（USDJPY491/TREND）に対し構造的に多様性を注入。ただし**個体群全体が複数サイクルで薄まるか**は daemon での多世代進化が必要（未検証）。

---

## 2. 手法（安全性）

```
# コンパイル
SWIMMY_PRIMITIVE_DIVERSITY=0 sbcl ... (asdf:load-system :swimmy) -> COMPILE-OK
# 育種測定（純粋・DB非open・ライブ発注なし）: logs/tribe_2c_wsl/measure.lisp
#  - (setf *strategy-knowledge-base* (swimmy.persistence::load-all-strategies))  ; Library read-only
#  - (setf swimmy.core:*enable-primitive-diversity* t)
#  - 8 rep × 各category priority-sort→find-diverse-breeding-partner→breed-strategies  ; add-to-kb 不使用
#  - diverse child を strategy-to-alist(flag ON)→primitive_scan manifest に emit
# 採点: primitive_scan selection+holdout -> score_forward (既述の forward-robust gate)
# 2f プール: logs/tribe_2c_wsl/pregate.lisp
```
`breed-strategies`/`strategy-to-alist` は副作用なし（make-strategy のみ）。DB 書込・zmq・発注は一切呼ばない。

---

## 3. 断定・本番投入の可否判断材料

1. **2c の生成機構は実働・実証済み**：flag-ON で単一栽培（491 USDJPY）から **非USDJPY/非TREND の forward-robust 個体（EURUSD Keltner-MR）を実際に育種**。設計の「吸収状態を破る」が実機で起きる。
2. **ただし 2f が初期プールをほぼ枯渇**（CPCV≥0.6 が 1-2 体）。厳格 2f のまま flag-ON 本番投入すると**育種がほぼ停止**（多様性注入も僅少）。
3. **forward-robust の歩留まりは低い**（52 diverse child → 1 robust ≈ 2%）。children の乱数パラメータが粗いため。finer param mutation か試行数増で改善余地。

**本番投入の推奨（可逆・段階）**:
- **(A) daemon 隔離検証を先に**：Guardian の**backtest service のみ**（ライブ発注ループ OFF）＋swimmy.db COPY で flag-ON daemon を数サイクル回し、Q1/Q2（add-to-kb→CPCV 通過）と多世代の単一栽培希釈を実測。
- **(B) 2f ブートストラップ緩和**：初期のみ `*2c-cpcv-pregate-min*` を下げる or 上記 Gen59 型の diverse Keltner を数体 seed（`save-recruit-to-lisp`）してプール枯渇を回避。
- **(C) diverse 注入の param を forward-robust 中心（EURUSD keltner p50-60 dev2 H4 ATR2x2）に寄せて歩留まり改善。**
- honest_gate/§4 floor は不変・merge/本番はオーナー確認後。

## 3b. 多世代希釈シミュレーション（2026-07-15 追補・安全）

daemon フルループ（zmq/add-to-kb）を起動せず、**実 2c/2d/2f ロジック＋honest 採点（primitive_scan.exe を WSL interop で呼出・OOS 2pip＋purged CPCV）**で進化ダイナミクスを N 世代シミュレート。ライブ発注ゼロ・live DB 無改変。
- **2f ブートストラップ**: `*2c-cpcv-pregate-min*` を 0.60→**0.55** に下げ、forward-robust な diverse Keltner/BB-MR を **20 体 seed**（実 population は CPCV≥0.6 が 1-2 体しかなく親枯渇するため）。seed は実個体を clone して breedability を保持・遺伝的に分散。
- 各世代: 2f-eligible pool から breed（2c）→ child を honest 採点 → 全体を 2d selection-fitness で cap 130 に cull → 分布記録。

**結果（単一栽培は世代ごとに実際に薄まる）**:

| Gen | USDJPY | EURUSD | GBPUSD | div-robust | USDJPY% |
|---:|---:|---:|---:|---:|---:|
| 0 | 120 | 14 | 6 | 20 | 86% |
| 1 | 104 | 18 | 8 | 21 | 80% |
| 2 | 102 | 20 | 8 | 22 | 78% |
| 3 | 98 | 24 | 8 | 23 | 75% |
| 4 | 95 | 27 | 8 | 23 | 73% |
| 5 | 92 | 30 | 8 | 24 | 71% |
| 6 | 87 | 34 | 9 | 24 | 67% |

- **単一栽培 USDJPY 120→87（86%→67%）と単調減少**、diverse（EURUSD/GBPUSD）20→43（14%→33%）と単調増加、**diverse forward-robust honest-PASS 20→24 と累積**。2f-eligible pool も 20→24（forward-robust child が育種プールに自給的に加わる）。
- **機構が世代を跨いで複利で効く**：2d fitness-sharing が巨大 USDJPY niche（÷~90-120）を diverse 個体の下に沈め cull、2f が非頑健 mono の育種を阻止、2c が diverse を注入。
- **限界（正直に）**: (i) seed 必須（実 population は CPCV-eligible が枯渇）＝**本番も初期 seed / 閾値緩和が要る**。(ii) 希釈は緩慢（~2-3%/gen、多数派化には ~15-20 世代）。(iii) daemon の add-to-kb/DB/zmq プラミングは未経由（ダイナミクスは実ロジック＋honest 採点だが cull は top-N 簡略）。

## 3c. daemon プラミング検証 → **2 箇所の live 汚染を発見・修正**（2026-07-15）

flag-ON の diverse child が live daemon の採点経路で正しく Keltner として採点されるかを検証したところ、
**backtester.rs（Guardian 本体）以外の 2 経路で keltner→sma に潰れ、diverse gene が捨てられていた**ことが判明。
これは 2b harness 汚染の live 版であり、放置すると **flag-ON でも diverse child が SMA として誤採点され CPCV 昇格しない**＝2c が本番で機能しない。

| 汚染箇所 | 症状 | 修正 |
|---|---|---|
| `tools/backtest_service.py`（Lisp→Guardian の backtest wrapper）| `_INDICATOR_TYPES` に keltner 無し→sma に正規化・`_sanitize_strategy` が band_mult/atr を forward せず | keltner を whitelist 追加＋4 gene を forward（既定=backtester.rs serde 既定＝flag-OFF 不変）|
| `guardian/src/cpcv.rs`（CPCV 採点＝**2f が読む経路**）| `_ => Sma`（keltner arm 無し）＋Strategy の band_mult/atr を **hardcode** で params 無視 | keltner arm 追加＋band_mult/atr を strategy_params から読取（未指定=既定＝flag-OFF 不変）|

**検証（Guardian `--backtest-only` 実走）**:
- keltner payload → **956,228 trades**、同 param の sma payload → **42,612 trades**＝keltner が SMA に潰れず**別物として dispatch**。
- keltner の ATR バリア有(2x2)=956,228 trades / 絶対バリア=444,993 trades ＝**gene が forward され効いている**。
- Rust 全 test green（85+27+26+26）。flag-OFF は gene 既定＝従来挙動（byte 不変）。

→ **live daemon の採点経路（BACKTEST＋CPCV）が diverse Keltner を正しく採点できるようになった**。これで 2c の flag-ON 本番の残る採点汚染は解消。（ZMQ フル daemon ループの起動実走は未実施＝発注ループ起動リスク回避。各コンポーネントは個別検証済み。）

## 3d. ZMQ daemon プラミング smoke test（2026-07-15・(a) 解除）

**発注ループ未起動・spare port(6580/6581)・swimmy.db は明示 COPY・live 未接触**で、修正後の採点チェーンを実走:
- **backtest_service.py → guardian の実チェーン**: 隔離した `backtest_service.py`（私の /mnt/c 版・Linux guardian をビルドして配置）に Brain 形式の keltner BACKTEST を ZMQ 投入 → **Keltner 956,228 trades ≠ SMA 38,875（DISTINCT=True）**。＝修正した service が keltner を潰さず guardian に通し、real Keltner 採点が返る（発注ループ非起動）。
- **Brain ingestion（add-to-kb）**: `swimmy.core::*db-path-default*` を COPY に明示 bind し、flag-ON で diverse Keltner child を `add-to-kb :require-bt nil` → **accepted=T status=ADDED in-kb=T**。emit type=KELTNER short=51。**dev DB md5 は前後不変**（今回は明示 COPY で本物を開かず。前回の dev DB 誤書きの反省を反映）。
- **cpcv.rs**（2f が読む）は同一 backtester engine で keltner dispatch＋gene 読取（コード修正・rebuild・26 cpcv test green）。

**チェーン全 link 検証済み**: emit(keltner+genes) → add-to-kb 受理 → backtest_service.py forward → guardian real Keltner → (cpcv 同engine)。**残る「full async Brain 無限ループ(start-evolution-service)」の実走のみ未実施**（発注ループ隣接＋legacy infra ＝2c 固有リスクは上記で解消済み）。→ **本番 flag-ON の (a) は実質解除**。

## 6. 本番 flag-ON deploy 手順（具体案）

前提: live は `/home/swimmy/swimmy`（systemd）。**この手順自体は未実行＝オーナー GO 待ち。**
1. **deploy**: `/home/swimmy/swimmy` で `git pull`（master=f6042ee4 系）→ `cargo build --release --bin guardian`（keltner/cpcv fix 反映）→ Lisp fasl 再コンパイル。**この時点では flag OFF＝挙動不変**。
2. **backtest サービス再起動**（backtest_service.py の keltner fix 反映）: `systemctl restart swimmy-backtest`。
3. **2f ブートストラップ seed 投入**: forward-robust 実績型（EURUSD Keltner p50-60 dev2 H4 ATR2x2・Gen59 型）を数体〜20体、`save-recruit-to-lisp` 相当で library/KB へ。閾値は `*2c-cpcv-pregate-min*` を初期 0.55（env 化推奨）。
4. **flag ON**: guardian/Brain の環境に `SWIMMY_PRIMITIVE_DIVERSITY=1` を設定し Brain daemon 再起動。
5. **監視**: 数サイクル、(i) 例外なく回るか (ii) diverse child が phase1→A→CPCV 昇格するか (iii) symbol/regime 分布が USDJPY 単一栽培から動くか。異常時は env を外して即 flag OFF＝完全 revert（可逆）。
6. **段階拡大**: 安定したら seed/閾値を通常運用へ。歩留まり ~2%・希釈 ~2-3%/gen は許容前提。

## 4. コミット/成果
- 実装は `claude/2c-primitive-diversity-breeding`（Stage1-3＋本 WSL 検証）。scripts: `logs/tribe_2c_wsl/{measure,pregate}.lisp`。
- 本 WSL 検証で 2c 実装は **compile-OK＋flag-ON 生成コア完走＋forward-robust diverse 個体 1 件を実育種**。未検証は daemon フルループ（要 Guardian backtest-only 隔離）。
