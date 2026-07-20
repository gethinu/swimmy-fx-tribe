# tribe 多様性探求 — 総括 (2d/2a investigation summary, canonical) — 2026-07-19

**これは何か.** 2026-07-18〜21 に走った tribe 多様性エンジン探求 **10 段**の集約・整合レコード（⑩ hold-to-barrier 軸を 2026-07-21 追加）。個別 doc は各段の
一次記録（そのまま保存）、本 doc が **state-of-truth の入口**。読む順に迷ったら **ここ → 該当段の一次 doc**。
設計正本は [`regen_engine_redesign_20260703.md`](regen_engine_redesign_20260703.md)、umbrella は
[`tribe_diversity_engine_rebuild_20260713.md`](tribe_diversity_engine_rebuild_20260713.md)、kill 基準は
[`../KILL_CRITERIA_20260703.md`](../KILL_CRITERIA_20260703.md) §4。

**モード（全段共通・厳守）:** オフライン・**ライブ発注ゼロ・ライブ `swimmy.db` 未接触（copy のみ）・本番フラグ既定 OFF・
honest_gate / §4 floor 不変・flag flip 未実施**。検証は redirect 済ハーネス（`daemon_verify.lisp` / `primitive_scan.exe`）
のみ、repo ルートで `run-all-tests` フル suite は回さない（library 汚染回避）。無関係 dirty（LEGEND/guardian）未接触。
ブランチ `claude/2d-behavioral-distance`（未マージ・未デプロイ）。

---

## 0. 最終確定（すべての doc をここに整合させる）

1. **単一栽培（USDJPY-TREND monoculture）を内側から破るのは不可能 — 突破には「物差し拡張」が必須。**
   探索深度でも・種の数でも・育種機構でもなく、**表現できる特徴量そのもの**の問題。現行の
   representable primitive は 6 種（`sma, rsi, stoch, bb, keltner, donchian`）で、その中で唯一 forward-robust に
   なる機序は **平均回帰 1 家系のみ**。真に別家系の多様性を得るには、今日表現できない特徴
   （session/time-of-day・microstructure/order-flow・multi-TF/cross-pair）＝**新プリミティブ**が要る。
   — この方向の**初回実測**が ⑨（session 軸）: 表現できる機序は確かに拡がる（frictionless で EURJPY・breakout・trend が
   出現し、初めて MR-EUR/USD 以外の multi-family mechanism が測定された）が、realistic 2-pip では **全銘柄 0 robust** に落ち、
   **壁は mechanism → COST に移る**。ゆえに有効レバーは『任意の新プリミティブ』ではなく **低頻度・1 トレード厚利**
   （fewer/larger trades — H4/H6 の MR-EUR/USD が生き残る理由）に絞られる。
   — その**厚利レバーの直接実測**が ⑩（hold-to-barrier 軸, `tribe_2d_hold_barrier_yardstick_20260721.md`）: 指標 exit を
   外して winner を wide barrier/multi-day time-stop まで走らせる＝1 トレードを厚くする。結果は **⑨ の COST 壁を突破**
   （厚いトレードは 2-pip を PF 1.30→1.23 程度でほぼ生き残る — ⑨ の薄い M15 は 1.0 割れだった）**が、realistic も frictionless も
   0 load-bearing・新家系 0**。壁が **COST → DENSITY（≥200-trade floor で 62% が脱落）＋ 2-window CPCV robustness** に再移動
   （厚利 ⟹ 低頻度 が density floor と直交衝突、frequent-and-fat な少数は両 window で不安定）。**⑨=fewer 軸・⑩=fatter 軸は
   どちらも実装・実測され、各々別の honest-floor で落ちた**。結論の更新: 壁は単一の軸ではなく **joint box
   {trades≥200, cost-survivable, 2-window-robust}**（現データの内点は MR-EUR/USD H4/H6 の 1 点のみ）。次軸が攻めるべき自由次元は
   cost でも density でもなく **2-window robustness そのもの**（両レジームで安定な直交エッジ = cross-pair relative-value か新データ軸）。
2. **bundle 移植ルート・seed 数増しルートは打ち止め（closed）。** 独立 3 run（seed escape `fed9bf7b` /
   bundle import `1db94384` / data-wall `c20098d8`）＋網羅 census（`6ee78541`）が同一結論。再燃させない。
3. **多様性機構は全て可逆・flag-gated・既定 OFF で棚載せ。** B-4 距離 / B-5 overflow sort / B-3 niche-quota+symbol-mutation /
   seed escape / 2f pre-gate / **session 軸 gate（scorer + 休眠 `:session` genome hook）** — すべて **OFF は byte-identical**
   （Lisp: master `*enable-primitive-diversity*` 既定 nil で全 path 分岐; Rust scorer: no-session 既定で SHA256 一致）、
   honest floor 未改変、flag flip 未実施。
   コードとしては正しく・安全に ship 可能だが、「単一栽培希釈」目的での本番 ON は**測定上正当化されない**。
4. **内側で唯一の forward-robust な多様エッジ = Keltner/BB 平均回帰・EUR/USD 系メジャー・H4/H6・1 家系のみ。**
   ただし **deployment gate 未満**（holdout PF ~1.13–1.20, median-sharpe ~0.5–0.8）＝多様化材料であって live edge ではない。
5. **数値の確定値（§1 の訂正後）:** USDJPY §4-floor 均衡 **絶対数 = 62（全 run 不変）**／honest な GEN6 希釈 floor **≈ 84–85%**／
   honest な diverse-robust plateau **= 2–3**。

> **§4 verdict との関係（不変）:** kill memo §4-2（多様性）は依然 **未充足**。honest PASS は出る（60, `tribe_gonogo` 参照）が、
> §4 決定的テストの「diverse ≥1 robust」は満たされない（[[kill-criteria-s4-verdict]]）。本探求はこの結論を
> **より強く・網羅的に**裏づけた（「非USDJPY 独立エッジ＋その price data を作らない限り §4-2 は永久に満たせない」）。

---

## 1. ⚠️ 訂正 — 「希釈 72.9–75.6% / diverse-robust ~17–23」は cosmetic seed のアーティファクト

**経緯.** 2a / B-5 / B-3 の 3 run（すべて 2026-07-18）は、同一の隔離ハーネスに **20 体の EUR/GBP REVERSION クローンを
`cpcv=0.7 / sharpe=0.7` を pre-set・`:require-bt nil`（honest gate 未通過）で seed** して回した。これらは
**検証を捏造した cosmetic seed**（構造上、非robust なのに robust としてカウントされた）。よって:

- これら 3 run の **diverse-robust ~17–23 は水増し**（fabricated seed が分母と robust 数を膨らませた）。
- これら 3 run の **GEN6 USDJPY% 72.9–75.6% も cosmetic に低め**（fake seed が非USDJPY 分母を嵩上げ）。

**訂正の初出は seed-escape doc（`fed9bf7b`）** で、そこが seeding を honest path に張り替えた（`inject-diversity-seeds`
→ `add-to-kb :require-bt t` → `primitive_scan` 実採点 → 実 Phase-1 → DB 永続、**perf 数値ゼロで entry のみ付与**）。
honest に採点すると fake seed は消え、**USDJPY% は ~84% に戻り**、**diverse-robust は 2–3 に収束**する。

**重要:** 3 run の**機序的結論はいずれも有効**（下表「mechanism verdict」）。cosmetic だったのは **絶対 %・robust 数**だけ。

### honest ↔ cosmetic 対照表（GEN6, copy DB, 6 世代）

| run（時系列） | seed の性質 | GEN6 USDJPY% | diverse-robust | 数値の正直さ |
|---|---|---:|---:|---|
| **A** (2a daemon-iso) | 20 fabricated `cpcv=0.7` REVERSION | 72.9%(Run B)/77.9%(Run A) | ~21 | **COSMETIC** |
| **B-5** (overflow sort) | 同・fabricated 20 | 75.6% | 20 | **COSMETIC** |
| **B-3** (niche-quota) | 同・fabricated 20 | 75.6% | 17 | **COSMETIC** |
| **seed escape** | 18 honest TREND-first（実採点） | **83.8%** | **3** | HONEST |
| **inside-out census** | 18 honest（Keltner 領域込・実採点） | **84.9%** | **2** | HONEST |
| **bundle founders** | 9 real bundle edge（実採点） | 96.9% | **0** | HONEST |

USDJPY **絶対数は全 run で 62**（§4 honest floor 均衡）。bundle run が 96.9% と高いのは、希釈に使える
forward-robust な多様 seed が 1 つも無かった（0/9）ため — 単一栽培が薄まらなかったことの正直な現れ。

---

## 2. 10 段の探求 — time-ordered

各段: 何を実装/測定したか → **機序の結論（有効）** → 数値注記。詳細は各一次 doc。

### ① B-4 行動距離 — `tribe_2d_behavioral_distance_20260718.md`
`calculate-genetic-distance` が銘柄/TF/behaviour を一切見ていなかった根本ギャップを修正（flag-gated, OFF=byte-identical）。
**結論:** 距離が niche を見るようになった — 真クローンは median **0.0010**（<0.02 clone-floor）、非USDJPY は 100%>0.20。
behaviour corr は `strategy_daily_pnl` が空のため休眠（**捏造せず**、データが入れば自動 PRIMARY 化）。距離は前提条件、まだ希釈はしない。

### ② A daemon-isolation 検証 — `tribe_2a_daemon_isolation_20260718.md`
2c が残した唯一の未検証リンク（flag-ON 実育種パイプラインが多世代で回るか）を copy DB 上で実測。
**結論（有効）:** 実パイプライン（add-to-kb→Phase-1→A-rank→cull）が flag-ON で完走、diverse child が rank-B 昇格・永続、
fitness-sharing 装置が発火＋自己制限（密 USDJPY niche 81→62 @gen1）、隔離厳守を実測。**希釈の主因は §4 honest floor**
（脆弱 mono の淘汰）であって 2c/2d 機構ではない、per-cycle cull は raw score（→ B-5 gap を予告）も確定。
**⚠️ 数値注記:** 当時の「72.9–77.9% 希釈 / diverse-robust 0→~21」は §1 のとおり cosmetic。honest 値は ~84–85% / 2。
当時の判定 "CONDITIONAL GO" は「検証ギャップが閉じた」意で有効だが、**デプロイ含意は §0 の最終確定（deploy NO）で上書き**。

### ③ B-5 overflow sort-key — `tribe_2d_regen_b5_overflow_20260718.md` (`519c3047`)
per-cycle `cull-pool-overflow` の sort を raw score → `selection-fitness`（fitness-sharing 分母つき）へ（flag-gated）。offline 2/2 PASS。
**結論（有効）:** **必要だが単独では不十分。** `cull-pool-overflow` が regime-class 単位スコープのため、単一銘柄プールでは
fitness-sharing 分母が一様＝除算がキャンセル → 単一栽培を優先淘汰できない。§B-3(3) part-1（per-niche スコープ）が別途必要。
**⚠️ 数値注記:** 75.6% vs A 72.9% は cosmetic（§1）。機序結論は不変。

### ④ B-3 niche-quota + symbol-mutation — `tribe_2d_regen_b3_niche_quota_20260718.md`
`cull-pool-overflow` を (symbol×regime) niche クォータ water-filling に再構成（§B-3(3) part-1）＋ 低頻度 symbol-mutation
オペレータ（§B-3(2)）。offline 4/4 PASS。
**結論（有効）:** **機構は正しいが今日の単一栽培には no-op。** TREND プールは **一度も育種されない**
（(a) 2f CPCV pre-gate が CPCV-poor mono を除外、(b) B-4 clone-block が USDJPY-TREND 同士を阻止）→ symbol-mutation が
TREND 親に発火しない → プールは 62/62 USDJPY = 単一 niche → quota が噛む相手が生成されない。**§B-3(2) と §B-6 の構造的緊張。**
niche クォータが唯一効いたのは銘柄混在の REVERSION プールのみ。**⚠️ 数値注記:** 75.6% は cosmetic（§1）。

### ⑤ seed escape route（最終手段）— `tribe_2d_regen_seed_escape_route_20260718.md` (`fed9bf7b`)
育種 CPCV pre-gate（ENTRY）**だけ**を緩め、非USDJPY-TREND clone を直接注入。**perf は一切捏造せず**、同じ honest gate で採点。
**ここで §1 の cosmetic 訂正が確定した。** 18 TREND-first seed。
**結論:** **majority 破れず（GEN6 83.8% — 正直に測ると 4 run 中最悪）**、USDJPY 絶対数 62 不変。
非USDJPY-TREND は weak Phase-1 は通るが **CPCV-robust 0（max 0.20）= robust bar では cosmetic**（SMA トレンドは
in-sample のみ通り OOS robust に届かない）。唯一の honest 多様エッジ = **EURUSD REVERSION MR**（2 seed robust・実 breed で
4 児・1 robust → diverse-robust 3）。**Deploy NO。** 脱出路は機構としては動くが単一栽培は破れない — honest floor が
非robust を正しく拒否し、robust USDJPY core が §4-floor 均衡（62）に座るため。

### ⑥ bundle-founder 移植 — `tribe_2d_bundle_founder_import_20260719.md` (`1db94384`)
「mt5_Bundle の実・多銘柄エッジで tribe を founding すれば多様 robust 宇宙になる」仮説の honest 検証。
**結論: 仮説 REJECTED（構造的）。** 実 bundle 非USDJPY エッジは大半が **import 不能**（MACD/CCI/EMA-cross/SuperTrend/
calendar/session/anchor = tribe genome/scorer 外、または tribe data 無し銘柄）。**{6-prim で表現可} ∩ {tribe data =
EURUSD/GBPUSD/EURJPY} ∩ {非USDJPY} ≈ 空。** 表現可能で data ある部分集合は **0/9 CPCV-robust**（anchor を剥ぐと net loser、
または ≥200-trade density floor で死）。**synthetic keltner seed（3 robust）が real bundle edge（0）に勝った** — レバーは
seed でなく gate/data。6-gen 96.9% USDJPY, diverse-robust 0。

### ⑦ data-wall s132/n0006 — `tribe_2d_data_wall_s132_n0006_20260719.md` (`c20098d8`)
⑥ で唯一「表現可・data のみが壁」だった 2 エッジ（s132=GBPJPY Donchian-7 D1, n0006=EURCHF BB-20 D1）の
**real canonical D1・real 銘柄**を tribe scorer に投入し、data 壁を除去。
**結論: 両者 VERDICT (ii) = 機序が非robust（density ではない）。** OOS 2021-25 は positive（PF~1.5, penSh~0.5）だが
**CPCV 0.30 pass-rate（0/10 density-starved・7/10 genuine loser）**、frictionless でも不変。single-window OOS が (i) に
見せかけるが、full-history CPCV が (ii) を暴く。**「data だけが壁」仮説 revived せず。** s132 block 2010-12 は tribe の
gate-less Donchian core が PF0.38 で負ける — full s132 が +124p だったのは表現できない EMA50/200 gate が edge だったから。

### ⑧ inside-out 網羅 census — `tribe_2d_inside_out_diversity_20260719.md` (`6ee78541`)
「tribe が自分の物差しだけで forward-robust 多様性を内側から育てられるか」を **9,408 config 網羅**（4 symbol × 6 prim ×
7 TF × 全パラメータ近傍、2-window forward gate）＋ **frictionless 上界**で決着。
**結論:** **内側 forward-robust の天井は Keltner/BB-MR・EUR/USD・H4/H6 の 1 家系のみ** — realistic でも frictionless でも同じ。
`rsi/stoch/donchian/sma` は全滅、M15/H1/H2/H12/D1 全滅、**EURJPY は cost=0 でも 0**（JPY-cross 不在は cost でなく mechanism）。
doc の "3" は代表部分集合で、網羅で **variant は 3→15 に増えるが家系は 1 のまま**（密度であって新規性ではない）。
honest seed で breed すると USDJPY 99.6→**84.9%** 希釈、diverse-robust は **seed した 2 で plateau**、§4 floor が 62 を保持。
**内側から 1 niche を育て・維持・希釈するのは YES（可逆）。真の multi-family 多様性を内側から育てるのは NO（網羅的に確定）
⇒ 物差し拡張が必須。**

### ⑨ 物差し拡張① — session/time-of-day 軸 — `tribe_2d_session_axis_yardstick_20260719.md` (`c5cd0f3d`)
⑧ の結論「物差し拡張が必須」を受けた**初の実行**。6 プリミティブ（全て OHLC の純関数）に直交する**時計の純関数**
= trading-session/time-of-day entry gate を honest scorer に追加（flag-gated; no-session 既定は SHA256 byte-identical、
genome 側 `:session` 項は `:behavior` と同型の休眠 hook で renormalise-away）。2,448 config を 2-window forward gate で
realistic 2-pip **と** frictionless 両方採点。
**結論（有効・新種の結果）: frictionless YES / realistic-cost NO。** 摩擦ゼロでは 24h scan が構造的に見えない
**18 の多様 robust エッジ・14 新家系**が出現（`EURJPY-bb-MR-ASIA`〔census では frictionless でも 0〕/
`USDJPY-donchian-BREAKOUT-OVERLAP`〔単一栽培の銘柄の**非TREND**エッジ〕/ `EURUSD-sma-TREND-NY` 等、session→regime 結合は
機序的に整合し 3 銘柄を跨ぐ = noise ではない）。**これは 2d 全線で初めて MR-EUR/USD 以外の mechanism 多様性を出した**。
だが realistic 2-pip では **全銘柄 0 robust** — session エッジは薄い高頻度 M15/H1 で spread が per-trade edge を食う
（例 `bb-EURUSD-ASIA` PF 1.12→0.65, pass-rate 0.60→0.00）。deploy に最も近い `keltner-GBPUSD-MR`（ASIA/OVERLAP）は
2-pip で sel-robust だが holdout 落ち、かつ既知の band-MR 家系で新規性なし。
**結論の含意:** 物差し拡張は representable *mechanism* を実際に拡げる（⑦ data-wall の鏡像 — あちらは壁が mechanism、
こちらは壁が **COST**）が、honest bar が報いるのは **低頻度・厚利**（H4/H6 の MR-EUR/USD が生き残る所以）。次軸は「更なる
intraday」ではなく **low-freq/fat-per-trade**（`dow_mask` 実装済・event/regime 条件・multi-day swing）に絞る。**Deploy NO, flag OFF。**

### ⑩ 物差し拡張② — hold-to-barrier / fat-per-trade 軸 — `tribe_2d_hold_barrier_yardstick_20260721.md`
⑨ の「低頻度・厚利を狙え」を受けた**最も直接的な厚利レバー**の実測。scorer の exit は従来
`sl || tp || indicator-exit || timeout` で指標 exit が**無条件 OR**＝MR は必ず mid で強制決済（TP をいくら広げても
band→mid で頭打ち＝厚利が高 TF でしか取れない根本原因）。唯一の representational gap = **指標 exit を外す** flag-gated
`hold_mode:"barrier"` を追加（no-key は SHA256 byte-identical、⑨ と同じハッシュ `0cfa3445…`; genome gene は
未populate＝edge 無しに carriage を作らない）。entry の稀少/確信は既存 `dev/period` で表現済ゆえ厚利のみを増分。
768 config（16 core × 3 TF{H1/H4/D1} × 4 exit）を 2-window forward gate で realistic 2-pip **と** frictionless 採点。
**結論（有効・新種の failure mode）: realistic 0 新規・frictionless も 0 load-bearing/0 新家系 — だが COST 壁は突破。**
厚いトレードは 2-pip をほぼ生き残る（PF 1.30→1.23、⑨ の薄い M15 は 1.0 割れ）＝**⑨ の cost 壁を解決した唯一のレバー**。
しかし壁が **DENSITY（barrier config の 62% が <200-trade で脱落）＋ 2-window CPCV robustness** に再移動: 厚利 ⟹ 低頻度が
≥200-trade floor と直交衝突し、frequent-and-fat な 26 個（`donchian-USDJPY-BREAKOUT-H4`・`EURJPY`・`GBPUSD` 等の**多様**含む）は
CPCV pass-rate 0.30–0.50 で両 window robust に届かない。realistic の最近接は `sma-EURUSD-TREND-H1-barrier`（多様・厚利・
2-pip で **selection-robust** pf 1.225/cpcv 0.60）だが **holdout 落ち**（2015-21 pf 1.086）＝single-window artifact。
唯一の 2-window robust は既知 `keltner-EURUSD-MR-H4`（signal exit・hold-to-barrier 不要）。**Deploy NO, flag OFF。**
含意: **⑨=fewer 軸・⑩=fatter 軸はどちらも別 honest-floor で落ちた**。壁は単一軸ではなく joint box
{trades≥200, cost-survivable, 2-window-robust}（内点＝MR-EUR/USD H4/H6 の 1 点）。次軸は cost/density でなく
**2-window robustness（両レジーム安定）を攻める** — cross-pair relative-value か新データ軸。

---

## 3. 次の一手（オーナー判断）— 物差し拡張のレバー

| 案 | 実体 | headroom |
|---|---|---|
| **cross-pair relative-value / 新データ軸（残る本命）** | 両レジーム（2015-21 & 2021-25）で安定な直交エッジ — cross-pair の構造 spread か、現4銘柄に無い家系を持つ新 instrument | **中** — ⑨/⑩ が自由次元を絞った: cost（⑩ が突破）でも density でもなく **2-window robustness そのもの**が残る壁。session（fewer）も hold-barrier（fatter）も別 floor で 0。狙うは「片 window だけでなく両 window で安定な」直交特徴 |
| ~~新プリミティブ/特徴（intraday/厚利）~~〔⑨⑨⑩ 実測・打ち止め〕 | session gate〔⑨〕・hold-to-barrier〔⑩〕 | **低（実測 0）** — ⑨ は frictionless 14 家系だが cost で 0、⑩ は cost 突破だが density+robustness で 0。fewer 軸・fatter 軸とも実装済・0 load-bearing。単純 intraday 追加や seasonality/regime gate 単体は⑨の cost 壁を継承、hold-barrier に stack すると density を悪化 |
| 新データ（単独では弱い） | GBPJPY/USDCAD/EURCHF/XAU の M1 を `data/historical/` に追加 | 低 — data-wall が実証: 非robust 機序は data を足しても robust にならない。edge のある primitive と**組んで初めて**効く |
| gate/instrument 変更 | rare-event D1 向け small-N sealed holdout（≥200-trade CPCV の代替） | 中 — ただし s132/n0006 は per-regime PF が 1.0 を跨ぐので、N 適正な instrument でも落ちる（cheap win ではない） |

**やらないこと（打ち止め）:** bundle-import の再試行、seed 数/構成の再チューニング、探索深度の追加、単一栽培への機構追加。
いずれも 10 段が「不足は機構でも深度でも seed でもない」と実測済み。

**今すぐ ship 安全なもの（コード）:** B-4/B-5/B-3/seed-escape/**session-軸 scorer+休眠 genome hook**/**hold-to-barrier 軸 scorer（`hold_mode`）** は全て可逆・OFF byte-identical・honest floor 中立・offline 実証済。
merge は安全だが、**単独での本番 flag ON を「単一栽培希釈」目的で正当化してはならない**。flag flip はオーナー判断（未実施）。

---

## 4. 一次 doc リンク（時系列）

| # | 段 | doc | commit |
|---|---|---|---|
| ① | B-4 行動距離 | `tribe_2d_behavioral_distance_20260718.md` | — |
| ② | A daemon-isolation | `tribe_2a_daemon_isolation_20260718.md` | `c96550ea` |
| ③ | B-5 overflow sort | `tribe_2d_regen_b5_overflow_20260718.md` | `519c3047` |
| ④ | B-3 niche-quota | `tribe_2d_regen_b3_niche_quota_20260718.md` | — |
| ⑤ | seed escape（訂正の初出） | `tribe_2d_regen_seed_escape_route_20260718.md` | `fed9bf7b` |
| ⑥ | bundle-founder 移植 | `tribe_2d_bundle_founder_import_20260719.md` | `1db94384` |
| ⑦ | data-wall s132/n0006 | `tribe_2d_data_wall_s132_n0006_20260719.md` | `c20098d8` |
| ⑧ | inside-out census | `tribe_2d_inside_out_diversity_20260719.md` | `6ee78541` |
| ⑨ | 物差し拡張① session 軸 | `tribe_2d_session_axis_yardstick_20260719.md` | `c5cd0f3d` |
| ⑩ | 物差し拡張② hold-to-barrier 軸（最終） | `tribe_2d_hold_barrier_yardstick_20260721.md` | (this run) |

前段: [`tribe_2c_wsl_verification_20260715.md`](tribe_2c_wsl_verification_20260715.md) /
[`tribe_2b_correction_honest_primitives_20260714.md`](tribe_2b_correction_honest_primitives_20260714.md) /
[`tribe_expanded_primitive_forward_20260714.md`](tribe_expanded_primitive_forward_20260714.md)（2-window forward baseline）。
