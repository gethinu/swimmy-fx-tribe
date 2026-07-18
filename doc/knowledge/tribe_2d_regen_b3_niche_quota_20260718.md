# 🧪 2d regen §B-3 — (symbol×regime) niche-quota overflow cull + symbol-mutation operator (implemented + measured)

**日付:** 2026-07-18 JST ／ **ブランチ:** `claude/2d-behavioral-distance`（base `3f377377` = B-5 doc）
**モード:** オフライン・**ライブ発注ゼロ・ライブ swimmy.db 未接触・本番フラグ既定 OFF のまま**。honest_gate/§4 floor 不変。flag flip 未実施（オーナー判断）。

> **これは何か:** [`tribe_2d_regen_b5_overflow_20260718.md`](tribe_2d_regen_b5_overflow_20260718.md) §4 が「sort-key 差し替え（§B-5/part-2）単独では単一栽培を希釈できない。`cull-pool-overflow` を (symbol×regime) niche 単位に再構成する §B-3(3) part-1 が別途必要」と実測で確定した follow-up。設計正本 [`regen_engine_redesign_20260703.md`](regen_engine_redesign_20260703.md) §B-3(2)/(3)。**結論を先に：2つの機構は実装済・オフラインで機序を実証したが、A比較（6世代・copy DB・実パイプライン）では単一栽培は希釈されなかった（GEN6 USDJPY% = 75.6% = B-5 と同値、A の 72.9% より弱いまま）。原因は「脱出路（銘柄変異）が発火しない」こと：CPCV-poor な USDJPY-TREND 単一栽培は 2f CPCV pre-gate と B-4 クローン検知で育種が完全停止しており、育種経路に載る銘柄変異オペレータは TREND 親に一度も適用されない。よって TREND プールは 62/62 = 100% USDJPY のままで、niche クォータが噛む相手（プール内の銘柄異質性）が生成されない。**

---

## 0. TL;DR

- **実装（flag-gated, OFF=byte-identical）:**
  - **§B-3(3) part-1**: `cull-pool-overflow` の生存者選抜を **(symbol×regime) niche クォータ water-filling** に再構成。各 niche の上限 = `max(*overflow-niche-min-quota*=3, ceil(cullable-slots / live-niches))`。過密 niche を先に溢れ淘汰し、疎な off-niche を保護。**単一 niche のプールでは cap == slot 数 = no-op**（＝ B-5 で判明した「除算キャンセル」の構造版：2つ目の niche が現れて初めて噛む）。B-5(part-2, selection-fitness sort)と組で動く。
  - **§B-3(2)**: 育種時の低頻度 **銘柄変異オペレータ** `maybe-mutate-child-symbol`（10%、flag ON、USDJPY→EURUSD/GBPUSD、**regime は保持**）。B-4 で :symbol は第一級遺伝子（`extract/implant-genome`）なので、それを摂動するだけ。normal（非 diverse-short-circuit）育種経路の子にのみ作用。
- **オフラインテスト（実 `cull-pool-overflow`, hermetic stub 駆動）: 4/4 PASS。** 新 2 テストが「単一銘柄優勢プールで **sort-key 単独は過密 USDJPY のみ残し疎な EURUSD を全滅**（B-5 gap 再現）／**niche クォータ ON は過密 USDJPY を fair-share(4) に上限し疎な EURUSD-TREND 3体を全保護**」を、同一 fixture・同一 sort で `*overflow-niche-min-quota*` のみ切替えて実証。
- **A比較（6世代・copy DB・実パイプライン・flag ON・B-3 込み）:** GEN0 99.6% → **GEN6 75.6% USDJPY**（active 82, USDJPY 62, diverse-robust 17）。**3者比較: A=72.9% ／ B-5=75.6% ／ B-3=75.6%。単一栽培は沈まなかった（B-5 と同値、A より弱いまま）。**
- **根本原因（実測で確定）:** 全世代 **bred=8 diverse=8**（＝育種児は毎回 100% diverse）。USDJPY-TREND 単一栽培は **一度も育種されない**：(a) 2f CPCV pre-gate（`breeding-cpcv-eligible-p`, cpcv≥0.55）が CPCV-poor な monoculture を除外、(b) B-4 クローン検知（`Pair too similar Dist 0.00 < 0.02`）が USDJPY-TREND 同士のペアを阻止。→ 銘柄変異は TREND 親に発火せず、**非USDJPY-TREND 児はゼロ** → copy DB の TREND プールは **62/62 = 100% USDJPY**（cross-tab で確認）→ niche クォータは単一 niche = no-op。
- **niche クォータが実際に効いた唯一の場所:** 銘柄混在の **REVERSION プール**（唯一異質性がある）。B-5 の EURUSD13/GBPUSD7 → B-3 の **EURUSD11/GBPUSD9** に再均衡（密な EURUSD sub-niche を上限し疎な GBPUSD を保護＝設計どおり）。ただし双方 diverse なので USDJPY% は不動。副作用で cpcv≥0.6 の diverse-robust が 20→17 に振れた。
- **デプロイ判断への影響: 変わらず。** 両 part は可逆（OFF byte-identical・honest floor 不変）で、**プールに銘柄異質性が生じれば設計どおり噛む**（REVERSION で実証）が、**今日の CPCV-starved 単一栽培では B-5 を超える希釈をしない。** 単独での本番 flag ON は「単一栽培希釈」目的では B-5 同様に正当化されない。flag は OFF のまま、flip はオーナー判断（不変）。

---

## 1. 変更箇所

| file:line | 変更 |
|---|---|
| `src/lisp/school/school-breeder.lisp` (cull-pool-overflow 直前) | **§B-3(3) part-1**: `*overflow-niche-min-quota*`(=3) ＋ `%overflow-niche-key`（(symbol . regime-class)）＋ `select-overflow-survivors`（flag OFF=legacy top-N slice、byte-identical／flag ON=niche-quota water-filling）。 |
| `src/lisp/school/school-breeder.lisp` (`cull-pool-overflow` 本体) | 生存者選抜を `(subseq …)` から `select-overflow-survivors` 呼び出しに差し替え（protected は従来どおり常に保持）。 |
| `src/lisp/school/school-breeder.lisp` (make-diverse-child の下) | **§B-3(2)**: `*breeder-symbol-mutation-rate*`(=0.10) ／ `*breeder-symbol-mutation-pool*`(EURUSD 2:1 GBPUSD) ／ `maybe-mutate-child-symbol`。 |
| `src/lisp/school/school-breeder.lisp:~1208` (`breed-strategies`) | `sym` 束縛を `(maybe-mutate-child-symbol (or (strategy-symbol parent1) "USDJPY"))` に。diverse short-circuit は sym を無視するので normal 経路のみに作用。 |
| `src/lisp/tests/evolution-tests.lisp` | 新 2 テスト（niche クォータ ON/OFF）＋ヘルパ `%b3-run-niche-overflow-scenario`。既存 B-5 ヘルパに `*overflow-niche-min-quota*` を高値固定する束縛を追加し、B-5 テストを **part-2 単独**の検証に隔離（元のアサーション維持）。 |
| `src/lisp/tests.lisp` | run-all-tests 登録に新 2 テスト追加。 |

`selection-fitness` / `fitness-sharing-denominator`（2c, `school-genome.lisp:385-409`）と `%b4-symbol-key`（B-4, `school-genome.lisp:57`）は既存を流用。**flag OFF は全経路 byte-identical**（`select-overflow-survivors` OFF 分岐＝旧 subseq/nthcdr、`maybe-mutate-child-symbol` OFF＝sym 素通し）。

## 2. オフラインテスト結果（4/4 PASS, hermetic）

WSL Ubuntu-22.04 userspace SBCL 2.1.11、実 `cull-pool-overflow` を hermetic（`init-db`/`execute-non-query`/`notify-death` stub → swimmy.db 未接触・library redirect）で駆動：

```
-- test-b5-overflow-cull-flag-off-keeps-crowded-sheds-sparse  ... PASS  (part-2 OFF: raw sort, EURUSD 全滅 = bug baseline)
-- test-b5-overflow-cull-flag-on-sheds-crowded-keeps-sparse   ... PASS  (part-2 ON:  selection-fitness sort, EURUSD 2体保護, USDJPY 4体に間引き)
-- test-b3-niche-quota-disabled-sort-alone-keeps-only-crowded ... PASS  (part-1 OFF: 単一銘柄優勢プールで sort 単独は USDJPY8/EURUSD0 = B-5 gap 再現)
-- test-b3-niche-quota-sheds-crowded-usdjpy-first-protects-sparse ... PASS  (part-1 ON: USDJPY を fair-share 4 に上限, EURUSD-TREND 3体全保護, 過密 USDJPY 8体淘汰)
==== RESULT: 4 passed, 0 failed ====
```

**隔離の妙（part-1 を part-2 から分離）:** 両者とも `*enable-primitive-diversity*` 配下なので、flag は両 run で ON に固定し **`*overflow-niche-min-quota*` のみ切替え**。高値(999) → cap が slot 数を超え no-op ＝純 part-2 sort。実値(3) → cap 発火。**同一 fixture・同一 sort で quota だけ反転 ⇒ 反転は fixture でも sort でもなく niche クォータが原因**、を実証。fixture は単一銘柄優勢（USDJPY sharpe1.5/denom12 = 0.125 > EURUSD sharpe0.3/denom3 = 0.100）で **過密側の per-member selection-fitness を高く**してあるので、sort 単独は USDJPY のみ残す（＝ part-2 が最も苦手とする、B-5 実測 gap の縮図）。

## 3. A比較（6世代・copy DB・実パイプライン）— 数字と根本原因

隔離ハーネス [`logs/tribe_2c_wsl/daemon_verify.lisp`](../../logs/tribe_2c_wsl/daemon_verify.lisp)（**B-5 と同一・未改変**。copy DB `/mnt/c/tmp/swimmy_daemon_verify.db`・library WRITE を `/mnt/c/tmp/verify_library/` に redirect＋起動時アサート・scoring = `primitive_scan.exe`= guardian engine）を **school-breeder に B-3 を積んだ状態で再走**（flag ON, 2f pregate 0.55, 20 diverse seed）。B-3 の変更は `school-breeder.lisp`（quickload 済）にあるので、同一ハーネスが自動的に §B-3 を行使する。

| Gen | active | USDJPY% | TREND% | diverse-robust | symbols(active) |
|---:|---:|---:|---:|---:|---|
| 0 | 488 | 99.6 | 100.0 | 0 | USDJPY486 |
| 0b(seed) | 508 | 95.7 | 96.1 | 20 | USDJPY486 EURUSD15 GBPUSD7 |
| 1 | 79 | 78.5 | 78.5 | 16 | USDJPY62 … |
| 2 | 80 | 77.5 | 77.5 | 16 | |
| 3–6 | 82 | **75.6** | 75.6 | 17 | USDJPY62 EURUSD11 GBPUSD9（flat）|

**copy DB cross-tab（active, GEN6）:** `USDJPY×TREND 62 / EURUSD×REVERSION 11 / GBPUSD×REVERSION 9`。**TREND プール = 62、全 USDJPY（非USDJPY-TREND = 0）。**

### 3者比較（GEN6, apples-to-apples = Run B: daily fitness-sharing cull あり）

| 版 | 追加機構 | USDJPY% | active | diverse-robust | TREND プール銘柄 |
|---|---|---:|---:|---:|---|
| **A** | raw per-cycle overflow ＋ daily cull | **72.9** | 85 | 23 | USDJPY only |
| **B-5** | ＋ overflow sort = selection-fitness (part-2) | **75.6** | 82 | 20 | USDJPY only |
| **B-3(今回)** | ＋ niche クォータ (part-1) ＋ 銘柄変異 (§B-3(2)) | **75.6** | 82 | 17 | USDJPY only |

**→ 単一栽培は沈まなかった。GEN6 USDJPY% は B-5 と同値 75.6%、A(72.9%)より弱いまま。**

### 根本原因（実測で確定）

1. **全世代 bred=8 diverse=8**（育種児は毎回 100% diverse）。＝ **USDJPY-TREND 単一栽培は一度も育種されない。**
   - (a) 2f CPCV pre-gate（`breeding-cpcv-eligible-p`, cpcv≥0.55）が CPCV-poor な monoculture を breeding warriors から除外。
   - (b) B-4 クローン検知（`strategies-correlation-ok-p`, dist<0.02）が USDJPY-TREND 同士のペアを阻止（ログに `Pair too similar Dist 0.00 < 0.02` 多発）。
   - 育種は全て **REVERSION の 20 diverse seed** 由来（EURUSD/GBPUSD、cpcv 0.7）。
2. **銘柄変異オペレータ（§B-3(2)）は normal 育種経路に載る。** その経路が TREND で発火するには USDJPY-TREND 親が育種される必要があるが、上記 (a)(b) で TREND 育種はゼロ。REVERSION seed 親は既に非USDJPY なので `maybe-mutate-child-symbol` は素通し（設計どおり USDJPY からのみ変異）。→ **銘柄変異は一度も実効せず、非USDJPY-TREND 児はゼロ。**
3. → TREND プールは 62/62 = 100% USDJPY のまま。**niche クォータ（§B-3(3) part-1）が噛む相手＝プール内の銘柄異質性が生成されない** → 単一 niche ＝ cap == slot ＝ no-op（オフラインテストの退化ケースそのもの）。USDJPY=62 は A/B-5/B-3 で不変（希釈は §4 honest floor＋daily cull が担い、GEN0→1 の 486→62 がそれ）。
4. **niche クォータが唯一効いたのは REVERSION プール**（銘柄混在）。B-5 の EURUSD13/GBPUSD7 → B-3 の EURUSD11/GBPUSD9 に再均衡（密な EURUSD を上限、疎な GBPUSD を保護＝設計どおり）。双方 diverse ゆえ USDJPY% は不動。diverse-robust 20→17 の振れはこの再均衡で cpcv≥0.6 の内訳が入替わった副作用。

## 4. 正直な限界 / 残课題 / デプロイ判断への影響

- **§B-3(2)+§B-3(3) は実装として正しく、機序はオフラインで実証したが、今日の単一栽培には効かない。** 効かない理由は実装バグではなく **パイプラインの構造的緊張**：脱出路（銘柄変異）が育種経路に載る一方、その育種経路は §B-6 の CPCV pre-gate（over-fit 伝播を止めるための正しいゲート）と B-4 クローン検知で単一栽培に対して閉じている。**§B-3(2) と §B-6 が単一栽培上で衝突する**（前者は monoculture が育種することを要求、後者は CPCV-poor monoculture の育種を止める）。
- **単一栽培に効かせるのに必要なのは「TREND regime に、育種-CPCV ゲートを経由しない経路で銘柄異質性を入れる」こと。** 候補（いずれも seed/gate ポリシー = オーナー判断・本タスク §B-3 コード範囲外）:
  1. **symbol-mutated 非USDJPY-TREND clone を直接 seed** する（現 2f seed が REVERSION に対してやっているのと同型を TREND にも）。これで TREND プールに 2 つ目の niche が生じ、niche クォータが即座に噛む（オフラインテストの状況が実 daemon で実現）。
  2. 銘柄変異を **既存 monoculture メンバー / clone-injection 経路**（育種-CPCV 非依存）に適用する。
  3. 銘柄変異の脱出路に限り **CPCV pre-gate を緩める**（over-fit リスクと引き換え）。
- **可逆・安全性:** flag OFF byte-identical、honest_gate/§4 floor 不変、overflow は keep-order/quota であって合否ゲートではない。niche クォータは **銘柄混在プールを正しく均衡させる**ので ship 自体は安全だが、**単独での本番 flag ON を「単一栽培希釈」で正当化してはならない**（B-5 と同じ結論クラス）。
- **希釈の主因は依然 §4 honest floor**（GEN0→1 の 486→62）で、2c/2d/B-5/B-3 の per-cycle 機構ではない、という 2a/B-5 の結論は不変。flag flip はオーナー判断（未実施）。

## 5. 隔離厳守（実測確認）

- ライブ swimmy.db mtime **不変**（`2026-07-18 16:48:07.997119100`、run 前後で一致）。copy DB は live main file の plain コピー（source を db として開かない・sidecar 除去）。
- repo `data/library` 汚染 **0**（library WRITE 549 件は全て `/mnt/c/tmp/verify_library/` に redirect＋起動時アサート）。
- **検証は redirect 済 `daemon_verify.lisp`／hermetic stub テストのみ。repo ルートで `run-all-tests` フル suite は回していない**（B-5 で起きた DB→library reconcile 汚染を回避）。`daemon_verify.lisp` は未改変。
- 変更ソースは `school-breeder.lisp` / `evolution-tests.lisp` / `tests.lisp` の 3 ファイルのみ。無関係な dirty（LEGEND/guardian 等）は未接触。

関連: [[tribe-2d-regen-b5-overflow]] [[tribe-2a-daemon-isolation]] [[tribe-2d-behavioral-distance]] [[tribe-diversity-engine-rebuild]] [[kill-criteria-s4-verdict]]
