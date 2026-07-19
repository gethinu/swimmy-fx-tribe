> ⚠️ **後続訂正 (2026-07-19) — 絶対% と diverse-robust 数は cosmetic seed のアーティファクト。**
> 本 run の 20 diverse seed は `cpcv=0.7` pre-set・`:require-bt nil`（honest gate 未通過）だった。よって
> **「GEN6 75.6% / diverse-robust 20」「A=72.9% / diverse 23」は水増し**。honest 採点では希釈 floor ≈ 84–85% /
> diverse-robust 2–3（seed-escape `fed9bf7b`）。**§0/§3 の機序的結論（sort-key 単独では単一栽培を希釈できない＝
> regime-class スコープで除算がキャンセル、§B-3(3) part-1 が別途必要）は不変・有効。** 集約:
> [`tribe_2d_diversity_investigation_summary_20260719.md`](tribe_2d_diversity_investigation_summary_20260719.md) §1・③。

# 🧪 2d regen §B-5 — fitness-sharing wired into per-cycle overflow cull (implemented + measured)

**日付:** 2026-07-18 JST ／ **ブランチ:** `claude/2d-behavioral-distance`（base `c96550ea` = 2a verify）／ **commit:** `519c3047`
**モード:** オフライン・**ライブ発注ゼロ・ライブ swimmy.db 未接触・本番フラグ既定 OFF のまま**。honest_gate/§4 floor 不変。

> **これは何か:** [`tribe_2a_daemon_isolation_20260718.md`](tribe_2a_daemon_isolation_20260718.md) §3.2 が「未実装」と明記した regen §B-3(3)/§B-5「溢れ淘汰の sort を selection_fitness に差し替え」を実装し、隔離ハーネス（copy DB）で希釈への効果を実測した記録。**結論を先に：sort-key の差し替え単独では単一栽培の希釈は強まらない（むしろ僅かに弱まる）。理由は cull-pool-overflow が regime-class 単位スコープで、単一銘柄の TREND プール内では fitness-sharing の分母が一様になり除算がキャンセルするため。§B-3(3) の「プール上限を (symbol×regime) niche 単位に」（part-1）が別途必要。**

---

## 0. TL;DR

- **実装（flag-gated, OFF=byte-identical）:** `cull-pool-overflow`（`school-breeder.lisp:1499-1521`）の overflow sort を、`*enable-primitive-diversity*` ON で raw `score-from-metrics` から **`selection-fitness`（evidence-adjusted-sharpe ÷ fitness-sharing niche 分母）** に差し替え。OFF は元の lambda と文字一致。honest_gate floor は不変（overflow は「どれを残すか」の順序付けであって合否ゲートではない）。
- **オフラインテスト（実 cull-pool-overflow, DB/notify stub, hermetic）:** 同一メトリクスの 8 crowded USDJPY-TREND(sharpe1.5)＋2 sparse EURUSD-TREND(sharpe0.9) を limit 6 で overflow。**OFF は EURUSD 2 体とも淘汰（monoculture 温存バグ）／ON は EURUSD 2 体とも生存・crowded USDJPY を 4 体淘汰**。→ 混在プールでは機構が設計どおり働くことを実証（PASS）。
- **再検証（6世代・copy DB・flag ON・B-5 配線あり）:** GEN0 **99.6%** → GEN6 **75.6%** USDJPY（active 82, diverse 20 = EURUSD13+GBPUSD7）。**A 検証 Run B（B-5 前）の 72.9%（active 85, diverse 23）より弱い希釈。**
- **根本原因（重要）:** `cull-pool-overflow(cat)` は **regime-class 単位**。単一銘柄の TREND プール（62 全て USDJPY-TREND）内では (symbol,category) niche 分母が **一様（=62）** → 除算がキャンセル → B-5 は raw ≒ adjusted-sharpe 順に退化し、**USDJPY 単一栽培を優先的に削れない**（USDJPY=62 は両 run で同一。486→62 の希釈は §4 honest floor＋daily fitness-sharing cull が担い、B-5 は無関係）。B-5 が差を生んだのは **混在銘柄の REVERSION プール（EURUSD15 vs GBPUSD7）だけ**で、より密な EURUSD sub-niche を 2 体削り → diverse が減り → USDJPY% が僅かに上昇。
- **結論:** sort-key 差し替え（§B-5/§B-3(3) part-2）は**必要だが単独では不十分**。単一栽培に効かせるには **cull-pool-overflow を (symbol×regime) niche 単位スコープに再構成（§B-3(3) part-1）** が別途必要。本タスクの依頼範囲は part-2 のみ。

---

## 1. 変更箇所

| file:line | 変更 |
|---|---|
| `src/lisp/school/school-breeder.lisp:1499-1521` | overflow sort `:key` を flag-gated 化。ON=`(selection-fitness s *strategy-knowledge-base*)`、OFF=元の `score-from-metrics` lambda（文字一致）。KEEP-ORDER のみ・honest floor 不変を明記するコメント。 |
| `src/lisp/tests/evolution-tests.lisp` | ヘルパ `%b5-make-overflow-strat` / `%b5-run-overflow-scenario` ＋ 2 テスト（OFF=bug 再現／ON=fix）。 |
| `src/lisp/tests.lisp` | run-all-tests 登録に 2 テスト追加。 |

`selection-fitness` / `fitness-sharing-denominator` は 2c 既存（`school-genome.lisp:385-409`）を**そのまま**利用（niche キー=既存の (symbol,category)）。B-4 距離（`school-genome.lisp:126-169`）は breeding compatibility / tournament 経路で作用し、overflow は selection-fitness 経由で fitness-sharing の混雑ペナルティを見る配線。

## 2. オフラインテスト結果

WSL Ubuntu-22.04 userspace SBCL 2.1.11 で system compile clean。実 `cull-pool-overflow` を hermetic（`init-db`/`execute-non-query`/`notify-death` stub → swimmy.db 未接触）に駆動：

```
flag OFF: [DEATHMATCH] killed = UT-B5-UJ-7, UT-B5-UJ-8, UT-B5-EU-1, UT-B5-EU-2   → 疎な EURUSD 2 体とも淘汰（bug）
flag ON : [DEATHMATCH] killed = UT-B5-UJ-5, -6, -7, -8                            → crowded USDJPY のみ淘汰（fix, EURUSD 生存）
[RESULT] 6 passed, 0 failed（B-5×2 ＋ B-4×4）
```

同一メトリクスで flag が結果を反転 → 反転は fixture ではなく配線が原因、を実証。**注意:** このテストは USDJPY と EURUSD を**同一 TREND プール**に混在させている。実 daemon の単一栽培プールは単一銘柄（§3 参照）。

## 3. 再検証（A 比較）— 数字と根本原因

隔離ハーネス [`logs/tribe_2c_wsl/daemon_verify.lisp`](../../logs/tribe_2c_wsl/daemon_verify.lisp)（copy DB `/mnt/c/tmp/swimmy_daemon_verify.db`・library WRITE を `/mnt/c/tmp/verify_library/` に redirect＋起動時アサート・discord/notify neutralize・scoring は primitive_scan.exe = guardian engine）を **B-5 配線ありで再走**（flag ON, 2f pregate 0.55, 20 diverse seed）。

| Gen | active | USDJPY% | TREND% | diverse-robust | symbols |
|---:|---:|---:|---:|---:|---|
| 0 | 488 | 99.6 | 100.0 | 0 | USDJPY486 |
| 0b(seed) | 508 | 95.7 | 96.1 | 20 | USDJPY486 EURUSD15 GBPUSD7 |
| 1 | 82 | 75.6 | 75.6 | 19 | USDJPY62 EURUSD13 GBPUSD7 |
| 2–6 | 82 | **75.6** | 75.6 | 19 | USDJPY62 EURUSD13 GBPUSD7（flat）|

**A 検証 Run B（B-5 前, raw overflow）との比較:** A=GEN6 **72.9%**（active85, diverse23）／ B-5=GEN6 **75.6%**（active82, diverse20）。**B-5 は希釈を強めず、むしろ diverse 蓄積を 2–3 体減らして USDJPY% を僅かに上げた。**

**根本原因（実測で確定）:**
1. `cull-pool-overflow(cat)` は **regime-class 単位**でプールを取る（`school-breeder.lisp:1462-1469`）。
2. 単一栽培は **全て USDJPY-TREND** → `cull-pool-overflow(:trend)` のプール内で (symbol,category) niche が一様 → `fitness-sharing-denominator` が全員 62 → **selection-fitness の除算がキャンセル**し、B-5 は「adjusted-sharpe 順」に退化。raw composite と順序が違うだけで、**銘柄多様性を報酬しない**。→ USDJPY=62 は両 run 同一（overflow の 105 DEATHMATCH kill は全て低 sharpe の Bred-Bred USDJPY-TREND で、希釈は §4 floor＋daily cull が担う）。
3. B-5 が差を生んだのは **混在銘柄の REVERSION プール**（EURUSD13-15 + GBPUSD7）だけ。EURUSD 分母(~15) > GBPUSD 分母(~7) → EURUSD の selection-fitness が低く、overflow が EURUSD を 2 体先に削った（15→13）→ diverse 減 → USDJPY% 上昇。

→ **「61 clone 不能化」は overflow の sort-key だけでは達成されない。単一銘柄プールでは除算が効かないため。**

## 4. デプロイ判断への影響 / 残课題

- **B-5 sort-key 差し替えは可逆・安全（flag OFF 既定・byte-identical）だが、単一栽培の希釈という主目的には単独で無効。** むしろ混在 diverse プールでは密な sub-niche を削り diverse 蓄積を僅かに阻害しうる。→ **これ単独での本番 flag ON は希釈目的では正当化されない。**
- **必要な follow-up（regen §B-3(3) part-1・未実装）:** `cull-pool-overflow` を **(symbol×regime) niche 単位**に再構成し、niche ごとに上限を課す（1 niche が 61 slot を占有不能に）。sort-key（part-2, 本コミット）は part-1 と組で初めて単一栽培に効く。part-1 は overflow ループの構造変更＝より大きな改修。
- **代替/補完:** (i) seed を near-clone 20 体ではなく**遺伝的に真に分散**させる（現 seed は EURUSD/GBPUSD REVERSION の mini-monoculture で、それ自体が fitness-sharing の標的）。(ii) 銘柄変異オペレータ（§B-3(2)）で USDJPY-TREND プール内に非USDJPY を混ぜれば、B-5 の除算が同一プール内で効く（オフラインテストの状況が実現する）。
- honest_gate/§4 floor 不変・flag flip はオーナー判断（未実施）。

## 5. 隔離厳守（実測確認）

- ライブ swimmy.db mtime **不変**（harness は copy に rebind）。repo `data/library` 汚染 **0**（library WRITE は `/mnt/c/tmp/verify_library/` に redirect＋起動時アサート）。harness の manifest/out JSON は gitignore 済 `logs/tribe_2c_wsl/`。
- **注意（別途記録）:** 本作業中に `run-all-tests`（フル suite）を repo working dir で誤って回し、DB→library reconcile が repo `data/library` に 339 untracked を書いた。**検知して全削除・repo をベースラインに復帰**。live strategies テーブルは main-file mtime 不変＋pre/merged で 18507 一致（新規行ゼロ）を確認。教訓：**フル suite は library path を redirect しないため repo 汚染する。検証は daemon_verify.lisp 系（redirect 済）のみを使うこと。**

関連: [[tribe-2a-daemon-isolation]] [[tribe-2d-behavioral-distance]] [[tribe-diversity-engine-rebuild]] [[kill-criteria-s4-verdict]]
