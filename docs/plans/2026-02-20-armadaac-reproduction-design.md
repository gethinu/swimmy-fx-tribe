# ArmadaAC-Centric Reproduction Model (2026-02-20)

## Goal
ArmadaAC の公開 Myfxbook 実績から、再現可能な範囲で「高収益だが破綻しにくい運用特性」を抽出し、Swimmy の採用ゲートと運用ルールへ落とし込む。

## Data Scope (Public Only)
- Source date: 2026-02-20
- Source: ArmadaAC メンバーページおよび各システムの公開 `Advanced Statistics`
- 制約: エントリー/イグジットのロジック本体は非公開のため、ここで定義するのは「挙動再現モデル（behavioral replica）」

## ArmadaAC Universe Snapshot
ArmadaAC の公開一覧には高成績口座と破綻口座が混在する。
- 例: `taiki +107.71% / DD 10.93%`, `パンダ次郎 +69.87% / DD 6.06%`
- 例: `はるちゃん -99.87% / DD 99.90%`（除外対象）

## Core Benchmark Set (5 systems)
破綻回避と再現性を重視し、以下をコア参照セットとする。

1. `taiki`: Gain `+107.71%`, DD `10.93%`, Trades `272`, PF `1.99`, AvgLen `6h32m`
2. `パンダ次郎`: Gain `+69.87%`, DD `6.06%`, Trades `871`, PF `1.79`, AvgLen `8h28m`
3. `ゆみみん`: Gain `+62.81%`, DD `7.34%`, Trades `898`, PF `1.62`, AvgLen `6h53m`
4. `波のゆくさきアルマダ`: Gain `+81.55%`, DD `10.95%`, Trades `536`, PF `1.79`, AvgLen `6h13m`
5. `こじりん`: Gain `+62.79%`, DD `7.02%`, Trades `336`, PF `1.93`, AvgLen `4h26m`

### Derived Core Ranges
- Drawdown: `6.06% - 10.95%`
- Profit Factor: `1.62 - 1.99`
- Trades: `272 - 898`
- Avg trade length: `4h26m - 8h28m`
- 特徴: 低頻度一撃型ではなく「中頻度〜高頻度の積み上げ」

## Satellite (Aggressive) Set
収益は強いが DD と分散の悪化が見える群。
- `ヒロ`: Gain `+65.38%`, DD `17.56%`, Trades `172`, PF `1.71`, AvgLen `12h30m`
- `極悪非道リーマン卍會`: Gain `+83.81%`, DD `19.00%`, Trades `946`, PF `1.74`, AvgLen `8h20m`

運用方針: 本番モデルの中核には入れず、リスク上限を別枠管理する。

## Reproduction Model v1 (Behavioral)
### 1) Candidate Admission Gate (Armada Core Profile)
候補戦略は以下をすべて満たすこと。
- `PF >= 1.60`
- `MaxDD <= 12.0%`
- `Trades >= 250`（同一評価窓）
- `Avg trade length` が `3h - 10h`
- `Long/Short` のどちらか片側偏重を回避（片側 80% 超を除外）

### 2) Promotion Gate (Live Deploy)
- OOS で `PF >= 1.30`
- 30日ローリングで `PF >= 1.10`
- 30日ローリングで `MaxDD <= 6.0%`
- 期待値が負転 (`expectancy <= 0`) なら即停止

### 3) Risk Budget
- 1トレードリスク: `0.25% - 0.40%`
- 同時保有上限: `3`
- 口座 DD ハード上限: `12%`
- ナンピン/マーチン禁止

### 4) Kill Switch
以下のいずれかで `AUTO_PAUSE`。
- 連続損失により週次 DD が `4%` 超
- 30日 PF `< 1.0`
- 60日トレード数が想定の 50% 未満（戦略崩壊シグナル）

## Why This Is Not a Blind Copy
- ArmadaAC 群は同一母集団でも破綻例が存在。
- よって「Armada っぽい全戦略」ではなく「Armada 上位の共通挙動だけ」を抽出して再現する。
- 再現対象はロジックそのものではなく、`PF/DD/回転率/保有時間` の運用プロファイル。

## Swimmy Implementation Targets (Minimal)
1. Rank gating: `src/lisp/school/school-rank-system.lisp`
2. Live execution guardrail: `src/lisp/school/school-execution.lisp`
3. Risk stop / pause hooks: `src/lisp/school/school-danger.lisp`
4. Narrative/alert thresholds: `src/lisp/school/school-narrative.lisp`

## Sources
- https://www.myfxbook.com/members/ArmadaAC
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-taiki/11671303
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%83%91%E3%83%B3%E3%83%80%E6%AC%A1%E9%83%8E/11664080
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%82%86%E3%81%BF%E3%81%BF%E3%82%93/11664297
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E6%B3%A2%E3%81%AE%E3%82%86%E3%81%8F%E3%81%95%E3%81%8D%E3%82%A2%E3%83%AB%E3%83%9E%E3%83%80/11670061
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93/11688014
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-sho-%E8%87%AA%E5%8B%95%E5%A3%B2%E8%B2%B7%E5%A4%A7%E5%A5%BD%E3%81%8D%E4%BA%BA%E9%96%93/11657290
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%83%92%E3%83%AD-fx-youtuber/11657258
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E6%A5%B5%E6%82%AA%E9%9D%9E%E9%81%93%E3%83%AA%E3%83%BC%E3%83%9E%E3%83%B3%E5%8D%8D%E6%9C%83/11657269
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-ug/11664061
