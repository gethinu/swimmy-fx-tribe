# ArmadaAC Strategy Assessment (2026-02-20)

## Scope
- 対象: ArmadaAC 公開口座（Myfxbook）
- 目的: 「戦略として有用か」「実力はどれほどか」を運用指標で定量評価
- 前提: ロジック本体（エントリー/イグジット条件）は非公開のため、評価対象は **運用プロファイル**

## Dataset
### Core set (再現対象)
1. taiki: Gain +107.71%, DD 10.93%, PF 1.99, Trades 272, AvgLen 6h32m
2. パンダ次郎: Gain +69.87%, DD 6.06%, PF 1.79, Trades 871, AvgLen 8h28m
3. ゆみみん: Gain +62.81%, DD 7.34%, PF 1.62, Trades 898, AvgLen 6h53m
4. 波のゆくさきアルマダ: Gain +81.55%, DD 10.95%, PF 1.79, Trades 536, AvgLen 6h13m
5. こじりん: Gain +62.79%, DD 7.02%, PF 1.93, Trades 336, AvgLen 4h26m

### Aggressive set (別枠管理)
1. ヒロ: Gain +65.38%, DD 17.56%, PF 1.71, Trades 172, AvgLen 12h30m
2. 極悪非道リーマン卍會: Gain +83.81%, DD 19.00%, PF 1.74, Trades 946, AvgLen 8h20m

## Quant Summary
### Core
- PF median: **1.79**
- DD median: **7.34%**
- Trades average: **582.6**
- Gain/DD efficiency average: **9.27**
- Avg hold-time band: **4h26m - 8h28m**

### Aggressive
- PF median: **1.73**
- DD median: **18.28%**
- Trades average: **559.0**
- Gain/DD efficiency average: **4.06**

## Usefulness Verdict
**Verdict: YES (Core only).**

理由:
- Core群は `PF >= 1.62` を維持しつつ `DD <= 10.95%` に収まる。
- 取引回数も十分（272〜898）で、単発ブレではなく積み上げ型。
- 平均保有時間が 3h〜10h 近辺に集中し、再現モデル化しやすい。

**Not recommended as-is:**
- Armada全体には破綻系（例: -99.87%）が混在するため、無選別コピーは不可。

## Strength Assessment
### Edge Strength (実力)
- 絶対収益力: **High**
- リスク効率: **Medium-High（Core） / Medium-Low（Aggressive）**
- 安定再現性: **Medium-High（Core）**
- 総合実力: **Core = 74/100, Aggressive = 56/100**

### Interpretation
- Coreは「派手な超高PF」ではなく、PF 1.6〜2.0帯の実務的エッジを高頻度運用で積むタイプ。
- Aggressiveはリターンは高いが、DD増加で効率が悪化。
- よって本番採用は Core を標準、Aggressiveは別枠・小サイズ限定が妥当。

## Swimmy Adoption Decision
- **Adopt:** Armada Core profile (`PF>=1.60`, `DD<=12%`, `Trades>=250`, hold 3h-10h, side-share<=80%)
- **Reject:** Armada全量コピー
- **Guardrail:** Hard DD 12%, weekly DD 4%, rolling PF 30d >= 1.0, trade-activity ratio 60d >= 0.5

## Limits
- Myfxbook公開データ由来のため、生存者バイアスと非公開ロジックの不確実性あり。
- 「戦略式の再現」ではなく「運用特性の再現」が対象。

## Sources
- https://www.myfxbook.com/members/ArmadaAC
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-taiki/11671303
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%83%91%E3%83%B3%E3%83%80%E6%AC%A1%E9%83%8E/11664080
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%82%86%E3%81%BF%E3%81%BF%E3%82%93/11664297
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E6%B3%A2%E3%81%AE%E3%82%86%E3%81%8F%E3%81%95%E3%81%8D%E3%82%A2%E3%83%AB%E3%83%9E%E3%83%80/11670061
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93/11688014
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E3%83%92%E3%83%AD-fx-youtuber/11657258
- https://www.myfxbook.com/members/ArmadaAC/aac-%E3%81%93%E3%81%98%E3%82%8A%E3%82%93%E6%9D%AF-%E6%A5%B5%E6%82%AA%E9%9D%9E%E9%81%93%E3%83%AA%E3%83%BC%E3%83%9E%E3%83%B3%E5%8D%8D%E6%9C%83/11657269
