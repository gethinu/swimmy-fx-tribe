# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-11  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「Aまで復活したが、S条件の PF/WR が厳しくて止まってる。1/2/3のどれで進めるか」
**Purpose:** S昇格とライブ移行の最短ルートを、基準の信頼性を壊さずに決める。  
**Constraints:**  
- `:S` は「LIVE TRADING PERMITTED」の意味を維持する（`src/lisp/school/school-rank-system.lisp:478`）。  
- 現行S基準（PF/WR）を安易に下げて、運用リスクを拡大しない。  
- 既存のA復旧導線を壊さない。  
**Success Criteria:**  
- `A` 母数が安定的に増え、`CPCV` dispatch が継続する。  
- `S` は「閾値緩和なし」で出るか、少なくとも `PF/WR` 改善トレンドが見える。  
- ライブ移行判断に必要な説明可能性（どのゲートで落ちたか）が担保される。  
**Assumptions:**  
- 2026-02-11 20:46 JST 時点のDBスナップショットは `A=1 / S=0`。  
- 当該A戦略は `Sharpe=1.81, PF=1.31, WR=0.3866, CPCV pass_rate=0.7556`（Sの `PF>=1.70`, `WR>=0.50` を未達）。  
**Uncertainties:**  
- レポート系の値にドリフトがあり、時点によって `A` 件数が揺れる。  
- `journalctl` 上では `Blocked S promotion ... (CPCV criteria missing)` と出るが、実際はPF/WR未達も混在しうる（ログ文言の粒度不足）。

## 🏛️ 常設顧問の意見
### Taleb: 「1は“即効薬”ではなく“意味破壊”」
- 事実: S基準は `PF>=1.70`, `WR>=0.50` をハードに要求している（`src/lisp/school/school-rank-system.lisp:26`, `src/lisp/school/school-rank-system.lisp:107`, `src/lisp/school/school-rank-system.lisp:108`）。
- いま通っているA個体は `PF/WR` がS水準に遠い。ここを段階制で下げると、`:S=ライブ許可` の意味が崩れる。
- 選択肢:
  - 1) S閾値段階制: **短期効果は最大**、ただし「Sの定義破壊」で長期の破滅リスクが高い。
  - 2) A母数拡大: 即効性は中、閾値の意味を保てる。
  - 3) 変異バイアス: 即効性は低いが、唯一「S条件を守ったままSを増やす」手段。

### Graham: 「最短で壊さないなら2→3の順」
- 事実: A候補抽出は既にA基礎条件とExpectancyを通している（`src/lisp/school/school-rank-system.lisp:384`, `src/lisp/school/school-rank-system.lisp:390`）。
- つまり導線は再建済み。ここで1を打つより、2で流量を増やし、3で質を上げる方が開発負債が少ない。
- 選択肢:
  - 2先行: パイプラインはそのまま、候補密度だけ上げる。
  - 3先行: 根本解だが時間がかかる。
  - 1先行: 最短だが将来の修復コストが最大。

### Naval: 「自動化対象を“PF/WR改善”へ寄せろ」
- 事実: CPCV候補抽出はSharpeエリート中心（`src/lisp/school/school-validation.lisp:261`, `src/lisp/school/school-validation.lisp:740`）。
- Sharpeだけ強くてもS基準PF/WRで落ちるなら、探索圧力の方向が違う。
- 選択肢:
  - 3) 変異バイアス導入（PF/WR改善報酬を追加）: 本筋。
  - 2) A母数拡大: バイアス導入までの暫定有効策。
  - 1) 閾値緩和: 自動化効率は上がるが、品質ラベルが壊れる。

### Jim Simons: 「ラベルを動かすな、分布を動かせ」
- 事実: 現行S判定はIS+Stage2（CPCV pass_rate/maxdd）をANDで見る設計（`src/lisp/school/school-rank-system.lisp:105`, `src/lisp/school/school-rank-system.lisp:112`）。
- いま必要なのは「S閾値を下げること」ではなく「PF/WR分布を右に寄せること」。
- 選択肢:
  - 3) 変異・選抜のスコアリングにPF/WR寄与を増やす。
  - 2) 試行数を増やして上位分位を取りに行く。
  - 1) 基準緩和は、統計的には“成功率の見かけ改善”でしかない。

## 💻 技術パネルの意見
### Fowler: 「まず観測可能性。今のログは原因を誤誘導する」
- 事実: S昇格ブロック時のログ文言が固定で `CPCV criteria missing`（`src/lisp/school/school-rank-system.lisp:178`）、実際の失敗要因を区別していない。
- ここが曖昧だと、1/2/3どれを打つべきかを毎回誤る。
- 選択肢:
  - 2/3に着手する前に、S失敗理由を `pf/wr/maxdd/cpcv/common-stage2` で分解出力する。
  - そのうえで 2→3 を実行。

### Hickey: 「Sという語に別の意味を乗せるな」
- 事実: Sはコード上も文言上もライブ許可ランク（`src/lisp/school/school-rank-system.lisp:10`, `src/lisp/school/school-rank-system.lisp:478`）。
- 1をやるなら、`S` 本体ではなく `S-candidate` など別ラベルで段階運用すべき。
- 選択肢:
  - 1’）S段階制をやるなら新ランク導入（Sは触らない）。
  - 2/3）既存意味を保ったまま改善。

### Uncle Bob: 「意思決定をテストで固定しろ」
- 事実: いまは運用ログ依存で判断しており、閾値/理由の回帰を防ぐテストが不足しやすい。
- 選択肢:
  - 1を採るなら、`S` と `S-candidate` の意味分離テストが必須。
  - 2/3を採るなら、`PF/WR未達でS不可` を固定するテストを先に置く。

## 🚀 ビジョナリーの意見
### Ng: 「モデルの目的関数を業務KPIに一致させる」
- 事実: 直近ボトルネックは `PF/WR`。探索がSharpe偏重のままではS化しない。
- 選択肢:
  - 3) PF/WRを直接最適化対象へ。
  - 2) データ母数拡大は補助輪。

### López de Prado: 「閾値変更は過適合の近道」
- 1は“現サンプルに合わせて定義を曲げる”行為で、将来のライブで破綻しやすい。
- 2/3は閾値固定のまま分布改善を狙うため、より頑健。

### Gene Kim: 「運用は2段で回せ」
- 2026-02-11のログ上、Aが `0→2→1` と揺れている。まずは観測を固め、次に改善を入れるべき。
- 選択肢:
  - 2) パイプライン流量の安定化（A母数の下支え）
  - 3) 上流品質改善（PF/WRバイアス）
  - 1) 本番意味の変更は最後の最後

## 反証 / Counterarguments
- 反証: 「とにかくライブを急ぐなら1しかない」  
  回答: その通り、速度だけなら1が最速。ただし `:S` の信頼ラベルが崩れ、以後の運用判断（資金配分・停止判断）が曖昧になる。  
- 反証: 「2をやってもSは増えないかも」  
  回答: 正しい。だから2単独ではなく、**2を回しながら3を実装**する前提が必要。

## 🚀 Musk's Decision (Final)
> 「結論は **2 → 3**。  
> 1はやらない。少なくとも `:S` 本体には触るな。  
> A母数を先に増やして検証流量を確保し、同時にPF/WR改善バイアスを入れる。  
> どうしても即時の“昇格表示”が必要なら、`S-candidate` を新設して `:S` の意味は守れ。」

## Actionable Items
1. **採択方針を固定**: 今回は「2を先行、3を並行、1は不採用（S本体）」を明文化する。  
2. **S失敗理由の分解ログを追加**: `src/lisp/school/school-rank-system.lisp:175` 近辺で、`pf/wr/maxdd/cpcv/common-stage2` を明示する。  
3. **A流量の維持**: `run-b-rank-culling-for-category` のA-ready選抜を維持し、カテゴリごとのA候補数を定点観測する（`src/lisp/school/school-rank-system.lisp:384`）。  
4. **PF/WRバイアス実装（本筋）**: 変異・選抜スコアでPF/WR寄与を強め、Sharpe単独優位を下げる。  
5. **もし1を実行するなら条件付き**: `:S` は据え置き、別ランク（例: `:S-candidate`）を導入して紙運用のみ許可する。  

## やらないこと
- `:S` の `PF/WR` を直接緩和して「ライブ許可」の意味を薄めること。  
- `Blocked S promotion ... CPCV criteria missing` の文言だけを根拠に、原因分析なしで閾値調整すること。  
