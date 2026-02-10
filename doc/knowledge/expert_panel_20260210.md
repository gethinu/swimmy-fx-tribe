# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-10
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** 「Deep Learningの選択（1:CLIP推論のみ / 2:軽いFT / 3:Siamese/Triplet自前学習）」
**Purpose:** Pattern Similarity Gateの学習戦略を決める
**Constraints:** ZMQはS式のみ（バイナリ送信禁止）／Pattern Similarity ServiceはREQ/REP 5564／データ正本はData Keeper・S式
**Success Criteria:** Lookaheadなしで再現性があり、OOS/CPCVで効果が確認でき、運用負債が増えない
**Assumptions:** OHLCV/TICKはData Keeperから取得可能、Pattern Similarity Gateの仕様は現行SPECに準拠
**Uncertainties:** ティック履歴の完全性、学習データ量とバランス、モデル再学習コスト

## 🏛️ 常設顧問の意見
### Taleb: 「Option3は破滅的な尾部リスク」
- 未来ラベルを使う設計で、**自前学習はLookahead汚染の増幅器**になる。Purged CV/Embargo無しの学習は即死。`docs/llm/SPEC.md:104-108`, `docs/llm/SPEC.md:37-62`
- ティック履歴が不完全ならVAP特徴は誤情報になる。**データ欠損時の挙動**を仕様化しない限り、Option3は事故る。`docs/llm/INTERFACES.md:415-460`, `docs/llm/STATE.md:39-40`
- 選択肢（トレードオフ）:
  - **Option1**: 低リスク・低適合
  - **Option2**: 適合と安全の妥協点
  - **Option3**: 最大の適合だが最大の破滅リスク

### Graham: 「価値検証前の“巨大開発”はやめろ」
- すでにGuardian側にNN予測が存在する。新たな自前学習は**価値証明前の肥大化**。`docs/llm/SPEC.md:11`
- Option2で十分に勝てるなら、Option3は不要。**不要なら削る**が原則。

### Naval: 「運用負債は複利で効く」
- Option3はモデル学習・更新・監視が恒常運用になる。現状のsystemd/運用規約に**学習ジョブ**の位置づけがない。`docs/llm/STATE.md:21-22`
- Option1/2なら、Pattern Similarity Service単体で管理できる。

### Jim Simons: 「データ設計が未熟なら学習は嘘をつく」
- 画像化 + VAP + ティック由来という複合特徴は**データ整合性が命**。ティック欠損やスプレッド異常で教師ラベルが崩れる。`docs/llm/SPEC.md:40-62`, `docs/llm/INTERFACES.md:415-460`
- Option3をやるなら**ラベル生成とデータ検証の数学**を先に固定すべき。

## 💻 技術パネルの意見
### Fowler: 「責務境界が壊れる」
- 自前学習を入れるなら、学習・推論・データ生成の境界を**サービス設計で分離**すべき。今は推論サービスしか定義されていない。`docs/llm/INTERFACES.md:560-641`
- Option3は新たな“Training Service”相当が必要だが、SPEC/STATEに空白がある。`docs/llm/SPEC.md:8-14`, `docs/llm/STATE.md:32-40`

### Hickey: 「今のNNと二重化する」
- 既にLisp側にNNオンライン学習の痕跡がある。別系統のDL学習を入れると**同じ問題に2つの解**が生まれる。`src/lisp/core/executor.lisp:267-270`
- Option2なら既存NNと役割が分かれるが、Option3は境界不明。

### Uncle Bob: 「テスト不能な学習は機能じゃない」
- Option3は再現性がないとテスト不能。**固定データセットと固定評価のテスト**が必須。`docs/llm/SPEC.md:104-108`
- Option1/2なら、推論のみの契約テストで安全性を担保しやすい。`docs/llm/INTERFACES.md:592-641`

## 🚀 ビジョナリーの意見
### Ng: 「Option3はMLOpsが前提」
- Siamese/Tripletは**学習データ・正例/負例・評価指標**が必要。現状の仕様には学習パイプラインがない。`docs/llm/SPEC.md:37-62`
- Option2（軽いFT）でも十分な改善が見込めるなら、**段階的に進めるのが合理的**。

### López de Prado: 「過学習回避が最優先」
- 近傍学習は簡単に“未来を覚える”。Purged CV/Embargoを前提にした検証が必須。`docs/llm/SPEC.md:104-108`, `docs/llm/STATE.md:35-36`
- Option3は**最も過学習しやすい**。最短で失敗するのは3。

### Gene Kim: 「運用可能性で見ろ」
- モデル更新頻度・リソース消費・失敗時のロールバックが設計されていない。Option3は**運用事故の確率が最も高い**。`docs/llm/STATE.md:21-22`
- Option2なら “たまの再学習” で回せる。

## 🚀 Musk's Decision (Final)
> 「**結論は“2→3の段階移行”。**
>  3をやるなら、学習パイプラインと検証を“仕様に書け”。
>  それがないなら2で止める。」

## Actionable Items
1. **学習パイプラインの設計追加**（Training Service or batch jobの責務/入出力/保存先）: `docs/llm/SPEC.md`, `docs/llm/STATE.md`
2. **ラベル生成と検証の仕様化**（Purged CV/Embargo、期間分割、再現性テスト）: `docs/llm/SPEC.md:37-62`, `docs/llm/SPEC.md:104-108`
3. **学習データの品質保証**（ティック欠損/VAP生成の挙動を明文化）: `docs/llm/INTERFACES.md:415-460`, `docs/llm/STATE.md:39-40`
4. **既存NNとの責務整理**（オンライン学習とパターンDLの境界を明確化）: `src/lisp/core/executor.lisp:267-270`
5. **モデル版管理・ロールバック方針**（運用中の安全弁）: `docs/llm/STATE.md:21-22`
