# Expected Value EA Design (FX, Pattern-As-A-Tool)

**Date:** 2026-02-10  
**Scope:** USDJPY / EURUSD / GBPUSD  
**Goal:** チャートパターン検出を「手段」として使いつつ、最終判断は **コスト込み期待値(EV)がプラスの局面だけ**をトレードする。  
**Notes:** 投資助言ではなく研究・設計。過学習と情報リークの回避を最優先とする。

---

## 1. 目的と前提（通貨ペア/時間足/コストモデル）

- 対象: `USDJPY` / `EURUSD` / `GBPUSD`
- 時間足:
  - 取引の意思決定は `M5/M15/H1/H4/D1` を主対象（最初は `H1/H4/D1` 推奨）
  - マルチTFは「ベース足 + 上位足コンテキスト」を特徴量として条件化
- コストモデル（pips換算の例）:
  - `cost_pips = spread_pips + slippage_pips + commission_pips_roundtrip + swap_pips_adjust`
  - 保守的シナリオを複数用意: `cost_pips_low / mid / high`
  - 実運用制約: 約定遅延、スプレッド拡大、指標時スリッページを **フィルタ条件** と **評価シナリオ** に反映

---

## 2. ラベル設計（数式・パラメータ例）

### 選択肢A: Triple-Barrier（推奨: v1）

- 目的: 「当たった/外れた」より、**リスクリワードと期限**をラベルに織り込む
- バリア（Longの場合）:
  - `TP = entry + k1 * ATR`
  - `SL = entry - k2 * ATR`
  - `T = horizon_bars`
- ラベル:
  - `y=+1` 期限内にTP先達
  - `y=-1` 期限内にSL先達
  - `y=0` 期限到来（No-Trade/Neutral扱い）
- 例パラメータ（ロバスト性確認用のグリッド）:
  - `k1 ∈ {0.5, 1.0, 1.5}`
  - `k2 ∈ {0.5, 1.0, 1.5}`
  - `T` はTFで固定（例）: `M5=48` / `M15=16` / `H1=24` / `H4=6` / `D1=7`
- Shortは上下を反転して同様に生成

### 選択肢B: コスト込みEVラベル（推奨: v2）

- 目的: **EV>0 の局面だけ**を学習目標にする（分類精度ではなく意思決定指標へ直結）
- 単純化した定義例（Long）:
  - `gross_return = (exit_price - entry_price)`
  - `net_return = gross_return - cost_price`
  - `label_ev = 1 if E[net_return | x] > 0 else 0`
- 実用上の推奨: いきなり `E[net_return]` を教師にせず、まずは Triple-Barrier の確率推定からEVへ落とす

### 方向とNo-Tradeの扱い（実装しやすい構成）

- Multi-head案（推奨）:
  - Head1: `P(TP先達 | Long)` / `P(SL先達 | Long)` / `P(timeout | Long)`
  - Head2: `P(TP先達 | Short)` / `P(SL先達 | Short)` / `P(timeout | Short)`
  - Optional Head3: `E[MFE]`, `E[MAE]` または `E[net_return]`（回帰）
- エントリーは「Long/ShortそれぞれのEV」を比較し、最大EVが閾値を超えた時のみ

---

## 3. 特徴量（計算式・窓幅例）

- 価格系列（スケール不変）:
  - 対数リターン: `r_t = log(close_t / close_{t-1})`
  - ボラ正規化: `r_t / ATR_t` または `zscore(r_t)`（ただし fit 範囲はリーク対策必須）
- ボラ:
  - `ATR(14)`、レンジ分位、`|r|` の分位
- トレンド:
  - MA乖離: `(close - SMA(n)) / ATR`
  - 回帰傾き: `slope(returns, window)`
  - ADX等は過剰に増やさず、ベースラインで効果確認してから追加
- 時間帯:
  - セッション（Tokyo/London/NY）、曜日、月初/週初フラグ
- スプレッド:
  - 直接取得できるなら履歴を特徴量に
  - できない場合は `spread_filter` として運用に組み込む
- マルチタイムフレーム:
  - 上位足の「状態ベクトル」（例: D1のトレンド/ATRレジーム）を付与
- チャートパターンの取り込み:
  - (案1) 代表パターンを 0/1 または score で特徴量化（Breakout, Pullback, DoubleTop等）
  - (案2) 副タスク（マルチタスク学習）としてパターン分類を置き、表現学習の正則化に使う
  - 推奨: まず(案1)で検証し、改善が見える場合のみ(案2)へ

---

## 4. モデル候補（ベースライン→DL）

- ベースライン（必須）:
  - Logistic Regression（スパース特徴に強い、解釈しやすい）
  - LightGBM（非線形、欠損耐性、強いベースライン）
- DL候補（段階導入）:
  - TCN(1D-CNN): 短期パターンに強い、推論が軽い
  - Transformer Encoder: 長期依存を扱えるが、過学習・運用コストが増えやすい
- 3通貨ペアの扱い:
  - (a) ペア別モデル: 最短で実務投入、ただしデータが薄いTFでは弱い
  - (b) 共有バックボーン + pair embedding: データ効率が良いが設計が重い
- 出力設計（EV直結）:
  - `p_win`, `avg_win`, `avg_loss`（または barrier到達確率）を推定し、EVを合成する

---

## 5. 学習手順（リーク対策、CV、最適化）

- リーク対策（必須）:
  - 正規化や特徴量のfitは「学習区間のみ」
  - 未来窓を使うラベル生成は Purged CV + Embargo を必須にする
- 評価設計:
  - WFV（ウォークフォワード）を基本
  - 最終期間は完全ホールドアウト（モデル選択に触れない）
- ハイパーパラメータ探索:
  - 探索は train/val 内のみ
  - テスト期間に触れたら破棄（ルール化）

---

## 6. エントリー/エグジット（EVベースの意思決定）

- エントリー:
  - `EV_net(Long) > EV_threshold` または `EV_net(Short) > EV_threshold`
  - `spread_pips <= spread_max`（時間帯別に閾値を変える案も可）
  - 低流動性/ロールオーバー/指標前後は見送り（ルール化）
- エグジット:
  - 基本は Triple-Barrier（TP/SL/TimeOut）
  - トレーリングや部分利確は「EV改善が統計的に確認できた場合のみ」

---

## 7. リスク管理（DD、相関、ポジションサイズ）

- ポジションサイズ:
  - ボラターゲティング: `lot ∝ risk_budget / ATR`
  - 口座DD制約、日次損失上限、連敗制限、同時保有上限
- 3ペア相関:
  - 同方向の同時エントリーを抑制
  - 総リスクが一定を超えたら新規を拒否（全体リスクゲート）

---

## 8. 検証計画（ウォークフォワード、頑健性、レポート）

- WFV分割例:
  - 例1: 3年train + 1年val を順送り、最後の2年をholdout
  - 例2: 四半期ごとのrolling（高頻度TFで有効）
- コストの入れ方:
  - spread/slippageは複数シナリオで同じモデルを再評価
  - スワップは長期TFのみ影響評価（必要なら）
- 指標:
  - EV（平均損益、net）、Sharpe/Sortino、MaxDD、PF、取引回数、平均保有時間
  - レジーム分解: 高ボラ/低ボラ、セッション別、スプレッド別
- 頑健性チェック:
  - `k1,k2,T,EV_threshold` の感度
  - ブローカー条件差（スプレッド拡大）耐性
  - 直近偏重の検知（年代別の性能分解）

---

## 9. MQL実装計画（推論方式、擬似コード、ログ設計）

- 学習はPython推奨
- 推論方式（2案）:
  - (a) ONNX化してMT5で推論: 配布しやすいがデバッグが重い
  - (b) ルール+軽量モデル: 実装容易、運用が軽い（最初はこれを推奨）
- モジュール分割:
  - Data: ヒストリカル取得、スプレッド取得、ティック/出来高保存
  - Features: TF別の特徴量計算（窓管理）
  - Model: 推論、EV算出、閾値判定
  - Execution: 注文発行、約定確認、再試行、スリッページ制御
  - Risk: DD/日次制限/相関制御
  - Logging: 予測値、EV、コスト、見送り理由、約定統計
- EAメインループ擬似コード（例）:
  - `OnTick`: 新しいバー確定を検知
  - `if not market_ok(): return`
  - `x = compute_features()`
  - `ev_long, ev_short = infer_ev(x)`
  - `if spread > max_spread: log_skip(); return`
  - `dir = argmax(ev_long, ev_short)`
  - `if ev_dir <= threshold: log_skip(); return`
  - `lot = position_size(ATR, risk_budget, corr_state)`
  - `place_order(dir, lot, TP, SL, timeout)`

---

## 10. 失敗しやすい落とし穴チェックリスト

- リーク:
  - 正規化fitが全期間にかかっている
  - ラベル生成の未来窓がCV分割を跨いでいる
- 過学習:
  - 指標は良いのにholdoutで崩れる
  - コストシナリオを上げると即死する
- 実運用:
  - 指標時のスプレッド拡大/滑りを無視している
  - 約定遅延・リトライで想定より悪化する
- 統計:
  - 取引回数が少なすぎて偶然に支配されている
  - レジーム分解で特定期間だけ勝っている

