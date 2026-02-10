# Pattern Similarity Gate Design

Date: 2026-02-10

## Purpose
チャートパターンの類似性を用いて、既存戦略の前段フィルタとしてレジーム/ゲートを提供する。

## Scope
- FX: USDJPY / EURUSD / GBPUSD
- Timeframes: M1保存、M5/M15リサンプル、H1/H4/D1/W1/MN1直取得
- Pattern Similarity Service (Python, ZMQ REQ/REP 5564, S-expression)

## Non-Goals
- 取引戦略の置き換え
- バイナリ送信（画像送信）
- HFTレベルの超低遅延

## Key Decisions
- QUERY入力は OHLCV の S式、画像生成はサービス側
- 類似度は距離重み確率（k=30、閾値=0.60）
- ソフトゲート: 不一致時ロット 0.7 倍（ライブ/OOS/CPCV/バックテストに適用）
- TF一致のみ適用、H1以上の足確定時に評価
- ペイロード上限はデフォルト 2MB（設定で変更可）
- VAPはティック由来、ヒストリカルは Data Keeper に保存
- Deep Learningは 2→3 の段階移行（軽いFT → Siamese/Triplet）

## Architecture
- Pattern Similarity Service を新設し Lisp から直接 REQ/REP 呼び出し
- Data Keeper は OHLCV と TICK を保存し、VAP生成の正本とする
- 埋め込みは CLIP ViT-B/32 相当（GPU可、CPUフォールバック）
- インデックスは `data/patterns/` に npz + FAISS を保存

## Data Flow
1. Data Keeper が OHLCV/TICK を保存
2. BUILD_INDEX でヒストリカルから埋め込み作成→FAISS化
3. H1以上の足確定時に Lisp が QUERY を送信
4. Service が画像生成→埋め込み→近傍探索→確率返却
5. Lisp が確率に基づきロットを 0.7 倍に減衰（不一致時）

## Interfaces
Pattern Similarity Service (5564):
- STATUS / BUILD_INDEX / QUERY
- QUERY入力: S式OHLCV窓（window_bars）
- QUERY出力: p_up/p_down/p_flat と top_k

Data Keeper (5561):
- GET_TICKS / ADD_TICK を追加
- TICK schema: timestamp, bid, ask, volume

## Labeling
- ATR基準の Up/Down/Flat
- 評価幅（固定時間）:
  - M5/M15: 4時間
  - H1/H4: 1日
  - D1: 1週間
  - W1/MN1: 1か月

## Window/Stride
- Window bars:
  - M5: 120
  - M15: 120
  - H1: 120
  - H4: 120
  - D1: 120
  - W1: 104
  - MN1: 120
- Stride:
  - M5: 30分ごと（6本）
  - M15: 1時間ごと（4本）
  - H1/H4/D1/W1/MN1: 1本ごと

## Error Handling
- QUERY error時はゲート無しで継続
- `candles` 長不一致やサイズ超過は error を返す
- VAP欠損はゼロ埋めし、telemetryに警告を記録

## Observability
- Structured Telemetry に確率・減衰率・対象TF・エラー理由を記録
- BUILD_INDEX は STATUS で進捗を確認可能

## EV-First補足（期待値で一貫させる）
Pattern Similarity Gate は単体で売買判断を置き換えず、コスト込み期待値(EV)最大化の前段フィルタとして位置付ける。
EV判断は Triple-Barrier の確率（Up/Down/Timeout）とコストモデル（spread/slippage/commission/swap）から
`EV_net` を算出し、`EV_net` が閾値未満なら No-Trade とする。Gate はこの EV 判定にソフトに作用し、
p_up/p_down/p_flat が不一致または低信頼のときは `EV_threshold` を引き上げる、または既定のロット 0.7 倍へ減衰する。
学習と検証は Purged CV + Embargo を必須とし、WFV で年代別・セッション別のレジーム耐性を確認する。
評価指標は分類精度ではなく `EV_net` の分布改善（平均EV、下方リスクの抑制）に置く。
パターン埋め込みは特徴量の一部として扱い、最終判断は常に EV に一貫させる。

## Testing
- S式契約テスト（STATUS/BUILD_INDEX/QUERY）
- QUERY error時のフォールバック継続テスト
- 再現性: 同一OHLCV入力で同一出力
- OOS/CPCV比較で過学習チェック（Purged CV/Embargo前提）

## Deep Learning Roadmap
- Phase 1: CLIP推論のみ or 軽いFT
- Phase 2: Siamese/Triplet（学習パイプライン仕様化後）

## Open Questions
- Training Serviceの責務/入出力/版管理
- Purged CV/Embargoの具体仕様
- VAP欠損時の補正ロジック
