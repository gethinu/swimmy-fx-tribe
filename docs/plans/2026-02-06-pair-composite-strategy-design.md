# Pair-Composite Strategy Design

**Date:** 2026-02-06
**Owner:** Swimmy
**Status:** Draft

## 1. Purpose
Pair-Composite は、既存の単一戦略パイプラインを壊さずに、**分散効果**を目的として“ペア戦略”を追加するためのオーバーレイ層である。単一戦略の方向・ロジックは変更しない。ペア層は「ペア候補の選定」「ペア枠の制約」「ペア用の記録」に限定する。

## 2. Scope & Constraints
- 既存単一戦略の挙動を変えない（方向・ロジック・シグナル生成は不変）。
- 介入は **ロット上限のみ**。
- `SWIMMY_PAIR_STRATEGY=1` のときのみ有効。無効時は完全にバイパス。
- 対象戦略: A/S/Legend
- 上限: 5ペア
- 相関: |corr| ≤ 0.2、サンプルN ≥ 100必須
- 目的: 分散重視
- 救済モード(|corr|≤0.3): **Phase2** かつ候補が5ペア未満のときのみ
- ペア枠: **0.05 lot**
- データ欠損（PnL系列が不足/欠落）は候補除外
- 片脚がA未満へ降格/無効化されたら当該ペアも即時無効化

## 3. Architecture (High-Level)
- **Pair Candidate Generator**: A/S/Legendから候補生成
- **Pair Evaluator**: PnL相関 + スコア計算
- **Execution Overlay**: ペア枠(0.05 lot)制約のみ適用
- **Persistence Layer**: SQLiteにペア定義/履歴/トレード系列保存

## 4. Data Flow
1. Backtest/OOS/CPCVの結果に「トレード一覧」を含めて返却（S式）。
2. Lisp側で `backtest_trade_logs` に保存（`request_id` + `oos_kind` で紐付け）。
3. `oos_kind` は `-OOS`=OOS、`-QUAL/-RR`=BACKTEST(IS) として保存（CPCVは trade_list 対応後）。
4. 候補生成時、PnL系列（OOS/CPCV/Backtest を結合）で相関を計算。
4. 合成PnL（0埋めマージ + リスクパリティ重み）でスコアを計算し上位5ペアを選定。
5. 実行時、単一戦略の出力に対しペア枠のみ適用。

## 5. Pair Selection Rules
- **相関閾値**: |corr| ≤ 0.2
- **サンプル条件**: N ≥ 100 トレード（不足は除外）
- **候補枠**: 上限5ペア
- **スコア**: `0.7 * Sharpe + 0.3 * PF`
- **評価ウィンドウ**: 直近100トレード
- **相関計算**: 合成PnL系列（クローズ時刻でマージ、片側欠損は0埋め）
- **重み**: リスクパリティ重みをそのまま使用
- **PnL系列ソース**: OOS/CPCV/Backtest のトレード履歴を結合（欠損は候補除外）
- **oos_kindの割当**: `-OOS`=OOS、`-QUAL/-RR`=BACKTEST(IS)。CPCVは trade_list 対応後に追加。
- **失効条件**: 片脚がA未満なら当該ペアを即時無効化

## 6. Execution Overlay (Non-Destructive Contract)
- **非破壊定義**: 単一戦略の方向・ロジック・シグナル生成は不変。ロット上限のみ介入。
- **片脚のみ発火**: 0.05枠をその脚で使用
- **両脚発火**: 重み比で分割
- **保証**: ペア層ON/OFFで単一戦略の出力が変わらないことをテストで保証
- **フェイルオープン**: ペア枠の計算に失敗した場合は上限を適用しない

## 7. Persistence Schema (SQLite)
### 7.1 trade_logs 追加
- `pair_id` を追加（NULL許容）

### 7.2 pair_definitions
```
pair_id TEXT PRIMARY KEY,
strategy_a TEXT,
strategy_b TEXT,
created_at INTEGER,
active INTEGER
```
※ `pair_id` は戦略名のソート済みペアの安定ハッシュ（順序非依存）

### 7.3 pair_scores_history
```
pair_id TEXT,
score REAL,
sharpe REAL,
pf REAL,
correlation REAL,
window_trades INTEGER,
created_at INTEGER
```

### 7.4 backtest_trade_logs
```
request_id TEXT,
strategy_name TEXT,
 timestamp INTEGER,
 pnl REAL,
 symbol TEXT,
 direction TEXT,
 entry_price REAL,
 exit_price REAL,
 sl REAL,
 tp REAL,
 volume REAL,
 hold_time INTEGER,
 rank TEXT,
 timeframe INTEGER,
 category TEXT,
 regime TEXT,
 oos_kind TEXT
```
- NULL許容
- 全件保持

## 8. Interfaces
- `BACKTEST_RESULT` に `trades` を追加（S式）
- トレード一覧は完全キーで返却
- **巨大メッセージ対策**: `trades_ref` + `trades_truncated=true` を返し、フルはDB参照へ切替

## 9. Error Handling & Safety
- トレード一覧欠損/DB書き込み失敗時はペア評価を無効化（単一戦略は不変）
- N<100は除外
- 失敗時は単一戦略挙動を変更しない（フェイルオープン）

## 10. Testing
- **境界テスト**: ペア層ON/OFFで単一戦略の方向/ロジック出力が一致
- **相関計算テスト**: 0埋め合成PnLの結果が期待値通り
- **DBテスト**: backtest_trade_logs保存/取得

## 11. Open Questions
- Backtestトレード一覧のサイズ制限（閾値）と分割方式
- Guardian/Backtest Service側の実装範囲

## 12. Next Steps
1. `docs/llm/STATE.md` / `docs/llm/INTERFACES.md` 更新
2. SQLiteスキーマ拡張
3. ペア層モジュールの実装
4. 境界テスト追加
