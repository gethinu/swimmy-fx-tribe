# Institutional Hunter EA Quickstart (MT5)

更新日: 2026-02-21 JST
対象EA: `src/mt5/InstitutionalHunterEA.mq5`

## 1. 目的

`MSB + Order Block + 出来高スパイク + ATR急拡大` を同時に満たす場面だけを狙う、厳格な大口追随EA。

- ゾーン検出: `H4`
- 執行判定: `M15`
- エントリー: OB接触後の反転確定足で成行
- TP: 反対側OB
- SL: OB外側 + ATRバッファ

## 2. 既定パラメータ（実装済み）

- `InpZoneTF=PERIOD_H4`
- `InpExecTF=PERIOD_M15`
- `InpVolumeSmaMult=2.0`
- `InpVolumeZMin=2.0`
- `InpATRExpansionMin=1.3`
- `InpRiskPerTradePct=0.5`
- `InpMaxOpenPositionsTotal=3`
- `InpOnePositionPerSymbol=true`
- `InpMaxTradesPerSymbolPerDay=2`
- `InpMinBarsBetweenEntries=4`
- `InpDailyDdStopPct=2.0`
- `InpSLAtrBufferMult=0.5`
- `InpMaxSpreadPoints=120.0`

## 3. MT5 への適用

1. `InstitutionalHunterEA.mq5` を MT5 `MQL5/Experts/` に配置
2. MetaEditor でコンパイル
3. 任意チャート1枚にEAを適用（EA内部で複数シンボル監視）
4. パラメータ `InpSymbolsCsv` を設定
  - 全監視: `ALL`
  - 指定監視: `XAUUSD,USDJPY,EURUSD,GBPUSD`

自動コンパイル（WSL→Windows MetaEditor）:

- `scripts/compile_institutionalhunter_mt5.sh`
- 既存汎用スクリプトを直接使う場合:
  - `scripts/compile_swimmybridge_mt5.sh --src src/mt5/InstitutionalHunterEA.mq5`

## 4. Strategy Tester 推奨

- Model: `Every tick based on real ticks`
- Main period: `M15`
- Forward: `1/3`
- 初期検証期間目安:
  - IS: 12か月
  - OOS: 6か月

チェック項目:

- PF >= 1.3
- Max DD <= 20%
- 連敗中に日次DD停止が機能すること
- 高スプレッド時間帯で新規停止すること

### 4.1 推奨プリセット

- 厳格デフォルト（全銘柄）:
  - `src/mt5/InstitutionalHunterEA_Strict_AllSymbols.set`
- 最適化（XAU + FX4）:
  - `src/mt5/InstitutionalHunterEA_OptimizeCore_XAU_FX4.set`
- フォワード（XAU + FX4）:
  - `src/mt5/InstitutionalHunterEA_Forward_XAU_FX4.set`

### 4.2 最適化の使い方（短手順）

1. `InstitutionalHunterEA_OptimizeCore_XAU_FX4.set` を読込
2. Genetic optimization を有効化
3. 期間を `IS 12か月` / `OOS 6か月` に設定
4. 上位5設定を `InstitutionalHunterEA_Forward_XAU_FX4.set` ベースに手動反映
5. Forward期間で再検証して採用を1本に絞る

## 5. 注意点

- このEAは「大口注文そのもの」ではなく、価格・出来高・ボラからの推定。
- `InpSymbolsCsv=ALL` はブローカー提供全銘柄を走査するため、CPU負荷が上がる。
- 実運用前に、まず監視対象を絞ってフォワード検証することを推奨。
