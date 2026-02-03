# Backtest Policy Alignment Design

**Date:** 2026-02-03

## Goal
Lisp側の閾値/期間/rolling end_time を仕様通りに整合させ、Guardian側CPCVを train/test split + purge/embargo 適用へ移行する。Backtest Service は契約通りの透過転送を保証する。

## Scope
- Lisp: `*phase2-min-sharpe*` を 0.3 に統一、Phase1/Phase2 期間を 2011–2020 / 2021–現在へ更新。Phase2 end_time は UTC 現在時刻、CSV末尾でクランプ。Phase識別は suffix 年号依存から `range_id` 依存へ移行。
- Backtest Service: `start_time/end_time/data_id/aux_candles(_files)/swap_history` を Guardian に透過転送。
- Guardian: CPCV を train/test split + purge/embargo 適用へ変更。test指標で判定、trainは健全性チェック。

## Data Flow
1. Lispが `start_time/end_time` を生成し、payloadに `phase`/`range_id` を明示。`strategy.name` suffix は `range_id` 由来。
2. Backtest Service が payload を欠落なく Guardian に転送（timeframe 正規化のみ）。
3. Guardian が `start_time/end_time` でスライス。CPCVは train/test を分離し purge/embargo を適用。test指標で合否。

## Error Handling
- Backtest Service が必須フィールド欠落を検出したら即エラー返却。
- Guardian は空スライス/全パス失敗を明示エラーにする。

## Testing
- Lisp: Phase2 end_time/範囲/phase識別のユニットテスト追加。
- Backtest Service: 透過転送の契約テストを追加。
- Guardian: purge/embargo 適用後のセグメント生成と無効化パスのテスト。

## Repos
- Lisp/Backtest Service: `/home/swimmy/swimmy/.worktrees/backtest-policy-fix`
- Guardian: `/home/swimmy/swimmy/guardian`（別リポジトリ）
