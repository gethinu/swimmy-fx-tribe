# Noncorrelation Coverage + Telemetry Design

Date: 2026-02-08

## Goal
- 非相関スコアが `N/A` のとき、データ不足理由を **定量的に表示** して誤解を減らす。
- `N/A` 発生を **構造化テレメトリ** で観測できるようにする。

## Decisions
- 表示形式: `非相関スコア: N/A (データ不足: 0/30日)`
- coverage は **ペアの重なり日数** を使用（相関計算に必要な日数の実データ）。
- 通知の発火条件・閾値・ランク判定は **変更しない**。
- テレメトリは `swimmy.json.log` に `event_type=noncorrelation.score` で記録。

## Architecture
### 1) Coverage計算
- `aligned-daily-pnl-vectors` が **重なり日数 (overlap)** を返す。
- `calculate-daily-pnl-correlation` は `(values corr overlap)` を返す。
- `calculate-noncorrelation-score` は `(values score reason coverage-days)` を返す。

### 2) 通知文言
- `notify-noncorrelated-promotion` で `reason=:insufficient-data` の場合のみ
  `N/A (データ不足: coverage/required日)` を表示。

### 3) テレメトリ
- `emit-telemetry-event` で以下の payload を送信:
  - `:strategy`, `:new-rank`, `:score`, `:reason`, `:coverage-days`, `:required-days`, `:portfolio-size`
  - `event_type = "noncorrelation.score"`

## Error Handling
- coverage が取得できない場合は既存の `N/A (データ不足)` を維持。
- テレメトリ失敗は既存のエラーハンドリングに委譲（通知は継続）。

## Testing
- coverage を返すこと（不足時に `coverage-days` が期待値になる）
- N/A 通知に coverage が含まれること
- telemetry が送出されること

## Non-Goals
- 相関計算の手法変更
- ランク判定や昇格条件の変更
- backtest 由来の相関スコア導入
