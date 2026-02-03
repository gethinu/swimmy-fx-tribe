# OOS Status Reporting Design

**Goal:** OOS自動審査の稼働状況を「即時性 + 取りこぼし補正」で可視化し、運用導線を一本化する。

**Architecture:** OOS送信/結果受信イベントで `data/reports/oos_status.txt` を即時更新し、1時間ごとに再生成する。Evolution Factory ReportにOOS行を追加してDiscord/ファイルの導線に統合。メモリのOOSメトリクスとDBの `oos_queue` を併用して状況を表示する。

**Tech Stack:** Common Lisp, SQLite (oos_queue), file-based reports

## Components

1) **OOS Status Line**
- `oos-metrics-summary-line` を軸に、`oos_queue` から pending数・最古ageを補足。

2) **oos_status.txt Writer**
- `data/reports/oos_status.txt` に ASCII 1〜2行で保存。
- 書き込み失敗は警告ログのみにして本体フローは止めない。

3) **Evolution Report Integration**
- `generate-evolution-report` に OOS 行を追加。

## Data Flow

- **Event-driven:** `maybe-request-oos-backtest` 成功時 / `handle-oos-backtest-result` 反映時に更新。
- **Periodic recovery:** `phase-7-report`（1時間）で再生成。

## Error Handling

- DBエラー時は `queue error` をstatus行に明示。
- 書き込み失敗は警告ログのみ。

## Testing

- OOS送信時に status writer が呼ばれること。
- Evolution Report に OOS 行が含まれること。

## Docs

- Owner’s Guide に `data/reports/oos_status.txt` を追記。
- OOS自動審査の確認は Evolution Report + oos_status.txt で行う旨を明記。

