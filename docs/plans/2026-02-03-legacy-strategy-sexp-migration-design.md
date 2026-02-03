# Legacy Strategy SEXP Migration Design

**Goal:** 旧フォーマット（plist 形式）の戦略データを `#S(STRATEGY ...)` 形式へ100%再シリアライズし、DB内の互換性エラーと誤検知ログを解消する。

## Approach
- オフライン一括移行（A案）。`swimmy-brain` と `swimmy-backtest` を停止し、DBをバックアップ後に移行実行。
- 既存 DB を読み取り、新規 DB を作成して全テーブルを移し替える。`strategies.data_sexp` だけは再生成。
- 再生成は「安全読み取り→構造体化→再シリアライズ」のみ。新規ロジック追加や指標再計算は行わない。
- 変換不能な行は `data/memory/quarantine/` に退避し、件数と理由をログで報告する。

## Data Handling
- `#S(` 形式：`safe-read-sexp` → `strategy-p` 確認 → `format "~s"` で再保存。
- plist 形式：既知キー（`NAME/TIMEFRAME/DIRECTION/SYMBOL/SL/TP/SHARPE/PROFIT-FACTOR/WIN-RATE/MAX-DD`）を `make-strategy` にマッピング。欠落は既定値。
- `strategies` の他カラム（`rank`, `sharpe`, `profit_factor`, `oos_sharpe`, `cpcv_median`, `cpcv_pass_rate` など）は元DBの値を保持。
- `trade_logs`, `swap_history`, `oos_queue` は無変換でコピー。

## Verification
- 変換後 DB で `data_sexp LIKE '#S(%'` が 100% になることを確認。
- 件数一致（旧DBと新DBの row count）を必須条件とする。
- 起動後に `Evolution Factory Report` とログを確認し、`Corrupted Strategy SEXP` が消えていることを確認。
