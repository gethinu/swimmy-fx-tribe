# Design: Stagnant C‑Rank 日次カリング + バッチフラッシュ独立化

**Date:** 2026-02-09

## 背景 / 問題
- Stagnant C‑Rank 通知はバッチ化済みだが、フラッシュが進化ループ末尾依存で、長時間フェーズ中に沈黙する。
- C‑Rank カリングは週次（土曜）限定で、"10日" 条件と運用期待がズレている。
- 理由判定は文字列検索で脆く、再起動でバッファが消えるため可観測性が低い。

## 目的
- Stagnant C‑Rank を**日次で確実に実行**する。
- バッチ通知を**進化ループ完走に依存させない**。
- 滞留をテレメトリで可視化する。

## 非目的
- Notifier プロトコルの変更。
- 既存の Max Age バッチの仕様変更。

## 提案
### 1) 日次カリングガード
- `school-breeder.lisp` に `*last-cull-day-key*` を導入。
- `process-breeding-cycle` で `maybe-cull-weak-strategies` を呼び、同日ならスキップ。
- これにより「毎日 1 回」の実行を保証。

### 2) 理由の構造化
- `kill-strategy` に `&key reason-code` を追加し、`reason-code` を優先。
- Stagnant C‑Rank では `:stagnant-c-rank` を渡す。
- 既存の文字列検索はフォールバックとして残す（後方互換）。

### 3) バッチフラッシュ独立化
- school プロセス起動時に軽量スレッドを立て、60 秒間隔で `swimmy.core:check-timeout-flushes` を呼ぶ。
- 進化ループ完走を待たずにフラッシュ可能にする。

### 4) テレメトリ
- `queue-stagnant-c-rank` 内で `buffer_len` と `oldest_secs` を算出し、JSONL テレメトリに記録。

## データフロー
1. 日次カリングで `kill-strategy` が `:stagnant-c-rank` を発火。
2. `queue-stagnant-c-rank` がバッファへ追加し、テレメトリ記録。
3. 独立フラッシュスレッドが 60s ごとに `check-timeout-flushes` を実行。
4. 1時間経過でサマリ通知を送信。

## エラー処理
- フラッシュスレッドは `handler-case` で保護し、例外はログに残して終了。
- 進化ループ本体は影響を受けない。

## テスト
- 日次ガードが同日に 1 回のみ実行されること。
- `reason-code` 経由でバッチに積まれること。
- 進化ループ非依存でフラッシュが動くこと（モックで確認）。

## ロールアウト
1. コード更新 → school 再起動。
2. 10分以内にテレメトリで `stagnant_c_rank_buffer_len` を確認。
3. 1時間以内にサマリ通知を確認。
