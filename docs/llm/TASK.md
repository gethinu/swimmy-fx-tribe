# Swimmy System Tasks

## 今やる1タスク: ドキュメント補完 (Completed)

### 目的
`implementation_plan_v50.6.md` と `owners_guide.md` に記載されている最新機能（V49.8以降）が、現在の `/docs/llm/` に十分に反映されていない可能性があるため、差分を埋める。

### スコープ
- [x] /docs/llm/SPEC.md 更新 (SQL Migration, Atomic Allocation, Regime Lock等)
- [x] /docs/llm/STATE.md 更新 (最新バージョン V50.5 反映)
- [x] /docs/llm/INTERFACES.md 更新 (もしあれば)
- [x] /docs/llm/ARCHITECTURE.md 更新 (Data Keeper, School Daemon等)

### 完了条件
- オーナーズガイドの V50.5 機能 (Symbolic Hashing, Service Isolation, etc.) がSPECに含まれている。
- Systemd構成（4サービス体制）がARCHITECTUREに含まれている。
- リスク管理の「Tiered Cooldown」や「Atomic Allocation」がSPECに含まれている。

### 影響範囲
- ドキュメントのみ

### 手順
1. `owners_guide.md` から V50.5 までの全機能を抽出。
2. `SPEC.md`, `STATE.md`, `ARCHITECTURE.md` に追記。
3. `TASK.md` を更新。

---

## ユーザー確認事項 (質問リスト)

---

## 次タスク: OOSパイプライン整合性修復 (Planned)

### 目的
OOS結果の相関ID欠落・UUID衝突・`oos_queue` 不整合を修正し、A→S昇格停滞を解消する。

### スコープ
- `tools/backtest_service.py` で `request_id` を結果へ伝播
- `src/lisp/core/execution-protocol.lisp` のUUID生成を高エントロピー化
- `src/lisp/school/school-validation.lisp` のリトライID再利用の見直し
- `oos_queue` の手動クリーンアップ手順追加
- `docs/llm/INTERFACES.md` の `BACKTEST_RESULT` 契約追記

### 完了条件
- `oos_queue` が結果受信時に減少し、`oos_status.txt` の success が増える
- `BACKTEST_RESULT` に `request_id` が必ず含まれる

### 手順
1. 失敗テスト追加（`request_id` 伝播）
2. 最小修正を実装
3. 検証手順実行（OOS 1件の往復確認）

### OOSキュー手動クリーンアップ手順
1. `sudo systemctl stop swimmy-school`
2. `sqlite3 data/memory/swimmy.db "DELETE FROM oos_queue;"`
3. `sqlite3 data/memory/swimmy.db "SELECT count(*) FROM oos_queue;"`
4. `sudo systemctl start swimmy-school`

---

## 今やる1タスク: 網羅的システムチェック (Completed)

### 目的
Evolution Factory Report に表示される主要指標（Active, Rank, Graveyard等）の算出ロジックが、仕様通りかつ意図した通りに動作しているかを網羅的に検証する。

### スコープ
- [x] 1. Knowledge Base (Active) 算出プロセス検証
- [x] 2. S-Rank (Verified Elite) 算出プロセス検証 (Sharpe≥0.5, CPCV)
- [x] 3. A-Rank (Pro) 算出プロセス検証 (Sharpe≥0.3, OOS)
- [x] 4. B-Rank (Selection) 算出プロセス検証 (Sharpe≥0.1)
- [x] 5. New Recruits (24h) 算出プロセス検証
- [x] 6. Graveyard 算出プロセス検証

### 完了条件
- 全6項目の算出ロジックがコード上で確認され、SPECとの整合性が取れていること。
- 必要に応じて修正が行われていること。

---

## ユーザー確認事項 (質問リスト)
1. **Lispのポートバインド**: Rustコードを見る限り、Lisp側が `5555` と `5556` にバインドしているようですが、Lispコード (`execution-protocol.lisp`等) 内での具体的な記述箇所は合っていますでしょうか？
   - 回答: `src/lisp/system/runner.lisp` で PULL/PUB を bind（`*port-sensory*`=5555, `*port-motor*`=5556）。ポート定義は `src/lisp/core/config.lisp`。
2. **Rustビルド**: `guardian` ディレクトリ内で `cargo build --release` を実行するのが標準手順で間違いないでしょうか？
   - 回答: 明文化は未確認。現状の運用スクリプトでは `cargo build --release` が使われている（`tools/run_benchmarks.sh`）。`tools/quality_gate.sh` は `cargo test --release`。
3. **Data Keeper**: Python製の `swimmy-data-keeper` (Port 5561) がアーキテクチャ図から漏れていましたが、これは現在もアクティブでしょうか？（オーナーズガイドには記載あり）
