# Expert Panel 2 (Reliability & Resilience) — 2026-02-04

## 対象
- 症状: `swimmy-brain` 起動時に `[HEADHUNTER] 🚽 Flushing deferred backtests...` が長時間走り、結果受信ループに入る前に大量送信してしまう。副作用として `data/reports/backtest_debug.log` が生成されない（受信処理に到達しない/遅延する）。
- 背景: Backtest Service(5580/5581) と Guardian `--backtest-only` に対し、Brainが起動直後にバックログを全放出する設計になっていた。

## 結論（推奨）
**「上限付き / 後回し」へ変更するのが信頼性・可用性の観点で圧勝**です。  
起動時の最優先は「受信ループへ入って生存し続けること（liveness）」で、重い一括処理は後段でレート制限して流すべきです。

---

## パネル所見

### 1) Werner Vogels（Scalability / Amazon CTO）
- “Everything fails, all the time.” を前提にすると、起動直後に「大量送信→受信開始が遅れる」は**自己DoS**に近い。
- **バックプレッシャーが無い一括送信**は、ZMQのHWM/バッファ、Backtest Service、Guardian、ディスク（ログ）を連鎖的に圧迫する。
- 推奨: 起動は最小化し、バックログ処理は **キュー＋レート制御** にして、システムを「常に動ける状態」に保つ。

### 2) Joe Armstrong（Fault Tolerance / Erlang）
- “Let it crash” の前提でも、クラッシュしない形で詰まる（起動が終わらない）構造は最悪。
- 長時間ブロックする処理は**隔離**すべき（別ループ/別スレッド/別プロセス/明確なワーカー）。
- 推奨: main loopは受信・監視を最優先し、バックテスト要求はワーカー化して **小分けに送る**（失敗したら再試行/再スケジュール可能に）。

### 3) Margaret Hamilton（Safety / NASA）
- 失敗モードの列挙が必要:
  - Backtest Service 停止/遅延
  - Guardian バイナリ欠損
  - ZMQバッファ飽和（sendが詰まる）
  - ログ爆発 → ディスク逼迫 → 連鎖障害
- 推奨: 「起動＝安全に安定稼働状態へ到達」が達成条件。大量処理は**安全弁（上限・間隔・無効化スイッチ）**が必須。

### 4) W. Edwards Deming（Quality / Process）
- “In God we trust, all others must bring data.”
- 起動のSLOを定義すべき（例: `brain` が **N秒以内に受信ループへ到達** / **バックログ件数** / **flush rate**）。
- 推奨: backlog件数・送信レート・受信レート・滞留時間を数値化して、改善前後で比較する。

### 5) Brendan Gregg（Performance / Observability）
- 観測できないものは直せない。
- 推奨:
  - `swimmy-brain` の「ループ遅延(ms)」「flush送信時間」「pending queue長」を出す
  - Backtest Service側の「受信→処理→送信」遅延や詰まりをログ/メトリクス化
  - 必要なら eBPF/`perf` でCPU/IOボトルネックを特定

---

## 具体アクション（優先順）
1) **起動時 flush を受信ループより後に回す**（起動ブロック禁止）
2) **レート制限**（batch上限＋実行間隔）を導入し、運用で調整可能にする
3) **重複送信防止**（同一戦略への再送を抑制）
4) **可観測性**: pending件数 / flush送信数 / エラー数 / backtest受信停滞アラート
5) （次）ZMQのHWM/timeout/linger を明示設定して「詰まった時の挙動」を固定する

---

## 変更方針（この判断の意味）
「上限付き/後回し」は“遅くする”のではなく、**システムの生存性を守りながら前に進める**設計です。  
結果処理ループ（受信）が先に回れば `backtest_debug.log` や `backtest_status.txt` の観測が成立し、障害時も原因切り分けが可能になります。

