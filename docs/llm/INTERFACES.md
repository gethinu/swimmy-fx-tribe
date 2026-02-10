# Interface Definitions (ZeroMQ)

## 通信方式
- **Transport**: TCP
- **Encoding**: 内部ZMQ＋補助サービス境界はS-expression（alist）に統一。**ZMQはS式のみでJSONは受理しない**。外部API境界（Discord/HTTP/MCP stdio）はJSON維持。
- **Key/文字**: キーはASCII小文字、文字列はUTF-8想定。
- **Ports**:
  - `5557`: Market Data (MT5 PUB -> Rust SUB)
  - `5560`: Execution (Rust PUB -> MT5 SUB)
  - `5555`: Sensory Input (Rust PUSH -> Lisp PULL)
  - `5556`: Motor Output (Lisp PUB -> Rust SUB)
  - `5559`: External Command (MCP/Tools) -> Rust SUB
  - `5561`: Data Keeper (REQ/REP, S-expression) -> Data Keeper Service
  - `5562`: Notifications (Rust/Lisp PUSH -> Notifier Service, S-expression)
  - `5563`: Risk Gateway (REQ/REP, S-expression) -> Risk Gateway Service
  - `5565`: Pattern Similarity (REQ/REP, S-expression) -> Pattern Similarity Service
  - `5580`: Backtest Commands (Lisp PUSH -> Backtest Service PULL)
  - `5581`: Backtest Results (Backtest Service PUSH -> Lisp PULL)

## MCP Tools (Gateway)
- `health.ping` (read-only)
- `system.status` / `system.metrics` (read-only)
- `backtest.submit` / `backtest.status` (backtest-exec)
- `trade.submit` (trade-capable; disabled / 403)

## MCP stdio (JSON-RPC 2.0)
- **Transport**: stdio（Content-Length フレーミング）
- **Methods**: `health.ping` / `system.status` / `system.metrics` / `backtest.submit` / `backtest.status` / `trade.submit`(403)
- **Params**: `api_key` 必須（`SWIMMY_MCP_API_KEY` と一致）
 - **Bridge**: MCP Gateway は JSON‑RPC を受け取り、S式に変換して `:5559`（External Command）へPUBする。

## データスキーマ (S-expression)
S式は alist 形式のS-expressionで表現する（例: `((type . "TICK") (symbol . "USDJPY") ...)`）。

### 1. Market Data (Port 5557)
MT5からブロードキャストされる。

**TICK**:
```
((type . "TICK")
 (symbol . "USDJPY")
 (bid . 145.205)
 (ask . 145.218))
```

**ACCOUNT_INFO**:
```
((type . "ACCOUNT_INFO")
 (timestamp . 1709234567)
 (equity . 1005000.0)
 (balance . 1000000.0)
 (margin . 50000.0)
 (free_margin . 950000.0)
 (margin_level . 2010.0)
 (profit . 5000.0)
 (leverage . 25))
```

**HISTORY**:
```
((type . "HISTORY")
 (symbol . "USDJPY")
 (tf . "M1")
 (batch . 0)
 (total . 3)
 (data . (((t . 1709234500) (o . 145.10000) (h . 145.20000) (l . 145.05000) (c . 145.18000))
          ((t . 1709234560) (o . 145.18000) (h . 145.24000) (l . 145.12000) (c . 145.21000)))))
```

**POSITIONS**:
```
((type . "POSITIONS")
 (symbol . "USDJPY")
 (data . (((ticket . 123456789) (magic . 123456) (type . "BUY") (volume . 0.10))
          ((ticket . 123456790) (magic . 123457) (type . "SELL") (volume . 0.05)))))
```

**SWAP_DATA**:
```
((type . "SWAP_DATA")
 (symbol . "USDJPY")
 (swap_long . -0.12)
 (swap_short . 0.05)
 (swap_mode . 1)
 (spread . 12))
```

**ORDER_ACK**:
```
((type . "ORDER_ACK")
 (id . "uuid-v4")
 (ticket . 123456789)
 (symbol . "USDJPY"))
```

**TRADE_CLOSED**:
```
((type . "TRADE_CLOSED")
 (won . true)
 (pnl . 1234.56)
 (symbol . "USDJPY")
 (ticket . 123456789)
 (magic . 123456)
 (entry_price . 145.120)  ; optional
 (exit_price . 145.300))  ; optional
```
注記: `entry_price` / `exit_price` は任意。存在する場合は執行スリッページの算出に利用する。

### 2. Execution / Commands (Port 5560)
RustからMT5へ送信される。

**ORDER_OPEN**:
```
((type . "ORDER_OPEN")
 (id . "uuid-v4")
 (side . "BUY")
 (instrument . "USDJPY")
 (lot . 0.1)
 (sl . 144.500)
 (tp . 146.500)
 (magic . 123456)
 (comment . "StrategyName"))
```
**備考**: `instrument` + `side` + `lot` が正式。`symbol/action` など旧キーは受理しない（Sender側もS式に統一する）。

**CLOSE**:
```
((type . "CLOSE")
 (close_all . false)
 (magic . 123456)
 (symbol . "USDJPY"))
```

**REQ_HISTORY**:
```
((type . "REQ_HISTORY")
 (symbol . "USDJPY")
 (tf . "M1")
 (count . 2000)
 (start . 1709234567))  ; optional (Unix seconds)
```

**GET_POSITIONS**:
```
((type . "GET_POSITIONS")
 (symbol . "USDJPY"))  ; optional
```

**GET_SWAP**:
```
((type . "GET_SWAP")
 (symbol . "USDJPY"))  ; optional
```

**CLOSE_SHORT_TF**:
```
((type . "CLOSE_SHORT_TF")
 (symbol . "USDJPY"))  ; optional
```

### 3. Sensory Input (Port 5555)
Rust -> Lispへ内部状態や正規化されたデータを送る。
(スキーマはTICK/ACCOUNT_INFOとほぼ同じだが、Rust側で前処理が入る場合あり)

**System Commands (Port 5555 / type=SYSTEM_COMMAND)**  
Brainが即時で処理する管理系コマンド。  
- `REPORT_STATUS`: アクティブポジションの一覧を返送（既存）。  
- `BACKTEST_SUMMARY`: 最新RRバックテスト概要をDiscord通知（既存）。  
- `BACKTEST_SUMMARY_QUAL`: 最新QUALバックテスト概要をDiscord通知（既存）。  
- `HEARTBEAT_NOW`: Discord向けHeartbeatを即時送信（新設、2026-01-31）。

**例**:
```
((type . "SYSTEM_COMMAND")
 (action . "REPORT_STATUS"))
```

### 4. Motor Output (Port 5556)
Lisp -> Rustへ「意図」を送る。
スキーマはExecution用とほぼ同じだが、Risk Gate用のメタデータが含まれる場合がある。

```
((type . "SIGNAL")
 (strategy . "Trend-Follow-V1")
 (symbol . "USDJPY")
 (action . "BUY")
 (lot . 0.1)
 (sl . 144.50)
 (tp . 146.50)
 (estimated_loss . 2000.0))
```

### 5. Backtest Service (Ports 5580/5581)
Brainのバックテスト要求を専用サービスへオフロードする。  
**S式プロトコルに統一**（2026-01-31）。Backtest Service側でS式を受け取り、そのままGuardian `--backtest-only` に流し、結果もS式で返却する。  
（Backtest Service が無い場合は 5556 経由の Guardian 直バックテストにフォールバック可能）

**Option値の表現（serde_lexpr）**: Guardian `--backtest-only` は `Option<T>` を **空リスト or 1要素リスト**で受け取る。  
- `None` → `(candles_file)` / `(data_id)` / `(start_time)` のように **値なし**（空リスト）  
- `Some(x)` → `(candles_file "path.csv")` / `(start_time 1700000000)` のように **値1つ**（1要素リスト）  

**BACKTEST (Request, S-Expression)**:
```
((action . "BACKTEST")
 (strategy . ((name . "Volvo-Scalp-Gen0")
              (timeframe . 1)
              (sma_short . 5)
              (sma_long . 20)
              (sl . 0.0010)
              (tp . 0.0015)
              (volume . 0.02)))
 (phase . "phase1")               ; optional
 (range_id . "P1")                ; optional
 (start_time 1293840000)          ; optional (Unix seconds) - Option<i64>
 (end_time 1700000000)            ; optional (Unix seconds) - Option<i64>
 (data_id "USDJPY_M1")            ; CSVを使う場合 - Option<String>
 (candles_file "data/historical/USDJPY_M1.csv") ; Option<String>
 (aux_candles . ((t . 1)))        ; optional
 (aux_candles_files . ("a.csv"))  ; optional
 (swap_history . ((t . 1) (sl . 0.1) (ss . 0.1))) ; optional
 (symbol . "USDJPY")
 (timeframe 1))                   ; Option<i64>
```

**BACKTEST_RESULT (Response, Guardianフォーマットそのまま)**:
**必須**: `result` 内に `request_id`（相関ID）。
Backtest Service は `request_id` が欠落した BACKTEST を受け取った場合、`BACKTEST_RESULT` を **error** で返し、`request_id` に `"MISSING"` を設定する。
```
((type . "BACKTEST_RESULT")
 (result . ((strategy_name . "Volvo-Scalp-Gen0")
            (request_id . "RID-123")  ; 必須: 相関ID
            (sharpe . 0.85)
            (trades . 120)          ; trades は件数（数値）
            (profit_factor . 1.6)
            (win_rate . 0.52)
            (max_dd . 0.12)
            ;; optional: トレード一覧（巨大メッセージ時は省略可）
            ;; trade_list は配列（alist の配列）
            (trade_list . (((timestamp . 1709234567)
                            (pnl . 12.3)
                            (symbol . "USDJPY")
                            (direction . "BUY")
                            (entry_price . 145.10)
                            (exit_price . 145.20)
                            (sl . 145.00)
                            (tp . 145.30)
                            (volume . 0.02)
                            (hold_time . 120)
                            (rank . "A")
                            (timeframe . 1)
                            (category . "trend")
                            (regime . "trend")
                            (oos_kind . "OOS"))))
            ;; optional: サイズ制御（trade_list 省略時）
            (trades_truncated . false)
            (trades_ref . "RID-123"))))
```

**BACKTEST_RESULT (Error, S-Expression only / JSON禁止)**:
```
((type . "BACKTEST_RESULT")
 (result . ((strategy_name . "Volvo-Scalp-Gen0")
            (request_id . "RID-123")  ; 必須: 相関ID
            (sharpe . 0.0)
            (error . "S-Exp parse error: ..."))))
```

**備考**: 以前のJSON要求形式は廃止（後方互換なし）。Backtest Service は **S式のみ**受理/返却する。  

**CPCV_VALIDATE (Request, S-Expression)**:
```
((action . "CPCV_VALIDATE")
 (strategy_name . "Volvo-Scalp-Gen0")  ; 必須
 (symbol . "USDJPY")                   ; 必須
 (candles_file . "/path/to/USDJPY_M1.csv") ; 必須
 (request_id . "RID-123")              ; optional: 相関ID
 (strategy_params . ((name . "Volvo-Scalp-Gen0")
                     (sma_short . 10)
                     (sma_long . 50)
                     (sl . 50.0)
                     (tp . 100.0)
                     (volume . 0.01))))) ; 必須(空でも可)
```
**Required keys**: `action`, `strategy_name`, `symbol`, `candles_file`, `strategy_params`  
**Optional keys**: `request_id`  
**Notes**: `strategy_params` は `strategy-to-alist` 出力（S式のalist）。空でもキー自体は必須。  

**CPCV_RESULT (Response, Guardianフォーマット)**:
```
((type . "CPCV_RESULT")
 (result . ((strategy_name . "Volvo-Scalp-Gen0")
            (request_id . "RID-123")  ; optional: 相関ID
            (median_sharpe . 0.55)
            (median_pf . 1.52)
            (median_wr . 0.47)
            (median_maxdd . 0.12)
            (path_count . 20)
            (passed_count . 12)
            (failed_count . 8)
            (pass_rate . 0.60)
            (is_passed . true)
            ;; optional: トレード一覧（巨大メッセージ時は省略可）
            ;; trade_list は BACKTEST_RESULT と同一形式
            (trade_list . (((timestamp . 1709234567)
                            (pnl . 12.3)
                            (symbol . "USDJPY")
                            (direction . "BUY")
                            (entry_price . 145.10)
                            (exit_price . 145.20)
                            (sl . 145.00)
                            (tp . 145.30)
                            (volume . 0.02)
                            (hold_time . 120)
                            (rank . "A")
                            (timeframe . 1)
                            (category . "trend")
                            (regime . "trend")
                            (oos_kind . "CPCV"))))
            (trades_truncated . false)
            (trades_ref . "RID-123"))))
```

### 6. Data Keeper Service (Port 5561)
ヒストリカルデータの参照/保存を担当する補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **S-expression（alist）**。  
**必須キー**: `type` / `schema_version` / `action`（`schema_version=1`）。

**STATUS (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "STATUS"))
```

**STATUS (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "running")
 (symbols . (((symbol . "USDJPY")
              (timeframes . (((tf . "M1") (count . 123456))
                             ((tf . "H1") (count . 1234))))))))
```

**GET_HISTORY (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "GET_HISTORY")
 (symbol . "USDJPY")
 (timeframe . "M1")  ; optional (default "M1")
 (count . 2000))
```

**GET_HISTORY (Response, newest first)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (symbol . "USDJPY")
 (timeframe . "M1")
 (count . 2)
 (candles . (((timestamp . 1709234560)
              (open . 145.18)
              (high . 145.24)
              (low . 145.12)
              (close . 145.21)
              (volume . 120))
             ((timestamp . 1709234500)
              (open . 145.10)
              (high . 145.20)
              (low . 145.05)
              (close . 145.18)
              (volume . 98)))))
```

**GET_FILE_PATH (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "GET_FILE_PATH")
 (symbol . "USDJPY")
 (timeframe . "M1"))
```

**GET_FILE_PATH (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (path . "/abs/path/to/USDJPY_M1.csv"))
```

**ADD_CANDLE (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "ADD_CANDLE")
 (symbol . "USDJPY")
 (timeframe . "M1")  ; optional (default "M1")
 (candle . ((timestamp . 1709234560)
            (open . 145.18)
            (high . 145.24)
            (low . 145.12)
            (close . 145.21)
            (volume . 120))))
```

**ADD_CANDLE (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (symbol . "USDJPY")
 (timeframe . "M1"))
```

**GET_TICKS (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "GET_TICKS")
 (symbol . "USDJPY")
 (count . 2000)
 (start_time . 1709234567)  ; optional (Unix seconds)
 (end_time . 1709238567))   ; optional (Unix seconds)
```

**GET_TICKS (Response, newest first)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (symbol . "USDJPY")
 (count . 2)
 (ticks . (((timestamp . 1709234567)
             (bid . 145.205)
             (ask . 145.218)
             (volume . 12))
            ((timestamp . 1709234566)
             (bid . 145.204)
             (ask . 145.217)
             (volume . 8)))))
```

**ADD_TICK (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "ADD_TICK")
 (symbol . "USDJPY")
 (tick . ((timestamp . 1709234567)
          (bid . 145.205)
          (ask . 145.218)
          (volume . 12))))
```

**ADD_TICK (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (symbol . "USDJPY"))
```

**SAVE_ALL (Request)**:
```
((type . "DATA_KEEPER")
 (schema_version . 1)
 (action . "SAVE_ALL"))
```

**SAVE_ALL (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "ok")
 (message . "Saving started in background"))
```

**Error (Response)**:
```
((type . "DATA_KEEPER_RESULT")
 (schema_version . 1)
 (status . "error")
 (error . "message"))
```

### 7. Notifier Service (Port 5562)
Discord通知の非同期中継（Python）。  
**プロトコル**: ZMQ **PUSH/PULL** + **S-expression（alist）**。  
**必須キー**: `type` / `schema_version` / `action`（`schema_version=1`）。

**SEND (Request)**:
```
((type . "NOTIFIER")
 (schema_version . 1)
 (action . "SEND")
 (webhook . "https://discord.com/api/webhooks/...")
 (payload . ((embeds . (((title . "Swimmy")
                         (description . "...")
                         (color . 3447003)))))))
```
**備考**:
`payload` はDiscordのJSONペイロードをS式（alist + list）で表現したもの。  
互換用途として `payload_json`（Discord JSON文字列）も受理するが、**`payload` と `payload_json` の同時指定は不可**。Notifier側でJSONとして送信する。
**運用例**:
非相関昇格通知は `title="⚖️ 非相関 昇格通知"` を使用し、戦略名・昇格ランク・非相関スコア・閾値・ポートフォリオ数を本文に含める。

### 8. Risk Gateway Service (Port 5563)
取引許可の判定を行う補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **S-expression（alist）**。  
**必須キー**: `type` / `schema_version` / `action`（`schema_version=1`）。

**CHECK_RISK (Request)**:
```
((type . "RISK_GATEWAY")
 (schema_version . 1)
 (action . "CHECK_RISK")
 (side . "BUY")
 (symbol . "USDJPY")
 (lot . 0.01)
 (daily_pnl . -500.0)
 (equity . 50000.0)
 (consecutive_losses . 2))
```

**CHECK_RISK (Response)**:
```
((type . "RISK_GATEWAY_RESULT")
 (schema_version . 1)
 (status . "APPROVED")  ; or "DENIED"
 (reason . "Risk checks passed"))
```

**RESET (Request)**:
```
((type . "RISK_GATEWAY")
 (schema_version . 1)
 (action . "RESET"))
```

**RESET (Response)**:
```
((type . "RISK_GATEWAY_RESULT")
 (schema_version . 1)
 (status . "RESET_COMPLETE"))
```

### 9. Pattern Similarity Service (Port 5565)
チャートパターンの画像化・埋め込み・近傍検索を行う補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **S-expression（alist）**。  
**必須キー**: `type` / `schema_version` / `action`（`schema_version=1`）。  
**注意**: **バイナリ送信は禁止**（画像は送らず、OHLCVのS式のみ）。  

**STATUS (Request)**:
```
((type . "PATTERN_SIMILARITY")
 (schema_version . 1)
 (action . "STATUS"))
```

**STATUS (Response)**:
```
((type . "PATTERN_SIMILARITY_RESULT")
 (schema_version . 1)
 (status . "ok")
 (model . "clip-vit-b32")
 (indices . (((symbol . "USDJPY")
              (timeframe . "H1")
              (count . 12345)
              (last_built . 1709234567))))))
```

**BUILD_INDEX (Request)**:
```
((type . "PATTERN_SIMILARITY")
 (schema_version . 1)
 (action . "BUILD_INDEX")
 (symbol . "USDJPY")
 (timeframes . ("M5" "M15" "H1" "H4" "D1" "W1" "MN1"))
 (start_time . 1167600000)   ; optional
 (end_time . 1735689600)     ; optional
 (force . false))            ; optional
```

**BUILD_INDEX (Response)**:
```
((type . "PATTERN_SIMILARITY_RESULT")
 (schema_version . 1)
 (status . "ok")
 (message . "build started"))
```

**QUERY (Request)**:
```
((type . "PATTERN_SIMILARITY")
 (schema_version . 1)
 (action . "QUERY")
 (symbol . "USDJPY")
 (timeframe . "H1")
 (as_of . 1709234560)    ; optional
 (k . 30)                ; optional
 (candles . (((timestamp . 1709234500)
              (open . 145.10)
              (high . 145.20)
              (low . 145.05)
              (close . 145.18)
              (volume . 98))
             ((timestamp . 1709234560)
              (open . 145.18)
              (high . 145.24)
              (low . 145.12)
              (close . 145.21)
              (volume . 120))))))
```

**QUERY (Response)**:
```
((type . "PATTERN_SIMILARITY_RESULT")
 (schema_version . 1)
 (status . "ok")
 (result . ((p_up . 0.62)
            (p_down . 0.21)
            (p_flat . 0.17)
            (top_k . (((id . "H1:USDJPY:1700000000")
                       (distance . 0.12)
                       (label . "UP"))
                      ((id . "H1:USDJPY:1690000000")
                       (distance . 0.15)
                       (label . "FLAT"))))))))
```

**Error (Response)**:
```
((type . "PATTERN_SIMILARITY_RESULT")
 (schema_version . 1)
 (status . "error")
 (error . "message"))
```

**Notes**:
- `candles` はTFごとの **window_bars** と一致する長さを要求する。  
- ペイロードサイズ上限は **デフォルト 2MB（設定で変更可）**（超過時は `error`）。  

## エラーとタイムアウト
- **タイムアウト**:
  - Heartbeat: 300秒 (Dead Man's Switch)
  - Connect: ノンブロッキング(`ZMQ_DONTWAIT`)推奨
- **エラー処理**:
  - 受信側でS式パースエラーが発生した場合、単にログ出力して無視する（クラッシュさせない）。
  - 数値フィールドは `string` で来る可能性も考慮し、パース時に安全策をとる。

## 互換性ルール
- 新しいフィールドの追加はよしとする（受信側で無視）。
- 既存フィールドの削除・型変更は破壊的変更とみなす。
- `type` フィールドは必須。
- 補助サービス（Data Keeper / Notifier / Risk Gateway）は `schema_version` を必須とする（現在 1）。
