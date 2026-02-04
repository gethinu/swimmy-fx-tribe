# Interface Definitions (ZeroMQ)

## 通信方式
- **Transport**: TCP
- **Encoding**: 原則S-expression（alist）。ただし補助サービス境界（Data Keeper / Risk Gateway / Notifier）は現在JSONを使用（各ポート定義を正とする）。
- **Key/文字**: キーはASCII小文字、文字列はUTF-8想定。
- **Ports**:
  - `5557`: Market Data (MT5 PUB -> Rust SUB)
  - `5560`: Execution (Rust PUB -> MT5 SUB)
  - `5555`: Sensory Input (Rust PUSH -> Lisp PULL)
  - `5556`: Motor Output (Lisp PUB -> Rust SUB)
  - `5559`: External Command (Legacy) -> Rust SUB
  - `5561`: Data Keeper (REQ/REP, JSON) -> Data Keeper Service
  - `5562`: Notifications (Rust PUSH -> Notifier Service)
  - `5563`: Risk Gateway (REQ/REP, JSON) -> Risk Gateway Service
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
 (magic . 123456))
```

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
```
((type . "BACKTEST_RESULT")
 (result . ((strategy_name . "Volvo-Scalp-Gen0")
            (sharpe . 0.85)
            (trades . 120)
            (profit_factor . 1.6)
            (win_rate . 0.52)
            (max_dd . 0.12))))
```

**BACKTEST_RESULT (Error, S-Expression only / JSON禁止)**:
```
((type . "BACKTEST_RESULT")
 (result . ((strategy_name . "Volvo-Scalp-Gen0")
            (sharpe . 0.0)
            (error . "S-Exp parse error: ..."))))
```

**備考**: 以前のJSON要求形式は廃止（後方互換なし）。Backtest Service は **S式のみ**受理/返却する。  

### 6. Data Keeper Service (Port 5561)
ヒストリカルデータの参照/保存を担当する補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **JSON**（現状の正本は `tools/data_keeper.py`）。

- Request: `STATUS`
- Request: `GET_HISTORY:SYMBOL:[TIMEFRAME:]COUNT`
- Request: `GET_FILE_PATH:SYMBOL:TF`
- Request: `ADD_CANDLE:SYMBOL:[TIMEFRAME:]JSON`
- Request: `SAVE_ALL`

### 7. Risk Gateway Service (Port 5563)
取引許可の判定を行う補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **JSON**（現状の正本は `tools/risk_gateway.py` / `src/lisp/core/risk-client.lisp`）。

- Request: `CHECK_RISK:{...json...}`
- Response: `{"status":"APPROVED"|"DENIED","reason":"..."}`

### 6. Data Keeper Service (Port 5561)
ヒストリカルデータの参照/保存を担当する補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **JSON**（現状の正本は `tools/data_keeper.py`）。

- Request: `STATUS`
- Request: `GET_HISTORY:SYMBOL:[TIMEFRAME:]COUNT`
- Request: `GET_FILE_PATH:SYMBOL:TF`
- Request: `ADD_CANDLE:SYMBOL:[TIMEFRAME:]JSON`
- Request: `SAVE_ALL`

### 7. Risk Gateway Service (Port 5563)
取引許可の判定を行う補助サービス（Python）。  
**プロトコル**: ZMQ **REQ/REP** + **JSON**（現状の正本は `tools/risk_gateway.py` / `src/lisp/core/risk-client.lisp`）。

- Request: `CHECK_RISK:{...json...}`
- Response: `{"status":"APPROVED"|"DENIED","reason":"..."}`

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
