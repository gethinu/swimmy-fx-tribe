# Interface Definitions (ZeroMQ)

## 通信方式
- **Transport**: TCP
- **Encoding**: S-expression（内部ZMQ）。外部API境界のみJSON。
- **Ports**:
  - `5557`: Market Data (MT5 PUB -> Rust SUB)
  - `5560`: Execution (Rust PUB -> MT5 SUB)
  - `5555`: Sensory Input (Rust PUSH -> Lisp PULL)
  - `5556`: Motor Output (Lisp PUB -> Rust SUB)
  - `5559`: External Command (Legacy) -> Rust SUB
  - `5562`: Notifications (Rust PUSH -> Notifier Service)
  - `5580`: Backtest Commands (Lisp PUSH -> Backtest Service PULL)
  - `5581`: Backtest Results (Backtest Service PUSH -> Lisp PULL)

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

### 2. Execution / Commands (Port 5560)
RustからMT5へ送信される。

**ORDER_OPEN**:
```
((type . "ORDER_OPEN")
 (id . "uuid-v4")
 (action . "BUY")
 (symbol . "USDJPY")
 (lot . 0.1)
 (sl . 144.500)
 (tp . 146.500)
 (magic . 123456)
 (comment . "StrategyName"))
```

**CLOSE**:
```
((type . "CLOSE")
 (close_all . false)
 (magic . 123456)
 (instrument . "USDJPY"))
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
 (start_time . 1293840000)        ; optional (Unix seconds)
 (end_time . 1700000000)          ; optional (Unix seconds)
 (data_id . "USDJPY_M1")          ; CSVを使う場合
 (candles_file . "data/historical/USDJPY_M1.csv")
 (aux_candles . ((t . 1)))        ; optional
 (aux_candles_files . ("a.csv"))  ; optional
 (swap_history . ((t . 1) (sl . 0.1) (ss . 0.1))) ; optional
 (symbol . "USDJPY")
 (timeframe . 1))
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

**備考**: 以前のJSON要求形式は廃止（後方互換なし）。Brain/School・Backtest ServiceともS式で送受信する。

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
