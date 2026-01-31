# Interface Definitions (ZeroMQ)

## 通信方式
- **Transport**: TCP
- **Encoding**: JSON (UTF-8, ASCII推奨)
- **Ports**:
  - `5557`: Market Data (MT5 PUB -> Rust SUB)
  - `5560`: Execution (Rust PUB -> MT5 SUB)
  - `5555`: Sensory Input (Rust PUSH -> Lisp PULL)
  - `5556`: Motor Output (Lisp PUB -> Rust SUB)
  - `5559`: External Command (Legacy) -> Rust SUB
  - `5562`: Notifications (Rust PUSH -> Notifier Service)
  - `5580`: Backtest Commands (Lisp PUSH -> Backtest Service PULL)
  - `5581`: Backtest Results (Backtest Service PUSH -> Lisp PULL)

## データスキーマ (JSON)

### 1. Market Data (Port 5557)
MT5からブロードキャストされる。

**TICK**:
```json
{
  "type": "TICK",
  "symbol": "USDJPY",
  "bid": 145.205,
  "ask": 145.218
}
```

**ACCOUNT_INFO**:
```json
{
  "type": "ACCOUNT_INFO",
  "timestamp": 1709234567,
  "equity": 1005000.0,
  "balance": 1000000.0,
  "margin": 50000.0,
  "free_margin": 950000.0,
  "margin_level": 2010.0,
  "profit": 5000.0,
  "leverage": 25
}
```

### 2. Execution / Commands (Port 5560)
RustからMT5へ送信される。

**ORDER_OPEN**:
```json
{
  "type": "ORDER_OPEN",
  "id": "uuid-v4",
  "action": "BUY",  // or SELL
  "symbol": "USDJPY",
  "lot": 0.1,
  "sl": 144.500,
  "tp": 146.500,
  "magic": 123456,
  "comment": "StrategyName"
}
```

**CLOSE**:
```json
{
  "type": "CLOSE",
  "close_all": false,
  "magic": 123456, // 0 or missing for symbol close
  "instrument": "USDJPY"
}
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

### 4. Motor Output (Port 5556)
Lisp -> Rustへ「意図」を送る。
スキーマはExecution用とほぼ同じだが、Risk Gate用のメタデータが含まれる場合がある。

```json
{
  "type": "SIGNAL",
  "strategy": "Trend-Follow-V1",
  "symbol": "USDJPY",
  "action": "BUY",
  "lot": 0.1,
  "sl": 144.50,
  "tp": 146.50,
  "estimated_loss": 2000.0
}
```

### 5. Backtest Service (Ports 5580/5581)
Brainのバックテスト要求を専用サービスへオフロードする。  
**S式プロトコルに統一**（2026-01-31）。Backtest Service側でS式を受け取り、そのままGuardian `--backtest-only` に流し、結果もS式/JSONをそのまま返却する。  
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
 (data_id . "USDJPY_M1")          ; CSVを使う場合
 (candles_file . "data/historical/USDJPY_M1.csv")
 (symbol . "USDJPY")
 (timeframe . 1))
```

**BACKTEST_RESULT (Response, Guardianフォーマットそのまま)**:
- S式またはJSON文字列として返却。Brain側は `type=BACKTEST_RESULT` を検出して処理する。  

**備考**: 以前のJSON要求形式は廃止（後方互換なし）。Brain/School・Backtest ServiceともS式で送受信する。

## エラーとタイムアウト
- **タイムアウト**:
  - Heartbeat: 300秒 (Dead Man's Switch)
  - Connect: ノンブロッキング(`ZMQ_DONTWAIT`)推奨
- **エラー処理**:
  - 受信側でJSONパースエラーが発生した場合、単にログ出力して無視する（クラッシュさせない）。
  - 数値フィールドは `string` で来る可能性も考慮し、パース時に安全策をとる。

## 互換性ルール
- 新しいフィールドの追加はよしとする（受信側で無視）。
- 既存フィールドの削除・型変更は破壊的変更とみなす。
- `type` フィールドは必須。
