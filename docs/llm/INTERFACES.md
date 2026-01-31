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
