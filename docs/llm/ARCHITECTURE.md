# System Architecture

## 全体データフロー
```mermaid
graph TD
    Market[Forex Market] <-->|Order/Feed| MT5[MT5 Terminal / SwimmyBridge]
    
    subgraph Windows Host
        MT5
    end
    
    subgraph WSL2 Container
        direction TB
        Rust[Rust Guardian]
        Lisp[Lisp Brain / School]
        DataKeeper[Python Data Keeper]
        DB[(SQLite / Files)]
    end

    MT5 -- "PUB :5557 (Tick/Account)" --> Rust
    Rust -- "PUB :5560 (Order/Cmd)" --> MT5
    
    Rust -- "PUSH :5555 (Sensor Data)" --> Lisp
    Lisp -- "PUB :5556 (Decision/Signal)" --> Rust
    
    Rust -- "PUSH :5561 (History)" --> DataKeeper
    Lisp -- "PUSH :5561 (Log)" --> DataKeeper
    
    Lisp <--> DB
    Rust <--> DB
```

**通信エンコーディング方針**: 内部ZMQはS-expression（alist形式）に統一。外部API境界（Discord/HTTP等）はJSONを維持する。

## コンポーネント詳細

### 1. MT5 (Limbs)
- **SwimmyBridge.mq5 (Ver 15.2)**
- 市場監視・注文執行。Dead Man's Switch (300s) 搭載。
- Multi-TF対応。

### 2. Rust Guardian (Spinal Cord)
- **Guardian (Port 5557/5560/5555/5556)**
- **Risk Gate**: 注文の拒否権を持つ (Veto)。
- **Reflex**: 急変動時の自律反応。
- **Audit**: `guardian_audit.jsonl` に記録。

### 3. Lisp Brain/School (Cortex)
- **Pure Lisp Daemon (Systemd)**
- **School**: 進化 (Breeding), 淘汰 (Culling), ランク付 (B/A/S)。
- **Brain**: シグナル生成、Atomic Allocation (スロット確保)。
- **Scribe**: I/O分離用ワーカー。
- **Hot Reload**: プロセスを止めずにロジック更新可能。

### 4. Data Keeper (Memory)
- **Python Service (Port 5561)**
- 10M Candle Buffer。ヒストリカルデータの非同期保存を担当。
- メインプロセスのI/O負荷を軽減。

## 実行タイミング
| 頻度 | 主体 | 処理内容 |
| :--- | :--- | :--- |
| **High Freq (Tick)** | MT5 | 価格更新、Rustへ送信 |
| **Realtime** | Rust | Risk Gate、Reflex |
| **Event** | Lisp | シグナル生成、Atomic Allocation |
| **Hourly** | Lisp | 学習、ファイルローテーション、**Evolution Report** |
| **Weekly (Sat)** | Guardian | Weekend Auto-Close (全決済) |
| **Weekly (Sat)** | Lisp | Culling (Rank C/D 排除) |

## Reports & Metrics (V50.5.1 Mechanism)
- **Evolution Factory Report**: `school-narrative.lisp` で生成。
  - **Trigger**: Scheduler (`start-evolution-service`) or Manual.
  - **Sync**: 生成直前に `refresh-strategy-metrics-from-db :force t` を実行し、SQLiteとInMemory状態を強制同期する。
  - **Persistence**: 戦略のメトリクス（Sharpe, Rank等）は、バックテストやランク更新のたびに `upsert-strategy` でDBに即時保存される（べきである）。

## フェイルセーフ & 監視
- **Watchdog**: Lisp内に `school-watchdog` (Broken Arrow) 実装。100msフリーズを検知。
- **Systemd**: コア4サービス（`swimmy-brain`, `swimmy-guardian`, `swimmy-school`, `swimmy-data-keeper`）＋補助（`swimmy-backtest`, `swimmy-risk`, `swimmy-notifier`, `swimmy-evolution`, `swimmy-watchdog`）を自動再起動。
- **DNA Verify**: 起動時のファイル改ざん検知 (SHA256)。
