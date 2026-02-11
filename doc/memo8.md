reports
アプリ
 — 22:472026年2月10日 火曜日 22:47
 Evolution Factory Report
 Evolution Factory Report
Current status of the autonomous strategy generation pipeline.

 Knowledge Base (Active)
61128 Strategies

 S-Rank (Verified Elite)
0 (IS Sharpe≥0.5 + CPCV median PF/WR/MaxDD + pass_rate)

 A-Rank (Pro)
7 (Sharpe≥0.3 PF≥1.2 WR≥40% MaxDD<20% + OOS)

 B-Rank (Selection)
491 (Sharpe≥0.1 PF≥1.0 WR≥30% MaxDD<30%)

 New Recruits (24h)
26

 Graveyard
136784

 Retired
960

 CPCV Status
0 queued | 0 sent | 16 received | 16 failed | last start: N/A
CPCV Gate Failures: sharpe<0.5=0 pf<1.5=7 wr<0.45=5 maxdd>=0.15=7 elite=7 total=7
CPCV Median Failures: pf<1.5=0 wr<0.45=0 maxdd>=0.15=0 total=0

OOS sent: 0 retry: 0 success: 115 failure: 0 pending: 0 oldest: - (data 0 send 0 db 0) latency(avg/min/max): 0.00/-/- sec


 Top Candidates:
Bred-Bred--798-Gen408 (S=2.16, NIL),
Bred-Bred--881-Gen559 (S=2.16, GRAVEYARD),
Bred-Bred--817-Gen425 (S=2.14, RETIRED),
Bred-Bred--499-Gen389 (S=2.14, GRAVEYARD),
Bred-Bred--930-Gen398 (S=2.11, GRAVEYARD),


 System Status
 Evolution Daemon Active
 Native Lisp Orchestration (V28)
02/10 22:47 JST / 13:47 UTC

---

## 2026-02-11: 「Brain Silence」で止まった件の引き継ぎ

### 症状
- Guardian が `💀 CRITICAL: Brain Silence Detected (>120s)` を連発し、EMERGENCY MODE に入る
- pipeline が止まったように見える（新規エントリー/処理がブロックされる）
- その後、Guardian の auto-revival が `systemctl --user` 経由だと `Failed to connect to bus: No medium found` で失敗することがある

### 原因（確定）
- Guardian の「Brain silence」は **Brain→Guardian の Port 5556** の受信が止まると発火する
- Brain が 5556 に定期送信（Heartbeat）していないと、Brain が生きていても **誤検知** になる

### 対応（実装）
- Brain の periodic maintenance から `swimmy.executor:send-heartbeat` を呼び出し、Port 5556 に `HEARTBEAT` を定期送信
- Heartbeat のスロットルを約 **10秒** に（Guardian timeout 120秒の十分手前で更新される）
- Watchdog は `systemctl restart` が失敗（polkit等）した場合に備え、`systemctl show -p MainPID` → `kill` のフォールバックを追加済み

### すぐ確認する（Port 5556のHEARTBEAT）
```bash
/home/swimmy/swimmy/.venv/bin/python3 - <<'PY'
import time
import zmq

ctx = zmq.Context.instance()
s = ctx.socket(zmq.SUB)
s.setsockopt(zmq.SUBSCRIBE, b"")
s.connect("tcp://127.0.0.1:5556")

print("listening on tcp://127.0.0.1:5556 for 20s...")
end = time.time() + 20
seen = 0
while time.time() < end:
    try:
        msg = s.recv_string(flags=zmq.NOBLOCK)
    except zmq.Again:
        time.sleep(0.1)
        continue
    seen += 1
    if "HEARTBEAT" in msg:
        print(msg)
print("done; total messages:", seen)
PY
```

### また起きた場合の対処（最短）
```bash
# 1) GuardianがBrain Silenceを言ってるか
journalctl -u swimmy-guardian -n 300 --no-pager | rg "Brain Silence|CRITICAL"

# 2) BrainがPort 5556でHEARTBEAT出してるか（上のpython）

# 3) Brainが詰まってそうなら（sudo不要で）MainPIDを落としてsystemdのRestart=に任せる
pid=$(systemctl show -p MainPID --value swimmy-brain); [ "${pid:-0}" -gt 0 ] && kill -TERM "$pid"
```

### 2026-02-11 12:29 JST 復帰確認
- `systemctl --user stop swimmy-guardian` 後、`sudo systemctl start swimmy-guardian` で **system unit** を復帰
- `systemctl status swimmy-guardian` は `active (running)`（PID: 21395）
- `systemctl --user status swimmy-guardian` は `inactive (dead)`（二重起動なし）
- Port `5557/5559/5560` は PID 21395 が listen

### 2026-02-11 追加ハードニング（Option 2）
- `systemd/swimmy-guardian.service` を `Restart=always` に変更
- `StartLimitIntervalSec=300` / `StartLimitBurst=5` を追加
- 反映手順（要 sudo）:
```bash
cd /home/swimmy/swimmy
sudo install -m 0644 systemd/swimmy-guardian.service /etc/systemd/system/swimmy-guardian.service
sudo systemctl daemon-reload
sudo systemctl restart swimmy-guardian
systemctl show swimmy-guardian -p Restart -p StartLimitIntervalUSec -p StartLimitBurst
```

### 2026-02-11 12:18 JST 実測結果（手順実行）
- Port 5556 の 20秒監視で `HEARTBEAT` を受信（`total messages: 1`）。
- Port 5556 の 65秒監視で `HEARTBEAT` 5件を受信。
  - gap: min `10.69s` / max `13.55s` / avg `12.16s`
- `.opus/live_status.sexp` は `total_trades = 0`（2026-02-11 12:18:19 JST 時点）。
- `data/memory/swimmy.db` の `trade_logs`, `backtest_trade_logs`, `strategy_daily_pnl` はすべて `0` 件。

### 判定（2026-02-11 12:18 JST 時点）
- **約定は確認できず**（新規トレードが入った証跡なし）。
- ただし `logs/guardian.log` には `Brain Silence -> EMERGENCY MODE -> EMERGENCY CLOSE -> restored` の痕跡が残っており、断続的な再発リスクは継続。

### 2026-02-11 12:37-12:38 JST 再確認（Option 2 選択後）
- `tools/update_history_smart.py` が実行中の間は、設計どおり一時的に Port `5557/5560` を Python 側で bind（Guardian を止めて履歴更新する用途）。
- 同プロセス終了後、`swimmy-guardian` は `12:37:50 JST` に systemd で自動復帰（`active (running)`、PID: `28197`）。
- ただし `/etc/systemd/system/swimmy-guardian.service` はまだ旧設定のまま（`Restart=on-failure`、`StartLimitIntervalUSec=10s`）。
- つまり **Option 2 のコード変更は完了済みだが、OS反映は未完了**。

### 残タスク（要 sudo・これで完了）
```bash
cd /home/swimmy/swimmy
sudo install -m 0644 systemd/swimmy-guardian.service /etc/systemd/system/swimmy-guardian.service
sudo systemctl daemon-reload
sudo systemctl restart swimmy-guardian
systemctl show swimmy-guardian -p Restart -p StartLimitIntervalUSec -p StartLimitBurst
```
期待値:
- `Restart=always`
- `StartLimitIntervalUSec=5min`（または `300s` 相当）
- `StartLimitBurst=5`

### 2026-02-11 12:41 JST Option 2 反映完了
- 実行結果:
  - `Restart=always`
  - `StartLimitIntervalUSec=5min`
  - `StartLimitBurst=5`
- `systemctl status swimmy-guardian` も `active (running)` を確認（PID: `31026`）。
- **Option 2 の残タスクは解消済み**。
