# Notification Test Plan
Created: 2026-01-10

## 1. Environment Variable & Routing Map

| Function | Env Var Key | Intended Channel | Description |
|---|---|---|---|
| `notify-apex` | `SWIMMY_DISCORD_APEX` | **Apex (General)** | System startup, shutdown, errors, tools. |
| `notify-discord` | `SWIMMY_DISCORD_WEBHOOK` | **Apex (General)** | Default fallback (formerly USDJPY). |
| `send-discord-heartbeat` | `SWIMMY_DISCORD_HEARTBEAT`| **Apex (General)** | Periodic "Alive" signals. |
| `notify-discord-daily` | `SWIMMY_DISCORD_DAILY` | **Daily Reports** | Daily PnL and Goal progress. |
| `notify-discord-weekly` | `SWIMMY_DISCORD_WEEKLY` | **Weekly Reports** | Weekly summary. |
| `notify-discord-alert` | `SWIMMY_DISCORD_ALERTS` | **Alerts** | Generic alerts (SL/TP touches, etc). |
| `notify-discord-emergency`| `SWIMMY_DISCORD_EMERGENCY`| **Alerts (Critical)**| Circuit breakers, crashes. |
| `notify-backtest-summary` | `SWIMMY_DISCORD_BACKTEST` | **Backtest** | Research and validation results. |
| `notify-discord-recruit` | `SWIMMY_DISCORD_RECRUIT` | **Recruitment** | New strategy evolution/promotion. |
| `notify-discord-journal` | `SWIMMY_DISCORD_JOURNAL` | **Journal** | AI thoughts, internal monologue. |

## 2. Symbol Specific Routing

| Symbol | Env Var Key | Intended Channel |
|---|---|---|
| USDJPY | `SWIMMY_DISCORD_WEBHOOK_USDJPY` | **USDJPY** |
| EURUSD | `SWIMMY_DISCORD_WEBHOOK_EURUSD` | **EURUSD** |
| GBPUSD | `SWIMMY_DISCORD_WEBHOOK_GBPUSD` | **GBPUSD** |

## 3. Test Procedure

Run the test script:
```bash
sbcl --script tools/test_all_notifications.lisp
```

## 4. Expected Results
- **Apex Channel**: Should receive "System Test", "Default Fallback", "Heartbeat Test".
- **Daily Channel**: Should receive "Daily Report Test".
- **Weekly Channel**: Should receive "Weekly Report Test".
- **Alerts Channel**: Should receive "Alert Test" and "EMERGENCY TEST".
- **Backtest Channel**: Should receive "Backtest Test".
- **Recruit Channel**: Should receive "Recruit Test".
- **Journal Channel**: Should receive "Journal Test".
- **USDJPY Channel**: Should receive "USDJPY Signal Test".
- **EURUSD Channel**: Should receive "EURUSD Signal Test".
- **GBPUSD Channel**: Should receive "GBPUSD Signal Test".
