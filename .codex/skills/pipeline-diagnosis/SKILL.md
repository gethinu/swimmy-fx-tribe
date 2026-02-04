---
name: pipeline-diagnosis
description: Use when diagnosing Swimmy evolution/backtest pipeline health, OOS/CPCV queue stalls, backtest_status counts not increasing, S/A ranks stuck at zero, or when systemd status conflicts with running processes/ports.
---

# Pipeline Diagnosis

## Overview
Diagnose Swimmy's evolution/backtest pipeline by collecting evidence from systemd, processes, ports, reports, DB, and logs before drawing conclusions or proposing fixes.

## When to Use
- OOS queue pending grows or oldest age climbs
- backtest_status count stops increasing
- S/A ranks are zero while B keeps growing
- systemd reports inactive but ports/processes are active
- need a concise health snapshot before escalation

## Workflow (Evidence First)
1. Check systemd state (system + user).
```bash
sudo systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
systemctl --user status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog
```
2. Confirm processes and ports (avoid double-start/port conflicts).
```bash
ps aux | rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"
ss -tulnp | rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"
```
3. Read pipeline reports.
```bash
cd /home/swimmy/swimmy
cat data/reports/backtest_status.txt 2>/dev/null || echo "File not found"
cat data/reports/oos_status.txt 2>/dev/null || echo "File not found"
cat data/reports/evolution_factory_report.txt 2>/dev/null | head -40
```
4. Check DB truth (oos_queue + rank summary).
```bash
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('/home/swimmy/swimmy/data/memory/swimmy.db')
cur = conn.cursor()
print('oos_queue:')
cur.execute('SELECT status, COUNT(*) FROM oos_queue GROUP BY status ORDER BY status')
for r in cur.fetchall():
    print(r)
print('\nrank summary:')
cur.execute('SELECT rank, COUNT(*) FROM strategies GROUP BY rank ORDER BY rank')
for r in cur.fetchall():
    print(r)
PY
```
5. Inspect logs for OOS/backtest results and errors.
```bash
journalctl --user -u swimmy-brain -n 100 --no-pager
journalctl --user -u swimmy-school -n 100 --no-pager
rg -n "OOS|BACKTEST_RESULT|ERROR" /home/swimmy/swimmy/logs/backtest.log | tail -40
rg -n "OOS|BACKTEST_RESULT|ERROR" /home/swimmy/swimmy/logs/guardian.log | tail -40
```
6. Interpret using the guide below, then summarize evidence and next checks (no fixes yet).

## Interpretation Guide
- oos_status pending grows and DB shows mostly `sent` with no `success/failure`: results not returning or not being applied.
- backtest_status count stagnant while backtest service is up: inspect backtest/guardian logs for parsing or pipe errors.
- systemd inactive but ports/processes active: treat as running; do not restart without stopping existing processes.
- S/A ranks zero with active backtest flow: OOS/CPCV pipeline is not advancing even if BT is working.

## Quick Reference
| Check | Command |
| --- | --- |
| systemd (system) | `sudo systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog` |
| systemd (user) | `systemctl --user status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog` |
| processes | `ps aux \| rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"` |
| ports | `ss -tulnp \| rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"` |
| reports | `cat data/reports/backtest_status.txt ; cat data/reports/oos_status.txt ; cat data/reports/evolution_factory_report.txt \| head -40` |
| oos_queue | `python3 - <<'PY' ...` |

## Example
Scenario: OOS pending is 30+ and S/A ranks are zero.
```bash
cd /home/swimmy/swimmy
cat data/reports/oos_status.txt
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db')
cur = conn.cursor()
cur.execute('SELECT status, COUNT(*) FROM oos_queue GROUP BY status ORDER BY status')
print(cur.fetchall())
PY
rg -n "OOS|BACKTEST_RESULT" logs/backtest.log | tail -20
```
Summarize: pending count, oldest age, DB status distribution, whether results are returning in logs.

## Common Mistakes
- Relying only on systemd status and ignoring ports/processes
- Treating report files as ground truth without checking DB
- Proposing restarts or fixes before collecting evidence

## Rationalization Table
| Excuse | Reality |
| --- | --- |
| "systemd says inactive, so restart" | Ports/processes may already be active; restart can cause port conflicts. |
| "oos_status is enough" | DB `oos_queue` and logs reveal whether results are returning or applied. |
| "Backtest improved because of X" | Verify with backtest_status counts and logs before attributing cause. |

## Red Flags - STOP and Re-check
- Skipping DB (`oos_queue`, rank summary)
- Ignoring ports/processes when systemd is inactive
- Drawing conclusions before checking logs
- Restarting services without confirming existing listeners
