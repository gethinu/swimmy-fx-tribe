---
name: pipeline-diagnosis
description: Use when diagnosing Swimmy evolution/backtest pipeline health when reports, DB, logs, and systemd disagree (for example units missing but ports active), OOS/CPCV queues stall, or backtest_status movement is unclear.
---

# Pipeline Diagnosis

## Overview
Diagnose Swimmy's evolution/backtest pipeline by collecting runtime evidence first (processes, ports, reports, DB, logs), then interpreting mismatches. Do not propose fixes until evidence is summarized.

## When to Use
- OOS queue pending grows or oldest age climbs
- backtest_status count stops increasing
- S/A ranks are zero while B keeps growing
- systemd says inactive or unit-not-found while ports/processes are active
- journalctl has no entries even though services are clearly running
- backtest.log appears stale while reports still move
- need a concise health snapshot before escalation

## Workflow (Evidence First)
1. Check systemd visibility (system first, then user), but do not block on sudo.
```bash
systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog --no-pager || true
sudo -n systemctl status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog --no-pager || echo "sudo unavailable or unit missing"
systemctl --user status swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-backtest swimmy-risk swimmy-notifier swimmy-evolution swimmy-watchdog --no-pager || true
systemctl --user list-unit-files 'swimmy*' --no-pager 2>/dev/null || true
```
2. Confirm runtime truth from processes and ports (avoid double-start/port conflicts).
```bash
ps aux | rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"
ss -tulnp | rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"
for p in $(pgrep -f "backtest_service.py|data_keeper.py|notifier.py|risk_gateway.py|brain.lisp|school-daemon.lisp|guardian/target/release/guardian" | head -n 10); do
  echo "== PID $p =="; cat /proc/$p/cgroup; done
```
3. Read pipeline reports and verify backtest count moves between two reads.
```bash
cd /home/swimmy/swimmy
cat data/reports/backtest_status.txt 2>/dev/null || echo "File not found"
sleep 2
cat data/reports/backtest_status.txt 2>/dev/null || echo "File not found"
cat data/reports/oos_status.txt 2>/dev/null || echo "File not found"
cat data/reports/evolution_factory_report.txt 2>/dev/null | head -40
```
4. Check DB truth (`oos_queue` schema + status + ranks).
```bash
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('/home/swimmy/swimmy/data/memory/swimmy.db')
cur = conn.cursor()
print('oos_queue schema:')
cur.execute("PRAGMA table_info(oos_queue)")
for r in cur.fetchall():
    print(r)
print('\noos_queue status:')
cur.execute('SELECT status, COUNT(*) FROM oos_queue GROUP BY status ORDER BY status')
for r in cur.fetchall():
    print(r)
print('\nrecent oos_queue rows:')
cur.execute('SELECT request_id, name, requested_at, status, last_error FROM oos_queue ORDER BY requested_at DESC LIMIT 10')
for r in cur.fetchall():
    print(r)
print('\nrank summary:')
cur.execute('SELECT rank, COUNT(*) FROM strategies GROUP BY rank ORDER BY rank')
for r in cur.fetchall():
    print(r)
PY
```
5. Inspect logs for OOS/CPCV/backtest results and errors.
```bash
journalctl -u swimmy-brain -n 100 --no-pager || true
journalctl -u swimmy-school -n 100 --no-pager || true
journalctl --user -u swimmy-brain -n 100 --no-pager || true
journalctl --user -u swimmy-school -n 100 --no-pager || true
rg -n "OOS|CPCV|BACKTEST_RESULT|ERROR" /home/swimmy/swimmy/logs/guardian.log | tail -40
rg -n "OOS|BACKTEST_RESULT|ERROR" /home/swimmy/swimmy/logs/backtest.log | tail -40
tail -40 /home/swimmy/swimmy/logs/oos_monitor.log
stat -c '%n | size=%s | mtime=%y' /home/swimmy/swimmy/logs/backtest.log /home/swimmy/swimmy/logs/guardian.log
```
6. If logs look stale, verify active log sinks from process file descriptors.
```bash
BACKTEST_PID=$(pgrep -f 'tools/backtest_service.py' | head -1)
echo "backtest pid: $BACKTEST_PID"
readlink -f /proc/$BACKTEST_PID/fd/1
readlink -f /proc/$BACKTEST_PID/fd/2
```
7. Interpret using the guide below, then summarize evidence and next checks (no fixes yet).

## Interpretation Guide
- `systemctl` unit-not-found/inactive but ports/processes active: treat services as running but unmanaged; do not restart blindly.
- process `cgroup` shows `/system.slice/swimmy-*.service`: services are managed by systemd (system scope), even if `systemctl --user` says unit-not-found.
- `journalctl` has no entries while processes run: logging likely not routed via systemd journal; inspect file logs and `/proc/<pid>/fd/*`.
- `journalctl --user` has no entries but `journalctl -u` has data: the service runs in system scope, not user scope.
- `backtest_status` count increases between reads: backtest path is alive even if specific log files look stale.
- `oos_status` pending/retry small (for example 1-2) and oldest low with oscillation `sent -> retry -> empty`: usually transient, not a hard stall.
- `oos_status` pending grows and DB shows mostly `sent` with no `success/failure`: results not returning or not being applied.
- `backtest_status` stagnant while backtest service is up: inspect guardian/backtest logs for parse errors or pipe issues.
- S/A ranks zero with active backtest flow: OOS/CPCV pipeline is not advancing even if BT is working.

## Quick Reference
| Check | Command |
| --- | --- |
| systemd (system, direct) | `systemctl status ... --no-pager || true` |
| systemd (system, non-blocking sudo) | `sudo -n systemctl status ... --no-pager || echo "sudo unavailable or unit missing"` |
| systemd (user) | `systemctl --user status ... || true` |
| unit discovery | `systemctl --user list-unit-files 'swimmy*' --no-pager 2>/dev/null || true` |
| processes | `ps aux \| rg -i "sbcl|guardian|data_keeper|notifier|risk_gateway|backtest_service"` |
| ports | `ss -tulnp \| rg "5555|5556|5557|5559|5560|5561|5562|5563|5580|5581"` |
| cgroup scope | `for p in $(pgrep -f "..."); do cat /proc/$p/cgroup; done` |
| reports | `cat data/reports/backtest_status.txt; sleep 2; cat data/reports/backtest_status.txt; cat data/reports/oos_status.txt; cat data/reports/evolution_factory_report.txt \| head -40` |
| DB | `PRAGMA table_info(oos_queue); SELECT status,count(*)...; SELECT request_id,name,requested_at,status,last_error...; SELECT rank,count(*)...` |
| logs | `journalctl -u ...; journalctl --user -u ...; rg ... guardian.log; rg ... backtest.log; tail oos_monitor.log; stat backtest.log guardian.log` |
| log sink | `BACKTEST_PID=$(pgrep -f 'tools/backtest_service.py' \| head -1); readlink -f /proc/$BACKTEST_PID/fd/1` |

## Example
Scenario: systemd units are missing, but pipeline may still be running.
```bash
cd /home/swimmy/swimmy
systemctl status swimmy-brain swimmy-school --no-pager || true
systemctl --user status swimmy-brain swimmy-school --no-pager || true
ps aux | rg -i "brain.lisp|school-daemon|backtest_service|guardian"
ss -tulnp | rg "5555|5556|5580"
for p in $(pgrep -f "brain.lisp|school-daemon|backtest_service|guardian" | head -n 5); do
  echo "== PID $p =="; cat /proc/$p/cgroup; done
cat data/reports/backtest_status.txt
sleep 2
cat data/reports/backtest_status.txt
python3 - <<'PY'
import sqlite3
conn = sqlite3.connect('data/memory/swimmy.db')
cur = conn.cursor()
cur.execute('SELECT status, COUNT(*) FROM oos_queue GROUP BY status ORDER BY status')
print(cur.fetchall())
PY
rg -n "OOS|CPCV|ERROR" logs/guardian.log | tail -20
```
Summarize: unit visibility, process/port truth, backtest count movement, OOS queue distribution, and active log source.

## Common Mistakes
- Relying only on systemd status and ignoring ports/processes
- Treating report files as ground truth without checking DB
- Assuming `oos_queue` has an `id` column across all schema versions
- Interpreting stale `backtest.log` mtime as a hard stop without checking report movement
- Proposing restarts or fixes before collecting evidence

## Rationalization Table
| Excuse | Reality |
| --- | --- |
| "systemd says inactive, so restart" | Ports/processes may already be active; restart can cause port conflicts. |
| "journalctl has no entries, so it's down" | Process may be running outside journal-managed units; check processes, ports, and file logs. |
| "oos_status is enough" | DB `oos_queue` and logs reveal whether results are returning or applied. |
| "backtest.log is stale, so backtest is dead" | Confirm `backtest_status` count movement and active process listeners first. |
| "Backtest improved because of X" | Verify with backtest_status counts and logs before attributing cause. |

## Red Flags - STOP and Re-check
- Skipping DB (`oos_queue`, rank summary)
- Ignoring ports/processes when systemd is inactive
- Assuming fixed DB schema without checking `PRAGMA table_info`
- Declaring outage from one stale log file without report/DB confirmation
- Drawing conclusions before checking logs
- Restarting services without confirming existing listeners
