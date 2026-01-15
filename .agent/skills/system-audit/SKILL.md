---
name: system-audit
description: Audits the Swimmy system status (ports, services, logs) to ensure health.
---

# System Audit Skill

This skill performs a comprehensive health check of the Swimmy trading system. It is useful for diagnosing connectivity issues, service failures, or configuration drift.

## Included Checks

### 1. Port Audit
Verifies that all required ZMQ ports are listening and that no deprecated ports are active.
- **Required**: 5555 (Brain Pull), 5556 (Brain Pub), 5557 (Guardian Sub), 5559 (Guardian Cmd)
- **Forbidden**: 5580, 5558 (Legacy)

### 2. Service Audit
Checks the status of systemd services and process existence.
- `swimmy-brain`
- `swimmy-guardian`
- `data_keeper`

### 3. Log Audit
Scans recent logs for keywords like "ERROR", "FATAL", "panic", "Address already in use".

## Usage

Run the following scripts from the `scripts/` directory:

```bash
./.agent/skills/system-audit/scripts/audit_ports.sh
./.agent/skills/system-audit/scripts/check_services.sh
```

## Troubleshooting

- If **Port Audit** fails:
  - Check `ss -tulnpe` to identify conflicting processes.
  - Restart services: `systemctl --user restart swimmy-brain swimmy-guardian`
  
- If **Service Audit** fails:
  - Check logs: `journalctl --user -u <service_name> -n 50`
