---
description: Systematic troubleshooting process for Swimmy ecosystem
---
# Troubleshooting Workflow

This workflow guides the AI through a systematic diagnosis of system issues.

## Steps

### 1. Identify the Symptom
- **What is broken?** (e.g., "Market Data missing", "Orders rejected", "Daemon crashed")
- **Where is it broken?** (Lisp Brain, Rust Guardian, Python Daemon, Systemd)

### 2. Run System Audit
// turbo
```bash
/home/swimmy/swimmy/.agent/skills/system-audit/scripts/check_services.sh
```

### 3. Check Logs (Targeted)
Scan logs for the specific affected component.
- **Brain**: `journalctl --user -u swimmy-brain -n 50`
- **School**: `journalctl --user -u swimmy-school -n 50`
- **Guardian**: `journalctl --user -u swimmy-guardian -n 50`

### 4. Consult Knowledge Base
Reference `.agent/skills/troubleshoot/SKILL.md` for known solutions.
```bash
cat /home/swimmy/swimmy/.agent/skills/troubleshoot/SKILL.md
```

### 5. Formulate Hypothesis & Test
- "If I restart service X, it should fix Y."
- "If I clean cache Z, it should fix A."

### 6. Execute Fix
Run the necessary commands (e.g., `systemctl restart ...`).

### 7. Verify Resolution
Re-run Audit or check logs to confirm stability.
