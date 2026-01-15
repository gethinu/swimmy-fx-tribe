---
description: Run a full system audit (ports, services, logs)
---

# System Audit Workflow

This workflow runs a full diagnostic on the Swimmy system. Use this when the system behaves strangely or after configuration changes.

1. **Check Service Status**
   Check if all core services are running.
   // turbo
   ```bash
   chmod +x /home/swimmy/swimmy/.agent/skills/system-audit/scripts/check_services.sh
   /home/swimmy/swimmy/.agent/skills/system-audit/scripts/check_services.sh
   ```

2. **Check Network Ports**
   Verify ZMQ port bindings.
   // turbo
   ```bash
   chmod +x /home/swimmy/swimmy/.agent/skills/system-audit/scripts/audit_ports.sh
   /home/swimmy/swimmy/.agent/skills/system-audit/scripts/audit_ports.sh
   ```

3. **Quick Log Scan**
   Check for recent critical errors in Brain and Guardian logs.
   ```bash
   echo "--- BRAIN ERRORS ---"
   grep -i "error" /home/swimmy/swimmy/logs/swimmy.log | tail -n 5
   echo "--- GUARDIAN ERRORS ---"
   grep -i "panic" /home/swimmy/swimmy/logs/guardian.log | tail -n 5
   grep -i "error" /home/swimmy/swimmy/logs/guardian.log | tail -n 5
   ```

4. **Report Generation**
   Based on the output above, create a summary report `system_health_<DATE>.md` if critical issues are found.

5. **Action Plan**
   If issues are found (e.g., dead ports, stopped services), propose immediate fixes like `systemctl restart`.
