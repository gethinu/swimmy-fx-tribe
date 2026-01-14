---
description: Create a new resilient service (Article 5 compliant)
---

# New Service Creation Workflow (Article 5 Compliant)

This workflow ensures all new services comply with Constitutional Article 5 (Service Resilience).

## When to Use
- Creating a new Python tool in `tools/`
- Creating a new background service
- Creating any independent process that runs continuously

## Pre-flight Checklist
Before starting, confirm:
- [ ] This is an independent service (not a library/module)
- [ ] It will run as a background process
- [ ] It needs to survive crashes

---

## Steps

### 1. Create the service script

// turbo
```bash
# Create from template
cp /home/swimmy/swimmy/tools/mt5_account_sync.py /home/swimmy/swimmy/tools/NEW_SERVICE_NAME.py
```

### 2. Required Code Structure (Article 5 Compliance)

Ensure your service has ALL of these:

```python
# === REQUIRED IMPORTS ===
import time
import sys
try:
    import requests
    HAS_REQUESTS = True
except ImportError:
    HAS_REQUESTS = False

# === REQUIRED CONSTANTS (Section 5.2) ===
MAX_CONSECUTIVE_FAILURES = 5
APEX_WEBHOOK = "https://discord.com/api/webhooks/1458820892623634686/..."

# === REQUIRED FUNCTION (Section 5.2, 5.3) ===
def send_discord_alert(message: str, is_error: bool = True):
    if not HAS_REQUESTS:
        print(f"[ALERT] {message}")
        return
    try:
        color = 15158332 if is_error else 3066993
        payload = {"embeds": [{"title": "🐟 SERVICE_NAME", "description": message, "color": color}]}
        requests.post(APEX_WEBHOOK, json=payload, timeout=5)
    except Exception as e:
        print(f"[ALERT FAILED] {e}")

# === REQUIRED MAIN LOOP STRUCTURE (Section 5.4) ===
def main():
    # Startup notification (Section 5.6)
    send_discord_alert("✅ SERVICE_NAME Started", is_error=False)
    
    consecutive_failures = 0
    alert_sent = False
    
    try:
        while True:
            try:
                # === YOUR MAIN LOGIC HERE ===
                
                # Reset on success (Section 5.3)
                if consecutive_failures > 0 and alert_sent:
                    send_discord_alert("✅ SERVICE_NAME Recovered", is_error=False)
                consecutive_failures = 0
                alert_sent = False
                
            except Exception as e:
                consecutive_failures += 1
                print(f"❌ Error: {e}")
                
                # Alert after threshold (Section 5.2)
                if consecutive_failures >= MAX_CONSECUTIVE_FAILURES and not alert_sent:
                    send_discord_alert(f"🚨 SERVICE_NAME Error: {e}")
                    alert_sent = True
                
                time.sleep(5)  # Backoff
                
    except KeyboardInterrupt:
        send_discord_alert("🛑 SERVICE_NAME Stopped", is_error=False)
    except Exception as e:
        send_discord_alert(f"💥 SERVICE_NAME CRASHED: {e}")
        raise
```

### 3. Create systemd service file (Section 5.1, 5.5)

// turbo
```bash
cat > /home/swimmy/swimmy/systemd/NEW_SERVICE_NAME.service << 'EOF'
[Unit]
Description=Swimmy NEW_SERVICE_NAME
After=network.target

[Service]
Type=simple
User=swimmy
WorkingDirectory=/home/swimmy/swimmy
ExecStart=/usr/bin/python3 /home/swimmy/swimmy/tools/NEW_SERVICE_NAME.py
Restart=always
RestartSec=10
StartLimitIntervalSec=300
StartLimitBurst=5
Environment=PYTHONUNBUFFERED=1
StandardOutput=append:/home/swimmy/swimmy/logs/NEW_SERVICE_NAME.log
StandardError=append:/home/swimmy/swimmy/logs/NEW_SERVICE_NAME.log

[Install]
WantedBy=default.target
EOF
```

### 4. Add to Makefile cleanup (prevents port conflicts)

Edit Makefile `run:` target to include:
```makefile
@-pkill -9 -f "NEW_SERVICE_NAME.py" 2>/dev/null || true
```

### 5. Verify Article 5 Compliance

// turbo
```bash
cd /home/swimmy/swimmy && grep -l "MAX_CONSECUTIVE_FAILURES\|send_discord_alert" tools/NEW_SERVICE_NAME.py && echo "✅ Compliance check passed"
```

---

## Final Checklist (Constitutional Compliance)

- [ ] **5.1 AUTO-RESTART**: systemd `Restart=always` in service file
- [ ] **5.2 CRASH ALERTING**: `MAX_CONSECUTIVE_FAILURES` + `send_discord_alert`
- [ ] **5.3 RECOVERY NOTIFICATION**: Alert on recovery after failure
- [ ] **5.4 EXCEPTION HANDLING**: try/except in main loop
- [ ] **5.5 SYSTEMD COMPATIBILITY**: Service file in `systemd/`
- [ ] **5.6 STARTUP NOTIFICATION**: Send alert on successful start

---

## Notes
- Reference implementation: `tools/mt5_account_sync.py`
- Service file template: `systemd/mt5_account_sync.service`
- All services MUST be added to Makefile cleanup
