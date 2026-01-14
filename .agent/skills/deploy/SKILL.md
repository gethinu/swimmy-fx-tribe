---
name: deploy
description: Safely deploys the system using systemd (restart Brain/Guardian).
---

# Deploy Skill

Updates the running services with the latest code changes.

## When to use
- After commiting changes (via `quality-check`).
- When the user asks to "deploy" or "restart".
- After fixing a bug (e.g. "Ghost Positions").

## Steps

### 1. Verification
Run quality checks if not already done. (Skip if just done)

```bash
cd /home/swimmy/swimmy && make quality-gate
```

### 2. Restart Services
Restart the systemd user services. Both Brain and Guardian should be restarted to ensure clean state.

```bash
systemctl --user restart swimmy-brain
systemctl --user restart swimmy-guardian
```

### 3. Verify Status
Check if services are running correctly. Active state should be `active (running)`.

```bash
systemctl --user status swimmy-brain --no-pager
systemctl --user status swimmy-guardian --no-pager
```

### 4. Log Check
Ensure no immediate startup errors. Look for "Initialization complete" or similar success messages.

```bash
journalctl --user -u swimmy-brain -n 50 --no-pager
```
