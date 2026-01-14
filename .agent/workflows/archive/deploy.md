---
description: Deploy Swimmy system to production
---

# Deploy Swimmy (Safe Deploy v2.0)

This workflow deploys the Swimmy trading system with backup and rollback.

## Pre-Deploy Checks
1. Run all tests first:
```bash
cd /home/swimmy/swimmy && ./ci-test.sh
```

If tests fail, **STOP HERE**. Do not deploy.

## Backup Current State
// turbo
2. Create backup of current brain.lisp:
```bash
cd /home/swimmy/swimmy && cp brain.lisp .opus/backup/brain.lisp.$(date +%Y%m%d_%H%M%S)
```

// turbo
3. Ensure backup directory exists:
```bash
mkdir -p /home/swimmy/swimmy/.opus/backup
```

4. Save current PID for rollback:
```bash
cd /home/swimmy/swimmy && screen -ls | grep brain | awk '{print $1}' > .opus/backup/last_brain_pid.txt
```

## Deploy Steps
5. Stop existing screen session if running:
```bash
screen -S brain -X quit 2>/dev/null || true
```

6. Wait for clean shutdown:
```bash
sleep 2
```

// turbo
7. Start new brain session:
```bash
cd /home/swimmy/swimmy && screen -dmS brain sbcl --load brain.lisp
```

8. Wait for startup:
```bash
sleep 5
```

## Post-Deploy Verification
9. Verify brain is running:
```bash
screen -ls | grep brain
```

If not running, proceed to **ROLLBACK** section.

10. Check for startup errors in log:
```bash
screen -S brain -X hardcopy /tmp/brain_status.txt && cat /tmp/brain_status.txt | tail -20
```

## Success Confirmation
11. If all checks pass:
```bash
echo "✅ Deploy successful at $(date)" >> /home/swimmy/swimmy/.opus/deploy_history.log
```

---

## ROLLBACK (If Deploy Failed)

If the deployment failed, run these steps:

R1. Stop the broken session:
```bash
screen -S brain -X quit 2>/dev/null || true
```

R2. Find latest backup:
```bash
ls -la /home/swimmy/swimmy/.opus/backup/ | tail -5
```

R3. Restore from backup (replace BACKUP_FILE with actual filename):
```bash
cp /home/swimmy/swimmy/.opus/backup/BACKUP_FILE /home/swimmy/swimmy/brain.lisp
```

R4. Start with restored version:
```bash
cd /home/swimmy/swimmy && screen -dmS brain sbcl --load brain.lisp
```

R5. Log the rollback:
```bash
echo "⚠️ Rollback performed at $(date)" >> /home/swimmy/swimmy/.opus/deploy_history.log
```
