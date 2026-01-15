---
description: Deploy Swimmy system to production
---

# Deploy Workflow (Systemd)

This workflow safely updates the live system.

## 1. Pre-Deploy Validation
// turbo
1. Run CI tests to ensure code quality.
   ```bash
   cd /home/swimmy/swimmy && ./ci-test.sh
   ```
   **If this fails, STOP.**

## 2. Backup
2. Backup critical source files.
   ```bash
   mkdir -p /home/swimmy/swimmy/.opus/backup
   cd /home/swimmy/swimmy && cp brain.lisp .opus/backup/brain.lisp.$(date +%Y%m%d_%H%M%S)
   ```

## 3. Restart Services
// turbo
3. Restart Brain and Guardian.
   ```bash
   systemctl --user restart swimmy-brain
   systemctl --user restart swimmy-guardian
   ```

## 4. Verification
4. Check service status.
   ```bash
   systemctl --user status swimmy-brain --no-pager
   systemctl --user status swimmy-guardian --no-pager
   ```
   
5. Check logs for clean startup.
   ```bash
   echo "--- Brain Logs ---"
   journalctl --user -u swimmy-brain -n 20 --no-pager
   ```

## 5. Rollback (Emergency Only)
If deployment fails:
1. Restore backup: `cp .opus/backup/brain.lisp.XXX brain.lisp`
2. Restart again: `systemctl --user restart swimmy-brain`
