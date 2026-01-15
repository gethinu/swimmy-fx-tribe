---
name: deploy
description: Safely deploys the system using systemd (restart Brain/Guardian).
---

# Deploy Skill (Systemd Edition)

This skill safely restarts the Swimmy Brain and Guardian services using `systemctl`. It ensures zero downtime for critical components like Data Keeper where possible, but Brain/Guardian updates require restart.

## Procedure

1. **Pre-Flight Check**
   - Run unit tests (`./ci-test.sh`).

2. **Backup (Optional)**
   - Backup `brain.lisp` if critical code changes were made.

3. **Restart Services**
   - Use `systemctl --user restart swimmy-brain`
   - Use `systemctl --user restart swimmy-guardian`

4. **Verification**
   - Check status: `systemctl --user status swimmy-brain`
   - Check logs: `msg follow` (alias for tailing logs)

## Rollback
If deployment fails (services don't start):
- Restore backup code.
- Restart services again.
