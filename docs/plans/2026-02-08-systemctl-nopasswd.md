# Systemctl No-Password Restart Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow user `swimmy` to restart and check status of core Swimmy systemd services without a sudo password.

**Architecture:** Add a sudoers drop-in in `/etc/sudoers.d/` that explicitly allows `systemctl restart/status/is-active` for a fixed list of Swimmy services. Validate with `visudo` and verify with `sudo -n` commands.

**Tech Stack:** sudoers, systemd, bash

### Task 1: Confirm the current behavior (failing check)

**Files:**
- Test: N/A (command only)

**Step 1: Run the no-password check (should fail)**

Run:
```bash
sudo -n /usr/bin/systemctl status swimmy-brain.service
```
Expected: non-zero exit and “password required” error.

### Task 2: Add sudoers drop-in for systemctl

**Files:**
- Create: `/etc/sudoers.d/swimmy-systemctl`

**Step 1: Write the sudoers file**

```sudoers
# Allow swimmy to manage Swimmy systemd services without a password
# This is intentionally limited to restart/status/is-active on specific units.

swimmy ALL=(root) NOPASSWD: \
  /usr/bin/systemctl restart swimmy-brain, \
  /usr/bin/systemctl restart swimmy-brain.service, \
  /usr/bin/systemctl restart swimmy-school, \
  /usr/bin/systemctl restart swimmy-school.service, \
  /usr/bin/systemctl restart swimmy-guardian, \
  /usr/bin/systemctl restart swimmy-guardian.service, \
  /usr/bin/systemctl restart swimmy-data-keeper, \
  /usr/bin/systemctl restart swimmy-data-keeper.service, \
  /usr/bin/systemctl restart swimmy-backtest, \
  /usr/bin/systemctl restart swimmy-backtest.service, \
  /usr/bin/systemctl restart swimmy-risk, \
  /usr/bin/systemctl restart swimmy-risk.service, \
  /usr/bin/systemctl restart swimmy-notifier, \
  /usr/bin/systemctl restart swimmy-notifier.service, \
  /usr/bin/systemctl restart swimmy-evolution, \
  /usr/bin/systemctl restart swimmy-evolution.service, \
  /usr/bin/systemctl restart swimmy-watchdog, \
  /usr/bin/systemctl restart swimmy-watchdog.service, \
  /usr/bin/systemctl status swimmy-brain, \
  /usr/bin/systemctl status swimmy-brain.service, \
  /usr/bin/systemctl status swimmy-school, \
  /usr/bin/systemctl status swimmy-school.service, \
  /usr/bin/systemctl status swimmy-guardian, \
  /usr/bin/systemctl status swimmy-guardian.service, \
  /usr/bin/systemctl status swimmy-data-keeper, \
  /usr/bin/systemctl status swimmy-data-keeper.service, \
  /usr/bin/systemctl status swimmy-backtest, \
  /usr/bin/systemctl status swimmy-backtest.service, \
  /usr/bin/systemctl status swimmy-risk, \
  /usr/bin/systemctl status swimmy-risk.service, \
  /usr/bin/systemctl status swimmy-notifier, \
  /usr/bin/systemctl status swimmy-notifier.service, \
  /usr/bin/systemctl status swimmy-evolution, \
  /usr/bin/systemctl status swimmy-evolution.service, \
  /usr/bin/systemctl status swimmy-watchdog, \
  /usr/bin/systemctl status swimmy-watchdog.service, \
  /usr/bin/systemctl is-active swimmy-brain, \
  /usr/bin/systemctl is-active swimmy-brain.service, \
  /usr/bin/systemctl is-active swimmy-school, \
  /usr/bin/systemctl is-active swimmy-school.service, \
  /usr/bin/systemctl is-active swimmy-guardian, \
  /usr/bin/systemctl is-active swimmy-guardian.service, \
  /usr/bin/systemctl is-active swimmy-data-keeper, \
  /usr/bin/systemctl is-active swimmy-data-keeper.service, \
  /usr/bin/systemctl is-active swimmy-backtest, \
  /usr/bin/systemctl is-active swimmy-backtest.service, \
  /usr/bin/systemctl is-active swimmy-risk, \
  /usr/bin/systemctl is-active swimmy-risk.service, \
  /usr/bin/systemctl is-active swimmy-notifier, \
  /usr/bin/systemctl is-active swimmy-notifier.service, \
  /usr/bin/systemctl is-active swimmy-evolution, \
  /usr/bin/systemctl is-active swimmy-evolution.service, \
  /usr/bin/systemctl is-active swimmy-watchdog, \
  /usr/bin/systemctl is-active swimmy-watchdog.service
```

**Step 2: Validate sudoers syntax**

Run:
```bash
visudo -cf /etc/sudoers.d/swimmy-systemctl
```
Expected: `parsed OK`.

### Task 3: Verify no-password commands

**Files:**
- Test: N/A (command only)

**Step 1: Re-run the no-password check (should pass)**

Run:
```bash
sudo -n /usr/bin/systemctl status swimmy-brain.service
```
Expected: status output, exit code 0.

**Step 2: Optional quick verify**

Run:
```bash
sudo -n /usr/bin/systemctl is-active swimmy-brain.service
```
Expected: `active`.
