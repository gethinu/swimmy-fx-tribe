---
description: Test all Discord notification channels
---

This workflow runs a script to trigger a test notification for every configured channel in the system (Apex, Daily, Weekly, Alerts, Emergency, Recruiter, Backtest, Journal, and all Symbol channels).

1. Ensure the Notifier service is running:
   ```bash
   pgrep -f "notifier.py"
   ```
   (If not running, start it via `make run` or systemd)

2. Run the test script:
   // turbo
   ```bash
   source .env && sbcl --script tools/test_all_notifications.lisp
   ```

3. Verify output in Discord channels. You should see a "🧪 TEST" message in every channel.
