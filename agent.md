# agent.md

Purpose:
- Single entrypoint for agents working on this repo. Always read this file first.
- Then read the core docs listed below.

Read order (must):
- doc/owners_guide.md (operations and runbook)
- doc/SYSTEM_ARCHITECTURE.md (system overview)
- owners_guide.md (operator quick start)

System overview:
- Brain (Lisp): brain.lisp and src/lisp/engine/*
- School/Evolution (Lisp): src/lisp/school/*
- Discord notifier (Python + ZMQ): tools/notifier.py (port 5562)
- Reports: data/reports/evolution_factory_report.txt (primary)

Key code paths:
- src/lisp/core/config.lisp (env + webhook config, .env fallback)
- src/lisp/core/discord.lisp (ZMQ notification queue)
- src/lisp/school/school-connector.lisp (main evolution loop)
- src/lisp/school/school-rank-system.lisp (B/A/S criteria + promotions)
- src/lisp/school/school-narrative.lisp (Evolution Factory Report)

Operational commands (see doc/owners_guide.md for details):
- Restart services: systemctl --user restart swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-notifier
- Stop services: systemctl --user stop swimmy-brain swimmy-guardian swimmy-school swimmy-data-keeper swimmy-notifier
- Logs: journalctl --user -f -u swimmy-brain -u swimmy-guardian -u swimmy-notifier -u swimmy-school
- Dashboard: python3 tools/dashboard.py

Notes:
- Discord webhooks come from SWIMMY_DISCORD_* env vars. .env is the local fallback.
- If Evolution Factory Report is missing in Discord, confirm .env is loaded and notifier is running.
- When editing, keep ZMQ notifier contract stable (payload with webhook + data).
