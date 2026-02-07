---
description: Manually trigger the experimental Agentic Hunt loop to generate and inject a new strategy.
---

# Trigger Hunt Workflow

This workflow activates the "Agentic Closure" mechanism to generate a new strategy for a specific category and inject it into the system.

## Usage

// turbo
```bash
# Syntax: python3 tools/trigger_hunt.py <category>
# Categories: scalp, trend, reversion, breakout
cd /home/swimmy/swimmy && python3 tools/trigger_hunt.py scalp
```

## Verification

Check the logs to ensure the strategy was recruited (Safety Gate passed).

// turbo
```bash
tail -n 20 /home/swimmy/swimmy/logs/swimmy.log | grep "HEADHUNTER"
```
