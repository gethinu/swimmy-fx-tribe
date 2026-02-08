# Heartbeat Webhook Separation Design (2026-02-08)

## Goal
Route Heartbeat notifications to a dedicated Discord channel so they are not buried by alert traffic.

## Context
Heartbeat notifications currently resolve to the alerts webhook, which makes them hard to spot. We need a dedicated webhook while preserving a safe fallback to alerts if the new variable is not configured.

## Approach
- Introduce a new environment variable: `SWIMMY_DISCORD_HEARTBEAT`.
- Update `get-discord-webhook` so `key="heartbeat"` prefers `SWIMMY_DISCORD_HEARTBEAT` and falls back to `SWIMMY_DISCORD_ALERTS`.
- Keep all other routing unchanged.

## Data Flow
1. Heartbeat sender requests `get-discord-webhook "heartbeat"`.
2. Config returns `SWIMMY_DISCORD_HEARTBEAT` if set; otherwise returns alerts webhook.
3. Notifier posts the heartbeat to the resolved webhook.

## Error Handling
- If no heartbeat webhook is set, the system continues to fall back to alerts.
- Existing logging remains unchanged.

## Testing
- Add a unit test that sets `SWIMMY_DISCORD_HEARTBEAT` and verifies it is preferred over `SWIMMY_DISCORD_ALERTS`.

## Rollout
- Add `SWIMMY_DISCORD_HEARTBEAT` to `.env`.
- Restart `swimmy-brain` and `swimmy-notifier` to reload config.
