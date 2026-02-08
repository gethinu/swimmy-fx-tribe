# Heartbeat Now Trigger Design

**Goal:** Allow operators to trigger an immediate heartbeat from the running Brain without restarting services.

**Context:** Heartbeats are already generated on a schedule inside the Brain. Operators need an on-demand mechanism that avoids shelling into SBCL, avoids ZMQ command wiring, and works with the existing Discord notifier pipeline.

## Proposed Approach (Trigger File)
Create a trigger file at `.opus/heartbeat.now`. The Brain’s periodic maintenance loop checks for this file each cycle. If present, it calls the existing `swimmy.engine::send-discord-heartbeat` and then deletes the file. Operators can run `touch /home/swimmy/swimmy/.opus/heartbeat.now` to request a heartbeat immediately.

## Data Flow
Operator `touch` → `.opus/heartbeat.now` created → Scheduler detects file → `send-discord-heartbeat` → notifier sends to Discord → scheduler deletes trigger file.

## Error Handling
If `send-discord-heartbeat` isn’t bound or raises an error, the scheduler should ignore the error and still delete the trigger file to prevent repeated sends. Operators can re-trigger if needed.

## Testing
Add a unit test that:
- creates the trigger file,
- stubs `send-discord-heartbeat`,
- calls the new trigger-check function,
- asserts the stub was called and the file was removed.

## Rollout
No config changes required. New behavior is inactive unless the trigger file is created.
