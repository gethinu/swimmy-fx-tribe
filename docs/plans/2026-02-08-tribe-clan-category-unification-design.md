# Tribe/Clan Removal & Category Vocabulary Unification (Design)

**Date:** 2026-02-08
**Status:** Approved
**Owner:** Swimmy

## Goal
Remove all `tribe/clan` vocabulary from user-visible output and internal decision logic, and make category terms the single source of truth. Preserve existing behavior, especially risk gates and trade throttling, while eliminating semantic drift.

## Non-Goals
- No large refactors of internal state layout or module boundaries.
- No behavior changes beyond vocabulary alignment and matching category keys.
- No schema changes to live_status payload (keys remain the same).

## Decisions
- **Vocabulary:** Use `category` terms everywhere (`:trend/:reversion/:breakout/:scalp`).
- **Risk gates:** Keep logic, but rewrite category checks to match the canonical category keys.
- **Schema:** Keep `live_status` at `schema_version=2` and update docs to match.
- **Display names:** Replace clan terms in all user-visible outputs with category names.

## Scope
### Code (behavioral alignment)
- **Risk gates**
  - `src/lisp/school/school-fortress.lisp` — replace `:hunters/:raiders/:shamans/:breakers` with category keys inside `verify-parallel-scenarios`.
  - `src/lisp/school/school-voting.lisp` — replace clan category checks in `convene-high-council`.

### Code (vocabulary cleanup)
- **Notifications/Logs**
  - `src/lisp/school/school-kb.lisp` — replace “Clan” in recruit notifications with “Category”.
  - `src/lisp/core/rituals.lisp` — remove “Clans Gather” phrasing.
  - `src/lisp/mixseek.lisp` — replace “4 Clans” announcements and any clan labels in output.
  - `src/lisp/quality.lisp` — replace “4 Great Clans” documentation output.
  - `src/lisp/school/school-execution.lisp` — replace “Clan ... Full” allocation log line.
- **Templates**
  - `src/lisp/templates/founder.lisp.template` — rename `:{{CLAN}}` to `:{{CATEGORY}}`.
- **Comments/Docs in code**
  - `src/lisp/school/school-strategy.lisp` — remove “Clan mapping” language near category allocation.
  - `src/lisp/school/school-state.lisp` — remove leftover tribe references in comments.

### Documentation
- `doc/owners_guide.md` — update live_status schema_version from 1 to 2 and confirm tribe removal.
- `doc/SYSTEM_ARCHITECTURE.md` — remove “氏族システム” references and align with category vocabulary.

## Data Flow Impact
No changes to payload structure, keys, or external interfaces. Only string content and category matching logic are updated. `live_status` remains schema v2 with no tribe/tribes fields.

## Error Handling
No new error handling required. Preserve existing guards and defaults. If any category value is unknown, keep current fallback behavior.

## Tests
- Keep existing “no tribe” tests:
  - `test-live-status-schema-v2-no-tribe`
  - `test-daily-report-omits-tribe`
- Add tests to prevent regressions in user-visible outputs:
  - Recruit notification output contains “Category”, not “Clan/tribe”.
  - Mixseek output contains no clan terms.
  - Rituals output contains no clan terms.
  - Allocation log contains no clan terms.
- Add smoke test to ensure risk-gate category matching behaves correctly for current category keys.

## Rollout
- Single PR/commit to update code + docs + tests.
- No migrations required.
- Verify with targeted tests and a short smoke run.

## Open Questions
None.
