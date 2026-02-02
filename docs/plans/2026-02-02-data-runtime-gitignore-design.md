# Data runtime gitignore design

## Goal
Reduce operational noise and merge conflicts by excluding runtime-generated data under `data/` from Git tracking, while preserving long-lived reference data that should remain versioned.

## Current state
- `.gitignore` already excludes: `data/memory/`, `data/macro/`, `data/library/INCUBATOR/`, `data/library/GRAVEYARD/`, `data/library/TRAINING/`, and `data/historical/`.
- Runtime heartbeat is written to `data/heartbeat/school.tick`.
- Reports and logs under `data/reports/` and `data/logs/` are currently tracked and frequently change.

## Proposal
Add ignore rules for runtime-only directories:
- `data/heartbeat/` (includes `school.tick`)
- `data/logs/`
- `data/reports/`

Keep long-lived reference data tracked, including:
- `data/knowledge_base.sexp`
- `data/system_metrics.json`
- `data/wisdom_state.json`

`data/backtest_cache.json` remains tracked for now; revisit if it becomes too volatile or large.

## Migration steps
1. Add the ignore entries to `.gitignore`.
2. If any files under the newly ignored paths are already tracked, remove them from the index with:
   - `git rm --cached data/heartbeat` (if tracked)
   - `git rm --cached data/logs` (if tracked)
   - `git rm --cached data/reports` (if tracked)
3. Commit the removals separately from unrelated changes.

## Risks and mitigations
- Risk: Losing useful shared artifacts (reports/logs).
  - Mitigation: Promote durable artifacts into `doc/` or external storage as needed.
- Risk: Accidental exclusion of data that should be versioned.
  - Mitigation: Use `!` exceptions if a specific file must be tracked later.

## Verification
- `git status -sb` shows no tracked files under ignored paths.
- Runtime continues to write heartbeat and reports without Git churn.
