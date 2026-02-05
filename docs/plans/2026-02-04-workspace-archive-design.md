# Workspace Archive Cleanup Design

**Goal:** Reduce confusion by archiving old, role-based files while preserving operational correctness.

**Architecture:** Use a deterministic, rules-based candidate extraction (role + date) with a protected path list derived from `docs/llm/*`. Archive is stored inside the repo as `archive/` with path mirroring, but excluded from Git. Changes are copy-verify-delete to ensure safe rollback.

**Tech Stack:** Shell utilities (find, sort), repo docs (`docs/llm/SPEC.md`, `ARCHITECTURE.md`, `INTERFACES.md`, `STATE.md`).

## Scope
- In-scope directories (role-based targets):
  - `logs/`
  - `backup/`
  - `.opus/backup/`
  - `doc/archive/`
  - `doc/handoff/`
  - `doc/logs/`
  - `docs/plans/`
  - `data/logs/`
  - `data/reports/`
  - `data/heartbeat/`
  - `data/memory/`
- Date cutoff: files with mtime **<= 2025-12-31**.
- Archive location: `archive/` with path mirroring (e.g., `archive/doc/logs/...`).
- Git: `archive/` is excluded via `.gitignore`.

## Out of Scope (Explicit Exclusions)
- `.worktrees/` and `.venv/` are left untouched.
- Operational paths that must remain intact:
  - `src/`, `systemd/`, `tools/`, `scripts/`, `config/`, `db/`
  - `data/library/`, `data/historical/`
  - Any file required by `docs/llm/*`.

## Protected Paths (Must Never Move)
Derived from `docs/llm/SPEC.md` and `docs/llm/STATE.md`:
- `data/backtest_cache.sexp`
- `data/system_metrics.sexp`
- `.opus/live_status.sexp`
- `data/reports/backtest_status.txt`
- `data/reports/backtest_debug.log`

If any additional required paths are discovered, the process must stop and ask for clarification.

## Procedure (Copy-Verify-Delete)
1. Build protected list (explicit paths above + any additional in `docs/llm/*`).
2. Collect candidates: files in target directories with mtime <= 2025-12-31.
3. Exclude protected paths from candidates.
4. Generate review artifacts:
   - `archive_candidate_list.txt`
   - `archive_summary.txt`
5. Review and confirm with human.
6. Copy candidates to `archive/` with path mirroring.
7. Verify:
   - File count matches.
   - Total bytes match.
   - Protected paths absent from candidate list.
8. Delete original files only after verification.

## Verification Rules
- No deletion if counts or sizes mismatch.
- `archive_candidate_list.txt` must not include any protected path.
- Report mismatches and stop for human decision.

## Rollback Plan
- Restore by copying from `archive/` back to original paths using the candidate list.
- Since archive is local and not in Git, rollback remains possible without network access.

## Notes
- This cleanup should not require updates to `docs/llm/STATE.md` or `docs/llm/INTERFACES.md` unless those documents explicitly describe log/backup storage locations to change.
