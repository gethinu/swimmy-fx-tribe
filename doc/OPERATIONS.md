# Operations Runbook — Storage (D: SSD) & Guardian Monorepo

Two operational changes the operator asked for:

1. **Run the factory on the new D: 1 TB SSD** without hitting the WSL2 I/O trap.
2. **Vendor `gethinu/guardian` into this repo** as a monorepo so Brain and the
   arena stay in lockstep with the ZMQ S-expr message contract.

---

## 1. Storage layout on the D: 1 TB SSD

The factory is data-heavy: historical bars/ticks, `backtest_cache.json`,
`data/reports/`, models, and MT5 history. The SSD is the right home for all of
it — **but how you mount it decides whether it is fast or slow.**

### ⚠️ The WSL2 trap: never point hot paths at `/mnt/d`
Brain (SBCL) and Guardian (Rust) — when they run inside WSL2 — do lots of small
random reads/writes (the breeding loop, the backtest cache). The Windows drive
is reached over the **9p** bridge (`/mnt/d/...`), which is an order of magnitude
slower for that access pattern. Pointing `data/historical/` or the backtest
cache at `/mnt/d` will make the factory *slower*, not faster.

### ✅ Correct: move the WSL2 distro itself onto the SSD (native ext4)
Everything inside the distro then lives on the SSD at ext4 speed.

```powershell
# Windows PowerShell (admin). Replace <Distro> with e.g. Ubuntu.
wsl --shutdown
wsl --export <Distro> D:\wsl\backup\<Distro>.tar
wsl --unregister <Distro>                      # removes the C: copy after export
wsl --import <Distro> D:\wsl\<Distro> D:\wsl\backup\<Distro>.tar --version 2
# Restore the default user (import logs in as root):
#   D:\wsl\<Distro> ... then in /etc/wsl.conf:  [user]\n default=<youruser>
```

Verify the data lives on ext4, not 9p:

```bash
df -T "$(git rev-parse --show-toplevel)/data" | awk 'NR==2{print $2}'   # want ext4, NOT 9p
```

### ✅ MT5 stays Windows-native on D:
MT5 is a Windows app; put its terminal + history on `D:\MT5\...` directly — no 9p
penalty because it is not crossing the WSL boundary. Guardian (in WSL) talks to
MT5 over ZMQ/TCP, not the filesystem, so this split is fine.

### What goes where (all git-ignored, so they belong on the SSD)
| Data | Path | Notes |
|---|---|---|
| Historical bars/ticks | `data/historical/` | largest; ext4 on SSD |
| Backtest cache | `data/backtest_cache.json` | hot random I/O — must be ext4, never 9p |
| Reports / snapshots / models | `data/reports/`, `data/snapshots/`, `data/models/` | bulk, append-mostly |
| MT5 terminal + history | `D:\MT5\...` (Windows) | native, reached via ZMQ |

### Caveat: SSD fixes I/O, not RAM
STATE.md previously flagged **memory pressure / SBCL heap**. The SSD raises
backtest throughput and capacity but does **not** add RAM. Keep the SBCL heap
and `SWIMMY_BACKTEST_WORKERS` / `*_MAX_INFLIGHT` knobs sized to physical memory;
revisit them separately from this storage move.

---

## 2. Vendoring guardian as a monorepo (git subtree)

**Why monorepo:** Brain ↔ Guardian are coupled by the ZMQ S-expr contract
(`docs/llm/INTERFACES.md`). Contract changes must land atomically on both sides;
two repos drift — which is exactly the failure this branch started from
(guardian missing → results unverifiable). `bundle-of-edge` stays a **separate**
downstream consumer; only guardian is vendored.

`git subtree` preserves guardian's full history under `guardian/`.

### Prerequisite (one of)
- **In a Claude Code session:** add `gethinu/guardian` to the session's
  repository scope first. Until it is in scope, the git proxy returns `403` for
  it and the merge cannot run.
- **On the host (母艦):** you already have credentials for the private repo, so
  run the command below directly.

### Merge

```bash
cd <repo root>
git checkout claude/system-status-question-6ivrvt        # or the integration branch

# .gitignore is already prepared (guardian/ is no longer ignored; only
# guardian/target/ build output is). Verify nothing stale is staged:
git status --short

git subtree add --prefix=guardian git@github.com:gethinu/guardian.git master --squash
#   --squash: collapse guardian history into one import commit (smaller history).
#   drop --squash to preserve guardian's full commit history instead.
```

### Post-merge wiring
1. **Cargo workspace** — add a root `Cargo.toml` so the tree builds as one:
   ```toml
   [workspace]
   members = ["guardian"]
   resolver = "2"
   ```
2. **Build check:** `cargo build --release --manifest-path guardian/Cargo.toml`
3. **Contract test:** confirm the ZMQ ports/messages in `guardian/src/main.rs`
   still match `docs/llm/INTERFACES.md` (ports 5557–5560, S-expr only).
4. **Docs:** flip `doc/FACTORY.md §2` from "arena is external" to
   "arena vendored at `guardian/`"; update `REFERENCE.md` paths.

### Updating guardian later
```bash
git subtree pull --prefix=guardian git@github.com:gethinu/guardian.git master --squash
```
(Push changes back upstream, if ever needed, with `git subtree push`.)

### Rollback
The merge is a normal commit. To undo before pushing:
`git reset --hard HEAD~1`. After pushing, revert the merge commit.
