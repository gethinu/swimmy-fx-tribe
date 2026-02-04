# Systemd Canonicalization Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** systemd(system) を唯一の正本として運用・ドキュメントを統一し、user unit の常用による二重起動を防止する。

**Architecture:** ドキュメント更新を段階的に行い、最小のガードテストで「正本の明文化」と「runbookに user unit が混入しないこと」を継続的に検証する。STATE を先行更新し、その後 owners_guide と runbook を整合させる。

**Tech Stack:** Markdown, Bash, ripgrep (`rg`).

---

### Task 1: STATE 正本化の明文化 + 最小ガードテスト

**Files:**
- Create: `tests/systemd_canonicalization_docs_test.sh`
- Modify: `docs/llm/STATE.md`

**Step 1: Write the failing test**

Create `tests/systemd_canonicalization_docs_test.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state="$root/docs/llm/STATE.md"

rg -q "systemd\(system\) を正本" "$state"
```

**Step 2: Run test to verify it fails**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: FAIL (STATE に "systemd(system) を正本" が未記載)

**Step 3: Write minimal implementation**

Update `docs/llm/STATE.md` に明示行を追加:

```markdown
- **運用（systemd正本）**: systemd(system) を正本とし、systemctl --user は診断用途のみ。
```

※ 既存の「運用（Brain起動）」の直下に置く。

**Step 4: Run test to verify it passes**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: PASS

**Step 5: Commit**

```bash
git add docs/llm/STATE.md tests/systemd_canonicalization_docs_test.sh
git commit -m "docs: declare systemd(system) as canonical"
```

---

### Task 2: owners_guide 正本化の明文化

**Files:**
- Modify: `tests/systemd_canonicalization_docs_test.sh`
- Modify: `doc/owners_guide.md`

**Step 1: Write the failing test**

Extend `tests/systemd_canonicalization_docs_test.sh` to require owners_guide の明記:

```bash
#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state="$root/docs/llm/STATE.md"
owners="$root/doc/owners_guide.md"

rg -q "systemd\(system\) を正本" "$state"
rg -q "systemd\(system\) を正本" "$owners"
rg -q "systemctl --user disable swimmy-brain" "$owners"
```

**Step 2: Run test to verify it fails**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: FAIL（owners_guide に正本明記・disable 手順が未記載）

**Step 3: Write minimal implementation**

Update `doc/owners_guide.md` の運用注記ブロックに以下を追加:
- 「正本は systemd(system)」の明記
- `systemctl --user stop/disable swimmy-brain` の明示
- user unit を誤起動した場合の復旧手順（system 再起動 + ポート確認）

**Step 4: Run test to verify it passes**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: PASS

**Step 5: Commit**

```bash
git add doc/owners_guide.md tests/systemd_canonicalization_docs_test.sh
git commit -m "docs: align owners guide with systemd(system) canonical"
```

---

### Task 3: runbook 正本化の明文化

**Files:**
- Modify: `tests/systemd_canonicalization_docs_test.sh`
- Modify: `doc/runbook.md`

**Step 1: Write the failing test**

Extend `tests/systemd_canonicalization_docs_test.sh` to enforce runbook policy:

```bash
#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state="$root/docs/llm/STATE.md"
owners="$root/doc/owners_guide.md"
runbook="$root/doc/runbook.md"

rg -q "systemd\(system\) を正本" "$state"
rg -q "systemd\(system\) を正本" "$owners"
rg -q "systemctl --user disable swimmy-brain" "$owners"
rg -q "systemd\(system\) を正本" "$runbook"

if rg -q "systemctl --user" "$runbook"; then
  echo "Runbook must not reference systemctl --user" >&2
  exit 1
fi
```

**Step 2: Run test to verify it fails**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: FAIL（runbook に正本明記が未記載）

**Step 3: Write minimal implementation**

Update `doc/runbook.md` に「systemd(system) 正本」明記を追記。
（`systemctl --user` は記載しない）

**Step 4: Run test to verify it passes**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: PASS

**Step 5: Commit**

```bash
git add doc/runbook.md tests/systemd_canonicalization_docs_test.sh
git commit -m "docs: align runbook with systemd(system) canonical"
```

---

### Task 4: Final Verification

**Files:**
- None

**Step 1: Run doc guard test**

Run: `bash tests/systemd_canonicalization_docs_test.sh`

Expected: PASS

**Step 2: Manual sanity check**

Run:

```bash
rg -n "systemd\(system\) を正本" docs/llm/STATE.md doc/owners_guide.md doc/runbook.md
```

Expected: 3 files all contain the phrase.

