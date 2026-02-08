# Evolution Report Decoupling Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Decouple evolution report generation from the Phase 7 loop, add staleness alerting, and align the Python report tool with SQLite as the source of truth.

**Architecture:** Add a `maybe-send-evolution-report` gate that uses report file mtime so throttling works across processes. Call it from the brain scheduler and the Phase 7 hook. Add a staleness checker that alerts when the report or heartbeat file is too old. Update `tools/report_evolution.py` to query SQLite rank counts instead of filesystem counts.

**Tech Stack:** Common Lisp (SBCL/ASDF), Python (sqlite3, zmq), Swimmy telemetry + Discord notifier.

### Task 1: Add failing Lisp tests for report throttle + staleness alerts

**Files:**
- Modify: `src/lisp/tests/scheduler-tests.lisp`

**Step 1: Write the failing tests**

```lisp
(deftest test-evolution-report-throttle-uses-last-write
  (let* ((orig (symbol-function 'swimmy.school::notify-evolution-report))
         (called 0)
         (now 100000)
         (interval 3600)
         (swimmy.school::*evolution-report-interval* interval))
    (setf (symbol-function 'swimmy.school::notify-evolution-report)
          (lambda () (incf called)))
    (unwind-protect
        (progn
          (swimmy.school::maybe-send-evolution-report :now now :last-write now)
          (assert-equal 0 called "Should not send when fresh")
          (swimmy.school::maybe-send-evolution-report :now now :last-write (- now interval 1))
          (assert-equal 1 called "Should send when stale"))
      (setf (symbol-function 'swimmy.school::notify-evolution-report) orig))))

(deftest test-evolution-report-staleness-alert-throttles
  (let* ((orig-alert (symbol-function 'swimmy.core:notify-discord-alert))
         (orig-emit (and (fboundp 'swimmy.core::emit-telemetry-event)
                         (symbol-function 'swimmy.core::emit-telemetry-event)))
         (called 0)
         (now 200000)
         (threshold 7200)
         (cooldown 3600)
         (swimmy.school::*evolution-report-stale-threshold* threshold)
         (swimmy.school::*evolution-report-alert-interval* cooldown)
         (swimmy.school::*last-evolution-report-alert-time* 0))
    (setf (symbol-function 'swimmy.core:notify-discord-alert)
          (lambda (&rest _args) (declare (ignore _args)) (incf called)))
    (when orig-emit
      (setf (symbol-function 'swimmy.core::emit-telemetry-event)
            (lambda (&rest _args) (declare (ignore _args)) t)))
    (unwind-protect
        (progn
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now now :last-report now :last-heartbeat now)
          (assert-equal 0 called "Should not alert when fresh")
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now now :last-report (- now threshold 10) :last-heartbeat (- now threshold 10))
          (assert-equal 1 called "Should alert when stale")
          (swimmy.school::maybe-alert-evolution-report-staleness
           :now (+ now 10) :last-report (- now threshold 10) :last-heartbeat (- now threshold 10))
          (assert-equal 1 called "Should respect cooldown"))
      (setf (symbol-function 'swimmy.core:notify-discord-alert) orig-alert)
      (when orig-emit
        (setf (symbol-function 'swimmy.core::emit-telemetry-event) orig-emit)))))
```

**Step 2: Run tests to verify they fail**

Run: `bash scripts/ci-test.sh`
Expected: FAIL with undefined function/variable errors for new report functions/params.

**Step 3: Commit**

```bash
git add src/lisp/tests/scheduler-tests.lisp
git commit -m "test: add evolution report scheduling/staleness tests"
```

### Task 2: Implement report gating + staleness alerting in Lisp

**Files:**
- Modify: `src/lisp/school/school-narrative.lisp:220-340`
- Modify: `src/lisp/school/school-connector.lisp:102-118`
- Modify: `src/lisp/core/scheduler.lisp:64-88`

**Step 1: Add report gating + staleness helpers**

```lisp
(defparameter *evolution-report-path* "data/reports/evolution_factory_report.txt")
(defparameter *evolution-heartbeat-path* "data/heartbeat/school.tick")
(defparameter *evolution-report-interval* (* 60 60))
(defparameter *evolution-report-stale-threshold* (* 2 60 60))
(defparameter *evolution-report-alert-interval* (* 60 60))
(defparameter *last-evolution-report-alert-time* 0)

(defun safe-file-write-date (path)
  (or (ignore-errors (file-write-date path)) 0))

(defun maybe-send-evolution-report (&key (now (get-universal-time)) last-write (reason "scheduled"))
  (let* ((last (or last-write (safe-file-write-date *evolution-report-path*)))
         (age (- now last)))
    (when (> age *evolution-report-interval*)
      (format t "[REPORT] 📨 Sending Evolution Report (~a)...~%" reason)
      (notify-evolution-report)
      (when (fboundp 'write-oos-status-file)
        (ignore-errors (write-oos-status-file :reason reason)))
      t)))

(defun maybe-alert-evolution-report-staleness
    (&key (now (get-universal-time)) last-report last-heartbeat)
  (let* ((report-last (or last-report (safe-file-write-date *evolution-report-path*)))
         (heartbeat-last (or last-heartbeat (safe-file-write-date *evolution-heartbeat-path*)))
         (report-age (if (> report-last 0) (- now report-last) nil))
         (heartbeat-age (if (> heartbeat-last 0) (- now heartbeat-last) nil))
         (report-stale (or (null report-age) (> report-age *evolution-report-stale-threshold*)))
         (heartbeat-stale (or (null heartbeat-age) (> heartbeat-age *evolution-report-stale-threshold*)))
         (cooldown-ok (> (- now *last-evolution-report-alert-time*)
                         *evolution-report-alert-interval*)))
    (when (and cooldown-ok (or report-stale heartbeat-stale))
      (setf *last-evolution-report-alert-time* now)
      (let ((msg (format nil "⚠️ Evolution report/heartbeat stale. report_age=~a heartbeat_age=~a" 
                         (or report-age "MISSING") (or heartbeat-age "MISSING"))))
        (swimmy.core:notify-discord-alert msg)
        (when (fboundp 'swimmy.core::emit-telemetry-event)
          (swimmy.core::emit-telemetry-event "evolution.report.stale"
                                             :service "school"
                                             :severity "warn"
                                             :data (list :report_age report-age
                                                         :heartbeat_age heartbeat-age)))))))
```

**Step 2: Wire Phase 7 hook to use new gate**

Replace `phase-7-report` with:

```lisp
(defun phase-7-report ()
  "Send report if interval passed"
  (when (fboundp 'swimmy.school::maybe-send-evolution-report)
    (swimmy.school::maybe-send-evolution-report :reason "phase-7")))
```

Delete obsolete `*last-report-time*` and `+report-interval+` definitions.

**Step 3: Call from scheduler (brain)**

Add inside the 60s throttled block in `run-periodic-maintenance` after `check-scheduled-tasks`:

```lisp
(when (fboundp 'swimmy.school::maybe-send-evolution-report)
  (funcall 'swimmy.school::maybe-send-evolution-report :reason "scheduled"))
(when (fboundp 'swimmy.school::maybe-alert-evolution-report-staleness)
  (funcall 'swimmy.school::maybe-alert-evolution-report-staleness))
```

**Step 4: Run tests to verify pass**

Run: `bash scripts/ci-test.sh`
Expected: PASS with new tests green.

**Step 5: Commit**

```bash
git add src/lisp/school/school-narrative.lisp src/lisp/school/school-connector.lisp src/lisp/core/scheduler.lisp
git commit -m "feat: decouple evolution report from phase loop"
```

### Task 3: Add failing Python test for DB-based counts

**Files:**
- Create: `tests/test_report_evolution_db.py`

**Step 1: Write failing test**

```python
import sqlite3
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
TOOLS = ROOT / "tools"
if str(TOOLS) not in sys.path:
    sys.path.insert(0, str(TOOLS))

import report_evolution as re


def test_get_db_rank_counts():
    tmp = ROOT / "data" / "memory" / "test-report-evolution.db"
    if tmp.exists():
        tmp.unlink()
    conn = sqlite3.connect(tmp)
    try:
        cur = conn.cursor()
        cur.execute("CREATE TABLE strategies (rank TEXT)")
        cur.executemany(
            "INSERT INTO strategies (rank) VALUES (?)",
            [(":S",), (":S",), (":A",), (":B",), (":GRAVEYARD",), (None,)],
        )
        conn.commit()
    finally:
        conn.close()

    counts = re.get_db_rank_counts(tmp)
    assert counts["s"] == 2
    assert counts["a"] == 1
    assert counts["b"] == 1
    assert counts["graveyard"] == 1
    assert counts["unranked"] == 1
    assert counts["active"] == 4
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/test_report_evolution_db.py -v`
Expected: FAIL (missing `get_db_rank_counts` or wrong behavior).

**Step 3: Commit**

```bash
git add tests/test_report_evolution_db.py
git commit -m "test: add report_evolution db count test"
```

### Task 4: Implement DB-based report_evolution

**Files:**
- Modify: `tools/report_evolution.py:1-120`

**Step 1: Implement DB helpers + switch to DB counts**

```python
import sqlite3

DB_PATH = os.getenv("SWIMMY_DB_PATH") or os.path.join(BASE_DIR, "data", "memory", "swimmy.db")


def normalize_rank(rank):
    if rank is None:
        return "NIL"
    s = str(rank).strip().upper()
    if s.startswith(":"):
        s = s[1:]
    return s


def get_db_rank_counts(db_path: Path | str = DB_PATH):
    conn = sqlite3.connect(str(db_path))
    try:
        cur = conn.cursor()
        cur.execute("SELECT rank, count(*) FROM strategies GROUP BY rank")
        rows = cur.fetchall()
    finally:
        conn.close()

    counts = {}
    total = 0
    for rank, count in rows:
        key = normalize_rank(rank)
        counts[key] = count
        total += count

    def count_rank(key):
        return counts.get(key, 0)

    s = count_rank("S")
    a = count_rank("A")
    b = count_rank("B")
    legend = count_rank("LEGEND")
    graveyard = count_rank("GRAVEYARD")
    retired = count_rank("RETIRED")
    incubator = count_rank("INCUBATOR")
    unranked = count_rank("NIL")
    active = total - graveyard - retired

    return {
        "total": total,
        "active": active,
        "s": s,
        "a": a,
        "b": b,
        "legend": legend,
        "graveyard": graveyard,
        "retired": retired,
        "incubator": incubator,
        "unranked": unranked,
    }
```

Then replace filesystem count usage in `main()` with:

```python
counts = get_db_rank_counts(DB_PATH)
count_s = counts["s"]
count_a = counts["a"]
count_b = counts["b"]
count_recruits = counts["incubator"]
count_graveyard = counts["graveyard"]
count_legend = counts["legend"]
active_total = counts["active"]
```

Update module docstring to say “SQLite DB is source of truth.”

**Step 2: Run pytest to verify pass**

Run: `pytest tests/test_report_evolution_db.py -v`
Expected: PASS.

**Step 3: Commit**

```bash
git add tools/report_evolution.py tests/test_report_evolution_db.py
git commit -m "feat: use sqlite rank counts in report_evolution"
```

### Task 5: Final validation

**Step 1: Run full test suite**

Run: `bash scripts/ci-test.sh`
Expected: PASS.

**Step 2: Summarize changes**

Document the updated report scheduling and DB-based reporting in the final response.
