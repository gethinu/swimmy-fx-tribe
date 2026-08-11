#!/usr/bin/env python3
"""P5 — end-to-end test for tools/persistence_audit.py against a synthetic DB."""
import hashlib
import json
import os
import shutil
import sqlite3
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
AUDIT = os.path.join(ROOT, "tools", "persistence_audit.py")
failures = []


def check(desc, cond):
    print(("  ok: " if cond else "FAIL: ") + desc)
    if not cond:
        failures.append(desc)


def sha(s):
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def build_db(path):
    con = sqlite3.connect(path)
    c = con.cursor()
    c.execute("""CREATE TABLE strategies (
        name TEXT PRIMARY KEY, rank TEXT, hash TEXT, sharpe REAL,
        data_sexp TEXT, data_sexp_sha256 TEXT, updated_at INTEGER)""")
    good = '#S(STRATEGY :NAME "Good")'
    rows = [
        ("Good1", ":B", "abc", 1.0, good, sha(good)),
        ("Good2", ":A", "def", 1.2, good, sha(good)),
        ("Gy1", ":GRAVEYARD", "h1", -1.0, good, sha(good)),
        ("Gy2", ":GRAVEYARD", "h2", -2.0, good, sha(good)),
        # corrupted blob: stored checksum no longer matches (bit-rot)
        ("Rot", ":B", "h3", 0.3, good + "XX", sha(good)),
        # quarantined
        ("Corr", ":CORRUPT", ":INVALID", 0.0, "@@garbage@@", sha("@@garbage@@")),
        # oversize sentinel
        ("Big", ":B", "h4", 0.4, ":OVERSIZE:999999", sha(":OVERSIZE:999999")),
        # hash invalid marker
        ("Inv", ":CORRUPT", ":INVALID", 0.0, "junk", sha("junk")),
    ]
    c.executemany(
        "INSERT INTO strategies (name,rank,hash,sharpe,data_sexp,data_sexp_sha256)"
        " VALUES (?,?,?,?,?,?)", rows)
    con.commit(); con.close()


def run_audit(db, hwm, lib):
    p = subprocess.run(
        [sys.executable, AUDIT, "--db", db, "--hwm", hwm, "--library", lib, "--json"],
        capture_output=True, text=True)
    return p.returncode, json.loads(p.stdout.strip().splitlines()[-1])


def main():
    d = tempfile.mkdtemp()
    try:
        db = os.path.join(d, "swimmy.db")
        hwm = os.path.join(d, ".graveyard_hwm")
        lib = os.path.join(d, "library")
        os.makedirs(os.path.join(lib, "B"))
        for nm in ("Good1", "Good2", "extra"):
            open(os.path.join(lib, "B", nm + ".lisp"), "w").close()
        build_db(db)

        rc, r = run_audit(db, hwm, lib)
        # run1 exits 1 because the DB seeds a checksum-mismatch row (Rot);
        # that is itself a violation. The hwm is still recorded regardless.
        check("run1 exit 1 (seeded checksum mismatch is a violation)", rc == 1)
        check("graveyard counted (2)", r["graveyard_count"] == 2)
        check("hwm initialized to 2", r["graveyard_hwm"] in (0, 2) and os.path.exists(hwm))
        check("checksum mismatch detected (Rot)", r["checksum_mismatch"] == 1)
        check("hash_invalid counted (2)", r["hash_invalid"] == 2)
        check("quarantined :CORRUPT counted (2)", r["quarantined_corrupt"] == 2)
        check("oversize sentinel counted (1)", r["oversize_sentinels"] == 1)
        check("library files counted (3)", r["library_files"] == 3)
        # active ranks = B/A/S/LEGEND*: Good1,Good2,Rot,Big = 4
        check("active sql rows (4)", r["active_sql_rows"] == 4)
        check("drift = 3 - 4 = -1", r["library_vs_sql_drift"] == -1)
        check("run1 reports checksum violation", any("checksum" in v for v in r["violations"]))

        # stamp should now hold hwm=2
        check("hwm persisted as 2", open(hwm).read().strip() == "2")

        # Now DELETE a graveyard row -> count drops below hwm -> HARD violation
        con = sqlite3.connect(db); con.execute("DELETE FROM strategies WHERE name='Gy1'")
        con.commit(); con.close()
        rc2, r2 = run_audit(db, hwm, lib)
        check("run2 graveyard shrank to 1", r2["graveyard_count"] == 1)
        check("run2 exit 1 (monotonic invariant violated)", rc2 == 1)
        check("run2 reports graveyard-shrank violation",
              any("graveyard shrank" in v for v in r2["violations"]))
    finally:
        shutil.rmtree(d, ignore_errors=True)

    if failures:
        print("\np5_audit_test: FAILED (%d)" % len(failures), file=sys.stderr)
        return 1
    print("\np5_audit_test: persistence audit behaves correctly")
    return 0


if __name__ == "__main__":
    sys.exit(main())
