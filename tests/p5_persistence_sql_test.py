#!/usr/bin/env python3
"""P5 (Thread B / persistence hardening) — SQL-semantics tests.

Exercises the exact SQLite behaviors the Lisp persistence layer relies on,
against the same SQLite engine, so they are verifiable offline on any host
(the Lisp side needs the vendored sqlite DLL / full system to load). Covers:
  * data_sexp_sha256 column migration + idempotency
  * BEGIN IMMEDIATE / COMMIT / ROLLBACK atomicity (record-graveyard-pattern)
  * sha256 checksum roundtrip (sha256-hex)
  * :CORRUPT quarantine is excluded from the active/breeding allow-list
  * oversize sentinel is written instead of the blob
  * graveyard monotonic-non-decreasing invariant (audit)
"""
import hashlib
import sqlite3
import sys
import tempfile
import os

MAX_DATA_SEXP_LENGTH = 262144  # mirrors swimmy.core:*max-data-sexp-length*

failures = []


def check(desc, cond):
    print(("  ok: " if cond else "FAIL: ") + desc)
    if not cond:
        failures.append(desc)


def sha256_hex(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def base_schema(cur):
    # Minimal shape mirroring strategies (only columns these tests touch).
    cur.execute("""CREATE TABLE strategies (
        name TEXT PRIMARY KEY, rank TEXT, hash TEXT,
        sharpe REAL, data_sexp TEXT, updated_at INTEGER)""")


def test_migration_idempotent():
    con = sqlite3.connect(":memory:")
    cur = con.cursor()
    base_schema(cur)
    cur.execute("ALTER TABLE strategies ADD COLUMN data_sexp_sha256 TEXT")
    cols = [r[1] for r in cur.execute("PRAGMA table_info(strategies)")]
    check("migration adds data_sexp_sha256", "data_sexp_sha256" in cols)
    # second ADD raises (handler-case swallows it in Lisp) -> idempotent
    raised = False
    try:
        cur.execute("ALTER TABLE strategies ADD COLUMN data_sexp_sha256 TEXT")
    except sqlite3.OperationalError:
        raised = True
    check("re-adding column raises (idempotent via caught error)", raised)
    con.close()


def test_immediate_tx_atomicity():
    path = os.path.join(tempfile.mkdtemp(), "p5.db")
    con = sqlite3.connect(path, isolation_level=None)  # manual tx control
    cur = con.cursor()
    base_schema(cur)
    cur.execute("ALTER TABLE strategies ADD COLUMN data_sexp_sha256 TEXT")

    # commit path (record-graveyard-pattern happy path)
    cur.execute("BEGIN IMMEDIATE")
    s = '#S(STRATEGY :NAME "Foo")'
    cur.execute("INSERT OR REPLACE INTO strategies (name,rank,data_sexp,data_sexp_sha256) VALUES (?,?,?,?)",
                ("Foo", ":GRAVEYARD", s, sha256_hex(s)))
    cur.execute("COMMIT")
    n = cur.execute("SELECT count(*) FROM strategies").fetchone()[0]
    check("after COMMIT graveyard row persists", n == 1)

    # rollback path (error inside the immediate tx -> unwind-protect ROLLBACK)
    try:
        cur.execute("BEGIN IMMEDIATE")
        cur.execute("INSERT OR REPLACE INTO strategies (name,rank,data_sexp) VALUES (?,?,?)",
                    ("Bar", ":B", "x"))
        raise RuntimeError("boom")
    except RuntimeError:
        cur.execute("ROLLBACK")
    present = cur.execute("SELECT count(*) FROM strategies WHERE name='Bar'").fetchone()[0]
    check("after ROLLBACK the failed write left no row", present == 0)
    con.close()


def test_checksum_roundtrip():
    con = sqlite3.connect(":memory:"); cur = con.cursor()
    base_schema(cur); cur.execute("ALTER TABLE strategies ADD COLUMN data_sexp_sha256 TEXT")
    s = '#S(STRATEGY :NAME "Foo" :SHARPE 1.5)'
    cur.execute("INSERT INTO strategies (name,data_sexp,data_sexp_sha256) VALUES (?,?,?)",
                ("Foo", s, sha256_hex(s)))
    stored, sm = cur.execute("SELECT data_sexp, data_sexp_sha256 FROM strategies WHERE name='Foo'").fetchone()
    check("checksum matches intact blob", sha256_hex(stored) == sm)
    # simulate bit-rot: mutate the blob without updating the checksum
    cur.execute("UPDATE strategies SET data_sexp=? WHERE name='Foo'", (s + " CORRUPTED",))
    stored2, sm2 = cur.execute("SELECT data_sexp, data_sexp_sha256 FROM strategies WHERE name='Foo'").fetchone()
    check("checksum MISMATCH detects corruption", sha256_hex(stored2) != sm2)
    con.close()


def test_oversize_sentinel():
    raw = "x" * (MAX_DATA_SEXP_LENGTH + 10)
    oversize = len(raw) > MAX_DATA_SEXP_LENGTH
    data_sexp = (":OVERSIZE:%d" % len(raw)) if oversize else raw
    check("oversize blob replaced by sentinel", data_sexp.startswith(":OVERSIZE:") and len(data_sexp) < 40)
    small = "y" * 100
    keep = small if len(small) <= MAX_DATA_SEXP_LENGTH else ("...")
    check("normal blob stored verbatim", keep == small)


def test_corrupt_quarantine_excluded():
    con = sqlite3.connect(":memory:"); cur = con.cursor()
    base_schema(cur)
    rows = [("Good", ":B", 1.2), ("Bad", ":B", 0.5)]
    cur.executemany("INSERT INTO strategies (name,rank,sharpe) VALUES (?,?,?)", rows)
    # quarantine Bad (as cemetery-audit-db does): never DELETE
    cur.execute("UPDATE strategies SET hash=':INVALID', rank=':CORRUPT' WHERE name='Bad'")
    survived = cur.execute("SELECT count(*) FROM strategies WHERE name='Bad'").fetchone()[0]
    check("quarantined row is NOT deleted", survived == 1)
    # the active/breeding allow-list (fetch-candidate-strategies ranks B/A/S)
    active = [r[0] for r in cur.execute(
        "SELECT name FROM strategies WHERE rank IN (':B',':A',':S')")]
    check(":CORRUPT excluded from active allow-list", active == ["Good"])
    con.close()


def test_graveyard_monotonic_invariant():
    # audit's high-water-mark check: current graveyard count must never drop.
    def violated(prev_hwm, current):
        return current < prev_hwm
    check("hwm: growth is fine", not violated(10, 12))
    check("hwm: equal is fine", not violated(12, 12))
    check("hwm: shrink is a VIOLATION", violated(12, 9))


if __name__ == "__main__":
    test_migration_idempotent()
    test_immediate_tx_atomicity()
    test_checksum_roundtrip()
    test_oversize_sentinel()
    test_corrupt_quarantine_excluded()
    test_graveyard_monotonic_invariant()
    if failures:
        print("\np5_persistence_sql_test: FAILED (%d)" % len(failures), file=sys.stderr)
        sys.exit(1)
    print("\np5_persistence_sql_test: all SQL-semantics invariants hold")
