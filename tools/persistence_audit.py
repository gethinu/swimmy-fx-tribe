#!/usr/bin/env python3
"""P5 (Thread B / persistence hardening) — read-only persistence audit.

A daily, READ-ONLY health check over the strategies store. It opens the DB in
SQLite read-only mode and never mutates strategy data; its only write is a tiny
high-water-mark stamp file used for the graveyard monotonicity invariant.

Checks:
  1. graveyard monotonic non-decreasing  — the graveyard is append-only; if the
     :GRAVEYARD count ever drops below the recorded high-water mark, rows were
     lost/deleted (a HARD invariant -> non-zero exit).
  2. data_sexp checksum integrity        — recompute sha256(data_sexp) and
     compare to the stored data_sexp_sha256; mismatches = silent bit-rot.
  3. hash ':INVALID' ratio               — rows the cemetery audit could not parse.
  4. :CORRUPT quarantine count           — rows quarantined by P5-D.
  5. oversize sentinel count             — data_sexp replaced by ':OVERSIZE:'.
  6. library files vs SQL rows           — reconcile data/library/<rank>/*.lisp
     file count against the SQL row count; report drift.

Exit code: 0 when the hard invariant holds, 1 when the graveyard shrank.
On violation, an opt-in Discord ping fires only if SWIMMY_DISCORD_ALERTS is set.
"""
import argparse
import glob
import hashlib
import json
import os
import sqlite3
import sys
import time

ACTIVE_RANKS = (":B", ":A", ":S", ":LEGEND", ":LEGEND-ARCHIVE")


def sha256_hex(s):
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def connect_ro(db_path):
    uri = "file:%s?mode=ro" % os.path.abspath(db_path).replace("?", "%3f")
    return sqlite3.connect(uri, uri=True)


def has_column(cur, table, col):
    return col in {r[1] for r in cur.execute("PRAGMA table_info(%s)" % table)}


def audit(db_path, hwm_path, library_path):
    report = {"ts": int(time.time()), "db": db_path, "violations": []}
    con = connect_ro(db_path)
    cur = con.cursor()

    total = cur.execute("SELECT COUNT(*) FROM strategies").fetchone()[0]
    report["total_rows"] = total

    # 1. graveyard monotonic non-decreasing
    gy = cur.execute(
        "SELECT COUNT(*) FROM strategies WHERE UPPER(TRIM(rank))=':GRAVEYARD'"
    ).fetchone()[0]
    prev_hwm = 0
    if os.path.exists(hwm_path):
        try:
            prev_hwm = int(open(hwm_path).read().strip() or "0")
        except (ValueError, OSError):
            prev_hwm = 0
    report["graveyard_count"] = gy
    report["graveyard_hwm"] = prev_hwm
    if gy < prev_hwm:
        report["violations"].append(
            "graveyard shrank: %d < high-water-mark %d (rows lost?)" % (gy, prev_hwm))
    new_hwm = max(gy, prev_hwm)
    try:
        os.makedirs(os.path.dirname(hwm_path) or ".", exist_ok=True)
        tmp = "%s.%d.tmp" % (hwm_path, os.getpid())
        with open(tmp, "w") as f:
            f.write(str(new_hwm))
        os.replace(tmp, hwm_path)  # atomic
    except OSError as e:
        report.setdefault("warnings", []).append("could not persist hwm: %s" % e)

    # 2. checksum integrity (only if the P5 column exists)
    mism = unchecked = 0
    if has_column(cur, "strategies", "data_sexp_sha256"):
        for blob, stored in cur.execute(
            "SELECT data_sexp, data_sexp_sha256 FROM strategies "
            "WHERE data_sexp IS NOT NULL AND TRIM(data_sexp) <> ''"
        ):
            if stored is None or stored == "":
                unchecked += 1
            elif sha256_hex(blob) != stored:
                mism += 1
    report["checksum_mismatch"] = mism
    report["checksum_unchecked"] = unchecked
    if mism > 0:
        report["violations"].append("%d data_sexp checksum mismatch(es)" % mism)

    # 3. hash ':INVALID' ratio
    invalid = cur.execute(
        "SELECT COUNT(*) FROM strategies WHERE hash=':INVALID'").fetchone()[0]
    report["hash_invalid"] = invalid
    report["hash_invalid_ratio"] = round(invalid / total, 4) if total else 0.0

    # 4. :CORRUPT quarantine count
    corrupt = cur.execute(
        "SELECT COUNT(*) FROM strategies WHERE UPPER(TRIM(rank))=':CORRUPT'"
    ).fetchone()[0]
    report["quarantined_corrupt"] = corrupt

    # 5. oversize sentinels
    oversize = cur.execute(
        "SELECT COUNT(*) FROM strategies WHERE data_sexp LIKE ':OVERSIZE:%'"
    ).fetchone()[0]
    report["oversize_sentinels"] = oversize

    con.close()

    # 6. library files vs SQL rows (active ranks)
    lib_files = 0
    if library_path and os.path.isdir(library_path):
        lib_files = len(glob.glob(os.path.join(library_path, "*", "*.lisp")))
    report["library_files"] = lib_files
    con2 = connect_ro(db_path)
    active_rows = con2.cursor().execute(
        "SELECT COUNT(*) FROM strategies WHERE UPPER(TRIM(rank)) IN (%s)"
        % ",".join("'%s'" % r for r in ACTIVE_RANKS)
    ).fetchone()[0]
    con2.close()
    report["active_sql_rows"] = active_rows
    report["library_vs_sql_drift"] = lib_files - active_rows

    return report


def maybe_alert(report):
    webhook = os.getenv("SWIMMY_DISCORD_ALERTS", "")
    if not webhook or not report["violations"]:
        return
    try:
        import urllib.request
        desc = "\\n".join("• " + v for v in report["violations"])
        payload = json.dumps({
            "content": "@here",
            "embeds": [{
                "title": "🧬 PERSISTENCE AUDIT VIOLATION",
                "description": desc[:1800],
                "color": 16711680,
            }],
        }).encode("utf-8")
        req = urllib.request.Request(
            webhook, data=payload, headers={"Content-Type": "application/json"})
        urllib.request.urlopen(req, timeout=10).read()
    except Exception:
        pass  # best-effort


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", default="data/memory/swimmy.db")
    ap.add_argument("--hwm", default="data/memory/.graveyard_hwm")
    ap.add_argument("--library", default="data/library")
    ap.add_argument("--log", default="logs/persistence_audit.log")
    ap.add_argument("--json", action="store_true", help="print report as JSON")
    args = ap.parse_args()

    if not os.path.exists(args.db):
        print("[AUDIT] DB not found: %s (skipping)" % args.db, file=sys.stderr)
        return 0

    report = audit(args.db, args.hwm, args.library)
    line = json.dumps(report, sort_keys=True)

    try:
        os.makedirs(os.path.dirname(args.log) or ".", exist_ok=True)
        with open(args.log, "a") as f:
            f.write(line + "\n")
    except OSError:
        pass

    print(line if args.json else
          "[AUDIT] rows=%d graveyard=%d(hwm=%d) checksum_mismatch=%d "
          "hash_invalid=%d corrupt=%d oversize=%d lib=%d/%d drift=%d %s" % (
              report["total_rows"], report["graveyard_count"], report["graveyard_hwm"],
              report["checksum_mismatch"], report["hash_invalid"],
              report["quarantined_corrupt"], report["oversize_sentinels"],
              report["library_files"], report["active_sql_rows"],
              report["library_vs_sql_drift"],
              "OK" if not report["violations"] else "VIOLATIONS: " + "; ".join(report["violations"])))

    if report["violations"]:
        maybe_alert(report)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
