#!/usr/bin/env python3
"""
reconcile_archive_db.py
=======================
Reconcile GRAVEYARD / RETIRED rank drift between Library files and SQLite.

Why:
- Evolution Report counts are DB-truth (`get-db-rank-counts`).
- In practice, archived strategies can exist as files while DB rank is NIL/other,
  or (for graveyard) may be missing from DB entirely.
- This tool scans `data/library/{GRAVEYARD,RETIRED}` and:
  - chooses a canonical rank per strategy name (newest mtime wins across the two dirs),
  - updates DB `strategies.rank` to match,
  - optionally inserts missing rows.

Notes:
- CL uses universal-time (epoch 1900-01-01). We store `updated_at` in universal-time.
- This tool is non-destructive to Library files; it only updates the DB.
"""

from __future__ import annotations

import argparse
import os
import sqlite3
import time
from dataclasses import dataclass
from pathlib import Path


UNIX_TO_UNIVERSAL = 2208988800  # seconds between 1900-01-01 and 1970-01-01


@dataclass(frozen=True)
class Candidate:
    name: str
    rank: str  # ":GRAVEYARD" or ":RETIRED"
    path: Path
    mtime: float


def universal_now() -> int:
    return int(time.time()) + UNIX_TO_UNIVERSAL


def scan_rank_dir(library_root: Path, dir_name: str) -> dict[str, Candidate]:
    path = library_root / dir_name
    out: dict[str, Candidate] = {}
    if not path.exists():
        return out
    rank = f":{dir_name.upper()}"
    with os.scandir(path) as it:
        for e in it:
            if not e.is_file():
                continue
            if not e.name.endswith(".lisp"):
                continue
            name = e.name[:-5]
            try:
                st = e.stat()
            except FileNotFoundError:
                continue
            cand = Candidate(name=name, rank=rank, path=Path(e.path), mtime=st.st_mtime)
            prev = out.get(name)
            if prev is None or cand.mtime > prev.mtime:
                out[name] = cand
    return out


def choose_canonical(grave: dict[str, Candidate], retired: dict[str, Candidate]) -> tuple[dict[str, Candidate], int]:
    """
    Return (name->candidate) where newest mtime wins across GRAVEYARD/RETIRED,
    and overlap count.
    """
    names = set(grave.keys()) | set(retired.keys())
    canonical: dict[str, Candidate] = {}
    overlap = 0
    for n in names:
        g = grave.get(n)
        r = retired.get(n)
        if g and r:
            overlap += 1
            canonical[n] = g if g.mtime >= r.mtime else r
        else:
            canonical[n] = g or r  # type: ignore[assignment]
    return canonical, overlap


def normalize_rank_db(rank: str | None) -> str:
    if rank is None:
        return "NIL"
    r = str(rank).strip().upper()
    if r.startswith(":"):
        r = r[1:]
    return r


def chunked(seq: list[str], size: int) -> list[list[str]]:
    return [seq[i : i + size] for i in range(0, len(seq), size)]


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", default="data/memory/swimmy.db", help="SQLite DB path")
    ap.add_argument("--library", default="data/library", help="Library root path")
    ap.add_argument("--dry-run", action="store_true", help="Report changes without writing")
    ap.add_argument(
        "--no-insert",
        action="store_true",
        help="Do not insert missing DB rows (only update existing ranks)",
    )
    ap.add_argument(
        "--no-data-sexp",
        action="store_true",
        help="When inserting missing rows, leave data_sexp NULL (faster, smaller DB)",
    )
    ap.add_argument("--limit", type=int, default=0, help="Process at most N strategies (0=all)")
    ap.add_argument("--batch", type=int, default=1000, help="SQLite batch size")
    args = ap.parse_args()

    db_path = Path(args.db).resolve()
    lib_root = Path(args.library).resolve()

    grave = scan_rank_dir(lib_root, "GRAVEYARD")
    retired = scan_rank_dir(lib_root, "RETIRED")
    canonical, overlap = choose_canonical(grave, retired)

    names = sorted(canonical.keys())
    if args.limit and args.limit > 0:
        names = names[: args.limit]

    print(f"[SCAN] library={lib_root}")
    print(f"[SCAN] graveyard_files={len(grave)} retired_files={len(retired)} overlap_names={overlap} canonical_names={len(canonical)}")
    print(f"[DB]   db={db_path} exists={db_path.exists()}")

    if not db_path.exists():
        print("[ERR] DB not found")
        return 2

    conn = sqlite3.connect(str(db_path), timeout=60)
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()

    updated = 0
    inserted = 0
    missing = 0
    wrong = 0

    # SQLite variable limit is typically 999.
    select_batch = min(max(1, args.batch), 900)
    write_batch = min(max(1, args.batch), 5000)

    # Use shorter transactions to reduce long DB locks in live systems.
    def commit():
        if not args.dry_run:
            conn.commit()

    now_ut = universal_now()

    for batch_names in chunked(names, select_batch):
        qmarks = ",".join("?" for _ in batch_names)
        rows = cur.execute(f"SELECT name, rank FROM strategies WHERE name IN ({qmarks})", batch_names).fetchall()
        rank_by_name = {r["name"]: r["rank"] for r in rows}

        # Buffer writes to apply in chunks.
        updates: list[tuple[str, str, int]] = []
        inserts: list[tuple[str, str, str | None, int]] = []

        for n in batch_names:
            desired = canonical[n]
            db_rank_raw = rank_by_name.get(n)
            if db_rank_raw is None:
                missing += 1
                if args.no_insert:
                    continue
                data_sexp: str | None = None
                if not args.no_data_sexp:
                    try:
                        data_sexp = desired.path.read_text(encoding="utf-8", errors="replace")
                    except OSError:
                        data_sexp = None
                inserts.append((n, desired.rank, data_sexp, now_ut))
            else:
                db_norm = normalize_rank_db(db_rank_raw)
                desired_norm = normalize_rank_db(desired.rank)
                if db_norm != desired_norm:
                    wrong += 1
                    updates.append((desired.rank, n, now_ut))

        if updates or inserts:
            if args.dry_run:
                updated += len(updates)
                inserted += len(inserts)
            else:
                # Apply updates/inserts in smaller write batches.
                if updates:
                    for u in chunked([str(i) for i in range(len(updates))], write_batch):
                        # slice updates list by index chunk
                        idxs = [int(x) for x in u]
                        sub = [updates[i] for i in idxs]
                        cur.executemany("UPDATE strategies SET rank=?, updated_at=? WHERE name=?", [(r, ut, name) for (r, name, ut) in sub])
                        updated += len(sub)
                        commit()
                if inserts:
                    for u in chunked([str(i) for i in range(len(inserts))], write_batch):
                        idxs = [int(x) for x in u]
                        sub = [inserts[i] for i in idxs]
                        cur.executemany(
                            "INSERT OR IGNORE INTO strategies (name, rank, data_sexp, updated_at) VALUES (?, ?, ?, ?)",
                            sub,
                        )
                        inserted += len(sub)
                        commit()

    if not args.dry_run:
        conn.commit()
    conn.close()

    print(f"[RESULT] processed={len(names)} missing_in_db={missing} wrong_rank={wrong} would_update={updated} would_insert={inserted}" if args.dry_run else
          f"[RESULT] processed={len(names)} missing_in_db={missing} wrong_rank={wrong} updated={updated} inserted={inserted}")
    if args.dry_run:
        print("[DRY] No changes written.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

