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
import re
import sqlite3
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence


UNIX_TO_UNIVERSAL = 2208988800  # seconds between 1900-01-01 and 1970-01-01
ARCHIVE_RANKS = (":GRAVEYARD", ":RETIRED")
VALID_SQL_IDENT = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


@dataclass(frozen=True)
class Candidate:
    name: str
    rank: str  # ":GRAVEYARD" or ":RETIRED"
    path: Path
    mtime: float


def universal_now() -> int:
    return int(time.time()) + UNIX_TO_UNIVERSAL


def scan_rank_dir(
    library_root: Path, dir_name: str, *, skip_newer_than_mtime: float | None = None
) -> tuple[dict[str, Candidate], int]:
    path = library_root / dir_name
    out: dict[str, Candidate] = {}
    skipped_recent = 0
    if not path.exists():
        return out, skipped_recent
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
            if skip_newer_than_mtime is not None and st.st_mtime > skip_newer_than_mtime:
                skipped_recent += 1
                continue
            cand = Candidate(name=name, rank=rank, path=Path(e.path), mtime=st.st_mtime)
            prev = out.get(name)
            if prev is None or cand.mtime > prev.mtime:
                out[name] = cand
    return out, skipped_recent


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


def chunked(seq: Sequence[str], size: int) -> list[list[str]]:
    return [seq[i : i + size] for i in range(0, len(seq), size)]


def fetch_db_archive_names(conn: sqlite3.Connection) -> list[str]:
    cur = conn.cursor()
    rows = cur.execute(
        "SELECT name FROM strategies WHERE rank IN (?, ?)",
        ARCHIVE_RANKS,
    ).fetchall()
    return [str(r[0]) for r in rows if r and r[0]]


def compute_db_only_archive_names(
    conn: sqlite3.Connection,
    canonical_names: Iterable[str],
) -> list[str]:
    canonical = {str(n) for n in canonical_names if n}
    db_archive_names = fetch_db_archive_names(conn)
    return sorted([n for n in db_archive_names if n not in canonical])


def _validate_sql_ident(name: str) -> str:
    if not VALID_SQL_IDENT.match(name):
        raise ValueError(f"Invalid SQL identifier: {name}")
    return name


def prune_db_only_archive_names(
    conn: sqlite3.Connection,
    db_only_names: Sequence[str],
    *,
    dry_run: bool = False,
    backup_table: str | None = "archive_reconcile_backup",
    now_ut: int | None = None,
    batch_size: int = 1000,
) -> tuple[int, int]:
    targets = sorted({str(n) for n in db_only_names if n})
    if not targets:
        return 0, 0
    if dry_run:
        return len(targets), len(targets if backup_table else [])

    cur = conn.cursor()
    backed_up = 0
    deleted = 0
    backed_up_at = now_ut if now_ut is not None else universal_now()
    table = _validate_sql_ident(backup_table) if backup_table else None

    if table:
        cur.execute(
            f"""
            CREATE TABLE IF NOT EXISTS {table} (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                name TEXT NOT NULL,
                rank TEXT NOT NULL,
                data_sexp TEXT,
                updated_at INTEGER,
                backed_up_at INTEGER NOT NULL
            )
            """
        )

    chunk_size = min(max(1, int(batch_size)), 5000)
    for names_chunk in chunked(targets, chunk_size):
        qmarks = ",".join("?" for _ in names_chunk)
        query_params = [*names_chunk, *ARCHIVE_RANKS]

        selected = cur.execute(
            f"""
            SELECT name, rank, data_sexp, updated_at
              FROM strategies
             WHERE name IN ({qmarks})
               AND rank IN (?, ?)
            """,
            query_params,
        ).fetchall()

        if table and selected:
            cur.executemany(
                f"""
                INSERT INTO {table} (name, rank, data_sexp, updated_at, backed_up_at)
                VALUES (?, ?, ?, ?, ?)
                """,
                [(r[0], r[1], r[2], r[3], backed_up_at) for r in selected],
            )
            backed_up += len(selected)

        cur.execute(
            f"""
            DELETE FROM strategies
             WHERE name IN ({qmarks})
               AND rank IN (?, ?)
            """,
            query_params,
        )
        deleted += int(cur.rowcount if cur.rowcount is not None else 0)

    return deleted, backed_up


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
    ap.add_argument(
        "--prune-db-only",
        action="store_true",
        help="Delete DB-only archive rows (name not found in Library archive dirs)",
    )
    ap.add_argument(
        "--db-only-limit",
        type=int,
        default=0,
        help="Prune at most N DB-only archive rows (0=all)",
    )
    ap.add_argument(
        "--backup-table",
        default="archive_reconcile_backup",
        help="Backup table for pruned rows (empty to disable backup)",
    )
    ap.add_argument(
        "--no-backup",
        action="store_true",
        help="Do not back up rows before pruning DB-only archive rows",
    )
    ap.add_argument(
        "--grace-sec",
        type=int,
        default=0,
        help="Skip archive files modified within last N seconds at scan start (0=disabled)",
    )
    args = ap.parse_args()

    db_path = Path(args.db).resolve()
    lib_root = Path(args.library).resolve()
    grace_sec = max(0, int(args.grace_sec))
    scan_started_at = time.time()
    mtime_cutoff = (scan_started_at - grace_sec) if grace_sec > 0 else None

    grave, grave_skipped = scan_rank_dir(
        lib_root, "GRAVEYARD", skip_newer_than_mtime=mtime_cutoff
    )
    retired, retired_skipped = scan_rank_dir(
        lib_root, "RETIRED", skip_newer_than_mtime=mtime_cutoff
    )
    canonical, overlap = choose_canonical(grave, retired)

    names = sorted(canonical.keys())
    if args.limit and args.limit > 0:
        names = names[: args.limit]

    print(f"[SCAN] library={lib_root}")
    if grace_sec > 0:
        print(
            f"[SCAN] grace_sec={grace_sec} cutoff_unix={int(mtime_cutoff)} "
            f"skipped_recent={grave_skipped + retired_skipped}"
        )
    else:
        print("[SCAN] grace_sec=0 (disabled)")
    print(
        f"[SCAN] graveyard_files={len(grave)} retired_files={len(retired)} "
        f"overlap_names={overlap} canonical_names={len(canonical)} "
        f"skipped_recent_graveyard={grave_skipped} skipped_recent_retired={retired_skipped}"
    )
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

    db_only_count = 0
    would_prune = 0
    pruned = 0
    backed_up = 0
    backup_table = None if args.no_backup else (args.backup_table.strip() or None)

    if args.limit and args.limit > 0:
        print("[DRIFT] Skipped DB-only drift check because --limit is set.")
    else:
        db_only_names = compute_db_only_archive_names(conn, canonical.keys())
        db_only_count = len(db_only_names)
        if db_only_count:
            sample = ", ".join(db_only_names[:5])
            print(f"[DRIFT] db_only_in_db={db_only_count} sample={sample}")
        else:
            print("[DRIFT] db_only_in_db=0")

        prune_targets = db_only_names
        if args.db_only_limit and args.db_only_limit > 0:
            prune_targets = prune_targets[: args.db_only_limit]
        if args.prune_db_only:
            pruned, backed_up = prune_db_only_archive_names(
                conn,
                prune_targets,
                dry_run=args.dry_run,
                backup_table=backup_table,
                now_ut=now_ut,
                batch_size=write_batch,
            )
            would_prune = len(prune_targets) if args.dry_run else 0
        elif args.dry_run:
            would_prune = len(prune_targets)

    if not args.dry_run:
        conn.commit()
    conn.close()

    if args.dry_run:
        print(
            f"[RESULT] processed={len(names)} missing_in_db={missing} wrong_rank={wrong} "
            f"would_update={updated} would_insert={inserted} "
            f"db_only_in_db={db_only_count} would_prune_db_only={would_prune}"
        )
    else:
        print(
            f"[RESULT] processed={len(names)} missing_in_db={missing} wrong_rank={wrong} "
            f"updated={updated} inserted={inserted} "
            f"db_only_in_db={db_only_count} pruned_db_only={pruned} backed_up={backed_up}"
        )
    if args.dry_run:
        print("[DRY] No changes written.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
