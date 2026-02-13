#!/usr/bin/env python3
"""
reseed_active_b.py
==================
Reseed active B-rank candidates from archive ranks (:GRAVEYARD/:RETIRED).

Use when active pool is starved after archive-heavy reconciliation.
The script selects archive strategies that pass Stage1 B thresholds, ranks them
by category (timeframe x direction x symbol), and restores top-N per category.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import sqlite3
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence


UNIX_TO_UNIVERSAL = 2208988800
ARCHIVE_RANK_KEYS = ("GRAVEYARD", "RETIRED")
B_THRESHOLDS = {
    "sharpe": 0.15,
    "pf": 1.05,
    "wr": 0.35,
    "maxdd": 0.25,
}
RANK_PATTERN = re.compile(r"(:RANK\s+):(?:GRAVEYARD|RETIRED)\b", re.IGNORECASE)
STATUS_PATTERN = re.compile(r"(:STATUS\s+):INACTIVE\b", re.IGNORECASE)


@dataclass(frozen=True)
class Candidate:
    name: str
    rank: str
    timeframe: int
    direction: str
    symbol: str
    score: float


@dataclass
class ReseedSummary:
    selected: int = 0
    would_update: int = 0
    updated: int = 0
    missing_files: int = 0
    conflicts: int = 0
    conflicts_resolved: int = 0
    rewrite_errors: int = 0
    move_errors: int = 0


def universal_now() -> int:
    return int(time.time()) + UNIX_TO_UNIVERSAL


def chunked(items: Sequence[str], size: int) -> list[list[str]]:
    return [list(items[i : i + size]) for i in range(0, len(items), size)]


def normalize_rank(rank: str | None) -> str:
    if rank is None:
        return "NIL"
    text = str(rank).strip().upper()
    if text.startswith(":"):
        text = text[1:]
    return text


def select_reseed_candidates(
    conn: sqlite3.Connection,
    *,
    per_category: int = 20,
    limit: int = 0,
) -> list[Candidate]:
    if per_category <= 0:
        return []
    cur = conn.cursor()
    rows = cur.execute(
        """
        WITH c AS (
            SELECT
                name,
                rank,
                COALESCE(timeframe, -1) AS timeframe,
                COALESCE(direction, 'BOTH') AS direction,
                COALESCE(symbol, 'UNKNOWN') AS symbol,
                COALESCE(sharpe, 0.0) AS sharpe,
                COALESCE(profit_factor, 0.0) AS profit_factor,
                COALESCE(win_rate, 0.0) AS win_rate,
                COALESCE(max_dd, 1.0) AS max_dd,
                (0.4*COALESCE(sharpe, 0.0)
                 + 0.25*COALESCE(profit_factor, 0.0)
                 + 0.2*COALESCE(win_rate, 0.0)
                 + 0.15*(1.0-COALESCE(max_dd, 1.0))) AS score
            FROM strategies
            WHERE UPPER(REPLACE(COALESCE(rank, ''), ':', '')) IN ('GRAVEYARD', 'RETIRED')
              AND COALESCE(sharpe, 0.0) >= ?
              AND COALESCE(profit_factor, 0.0) >= ?
              AND COALESCE(win_rate, 0.0) >= ?
              AND COALESCE(max_dd, 1.0) < ?
        ),
        ranked AS (
            SELECT *,
                   ROW_NUMBER() OVER (
                       PARTITION BY timeframe, direction, symbol
                       ORDER BY score DESC, sharpe DESC, profit_factor DESC, win_rate DESC, name ASC
                   ) AS rn
            FROM c
        )
        SELECT name, rank, timeframe, direction, symbol, score
        FROM ranked
        WHERE rn <= ?
        ORDER BY timeframe, direction, symbol, rn
        """,
        (
            B_THRESHOLDS["sharpe"],
            B_THRESHOLDS["pf"],
            B_THRESHOLDS["wr"],
            B_THRESHOLDS["maxdd"],
            per_category,
        ),
    ).fetchall()

    out = [
        Candidate(
            name=str(r[0]),
            rank=f":{normalize_rank(r[1])}",
            timeframe=int(r[2]),
            direction=str(r[3]),
            symbol=str(r[4]),
            score=float(r[5]),
        )
        for r in rows
    ]
    if limit > 0:
        return out[:limit]
    return out


def _archive_dirs_for_rank(rank: str) -> tuple[str, str]:
    norm = normalize_rank(rank)
    if norm == "RETIRED":
        return ("RETIRED", "GRAVEYARD")
    return ("GRAVEYARD", "RETIRED")


def find_archive_file(library_root: Path, name: str, rank: str) -> Path | None:
    preferred, fallback = _archive_dirs_for_rank(rank)
    candidates = [
        library_root / preferred / f"{name}.lisp",
        library_root / fallback / f"{name}.lisp",
        library_root / "B" / f"{name}.lisp",
    ]
    for p in candidates:
        if p.exists() and p.is_file():
            return p
    return None


def rewrite_strategy_file(path: Path) -> None:
    text = path.read_text(encoding="utf-8", errors="ignore")
    updated = RANK_PATTERN.sub(r"\1:B", text)
    updated = STATUS_PATTERN.sub(r"\1:ACTIVE", updated)
    if updated != text:
        # Atomic replace avoids in-place permission errors when file ownership
        # differs (e.g., root-owned archive artifacts).
        tmp_fd, tmp_path = tempfile.mkstemp(
            prefix=f".{path.name}.",
            suffix=".tmp",
            dir=str(path.parent),
            text=True,
        )
        try:
            with os.fdopen(tmp_fd, "w", encoding="utf-8") as out:
                out.write(updated)
            os.replace(tmp_path, path)
        except Exception:
            try:
                os.unlink(tmp_path)
            except OSError:
                pass
            raise


def _update_db_ranks(
    conn: sqlite3.Connection,
    names: Sequence[str],
    *,
    now_ut: int,
    batch_size: int = 1000,
) -> int:
    if not names:
        return 0
    cur = conn.cursor()
    updated = 0
    for chunk in chunked(list(names), max(1, min(batch_size, 5000))):
        marks = ",".join("?" for _ in chunk)
        params = [now_ut, *chunk]
        cur.execute(
            f"""
            UPDATE strategies
               SET rank=':B', updated_at=?
             WHERE name IN ({marks})
               AND UPPER(REPLACE(COALESCE(rank, ''), ':', '')) IN ('GRAVEYARD', 'RETIRED')
            """,
            params,
        )
        updated += int(cur.rowcount if cur.rowcount is not None else 0)
    return updated


def apply_reseed(
    conn: sqlite3.Connection,
    library_root: Path,
    candidates: Iterable[Candidate],
    *,
    dry_run: bool = False,
    now_ut: int | None = None,
) -> ReseedSummary:
    summary = ReseedSummary()
    if now_ut is None:
        now_ut = universal_now()

    selected = list(candidates)
    summary.selected = len(selected)
    names_to_update: list[str] = []
    b_dir = library_root / "B"

    for cand in selected:
        src = find_archive_file(library_root, cand.name, cand.rank)
        if src is None:
            summary.missing_files += 1
            continue
        dst = b_dir / f"{cand.name}.lisp"
        if dst.exists() and dst.resolve() != src.resolve():
            summary.conflicts += 1
            summary.conflicts_resolved += 1
            if dry_run:
                names_to_update.append(cand.name)
                continue
            try:
                rewrite_strategy_file(dst)
            except OSError:
                summary.rewrite_errors += 1
            names_to_update.append(cand.name)
            continue
        if dry_run:
            names_to_update.append(cand.name)
            continue

        b_dir.mkdir(parents=True, exist_ok=True)
        if src.resolve() != dst.resolve():
            try:
                os.replace(src, dst)
            except OSError:
                summary.move_errors += 1
                continue
        try:
            rewrite_strategy_file(dst)
        except OSError:
            summary.rewrite_errors += 1
        names_to_update.append(cand.name)

    summary.would_update = len(names_to_update)
    if dry_run:
        return summary

    summary.updated = _update_db_ranks(conn, names_to_update, now_ut=now_ut)
    conn.commit()
    return summary


def backup_db(db_path: Path, backup_dir: Path) -> Path:
    backup_dir.mkdir(parents=True, exist_ok=True)
    stamp = time.strftime("%Y%m%d_%H%M%S")
    dst = backup_dir / f"b_reseed_{stamp}.db"
    shutil.copy2(db_path, dst)
    return dst


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", default="data/memory/swimmy.db", help="SQLite DB path")
    ap.add_argument("--library", default="data/library", help="Library root path")
    ap.add_argument("--per-category", type=int, default=20, help="Max candidates per category")
    ap.add_argument("--limit", type=int, default=0, help="Process at most N selected candidates (0=all)")
    ap.add_argument("--dry-run", action="store_true", help="Show changes without applying")
    ap.add_argument("--backup-dir", default="data/memory/backup", help="Backup directory for DB snapshot")
    ap.add_argument("--no-backup", action="store_true", help="Skip DB backup before applying")
    args = ap.parse_args()

    db_path = Path(args.db).resolve()
    library_root = Path(args.library).resolve()
    backup_dir = Path(args.backup_dir).resolve()

    if not db_path.exists():
        print(f"[RESEED] DB not found: {db_path}")
        return 2
    if not library_root.exists():
        print(f"[RESEED] Library root not found: {library_root}")
        return 2

    conn = sqlite3.connect(str(db_path))
    try:
        candidates = select_reseed_candidates(
            conn,
            per_category=max(1, args.per_category),
            limit=max(0, args.limit),
        )
        print(
            f"[RESEED] selected={len(candidates)} per_category={args.per_category} limit={args.limit}"
        )
        if not args.dry_run and not args.no_backup:
            backup_path = backup_db(db_path, backup_dir)
            print(f"[RESEED] backup={backup_path}")

        summary = apply_reseed(
            conn,
            library_root,
            candidates,
            dry_run=args.dry_run,
        )
        print(
            "[RESEED] "
            f"would_update={summary.would_update} "
            f"updated={summary.updated} "
            f"missing_files={summary.missing_files} "
            f"conflicts={summary.conflicts} "
            f"conflicts_resolved={summary.conflicts_resolved} "
            f"move_errors={summary.move_errors} "
            f"rewrite_errors={summary.rewrite_errors}"
        )
    finally:
        conn.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
