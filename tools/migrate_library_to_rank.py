#!/usr/bin/env python3
"""
migrate_library_to_rank.py
==========================
Reorganize data/library from legacy tier dirs to rank dirs.
- SELECTION -> B
- TRAINING  -> A
- BATTLEFIELD/VETERAN -> S
- INCUBATOR/LEGEND/GRAVEYARD remain

Use --dry-run to preview, --cleanup to remove empty legacy dirs after move.
"""

from __future__ import annotations

import argparse
import os
import shutil
from pathlib import Path

LEGACY_TO_RANK = {
    "SELECTION": "B",
    "TRAINING": "A",
    "BATTLEFIELD": "S",
    "VETERAN": "S",
    "INCUBATOR": "INCUBATOR",
    "LEGEND": "LEGEND",
    "GRAVEYARD": "GRAVEYARD",
}


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def move_file(src: Path, dest: Path, dry_run: bool) -> bool:
    if dest.exists():
        print(f"[SKIP] exists: {dest}")
        return False
    if dry_run:
        print(f"[DRY]  move {src} -> {dest}")
        return True
    try:
        dest.parent.mkdir(parents=True, exist_ok=True)
        src.rename(dest)
        return True
    except OSError:
        dest.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(src), str(dest))
        return True


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dry-run", action="store_true", help="Preview moves only")
    parser.add_argument("--cleanup", action="store_true", help="Remove empty legacy dirs")
    args = parser.parse_args()

    base_dir = resolve_base_dir()
    library = base_dir / "data" / "library"
    if not library.exists():
        print(f"Library dir not found: {library}")
        return 1

    moved = 0
    scanned = 0

    for legacy, rank in LEGACY_TO_RANK.items():
        src_dir = library / legacy
        if not src_dir.exists():
            continue
        # Skip in-place directories
        if legacy == rank:
            continue
        dest_dir = library / rank
        files = list(src_dir.glob("*.lisp"))
        scanned += len(files)
        for f in files:
            if move_file(f, dest_dir / f.name, args.dry_run):
                moved += 1

        if args.cleanup and not args.dry_run:
            try:
                if not any(src_dir.iterdir()):
                    src_dir.rmdir()
                    print(f"[CLEAN] removed empty {src_dir}")
            except OSError:
                pass

    print(f"[DONE] scanned={scanned} moved={moved} dry_run={args.dry_run}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
