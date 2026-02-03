#!/usr/bin/env python3
"""Migrate local storage JSON to S-expression (.sexp)."""

from __future__ import annotations

import json
import os
import shutil
from datetime import datetime
from pathlib import Path


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


def _is_simple_symbol(value: str) -> bool:
    if not value:
        return False
    for ch in value:
        if ch.isspace() or ch in "()\"":
            return False
    return True


def _escape_string(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', "\\\"")


def _sexp_atom(value) -> str:
    if value is True:
        return "t"
    if value is False or value is None:
        return "nil"
    if isinstance(value, (int, float)):
        return repr(value)
    if isinstance(value, str):
        if _is_simple_symbol(value):
            return value
        return f'"{_escape_string(value)}"'
    return _sexp_atom(str(value))


def _sexp_from_obj(obj) -> str:
    if isinstance(obj, dict):
        items = []
        for key, value in obj.items():
            key_atom = _sexp_atom(str(key))
            items.append(f"({key_atom} . {_sexp_from_obj(value)})")
        return f"({' '.join(items)})"
    if isinstance(obj, list):
        return f"({' '.join(_sexp_from_obj(item) for item in obj)})"
    return _sexp_atom(obj)


def _write_atomic(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp_path = path.with_suffix(path.suffix + ".tmp")
    with open(tmp_path, "w", encoding="utf-8") as f:
        f.write(text)
    os.replace(tmp_path, path)


def _backup_path(src: Path) -> Path:
    stamp = datetime.now().strftime("%Y%m%d%H%M")
    candidate = src.with_name(f"{src.name}.bak.{stamp}")
    if not candidate.exists():
        return candidate
    idx = 1
    while True:
        alt = src.with_name(f"{src.name}.bak.{stamp}.{idx}")
        if not alt.exists():
            return alt
        idx += 1


def _load_json(path: Path):
    if path.suffix == ".jsonl":
        entries = []
        with open(path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                entries.append(json.loads(line))
        return entries
    with open(path, "r", encoding="utf-8") as f:
        return json.load(f)


def migrate_file(path_json: str) -> str:
    src = Path(path_json)
    if not src.exists():
        raise FileNotFoundError(str(src))
    data = _load_json(src)
    if isinstance(data, list):
        payload = {"schema_version": 1, "entries": data}
    elif isinstance(data, dict):
        payload = {"schema_version": 1, **data}
    else:
        raise ValueError("Unsupported JSON root type")

    sexp_text = _sexp_from_obj(payload) + "\n"
    dst = src.with_suffix(".sexp")
    _write_atomic(dst, sexp_text)

    backup = _backup_path(src)
    shutil.move(str(src), str(backup))

    return str(dst)


def main() -> int:
    base = resolve_base_dir()
    default_files = [
        base / "data" / "backtest_cache.json",
        base / "data" / "system_metrics.json",
        base / ".opus" / "live_status.json",
    ]
    paths = [Path(p) for p in default_files]

    if len(os.sys.argv) > 1:
        paths = [Path(p) for p in os.sys.argv[1:]]

    for path in paths:
        if not path.exists():
            print(f"skip: {path} (missing)")
            continue
        try:
            out = migrate_file(str(path))
            print(f"migrated: {path} -> {out}")
        except Exception as e:
            print(f"error: {path}: {e}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
