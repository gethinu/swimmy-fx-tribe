#!/usr/bin/env python3
"""Sync OpenClaw signals into local JSONL for Polymarket cycle."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import subprocess
from dataclasses import asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
import sys

if str(BASE_DIR) not in sys.path:
    sys.path.insert(0, str(BASE_DIR))

from tools.polymarket_openclaw_bot import OpenClawSignal, load_openclaw_signals


def _env_bool(env: Mapping[str, str], key: str, default: bool = False) -> bool:
    text = str(env.get(key, "")).strip().lower()
    if not text:
        return default
    return text in {"1", "true", "yes", "on"}


def _env_int(env: Mapping[str, str], key: str, default: int) -> int:
    text = str(env.get(key, "")).strip()
    if not text:
        return default
    try:
        return int(text)
    except ValueError:
        return default


def _env_csv(env: Mapping[str, str], key: str, default: str = "") -> List[str]:
    text = str(env.get(key, "")).strip()
    if not text:
        text = default
    if not text:
        return []
    return [item.strip() for item in text.split(",") if item and item.strip()]


def resolve_openclaw_cmd(*, provided_cmd: str, env: Mapping[str, str], base_dir: Path) -> str:
    cmd = provided_cmd.strip()
    if cmd:
        return cmd
    if not _env_bool(env, "POLYCLAW_USE_HEURISTIC_IF_NO_OPENCLAW_CMD", True):
        return ""

    script = env.get("POLYCLAW_HEURISTIC_SCRIPT", str(base_dir / "tools" / "openclaw_signal_heuristic.py")).strip()
    gamma_url = env.get("POLYCLAW_HEURISTIC_GAMMA_URL", "https://gamma-api.polymarket.com/markets").strip()
    limit = _env_int(env, "POLYCLAW_HEURISTIC_LIMIT", _env_int(env, "POLYCLAW_LIMIT", 250))
    keywords = _env_csv(
        env,
        "POLYCLAW_HEURISTIC_QUESTION_KEYWORDS",
        default="nba,nfl,mlb,nhl,ncaa,lakers,warriors,chiefs,yankees",
    )
    parts = [sys.executable, script, "--gamma-url", gamma_url, "--limit", str(max(1, limit))]
    for keyword in keywords:
        parts.extend(["--question-keyword", keyword])
    return " ".join(shlex.quote(part) for part in parts)


def parse_signals_stdout(stdout: str) -> Dict[str, OpenClawSignal]:
    text = stdout.strip()
    if not text:
        return {}

    try:
        payload = json.loads(text)
    except json.JSONDecodeError:
        payload = None
    if isinstance(payload, list):
        temp = BASE_DIR / "data" / "openclaw" / ".sync_tmp_parse.jsonl"
        temp.parent.mkdir(parents=True, exist_ok=True)
        temp.write_text(
            "\n".join(json.dumps(item, ensure_ascii=False) for item in payload if isinstance(item, Mapping)),
            encoding="utf-8",
        )
        try:
            return load_openclaw_signals(temp)
        finally:
            temp.unlink(missing_ok=True)

    lines = [line.strip() for line in text.splitlines() if line.strip()]
    for line in lines:
        if not line.startswith("["):
            continue
        try:
            payload = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(payload, list):
            continue
        temp = BASE_DIR / "data" / "openclaw" / ".sync_tmp_parse.jsonl"
        temp.parent.mkdir(parents=True, exist_ok=True)
        temp.write_text(
            "\n".join(json.dumps(item, ensure_ascii=False) for item in payload if isinstance(item, Mapping)),
            encoding="utf-8",
        )
        try:
            return load_openclaw_signals(temp)
        finally:
            temp.unlink(missing_ok=True)

    temp = BASE_DIR / "data" / "openclaw" / ".sync_tmp_parse.jsonl"
    temp.parent.mkdir(parents=True, exist_ok=True)
    temp.write_text("\n".join(lines), encoding="utf-8")
    try:
        return load_openclaw_signals(temp)
    finally:
        temp.unlink(missing_ok=True)


def serialize_signals_jsonl(signals: Dict[str, OpenClawSignal]) -> str:
    rows = []
    for market_id in sorted(signals.keys()):
        row = asdict(signals[market_id])
        rows.append(json.dumps(row, ensure_ascii=False))
    return ("\n".join(rows) + "\n") if rows else ""


def _write_atomic(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, path)


def sync_from_command(
    *,
    openclaw_cmd: str,
    signals_file: Path,
    meta_file: Path,
    min_signals: int,
    timeout_seconds: int,
) -> Dict[str, Any]:
    started = datetime.now(timezone.utc).isoformat()
    try:
        proc = subprocess.run(
            openclaw_cmd,
            shell=True,
            capture_output=True,
            text=True,
            check=True,
            timeout=max(1, timeout_seconds),
        )
    except Exception as exc:
        return {"ok": False, "error": f"command failed: {exc}", "signal_count": 0, "started_at": started}

    try:
        signals = parse_signals_stdout(proc.stdout)
    except Exception as exc:
        return {"ok": False, "error": f"parse failed: {exc}", "signal_count": 0, "started_at": started}

    count = len(signals)
    if count < max(0, min_signals):
        return {
            "ok": False,
            "error": f"too few signals: {count} < {min_signals}",
            "signal_count": count,
            "started_at": started,
        }

    jsonl = serialize_signals_jsonl(signals)
    _write_atomic(signals_file, jsonl)
    meta = {
        "updated_at": datetime.now(timezone.utc).isoformat(),
        "signal_count": count,
        "openclaw_cmd": openclaw_cmd,
        "signals_file": str(signals_file),
    }
    _write_atomic(meta_file, json.dumps(meta, ensure_ascii=False, indent=2) + "\n")
    return {"ok": True, "signal_count": count, "signals_file": str(signals_file), "meta_file": str(meta_file)}


def main() -> None:
    parser = argparse.ArgumentParser(description="Sync OpenClaw signal output to local JSONL")
    parser.add_argument("--openclaw-cmd", default=os.getenv("POLYCLAW_OPENCLAW_CMD", ""))
    parser.add_argument("--signals-file", default=os.getenv("POLYCLAW_SIGNALS_FILE", str(BASE_DIR / "data" / "openclaw" / "signals.jsonl")))
    parser.add_argument("--meta-file", default=os.getenv("POLYCLAW_SIGNALS_META_FILE", str(BASE_DIR / "data" / "openclaw" / "signals.meta.json")))
    parser.add_argument("--min-signals", type=int, default=int(os.getenv("POLYCLAW_SIGNAL_SYNC_MIN_SIGNALS", "1")))
    parser.add_argument("--timeout-seconds", type=int, default=int(os.getenv("POLYCLAW_SIGNAL_SYNC_TIMEOUT_SECONDS", "30")))
    args = parser.parse_args()

    env = dict(os.environ)
    cmd = resolve_openclaw_cmd(provided_cmd=args.openclaw_cmd, env=env, base_dir=BASE_DIR)
    if not cmd:
        raise SystemExit("set --openclaw-cmd/POLYCLAW_OPENCLAW_CMD or enable heuristic fallback")

    result = sync_from_command(
        openclaw_cmd=cmd,
        signals_file=Path(args.signals_file),
        meta_file=Path(args.meta_file),
        min_signals=max(0, args.min_signals),
        timeout_seconds=max(1, args.timeout_seconds),
    )
    print(json.dumps(result, ensure_ascii=False, indent=2))
    if not result.get("ok"):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
