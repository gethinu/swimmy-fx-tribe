#!/usr/bin/env python3
"""Self-heal guard for Windows XAU live loop processes.

Contract:
- Resolve expected live loops from run meta files.
- Detect running Windows powershell live loops by config path.
- Restart missing loops and terminate duplicate loops.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, Sequence, Tuple


DEFAULT_RUN_META_FILES: Sequence[Tuple[str, str]] = (
    ("primary", "data/reports/xau_autobot_trial_v2_current_run.json"),
    ("secondary", "data/reports/xau_autobot_trial_v2_current_run_r2.json"),
)

OPTIONAL_R3_META: Tuple[str, str] = (
    "explore_alpha3",
    "data/reports/xau_autobot_trial_v2_current_run_r3.json",
)


@dataclass(frozen=True)
class ExpectedLoop:
    slot: str
    run_id: str
    config_path: str
    poll_seconds: int
    config_key: str


_CONFIG_PATH_PATTERN = re.compile(
    r"(?i)-ConfigPath\s+(?:\"([^\"]+)\"|'([^']+)'|([^\s]+))"
)
_TRANSIENT_VSOCK_ERROR_PATTERN = re.compile(r"UtilAcceptVsock|accept4 failed 110", re.IGNORECASE)


def normalize_config_key(config_path: str) -> str:
    text = str(config_path or "").strip().strip('"').strip("'")
    if not text:
        return ""
    return Path(text.replace("\\", "/")).name.lower()


def extract_config_path_from_commandline(command_line: str) -> str:
    text = str(command_line or "")
    match = _CONFIG_PATH_PATTERN.search(text)
    if not match:
        return ""
    value = match.group(1) or match.group(2) or match.group(3) or ""
    return str(value).strip().strip('"').strip("'")


def _as_int(value: object, default: int) -> int:
    try:
        if value is None:
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _load_json(path: Path) -> Dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise RuntimeError(f"JSON object required: {path}")
    return payload


def load_expected_loops(entries: Iterable[Tuple[str, Path]]) -> List[ExpectedLoop]:
    loops: List[ExpectedLoop] = []
    seen_keys: set[str] = set()
    for slot, path in entries:
        if not path.exists():
            continue
        payload = _load_json(path)
        run_id = str(payload.get("run_id", "")).strip()
        config_path = str(payload.get("trial_config", "")).strip()
        if not run_id or not config_path:
            continue
        config_key = normalize_config_key(config_path)
        if not config_key or config_key in seen_keys:
            continue
        seen_keys.add(config_key)
        loops.append(
            ExpectedLoop(
                slot=str(slot),
                run_id=run_id,
                config_path=config_path,
                poll_seconds=max(1, _as_int(payload.get("poll_seconds"), 10)),
                config_key=config_key,
            )
        )
    return loops


def parse_running_loops(process_rows: Sequence[Mapping[str, Any]]) -> Dict[str, List[int]]:
    out: Dict[str, List[int]] = {}
    for row in process_rows:
        try:
            pid = int(row.get("ProcessId", 0))
        except (TypeError, ValueError):
            continue
        if pid <= 0:
            continue
        cmd = str(row.get("CommandLine", "") or "")
        config_path = extract_config_path_from_commandline(cmd)
        config_key = normalize_config_key(config_path)
        if not config_key:
            continue
        out.setdefault(config_key, []).append(pid)
    for key in list(out.keys()):
        out[key] = sorted(set(out[key]))
    return out


def plan_heal_actions(expected_keys: Iterable[str], running_by_key: Mapping[str, Sequence[int]]) -> Dict[str, Any]:
    expected = sorted({str(key).strip().lower() for key in expected_keys if str(key).strip()})
    running_keys = sorted({str(key).strip().lower() for key in running_by_key.keys() if str(key).strip()})

    missing_keys = [key for key in expected if key not in running_by_key]
    duplicate_terminate_pids: Dict[str, List[int]] = {}
    terminate_pids: List[int] = []
    for key in expected:
        pids = sorted({int(pid) for pid in running_by_key.get(key, []) if int(pid) > 0})
        if len(pids) <= 1:
            continue
        extra = pids[1:]
        duplicate_terminate_pids[key] = extra
        terminate_pids.extend(extra)
    terminate_pids = sorted(set(terminate_pids))

    unmanaged_keys = [key for key in running_keys if key not in expected]
    return {
        "expected_keys": expected,
        "running_keys": running_keys,
        "missing_keys": missing_keys,
        "duplicate_terminate_pids": duplicate_terminate_pids,
        "terminate_pids": terminate_pids,
        "unmanaged_keys": unmanaged_keys,
    }


def _windows_powershell_exe() -> str:
    for candidate in (
        "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe",
        "/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe",
    ):
        if Path(candidate).exists():
            return candidate
    return "powershell.exe"


def _windows_cmd_exe() -> str:
    for candidate in (
        "/mnt/c/Windows/System32/cmd.exe",
        "/mnt/c/WINDOWS/system32/cmd.exe",
    ):
        if Path(candidate).exists():
            return candidate
    return "cmd.exe"


def _to_windows_path(path: Path) -> str:
    proc = subprocess.run(
        ["wslpath", "-w", str(path)],
        capture_output=True,
        text=True,
        check=True,
    )
    return str(proc.stdout).strip()


def _discover_windows_python() -> str:
    cmd_exe = _windows_cmd_exe()
    proc = _run_windows_command(
        [cmd_exe, "/c", "py", "-3", "-c", "import sys; print(sys.executable)"],
        text=False,
        max_attempts=3,
    )
    if proc.returncode != 0:
        raise RuntimeError("unable to resolve Windows python executable via 'py -3'")
    stdout_text = bytes(proc.stdout or b"").decode("utf-8", errors="ignore")
    for line in stdout_text.splitlines():
        text = line.strip().strip("\r")
        if re.match(r"^[A-Za-z]:\\", text):
            return text
    raise RuntimeError("unable to parse Windows python executable path")


def list_windows_live_loop_processes(powershell_exe: str) -> List[Dict[str, Any]]:
    script = r"""
$procs = Get-CimInstance Win32_Process | Where-Object {
  $_.Name -eq "powershell.exe" -and
  $_.CommandLine -match "xau_autobot_live_loop\.ps1" -and
  $_.CommandLine -match "-Live" -and
  $_.CommandLine -notmatch "Get-CimInstance"
} | Select-Object ProcessId, CommandLine
$json = $procs | ConvertTo-Json -Compress
if ($null -eq $json) { Write-Output "[]" } else { Write-Output $json }
"""
    proc = _run_windows_command(
        [powershell_exe, "-NoProfile", "-Command", script],
        text=True,
        max_attempts=3,
    )
    if proc.returncode != 0:
        raise RuntimeError(f"powershell process listing failed: {proc.stderr.strip()}")
    raw = str(proc.stdout).strip()
    if not raw:
        return []
    payload = json.loads(raw)
    if isinstance(payload, dict):
        return [payload]
    if isinstance(payload, list):
        return [item for item in payload if isinstance(item, dict)]
    return []


def _stderr_text(proc: subprocess.CompletedProcess[Any], *, text: bool) -> str:
    err = proc.stderr
    if err is None:
        return ""
    if text:
        return str(err)
    return bytes(err).decode("utf-8", errors="ignore")


def _run_windows_command(
    args: Sequence[str],
    *,
    text: bool,
    max_attempts: int = 3,
    retry_seconds: float = 0.6,
) -> subprocess.CompletedProcess[Any]:
    attempts = max(1, int(max_attempts))
    last_proc: subprocess.CompletedProcess[Any] | None = None
    for idx in range(attempts):
        proc = subprocess.run(
            list(args),
            capture_output=True,
            text=text,
            check=False,
        )
        last_proc = proc
        if proc.returncode == 0:
            return proc
        stderr_text = _stderr_text(proc, text=text)
        is_transient = bool(_TRANSIENT_VSOCK_ERROR_PATTERN.search(stderr_text))
        if (not is_transient) or idx == attempts - 1:
            return proc
        time.sleep(retry_seconds)
    assert last_proc is not None
    return last_proc


def stop_windows_processes(powershell_exe: str, pids: Sequence[int]) -> None:
    filtered = sorted({int(pid) for pid in pids if int(pid) > 0})
    if not filtered:
        return
    csv = ",".join(str(pid) for pid in filtered)
    script = f"Stop-Process -Id {csv} -Force -ErrorAction SilentlyContinue"
    subprocess.run(
        [powershell_exe, "-NoProfile", "-Command", script],
        capture_output=True,
        text=True,
        check=False,
    )


def _resolve_config_path(repo_root: Path, config_path: str) -> Path:
    raw = str(config_path or "").strip()
    if not raw:
        raise RuntimeError("empty config path")
    path = Path(raw)
    if path.is_absolute():
        return path
    return repo_root / path


def start_windows_live_loop(
    *,
    powershell_exe: str,
    windows_python: str,
    repo_root: Path,
    loop: ExpectedLoop,
) -> int:
    loop_script = repo_root / "tools" / "windows" / "xau_autobot_live_loop.ps1"
    config_path = _resolve_config_path(repo_root, loop.config_path)
    if not loop_script.exists():
        raise RuntimeError(f"live loop script not found: {loop_script}")
    if not config_path.exists():
        raise RuntimeError(f"trial config not found: {config_path}")

    cmd = [
        powershell_exe,
        "-NoProfile",
        "-ExecutionPolicy",
        "Bypass",
        "-File",
        _to_windows_path(loop_script),
        "-RepoRoot",
        _to_windows_path(repo_root),
        "-ConfigPath",
        _to_windows_path(config_path),
        "-PythonExe",
        windows_python,
        "-PollSeconds",
        str(loop.poll_seconds),
        "-Live",
    ]
    process = subprocess.Popen(  # noqa: S603
        cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    return int(process.pid)


def _build_default_meta_entries(repo_root: Path, *, include_r3: bool) -> List[Tuple[str, Path]]:
    out = [(slot, repo_root / rel) for slot, rel in DEFAULT_RUN_META_FILES]
    if include_r3:
        out.append((OPTIONAL_R3_META[0], repo_root / OPTIONAL_R3_META[1]))
    return out


def run_guard(
    *,
    repo_root: Path,
    report_path: Path,
    dry_run: bool,
    restart_missing: bool,
    terminate_duplicates: bool,
    include_r3: bool,
) -> Dict[str, Any]:
    powershell_exe = _windows_powershell_exe()
    entries = _build_default_meta_entries(repo_root, include_r3=include_r3)
    expected_loops = load_expected_loops(entries)
    expected_by_key = {item.config_key: item for item in expected_loops}
    expected_keys = list(expected_by_key.keys())

    running_rows_pre = list_windows_live_loop_processes(powershell_exe)
    running_by_key_pre = parse_running_loops(running_rows_pre)
    plan = plan_heal_actions(expected_keys, running_by_key_pre)

    terminated_duplicates: List[int] = []
    restarted: List[Dict[str, Any]] = []

    if not dry_run:
        if terminate_duplicates:
            terminate_pids = [int(pid) for pid in plan.get("terminate_pids", [])]
            if terminate_pids:
                stop_windows_processes(powershell_exe, terminate_pids)
                terminated_duplicates = terminate_pids

        if restart_missing:
            missing_keys = [str(key) for key in plan.get("missing_keys", [])]
            if missing_keys:
                windows_python = _discover_windows_python()
                for key in missing_keys:
                    loop = expected_by_key.get(key)
                    if loop is None:
                        continue
                    started_pid = start_windows_live_loop(
                        powershell_exe=powershell_exe,
                        windows_python=windows_python,
                        repo_root=repo_root,
                        loop=loop,
                    )
                    restarted.append(
                        {
                            "config_key": key,
                            "run_id": loop.run_id,
                            "slot": loop.slot,
                            "starter_pid": started_pid,
                        }
                    )

    running_rows_post = list_windows_live_loop_processes(powershell_exe)
    running_by_key_post = parse_running_loops(running_rows_post)
    post_counts = {key: len(running_by_key_post.get(key, [])) for key in expected_keys}
    unhealthy = [key for key, count in post_counts.items() if count != 1]

    payload = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "repo_root": str(repo_root),
        "dry_run": bool(dry_run),
        "include_r3": bool(include_r3),
        "expected_loops": [
            {
                "slot": item.slot,
                "run_id": item.run_id,
                "config_path": item.config_path,
                "config_key": item.config_key,
                "poll_seconds": item.poll_seconds,
            }
            for item in expected_loops
        ],
        "running_rows_count_pre": len(running_rows_pre),
        "running_by_config_pre": running_by_key_pre,
        "plan": plan,
        "actions": {
            "terminated_duplicates": terminated_duplicates,
            "restarted": restarted,
        },
        "running_rows_count_post": len(running_rows_post),
        "running_by_config_post": running_by_key_post,
        "expected_config_counts_post": post_counts,
        "status": "HEALTHY" if not unhealthy else "DEGRADED",
        "unhealthy_config_keys": unhealthy,
    }

    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(json.dumps(payload, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")
    return payload


def main() -> None:
    parser = argparse.ArgumentParser(description="Self-heal Windows XAU live loop processes")
    parser.add_argument("--repo-root", default=str(Path(__file__).resolve().parents[1]))
    parser.add_argument("--report-path", default="data/reports/xau_autobot_live_loop_guard_latest.json")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--no-restart-missing", action="store_true")
    parser.add_argument("--no-terminate-duplicates", action="store_true")
    parser.add_argument("--include-r3", action="store_true")
    args = parser.parse_args()

    repo_root = Path(args.repo_root).resolve()
    report_path = Path(args.report_path)
    if not report_path.is_absolute():
        report_path = repo_root / report_path

    include_r3 = bool(args.include_r3)
    if not include_r3:
        include_r3 = str(os.getenv("XAU_AUTOBOT_LIVE_GUARD_INCLUDE_R3", "0")).strip() == "1"

    payload = run_guard(
        repo_root=repo_root,
        report_path=report_path,
        dry_run=bool(args.dry_run),
        restart_missing=not bool(args.no_restart_missing),
        terminate_duplicates=not bool(args.no_terminate_duplicates),
        include_r3=include_r3,
    )
    print(json.dumps(payload, ensure_ascii=True))

    if payload.get("status") != "HEALTHY":
        raise SystemExit(1)


if __name__ == "__main__":
    main()
