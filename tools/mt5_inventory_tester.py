#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import tempfile
from dataclasses import asdict, dataclass
from datetime import date, datetime, timedelta, timezone
from html.parser import HTMLParser
from pathlib import Path
from typing import Iterable, Sequence


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_WIN_USER = "stair"
DEFAULT_TERMINAL_ID = "D0E8209F77C8CF37AD8BF550E51FF075"
DEFAULT_INSTALL_WIN = r"C:\Program Files\MetaTrader 5"
DEFAULT_PORTABLE_ROOT_WIN = r"C:\Users\stair\AppData\Local\SwimmyMT5Portable\inventory_tester"
DEFAULT_REPORTS_DIR = REPO_ROOT / "data" / "reports" / "mt5" / "inventory_tester"
DEFAULT_SKIPUPDATE = "C8CD4EE300519278312FD64A5DC0F6B7"
REPORT_KEY_MAP = {
    "初期証拠金": "initial_deposit",
    "ヒストリー品質": "history_quality",
    "総損益": "total_net_profit",
    "総利益": "gross_profit",
    "総損失": "gross_loss",
    "プロフィットファクター": "profit_factor",
    "期待利得": "expected_payoff",
    "シャープレシオ": "sharpe_ratio",
    "取引数": "total_trades",
    "ショート (勝率 %)": "short_trades",
    "ロング (勝率 %)": "long_trades",
    "残高相対ドローダウン": "balance_drawdown_relative",
    "証拠金相対ドローダウン": "equity_drawdown_relative",
}
RUNTIME_FILES = [
    "terminal64.exe",
    "metatester64.exe",
    "MetaEditor64.exe",
    "Terminal.ico",
]


@dataclass(frozen=True)
class TesterJob:
    slug: str
    job_set: str
    expert: str
    source_rel: str
    symbol: str
    period: str
    notes: str = ""


@dataclass(frozen=True)
class PortablePaths:
    root_win: str
    root_wsl: Path
    terminal_exe_win: str
    config_dir: Path
    experts_dir: Path
    reports_dir: Path


class _TextCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.items: list[str] = []

    def handle_data(self, data: str) -> None:
        text = data.strip()
        if text:
            self.items.append(text)


def _job(
    slug: str,
    job_set: str,
    expert: str,
    source_rel: str,
    symbol: str,
    period: str,
    notes: str = "",
) -> TesterJob:
    return TesterJob(
        slug=slug,
        job_set=job_set,
        expert=expert,
        source_rel=source_rel,
        symbol=symbol,
        period=period,
        notes=notes,
    )


JOB_REGISTRY: tuple[TesterJob, ...] = (
    _job(
        "legend-perfect-order-sma",
        "legend",
        "Legend_PerfectOrderSMA.ex5",
        "src/mt5/legend_batch1/Legend_PerfectOrderSMA.mq5",
        "USDJPY",
        "M30",
    ),
    _job(
        "legend-simple-momentum-sync",
        "legend",
        "Legend_SimpleMomentumSync.ex5",
        "src/mt5/legend_batch1/Legend_SimpleMomentumSync.mq5",
        "USDJPY",
        "M30",
    ),
    _job(
        "legend-pullback-breakout",
        "legend",
        "Legend_PullbackBreakout.ex5",
        "src/mt5/legend_batch1/Legend_PullbackBreakout.mq5",
        "USDJPY",
        "H1",
    ),
    _job(
        "legend-trend-pullback-entry",
        "legend",
        "Legend_TrendPullbackEntry.ex5",
        "src/mt5/legend_batch2/Legend_TrendPullbackEntry.mq5",
        "USDJPY",
        "M15",
    ),
    _job(
        "legend-sweet-chariot-sma-40",
        "legend",
        "Legend_SweetChariotSMA40.ex5",
        "src/mt5/legend_batch2/Legend_SweetChariotSMA40.mq5",
        "USDJPY",
        "M30",
    ),
    _job(
        "legend-macd-above-zero-cross",
        "legend",
        "Legend_MACDAboveZeroCross.ex5",
        "src/mt5/legend_batch2/Legend_MACDAboveZeroCross.mq5",
        "USDJPY",
        "H4",
    ),
    _job(
        "legend-macd-signal-cross",
        "legend",
        "Legend_MACDSignalCross.ex5",
        "src/mt5/legend_batch3/Legend_MACDSignalCross.mq5",
        "USDJPY",
        "H4",
    ),
    _job(
        "legend-london-breakout-v1",
        "legend",
        "Legend_LondonBreakoutV1.ex5",
        "src/mt5/legend_batch3/Legend_LondonBreakoutV1.mq5",
        "USDJPY",
        "H1",
    ),
    _job(
        "legend-rsi-reversion-v1",
        "legend",
        "Legend_RSIReversionV1.ex5",
        "src/mt5/legend_batch3/Legend_RSIReversionV1.mq5",
        "USDJPY",
        "M5",
    ),
    _job(
        "historical-s-bred222-trend-core",
        "historical_s",
        "HistS_Bred222TrendCore.ex5",
        "src/mt5/historical_s_batch1/HistS_Bred222TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred723-trend-core",
        "historical_s",
        "HistS_Bred723TrendCore.ex5",
        "src/mt5/historical_s_batch1/HistS_Bred723TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-recruit-rnd-trend-cross",
        "historical_s",
        "HistS_RecruitRndTrendCross.ex5",
        "src/mt5/historical_s_batch1/HistS_RecruitRndTrendCross.mq5",
        "USDJPY",
        "D1",
    ),
    _job(
        "historical-s-bred508-trend-core",
        "historical_s",
        "HistS_Bred508TrendCore.ex5",
        "src/mt5/historical_s_batch2/HistS_Bred508TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred794-trend-core",
        "historical_s",
        "HistS_Bred794TrendCore.ex5",
        "src/mt5/historical_s_batch2/HistS_Bred794TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred128-trend-core",
        "historical_s",
        "HistS_Bred128TrendCore.ex5",
        "src/mt5/historical_s_batch2/HistS_Bred128TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred187-trend-core",
        "historical_s",
        "HistS_Bred187TrendCore.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred187TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred436-trend-core",
        "historical_s",
        "HistS_Bred436TrendCore.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred436TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred940-trend-core",
        "historical_s",
        "HistS_Bred940TrendCore.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred940TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred586-trend-core",
        "historical_s",
        "HistS_Bred586TrendCore.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred586TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred458-trend-core",
        "historical_s",
        "HistS_Bred458TrendCore.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred458TrendCore.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred139-trend-core-3979972567",
        "historical_s",
        "HistS_Bred139TrendCore_3979972567.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972567.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
    _job(
        "historical-s-bred139-trend-core-3979972610",
        "historical_s",
        "HistS_Bred139TrendCore_3979972610.ex5",
        "src/mt5/historical_s_batch3/HistS_Bred139TrendCore_3979972610.mq5",
        "USDJPY",
        "H1",
        notes="Original row uses timeframe=3600; tester maps it to H1.",
    ),
)


def windows_to_wsl_path(path_text: str) -> Path:
    match = re.match(r"^([A-Za-z]):[\\/]*(.*)$", path_text)
    if not match:
        raise ValueError(f"Unsupported Windows path: {path_text}")
    drive = match.group(1).lower()
    tail = match.group(2).replace("\\", "/").strip("/")
    root = Path("/mnt") / drive
    if not tail:
        return root
    return root / Path(tail)


def wsl_to_windows_path(path: str | Path) -> str:
    resolved = Path(path).resolve()
    parts = resolved.parts
    if len(parts) >= 3 and parts[1] == "mnt" and len(parts[2]) == 1:
        drive = f"{parts[2].upper()}:"
        tail = "\\".join(parts[3:])
        return f"{drive}\\{tail}" if tail else drive
    raise ValueError(f"Unsupported WSL path: {resolved}")


def source_terminal_root_win(win_user: str, terminal_id: str) -> str:
    return rf"C:\Users\{win_user}\AppData\Roaming\MetaQuotes\Terminal\{terminal_id}"


def join_windows_path(base: str, *parts: str) -> str:
    value = base.rstrip("\\/")
    for part in parts:
        value = value + "\\" + part.strip("\\/")
    return value


def _copy_if_needed(src: Path, dst: Path) -> None:
    dst.parent.mkdir(parents=True, exist_ok=True)
    if dst.exists():
        try:
            if src.stat().st_size == dst.stat().st_size and int(src.stat().st_mtime) == int(dst.stat().st_mtime):
                return
        except OSError:
            pass
    shutil.copy2(src, dst)


def _copy_tree_contents(src: Path, dst: Path) -> None:
    if not src.exists():
        raise FileNotFoundError(f"Source directory not found: {src}")
    dst.mkdir(parents=True, exist_ok=True)
    for child in src.iterdir():
        target = dst / child.name
        if child.is_dir():
            shutil.copytree(child, target, dirs_exist_ok=True)
        else:
            shutil.copy2(child, target)


def ensure_portable_terminal(
    portable_root_win: str,
    install_win: str,
    source_terminal_win: str,
) -> PortablePaths:
    portable_root = windows_to_wsl_path(portable_root_win)
    install_root = windows_to_wsl_path(install_win)
    source_terminal_root = windows_to_wsl_path(source_terminal_win)
    config_src = source_terminal_root / "config"

    portable_root.mkdir(parents=True, exist_ok=True)
    for filename in RUNTIME_FILES:
        _copy_if_needed(install_root / filename, portable_root / filename)

    config_dir = portable_root / "config"
    _copy_tree_contents(config_src, config_dir)

    experts_dir = portable_root / "MQL5" / "Experts"
    reports_dir = portable_root / "reports"
    experts_dir.mkdir(parents=True, exist_ok=True)
    reports_dir.mkdir(parents=True, exist_ok=True)

    return PortablePaths(
        root_win=portable_root_win,
        root_wsl=portable_root,
        terminal_exe_win=join_windows_path(portable_root_win, "terminal64.exe"),
        config_dir=config_dir,
        experts_dir=experts_dir,
        reports_dir=reports_dir,
    )


def ensure_expert_files(paths: PortablePaths, source_terminal_win: str, jobs: Sequence[TesterJob]) -> None:
    source_experts = windows_to_wsl_path(source_terminal_win) / "MQL5" / "Experts"
    for job in jobs:
        src = source_experts / job.expert
        if not src.exists():
            raise FileNotFoundError(f"Compiled expert not found: {src}")
        _copy_if_needed(src, paths.experts_dir / job.expert)


def _normalize_label(label: str) -> str:
    return label.strip().lstrip("\ufeff").rstrip(":：")


def decode_report_html(path: Path) -> str:
    raw = path.read_bytes()
    encodings = ["utf-16le", "utf-8", "utf-8-sig", "cp932"]
    for encoding in encodings:
        try:
            return raw.decode(encoding)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="ignore")


def parse_report_summary(path: Path) -> dict[str, str]:
    parser = _TextCollector()
    parser.feed(decode_report_html(path))
    items = [_normalize_label(item) for item in parser.items]
    summary: dict[str, str] = {}
    for index, item in enumerate(items[:-1]):
        key = REPORT_KEY_MAP.get(item)
        if not key:
            continue
        value = items[index + 1]
        if value in REPORT_KEY_MAP:
            continue
        summary[key] = value
    return summary


def find_report_artifacts(report_root: Path, prefix: str) -> list[Path]:
    artifacts = [
        path
        for path in report_root.glob(f"{prefix}*")
        if path.is_file() and path.suffix.lower() in {".htm", ".png"}
    ]
    return sorted(artifacts, key=lambda path: path.name)


def render_tester_ini(
    job: TesterJob,
    report_rel_path: str,
    from_date: str,
    to_date: str,
    deposit: int = 10000,
    currency: str = "USD",
    leverage: int = 100,
) -> str:
    lines = [
        "[Tester]",
        f"Expert={job.expert}",
        f"Symbol={job.symbol}",
        f"Period={job.period}",
        "Model=4",
        "ExecutionMode=0",
        "Optimization=0",
        "OptimizationCriterion=0",
        f"FromDate={from_date}",
        f"ToDate={to_date}",
        f"Deposit={deposit}",
        f"Currency={currency}",
        f"Leverage={leverage}",
        "UseLocal=1",
        "UseRemote=0",
        "UseCloud=0",
        f"Report={report_rel_path}",
        "ReplaceReport=1",
        "ShutdownTerminal=1",
    ]
    return "\n".join(lines) + "\n"


def select_jobs(job_set: str, requested: Sequence[str] | None = None) -> list[TesterJob]:
    normalized_set = job_set.strip().lower()
    if normalized_set not in {"legend", "historical_s", "all"}:
        raise ValueError(f"Unknown job-set: {job_set}")

    jobs = list(JOB_REGISTRY if normalized_set == "all" else [job for job in JOB_REGISTRY if job.job_set == normalized_set])
    if not requested:
        return jobs

    lookup = {job.slug: job for job in jobs}
    selected: list[TesterJob] = []
    missing: list[str] = []
    for raw_name in requested:
        name = raw_name.strip().lower()
        job = lookup.get(name)
        if not job:
            missing.append(raw_name)
            continue
        if job not in selected:
            selected.append(job)
    if missing:
        raise ValueError(f"Unknown job slug(s): {', '.join(missing)}")
    return selected


def _default_from_date() -> str:
    return (date.today() - timedelta(days=365)).strftime("%Y.%m.%d")


def _default_to_date() -> str:
    return date.today().strftime("%Y.%m.%d")


def _write_tester_ini(paths: PortablePaths, job: TesterJob, report_prefix: str, from_date: str, to_date: str) -> Path:
    report_rel_path = f"reports\\{report_prefix}"
    config_path = paths.config_dir / f"{report_prefix}.ini"
    config_path.write_text(
        render_tester_ini(job, report_rel_path=report_rel_path, from_date=from_date, to_date=to_date),
        encoding="ascii",
    )
    return config_path


def _launch_portable_terminal(terminal_exe_win: str, ini_path: Path, skipupdate: str, timeout_sec: int) -> dict[str, str | int]:
    script = """
param(
  [string]$terminal,
  [string]$ini,
  [string]$skip,
  [int]$timeoutSec
)
$ErrorActionPreference = 'Stop'
$proc = Start-Process -FilePath $terminal -ArgumentList '/portable',('/config:' + $ini),('/skipupdate:' + $skip) -PassThru
Write-Output ('PID=' + $proc.Id)
$done = $proc.WaitForExit($timeoutSec * 1000)
if (-not $done) {
  try { Stop-Process -Id $proc.Id -Force } catch {}
  throw ('timeout waiting for portable tester after ' + $timeoutSec + 's')
}
Write-Output ('EXIT=' + $proc.ExitCode)
"""
    script_dir = windows_to_wsl_path(terminal_exe_win).parent / "tmp"
    script_dir.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        "w",
        suffix=".ps1",
        delete=False,
        encoding="utf-8",
        dir=script_dir,
    ) as handle:
        handle.write(script)
        script_path = Path(handle.name)
    try:
        proc = subprocess.run(
            [
                "powershell.exe",
                "-NoProfile",
                "-NonInteractive",
                "-ExecutionPolicy",
                "Bypass",
                "-File",
                wsl_to_windows_path(script_path),
                terminal_exe_win,
                wsl_to_windows_path(ini_path),
                skipupdate,
                str(timeout_sec),
            ],
            capture_output=True,
            text=True,
            check=False,
        )
    finally:
        script_path.unlink(missing_ok=True)

    stdout = proc.stdout.strip()
    stderr = proc.stderr.strip()
    if proc.returncode != 0:
        raise RuntimeError(f"Portable terminal launch failed rc={proc.returncode} stderr={stderr or '-'}")

    pid_match = re.search(r"PID=(\d+)", stdout)
    exit_match = re.search(r"EXIT=(-?\d+)", stdout)
    return {
        "stdout": stdout,
        "stderr": stderr,
        "pid": int(pid_match.group(1)) if pid_match else 0,
        "exit_code": int(exit_match.group(1)) if exit_match else 0,
    }


def _copy_artifacts_to_repo(artifacts: Iterable[Path], destination: Path) -> list[Path]:
    destination.mkdir(parents=True, exist_ok=True)
    copied: list[Path] = []
    for src in artifacts:
        dst = destination / src.name
        shutil.copy2(src, dst)
        copied.append(dst)
    return copied


def run_job(
    job: TesterJob,
    paths: PortablePaths,
    reports_root: Path,
    from_date: str,
    to_date: str,
    run_id: str,
    skipupdate: str,
    timeout_sec: int,
) -> dict[str, object]:
    report_prefix = f"{run_id}_{job.slug}"
    ini_path = _write_tester_ini(paths, job, report_prefix, from_date, to_date)

    launch_started_at = datetime.now(timezone.utc)
    launch_meta = _launch_portable_terminal(paths.terminal_exe_win, ini_path, skipupdate=skipupdate, timeout_sec=timeout_sec)
    launch_finished_at = datetime.now(timezone.utc)

    artifacts = find_report_artifacts(paths.reports_dir, report_prefix)
    if not artifacts:
        raise FileNotFoundError(f"No report artifacts produced for {report_prefix} in {paths.reports_dir}")

    destination = reports_root / run_id / job.slug
    copied_artifacts = _copy_artifacts_to_repo(artifacts, destination)
    html_reports = [path for path in copied_artifacts if path.suffix.lower() == ".htm"]
    summary = parse_report_summary(html_reports[0]) if html_reports else {}

    manifest = {
        "job": asdict(job),
        "run_id": run_id,
        "from_date": from_date,
        "to_date": to_date,
        "portable_root_win": paths.root_win,
        "config_path": str(ini_path),
        "report_prefix": report_prefix,
        "artifacts": [str(path) for path in copied_artifacts],
        "summary": summary,
        "launch": {
            "pid": launch_meta["pid"],
            "exit_code": launch_meta["exit_code"],
            "stdout": launch_meta["stdout"],
            "stderr": launch_meta["stderr"],
            "started_at": launch_started_at.isoformat(),
            "finished_at": launch_finished_at.isoformat(),
        },
    }
    manifest_path = destination / f"{report_prefix}.summary.json"
    manifest_path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    return manifest


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Run isolated MT5 Strategy Tester jobs for Legend/Historical MQ5 inventory.")
    parser.add_argument("--job-set", choices=["legend", "historical_s", "all"], default="all")
    parser.add_argument("--job", action="append", default=[], help="Restrict to a specific job slug. Repeatable.")
    parser.add_argument("--from-date", default=_default_from_date(), help="Tester FromDate in YYYY.MM.DD")
    parser.add_argument("--to-date", default=_default_to_date(), help="Tester ToDate in YYYY.MM.DD")
    parser.add_argument("--reports-dir", default=str(DEFAULT_REPORTS_DIR), help="Repo directory for copied report artifacts")
    parser.add_argument("--portable-root-win", default=DEFAULT_PORTABLE_ROOT_WIN)
    parser.add_argument("--install-win", default=DEFAULT_INSTALL_WIN)
    parser.add_argument("--win-user", default=DEFAULT_WIN_USER)
    parser.add_argument("--terminal-id", default=DEFAULT_TERMINAL_ID)
    parser.add_argument("--skipupdate", default=DEFAULT_SKIPUPDATE)
    parser.add_argument("--timeout-sec", type=int, default=900)
    parser.add_argument("--list-jobs", action="store_true", help="List job slugs and exit")
    return parser


def main() -> int:
    parser = _build_parser()
    args = parser.parse_args()

    jobs = select_jobs(args.job_set, args.job)
    if args.list_jobs:
        for job in jobs:
            print(f"{job.slug}\t{job.job_set}\t{job.expert}\t{job.period}\t{job.symbol}")
        return 0

    reports_root = Path(args.reports_dir).expanduser()
    source_terminal_win = source_terminal_root_win(args.win_user, args.terminal_id)
    portable_paths = ensure_portable_terminal(
        portable_root_win=args.portable_root_win,
        install_win=args.install_win,
        source_terminal_win=source_terminal_win,
    )
    ensure_expert_files(portable_paths, source_terminal_win=source_terminal_win, jobs=jobs)

    run_id = datetime.now(timezone.utc).strftime("run_%Y%m%d_%H%M%S")
    manifests: list[dict[str, object]] = []
    for job in jobs:
        manifest = run_job(
            job,
            paths=portable_paths,
            reports_root=reports_root,
            from_date=args.from_date,
            to_date=args.to_date,
            run_id=run_id,
            skipupdate=args.skipupdate,
            timeout_sec=args.timeout_sec,
        )
        manifests.append(manifest)
        summary = manifest.get("summary", {})
        print(
            "[OK]",
            job.slug,
            f"net={summary.get('total_net_profit', '-')}",
            f"pf={summary.get('profit_factor', '-')}",
            f"trades={summary.get('total_trades', '-')}",
        )

    aggregate_path = reports_root / run_id / "inventory_tester_manifest.json"
    aggregate_path.parent.mkdir(parents=True, exist_ok=True)
    aggregate_path.write_text(json.dumps(manifests, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(f"[OK] aggregate_manifest={aggregate_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
