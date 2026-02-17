#!/usr/bin/env python3
"""Update weather probability calibration using rolling backtests.

Runs three accuracy-only backtests:
1) train window -> writes calibration JSON
2) eval window (uncalibrated baseline)
3) eval window (with calibration)

Outputs a compact JSON summary with date ranges and metric deltas.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Dict, List, Mapping, MutableSequence, Sequence, Tuple


DEFAULT_FORECAST_MODELS: Tuple[str, ...] = ("gfs_seamless", "ecmwf_ifs025")


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


def normalize_forecast_models(raw_models: Sequence[str], *, defaults: Sequence[str] = DEFAULT_FORECAST_MODELS) -> List[str]:
    out: List[str] = []
    seen: set[str] = set()
    for item in raw_models or ():
        for part in str(item or "").split(","):
            name = str(part or "").strip()
            if not name or name in seen:
                continue
            seen.add(name)
            out.append(name)
    if out:
        return out
    for part in defaults or ():
        name = str(part or "").strip()
        if not name or name in seen:
            continue
        seen.add(name)
        out.append(name)
    return out


def parse_json_from_stdout(text: str) -> Mapping[str, Any]:
    raw = str(text or "").strip()
    if not raw:
        return {}
    # Fast path
    try:
        payload = json.loads(raw)
        return payload if isinstance(payload, Mapping) else {}
    except json.JSONDecodeError:
        pass
    # Fallback: parse from the last object-looking chunk.
    for idx in range(len(raw) - 1, -1, -1):
        if raw[idx] != "{":
            continue
        chunk = raw[idx:]
        try:
            payload = json.loads(chunk)
        except json.JSONDecodeError:
            continue
        if isinstance(payload, Mapping):
            return payload
    return {}


def build_backtest_cmd(
    *,
    python_bin: str,
    backtest_script: Path,
    start_date: date,
    end_date: date,
    forecast_models: Sequence[str],
    write_calibration: str = "",
    calibration_file: str = "",
    write_report: str = "",
) -> List[str]:
    cmd: List[str] = [
        str(python_bin),
        str(backtest_script),
        "--closed",
        "--start-date",
        start_date.isoformat(),
        "--end-date",
        end_date.isoformat(),
        "--skip-trading",
    ]
    for model_name in forecast_models:
        cmd.extend(["--forecast-model", str(model_name)])
    if str(write_calibration or "").strip():
        cmd.extend(["--write-calibration", str(write_calibration)])
    if str(calibration_file or "").strip():
        cmd.extend(["--calibration-file", str(calibration_file)])
    if str(write_report or "").strip():
        cmd.extend(["--write-report", str(write_report)])
    return cmd


def run_backtest(cmd: Sequence[str]) -> Tuple[int, Mapping[str, Any], str, str]:
    proc = subprocess.run(list(cmd), capture_output=True, text=True, check=False)
    payload = parse_json_from_stdout(proc.stdout)
    return int(proc.returncode), payload, str(proc.stdout or ""), str(proc.stderr or "")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--python-bin", default=sys.executable)
    parser.add_argument(
        "--backtest-script",
        default=str(BASE_DIR / "tools" / "polymarket_weather_backtest.py"),
    )
    parser.add_argument(
        "--calibration-file",
        default=str(BASE_DIR / "data" / "models" / "polymarket_weather" / "calibration.json"),
    )
    parser.add_argument(
        "--write-report",
        default=str(BASE_DIR / "data" / "reports" / "weather_calibration_update_latest.json"),
    )
    parser.add_argument("--train-days", type=int, default=120)
    parser.add_argument("--eval-days", type=int, default=30)
    parser.add_argument(
        "--end-date",
        default="",
        help="Evaluation end date (UTC, YYYY-MM-DD). Default: yesterday UTC.",
    )
    parser.add_argument(
        "--forecast-model",
        action="append",
        default=[],
        help="Open-Meteo model name (repeatable, comma-separated allowed).",
    )
    args = parser.parse_args()

    today_utc = datetime.now(timezone.utc).date()
    end_date = date.fromisoformat(str(args.end_date)) if str(args.end_date or "").strip() else (today_utc - timedelta(days=1))

    train_days = max(14, int(args.train_days))
    eval_days = max(7, int(args.eval_days))

    eval_start = end_date - timedelta(days=eval_days - 1)
    train_end = eval_start - timedelta(days=1)
    train_start = train_end - timedelta(days=train_days - 1)

    models = normalize_forecast_models(args.forecast_model or [])
    calibration_file = str(Path(str(args.calibration_file)).expanduser())
    backtest_script = Path(str(args.backtest_script)).expanduser()

    train_cmd = build_backtest_cmd(
        python_bin=str(args.python_bin),
        backtest_script=backtest_script,
        start_date=train_start,
        end_date=train_end,
        forecast_models=models,
        write_calibration=calibration_file,
    )
    base_eval_cmd = build_backtest_cmd(
        python_bin=str(args.python_bin),
        backtest_script=backtest_script,
        start_date=eval_start,
        end_date=end_date,
        forecast_models=models,
    )
    calib_eval_cmd = build_backtest_cmd(
        python_bin=str(args.python_bin),
        backtest_script=backtest_script,
        start_date=eval_start,
        end_date=end_date,
        forecast_models=models,
        calibration_file=calibration_file,
    )

    train_rc, train_report, train_out, train_err = run_backtest(train_cmd)
    base_rc, base_report, base_out, base_err = run_backtest(base_eval_cmd)
    calib_rc, calib_report, calib_out, calib_err = run_backtest(calib_eval_cmd)

    def _metric(report: Mapping[str, Any], key: str) -> Any:
        acc = report.get("accuracy") if isinstance(report, Mapping) else None
        return acc.get(key) if isinstance(acc, Mapping) else None

    summary: Dict[str, Any] = {
        "schema_version": 1,
        "generated_at": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "models": list(models),
        "ranges": {
            "train": {"start": train_start.isoformat(), "end": train_end.isoformat()},
            "eval": {"start": eval_start.isoformat(), "end": end_date.isoformat()},
        },
        "commands": {
            "train": train_cmd,
            "eval_uncalibrated": base_eval_cmd,
            "eval_calibrated": calib_eval_cmd,
        },
        "status": {
            "train_rc": int(train_rc),
            "eval_uncalibrated_rc": int(base_rc),
            "eval_calibrated_rc": int(calib_rc),
            "ok": bool(train_rc == 0 and base_rc == 0 and calib_rc == 0),
        },
        "metrics": {
            "train": {
                "brier": _metric(train_report, "brier"),
                "logloss": _metric(train_report, "logloss"),
                "resolved_markets": _metric(train_report, "resolved_markets"),
            },
            "eval_uncalibrated": {
                "brier": _metric(base_report, "brier"),
                "logloss": _metric(base_report, "logloss"),
                "resolved_markets": _metric(base_report, "resolved_markets"),
            },
            "eval_calibrated": {
                "brier": _metric(calib_report, "brier"),
                "logloss": _metric(calib_report, "logloss"),
                "resolved_markets": _metric(calib_report, "resolved_markets"),
            },
        },
    }

    brier_u = summary["metrics"]["eval_uncalibrated"]["brier"]
    brier_c = summary["metrics"]["eval_calibrated"]["brier"]
    logloss_u = summary["metrics"]["eval_uncalibrated"]["logloss"]
    logloss_c = summary["metrics"]["eval_calibrated"]["logloss"]

    def _delta(a: Any, b: Any) -> Any:
        try:
            return round(float(b) - float(a), 6)
        except (TypeError, ValueError):
            return None

    summary["metrics"]["delta_calibrated_minus_uncalibrated"] = {
        "brier": _delta(brier_u, brier_c),
        "logloss": _delta(logloss_u, logloss_c),
    }

    # Keep tails for debugging when any run fails.
    if not summary["status"]["ok"]:
        summary["debug"] = {
            "train_stderr_tail": train_err[-2000:],
            "train_stdout_tail": train_out[-2000:],
            "eval_uncalibrated_stderr_tail": base_err[-2000:],
            "eval_uncalibrated_stdout_tail": base_out[-2000:],
            "eval_calibrated_stderr_tail": calib_err[-2000:],
            "eval_calibrated_stdout_tail": calib_out[-2000:],
        }

    text = json.dumps(summary, ensure_ascii=False, indent=2)
    print(text)

    report_path = Path(str(args.write_report)).expanduser()
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(text + "\n", encoding="utf-8")

    return 0 if summary["status"]["ok"] else 2


if __name__ == "__main__":
    raise SystemExit(main())
