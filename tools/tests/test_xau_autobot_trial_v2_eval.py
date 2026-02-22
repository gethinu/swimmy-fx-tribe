import json
import os
import subprocess
import tempfile
from datetime import datetime, timedelta, timezone
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools" / "xau_autobot_trial_v2_eval.sh"


def _write_fake_python(path: Path) -> None:
    script = r"""#!/usr/bin/env bash
set -euo pipefail

if [ "${1:-}" = "-c" ] && [[ "${2:-}" == *"import MetaTrader5"* ]]; then
  exit 0
fi

if [[ "${1:-}" == */xau_autobot_live_report.py ]]; then
  exec python3 - "$@" <<'PY'
import json
import os
import sys
from datetime import datetime, timezone, timedelta
from pathlib import Path

args = sys.argv[1:]
capture_path = os.environ.get("FAKE_LIVE_ARGS_PATH", "")
if capture_path:
    Path(capture_path).write_text(json.dumps(args, ensure_ascii=True), encoding="utf-8")
write_report = None
i = 0
while i < len(args):
    if args[i] == "--write-report":
        write_report = args[i + 1]
        i += 2
        continue
    i += 1
if not write_report:
    raise SystemExit("missing --write-report")

end_utc = datetime.now(timezone.utc)
start_utc = end_utc - timedelta(days=14)
payload = {
    "timestamp": end_utc.isoformat(),
    "symbol": "XAUUSD",
    "magic": 560072,
    "comment_prefix": "xau_autobot_trial_v2_20260222",
    "start_utc": start_utc.isoformat(),
    "end_utc": end_utc.isoformat(),
    "history_deals_scanned": 10.0,
    "summary": {
        "closed_positions": 0.0,
        "win_rate": 0.0,
        "net_profit": 0.0,
        "gross_profit": 0.0,
        "gross_loss": 0.0,
        "avg_win": 0.0,
        "avg_loss": 0.0,
        "profit_factor": 0.0,
        "max_drawdown_abs": 0.0,
    },
    "diagnostics": {
        "total_deals": 10.0,
        "tradable_deals": 10.0,
        "after_symbol_filter": 3.0,
        "after_magic_filter": 0.0,
        "after_comment_prefix_filter": 0.0,
    },
}
target = Path(write_report)
target.parent.mkdir(parents=True, exist_ok=True)
target.write_text(json.dumps(payload, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")
print(json.dumps({"written_report": str(target)}, ensure_ascii=True))
PY
fi

if [[ "${1:-}" == */xau_autobot_trial_judge.py ]]; then
  exec python3 - "$@" <<'PY'
import json
import sys
from datetime import datetime, timezone
from pathlib import Path

args = sys.argv[1:]
live_report = None
write_report = None
i = 0
while i < len(args):
    if args[i] == "--live-report":
        live_report = args[i + 1]
        i += 2
        continue
    if args[i] == "--write-report":
        write_report = args[i + 1]
        i += 2
        continue
    i += 1
if not live_report or not write_report:
    raise SystemExit("missing --live-report/--write-report")

payload = {
    "generated_at": datetime.now(timezone.utc).isoformat(),
    "live_report": str(live_report),
    "verdict": "INVALID_TRIAL",
    "trial_valid": False,
    "invalid_reasons": ["after_magic_filter", "after_comment_prefix_filter"],
    "window_days": 14.0,
    "summary": {
        "closed_positions": 0.0,
        "profit_factor": 0.0,
        "win_rate": 0.0,
        "net_profit": 0.0,
    },
    "thresholds": {
        "min_days": 14.0,
        "min_closed_positions": 30.0,
        "min_profit_factor": 1.1,
        "min_win_rate": 0.42,
        "min_net_profit": 0.0,
    },
    "checks": {
        "window_days": True,
        "closed_positions": False,
        "profit_factor": False,
        "win_rate": False,
        "net_profit": False,
    },
    "failed_checks": ["closed_positions", "profit_factor", "win_rate", "net_profit"],
}
target = Path(write_report)
target.parent.mkdir(parents=True, exist_ok=True)
target.write_text(json.dumps(payload, ensure_ascii=True, indent=2) + "\n", encoding="utf-8")
print(json.dumps(payload, ensure_ascii=True))
raise SystemExit("trial verdict: INVALID_TRIAL")
PY
fi

exec python3 "$@"
"""
    path.write_text(script, encoding="utf-8")
    path.chmod(0o755)


def test_eval_updates_latest_even_on_invalid_trial_exit() -> None:
    with tempfile.TemporaryDirectory() as td:
        tempdir = Path(td)
        fake_python = tempdir / "fake_python.sh"
        _write_fake_python(fake_python)

        trial_config = tempdir / "trial_config.json"
        trial_config.write_text(
            json.dumps(
                {
                    "symbol": "XAUUSD",
                    "magic": 560072,
                    "comment": "xau_autobot_trial_v2_20260222",
                },
                ensure_ascii=True,
            )
            + "\n",
            encoding="utf-8",
        )

        run_id = "trial_v2_test_eval_alias"
        live_report = tempdir / f"xau_autobot_live_report_{run_id}.json"
        judge_report = tempdir / f"xau_autobot_trial_judge_{run_id}.json"
        live_latest = tempdir / "xau_autobot_live_report_trial_v2_latest.json"
        judge_latest = tempdir / "xau_autobot_trial_judge.json"

        env = dict(os.environ)
        env.update(
            {
                "XAU_AUTOBOT_PYTHON": str(fake_python),
                "XAU_AUTOBOT_TRIAL_CONFIG": str(trial_config),
                "XAU_AUTOBOT_TRIAL_RUN_ID": run_id,
                "XAU_AUTOBOT_TRIAL_LIVE_REPORT": str(live_report),
                "XAU_AUTOBOT_TRIAL_JUDGE_REPORT": str(judge_report),
                "XAU_AUTOBOT_TRIAL_LIVE_REPORT_LATEST": str(live_latest),
                "XAU_AUTOBOT_TRIAL_JUDGE_REPORT_LATEST": str(judge_latest),
            }
        )

        proc = subprocess.run(
            ["bash", str(SCRIPT)],
            cwd=ROOT,
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )

        assert proc.returncode == 1
        assert live_report.exists()
        assert judge_report.exists()
        assert live_latest.exists()
        assert judge_latest.exists()
        assert live_latest.read_text(encoding="utf-8") == live_report.read_text(encoding="utf-8")
        assert judge_latest.read_text(encoding="utf-8") == judge_report.read_text(encoding="utf-8")


def test_eval_uses_fixed_window_from_run_metadata_when_available() -> None:
    with tempfile.TemporaryDirectory() as td:
        tempdir = Path(td)
        fake_python = tempdir / "fake_python.sh"
        _write_fake_python(fake_python)

        trial_config = tempdir / "trial_config.json"
        trial_config.write_text(
            json.dumps(
                {
                    "symbol": "XAUUSD",
                    "magic": 560072,
                    "comment": "xau_autobot_trial_v2_20260222",
                },
                ensure_ascii=True,
            )
            + "\n",
            encoding="utf-8",
        )

        run_id = "trial_v2_test_fixed_window"
        started_at = "2026-01-01T00:00:00+00:00"
        run_meta = tempdir / "current_run.json"
        run_meta.write_text(
            json.dumps(
                {
                    "run_id": run_id,
                    "started_at_utc": started_at,
                },
                ensure_ascii=True,
            )
            + "\n",
            encoding="utf-8",
        )

        live_report = tempdir / f"xau_autobot_live_report_{run_id}.json"
        judge_report = tempdir / f"xau_autobot_trial_judge_{run_id}.json"
        live_latest = tempdir / "xau_autobot_live_report_trial_v2_latest.json"
        judge_latest = tempdir / "xau_autobot_trial_judge.json"
        live_args_path = tempdir / "live_args.json"

        env = dict(os.environ)
        env.update(
            {
                "XAU_AUTOBOT_PYTHON": str(fake_python),
                "XAU_AUTOBOT_TRIAL_CONFIG": str(trial_config),
                "XAU_AUTOBOT_TRIAL_RUN_ID": run_id,
                "XAU_AUTOBOT_TRIAL_RUN_META_PATH": str(run_meta),
                "XAU_AUTOBOT_TRIAL_DAYS": "14",
                "XAU_AUTOBOT_TRIAL_LIVE_REPORT": str(live_report),
                "XAU_AUTOBOT_TRIAL_JUDGE_REPORT": str(judge_report),
                "XAU_AUTOBOT_TRIAL_LIVE_REPORT_LATEST": str(live_latest),
                "XAU_AUTOBOT_TRIAL_JUDGE_REPORT_LATEST": str(judge_latest),
                "FAKE_LIVE_ARGS_PATH": str(live_args_path),
            }
        )

        proc = subprocess.run(
            ["bash", str(SCRIPT)],
            cwd=ROOT,
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )

        assert proc.returncode == 1
        assert live_args_path.exists()
        args = json.loads(live_args_path.read_text(encoding="utf-8"))

        assert "--start-utc" in args
        assert "--end-utc" in args
        assert "--days" not in args

        start_value = args[args.index("--start-utc") + 1]
        end_value = args[args.index("--end-utc") + 1]

        assert datetime.fromisoformat(start_value.replace("Z", "+00:00")) == datetime.fromisoformat(started_at)
        assert datetime.fromisoformat(end_value.replace("Z", "+00:00")) == (
            datetime.fromisoformat(started_at) + timedelta(days=14)
        ).astimezone(timezone.utc)
