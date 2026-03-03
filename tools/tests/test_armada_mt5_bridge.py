import json
import subprocess
import sys
from pathlib import Path

from tools.ops.armada_mt5_bridge import build_mt5_bridge_plan

ROOT = Path(__file__).resolve().parents[2]


def _sample_report() -> dict:
    return {
        "players": [
            {
                "profile": {"key": "taiki"},
                "selected": [
                    {
                        "candidate": {
                            "indicator_type": "ema",
                            "sma_short": 24,
                            "sma_long": 160,
                            "timeframe": 60,
                            "sl": 1.5,
                            "tp": 3.0,
                        },
                        "selection_score": 1.2,
                    },
                    {
                        "candidate": {
                            "indicator_type": "vwapvr",
                            "sma_short": 20,
                            "sma_long": 140,
                            "timeframe": 60,
                            "sl": 1.2,
                            "tp": 3.6,
                        },
                        "selection_score": 1.1,
                    },
                    {
                        "candidate": {
                            "indicator_type": "ema",
                            "sma_short": 28,
                            "sma_long": 140,
                            "timeframe": 120,
                            "sl": 1.8,
                            "tp": 2.8,
                        },
                        "selection_score": 1.0,
                    },
                ],
            },
            {
                "profile": {"key": "kojirin"},
                "selected": [
                    {
                        "candidate": {
                            "indicator_type": "ema",
                            "sma_short": 30,
                            "sma_long": 140,
                            "timeframe": 240,
                            "sl": 2.0,
                            "tp": 3.0,
                        },
                        "selection_score": 0.9,
                    }
                ],
            },
        ]
    }


def test_build_mt5_bridge_plan_filters_and_writes_configs(tmp_path: Path) -> None:
    plan = build_mt5_bridge_plan(
        report=_sample_report(),
        run_tag="20260303",
        config_dir=tmp_path,
        top_per_player=2,
        allowed_indicators={"ema"},
        symbol="XAUUSD",
        magic_base=561000,
        pullback_atr=0.2,
        session_start_hour_utc=7,
        session_end_hour_utc=19,
        min_atr_ratio_to_median=0.0,
        max_atr_ratio_to_median=999.0,
        max_spread_points=90.0,
    )

    assert plan["summary"]["planned_runs"] == 2
    assert plan["summary"]["skipped"]["unsupported_indicator"] == 1
    assert plan["summary"]["skipped"]["unsupported_timeframe"] == 0

    first = plan["runs"][0]
    cfg = json.loads(Path(first["config_path"]).read_text(encoding="utf-8"))
    assert cfg["timeframe"] == "H1"
    assert cfg["fast_ema"] == 24
    assert cfg["slow_ema"] == 160
    assert cfg["sl_atr"] == 1.5
    assert cfg["tp_atr"] == 3.0
    assert cfg["max_spread_points"] == 90.0
    assert len(cfg["comment"]) <= 31


def test_build_mt5_bridge_plan_assigns_unique_magic_and_run_id(tmp_path: Path) -> None:
    plan = build_mt5_bridge_plan(
        report=_sample_report(),
        run_tag="20260303",
        config_dir=tmp_path,
        top_per_player=1,
        allowed_indicators={"ema"},
        symbol="XAUUSD",
        magic_base=562000,
        pullback_atr=0.25,
        session_start_hour_utc=0,
        session_end_hour_utc=23,
        min_atr_ratio_to_median=0.9,
        max_atr_ratio_to_median=1.4,
        max_spread_points=80.0,
    )

    assert plan["summary"]["planned_runs"] == 2
    run_ids = [row["run_id"] for row in plan["runs"]]
    magics = [row["magic"] for row in plan["runs"]]
    assert len(set(run_ids)) == len(run_ids)
    assert magics == [562000, 562001]
    assert all("tools/xau_autobot_trial_v2_start.sh" in row["start_command"] for row in plan["runs"])


def test_cli_writes_plan_json(tmp_path: Path) -> None:
    report_path = tmp_path / "replica.json"
    report_path.write_text(json.dumps(_sample_report(), ensure_ascii=True), encoding="utf-8")
    output_path = tmp_path / "plan.json"
    config_dir = tmp_path / "configs"

    proc = subprocess.run(
        [
            sys.executable,
            str(ROOT / "tools" / "ops" / "armada_mt5_bridge.py"),
            "--replica-report",
            str(report_path),
            "--output",
            str(output_path),
            "--config-dir",
            str(config_dir),
            "--top-per-player",
            "1",
            "--allowed-indicators",
            "ema",
        ],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )

    assert proc.returncode == 0, proc.stderr
    assert output_path.exists()
