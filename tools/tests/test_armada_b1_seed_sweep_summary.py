import json
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
SRC_PY = ROOT / "src" / "python"
if str(SRC_PY) not in sys.path:
    sys.path.insert(0, str(SRC_PY))

from tools.ops.armada_b1_seed_sweep_summary import build_seed_sweep_summary


def _candidate(
    *,
    oos_ok: bool,
    cpcv_ok: bool = True,
    bt_ok: bool = False,
    is_strong: bool = False,
    timeframe: int = 120,
    indicator: str = "volsma",
) -> dict:
    return {
        "candidate": {
            "indicator_type": indicator,
            "timeframe": timeframe,
        },
        "verdict": {
            "bt_ok": bt_ok,
            "oos_ok": oos_ok,
            "cpcv_ok": cpcv_ok,
            "is_strong": is_strong,
        },
    }


def _write_player_replica_report(path: Path, *, taiki: list[dict], kojirin: list[dict]) -> None:
    payload = {
        "players": [
            {"profile": {"key": "taiki"}, "selected": taiki},
            {"profile": {"key": "kojirin"}, "selected": kojirin},
        ]
    }
    path.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")


def test_build_seed_sweep_summary_marks_completed_when_rule_met(tmp_path: Path) -> None:
    seed11 = tmp_path / "seed11.json"
    seed23 = tmp_path / "seed23.json"

    _write_player_replica_report(
        seed11,
        taiki=[_candidate(oos_ok=True), _candidate(oos_ok=False), _candidate(oos_ok=False)],
        kojirin=[_candidate(oos_ok=True), _candidate(oos_ok=False), _candidate(oos_ok=False)],
    )
    _write_player_replica_report(
        seed23,
        taiki=[_candidate(oos_ok=True), _candidate(oos_ok=True), _candidate(oos_ok=False)],
        kojirin=[_candidate(oos_ok=True), _candidate(oos_ok=False), _candidate(oos_ok=False)],
    )

    summary = build_seed_sweep_summary(
        players=("taiki", "kojirin"),
        seeds=(11, 23),
        report_template=str(tmp_path / "seed{seed}.json"),
        required_min_both_pass=2,
        top_per_player=3,
    )

    assert summary["missing"] == []
    assert len(summary["per_seed"]) == 2
    assert summary["aggregate"]["player_pass_counts"] == {"taiki": 2, "kojirin": 2}
    assert summary["aggregate"]["both_players_pass_count"] == 2
    assert summary["aggregate"]["total_seeds"] == 2
    assert summary["aggregate"]["b1r_completed"] is True


def test_build_seed_sweep_summary_marks_incomplete_with_missing_reports(tmp_path: Path) -> None:
    seed11 = tmp_path / "seed11.json"
    _write_player_replica_report(
        seed11,
        taiki=[_candidate(oos_ok=True), _candidate(oos_ok=False), _candidate(oos_ok=False)],
        kojirin=[_candidate(oos_ok=True), _candidate(oos_ok=False), _candidate(oos_ok=False)],
    )

    summary = build_seed_sweep_summary(
        players=("taiki", "kojirin"),
        seeds=(11, 23),
        report_template=str(tmp_path / "seed{seed}.json"),
        required_min_both_pass=2,
        top_per_player=3,
    )

    assert len(summary["missing"]) == 1
    assert summary["per_seed"][1]["status"] == "missing"
    assert summary["aggregate"]["player_pass_counts"] == {"taiki": 1, "kojirin": 1}
    assert summary["aggregate"]["both_players_pass_count"] == 1
    assert summary["aggregate"]["b1r_completed"] is False
