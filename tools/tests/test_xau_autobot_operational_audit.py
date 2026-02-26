import json
from datetime import datetime, timezone
from pathlib import Path

from tools.xau_autobot_operational_audit import (
    evaluate_audit_status,
    load_live_metrics,
    load_runtime_metrics,
    load_runtime_metrics_from_live_reports,
)


def _write_json(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, ensure_ascii=True) + "\n", encoding="utf-8")


def test_load_runtime_metrics_handles_counter_reset(tmp_path: Path) -> None:
    runtime_file = tmp_path / "runtime.jsonl"
    rows = [
        {
            "timestamp_utc": "2026-02-26T00:00:00+00:00",
            "runtime_metrics": {
                "gate_check_count": 10,
                "gate_reject_gap_count": 2,
                "signal_counts": {"BUY": 3, "SELL": 2, "HOLD": 5},
            },
        },
        {
            "timestamp_utc": "2026-02-26T01:00:00+00:00",
            "runtime_metrics": {
                "gate_check_count": 15,
                "gate_reject_gap_count": 5,
                "signal_counts": {"BUY": 5, "SELL": 3, "HOLD": 7},
            },
        },
        {
            "timestamp_utc": "2026-02-26T02:00:00+00:00",
            "runtime_metrics": {
                "gate_check_count": 4,
                "gate_reject_gap_count": 1,
                "signal_counts": {"BUY": 1, "SELL": 1, "HOLD": 2},
            },
        },
    ]
    runtime_file.write_text("".join(json.dumps(row, ensure_ascii=True) + "\n" for row in rows), encoding="utf-8")

    metrics = load_runtime_metrics(
        runtime_globs=[str(runtime_file)],
        start_utc=datetime(2026, 2, 25, 0, 0, tzinfo=timezone.utc),
        end_utc=datetime(2026, 2, 27, 0, 0, tzinfo=timezone.utc),
        run_id_filter="",
    )

    assert metrics["snapshot_count"] == 3
    assert metrics["gate_check_count"] == 9
    assert metrics["gate_reject_gap_count"] == 4
    assert abs(metrics["gap_reject_rate"] - (4.0 / 9.0)) < 1e-9
    assert metrics["signal_counts"]["BUY"] == 3
    assert metrics["signal_counts"]["SELL"] == 2
    assert metrics["signal_counts"]["HOLD"] == 4


def test_load_live_metrics_uses_latest_per_day(tmp_path: Path) -> None:
    _write_json(
        tmp_path / "xau_autobot_live_report_trial_v2_20260226_a.json",
        {
            "timestamp": "2026-02-26T01:00:00+00:00",
            "summary": {
                "tp_sl_ratio": 0.5,
                "closed_per_day": 0.4,
                "win_rate": 0.40,
                "avg_win": 10.0,
                "avg_loss": -8.0,
            },
        },
    )
    _write_json(
        tmp_path / "xau_autobot_live_report_trial_v2_20260226_b.json",
        {
            "timestamp": "2026-02-26T09:00:00+00:00",
            "summary": {
                "tp_sl_ratio": 0.8,
                "closed_per_day": 0.9,
                "win_rate": 0.50,
                "avg_win": 12.0,
                "avg_loss": -7.0,
            },
        },
    )
    _write_json(
        tmp_path / "xau_autobot_live_report_trial_v2_20260227.json",
        {
            "timestamp": "2026-02-27T08:00:00+00:00",
            "summary": {
                "tp_sl_ratio": 0.9,
                "closed_per_day": 1.1,
                "win_rate": 0.52,
                "avg_win": 11.0,
                "avg_loss": -6.5,
            },
        },
    )

    metrics = load_live_metrics(
        live_globs=[str(tmp_path / "xau_autobot_live_report_trial_v2_*.json")],
        start_utc=datetime(2026, 2, 26, 0, 0, tzinfo=timezone.utc),
        end_utc=datetime(2026, 2, 28, 0, 0, tzinfo=timezone.utc),
        run_id_filter="",
    )

    assert metrics["report_count"] == 2
    assert abs(metrics["tp_sl_ratio"] - 0.85) < 1e-9
    assert abs(metrics["closed_per_day"] - 1.0) < 1e-9
    assert metrics["expectancy"] > 0.0


def test_load_runtime_metrics_from_live_reports(tmp_path: Path) -> None:
    _write_json(
        tmp_path / "xau_autobot_live_report_trial_v2_20260226_a.json",
        {
            "timestamp": "2026-02-26T01:00:00+00:00",
            "runtime_metrics": {
                "gate_check_count": 10,
                "gate_reject_gap_count": 2,
                "signal_counts": {"BUY": 4, "SELL": 2, "HOLD": 4},
            },
        },
    )
    _write_json(
        tmp_path / "xau_autobot_live_report_trial_v2_20260226_b.json",
        {
            "timestamp": "2026-02-26T03:00:00+00:00",
            "runtime_metrics": {
                "gate_check_count": 16,
                "gate_reject_gap_count": 5,
                "signal_counts": {"BUY": 7, "SELL": 4, "HOLD": 5},
            },
        },
    )

    metrics = load_runtime_metrics_from_live_reports(
        live_globs=[str(tmp_path / "xau_autobot_live_report_trial_v2_*.json")],
        start_utc=datetime(2026, 2, 26, 0, 0, tzinfo=timezone.utc),
        end_utc=datetime(2026, 2, 26, 23, 59, tzinfo=timezone.utc),
        run_id_filter="",
    )

    assert metrics["snapshot_count"] == 2
    assert metrics["gate_check_count"] == 6
    assert metrics["gate_reject_gap_count"] == 3
    assert abs(metrics["gap_reject_rate"] - 0.5) < 1e-9
    assert metrics["signal_counts"]["BUY"] == 3
    assert metrics["signal_counts"]["SELL"] == 2
    assert metrics["signal_counts"]["HOLD"] == 1
    assert metrics["source"] == "live_report.runtime_metrics"


def test_evaluate_audit_status_pass() -> None:
    result = evaluate_audit_status(
        runtime_metrics={
            "snapshot_count": 4,
            "gap_reject_rate": 0.3,
        },
        live_metrics={
            "report_count": 3,
            "tp_sl_ratio": 0.8,
            "closed_per_day": 0.9,
            "expectancy": 0.5,
        },
    )
    assert result["status"] == "PASS"
    assert result["checks"]["gap_reject_rate"]["ok"] is True
    assert result["checks"]["tp_sl_ratio"]["ok"] is True
    assert result["checks"]["closed_per_day"]["ok"] is True
    assert result["checks"]["expectancy"]["ok"] is True
    assert result["recommendations"] == []


def test_evaluate_audit_status_pattern_a() -> None:
    result = evaluate_audit_status(
        runtime_metrics={
            "snapshot_count": 4,
            "gap_reject_rate": 0.65,
        },
        live_metrics={
            "report_count": 3,
            "tp_sl_ratio": 0.6,
            "closed_per_day": 0.3,
            "expectancy": -0.1,
        },
    )
    assert result["status"] == "FAIL"
    assert any(item.get("pattern") == "A" for item in result["recommendations"])


def test_evaluate_audit_status_insufficient_data() -> None:
    result = evaluate_audit_status(
        runtime_metrics={
            "snapshot_count": 0,
            "gap_reject_rate": None,
        },
        live_metrics={
            "report_count": 0,
            "tp_sl_ratio": None,
            "closed_per_day": None,
            "expectancy": None,
        },
    )
    assert result["status"] == "INSUFFICIENT_DATA"
