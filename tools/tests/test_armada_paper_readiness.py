import json
import sqlite3
from pathlib import Path

from tools.ops.armada_paper_readiness import build_armada_paper_readiness_report


def _create_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(
        """
        CREATE TABLE trade_logs (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          timestamp INTEGER,
          strategy_name TEXT,
          pnl REAL,
          execution_mode TEXT NOT NULL DEFAULT 'LIVE'
        );
        CREATE TABLE dryrun_slippage_samples (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          strategy_name TEXT NOT NULL,
          sample_abs_pips REAL NOT NULL,
          observed_at INTEGER NOT NULL
        );
        """
    )
    conn.commit()


def _write_deploy_readiness(path: Path, players: list[dict]) -> None:
    payload = {"players": players, "overall": {"deploy_decision": "保留"}}
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")


def test_build_report_holds_when_paper_evidence_is_short(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [
        {"player": "taiki", "classification": "保留"},
        {"player": "kojirin", "classification": "保留"},
    ]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    conn.execute(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        (1700000000, "sample_live", 0.01, "LIVE"),
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_unit",
    )

    assert report["summary"]["decision"] == "HOLD"
    assert report["summary"]["paper_trade_count"] == 0
    assert report["summary"]["slippage_sample_count"] == 0
    assert "paper_evidence_shortage: 0/20" in report["summary"]["blockers"]
    assert "slippage_samples_shortage: 0/20" in report["summary"]["blockers"]
    assert report["metrics"]["compat_with_live"]["trades"] == 1
    assert all(p["classification"] == "保留" for p in report["players"])


def test_build_report_goes_when_paper_and_slippage_thresholds_met(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [
        {"player": "taiki", "classification": "保留"},
        {"player": "kojirin", "classification": "再探索"},
    ]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    rows = []
    ts = 1700000000
    for i in range(20):
        pnl = 1.2 if i % 2 == 0 else -0.4
        rows.append((ts + i, f"paper_{i}", pnl, "PAPER"))
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        rows,
    )
    slip_rows = [(f"paper_{i}", 1.0 + (i * 0.05), ts + i) for i in range(20)]
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        slip_rows,
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_unit_go",
    )

    assert report["summary"]["decision"] == "GO"
    assert report["summary"]["paper_trade_count"] == 20
    assert report["summary"]["slippage_sample_count"] == 20
    assert report["summary"]["blockers"] == []
    assert report["metrics"]["paper_primary"]["profit_factor"] == 3.0
    assert report["players"][0]["classification"] == "投入可"
    assert report["players"][0]["l3_ready"] is True
    assert report["players"][1]["classification"] == "再探索"
    assert report["players"][1]["l3_ready"] is False


def test_build_report_computes_monthly_return_with_universal_timestamps(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [{"player": "taiki", "classification": "保留"}]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        [
            # Universal-time (1900 epoch): 2026-02-24 UTC
            (3980928858, "paper_u1", 10.0, "PAPER"),
            (3980928860, "paper_u2", -4.0, "PAPER"),
            # Unix-time (1970 epoch): 2023-11-14 UTC
            (1700000000, "paper_x1", 1.0, "PAPER"),
        ],
    )
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        [(f"paper_{i}", 1.0, 1700000000 + i) for i in range(20)],
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_monthly_unit",
        paper_min_trades=1,
        slippage_min_samples=1,
    )

    monthly = report["metrics"]["paper_primary_monthly"]
    assert monthly["baseline_equity"] == 100000.0
    assert monthly["latest_month"]["month"] == "2026-02"
    assert monthly["latest_month"]["trade_count"] == 2
    assert abs(monthly["latest_month"]["net_pnl"] - 6.0) < 1e-9
    # 6 / 100000 * 100 = 0.006%
    assert abs(monthly["latest_month"]["return_pct"] - 0.006) < 1e-12


def test_build_report_holds_when_monthly_return_target_not_met(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [{"player": "taiki", "classification": "保留"}]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    ts = 3980928858  # 2026-02-24 (universal-time)
    rows = [(ts + i, f"paper_{i}", 0.05, "PAPER") for i in range(20)]  # net=1.0
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        rows,
    )
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        [(f"paper_{i}", 1.0, ts + i) for i in range(20)],
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_monthly_target_unit",
        paper_min_trades=20,
        slippage_min_samples=20,
        monthly_baseline_equity=100000.0,
        monthly_return_target_pct=1.0,
    )

    assert report["summary"]["decision"] == "HOLD"
    assert report["summary"]["latest_month"] == "2026-02"
    assert abs(report["summary"]["latest_month_return_pct"] - 0.001) < 1e-12
    assert "monthly_return_below_target: 0.001000<1.000000" in report["summary"]["blockers"]


def test_build_report_excludes_synthetic_probe_rows_by_default(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [{"player": "taiki", "classification": "保留"}]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    ts = 1700000000
    rows = []
    for i in range(20):
        rows.append((ts + i, f"Synthetic-Close-Probe-v50_7-P{i:02d}", 0.2, "SHADOW"))
    rows.append((ts + 100, "real_shadow_1", 0.2, "SHADOW"))
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        rows,
    )
    slip_rows = [(f"Synthetic-Close-Probe-v50_7-P{i:02d}", 0.8, ts + i) for i in range(20)]
    slip_rows.append(("real_shadow_1", 0.8, ts + 100))
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        slip_rows,
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_probe_excluded",
    )

    assert report["summary"]["decision"] == "HOLD"
    assert report["summary"]["paper_trade_count"] == 1
    assert report["summary"]["slippage_sample_count"] == 1
    assert "paper_evidence_shortage: 1/20" in report["summary"]["blockers"]
    assert "slippage_samples_shortage: 1/20" in report["summary"]["blockers"]


def test_build_report_can_include_synthetic_probe_rows_when_explicit(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [{"player": "taiki", "classification": "保留"}]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    ts = 1700000000
    rows = []
    for i in range(20):
        rows.append((ts + i, f"Synthetic-Close-Probe-v50_7-P{i:02d}", 0.2, "SHADOW"))
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        rows,
    )
    slip_rows = [(f"Synthetic-Close-Probe-v50_7-P{i:02d}", 0.8, ts + i) for i in range(20)]
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        slip_rows,
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_probe_included",
        excluded_strategy_prefixes=(),
    )

    assert report["summary"]["decision"] == "GO"
    assert report["summary"]["paper_trade_count"] == 20
    assert report["summary"]["slippage_sample_count"] == 20


def test_build_report_uses_recent_window_for_runtime_guard(tmp_path: Path) -> None:
    db_path = tmp_path / "swimmy.db"
    deploy_path = tmp_path / "deploy.json"
    deploy_proxy_path = tmp_path / "deploy_proxy.json"
    b1r_path = tmp_path / "b1r.json"
    b1r_path.write_text("{}", encoding="utf-8")
    players = [{"player": "taiki", "classification": "保留"}]
    _write_deploy_readiness(deploy_path, players)
    _write_deploy_readiness(deploy_proxy_path, players)

    conn = sqlite3.connect(str(db_path))
    _create_schema(conn)
    ts = 1700000000
    rows = []
    # Older losses make all-history net negative.
    for i in range(10):
        rows.append((ts + i, f"paper_old_{i}", -1.0, "PAPER"))
    # Recent 20 trades are positive.
    for i in range(20):
        rows.append((ts + 100 + i, f"paper_recent_{i}", 0.3, "PAPER"))
    conn.executemany(
        "INSERT INTO trade_logs(timestamp, strategy_name, pnl, execution_mode) VALUES(?,?,?,?)",
        rows,
    )
    slip_rows = [(f"paper_recent_{i}", 0.9, ts + 100 + i) for i in range(20)]
    conn.executemany(
        "INSERT INTO dryrun_slippage_samples(strategy_name, sample_abs_pips, observed_at) VALUES(?,?,?)",
        slip_rows,
    )
    conn.commit()
    conn.close()

    report = build_armada_paper_readiness_report(
        db_path=db_path,
        b1r_summary_path=b1r_path,
        deploy_readiness_path=deploy_path,
        deploy_readiness_proxy_path=deploy_proxy_path,
        title="armada_paper_readiness_runtime_window",
    )

    assert report["summary"]["paper_trade_count"] == 30
    assert report["summary"]["slippage_sample_count"] == 20
    assert report["metrics"]["paper_primary"]["net_pnl"] == -4.0
    assert report["summary"]["decision"] == "GO"
