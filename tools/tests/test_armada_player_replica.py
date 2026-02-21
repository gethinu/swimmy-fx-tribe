import json
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))
SRC_PY = ROOT / "src" / "python"
if str(SRC_PY) not in sys.path:
    sys.path.insert(0, str(SRC_PY))

from tools.ops.armada_player_replica import (
    _strength_verdict,
    PlayerProfile,
    build_walkforward_windows,
    build_candidate_pool,
    core_bt_floor_pass,
    compute_cpcv_summary,
    compute_replica_score,
    compute_selection_score,
    cpcv_is_strong,
    load_profiles,
    parse_indicator_filter,
    parse_args,
    normalize_backtest_response,
    tempo_bucket,
)


def _write_profiles_config(path: Path) -> None:
    path.write_text(
        json.dumps(
            {
                "players": [
                    {
                        "key": "taiki",
                        "label": "taiki",
                        "cluster": "core",
                        "target": {
                            "profit_factor": 1.99,
                            "drawdown_percent": 10.93,
                            "trades": 272,
                            "avg_hold_hours": 6.53,
                        },
                    },
                    {
                        "key": "hiro",
                        "label": "hiro",
                        "cluster": "aggressive",
                        "target": {
                            "profit_factor": 1.71,
                            "drawdown_percent": 17.56,
                            "trades": 172,
                            "avg_hold_hours": 12.50,
                        },
                    },
                ]
            },
            ensure_ascii=False,
        ),
        encoding="utf-8",
    )


def test_load_profiles_respects_cluster_and_player_filter(tmp_path: Path) -> None:
    config = tmp_path / "profiles.json"
    _write_profiles_config(config)

    core_only = load_profiles(config, include_aggressive=False, players=None)
    assert [p.key for p in core_only] == ["taiki"]

    filtered = load_profiles(config, include_aggressive=True, players={"hiro"})
    assert [p.key for p in filtered] == ["hiro"]


def test_tempo_bucket_thresholds() -> None:
    assert tempo_bucket(900) == "fast"
    assert tempo_bucket(600) == "mid"
    assert tempo_bucket(280) == "slow"


def test_build_candidate_pool_is_deterministic_per_seed() -> None:
    profile = PlayerProfile(
        key="taiki",
        label="taiki",
        cluster="core",
        target_profit_factor=1.99,
        target_drawdown_ratio=0.1093,
        target_trades=272,
        target_avg_hold_hours=6.53,
    )
    first = build_candidate_pool(profile, candidates_per_player=12, seed=20260220)
    second = build_candidate_pool(profile, candidates_per_player=12, seed=20260220)
    assert first == second
    assert len(first) == 12


def test_build_candidate_pool_includes_extended_indicator_types() -> None:
    profile = PlayerProfile(
        key="hiro",
        label="hiro",
        cluster="aggressive",
        target_profit_factor=1.71,
        target_drawdown_ratio=0.1756,
        target_trades=172,
        target_avg_hold_hours=12.5,
    )
    pool = build_candidate_pool(profile, candidates_per_player=99999, seed=20260220)
    indicator_types = {c.indicator_type for c in pool}
    assert {"sma", "ema", "rsi", "bb", "macd", "stoch", "vwap", "volsma", "vpoc", "vwapvr"}.issubset(indicator_types)
    assert len(pool) == len(set(pool)), "candidate pool should not contain exact duplicates"


def test_compute_replica_score_prefers_closer_profile_match() -> None:
    profile = PlayerProfile(
        key="pandajiro",
        label="pandajiro",
        cluster="core",
        target_profit_factor=1.79,
        target_drawdown_ratio=0.0606,
        target_trades=871,
        target_avg_hold_hours=8.47,
    )

    good_metrics = {
        "profit_factor": 1.75,
        "max_drawdown": 0.065,
        "trades": 850,
        "sharpe": 0.25,
    }
    bad_metrics = {
        "profit_factor": 0.9,
        "max_drawdown": 0.35,
        "trades": 35,
        "sharpe": -0.3,
    }

    assert compute_replica_score(profile, good_metrics) > compute_replica_score(profile, bad_metrics)


def test_normalize_backtest_response_accepts_sexp_result() -> None:
    sexp = (
        '((type . "BACKTEST_RESULT") '
        '(result . ((strategy_name . "UT") (profit_factor . 1.21) '
        '(max_drawdown . 0.07) (trades . 320) (sharpe . 0.12))))'
    )
    result = normalize_backtest_response(sexp)
    assert result["strategy_name"] == "UT"
    assert result["profit_factor"] == 1.21
    assert result["trades"] == 320


def test_build_candidate_pool_supports_indicator_filter() -> None:
    profile = PlayerProfile(
        key="hiro",
        label="hiro",
        cluster="aggressive",
        target_profit_factor=1.71,
        target_drawdown_ratio=0.1756,
        target_trades=172,
        target_avg_hold_hours=12.5,
    )
    only_vwapvr = parse_indicator_filter("vwapvr")
    pool = build_candidate_pool(
        profile,
        candidates_per_player=99999,
        seed=20260220,
        indicators=only_vwapvr,
    )
    assert pool, "vwapvr filter should still produce candidates"
    assert {c.indicator_type for c in pool} == {"vwapvr"}


def test_compute_cpcv_summary_calculates_pass_rate_and_medians() -> None:
    folds = [
        {"profit_factor": 1.20, "sharpe": 0.05, "trades": 25},
        {"profit_factor": 0.98, "sharpe": -0.02, "trades": 30},
        {"profit_factor": 1.40, "sharpe": 0.12, "trades": 40},
        {"profit_factor": 1.10, "sharpe": 0.01, "trades": 18},  # below min evidence -> ignored
    ]
    summary = compute_cpcv_summary(
        folds,
        min_fold_trades=20,
        pf_floor=1.05,
        sharpe_floor=0.0,
    )
    assert summary["folds"] == 4
    assert summary["valid_folds"] == 3
    assert summary["passed_folds"] == 2
    assert abs(summary["pass_rate"] - (2 / 3)) < 1e-9
    assert abs(summary["median_profit_factor"] - 1.20) < 1e-9
    assert abs(summary["median_sharpe"] - 0.05) < 1e-9


def test_cpcv_is_strong_requires_quality_and_coverage() -> None:
    strong_summary = {
        "valid_folds": 5,
        "pass_rate": 0.8,
        "median_profit_factor": 1.18,
        "median_sharpe": 0.04,
    }
    weak_summary = {
        "valid_folds": 2,
        "pass_rate": 1.0,
        "median_profit_factor": 1.35,
        "median_sharpe": 0.20,
    }
    assert cpcv_is_strong(strong_summary)
    assert not cpcv_is_strong(weak_summary)


def test_strength_verdict_core_requires_cpcv_when_enabled() -> None:
    profile = PlayerProfile(
        key="taiki",
        label="taiki",
        cluster="core",
        target_profit_factor=1.99,
        target_drawdown_ratio=0.1093,
        target_trades=272,
        target_avg_hold_hours=6.53,
    )
    bt = {"profit_factor": 1.35, "sharpe": 0.20, "max_drawdown": 0.06, "trades": 120}
    oos = {"profit_factor": 1.15, "sharpe": 0.02, "trades": 30}
    weak_cpcv = {"valid_folds": 4, "pass_rate": 0.25, "median_profit_factor": 1.0, "median_sharpe": -0.01}
    verdict = _strength_verdict(
        profile,
        bt,
        oos,
        weak_cpcv,
        require_cpcv_for_core=True,
    )
    assert verdict["bt_ok"] is True
    assert verdict["oos_ok"] is True
    assert verdict["cpcv_ok"] is False
    assert verdict["is_strong"] is False


def test_strength_verdict_aggressive_does_not_require_cpcv_by_default() -> None:
    profile = PlayerProfile(
        key="hiro",
        label="hiro",
        cluster="aggressive",
        target_profit_factor=1.71,
        target_drawdown_ratio=0.1756,
        target_trades=172,
        target_avg_hold_hours=12.50,
    )
    bt = {"profit_factor": 1.35, "sharpe": 0.20, "max_drawdown": 0.06, "trades": 120}
    oos = {"profit_factor": 1.15, "sharpe": 0.02, "trades": 30}
    weak_cpcv = {"valid_folds": 4, "pass_rate": 0.25, "median_profit_factor": 1.0, "median_sharpe": -0.01}
    verdict = _strength_verdict(
        profile,
        bt,
        oos,
        weak_cpcv,
        require_cpcv_for_core=True,
    )
    assert verdict["bt_ok"] is True
    assert verdict["oos_ok"] is True
    assert verdict["cpcv_ok"] is False
    assert verdict["is_strong"] is True


def test_build_walkforward_windows_spreads_folds_across_series() -> None:
    windows = build_walkforward_windows(
        total_rows=1000,
        folds=5,
        test_ratio=0.2,
        min_test_rows=100,
    )
    assert len(windows) == 5
    assert windows[0] == (0, 200)
    assert windows[-1] == (800, 1000)
    assert all((end - start) == 200 for start, end in windows)


def test_build_walkforward_windows_returns_empty_when_data_too_short() -> None:
    windows = build_walkforward_windows(
        total_rows=150,
        folds=5,
        test_ratio=0.2,
        min_test_rows=100,
    )
    assert windows == []


def test_parse_args_accepts_cpcv_flags() -> None:
    args = parse_args(
        [
            "--cpcv-folds",
            "6",
            "--cpcv-test-ratio",
            "0.25",
            "--cpcv-min-fold-trades",
            "30",
            "--cpcv-require-for-core",
        ]
    )
    assert args.cpcv_folds == 6
    assert abs(args.cpcv_test_ratio - 0.25) < 1e-9
    assert args.cpcv_min_fold_trades == 30
    assert args.cpcv_require_for_core is True


def test_core_bt_floor_pass_applies_only_to_core_cluster() -> None:
    core_profile = PlayerProfile(
        key="taiki",
        label="taiki",
        cluster="core",
        target_profit_factor=1.99,
        target_drawdown_ratio=0.1093,
        target_trades=272,
        target_avg_hold_hours=6.53,
    )
    aggr_profile = PlayerProfile(
        key="hiro",
        label="hiro",
        cluster="aggressive",
        target_profit_factor=1.71,
        target_drawdown_ratio=0.1756,
        target_trades=172,
        target_avg_hold_hours=12.50,
    )
    weak_bt = {"profit_factor": 1.08, "sharpe": 0.03, "max_drawdown": 0.02, "trades": 300}
    assert not core_bt_floor_pass(core_profile, weak_bt)
    assert core_bt_floor_pass(aggr_profile, weak_bt), "aggressive should bypass core floor"


def test_compute_selection_score_penalizes_weak_core_even_if_replica_close() -> None:
    profile = PlayerProfile(
        key="taiki",
        label="taiki",
        cluster="core",
        target_profit_factor=1.99,
        target_drawdown_ratio=0.1093,
        target_trades=272,
        target_avg_hold_hours=6.53,
    )
    close_but_weak = {"profit_factor": 1.08, "sharpe": 0.03, "max_drawdown": 0.03, "trades": 280}
    farther_but_stronger = {"profit_factor": 1.32, "sharpe": 0.15, "max_drawdown": 0.07, "trades": 260}

    weak_score = compute_selection_score(profile, close_but_weak)
    strong_score = compute_selection_score(profile, farther_but_stronger)
    assert strong_score > weak_score


def test_parse_args_accepts_selection_flags() -> None:
    args = parse_args(
        [
            "--selection-strength-weight",
            "0.75",
            "--core-bt-pf-floor",
            "1.25",
            "--core-bt-sharpe-floor",
            "0.08",
            "--core-bt-dd-cap",
            "0.10",
            "--core-bt-trade-ratio-floor",
            "0.20",
            "--core-strength-penalty",
            "1.50",
        ]
    )
    assert abs(args.selection_strength_weight - 0.75) < 1e-9
    assert abs(args.core_bt_pf_floor - 1.25) < 1e-9
    assert abs(args.core_bt_sharpe_floor - 0.08) < 1e-9
    assert abs(args.core_bt_dd_cap - 0.10) < 1e-9
    assert abs(args.core_bt_trade_ratio_floor - 0.20) < 1e-9
    assert abs(args.core_strength_penalty - 1.50) < 1e-9
