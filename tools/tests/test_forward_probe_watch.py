from tools.ops import forward_probe_watch as watch


def test_assess_probe_growth_alert_on_zero_increment_after_interval() -> None:
    verdict = watch.assess_probe_growth(
        previous_trades=12,
        current_trades=12,
        elapsed_seconds=3600,
        interval_seconds=3600,
    )

    assert verdict["status"] == "ALERT"
    assert verdict["delta"] == 0
    assert "delta=0" in verdict["reason"]


def test_assess_probe_growth_ok_when_increment_positive() -> None:
    verdict = watch.assess_probe_growth(
        previous_trades=12,
        current_trades=14,
        elapsed_seconds=3900,
        interval_seconds=3600,
    )

    assert verdict["status"] == "OK"
    assert verdict["delta"] == 2


def test_assess_probe_growth_skips_before_interval() -> None:
    verdict = watch.assess_probe_growth(
        previous_trades=12,
        current_trades=12,
        elapsed_seconds=900,
        interval_seconds=3600,
    )

    assert verdict["status"] == "SKIP"
    assert verdict["delta"] == 0


def test_build_alert_message_includes_context() -> None:
    msg = watch.build_alert_message(
        strategy_name="Bred-Bred--518-Gen37-N3980103299-243",
        symbol="USDJPY",
        current_trades=0,
        elapsed_seconds=7200,
        decision="FORWARD_RUNNING",
        reason="elapsed=5/30 days",
    )

    assert "FORWARD probe stalled" in msg
    assert "Bred-Bred--518-Gen37-N3980103299-243" in msg
    assert "USDJPY" in msg
    assert "elapsed=5/30 days" in msg
