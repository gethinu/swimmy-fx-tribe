#!/usr/bin/env python3
from backtest_service import _sexp_serialize, _coerce_bool


def _assert_in(needle, haystack, label):
    if needle not in haystack:
        raise AssertionError(f"{label} missing: {needle}\nGot: {haystack}")


def main():
    # Bool coercion
    assert _coerce_bool(True) is True
    assert _coerce_bool(False) is False
    assert _coerce_bool("false") is False
    assert _coerce_bool("true") is True
    assert _coerce_bool(0) is False
    assert _coerce_bool(1) is True

    sexp = _sexp_serialize({"filter_enabled": "false"})
    _assert_in("(filter_enabled . #f)", sexp, "filter_enabled false")

    # Optional list wrapping
    sexp = _sexp_serialize({"timeframe": 5})
    _assert_in("(timeframe . (5))", sexp, "timeframe option")

    sexp = _sexp_serialize({"candles_file": "file.csv"})
    _assert_in('(candles_file . ("file.csv"))', sexp, "candles_file option")

    sexp = _sexp_serialize({"aux_candles_files": ["a.csv", "b.csv"]})
    _assert_in('aux_candles_files', sexp, "aux_candles_files key")
    _assert_in('(("a.csv" "b.csv"))', sexp, "aux_candles_files nested list")

    # Incoming message parsing: JSON should never be parsed
    import backtest_service as svc
    called = {"value": False}

    def _boom(*_args, **_kwargs):
        called["value"] = True
        raise AssertionError("json.loads must not be called for incoming messages")

    orig_json_loads = svc.json.loads
    try:
        svc.json.loads = _boom
        kind, payload = svc._parse_incoming_message("not-a-sexp")
        assert kind is None and payload is None

        kind, payload = svc._parse_incoming_message("  (foo . bar)")
        assert kind == "sexpr"
        assert payload == "(foo . bar)"
    finally:
        svc.json.loads = orig_json_loads

    assert called["value"] is False

    # Incoming message preview helpers
    import os
    os.environ.pop("SWIMMY_BACKTEST_DUMP_INCOMING", None)
    assert svc._should_dump_incoming() is False
    os.environ["SWIMMY_BACKTEST_DUMP_INCOMING"] = "1"
    assert svc._should_dump_incoming() is True
    os.environ["SWIMMY_BACKTEST_DUMP_INCOMING"] = "false"
    assert svc._should_dump_incoming() is False

    preview = svc._format_incoming_preview("line1\nline2\r\n" + ("x" * 200), limit=40)
    assert "\n" not in preview and "\r" not in preview
    assert len(preview) <= 43

    # Guardian output preview helpers
    os.environ.pop("SWIMMY_BACKTEST_DUMP_GUARDIAN", None)
    assert svc._should_dump_guardian() is False
    os.environ["SWIMMY_BACKTEST_DUMP_GUARDIAN"] = "yes"
    assert svc._should_dump_guardian() is True
    os.environ["SWIMMY_BACKTEST_DUMP_GUARDIAN"] = "0"
    assert svc._should_dump_guardian() is False

    out_preview = svc._format_guardian_preview("out1\nout2\r\n" + ("y" * 200), limit=32)
    assert "\n" not in out_preview and "\r" not in out_preview
    assert len(out_preview) <= 35

    # Worker count helpers
    saved_workers = os.environ.get("SWIMMY_BACKTEST_WORKERS")
    saved_max_inflight = os.environ.get("SWIMMY_BACKTEST_MAX_INFLIGHT")
    saved_max_pending = os.environ.get("SWIMMY_BACKTEST_MAX_PENDING")
    saved_rate_limit = os.environ.get("SWIMMY_BACKTEST_RATE_LIMIT")
    orig_cpu_count = svc.os.cpu_count
    try:
        svc.os.cpu_count = lambda: 8
        os.environ.pop("SWIMMY_BACKTEST_WORKERS", None)
        assert svc._default_worker_count() == 4
        assert svc._resolve_worker_count() == 4

        os.environ["SWIMMY_BACKTEST_WORKERS"] = "3"
        assert svc._resolve_worker_count() == 3

        os.environ["SWIMMY_BACKTEST_WORKERS"] = "0"
        assert svc._resolve_worker_count() == 1

        # Runtime knob alignment helpers
        os.environ["SWIMMY_BACKTEST_WORKERS"] = "3"
        os.environ["SWIMMY_BACKTEST_MAX_PENDING"] = "1200"
        os.environ["SWIMMY_BACKTEST_RATE_LIMIT"] = "140"
        os.environ.pop("SWIMMY_BACKTEST_MAX_INFLIGHT", None)
        knobs = svc._resolve_runtime_knobs()
        assert knobs["worker_count"] == 3
        assert knobs["max_inflight"] == 6
        assert knobs["max_pending"] == 1200
        assert knobs["rate_limit"] == 140
        assert knobs["inflight_explicit"] is False

        warns = svc._knob_alignment_warnings(knobs)
        assert any("MAX_INFLIGHT" in w for w in warns)

        os.environ["SWIMMY_BACKTEST_MAX_INFLIGHT"] = "7"
        knobs = svc._resolve_runtime_knobs()
        assert knobs["max_inflight"] == 7
        assert knobs["inflight_explicit"] is True
        warns = svc._knob_alignment_warnings(knobs)
        assert not any("MAX_INFLIGHT" in w for w in warns)

        os.environ["SWIMMY_BACKTEST_MAX_INFLIGHT"] = "9"
        os.environ["SWIMMY_BACKTEST_MAX_PENDING"] = "4"
        knobs = svc._resolve_runtime_knobs()
        warns = svc._knob_alignment_warnings(knobs)
        assert any("MAX_PENDING" in w for w in warns)
    finally:
        svc.os.cpu_count = orig_cpu_count
        if saved_workers is None:
            os.environ.pop("SWIMMY_BACKTEST_WORKERS", None)
        else:
            os.environ["SWIMMY_BACKTEST_WORKERS"] = saved_workers
        if saved_max_inflight is None:
            os.environ.pop("SWIMMY_BACKTEST_MAX_INFLIGHT", None)
        else:
            os.environ["SWIMMY_BACKTEST_MAX_INFLIGHT"] = saved_max_inflight
        if saved_max_pending is None:
            os.environ.pop("SWIMMY_BACKTEST_MAX_PENDING", None)
        else:
            os.environ["SWIMMY_BACKTEST_MAX_PENDING"] = saved_max_pending
        if saved_rate_limit is None:
            os.environ.pop("SWIMMY_BACKTEST_RATE_LIMIT", None)
        else:
            os.environ["SWIMMY_BACKTEST_RATE_LIMIT"] = saved_rate_limit

    # Inject request_id into guardian result without dotted (result .) form
    sexp = '((type . "BACKTEST_RESULT") (result ((strategy_name . "UT") (sharpe . 0.1))))'
    injected = svc.BacktestService._inject_request_id_into_sexpr(sexp, "RID-LIST")
    _assert_in("request_id", injected, "request_id injected")
    _assert_in("RID-LIST", injected, "request_id value")

    # Guardian payload passthrough (range + aux fields)
    payload = svc._build_guardian_payload(
        {
            "strategy": {"name": "t"},
            "start_time": 100,
            "end_time": 200,
            "data_id": "USDJPY_M1",
            "aux_candles": [{"t": 1}],
            "aux_candles_files": ["a.csv"],
            "swap_history": [{"t": 1, "sl": 0.1, "ss": 0.1}],
            "timeframe": 1,
            "candles_file": "file.csv",
            "phase": "phase2",
            "range_id": "P2",
        }
    )
    _assert_in("start_time", payload, "start_time")
    _assert_in("end_time", payload, "end_time")
    _assert_in("data_id", payload, "data_id")
    _assert_in("aux_candles", payload, "aux_candles")
    _assert_in("aux_candles_files", payload, "aux_candles_files")
    _assert_in("swap_history", payload, "swap_history")
    _assert_in("phase", payload, "phase")
    _assert_in("range_id", payload, "range_id")
    assert payload["candles_file"] == "file.csv"
    assert payload["timeframe"] == 1

    payload_with_legacy_candles = svc._build_guardian_payload(
        {
            "strategy": {"name": "t"},
            "candles": [
                {"timestamp": 1700000010, "open": 1.0, "high": 2.0, "low": 0.5, "close": 1.5, "volume": 10.0},
                {"time": 1700000011, "o": 2.0, "h": 3.0, "l": 1.0, "c": 2.5, "v": 20.0},
            ],
            "swap_history": [
                {"timestamp": 1700000020, "sl": -1.0, "ss": 0.5},
            ],
        }
    )
    assert payload_with_legacy_candles["candles"][0]["t"] == 1700000010
    assert payload_with_legacy_candles["candles"][0]["o"] == 1.0
    assert payload_with_legacy_candles["candles"][0]["h"] == 2.0
    assert payload_with_legacy_candles["candles"][0]["l"] == 0.5
    assert payload_with_legacy_candles["candles"][0]["c"] == 1.5
    assert payload_with_legacy_candles["candles"][0]["v"] == 10.0
    assert payload_with_legacy_candles["candles"][1]["t"] == 1700000011
    assert payload_with_legacy_candles["candles"][1]["o"] == 2.0
    assert payload_with_legacy_candles["swap_history"][0]["t"] == 1700000020

    # Incoming SEXP should be normalized for bool fields before guardian parse
    raw = (
        '((action . "BACKTEST") '
        '(strategy ((name . "UT") (sma_short . 5) (sma_long . 20) '
        '(sl . 0.1) (tp . 0.2) (volume . 0.01) (indicator_type . sma) '
        '(filter_enabled . t) (filter_tf . "") (filter_period . 0) (filter_logic . ""))) '
        '(request_id . "RID-BOOL") (candles_file . "/tmp/x.csv") (timeframe . 1440))'
    )
    normalized = svc._normalize_backtest_sexpr(raw)
    _assert_in("(filter_enabled . #t)", normalized, "filter_enabled normalized true")
    if "(filter_enabled . t)" in normalized:
        raise AssertionError("filter_enabled symbol 't' should not be forwarded to guardian")

    # Incoming SEXP with dotted-pair shorthand should also normalize bools.
    raw_shorthand = (
        '((action . "BACKTEST") '
        '(strategy (name . "UT-SHORT") (sma_short . 5) (sma_long . 20) '
        '(sl . 0.1) (tp . 0.2) (volume . 0.01) (indicator_type . sma) '
        '(filter_enabled . t) (filter_tf . "") (filter_period . 0) (filter_logic . "")) '
        '(request_id . "RID-BOOL-SHORT") (data_id "USDJPY_M1") '
        '(candles_file "/tmp/x.csv") (symbol . "USDJPY") (timeframe 1440))'
    )
    normalized_shorthand = svc._normalize_backtest_sexpr(raw_shorthand)
    _assert_in("(filter_enabled . #t)", normalized_shorthand, "shorthand filter_enabled normalized true")
    if "(filter_enabled . t)" in normalized_shorthand:
        raise AssertionError("shorthand bool symbol 't' should not be forwarded to guardian")

    raw_candles = (
        '((action . "BACKTEST") '
        '(request_id . "RID-CANDLES") '
        '(strategy ((name . "UT") (sma_short . 5) (sma_long . 20) (sl . 0.1) (tp . 0.2) (volume . 0.01) (indicator_type . sma) '
        '(filter_enabled . t) (filter_tf . "") (filter_period . 0) (filter_logic . ""))) '
        '(candles (((timestamp . 1) (open . 100.5) (high . 101.0) (low . 99.8) (close . 100.2) (volume . 1200.0)) '
        '((time . 2) (o . 99.0) (h . 100.0) (l . 98.5) (c . 99.6) (v . 800.0)))) '
        '(aux_candles ()) (swap_history (( (timestamp . 3) (sl . 1.1) (ss . 1.2))))'
    )
    normalized_candles = svc._normalize_backtest_sexpr(raw_candles)
    _assert_in("(t . 1)", normalized_candles, "timestamp key normalized to t")
    _assert_in("(o . 100.5)", normalized_candles, "open key normalized to o")
    _assert_in("(h . 101)", normalized_candles, "high key normalized to h")
    _assert_in("(l . 99.8)", normalized_candles, "low key normalized to l")
    _assert_in("(c . 100.2)", normalized_candles, "close key normalized to c")
    _assert_in("(v . 1200)", normalized_candles, "volume key normalized to v")
    _assert_in("(t . 2)", normalized_candles, "time key normalized to t")
    _assert_in("(t . 3)", normalized_candles, "swap_history timestamp mapped to t")
    _assert_in("(sl . 1.1)", normalized_candles, "swap_history keeps swap fields")
    if "(timestamp . 1)" in normalized_candles:
        raise AssertionError("legacy timestamp key should be removed for candles")

    # Fallback path: even if parse fails, known bool keys should be normalized.
    orig_parse_sexp = svc.parse_sexp
    try:
        def _fail_parse(_text):
            raise svc.SexpParseError("forced")

        svc.parse_sexp = _fail_parse
        fallback = svc._normalize_backtest_sexpr(raw_shorthand)
        _assert_in("(filter_enabled . #t)", fallback, "fallback bool normalization true")
        if "(filter_enabled . t)" in fallback:
            raise AssertionError("fallback path must not keep symbol 't'")
    finally:
        svc.parse_sexp = orig_parse_sexp

    print("ok")


if __name__ == "__main__":
    main()
