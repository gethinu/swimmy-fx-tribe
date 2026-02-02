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

    print("ok")


if __name__ == "__main__":
    main()
