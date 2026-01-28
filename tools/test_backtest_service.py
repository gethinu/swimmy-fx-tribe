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

    print("ok")


if __name__ == "__main__":
    main()
