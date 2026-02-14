import sys
from pathlib import Path
import os


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))
sys.path.insert(0, str(BASE_DIR / "tools"))

# Keep contract tests deterministic even if optional DL deps are installed.
os.environ.setdefault("SWIMMY_PATTERN_EMBED_BACKEND", "pixel")

from sexp_utils import parse_sexp_alist
from aux_sexp import sexp_request

import pattern_similarity_service


def make_trend_candles(*, n: int, start_ts: int, step_sec: int, start_price: float, delta: float):
    candles = []
    price = start_price
    for i in range(n):
        ts = start_ts + i * step_sec
        o = price
        c = price + delta
        hi = max(o, c) + abs(delta) * 0.2
        lo = min(o, c) - abs(delta) * 0.2
        candles.append(
            {
                "timestamp": ts,
                "open": o,
                "high": hi,
                "low": lo,
                "close": c,
                "volume": 10,
            }
        )
        price = c
    return candles


def main():
    status = pattern_similarity_service.handle_request_sexp(
        '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "STATUS"))'
    )
    parsed = parse_sexp_alist(status)
    assert parsed["type"] == "PATTERN_SIMILARITY_RESULT"
    assert parsed["schema_version"] == 1
    assert parsed["status"] in ("ok", "error")

    # No index yet -> QUERY should error (fail-open is on the Lisp side).
    query = pattern_similarity_service.handle_request_sexp(
        '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "QUERY") (symbol . "USDJPY") (timeframe . "H1") (candles . ()))'
    )
    parsed_q = parse_sexp_alist(query)
    assert parsed_q["status"] == "error"

    bad = pattern_similarity_service.handle_request_sexp('((schema_version . 1))')
    parsed_bad = parse_sexp_alist(bad)
    assert parsed_bad["status"] == "error"

    # Build a tiny in-memory index and ensure QUERY returns probabilities.
    up = make_trend_candles(n=120, start_ts=1709230000, step_sec=3600, start_price=100.0, delta=0.1)
    down = make_trend_candles(n=120, start_ts=1709230000, step_sec=3600, start_price=200.0, delta=-0.1)
    pattern_similarity_service._reset_state_for_tests()
    pattern_similarity_service._build_index_from_inline_samples(
        symbol="USDJPY",
        timeframe="H1",
        samples=[
            {"id": "H1:USDJPY:1709230000", "label": "UP", "candles": up},
            {"id": "H1:USDJPY:1709230000:DOWN", "label": "DOWN", "candles": down},
        ],
    )

    query2_msg = sexp_request(
        {
            "type": "PATTERN_SIMILARITY",
            "schema_version": 1,
            "action": "QUERY",
            "symbol": "USDJPY",
            "timeframe": "H1",
            "k": 2,
            "candles": up,
        }
    )
    query2 = pattern_similarity_service.handle_request_sexp(query2_msg)
    parsed_q2 = parse_sexp_alist(query2)
    assert parsed_q2["status"] == "ok"
    result = parsed_q2["result"]
    assert 0.0 <= result["p_up"] <= 1.0
    assert 0.0 <= result["p_down"] <= 1.0
    assert 0.0 <= result["p_flat"] <= 1.0
    assert result["p_up"] > result["p_down"]
    assert len(result["top_k"]) == 2


if __name__ == "__main__":
    main()
    print("OK")
