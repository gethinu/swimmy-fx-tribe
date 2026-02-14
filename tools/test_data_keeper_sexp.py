import sys
import os
import tempfile
from pathlib import Path


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))
sys.path.insert(0, str(BASE_DIR / "tools"))

from sexp_utils import parse_sexp_alist

# Isolate tick persistence away from the real repo `data/`.
_tmp = tempfile.TemporaryDirectory()
os.environ["SWIMMY_TICKS_DIR"] = str(Path(_tmp.name) / "ticks")

import data_keeper


def main():
    response = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "STATUS"))'
    )
    parsed = parse_sexp_alist(response)
    assert parsed["type"] == "DATA_KEEPER_RESULT"
    assert parsed["schema_version"] == 1
    assert parsed["status"] in ("ok", "running")

    err = data_keeper.handle_request_sexp('((schema_version . 1))')
    parsed_err = parse_sexp_alist(err)
    assert parsed_err["status"] == "error"

    add1 = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "ADD_TICK") '
        '(symbol . "USDJPY") '
        '(tick . ((timestamp . 1709234567) (bid . 145.205) (ask . 145.218) (volume . 12))))'
    )
    parsed_add1 = parse_sexp_alist(add1)
    assert parsed_add1["status"] == "ok"
    assert parsed_add1["symbol"] == "USDJPY"

    add2 = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "ADD_TICK") '
        '(symbol . "USDJPY") '
        '(tick . ((timestamp . 1709234568) (bid . 145.206) (ask . 145.219) (volume . 8))))'
    )
    parsed_add2 = parse_sexp_alist(add2)
    assert parsed_add2["status"] == "ok"

    get_ticks = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "GET_TICKS") '
        '(symbol . "USDJPY") (count . 2))'
    )
    parsed_ticks = parse_sexp_alist(get_ticks)
    assert parsed_ticks["status"] == "ok"
    assert parsed_ticks["symbol"] == "USDJPY"
    assert parsed_ticks["count"] == 2
    ticks = parsed_ticks["ticks"]
    assert isinstance(ticks, list)
    assert ticks[0]["timestamp"] >= ticks[1]["timestamp"]
    assert "bid" in ticks[0]
    assert "ask" in ticks[0]

    get_ticks_window = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "GET_TICKS") '
        '(symbol . "USDJPY") (count . 10) (start_time . 1709234568) (end_time . 1709234568))'
    )
    parsed_ticks_window = parse_sexp_alist(get_ticks_window)
    assert parsed_ticks_window["status"] == "ok"
    assert parsed_ticks_window["count"] == 1


if __name__ == "__main__":
    main()
    print("OK")
