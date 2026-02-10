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

    add_tick = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "ADD_TICK") (symbol . "USDJPY") (tick . ((timestamp . 1709234567) (bid . 145.205) (ask . 145.218) (volume . 12))))'
    )
    parsed_add = parse_sexp_alist(add_tick)
    assert parsed_add["status"] == "ok"

    get_ticks = data_keeper.handle_request_sexp(
        '((type . "DATA_KEEPER") (schema_version . 1) (action . "GET_TICKS") (symbol . "USDJPY") (count . 10))'
    )
    parsed_get = parse_sexp_alist(get_ticks)
    assert parsed_get["status"] == "ok"
    assert parsed_get["count"] >= 1
    ticks = parsed_get.get("ticks") or []
    assert ticks[0]["timestamp"] == 1709234567


if __name__ == "__main__":
    main()
    print("OK")
