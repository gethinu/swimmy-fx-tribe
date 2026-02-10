import sys
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

import pattern_similarity_service


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


if __name__ == "__main__":
    main()
    print("OK")

