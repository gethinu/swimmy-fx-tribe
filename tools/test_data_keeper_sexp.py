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


if __name__ == "__main__":
    main()
    print("OK")
