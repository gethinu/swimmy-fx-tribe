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

from aux_sexp import parse_aux_request, sexp_response


def _assert_raises(fn):
    ok = False
    try:
        fn()
    except Exception:
        ok = True
    assert ok, "expected error"


def main():
    ok = parse_aux_request('((type . "DATA_KEEPER") (schema_version . 1) (action . "STATUS"))')
    assert ok["type"] == "DATA_KEEPER"
    assert ok["schema_version"] == 1
    _assert_raises(lambda: parse_aux_request('((schema_version . 1))'))
    out = sexp_response({"type": "DATA_KEEPER_RESULT", "schema_version": 1, "status": "ok"})
    assert "schema_version" in out


if __name__ == "__main__":
    main()
    print("OK")
