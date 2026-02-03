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

from sexp_serialize import sexp_serialize


def _assert_in(needle, haystack, label):
    if needle not in haystack:
        raise AssertionError(f"{label} missing: {needle}\nGot: {haystack}")


def main():
    sexp = sexp_serialize({"filter_enabled": "false"})
    _assert_in("(filter_enabled . #f)", sexp, "bool coercion")

    sexp = sexp_serialize({"timeframe": 5})
    _assert_in("(timeframe . (5))", sexp, "timeframe option")

    sexp = sexp_serialize({"candles_file": "file.csv"})
    _assert_in('(candles_file . ("file.csv"))', sexp, "candles_file option")


if __name__ == "__main__":
    main()
