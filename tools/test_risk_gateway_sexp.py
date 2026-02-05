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
import risk_gateway


def main():
    response = risk_gateway.handle_request_sexp(
        '((type . "RISK_GATEWAY") (schema_version . 1) (action . "CHECK_RISK") '
        '(side . "BUY") (symbol . "USDJPY") (lot . 0.01) (daily_pnl . 0.0) '
        '(equity . 100000.0) (consecutive_losses . 0))'
    )
    parsed = parse_sexp_alist(response)
    assert parsed["type"] == "RISK_GATEWAY_RESULT"
    assert parsed["schema_version"] == 1
    assert parsed["status"] in ("APPROVED", "DENIED")

    reset = risk_gateway.handle_request_sexp(
        '((type . "RISK_GATEWAY") (schema_version . 1) (action . "RESET"))'
    )
    parsed_reset = parse_sexp_alist(reset)
    assert parsed_reset["status"] == "RESET_COMPLETE"

    err = risk_gateway.handle_request_sexp('((schema_version . 1))')
    parsed_err = parse_sexp_alist(err)
    assert parsed_err["status"] == "ERROR"


if __name__ == "__main__":
    main()
    print("OK")
