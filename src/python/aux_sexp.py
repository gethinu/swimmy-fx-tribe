from sexp_utils import parse_sexp_alist
from sexp_serialize import sexp_serialize

REQUIRED_KEYS = ("type", "schema_version")


def parse_aux_request(text: str) -> dict:
    data = parse_sexp_alist(text)
    missing = [key for key in REQUIRED_KEYS if key not in data]
    if missing:
        raise ValueError(f"missing required keys: {missing}")
    return data


def sexp_response(payload: dict) -> str:
    if "schema_version" not in payload:
        payload = dict(payload)
        payload["schema_version"] = 1
    return sexp_serialize(
        payload,
        symbol_value_keys=set(),
        bool_value_keys=set(),
        optional_list_keys=set(),
    )
