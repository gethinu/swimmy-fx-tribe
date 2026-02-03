import json

DEFAULT_SYMBOL_VALUE_KEYS = {"indicator_type"}
DEFAULT_BOOL_VALUE_KEYS = {"filter_enabled"}
DEFAULT_OPTIONAL_LIST_KEYS = {"data_id", "timeframe", "candles_file", "aux_candles_files", "start_time", "end_time"}


def _sexp_key(key) -> str:
    return str(key).lower()


def _sexp_symbol(value: str) -> str:
    return str(value).lower()


def coerce_bool(value) -> bool:
    if isinstance(value, bool):
        return value
    if value is None:
        return False
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return value != 0
    if isinstance(value, str):
        v = value.strip().lower()
        if v in {"", "0", "false", "nil", "null", "none", "no", "off"}:
            return False
        if v in {"1", "true", "yes", "on", "t"}:
            return True
    if isinstance(value, (list, tuple)):
        if len(value) == 0:
            return False
        if len(value) == 1:
            return coerce_bool(value[0])
    return bool(value)


def _normalize_optional_list(key_norm: str, value, optional_list_keys):
    if key_norm not in optional_list_keys:
        return value
    if value is None:
        return None
    if key_norm == "aux_candles_files":
        if not (isinstance(value, (list, tuple)) and len(value) == 1 and isinstance(value[0], (list, tuple))):
            return [value]
        return value
    if not isinstance(value, (list, tuple)):
        return [value]
    return value


def sexp_serialize(
    value,
    *,
    symbol_value_keys=None,
    bool_value_keys=None,
    optional_list_keys=None,
    key_normalizer=None,
    symbol_normalizer=None,
) -> str:
    symbol_value_keys = DEFAULT_SYMBOL_VALUE_KEYS if symbol_value_keys is None else symbol_value_keys
    bool_value_keys = DEFAULT_BOOL_VALUE_KEYS if bool_value_keys is None else bool_value_keys
    optional_list_keys = DEFAULT_OPTIONAL_LIST_KEYS if optional_list_keys is None else optional_list_keys
    key_normalizer = _sexp_key if key_normalizer is None else key_normalizer
    symbol_normalizer = _sexp_symbol if symbol_normalizer is None else symbol_normalizer

    if value is None:
        return "()"
    if isinstance(value, bool):
        return "#t" if value else "#f"
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        if isinstance(value, float):
            return format(value, ".10g")
        return str(value)
    if isinstance(value, str):
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, dict):
        parts = []
        for k, v in value.items():
            key_norm = key_normalizer(k)
            if (
                key_norm in bool_value_keys
                or key_norm.endswith("enabled")
                or key_norm.startswith("is_")
                or key_norm.startswith("has_")
            ):
                v = coerce_bool(v)
            v = _normalize_optional_list(key_norm, v, optional_list_keys)
            if v is None:
                if (
                    key_norm in bool_value_keys
                    or key_norm.endswith("enabled")
                    or key_norm.startswith("is_")
                    or key_norm.startswith("has_")
                ):
                    v = False
                else:
                    continue
            if (
                isinstance(v, list)
                and len(v) == 0
                and (
                    key_norm in bool_value_keys
                    or key_norm.endswith("enabled")
                    or key_norm.startswith("is_")
                    or key_norm.startswith("has_")
                )
            ):
                v = False
            if key_norm in symbol_value_keys and isinstance(v, str):
                v_str = symbol_normalizer(v)
            else:
                v_str = sexp_serialize(
                    v,
                    symbol_value_keys=symbol_value_keys,
                    bool_value_keys=bool_value_keys,
                    optional_list_keys=optional_list_keys,
                    key_normalizer=key_normalizer,
                    symbol_normalizer=symbol_normalizer,
                )
            parts.append(f"({key_norm} . {v_str})")
        return f"({' '.join(parts)})"
    if isinstance(value, (list, tuple)):
        return f"({' '.join(sexp_serialize(v,
                                            symbol_value_keys=symbol_value_keys,
                                            bool_value_keys=bool_value_keys,
                                            optional_list_keys=optional_list_keys,
                                            key_normalizer=key_normalizer,
                                            symbol_normalizer=symbol_normalizer) for v in value)})"
    return json.dumps(str(value), ensure_ascii=False)
