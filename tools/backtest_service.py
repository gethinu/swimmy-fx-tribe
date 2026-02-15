#!/usr/bin/env python3
"""
Backtest Service - Independent ZMQ Service
Naval's Proposal: Decouple Backtest from Guardian main loop

Architecture:
    Brain (Lisp) --ZMQ 5580--> Backtest Service --subprocess--> guardian --backtest-only
                 <--ZMQ 5581-- Backtest Service <-------------

Ports:
    5580: PULL (receive BACKTEST commands from Brain)
    5581: PUSH (send BACKTEST_RESULT to Brain)
"""
import zmq
import json
import subprocess
import time
import os
import errno
import re
import threading
from concurrent.futures import ThreadPoolExecutor
import contextlib

import sys
from pathlib import Path
import urllib.request
import urllib.error
try:
    import fcntl  # Unix-only
except Exception:  # pragma: no cover
    fcntl = None


def _resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = _resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))

from sexp_serialize import sexp_serialize as _sexp_serialize
from sexp_serialize import coerce_bool as _coerce_bool
from sexp_utils import parse_sexp, SexpParseError

# Port configuration (env override)
def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default

BACKTEST_REQ_PORT = _env_int("SWIMMY_PORT_BACKTEST_REQ", 5580)
BACKTEST_RES_PORT = _env_int("SWIMMY_PORT_BACKTEST_RES", 5581)

_LOCK_PATH_ENV = "SWIMMY_BACKTEST_LOCK_PATH"
_LOCK_DIR_ENV = "SWIMMY_BACKTEST_LOCK_DIR"
_NO_LOCK_ENV = "SWIMMY_BACKTEST_NO_LOCK"


def _resolve_lock_path(port: int) -> Path:
    explicit = os.getenv(_LOCK_PATH_ENV, "").strip()
    if explicit:
        return Path(explicit)
    lock_dir = os.getenv(_LOCK_DIR_ENV, "/tmp").strip() or "/tmp"
    return Path(lock_dir) / f"swimmy-backtest-svc-{port}.lock"


def _acquire_single_instance_lock(lock_path: Path):
    """Ensure we don't accidentally run two backtest services on the same port."""
    if os.getenv(_NO_LOCK_ENV, "").strip().lower() in ("1", "true", "yes", "on"):
        return None
    if fcntl is None:
        return None
    try:
        lock_path.parent.mkdir(parents=True, exist_ok=True)
    except Exception:
        pass
    fh = open(lock_path, "a+", encoding="utf-8")
    try:
        fcntl.flock(fh.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        holder_pid = None
        try:
            fh.seek(0)
            holder_pid = fh.read().strip() or None
        except Exception:
            holder_pid = None
        with contextlib.suppress(Exception):
            fh.close()
        holder = f" pid={holder_pid}" if holder_pid else ""
        print(
            f"[BACKTEST-SVC] ❌ Another backtest service instance is already running "
            f"(lock={lock_path}{holder})."
        )
        print(
            f"[BACKTEST-SVC] Fix: stop the existing service, or set SWIMMY_PORT_BACKTEST_REQ "
            f"to a different port for manual runs."
        )
        raise SystemExit(0)
    try:
        fh.seek(0)
        fh.truncate()
        fh.write(str(os.getpid()))
        fh.flush()
    except Exception:
        pass
    return fh


def _default_worker_count() -> int:
    cpu = os.cpu_count() or 1
    return max(1, min(4, cpu))


def _resolve_worker_count() -> int:
    return max(1, _env_int("SWIMMY_BACKTEST_WORKERS", _default_worker_count()))


def _resolve_max_inflight(worker_count: int) -> int:
    baseline = max(1, worker_count * 2)
    return max(worker_count, _env_int("SWIMMY_BACKTEST_MAX_INFLIGHT", baseline))


def _resolve_runtime_knobs(worker_count=None) -> dict:
    """Resolve backtest knobs from env and normalize operational defaults."""
    workers = worker_count if isinstance(worker_count, int) and worker_count > 0 else _resolve_worker_count()
    max_inflight = _resolve_max_inflight(workers)
    max_pending = max(1, _env_int("SWIMMY_BACKTEST_MAX_PENDING", 500))
    rate_limit = max(1, _env_int("SWIMMY_BACKTEST_RATE_LIMIT", 5))
    inflight_explicit = bool(os.getenv("SWIMMY_BACKTEST_MAX_INFLIGHT", "").strip())
    return {
        "worker_count": workers,
        "max_inflight": max_inflight,
        "max_pending": max_pending,
        "rate_limit": rate_limit,
        "inflight_explicit": inflight_explicit,
    }


def _knob_alignment_warnings(knobs: dict):
    """Return operator-facing warnings when backtest knobs are likely misconfigured."""
    warnings = []
    if not knobs.get("inflight_explicit"):
        warnings.append(
            "[BACKTEST-SVC] ⚠️ SWIMMY_BACKTEST_MAX_INFLIGHT is unset; "
            "MAX_PENDING does not cap service inflight. Set it explicitly."
        )
    if int(knobs.get("max_pending", 0)) < int(knobs.get("max_inflight", 0)):
        warnings.append(
            "[BACKTEST-SVC] ⚠️ SWIMMY_BACKTEST_MAX_PENDING is smaller than "
            "SWIMMY_BACKTEST_MAX_INFLIGHT; sender throttling may underutilize workers."
        )
    return warnings

# Force unbuffered output for debugging
sys.stdout.reconfigure(line_buffering=True)

_ALERT_WEBHOOK_ENV = ("SWIMMY_DISCORD_ALERTS", "SWIMMY_DISCORD_SYSTEM_LOGS")
_STRATEGY_FIELDS = {
    "name",
    "sma_short",
    "sma_long",
    "sl",
    "tp",
    "volume",
    "indicator_type",
    "filter_enabled",
    "filter_tf",
    "filter_period",
    "filter_logic",
}
_INDICATOR_TYPES = {"sma", "ema", "rsi", "macd", "bb", "stoch"}
_OPTIONAL_LIST_KEYS = (
    "data_id",
    "candles_file",
    "start_time",
    "end_time",
    "timeframe",
    "aux_candles_files",
)
_CANDLE_KEY_ALIASES = {
    # Guardian expects Candle keys: t/o/h/l/c/v
    "timestamp": "t",
    "time": "t",
    "open": "o",
    "high": "h",
    "low": "l",
    "close": "c",
    "volume": "v",
    # Guardian expects SwapRecord keys: t/sl/ss
    "swap_long": "sl",
    "swap_short": "ss",
}
_FALLBACK_BOOL_PAIR_RE = re.compile(
    r"\((?P<key>[A-Za-z0-9_:\-]+)\s+\.\s+(?P<val>t|nil|true|false)\)",
    flags=re.IGNORECASE,
)

def _post_webhook(url: str, payload: dict) -> bool:
    try:
        data = json.dumps(payload).encode("utf-8")
        req = urllib.request.Request(url, data=data, headers={"Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=5) as resp:
            resp.read()
        return True
    except Exception as e:
        print(f"[BACKTEST-SVC] ⚠️ Failed to send webhook: {e}")
        return False

_SEXP_NAME_RE = re.compile(r'\(name\s+\.\s+"([^"]+)"\)')
_SEXP_NAME_JSONISH_RE = re.compile(r'"name"\s+"([^"]+)"')
_SEXP_REQUEST_ID_RE = re.compile(r'\(request_id\s+\.\s+"([^"]+)"\)')

_DUMP_INCOMING_ENV = "SWIMMY_BACKTEST_DUMP_INCOMING"
_DUMP_GUARDIAN_ENV = "SWIMMY_BACKTEST_DUMP_GUARDIAN"

def _should_dump_incoming() -> bool:
    val = os.getenv(_DUMP_INCOMING_ENV, "").strip().lower()
    return val in ("1", "true", "yes", "on")

def _format_incoming_preview(msg: str, limit: int = 240) -> str:
    if msg is None:
        return ""
    cleaned = msg.replace("\r", " ").replace("\n", " ").strip()
    if len(cleaned) <= limit:
        return cleaned
    # Preserve request_id/name in logs even when preview is truncated.
    parts = []
    name_match = _SEXP_NAME_RE.search(cleaned) or _SEXP_NAME_JSONISH_RE.search(cleaned)
    if name_match:
        parts.append(f"name={name_match.group(1)}")
    req_match = _SEXP_REQUEST_ID_RE.search(cleaned)
    if req_match:
        parts.append(f"request_id={req_match.group(1)}")
    suffix = f" [{' '.join(parts)}]" if parts else ""
    return cleaned[:limit] + "..." + suffix

def _should_dump_guardian() -> bool:
    val = os.getenv(_DUMP_GUARDIAN_ENV, "").strip().lower()
    return val in ("1", "true", "yes", "on")

def _format_guardian_preview(msg: str, limit: int = 240) -> str:
    if msg is None:
        return ""
    cleaned = msg.replace("\r", " ").replace("\n", " ").strip()
    if len(cleaned) <= limit:
        return cleaned
    # Preserve request_id/name in logs even when preview is truncated.
    parts = []
    name_match = _SEXP_NAME_RE.search(cleaned) or _SEXP_NAME_JSONISH_RE.search(cleaned)
    if name_match:
        parts.append(f"name={name_match.group(1)}")
    req_match = _SEXP_REQUEST_ID_RE.search(cleaned)
    if req_match:
        parts.append(f"request_id={req_match.group(1)}")
    suffix = f" [{' '.join(parts)}]" if parts else ""
    return cleaned[:limit] + "..." + suffix


def _normalize_sexp_key(key):
    # sexp_utils.parse_sexp() converts the symbol `t` into boolean True, even when it is
    # used as a key (e.g. candle timestamp key). Convert it back to the expected symbol.
    if key is True:
        return "t"
    if key is False:
        return "f"
    if isinstance(key, str):
        key = key.strip()
        if "::" in key:
            key = key.rsplit("::", 1)[-1]
        elif ":" in key and not key.startswith(":"):
            key = key.rsplit(":", 1)[-1]
        if key.startswith(":"):
            key = key[1:]
        return key.replace("-", "_").lower()
    return str(key)


def _normalize_record_keys(raw: dict):
    if not isinstance(raw, dict):
        return raw
    out = {}
    for key, value in raw.items():
        norm_key = _normalize_sexp_key(key)
        mapped = _CANDLE_KEY_ALIASES.get(norm_key, norm_key)
        # Prefer the canonical key when both are present.
        if mapped in out and mapped != norm_key:
            continue
        out[mapped] = value
    return out


def _normalize_record_list(raw):
    if isinstance(raw, dict):
        return _normalize_record_keys(raw)
    if isinstance(raw, (list, tuple)):
        return [_normalize_record_keys(item) if isinstance(item, dict) else item for item in raw]
    return raw


def _normalize_aux_candles(raw):
    if not isinstance(raw, dict):
        return raw
    out = {}
    for key, value in raw.items():
        out[_normalize_sexp_key(key)] = _normalize_record_list(value)
    return out


def _normalize_backtest_payload(payload: dict) -> dict:
    """Fix legacy/backwards-compatible shapes before feeding guardian."""
    if not isinstance(payload, dict):
        return payload
    output = dict(payload)

    strategy = output.get("strategy")
    if isinstance(strategy, dict) and strategy.get("filter_tf") is not None:
        # aux_candles keys are normalized/lowercased by serialization; match that.
        strat2 = dict(strategy)
        strat2["filter_tf"] = str(strategy.get("filter_tf", "")).strip().lower()
        output["strategy"] = strat2

    if "candles" in output:
        output["candles"] = _normalize_record_list(output.get("candles"))
    if "swap_history" in output:
        output["swap_history"] = _normalize_record_list(output.get("swap_history"))
    if "aux_candles" in output:
        output["aux_candles"] = _normalize_aux_candles(output.get("aux_candles"))
    return output


def _as_pair_like(item):
    if isinstance(item, tuple) and len(item) == 2:
        return item[0], item[1]
    if isinstance(item, list) and len(item) >= 2 and isinstance(item[0], str):
        if len(item) == 2:
            return item[0], item[1]
        # Dotted-pair shorthand like: (strategy (k . v) (k2 . v2) ...)
        return item[0], item[1:]
    return None


def _sexp_to_python(obj):
    if isinstance(obj, tuple) and len(obj) == 2:
        return (_normalize_sexp_key(obj[0]), _sexp_to_python(obj[1]))
    if isinstance(obj, list):
        # List of pairs -> dict.
        if obj:
            pairs = []
            pairish = True
            for item in obj:
                pair = _as_pair_like(item)
                if pair is None:
                    pairish = False
                    break
                pairs.append(pair)
            if pairish:
                return {_normalize_sexp_key(k): _sexp_to_python(v) for k, v in pairs}
        # Alternating key/value list -> dict
        if len(obj) % 2 == 0 and all(isinstance(obj[i], str) for i in range(0, len(obj), 2)):
            return {_normalize_sexp_key(obj[i]): _sexp_to_python(obj[i + 1]) for i in range(0, len(obj), 2)}
        return [_sexp_to_python(x) for x in obj]
    return obj

def _parse_incoming_message(msg: str):
    if not msg:
        return None, None
    if msg.lstrip().startswith("("):
        return "sexpr", msg.strip()
    return None, None


def _is_fallback_bool_key(key_token: str) -> bool:
    key_norm = _normalize_sexp_key(key_token or "")
    return (
        key_norm == "filter_enabled"
        or key_norm.endswith("enabled")
        or key_norm.startswith("is_")
        or key_norm.startswith("has_")
    )


def _fallback_normalize_bool_pairs(text: str) -> str:
    """Best-effort bool normalization for known dotted pairs when parse fails."""

    def _replace(match: re.Match) -> str:
        key = match.group("key")
        val = match.group("val")
        if not _is_fallback_bool_key(key):
            return match.group(0)
        return f"({key} . {'#t' if _coerce_bool(val) else '#f'})"

    return _FALLBACK_BOOL_PAIR_RE.sub(_replace, text)


def _normalize_backtest_sexpr(msg: str) -> str:
    """Canonicalize incoming BACKTEST S-expression for guardian compatibility."""
    text = (msg or "").strip()
    if not text or not text.startswith("("):
        return text
    try:
        payload = _sexp_to_python(parse_sexp(text))
    except (SexpParseError, ValueError, TypeError):
        return _fallback_normalize_bool_pairs(text)
    if not isinstance(payload, dict):
        return _fallback_normalize_bool_pairs(text)
    action = str(payload.get("action", "")).strip().upper()
    if action != "BACKTEST":
        return text
    return _sexp_serialize(_normalize_backtest_payload(payload))



def _sanitize_strategy(raw):
    """Normalize strategy payload to match Guardian backtest schema."""
    if not isinstance(raw, dict):
        return {}

    def _get(key, fallback=None):
        return raw.get(key, raw.get(key.replace("_", "-"), fallback))

    name = _get("name")
    if name is None:
        name = "unknown"
    name = str(name)

    def _to_int(val, default=0):
        try:
            if isinstance(val, bool):
                return int(val)
            if isinstance(val, (int, float)):
                return int(val)
            if isinstance(val, str) and val.strip():
                return int(float(val))
        except Exception:
            return default
        return default

    def _to_float(val, default=0.0):
        try:
            if isinstance(val, bool):
                return float(int(val))
            if isinstance(val, (int, float)):
                return float(val)
            if isinstance(val, str) and val.strip():
                return float(val)
        except Exception:
            return default
        return default

    indicator_type = _get("indicator_type", "sma")
    indicator_type = str(indicator_type).strip().lower() if indicator_type is not None else "sma"
    if indicator_type not in _INDICATOR_TYPES:
        indicator_type = "sma"

    filter_enabled = _coerce_bool(_get("filter_enabled", False))
    filter_tf = _get("filter_tf", "")
    filter_tf = "" if filter_tf is None else str(filter_tf).strip().lower()
    filter_period = _to_int(_get("filter_period", 0), default=0)
    filter_logic = _get("filter_logic", "")
    filter_logic = "" if filter_logic is None else str(filter_logic)

    return {
        "name": name,
        "sma_short": _to_int(_get("sma_short", 5), default=5),
        "sma_long": _to_int(_get("sma_long", 20), default=20),
        "sl": _to_float(_get("sl", 0.0), default=0.0),
        "tp": _to_float(_get("tp", 0.0), default=0.0),
        "volume": _to_float(_get("volume", 0.01), default=0.01),
        "indicator_type": indicator_type,
        "filter_enabled": filter_enabled,
        "filter_tf": filter_tf,
        "filter_period": filter_period,
        "filter_logic": filter_logic,
    }


def _build_guardian_payload(request: dict) -> dict:
    strategy = _sanitize_strategy(request.get("strategy", {}))
    payload = {"action": "BACKTEST", "strategy": strategy}

    if request.get("candles_file"):
        payload["candles_file"] = request["candles_file"]
    elif "candles" in request:
        payload["candles"] = request.get("candles", [])

    for key in (
        "data_id",
        "start_time",
        "end_time",
        "aux_candles",
        "aux_candles_files",
        "swap_history",
        "timeframe",
        "phase",
        "range_id",
    ):
        if key in request and request[key] is not None:
            payload[key] = request[key]

    if "timeframe" in payload:
        strategy["timeframe"] = payload["timeframe"]

    return _normalize_backtest_payload(payload)

class BacktestService:
    def __init__(self, use_zmq=True):
        self.context = zmq.Context()
        self.data_cache = {}  # Data store: {data_id: candles}
        self.use_zmq = use_zmq
        self._instance_lock = None
        self.worker_count = _resolve_worker_count()
        self.runtime_knobs = _resolve_runtime_knobs(worker_count=self.worker_count)
        self.max_inflight = int(self.runtime_knobs["max_inflight"])
        self._error_dumped = 0
        self._guardian_missing_logged_at = 0.0
        self._guardian_missing_log_interval = 60.0
        self._guardian_missing_alert_at = 0.0
        self._guardian_missing_alert_interval = 600.0
        self._thread_local = threading.local()
        self._guardian_processes = set()
        self._guardian_processes_lock = threading.Lock()

        if self.use_zmq:
            self._instance_lock = _acquire_single_instance_lock(_resolve_lock_path(BACKTEST_REQ_PORT))
            # Receive backtest requests
            self.receiver = self.context.socket(zmq.PULL)
            try:
                self.receiver.bind(f"tcp://*:{BACKTEST_REQ_PORT}")
            except zmq.ZMQError as e:
                if getattr(e, "errno", None) == errno.EADDRINUSE:
                    print(
                        f"[BACKTEST-SVC] ❌ Cannot bind tcp://*:{BACKTEST_REQ_PORT} "
                        f"(Address already in use)."
                    )
                    print(
                        f"[BACKTEST-SVC] Check who holds the port: ss -tulnp | rg ':{BACKTEST_REQ_PORT}'"
                    )
                    raise SystemExit(1)
                raise

            # Send results back to Brain (Backtest PULL port 5581)
            self.sender = self.context.socket(zmq.PUSH)
            self.sender.connect(f"tcp://localhost:{BACKTEST_RES_PORT}")

        # Path to guardian binary
        self.guardian_bin = (
            Path(__file__).parent.parent
            / "guardian"
            / "target"
            / "release"
            / "guardian"
        )
        self._refresh_guardian_bin(log=True)

    def _guardian_alert_url(self):
        for key in _ALERT_WEBHOOK_ENV:
            url = os.getenv(key)
            if url:
                return url
        return None

    def _maybe_alert_guardian_missing(self):
        now = time.time()
        if (now - self._guardian_missing_alert_at) < self._guardian_missing_alert_interval:
            return
        url = self._guardian_alert_url()
        if not url:
            return
        self._guardian_missing_alert_at = now
        _post_webhook(
            url,
            {
                "content": (
                    "🚨 Backtest停止: guardianバイナリが見つかりません。\n"
                    f"path: {self.guardian_bin}\n"
                    "対応: guardian を復旧/ビルドして backtest を再開してください。"
                )
            },
        )

    def _refresh_guardian_bin(self, log=False) -> bool:
        if self.guardian_bin.exists():
            return True
        now = time.time()
        if log or (now - self._guardian_missing_logged_at) >= self._guardian_missing_log_interval:
            print(f"[BACKTEST-SVC] ⚠️ Guardian binary not found at {self.guardian_bin}")
            print("[BACKTEST-SVC] Run 'cd guardian && cargo build --release' first")
            self._guardian_missing_logged_at = now
        self._maybe_alert_guardian_missing()
        return False

    def _log_input_on_error(self, input_data):
        if self._error_dumped >= 3:
            return
        head = input_data[:900].replace("\n", "\\n")
        tail = input_data[-300:].replace("\n", "\\n") if len(input_data) > 1200 else ""
        print(f"[BACKTEST-SVC] ⚠️ ERROR_INPUT_HEAD: {head}")
        if tail:
            print(f"[BACKTEST-SVC] ⚠️ ERROR_INPUT_TAIL: {tail}")
        self._error_dumped += 1

    @staticmethod
    def _is_sexpr(msg: str) -> bool:
        return msg.lstrip().startswith("(")

    @staticmethod
    def _extract_name_from_sexpr(msg: str) -> str:
        m = _SEXP_NAME_RE.search(msg)
        if not m:
            m = _SEXP_NAME_JSONISH_RE.search(msg)
        return m.group(1) if m else "unknown"

    @staticmethod
    def _extract_request_id_from_sexpr(msg: str):
        if not msg:
            return None
        m = _SEXP_REQUEST_ID_RE.search(msg)
        return m.group(1) if m else None

    @staticmethod
    def _inject_request_id_into_sexpr(sexp: str, request_id: str) -> str:
        if not sexp or not request_id or "request_id" in sexp:
            return sexp
        try:
            data = _sexp_to_python(parse_sexp(sexp))
        except (SexpParseError, ValueError, TypeError):
            data = None
        if isinstance(data, dict):
            result = data.get("result")
            if isinstance(result, dict) and "request_id" not in result:
                result["request_id"] = request_id
                data["result"] = result
                return _sexp_serialize(data)
        patched = re.sub(r'\(result\s+\.\s+\(\(', f'(result . ((request_id . "{request_id}") ', sexp, count=1)
        if patched != sexp:
            return patched
        return re.sub(r'\(result\s+\.\s+\(', f'(result . ((request_id . "{request_id}") ', sexp, count=1)

    def _handle_sexpr(self, msg: str):
        strategy_name = self._extract_name_from_sexpr(msg)
        request_id = self._extract_request_id_from_sexpr(msg)
        if not request_id:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "missing request_id",
                    "sharpe": 0.0,
                    "request_id": "MISSING",
                },
            }
        proc = self._get_guardian_process()
        if proc is None:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "Guardian binary missing",
                    "sharpe": 0.0,
                    "request_id": request_id,
                },
            }

        input_data = _normalize_backtest_sexpr(msg)
        try:
            proc.stdin.write(input_data + "\n")
            proc.stdin.flush()
        except Exception as e:
            print(f"[BACKTEST-SVC] ❌ Failed to send S-exp to guardian: {e}")
            self._reset_guardian_process()
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": str(e),
                    "sharpe": 0.0,
                    "request_id": request_id,
                },
            }

        output_line = self._read_result_line(proc, strategy_name, input_data)

        if not output_line:
            print("[BACKTEST-SVC] ❌ Guardian process died or returned empty line.")
            self._reset_guardian_process()
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "Empty output",
                    "sharpe": 0.0,
                    "request_id": request_id,
                },
            }

        if isinstance(output_line, str):
            out = output_line.strip()
            if out.startswith("{"):
                try:
                    obj = json.loads(out)
                except Exception:
                    obj = None
                if isinstance(obj, dict):
                    result = obj.get("result")
                    if isinstance(result, dict) and request_id and "request_id" not in result:
                        result["request_id"] = request_id
                        obj["result"] = result
                        return obj
            if request_id:
                return self._inject_request_id_into_sexpr(out, request_id)
        return output_line

    @staticmethod
    def _normalize_timeframe(tf):
        """Convert TF string (M1,H1,D1,...) to minutes integer."""
        if isinstance(tf, (list, tuple)) and tf:
            tf = tf[0]
        if isinstance(tf, (int, float)):
            return int(tf)
        if isinstance(tf, str):
            tfu = tf.upper()
            mapping = {
                "M1": 1, "M5": 5, "M15": 15, "M30": 30,
                "H1": 60, "H4": 240, "H12": 720,
                "D1": 1440, "W1": 10080, "MN": 43200, "MN1": 43200,
            }
            return mapping.get(tfu, None)
        return None

    def _read_result_line(self, proc, strategy_name, input_data):
        """Read stdout until a BACKTEST_RESULT line is found (skip banner/log lines)."""
        skipped = 0
        dump_guardian = _should_dump_guardian()
        request_id = self._extract_request_id_from_sexpr(input_data)
        while True:
            output_line = proc.stdout.readline()
            if not output_line:
                return None
            if dump_guardian:
                preview = _format_guardian_preview(output_line)
                # Ensure request_id is visible even if guardian output omits it.
                if request_id and "request_id=" not in preview:
                    preview += f" [request_id={request_id}]"
                print(f"[BACKTEST-SVC] OUT: {preview}")
            output_line = output_line.strip()
            if not output_line:
                continue
            if output_line.startswith("("):
                if '(type . "ERROR")' in output_line:
                    if self._error_dumped < 3:
                        print(f"[BACKTEST-SVC] ⚠️ ERROR_LINE: {output_line}")
                    self._log_input_on_error(input_data)
                    return {
                        "type": "BACKTEST_RESULT",
                        "result": {
                            "strategy_name": strategy_name,
                            "error": output_line,
                            "sharpe": 0.0,
                            "request_id": request_id,
                        },
                    }
                if ("BACKTEST_RESULT" in output_line or
                        "backtest_result" in output_line or
                        "backtest-result" in output_line):
                    if request_id:
                        return self._inject_request_id_into_sexpr(output_line, request_id)
                    return output_line
            elif output_line.startswith("{"):
                try:
                    obj = json.loads(output_line)
                except json.JSONDecodeError:
                    obj = None
                if obj:
                    if obj.get("type") == "ERROR":
                        if self._error_dumped < 3:
                            print(f"[BACKTEST-SVC] ⚠️ ERROR_LINE: {output_line}")
                        self._log_input_on_error(input_data)
                        return {
                            "type": "BACKTEST_RESULT",
                            "result": {
                                "strategy_name": strategy_name,
                                "error": output_line,
                                "sharpe": 0.0,
                                "request_id": request_id,
                            },
                        }
                    if obj.get("type") == "BACKTEST_RESULT":
                        result = obj.get("result")
                        if isinstance(result, dict) and request_id and "request_id" not in result:
                            result["request_id"] = request_id
                            obj["result"] = result
                            return obj
                        return output_line
            skipped += 1
            if skipped <= 3:
                print(f"[BACKTEST-SVC] ⚠️ Skipping non-result output: {output_line}")

    def _get_guardian_process(self):
        """Thread-local lazy initialization of persistent Guardian process."""
        if not self._refresh_guardian_bin():
            return None
        proc = getattr(self._thread_local, "guardian_process", None)
        if proc is None or proc.poll() is not None:
            self._reset_guardian_process(proc=proc)
            print("[BACKTEST-SVC] 🔄 Spawning new Guardian process...")
            proc = subprocess.Popen(
                [str(self.guardian_bin), "--backtest-only", "--stdin"],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=sys.stderr,  # Forward logs to stderr
                text=True,
                bufsize=1,  # Line buffered
                close_fds=True,
                start_new_session=True,
            )
            self._thread_local.guardian_process = proc
            with self._guardian_processes_lock:
                self._guardian_processes.add(proc)
        return proc

    @staticmethod
    def _terminate_guardian_process(proc):
        """Close pipes and terminate a guardian process to prevent FD leaks."""
        if not proc:
            return
        try:
            if proc.stdin:
                proc.stdin.close()
        except Exception:
            pass
        try:
            if proc.stdout:
                proc.stdout.close()
        except Exception:
            pass
        try:
            if proc.poll() is None:
                proc.terminate()
                proc.wait(timeout=2)
        except Exception:
            try:
                proc.kill()
            except Exception:
                pass
        with contextlib.suppress(Exception):
            proc.wait(timeout=0.5)

    def _reset_guardian_process(self, proc=None):
        """Reset thread-local guardian process (or explicit process handle)."""
        target = proc
        if target is None:
            target = getattr(self._thread_local, "guardian_process", None)
        if not target:
            return
        self._terminate_guardian_process(target)
        with self._guardian_processes_lock:
            self._guardian_processes.discard(target)
        if getattr(self._thread_local, "guardian_process", None) is target:
            self._thread_local.guardian_process = None

    def _cleanup_guardian_processes(self):
        """Terminate all known guardian worker processes."""
        with self._guardian_processes_lock:
            procs = list(self._guardian_processes)
            self._guardian_processes.clear()
        for proc in procs:
            self._terminate_guardian_process(proc)
        self._thread_local.guardian_process = None

    def run(self):
        print("[BACKTEST-SVC] ═══════════════════════════════════════")
        print("[BACKTEST-SVC] 🧪 Backtest Service Started (Persistent Mode)")
        print(f"[BACKTEST-SVC] Listening on port {BACKTEST_REQ_PORT}")
        print(f"[BACKTEST-SVC] Guardian binary: {self.guardian_bin}")
        print(
            f"[BACKTEST-SVC] Workers: {self.worker_count} | Max inflight: {self.max_inflight}"
        )
        print(
            "[BACKTEST-SVC] Lisp sender knobs: "
            f"MAX_PENDING={self.runtime_knobs['max_pending']} "
            f"| RATE_LIMIT={self.runtime_knobs['rate_limit']}/s"
        )
        for warning in _knob_alignment_warnings(self.runtime_knobs):
            print(warning)
        print("[BACKTEST-SVC] ═══════════════════════════════════════")

        if not self.use_zmq:
            raise RuntimeError("BacktestService.run requires use_zmq=True")

        dump_incoming = _should_dump_incoming()
        poller = zmq.Poller()
        poller.register(self.receiver, zmq.POLLIN)
        inflight = set()

        def _send_result(result):
            if result is None:
                return
            if isinstance(result, str):
                out = result.strip()
                if out.startswith("{"):
                    try:
                        obj = json.loads(out)
                    except Exception:
                        obj = None
                    if isinstance(obj, dict):
                        out = _sexp_serialize(obj)
                self.sender.send_string(out)
            else:
                self.sender.send_string(_sexp_serialize(result))

        def _drain_done_futures():
            done = [f for f in list(inflight) if f.done()]
            for fut in done:
                inflight.discard(fut)
                try:
                    _send_result(fut.result())
                except Exception as e:
                    print(f"[BACKTEST-SVC] ❌ Worker future failed: {e}")

        try:
            with ThreadPoolExecutor(
                max_workers=self.worker_count, thread_name_prefix="bt-worker"
            ) as executor:
                while True:
                    try:
                        _drain_done_futures()
                        timeout_ms = 25 if inflight else 250
                        events = dict(poller.poll(timeout_ms))
                        if self.receiver not in events:
                            continue

                        # Bound in-flight tasks to avoid unbounded memory growth.
                        while len(inflight) < self.max_inflight:
                            try:
                                msg = self.receiver.recv_string(flags=zmq.NOBLOCK)
                            except zmq.Again:
                                break
                            kind, payload = _parse_incoming_message(msg)

                            if dump_incoming:
                                label = "SXP" if kind == "sexpr" else "RAW"
                                preview_src = payload if payload is not None else msg
                                print(
                                    f"[BACKTEST-SVC] IN {label}: {_format_incoming_preview(preview_src)}"
                                )

                            if kind == "sexpr":
                                inflight.add(executor.submit(self._handle_sexpr, payload))
                                continue

                            if msg.strip():
                                print("[BACKTEST-SVC] ⚠️ Ignoring non-S-expression message")
                    except Exception as e:
                        print(f"[BACKTEST-SVC] Error in loop: {e}")
        except KeyboardInterrupt:
            print("\n[BACKTEST-SVC] Stopping...")
        finally:
            # Flush completed futures before shutdown.
            for fut in list(inflight):
                if not fut.done():
                    continue
                inflight.discard(fut)
                with contextlib.suppress(Exception):
                    _send_result(fut.result())
            self._cleanup_guardian_processes()

    def run_backtest(self, request):
        """Execute backtest via persistent guardian subprocess"""
        strategy = _sanitize_strategy(request.get("strategy", {}))
        strategy_name = strategy.get("name", "unknown")
        candles = request.get("candles", [])
        candles_file = request.get("candles_file")
        request_id = request.get("request_id") or "MISSING"

        if not candles and not candles_file:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "No data available",
                    "sharpe": 0.0,
                    "request_id": request_id,
                },
            }

        # Normalize timeframe to integer minutes
        tf_norm = self._normalize_timeframe(request.get("timeframe"))
        if tf_norm is None:
            tf_norm = self._normalize_timeframe(strategy.get("timeframe"))
        if tf_norm is None:
            tf_norm = 1
        request["timeframe"] = tf_norm
        strategy["timeframe"] = tf_norm

        # Prepare input for guardian (S-Expression protocol)
        if candles_file:
            request["candles_file"] = candles_file
        else:
            request["candles"] = candles
        input_payload = _build_guardian_payload(request)

        # Wrap Option<T> fields as one-element lists for serde_lexpr Option decoding.
        for key in _OPTIONAL_LIST_KEYS:
            if key in input_payload and input_payload[key] is not None and not isinstance(input_payload[key], (list, tuple)):
                input_payload[key] = [input_payload[key]]

        input_data = _sexp_serialize(input_payload)

        try:
            # DEBUG: Trace
            print(
                f"[BACKTEST-SVC] ⏳ Processing: {strategy.get('name')} | Timeframe: {request.get('timeframe', 1)}"
            )

            proc = self._get_guardian_process()
            if proc is None:
                return {
                    "type": "BACKTEST_RESULT",
                    "result": {
                        "strategy_name": strategy_name,
                        "error": "Guardian binary missing",
                        "sharpe": 0.0,
                        "request_id": request_id,
                    },
                }

            # Send Request (JSON + newline)
            proc.stdin.write(input_data + "\n")
            proc.stdin.flush()

            # Read Response (Line)
            output_line = self._read_result_line(proc, strategy_name, input_data)

            if not output_line:
                print("[BACKTEST-SVC] ❌ Guardian process died or returned empty line.")
                self._reset_guardian_process()
                return {
                    "type": "BACKTEST_RESULT",
                    "result": {
                        "strategy_name": strategy_name,
                        "error": "Guardian process failure",
                        "sharpe": 0.0,
                        "request_id": request_id,
                    },
                }

            if isinstance(output_line, dict):
                result = output_line.get("result")
                if isinstance(result, dict) and request_id and "request_id" not in result:
                    result["request_id"] = request_id
                    output_line["result"] = result
                return output_line

            output_line = output_line.strip()
            if not output_line:
                print("[BACKTEST-SVC] ❌ Guardian returned empty output.")
                return {
                    "type": "BACKTEST_RESULT",
                    "result": {
                        "strategy_name": strategy_name,
                        "error": "Guardian returned empty output",
                        "sharpe": 0.0,
                        "request_id": request_id,
                    },
                }

            if output_line.startswith("{"):
                try:
                    obj = json.loads(output_line)
                except Exception:
                    obj = None
                if isinstance(obj, dict):
                    result = obj.get("result")
                    if isinstance(result, dict) and request_id and "request_id" not in result:
                        result["request_id"] = request_id
                        obj["result"] = result
                        return obj
            if request_id:
                return self._inject_request_id_into_sexpr(output_line, request_id)

            # Forward raw S-Expression (or JSON) to Brain
            return output_line

        except Exception as e:
            print(f"[BACKTEST-SVC] Error communicating with Guardian: {e}")
            self._reset_guardian_process()
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy.get("name", "unknown"),
                    "error": str(e),
                    "sharpe": 0.0,
                    "request_id": request_id,
                },
            }


def main():
    service = BacktestService()
    service.run()


if __name__ == "__main__":
    main()
