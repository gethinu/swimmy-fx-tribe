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
import re

import sys
from pathlib import Path
import urllib.request
import urllib.error

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

# Force unbuffered output for debugging
sys.stdout.reconfigure(line_buffering=True)

_SYMBOL_VALUE_KEYS = {"indicator_type"}
_BOOL_VALUE_KEYS = {"filter_enabled"}
_OPTIONAL_LIST_KEYS = {"data_id", "timeframe", "candles_file", "aux_candles_files"}
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
    return cleaned[:limit] + "..."

def _should_dump_guardian() -> bool:
    val = os.getenv(_DUMP_GUARDIAN_ENV, "").strip().lower()
    return val in ("1", "true", "yes", "on")

def _format_guardian_preview(msg: str, limit: int = 240) -> str:
    if msg is None:
        return ""
    cleaned = msg.replace("\r", " ").replace("\n", " ").strip()
    if len(cleaned) <= limit:
        return cleaned
    return cleaned[:limit] + "..."

def _parse_incoming_message(msg: str):
    if not msg:
        return None, None
    if msg.lstrip().startswith("("):
        return "sexpr", msg.strip()
    return None, None


def _sexp_key(key) -> str:
    return str(key).lower()

def _sexp_symbol(value: str) -> str:
    return str(value).lower()

def _coerce_bool(value) -> bool:
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
            return _coerce_bool(value[0])
    return bool(value)

def _sexp_serialize(value) -> str:
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
            key_norm = _sexp_key(k)
            if (key_norm in _BOOL_VALUE_KEYS or
                    key_norm.endswith("enabled") or
                    key_norm.startswith("is_") or
                    key_norm.startswith("has_")):
                v = _coerce_bool(v)
            if key_norm in _OPTIONAL_LIST_KEYS:
                if v is not None:
                    if key_norm == "aux_candles_files":
                        if not (isinstance(v, (list, tuple)) and len(v) == 1 and isinstance(v[0], (list, tuple))):
                            v = [v]
                    elif not isinstance(v, (list, tuple)):
                        v = [v]
            if v is None:
                if (key_norm in _BOOL_VALUE_KEYS or
                        key_norm.endswith("enabled") or
                        key_norm.startswith("is_") or
                        key_norm.startswith("has_")):
                    v = False
                else:
                    continue
            if (isinstance(v, list) and len(v) == 0 and
                    (key_norm in _BOOL_VALUE_KEYS or
                     key_norm.endswith("enabled") or
                     key_norm.startswith("is_") or
                     key_norm.startswith("has_"))):
                v = False
            if key_norm in _SYMBOL_VALUE_KEYS and isinstance(v, str):
                v_str = _sexp_symbol(v)
            else:
                v_str = _sexp_serialize(v)
            parts.append(f"({key_norm} . {v_str})")
        return f"({' '.join(parts)})"
    if isinstance(value, (list, tuple)):
        return f"({' '.join(_sexp_serialize(v) for v in value)})"
    return json.dumps(str(value), ensure_ascii=False)

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
    filter_tf = "" if filter_tf is None else str(filter_tf)
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

class BacktestService:
    def __init__(self, use_zmq=True):
        self.context = zmq.Context()
        self.data_cache = {}  # Data store: {data_id: candles}
        self.guardian_process = None  # Persistent process
        self.use_zmq = use_zmq
        self._error_dumped = 0
        self._guardian_missing_logged_at = 0.0
        self._guardian_missing_log_interval = 60.0
        self._guardian_missing_alert_at = 0.0
        self._guardian_missing_alert_interval = 600.0

        if self.use_zmq:
            # Receive backtest requests
            self.receiver = self.context.socket(zmq.PULL)
            self.receiver.bind(f"tcp://*:{BACKTEST_REQ_PORT}")

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

    def _handle_sexpr(self, msg: str):
        strategy_name = self._extract_name_from_sexpr(msg)
        proc = self._get_guardian_process()
        if proc is None:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "Guardian binary missing",
                    "sharpe": 0.0,
                },
            }

        input_data = msg.strip()
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
                },
            }

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
        while True:
            output_line = proc.stdout.readline()
            if not output_line:
                return None
            if dump_guardian:
                print(f"[BACKTEST-SVC] OUT: {_format_guardian_preview(output_line)}")
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
                        },
                    }
                if ("BACKTEST_RESULT" in output_line or
                        "backtest_result" in output_line or
                        "backtest-result" in output_line):
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
                            },
                        }
                    if obj.get("type") == "BACKTEST_RESULT":
                        return output_line
            skipped += 1
            if skipped <= 3:
                print(f"[BACKTEST-SVC] ⚠️ Skipping non-result output: {output_line}")

    def _get_guardian_process(self):
        """Lazy initialization of persistent Guardian process"""
        if not self._refresh_guardian_bin():
            return None
        if self.guardian_process is None or self.guardian_process.poll() is not None:
            self._reset_guardian_process()
            print("[BACKTEST-SVC] 🔄 Spawning new Guardian process...")
            self.guardian_process = subprocess.Popen(
                [str(self.guardian_bin), "--backtest-only", "--stdin"],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=sys.stderr,  # Forward logs to stderr
                text=True,
                bufsize=1,  # Line buffered
                close_fds=True,
                start_new_session=True,
            )
        return self.guardian_process

    def _reset_guardian_process(self):
        """Close pipes and terminate guardian process to prevent FD leaks."""
        proc = self.guardian_process
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
        self.guardian_process = None

    def run(self):
        print("[BACKTEST-SVC] ═══════════════════════════════════════")
        print("[BACKTEST-SVC] 🧪 Backtest Service Started (Persistent Mode)")
        print(f"[BACKTEST-SVC] Listening on port {BACKTEST_REQ_PORT}")
        print(f"[BACKTEST-SVC] Guardian binary: {self.guardian_bin}")
        print("[BACKTEST-SVC] ═══════════════════════════════════════")

        dump_incoming = _should_dump_incoming()

        try:
            while True:
                try:
                    msg = self.receiver.recv_string()
                    kind, payload = _parse_incoming_message(msg)

                    if dump_incoming:
                        label = "SXP" if kind == "sexpr" else "RAW"
                        preview_src = payload if payload is not None else msg
                        print(f"[BACKTEST-SVC] IN {label}: {_format_incoming_preview(preview_src)}")

                    if kind == "sexpr":
                        result = self._handle_sexpr(payload)
                        if result is None:
                            continue
                        if isinstance(result, str):
                            self.sender.send_string(result)
                        else:
                            self.sender.send_string(json.dumps(result))
                        continue

                    if msg.strip():
                        print("[BACKTEST-SVC] ⚠️ Ignoring non-S-expression message")
                except Exception as e:
                    print(f"[BACKTEST-SVC] Error in loop: {e}")
                    # If fatal, maybe break or restart?
                    # Keep loop alive.

        except KeyboardInterrupt:
            print("\n[BACKTEST-SVC] Stopping...")
            if self.guardian_process:
                self._reset_guardian_process()

    def run_backtest(self, request):
        """Execute backtest via persistent guardian subprocess"""
        strategy = _sanitize_strategy(request.get("strategy", {}))
        strategy_name = strategy.get("name", "unknown")
        candles = request.get("candles", [])
        candles_file = request.get("candles_file")

        if not candles and not candles_file:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy_name,
                    "error": "No data available",
                    "sharpe": 0.0,
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
        input_payload = {"action": "BACKTEST", "strategy": strategy}
        if candles_file:
            input_payload["candles_file"] = candles_file
        else:
            input_payload["candles"] = candles

        # V10.2: Forward timeframe for resizing (MTF Fix)
        if "timeframe" in request:
            input_payload["timeframe"] = request["timeframe"]

        # Wrap Option<T> fields as one-element lists for serde_lexpr Option decoding.
        for key in ("data_id", "timeframe", "candles_file", "aux_candles_files"):
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
                    },
                }

            if isinstance(output_line, dict):
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
                    },
                }

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
                },
            }


def main():
    service = BacktestService()
    service.run()


if __name__ == "__main__":
    main()
