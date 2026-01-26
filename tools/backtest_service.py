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

import sys
from pathlib import Path

# Force unbuffered output for debugging
sys.stdout.reconfigure(line_buffering=True)

_SYMBOL_VALUE_KEYS = {"indicator_type"}
_BOOL_VALUE_KEYS = {"filter_enabled"}

def _sexp_key(key) -> str:
    return str(key).lower()

def _sexp_symbol(value: str) -> str:
    return str(value).lower()

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

class BacktestService:
    def __init__(self, use_zmq=True):
        self.context = zmq.Context()
        self.data_cache = {}  # Data store: {data_id: candles}
        self.guardian_process = None  # Persistent process
        self.use_zmq = use_zmq
        self._error_dumped = 0

        if self.use_zmq:
            # Receive backtest requests
            self.receiver = self.context.socket(zmq.PULL)
            self.receiver.bind("tcp://*:5580")

            # Send results back to Brain (Backtest PULL port 5581)
            self.sender = self.context.socket(zmq.PUSH)
            self.sender.connect("tcp://localhost:5581")

        # Path to guardian binary
        self.guardian_bin = (
            Path(__file__).parent.parent
            / "guardian"
            / "target"
            / "release"
            / "guardian"
        )

        if not self.guardian_bin.exists():
            print(f"[BACKTEST-SVC] ⚠️ Guardian binary not found at {self.guardian_bin}")
            print("[BACKTEST-SVC] Run 'cd guardian && cargo build --release' first")
            sys.exit(1)

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
    def _normalize_timeframe(tf):
        """Convert TF string (M1,H1,D1,...) to minutes integer."""
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
        while True:
            output_line = proc.stdout.readline()
            if not output_line:
                return None
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
        print("[BACKTEST-SVC] Listening on port 5580")
        print(f"[BACKTEST-SVC] Guardian binary: {self.guardian_bin}")
        print("[BACKTEST-SVC] ═══════════════════════════════════════")

        try:
            while True:
                try:
                    msg = self.receiver.recv_string()
                    request = json.loads(msg)

                    action = request.get("action", "")

                    if action == "BACKTEST":
                        # Standard Backtest Request
                        result = self.run_backtest(request)
                        if isinstance(result, str):
                            self.sender.send_string(result)
                        else:
                            self.sender.send_string(json.dumps(result))

                    elif action == "HEALTH_CHECK":
                        self.sender.send_string(
                            json.dumps(
                                {
                                    "type": "HEALTH_RESPONSE",
                                    "status": "ok",
                                    "service": "backtest",
                                    "cache_size": len(self.data_cache),
                                }
                            )
                        )
                    # Note: CACHE_DATA and BATCH_BACKTEST legacy handlers removed for clarity/Focus on Direct CSV

                except json.JSONDecodeError as e:
                    print(f"[BACKTEST-SVC] JSON decode error: {e}")
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
        strategy = request.get("strategy", {})
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
            if key in input_payload and input_payload[key] is not None:
                input_payload[key] = [input_payload[key]]

        input_data = _sexp_serialize(input_payload)

        try:
            # DEBUG: Trace
            print(
                f"[BACKTEST-SVC] ⏳ Processing: {strategy.get('name')} | Timeframe: {request.get('timeframe', 1)}"
            )

            proc = self._get_guardian_process()

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
