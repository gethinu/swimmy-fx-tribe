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
import os

import sys
from pathlib import Path

# Force unbuffered output for debugging
sys.stdout.reconfigure(line_buffering=True)


class BacktestService:
    def __init__(self, use_zmq=True):
        self.context = zmq.Context()
        self.data_cache = {}  # Data store: {data_id: candles}
        self.guardian_process = None  # Persistent process
        self.use_zmq = use_zmq

        if self.use_zmq:
            # Receive backtest requests
            self.receiver = self.context.socket(zmq.PULL)
            self.receiver.bind("tcp://*:5580")

            # Send results back to Brain (Brain PULL port is 5555)
            self.sender = self.context.socket(zmq.PUSH)
            self.sender.connect("tcp://localhost:5555")

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

    def _get_guardian_process(self):
        """Lazy initialization of persistent Guardian process"""
        if self.guardian_process is None or self.guardian_process.poll() is not None:
            print("[BACKTEST-SVC] 🔄 Spawning new Guardian process...")
            self.guardian_process = subprocess.Popen(
                [str(self.guardian_bin), "--backtest-only", "--stdin"],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=sys.stderr,  # Forward logs to stderr
                text=True,
                bufsize=1,  # Line buffered
            )
        return self.guardian_process

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
                self.guardian_process.terminate()

    def run_backtest(self, request):
        """Execute backtest via persistent guardian subprocess"""
        strategy = request.get("strategy", {})
        candles = request.get("candles", [])
        candles_file = request.get("candles_file")

        if not candles and not candles_file:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy.get("name", "unknown"),
                    "error": "No data available",
                    "sharpe": 0.0,
                },
            }

        # Prepare input for guardian
        input_payload = {"strategy": strategy}
        if candles_file:
            input_payload["candles_file"] = candles_file
        else:
            input_payload["candles"] = candles

        # V10.2: Forward timeframe for resizing (MTF Fix)
        if "timeframe" in request:
            input_payload["timeframe"] = request["timeframe"]

        input_data = json.dumps(input_payload)

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
            output_line = proc.stdout.readline()

            if not output_line:
                print("[BACKTEST-SVC] ❌ Guardian process died or returned empty line.")
                self.guardian_process = None  # Force restart next time
                return {
                    "type": "BACKTEST_RESULT",
                    "result": {
                        "strategy_name": strategy.get("name", "unknown"),
                        "error": "Guardian process failure",
                        "sharpe": 0.0,
                    },
                }

            result = json.loads(output_line)
            # Ensure type is set (Guardian usually sets it inside 'output' wrapper but let's be safe)
            if "type" not in result:
                result["type"] = "BACKTEST_RESULT"

            return result

        except Exception as e:
            print(f"[BACKTEST-SVC] Error communicating with Guardian: {e}")
            self.guardian_process = None
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
