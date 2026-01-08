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


class BacktestService:
    def __init__(self):
        self.context = zmq.Context()

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

    def run(self):
        print("[BACKTEST-SVC] ═══════════════════════════════════════")
        print("[BACKTEST-SVC] 🧪 Backtest Service Started")
        print("[BACKTEST-SVC] Listening on port 5580")
        print(f"[BACKTEST-SVC] Guardian binary: {self.guardian_bin}")
        print("[BACKTEST-SVC] ═══════════════════════════════════════")

        while True:
            try:
                msg = self.receiver.recv_string()
                request = json.loads(msg)

                action = request.get("action", "")

                if action == "BACKTEST":
                    result = self.run_backtest(request)
                    self.sender.send_string(json.dumps(result))

                elif action == "BATCH_BACKTEST":
                    # Handle batch requests
                    strategies = request.get("strategies", [])
                    candles = request.get("candles", [])

                    for strategy in strategies:
                        result = self.run_backtest(
                            {
                                "action": "BACKTEST",
                                "strategy": strategy,
                                "candles": candles,
                            }
                        )
                        self.sender.send_string(json.dumps(result))

                elif action == "HEALTH_CHECK":
                    self.sender.send_string(
                        json.dumps(
                            {
                                "type": "HEALTH_RESPONSE",
                                "status": "ok",
                                "service": "backtest",
                            }
                        )
                    )

            except json.JSONDecodeError as e:
                print(f"[BACKTEST-SVC] JSON decode error: {e}")
            except Exception as e:
                print(f"[BACKTEST-SVC] Error: {e}")

    def run_backtest(self, request):
        """Execute backtest via guardian subprocess"""
        strategy = request.get("strategy", {})
        candles = request.get("candles", [])

        # Prepare input for guardian
        input_data = json.dumps({"strategy": strategy, "candles": candles})

        try:
            proc = subprocess.run(
                [str(self.guardian_bin), "--backtest-only", "--stdin"],
                input=input_data,
                capture_output=True,
                text=True,
                timeout=30,
            )

            if proc.returncode == 0:
                result = json.loads(proc.stdout)
                result["type"] = "BACKTEST_RESULT"
                return result
            else:
                print(f"[BACKTEST-SVC] Guardian error: {proc.stderr}")
                return {
                    "type": "BACKTEST_RESULT",
                    "result": {
                        "strategy_name": strategy.get("name", "unknown"),
                        "error": proc.stderr,
                        "sharpe": 0.0,
                        "trades": 0,
                        "pnl": 0.0,
                        "win_rate": 0.0,
                    },
                }

        except subprocess.TimeoutExpired:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy.get("name", "unknown"),
                    "error": "timeout",
                    "sharpe": 0.0,
                    "trades": 0,
                    "pnl": 0.0,
                    "win_rate": 0.0,
                },
            }
        except Exception as e:
            return {
                "type": "BACKTEST_RESULT",
                "result": {
                    "strategy_name": strategy.get("name", "unknown"),
                    "error": str(e),
                    "sharpe": 0.0,
                    "trades": 0,
                    "pnl": 0.0,
                    "win_rate": 0.0,
                },
            }


def main():
    service = BacktestService()
    service.run()


if __name__ == "__main__":
    main()
