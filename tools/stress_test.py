#!/usr/bin/env python3
"""
stress_test.py - Deep Validation Service (Updated for MT5 D1/W1 + M5/M15 Aux)
"""
import zmq
import json
import subprocess
import os
import sys
import argparse
import csv
from datetime import datetime

# Configuration
DATA_KEEPER_ADDRESS = "tcp://localhost:5561"
GUARDIAN_BIN = os.path.join(
    os.path.dirname(__file__), "../guardian/target/release/guardian"
)


def get_data_file_path(symbol, timeframe):
    """Ask Data Keeper for the file path."""
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect(DATA_KEEPER_ADDRESS)

    cmd = f"GET_FILE_PATH:{symbol}:{timeframe}"
    socket.send_string(cmd)

    poller = zmq.Poller()
    poller.register(socket, zmq.POLLIN)

    if poller.poll(30000):  # 30s timeout (Increased for Deep Load)
        msg = socket.recv_string()
        resp = json.loads(msg)
        if "path" in resp:
            return resp["path"]
        else:
            return None
    else:
        # print("[ERROR] Data Keeper timeout")
        return None


def parse_mt5_datetime(date_str, time_str=None):
    """Parse MT5 formats. Time is optional for D1/W1."""
    try:
        if time_str:
            # Format: YYYY.MM.DD HH:MM:SS
            dt_str = f"{date_str} {time_str}"
            dt = datetime.strptime(dt_str, "%Y.%m.%d %H:%M:%S")
        else:
            # Format: YYYY.MM.DD
            dt = datetime.strptime(date_str, "%Y.%m.%d")
        return int(dt.timestamp())
    except ValueError:
        return 0

        return candles
    except Exception as e:
        print(f" [ERROR] CSV Load failed: {e}")
        return []


def run_guardian_backtest(strategy, candles_path, symbol, main_tf):
    if not os.path.exists(GUARDIAN_BIN):
        print(f"[ERROR] Guardian binary not found at {GUARDIAN_BIN}")
        return None

    # Auto-load Aux Candles Files (MTF Support)
    aux_candles_files = {}
    timeframes = ["M1", "M5", "M15", "M30", "H1", "H4", "D1", "W1", "MN"]

    print(f"[STRESS] Checking for auxiliary Timeframes for {symbol}...")
    for tf in timeframes:
        if tf == main_tf:
            continue

        target_tf = "Monthly" if tf == "MN" else tf

        # Expanded List: M5, M15 added
        if tf in ["M5", "M15", "H1", "H4", "D1", "W1", "MN"]:
            path = get_data_file_path(symbol, target_tf)
            if path and os.path.exists(path):
                aux_candles_files[tf] = path

    # V9.1: Optimization - Send file paths instead of raw data
    request = {
        "strategy": strategy,
        "candles": [],  # Empty list (using file)
        "candles_file": candles_path,
        "aux_candles": {},  # Empty map (using files)
        "aux_candles_files": aux_candles_files,
    }

    print(
        f"[STRESS] Invoking Guardian with main file: {candles_path} + {len(aux_candles_files)} aux files..."
    )
    input_data = json.dumps(request)

    try:
        # Use Popen to stream output
        process = subprocess.Popen(
            [GUARDIAN_BIN, "--backtest-only", "--stdin"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,  # Line buffering
        )

        # Send input in a separate thread to avoid deadlock or just write if small?
        # JSON might be large-ish but usually fits in pipe buffer?
        # Actually, if input is large, better to use communicate? But communicate blocks reading.
        # We need to write input, close stdin, THEN read stdout stream.
        try:
            process.stdin.write(input_data)
            process.stdin.close()
        except BrokenPipeError:
            print("[ERROR] Guardian closed stdin unexpectedly.")

        # Stream stdout
        full_output = []
        last_alert_pct = 0.0

        while True:
            line = process.stdout.readline()
            if not line and process.poll() is not None:
                break

            if line:
                line_str = line.strip()
                print(line_str)  # Print to log
                full_output.append(line_str)

                # Check for Progress
                if "PROGRESS:" in line_str and "%" in line_str:
                    try:
                        # Extract percentage: "PROGRESS: ... (25.5%)"
                        idx = line_str.rfind("(")
                        if idx != -1:
                            pct_str = line_str[idx + 1 : -2]  # remove "%)"
                            pct = float(pct_str)
                            if pct - last_alert_pct >= 10.0:
                                # Send Alert
                                subprocess.Popen(
                                    [
                                        "python3",
                                        "tools/alert.py",
                                        f"⏳ {symbol} {main_tf} Progress: {pct:.1f}%",
                                        "--type",
                                        "info",
                                    ],
                                    stdout=subprocess.DEVNULL,
                                    stderr=subprocess.DEVNULL,
                                )
                                last_alert_pct = pct
                    except Exception:
                        pass

        # Check errors
        stderr_output = process.stderr.read()
        if stderr_output:
            print(f"[STDERR] {stderr_output}")

        if process.returncode != 0:
            print(f"[ERROR] Guardian Failed with code {process.returncode}")
            return None

        # Parse the JSON result (It should be the LAST line or find valid JSON)
        # Guardian prints progress lines, THEN JSON at the end?
        # My guardian modification prints "PROGRESS..." to stdout.
        # So stdout now contains non-JSON text.
        # `json.loads(proc.stdout)` will FAIL if I just pass the whole blob.
        # I need to find the BacktestResult JSON.
        # Guardian `backtest-only` usually prints JUST the JSON.
        # My added `println!` corrupts that.
        # I must make sure Guardian prints JSON as the *final* output or distinct block.
        # Or parse the last line?
        # Let's try parsing the LAST non-empty line or scanning for valid JSON object.

        for line in reversed(full_output):
            if line.startswith("{") and line.endswith("}"):
                try:
                    return json.loads(line)
                except:
                    continue

        print("[ERROR] No valid JSON result found in output.")
        return None

    except Exception as e:
        print(f"[ERROR] Subprocess failed: {e}")
        return None


def main():
    parser = argparse.ArgumentParser(description="Swimmy Stress Test Service")
    parser.add_argument("--symbol", required=True, help="e.g. USDJPY")
    parser.add_argument("--timeframe", default="M5", help="e.g. M5, H1")
    parser.add_argument("--strategy", required=True, help="Path to Strategy JSON file")
    parser.add_argument("--start-year", type=int, help="Start year (e.g. 2015)")
    args = parser.parse_args()

    if not os.path.exists(args.strategy):
        print(f"[ERROR] Strategy file not found: {args.strategy}")
        sys.exit(1)

    with open(args.strategy, "r") as f:
        strategy_def = json.load(f)

    path = get_data_file_path(args.symbol, args.timeframe)
    if not path:
        print("[ERROR] Could not locate historical data. Ensure Data Keeper has data.")
        sys.exit(1)

    candles_path = path

    # V9.1: Skip Python-side loading to prevent OOM
    # Just check if file exists and has content
    if not os.path.exists(candles_path):
        print("[ERROR] Data file not found.")
        sys.exit(1)

    print("=========================================")
    print(
        f"🚀 STRESS TEST: {strategy_def.get('name', 'std')} on {args.symbol} {args.timeframe}"
    )
    print("=========================================")

    result = run_guardian_backtest(
        strategy_def, candles_path, args.symbol, args.timeframe
    )

    if result:
        print("\n📊 RESULT REPORT")
        print("-----------------")
        print(json.dumps(result, indent=2))
    else:
        print("❌ Test Failed")


if __name__ == "__main__":
    main()
