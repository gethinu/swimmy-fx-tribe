import zmq
import json
import time
import os
import sys
from pathlib import Path


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent

# Configuration
BACKTEST_PORT = _env_int("SWIMMY_PORT_BACKTEST_REQ", 5580)
BRAIN_PORT = _env_int("SWIMMY_PORT_SENSORY", 5555)
SYMBOLS = ["USDJPY", "EURUSD", "GBPUSD"]
BASE_DIR = str(resolve_base_dir())
CSV_BASE = os.path.join(BASE_DIR, "data", "historical")

PYTHON_SRC = Path(BASE_DIR) / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from sexp_serialize import sexp_serialize
from sexp_utils import parse_sexp_alist


def load_strategies(path=None):
    if path is None:
        path = os.path.join(BASE_DIR, "strategies.json")
    if not os.path.exists(path):
        print(f"❌ Strategy file not found: {path}")
        return []
    with open(path, "r") as f:
        try:
            return json.load(f)
        except json.JSONDecodeError:
            print(f"❌ Invalid JSON in {path}")
            return []


def main():
    strats = load_strategies()
    if not strats:
        return

    print(
        f"🚀 Loaded {len(strats)} strategies. Benchmarking on {len(SYMBOLS)} pairs..."
    )

    context = zmq.Context()

    # 1. Be the "Brain" receiver
    receiver = context.socket(zmq.PULL)
    print(f"Binding to tcp://*:{BRAIN_PORT} (Simulating Brain)...")
    try:
        receiver.bind(f"tcp://*:{BRAIN_PORT}")
    except zmq.error.ZMQError:
        print("❌ Port 5555 in use! Stop 'swimmy-brain' first.")
        return

    # 2. Sender to Backtest Service
    sender = context.socket(zmq.PUSH)
    sender.connect(f"tcp://localhost:{BACKTEST_PORT}")

    total_tests = len(strats) * len(SYMBOLS)
    print(f"📊 Starting Benchmark: {total_tests} tests total.")

    results = []

    start_time_global = time.time()

    # Send all requests rapidly
    requests_sent = 0
    for strat in strats:
        base_name = strat.get("name", "Unknown")
        for symbol in SYMBOLS:
            csv_path = f"{CSV_BASE}/{symbol}_M1.csv"
            # Clone and modify name for tracking
            test_strat = strat.copy()
            test_name = f"{base_name}|{symbol}"
            test_strat["name"] = test_name

            # Match timeframe extraction or inference
            tf_val = strat.get("timeframe", 1)

            # Helper: Infer from name if default
            if tf_val == 1 and "name" in strat:
                nm = strat["name"]
                if "-W1-" in nm:
                    tf_val = 10080
                elif "-D1-" in nm:
                    tf_val = 1440
                elif "-H4-" in nm:
                    tf_val = 240
                elif "-H1-" in nm:
                    tf_val = 60
                elif "-M30-" in nm:
                    tf_val = 30
                elif "-M15-" in nm:
                    tf_val = 15
                elif "-M5-" in nm:
                    tf_val = 5

            request = {
                "action": "BACKTEST",
                "strategy": test_strat,
                "candles_file": csv_path,
                "timeframe": tf_val,
            }
            sender.send_string(sexp_serialize(request))
            requests_sent += 1
            if requests_sent % 10 == 0:
                sys.stdout.write(f"\r📤 Sent {requests_sent}/{total_tests} requests...")
                sys.stdout.flush()

    print("\n✅ All requests sent. Waiting for results...")

    # Collect results
    received = 0
    poller = zmq.Poller()
    poller.register(receiver, zmq.POLLIN)

    # Wait loop
    no_msg_count = 0
    while received < total_tests:
        if poller.poll(5000):  # 5s timeout per message
            msg = receiver.recv_string()
            try:
                data = parse_sexp_alist(msg)
                mtype = data.get("type")
                if mtype == "BACKTEST_RESULT":
                    res = data.get("result", {})
                    # Parse name back
                    full_name = res.get("strategy_name", "")
                    if "|" in full_name:
                        sname, sym = full_name.split("|", 1)
                        res["strategy_name"] = sname
                        res["symbol"] = sym
                    else:
                        res["symbol"] = "UNKNOWN"

                    results.append(res)
                    received += 1
                    sys.stdout.write(
                        f"\r📥 Received {received}/{total_tests} results... Last: {full_name[:20]}"
                    )
                    sys.stdout.flush()

                    # DEBUG: Print full line occasionally or on error?
                    # actually just overwrite line is fine, but if it hangs I want to know the last one.
                    print(f"\n✅ Finished: {full_name}")

                    no_msg_count = 0
            except Exception as e:
                print(f"\n❌ Error handling result: {e}")
                import traceback

                traceback.print_exc()
        else:
            no_msg_count += 1
            if no_msg_count > 12:  # 60s silence -> timeout
                print("\n❌ Timeout waiting for results. Aborting.")
                break

    total_time = time.time() - start_time_global
    print(
        f"\n⏱️ Benchmark completed in {total_time:.2f}s ({total_time/total_tests:.3f}s/test avg)"
    )

    # Generate Report
    print_summary(results)


def print_summary(results):
    print("\n\n")
    print("## 📊 Mass Benchmark Summary (20-Year History)")
    print(f"**Total Tested:** {len(results)}")

    # Group by Symbol
    by_symbol = {s: [] for s in SYMBOLS}
    for r in results:
        # Check if result belongs to known symbol
        sym = r.get("symbol")
        if sym in by_symbol:
            by_symbol[sym].append(r)

    for sym in SYMBOLS:
        rs = by_symbol[sym]
        avg_sharpe = sum(r.get("sharpe", 0) for r in rs) / max(1, len(rs))
        avg_trades = sum(r.get("trades", 0) for r in rs) / max(1, len(rs))
        best_sharpe = max([r.get("sharpe", -99) for r in rs], default=-99)
        best_strat = next(
            (r["strategy_name"] for r in rs if r.get("sharpe") == best_sharpe), "None"
        )

        print(f"\n### {sym}")
        print(f"- **Avg Sharpe:** {avg_sharpe:.2f}")
        print(f"- **Avg Trades:** {avg_trades:.0f}")
        print(f"- **Best Strategy:** {best_strat} (SR: {best_sharpe:.2f})")

        # Top 3 Table
        print("\n| Rank | Strategy | Sharpe | PF | Win Rate | Trades |")
        print("|---|---|---|---|---|---|")

        top3 = sorted(rs, key=lambda x: x.get("sharpe", -99), reverse=True)[:3]
        for i, r in enumerate(top3):
            # Safe handling for None
            sr_val = r.get("sharpe") or 0.0
            pf_val = r.get("profit_factor") or 0.0
            wr_val = r.get("win_rate") or 0.0
            tr_val = r.get("trades") or 0

            print(
                f"| {i+1} | {r.get('strategy_name', 'Unknown')} | {sr_val:.2f} | {pf_val:.2f} | {wr_val:.1f}% | {tr_val} |"
            )


if __name__ == "__main__":
    main()
