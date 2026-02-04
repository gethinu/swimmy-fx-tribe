#!/usr/bin/env python3
import os
import subprocess
import re
from datetime import datetime
import time
from pathlib import Path


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent

# ANSI Colors
CYAN = "\033[96m"
GREEN = "\033[92m"
YELLOW = "\033[93m"
RED = "\033[91m"
BOLD = "\033[1m"
RESET = "\033[0m"
CLEAR = "\033[H\033[J"

BASE_DIR = str(resolve_base_dir())
SWIMMY_LOG = os.path.join(BASE_DIR, "logs", "swimmy.log")
GUARDIAN_LOG = os.path.join(BASE_DIR, "logs", "guardian.log")


def get_service_status(service_name):
    try:
        cmd = f"systemctl is-active {service_name}"
        result = subprocess.run(cmd.split(), capture_output=True, text=True)
        status = result.stdout.strip()
        if status == "active":
            return f"{GREEN}● Active{RESET}"
        else:
            return f"{RED}● {status}{RESET}"
    except:
        return f"{RED}Unknown{RESET}"


def get_log_tail(filepath, lines=10):
    if not os.path.exists(filepath):
        return [f"{YELLOW}Log file not found: {filepath}{RESET}"]
    try:
        cmd = f"tail -n {lines} {filepath}"
        result = subprocess.run(cmd.split(), capture_output=True, text=True)
        return result.stdout.strip().split("\n")
    except:
        return [f"{RED}Error reading log{RESET}"]


def parse_brain_metrics():
    metrics = {
        "regime": "Unknown",
        "volatility": "Unknown",
        "strategies": 0,
        "pnl": "Unknown",
    }
    if not os.path.exists(SWIMMY_LOG):
        return metrics

    # Read last 100 lines for metrics
    logs = get_log_tail(SWIMMY_LOG, 100)
    for line in logs:
        # Regime: [L] 📊 Regime: RANGING | Volatility: NORMAL (100.00%)
        if "Regime:" in line:
            m = re.search(r"Regime:\s*([A-Za-z]+)", line)
            if m:
                metrics["regime"] = m.group(1)
            m2 = re.search(r"Volatility:\s*([A-Za-z]+)", line)
            if m2:
                metrics["volatility"] = m2.group(1)

        # Strategies: [L] 🏆 Top strategies: ...
        # Or better count "Active: X" if available.
        # Fallback: Count recent signals? No, hard.
        pass

    return metrics


def print_dashboard():
    print(CLEAR)
    print(
        f"{CYAN}{BOLD}╔════════════════════════════════════════════════════════════╗{RESET}"
    )
    print(
        f"{CYAN}{BOLD}║               🦅 SWIMMY SYSTEM DASHBOARD                   ║{RESET}"
    )
    print(
        f"{CYAN}{BOLD}╚════════════════════════════════════════════════════════════╝{RESET}"
    )
    print(f"Timestamp: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("")

    print(f"{BOLD}CORE SERVICES:{RESET}")
    print(f"  🧠 Brain:      {get_service_status('swimmy-brain')}")
    print(f"  🛡️  Guardian:   {get_service_status('swimmy-guardian')}")
    print(f"  🔔 Notifier:   {get_service_status('swimmy-notifier')}")
    print(f"  💾 DataKeeper: {get_service_status('swimmy-data-keeper')}")
    print("")

    metrics = parse_brain_metrics()
    print(f"{BOLD}MARKET INSIGHTS:{RESET}")
    print(f"  🌍 Regime:     {YELLOW}{metrics.get('regime')}{RESET}")
    print(f"  🌊 Volatility: {YELLOW}{metrics.get('volatility')}{RESET}")
    print("")

    print(f"{BOLD}RECENT BRAIN LOGS:{RESET}")
    for line in get_log_tail(SWIMMY_LOG, 5):
        # Truncate long lines
        if len(line) > 80:
            line = line[:77] + "..."
        print(f"  {line}")
    print("")

    print(f"{BOLD}RECENT GUARDIAN LOGS:{RESET}")
    for line in get_log_tail(GUARDIAN_LOG, 5):
        if len(line) > 80:
            line = line[:77] + "..."
        print(f"  {line}")
    print("")
    print(f"{CYAN}Run 'make logs' for realtime feed.{RESET}")


if __name__ == "__main__":
    print_dashboard()
