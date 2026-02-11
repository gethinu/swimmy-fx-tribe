import fcntl
import os
import subprocess
import sys
import time

BASE = os.path.abspath(os.path.dirname(__file__) + "/..")
LOG = os.path.join(BASE, "logs", "evolution_daemon.log")
LOCK_PATH = "/tmp/swimmy_evolution_daemon.lock"


def _is_truthy(value: str) -> bool:
    return value.strip().lower() in {"1", "true", "yes", "on"}


def _sbcl_dynamic_space_mb() -> str:
    val = os.getenv("SWIMMY_SBCL_DYNAMIC_SPACE_MB", "").strip()
    return val if val else "4096"


def _school_daemon_running() -> bool:
    try:
        proc = subprocess.run(
            ["pgrep", "-f", "school-daemon.lisp"],
            capture_output=True,
            text=True,
            check=False,
        )
    except Exception:
        return False
    return proc.returncode == 0 and bool(proc.stdout.strip())


def should_pause_for_school() -> bool:
    if _is_truthy(os.getenv("SWIMMY_ALLOW_PARALLEL_EVOLUTION", "")):
        return False
    return _school_daemon_running()


CMD = [
    "sbcl",
    "--dynamic-space-size",
    _sbcl_dynamic_space_mb(),
    "--disable-debugger",
    "--script",
    os.path.join(BASE, "tools", "run_lisp_evolution.lisp"),
]


def _acquire_singleton_lock(log):
    lock_file = open(LOCK_PATH, "w")
    try:
        fcntl.lockf(lock_file, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except IOError:
        log.write("[EVODAEMON] another instance is already running. exiting.\n")
        sys.exit(0)
    return lock_file


def _wait_if_school_running(log):
    if not should_pause_for_school():
        return
    log.write(
        "[EVODAEMON] school-daemon.lisp is running; pausing to avoid duplicate evolution loops.\n"
    )
    loops = 0
    while should_pause_for_school():
        time.sleep(15)
        loops += 1
        if loops % 8 == 0:
            log.write("[EVODAEMON] still paused (school-daemon active).\n")
    log.write("[EVODAEMON] school-daemon stopped; starting evolution child process.\n")


def main() -> int:
    with open(LOG, "a", buffering=1) as log:
        log.write(f"[EVODAEMON] starting at {time.ctime()}\n")
        lock_file = _acquire_singleton_lock(log)
        try:
            _wait_if_school_running(log)
            proc = subprocess.Popen(CMD, cwd=BASE, stdout=log, stderr=log)
            proc.wait()
            log.write(f"[EVODAEMON] exit code {proc.returncode} at {time.ctime()}\n")
            return proc.returncode
        finally:
            lock_file.close()


if __name__ == "__main__":
    sys.exit(main())
