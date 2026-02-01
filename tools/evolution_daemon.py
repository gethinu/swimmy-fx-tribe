import os, subprocess, sys, time
BASE = os.path.abspath(os.path.dirname(__file__) + '/..')
LOG = os.path.join(BASE, 'logs', 'evolution_daemon.log')

def _sbcl_dynamic_space_mb() -> str:
    val = os.getenv("SWIMMY_SBCL_DYNAMIC_SPACE_MB", "").strip()
    return val if val else "4096"

CMD = [
    "sbcl",
    "--dynamic-space-size",
    _sbcl_dynamic_space_mb(),
    "--disable-debugger",
    "--script",
    os.path.join(BASE, "tools", "run_lisp_evolution.lisp"),
]

if __name__ == "__main__":
    with open(LOG, "a", buffering=1) as log:
        log.write(f"[EVODAEMON] starting at {time.ctime()}\n")
        proc = subprocess.Popen(CMD, cwd=BASE, stdout=log, stderr=log)
        proc.wait()
        log.write(f"[EVODAEMON] exit code {proc.returncode} at {time.ctime()}\n")
        sys.exit(proc.returncode)
