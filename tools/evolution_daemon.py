import os, subprocess, sys, time
BASE = os.path.abspath(os.path.dirname(__file__) + '/..')
LOG = os.path.join(BASE, 'logs', 'evolution_daemon.log')

CMD = ["sbcl", "--disable-debugger", "--script", os.path.join(BASE, "tools", "run_lisp_evolution.lisp")]

if __name__ == "__main__":
    with open(LOG, "a", buffering=1) as log:
        log.write(f"[EVODAEMON] starting at {time.ctime()}\n")
        proc = subprocess.Popen(CMD, cwd=BASE, stdout=log, stderr=log)
        proc.wait()
        log.write(f"[EVODAEMON] exit code {proc.returncode} at {time.ctime()}\n")
        sys.exit(proc.returncode)
