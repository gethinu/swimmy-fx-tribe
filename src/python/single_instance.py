"""Cross-platform single-instance lock (replaces Unix-only fcntl.lockf).

Handbook §3: fcntl is POSIX-only and crashes on `import` under native Windows.
portalocker wraps msvcrt (Windows) and fcntl (Linux) behind one API, so the same
call runs on both. Lock files live under %SWIMMY_HOME%/data/locks/ (no /tmp).

Usage (keep the returned handle alive for the whole process lifetime):

    from single_instance import acquire
    _lock = acquire("data_keeper", message="[DATA-KEEPER] already running. Exiting.")
"""
import os
import sys

import portalocker


def _repo_root() -> str:
    # src/python/ -> repo root; SWIMMY_HOME wins when set (handbook §5).
    return os.environ.get("SWIMMY_HOME") or os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "..")
    )


def _locks_dir() -> str:
    d = os.path.join(_repo_root(), "data", "locks")
    os.makedirs(d, exist_ok=True)
    return d


def acquire(name: str, *, on_busy_exit: bool = True, message: str | None = None):
    """Acquire an exclusive, non-blocking lock named <name>.lock.

    Returns the open file handle (retain it — releasing it drops the lock).
    If the lock is already held: prints `message` (if given) and, when
    on_busy_exit is True, sys.exit(0); otherwise re-raises.
    """
    path = os.path.join(_locks_dir(), f"{name}.lock")
    fh = open(path, "w")
    try:
        portalocker.lock(fh, portalocker.LOCK_EX | portalocker.LOCK_NB)
    except (portalocker.exceptions.LockException, OSError):
        if message:
            print(message)
        if on_busy_exit:
            sys.exit(0)
        fh.close()
        raise
    try:
        fh.write(str(os.getpid()))
        fh.flush()
    except Exception:
        pass
    return fh
