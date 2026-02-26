import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from tools.backtest_service import BacktestService


class _Pipe:
    def __init__(self, owner, name):
        self.owner = owner
        self.name = name

    def close(self):
        self.owner.events.append(f"{self.name}.close")
        if self.name == "stdout" and self.owner.poll() is None:
            self.owner.stdout_closed_while_running = True


class _Proc:
    def __init__(self):
        self.events = []
        self.running = True
        self.stdout_closed_while_running = False
        self.stdin = _Pipe(self, "stdin")
        self.stdout = _Pipe(self, "stdout")

    def poll(self):
        return None if self.running else 0

    def terminate(self):
        self.events.append("terminate")
        self.running = False

    def wait(self, timeout=None):
        self.events.append(f"wait({timeout})")
        return 0

    def kill(self):
        self.events.append("kill")
        self.running = False


def test_terminate_guardian_process_does_not_close_stdout_before_terminate():
    proc = _Proc()

    BacktestService._terminate_guardian_process(proc)

    assert not proc.stdout_closed_while_running
    assert "terminate" in proc.events
    assert proc.events.index("terminate") < proc.events.index("stdout.close")
