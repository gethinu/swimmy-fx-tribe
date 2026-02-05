import subprocess
import sys


def main() -> int:
    result = subprocess.run(
        [sys.executable, "tools/report_system_status.py", "--dry-run"],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert "Stats (Sharded)" in result.stdout
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
