import subprocess
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "tools" / "ops" / "cpcv_smoke.py"


class TestCpcvSmokeCli(unittest.TestCase):
    def run_cli(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            cwd=str(REPO_ROOT),
            capture_output=True,
            text=True,
            check=False,
        )

    def test_runtime_dry_run_contains_missing_file(self):
        proc = self.run_cli("--mode", "runtime", "--dry-run")
        self.assertEqual(
            proc.returncode,
            0,
            msg=f"stdout={proc.stdout}\nstderr={proc.stderr}",
        )
        self.assertIn("CPCV_VALIDATE", proc.stdout)
        self.assertIn("/tmp/does-not-exist.csv", proc.stdout)
        self.assertIn("MANUAL-RUNTIME-", proc.stdout)

    def test_criteria_dry_run_contains_default_candles_file(self):
        proc = self.run_cli("--mode", "criteria", "--dry-run")
        self.assertEqual(
            proc.returncode,
            0,
            msg=f"stdout={proc.stdout}\nstderr={proc.stderr}",
        )
        self.assertIn("CPCV_VALIDATE", proc.stdout)
        self.assertIn("/data/historical/USDJPY_M1.csv", proc.stdout)
        self.assertIn("MANUAL-CRITERIA-", proc.stdout)


if __name__ == "__main__":
    unittest.main()
