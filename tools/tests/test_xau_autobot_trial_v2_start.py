import json
import os
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools" / "xau_autobot_trial_v2_start.sh"


def test_trial_start_rejects_overlength_comment_before_run_meta_write() -> None:
    with tempfile.TemporaryDirectory() as td:
        tempdir = Path(td)
        bad_config = tempdir / "bad_trial_config.json"
        run_meta = tempdir / "run_meta.json"

        bad_config.write_text(
            json.dumps(
                {
                    "symbol": "XAUUSD",
                    "timeframe": "M5",
                    "magic": 560077,
                    "comment": "xau_autobot_trial_v2_20260226_v50_8_30d_r2",
                },
                ensure_ascii=True,
            )
            + "\n",
            encoding="utf-8",
        )

        env = os.environ.copy()
        env.update(
            {
                "XAU_AUTOBOT_TRIAL_ALLOW_EXISTING_PROCESSES": "1",
                "XAU_AUTOBOT_TRIAL_CONFIG": str(bad_config),
                "XAU_AUTOBOT_TRIAL_RUN_ID": "trial_v2_bad_comment_test",
                "XAU_AUTOBOT_TRIAL_RUN_META_PATH": str(run_meta),
            }
        )

        proc = subprocess.run(
            ["bash", str(SCRIPT)],
            cwd=ROOT,
            env=env,
            text=True,
            capture_output=True,
            timeout=30,
            check=False,
        )

        assert proc.returncode != 0
        assert "trial_config_invalid_comment_length" in proc.stderr
        assert not run_meta.exists()
