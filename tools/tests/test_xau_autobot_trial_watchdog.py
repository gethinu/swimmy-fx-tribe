import unittest
from datetime import datetime, timedelta, timezone
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import Mock, patch

from tools import xau_autobot_trial_watchdog as watchdog


def _iso(dt: datetime) -> str:
    return dt.astimezone(timezone.utc).isoformat()


class TestXauAutobotTrialWatchdog(unittest.TestCase):
    def test_zero_trade_gate_triggers_after_24h_with_no_closed_positions(self) -> None:
        start = datetime(2026, 2, 24, 0, 0, tzinfo=timezone.utc)
        end = start + timedelta(hours=25)
        live_report = {
            "start_utc": _iso(start),
            "end_utc": _iso(end),
            "summary": {"closed_positions": 0.0},
        }
        judge_report = {"summary": {"closed_positions": 0.0}}

        out = watchdog.evaluate_zero_trade_gate(
            run_id="trial_v2_test",
            live_report=live_report,
            judge_report=judge_report,
            min_window_hours=24.0,
            max_closed_positions=0.0,
        )

        self.assertTrue(out["triggered"])
        self.assertEqual("NO_GO_ROTATE_REQUIRED", out["decision"])
        self.assertIn("zero_closed_positions_24h", out["reason_codes"])

    def test_zero_trade_gate_does_not_trigger_when_closed_positions_exist(self) -> None:
        start = datetime(2026, 2, 24, 0, 0, tzinfo=timezone.utc)
        end = start + timedelta(hours=25)
        live_report = {
            "start_utc": _iso(start),
            "end_utc": _iso(end),
            "summary": {"closed_positions": 1.0},
        }
        judge_report = {"summary": {"closed_positions": 1.0}}

        out = watchdog.evaluate_zero_trade_gate(
            run_id="trial_v2_test",
            live_report=live_report,
            judge_report=judge_report,
            min_window_hours=24.0,
            max_closed_positions=0.0,
        )

        self.assertFalse(out["triggered"])
        self.assertEqual("CONTINUE", out["decision"])
        self.assertEqual([], out["reason_codes"])

    def test_zero_trade_gate_does_not_trigger_before_24h(self) -> None:
        start = datetime(2026, 2, 24, 0, 0, tzinfo=timezone.utc)
        end = start + timedelta(hours=12)
        live_report = {
            "start_utc": _iso(start),
            "end_utc": _iso(end),
            "summary": {"closed_positions": 0.0},
        }
        judge_report = {"summary": {"closed_positions": 0.0}}

        out = watchdog.evaluate_zero_trade_gate(
            run_id="trial_v2_test",
            live_report=live_report,
            judge_report=judge_report,
            min_window_hours=24.0,
            max_closed_positions=0.0,
        )

        self.assertFalse(out["triggered"])
        self.assertEqual("CONTINUE", out["decision"])

    @patch("tools.xau_autobot_trial_watchdog.subprocess.Popen")
    @patch("tools.xau_autobot_trial_watchdog.subprocess.run")
    @patch("tools.xau_autobot_trial_watchdog._find_autobot_pids")
    def test_rotate_trial_run_starts_new_trial_process(
        self,
        mock_find_pids: Mock,
        mock_run: Mock,
        mock_popen: Mock,
    ) -> None:
        mock_find_pids.return_value = [111, 222]
        fake_proc = Mock()
        fake_proc.pid = 9999
        mock_popen.return_value = fake_proc

        with TemporaryDirectory() as td:
            root = Path(td)
            (root / "tools").mkdir(parents=True, exist_ok=True)
            (root / "tools" / "xau_autobot_trial_v2_start.sh").write_text(
                "#!/usr/bin/env bash\nexit 0\n",
                encoding="utf-8",
            )
            log_path = root / "data" / "runtime" / "start.log"

            out = watchdog.rotate_trial_run(
                root=root,
                next_config="tools/configs/next.json",
                next_run_id="trial_v2_rotated_test",
                log_path=log_path,
            )

            self.assertEqual([111, 222], out["stopped_pids"])
            self.assertEqual(9999, out["started_pid"])
            self.assertEqual("trial_v2_rotated_test", out["next_run_id"])
            self.assertEqual("tools/configs/next.json", out["next_config"])
            self.assertTrue(log_path.exists())

            mock_run.assert_called_once()
            kill_cmd = mock_run.call_args.args[0]
            self.assertEqual("kill", kill_cmd[0])
            self.assertEqual(["111", "222"], kill_cmd[1:])

            self.assertTrue(mock_popen.called)
            popen_args = mock_popen.call_args.args[0]
            popen_kwargs = mock_popen.call_args.kwargs
            self.assertIn("xau_autobot_trial_v2_start.sh", popen_args[0])
            self.assertEqual("trial_v2_rotated_test", popen_kwargs["env"]["XAU_AUTOBOT_TRIAL_RUN_ID"])
            self.assertEqual("tools/configs/next.json", popen_kwargs["env"]["XAU_AUTOBOT_TRIAL_CONFIG"])


if __name__ == "__main__":
    unittest.main()
