import json
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

from tools import xau_autobot_live_loop_guard as guard


class TestXauAutobotLiveLoopGuard(unittest.TestCase):
    def test_default_meta_entries_include_r3_only_when_enabled(self) -> None:
        root = Path("/tmp/swimmy")
        entries_without_r3 = guard._build_default_meta_entries(root, include_r3=False)
        entries_with_r3 = guard._build_default_meta_entries(root, include_r3=True)

        self.assertEqual(2, len(entries_without_r3))
        self.assertEqual(3, len(entries_with_r3))
        slots = {slot for slot, _ in entries_with_r3}
        self.assertIn("explore_alpha3", slots)

    def test_load_expected_loops_deduplicates_same_config(self) -> None:
        with TemporaryDirectory() as td:
            root = Path(td)
            meta_a = root / "current_run.json"
            meta_b = root / "current_run_r2.json"
            meta_c = root / "current_run_r3.json"

            payload_a = {
                "run_id": "trial_v2_primary",
                "trial_config": "tools/configs/xau_autobot.trial_v2_primary.json",
                "poll_seconds": "10",
            }
            payload_b = {
                "run_id": "trial_v2_primary_clone",
                "trial_config": "tools/configs/xau_autobot.trial_v2_primary.json",
                "poll_seconds": "10",
            }
            payload_c = {
                "run_id": "trial_v2_alpha",
                "trial_config": "tools/configs/xau_autobot.trial_v2_alpha.json",
                "poll_seconds": "10",
            }
            meta_a.write_text(json.dumps(payload_a), encoding="utf-8")
            meta_b.write_text(json.dumps(payload_b), encoding="utf-8")
            meta_c.write_text(json.dumps(payload_c), encoding="utf-8")

            loops = guard.load_expected_loops(
                [
                    ("primary", meta_a),
                    ("secondary", meta_b),
                    ("explore_alpha3", meta_c),
                ]
            )

            self.assertEqual(2, len(loops))
            keys = {item.config_key for item in loops}
            self.assertIn("xau_autobot.trial_v2_primary.json", keys)
            self.assertIn("xau_autobot.trial_v2_alpha.json", keys)

    def test_extract_config_path_from_commandline(self) -> None:
        cmd_quoted = (
            "powershell.exe -NoProfile -ExecutionPolicy Bypass -File "
            "\\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\windows\\xau_autobot_live_loop.ps1 "
            "-RepoRoot \\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy "
            "-ConfigPath \"\\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\configs\\xau_autobot.trial_v2_a.json\" "
            "-PythonExe C:\\Users\\x\\python.exe -PollSeconds 10 -Live"
        )
        cmd_plain = (
            "powershell.exe -NoProfile -ExecutionPolicy Bypass -File "
            "\\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\windows\\xau_autobot_live_loop.ps1 "
            "-ConfigPath \\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\configs\\xau_autobot.trial_v2_b.json "
            "-Live"
        )
        cmd_single = (
            "powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "
            "\"& '\\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\windows\\xau_autobot_live_loop.ps1' "
            "-ConfigPath '\\\\wsl.localhost\\Ubuntu\\home\\swimmy\\swimmy\\tools\\configs\\xau_autobot.trial_v2_c.json' -Live\""
        )

        path_a = guard.extract_config_path_from_commandline(cmd_quoted)
        path_b = guard.extract_config_path_from_commandline(cmd_plain)
        path_c = guard.extract_config_path_from_commandline(cmd_single)

        self.assertTrue(path_a.endswith("xau_autobot.trial_v2_a.json"))
        self.assertTrue(path_b.endswith("xau_autobot.trial_v2_b.json"))
        self.assertTrue(path_c.endswith("xau_autobot.trial_v2_c.json"))

    def test_plan_heal_actions_detects_missing_and_duplicates(self) -> None:
        expected = {
            "xau_autobot.trial_v2_a.json",
            "xau_autobot.trial_v2_b.json",
            "xau_autobot.trial_v2_c.json",
        }
        running = {
            "xau_autobot.trial_v2_a.json": [111, 222],
            "xau_autobot.trial_v2_b.json": [333],
            "xau_autobot.trial_v2_x.json": [999],
        }

        plan = guard.plan_heal_actions(expected, running)

        self.assertEqual(["xau_autobot.trial_v2_c.json"], plan["missing_keys"])
        self.assertEqual({"xau_autobot.trial_v2_a.json": [222]}, plan["duplicate_terminate_pids"])
        self.assertEqual([222], plan["terminate_pids"])

    @patch("tools.xau_autobot_live_loop_guard._windows_cmd_exe", return_value="/mnt/c/Windows/System32/cmd.exe")
    @patch("tools.xau_autobot_live_loop_guard.subprocess.run")
    def test_discover_windows_python_ignores_non_utf8_stdout(
        self,
        mock_run,
        _mock_cmd,
    ) -> None:
        class _Proc:
            returncode = 0
            # non-utf8 bytes around valid ASCII Windows path
            stdout = b"\x8f\xaa\x95\x9c\r\nC:\\Users\\stair\\AppData\\Local\\Programs\\Python\\Python312\\python.exe\r\n"

        mock_run.return_value = _Proc()

        path = guard._discover_windows_python()
        self.assertEqual(
            "C:\\Users\\stair\\AppData\\Local\\Programs\\Python\\Python312\\python.exe",
            path,
        )

    @patch("tools.xau_autobot_live_loop_guard.time.sleep")
    @patch("tools.xau_autobot_live_loop_guard.subprocess.run")
    def test_run_windows_command_retries_transient_vsock_error(self, mock_run, _mock_sleep) -> None:
        class _Proc:
            def __init__(self, code: int, stderr: str = "") -> None:
                self.returncode = code
                self.stderr = stderr
                self.stdout = "[]"

        mock_run.side_effect = [
            _Proc(1, "<3>WSL (3273494 - ) ERROR: UtilAcceptVsock:271: accept4 failed 110"),
            _Proc(0, ""),
        ]

        proc = guard._run_windows_command(
            ["/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe", "-NoProfile", "-Command", "echo ok"],
            text=True,
            max_attempts=2,
        )
        self.assertEqual(0, proc.returncode)
        self.assertEqual(2, mock_run.call_count)


if __name__ == "__main__":
    unittest.main()
