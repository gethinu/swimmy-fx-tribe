# Guarddog run.sh Cleanup Design

**Goal:** Systemd-only Brain運用を強制し、二重起動（stray `run.sh`）によるZMQポート競合を5分以内に自動解消する。

## Architecture
- 既存の cron watchdog `tools/check_guardian_health.sh`（5分毎）に **Brainのstray検知/停止** を追加する。
- `systemctl --user show -p MainPID swimmy-brain.service` で **正規MainPID** を取得。
- `pgrep -af "/home/swimmy/swimmy/run.sh"` で `run.sh` 起動プロセスを列挙し、**MainPID以外**を停止対象にする。
- Guardian監視・再起動ロジックは **一切変更せず**、Guardianが正常時は従来通り `exit 0`。

## Data Flow
1. Guardianがactive → stray cleanupブロック実行 → ログ出力 → exit 0
2. Guardianがinactive → 既存の再起動/Discord通知フローへ（cleanupは実行しない）

## Error Handling & Safety
- `systemctl`が失敗・MainPID=0 の場合は **systemd未管理とみなし**、`run.sh` を全停止（systemd-only方針の強制）。
- 停止は **TERM → 短い待機 → KILL** の順で実施。
- MainPIDは必ず除外し、誤停止を防止する。
- Discord通知は不要（抑制）。cronログに残すのみ。

## Testing
- 新規テスト `tests/test_check_guardian_health.sh` で **フェイクコマンド**を注入し挙動確認。
  - `SYSTEMCTL_CMD` / `PGREP_CMD` / `KILL_CMD` / `SLEEP_CMD` を使い、実サービスに影響しないようにする。
  - Guardian active + MainPID + stray PID のシナリオで **strayだけ kill** を期待。
- 追加の簡易検証: `bash -n tools/check_guardian_health.sh`

## Docs
- `docs/llm/STATE.md` に「cron watchdog が stray run.sh を自動停止する」旨を追記。
