# Systemd Canonicalization Design

**Date:** 2026-02-05
**Scope:** systemd(system) を正本に統一し、運用手順とドキュメントを整合させる。

## Goal
systemd(system) を唯一の正本とし、user unit の常用による二重起動・ポート競合を防止する。

## Non-Goals
- systemd ユニット自体の大規模改修
- run.sh/brain.lisp の起動方式変更
- 既存の運用フロー全体の再設計

## Current Facts
- `docs/llm/STATE.md` は systemd(system) を正本と明記している。
- `doc/owners_guide.md` は system/user 両方確認を推奨し、運用上の混乱が残っている。
- `doc/runbook.md` は systemd(system) 前提の記述が多い。

## Design Decisions
- 正本は systemd(system) に統一する。
- `systemctl --user` は診断用途（残存プロセス検出）に限定する。
- 誤って user unit を起動した場合の復旧手順を明文化する。

## Planned Documentation Updates
1. `doc/owners_guide.md`
   - 「systemd状態と実稼働の不一致」ブロックを再構成し、正本を systemd(system) と明記。
   - `systemctl --user` は診断用途限定と明記。
   - user unit の stop/disable と復旧手順（system 再起動 + ポート確認）を追加。
2. `doc/runbook.md`
   - systemd(system) 前提へ統一。
   - user unit への記述があれば診断用途に限定。
3. `docs/llm/STATE.md`
   - 既に正本は systemd(system) と明記済み。
   - user unit を常用しない方針が未記載なら追記候補。

## Verification (Operational)
- `sudo systemctl status swimmy-brain` が active (running)
- `systemctl --user status swimmy-brain` が inactive/disabled (or stopped)
- `ss -lntp | rg '5555|5556|5581'` で brain の LISTEN が単一プロセス

## Risks
- user unit を使った運用が現場に残っている場合、移行時に混乱する可能性。
- 既存の user unit の設定が systemd(system) と違うため、期待挙動に差異が出る可能性。

## Rollback
- ドキュメントの変更のみであり、運用変更は手順に従い systemd(user) を再度有効化することで戻せる。
