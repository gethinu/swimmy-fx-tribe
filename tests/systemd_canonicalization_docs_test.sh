#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state="$root/docs/llm/STATE.md"
owners="$root/doc/owners_guide.md"

rg -q "systemd\(system\) を正本" "$state"
rg -q "systemd\(system\) を正本" "$owners"
rg -q "systemctl --user disable swimmy-brain" "$owners"
