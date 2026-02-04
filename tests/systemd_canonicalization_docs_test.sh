#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
state="$root/docs/llm/STATE.md"

rg -q "systemd\(system\) を正本" "$state"
