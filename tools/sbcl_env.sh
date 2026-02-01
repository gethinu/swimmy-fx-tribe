#!/usr/bin/env bash
set -euo pipefail

# Standard SBCL dynamic space size (MB). Override via env.
SWIMMY_SBCL_DYNAMIC_SPACE_MB="${SWIMMY_SBCL_DYNAMIC_SPACE_MB:-4096}"
export SWIMMY_SBCL_DYNAMIC_SPACE_MB
