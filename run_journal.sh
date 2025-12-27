#!/bin/bash
# Auto-Journal Run Script
# Usage: ./run_journal.sh [capture|summarize|daemon|status|cleanup]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Load environment
source .env 2>/dev/null || true

# Activate venv
source .venv/bin/activate

# Run auto_journal
python -m auto_journal.auto_journal "$@"
