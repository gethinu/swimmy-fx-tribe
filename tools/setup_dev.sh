#!/bin/bash
set -e

echo "📦 Installing pre-commit hook..."
if [ -f "hooks/pre-commit" ]; then
    cp hooks/pre-commit .git/hooks/pre-commit
    chmod +x .git/hooks/pre-commit
    echo "✅ Pre-commit hook installed"
else
    echo "⚠️ hooks/pre-commit not found!"
fi
