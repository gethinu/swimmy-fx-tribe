#!/bin/bash

echo "🧹 Cleaning up cache and FASL files..."
rm -rf ~/.cache/common-lisp/ || true
rm -f *.fasl
echo "✨ Clean complete"
