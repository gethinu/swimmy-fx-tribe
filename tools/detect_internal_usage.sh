#!/bin/bash
# detect_internal_usage.sh - Find usages of internal symbols (::)

echo "🔍 Scanning for internal symbol usage (::)..."
echo "============================================="

# Find all Lisp files
find src/lisp -name "*.lisp" | while read f; do
    # Grep for :: but exclude comments and common false positives
    grep -n "::" "$f" | grep -v "^;" | grep -v "\";" > /tmp/grep_results.txt
    
    if [ -s /tmp/grep_results.txt ]; then
        echo "📂 $f"
        cat /tmp/grep_results.txt
        echo ""
    fi
done

echo "============================================="
echo "✅ Scan complete."
