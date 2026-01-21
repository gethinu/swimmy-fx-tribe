#!/usr/bin/env python3
import os
import sys

# Expert Panel Config (Musk/Uncle Bob)
MAX_LINES = 600
MAX_FUNCTIONS = 25
EXCLUDELIST = [
    "src/lisp/school/school-hunter.lisp",
    "src/lisp/school/school-hunter-auto.lisp",  # P9: Auto-generated strategies
    "src/lisp/dsl.lisp",
    "src/lisp/strategies/strategies-dynamic.lisp",
    "src/lisp/school/school-optimized-params.lisp",
    "src/lisp/school/graveyard-persistence.lisp",
]  # Generated/Legacy files allowed


def analyzed_file(filepath):
    with open(filepath, "r") as f:
        lines = f.readlines()

    line_count = len(lines)
    func_count = sum(1 for line in lines if "(defun " in line)

    return line_count, func_count


def main():
    root_dir = "src/lisp"
    violations = []

    print(f"🦅 SRP GESTAPO: Scanning {root_dir}...")
    print(f"   Max Lines: {MAX_LINES} | Max Functions: {MAX_FUNCTIONS}")

    for subdir, dirs, files in os.walk(root_dir):
        for file in files:
            if file.endswith(".lisp"):
                path = os.path.join(subdir, file)
                rel_path = os.path.relpath(path, os.getcwd())

                if rel_path in EXCLUDELIST:
                    continue

                lines, funcs = analyzed_file(path)

                if lines > MAX_LINES:
                    violations.append(
                        f"❌ {rel_path}: {lines} lines (Max {MAX_LINES}) - Too Big (God Class)"
                    )
                if funcs > MAX_FUNCTIONS:
                    violations.append(
                        f"❌ {rel_path}: {funcs} functions (Max {MAX_FUNCTIONS}) - Too Many Responsibilities"
                    )

    if violations:
        print("\n🚨 SRP VIOLATIONS DETECTED:")
        for v in violations:
            print(v)
        print("\nVerdict: FAILED. Split these files immediately.")
        sys.exit(1)
    else:
        print("\n✅ All files compliant with SRP.")
        sys.exit(0)


if __name__ == "__main__":
    main()
