#!/usr/bin/env python3
"""Lisp-aware paren/bracket balance checker. Skips ; line comments, #| |# block
comments, "strings", #\char literals, and |pipe symbols|. Reports the first
imbalance. Usage: python balance.py <file.lisp> ..."""
import sys
def check(path):
    s = open(path, encoding="utf-8").read()
    depth = 0; i = 0; n = len(s); line = 1
    stack = []
    while i < n:
        c = s[i]
        if c == "\n": line += 1; i += 1; continue
        if c == ";":
            while i < n and s[i] != "\n": i += 1
            continue
        if c == "#" and i+1 < n and s[i+1] == "|":
            d = 1; i += 2
            while i < n and d:
                if s[i:i+2] == "#|": d += 1; i += 2
                elif s[i:i+2] == "|#": d -= 1; i += 2
                else:
                    if s[i] == "\n": line += 1
                    i += 1
            continue
        if c == "#" and i+1 < n and s[i+1] == "\\":
            i += 3  # #\x  (also #\Space etc. — approx: skip the backslash+one char; word chars handled below)
            while i < n and (s[i].isalnum() or s[i] == "-"): i += 1
            continue
        if c == '"':
            i += 1
            while i < n:
                if s[i] == "\\": i += 2; continue
                if s[i] == '"': i += 1; break
                if s[i] == "\n": line += 1
                i += 1
            continue
        if c == "|":
            i += 1
            while i < n and s[i] != "|":
                if s[i] == "\n": line += 1
                i += 1
            i += 1
            continue
        if c in "([":
            stack.append((c, line)); i += 1; continue
        if c in ")]":
            if not stack:
                print(f"FAIL {path}: unmatched '{c}' at line {line}"); return False
            op, ol = stack.pop(); i += 1; continue
        i += 1
    if stack:
        op, ol = stack[-1]
        print(f"FAIL {path}: unclosed '{op}' opened at line {ol} ({len(stack)} unclosed)"); return False
    print(f"OK   {path}: balanced")
    return True
ok = all(check(p) for p in sys.argv[1:])
sys.exit(0 if ok else 1)
