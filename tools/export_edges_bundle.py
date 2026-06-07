#!/usr/bin/env python3
"""Export the full strategy library (data/library/<TIER>/*.lisp) into a portable
edge bundle for downstream repos (e.g. mt5_bundle_of_edges).

Each library file is a Common Lisp ``#S(SWIMMY.SCHOOL::STRATEGY ...)`` struct dump.
This tool parses every struct (including the ``#A(...)`` string-array reader macro
quirk) and emits language-agnostic artifacts:

  exports/mt5_bundle_of_edges/edges_library.json  - full structured records + header
  exports/mt5_bundle_of_edges/edges_index.csv     - flat triage table
  exports/mt5_bundle_of_edges/README.md           - schema + DSL glossary + caveats

The entry/exit DSL is preserved three ways for maximum reuse:
  *_lisp   : normalized lisp source (package prefixes stripped)
  *_ast    : nested-array AST (operator-prefix form) for programmatic translation
  *_pseudo : best-effort human-readable infix pseudocode

Re-runnable and idempotent. No external dependencies (stdlib only).
"""
import csv
import json
import os
import sys
from datetime import datetime, timezone

LIBRARY_DIR = "data/library"
OUT_DIR = "exports/mt5_bundle_of_edges"
TIERS = ["LEGEND", "S", "A", "B", "RETIRED"]

# Common Lisp universal-time epoch (1900-01-01) -> Unix epoch offset (seconds).
LISP_EPOCH_OFFSET = 2208988800

# Timeframe minutes -> conventional MT5 label.
TF_LABELS = {
    1: "M1", 5: "M5", 15: "M15", 30: "M30",
    60: "H1", 240: "H4", 1440: "D1", 10080: "W1", 43200: "MN1",
    300: "M5",  # 300s legacy buckets sometimes stored as seconds; see note below
    3600: "H1", 86400: "D1",
}


# --------------------------------------------------------------------------
# S-expression reader (handles #S struct, #A array, dotted pairs, keywords)
# --------------------------------------------------------------------------

class Sym:
    __slots__ = ("name",)

    def __init__(self, name):
        # strip package qualifier (SWIMMY.SCHOOL::EMA -> EMA, KEYWORD :FOO kept as :FOO)
        if "::" in name:
            name = name.split("::", 1)[1]
        elif ":" in name and not name.startswith(":"):
            # package:exported -> exported
            name = name.split(":", 1)[1]
        self.name = name

    def __repr__(self):
        return self.name


class Str:
    __slots__ = ("value",)

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return json.dumps(self.value)


class Reader:
    def __init__(self, text):
        self.s = text
        self.i = 0
        self.n = len(text)

    def peek(self):
        return self.s[self.i] if self.i < self.n else ""

    def skip_ws(self):
        while self.i < self.n:
            c = self.s[self.i]
            if c in " \t\r\n":
                self.i += 1
            elif c == ";":  # line comment
                while self.i < self.n and self.s[self.i] != "\n":
                    self.i += 1
            else:
                break

    def read(self):
        self.skip_ws()
        if self.i >= self.n:
            return None
        c = self.s[self.i]
        if c == "#":
            return self.read_hash()
        if c == "(":
            return self.read_list()
        if c == '"':
            return self.read_string()
        return self.read_atom()

    def read_hash(self):
        # at '#'
        self.i += 1
        c = self.peek()
        if c in "Ss":  # #S(TYPE ...)
            self.i += 1
            lst = self.read_list()
            struct_type = lst[0] if lst else None
            plist = lst[1:]
            d = {"__struct__": str(struct_type)}
            j = 0
            while j + 1 < len(plist) + 1 and j < len(plist):
                key = plist[j]
                val = plist[j + 1] if j + 1 < len(plist) else None
                d[str(key)] = val
                j += 2
            return d
        if c in "Aa":  # #A((dims) ELEMENT-TYPE . "string")
            self.i += 1
            lst = self.read_list()
            # find the trailing string literal
            for el in reversed(lst):
                if isinstance(el, Str):
                    return el
            return Str("")
        # unknown reader macro: skip the '#' char-group conservatively
        return self.read_atom()

    def read_list(self):
        assert self.peek() == "("
        self.i += 1
        out = []
        while True:
            self.skip_ws()
            c = self.peek()
            if c == "":
                break
            if c == ")":
                self.i += 1
                break
            if c == "." and self._is_lone_dot():
                # dotted pair: consume dot, continue reading the tail element(s)
                self.i += 1
                continue
            out.append(self.read())
        return out

    def _is_lone_dot(self):
        nxt = self.s[self.i + 1] if self.i + 1 < self.n else " "
        return nxt in " \t\r\n)"

    def read_string(self):
        assert self.peek() == '"'
        self.i += 1
        buf = []
        while self.i < self.n:
            c = self.s[self.i]
            if c == "\\":
                self.i += 1
                if self.i < self.n:
                    buf.append(self.s[self.i])
                    self.i += 1
                continue
            if c == '"':
                self.i += 1
                break
            buf.append(c)
            self.i += 1
        return Str("".join(buf))

    def read_atom(self):
        start = self.i
        while self.i < self.n and self.s[self.i] not in " \t\r\n()\";":
            self.i += 1
        tok = self.s[start:self.i]
        up = tok.upper()
        if up == "NIL":
            return None
        if up == "T":
            return True
        num = parse_number(tok)
        if num is not None:
            return num
        return Sym(tok)


def parse_number(tok):
    try:
        return int(tok)
    except ValueError:
        pass
    try:
        # accept lisp float exponent forms: 4.98e-4, 1.0d0
        t = tok.replace("d", "e").replace("D", "e")
        return float(t)
    except ValueError:
        return None


# --------------------------------------------------------------------------
# AST normalization helpers
# --------------------------------------------------------------------------

def to_jsonable(node):
    """Convert parsed node into JSON-friendly nested structure."""
    if isinstance(node, Sym):
        return node.name
    if isinstance(node, Str):
        return node.value
    if isinstance(node, list):
        return [to_jsonable(x) for x in node]
    if node is None:
        return None
    if node is True:
        return True
    return node


def serialize_lisp(node):
    if isinstance(node, Sym):
        return node.name
    if isinstance(node, Str):
        return json.dumps(node.value)
    if isinstance(node, list):
        return "(" + " ".join(serialize_lisp(x) for x in node) + ")"
    if node is None:
        return "NIL"
    if node is True:
        return "T"
    if isinstance(node, float):
        return repr(node)
    return str(node)


INFIX = {">": ">", "<": "<", ">=": ">=", "<=": "<=", "=": "==", "/=": "!="}
CROSS = {
    "CROSS-ABOVE": "crosses above", "CROSS-BELOW": "crosses below",
    "CROSS-OVER": "crosses over", "CROSS-UNDER": "crosses under",
}


def pseudo(node):
    """Best-effort human-readable infix rendering of a DSL clause."""
    if isinstance(node, Sym):
        return node.name
    if isinstance(node, Str):
        return node.value
    if node is None:
        return "NIL"
    if node is True:
        return "T"
    if not isinstance(node, list):
        return str(node)
    if not node:
        return "()"
    head = node[0]
    name = head.name if isinstance(head, Sym) else None
    args = node[1:]
    if name in ("AND", "OR"):
        joiner = f" {name} "
        return "(" + joiner.join(pseudo(a) for a in args) + ")"
    if name in INFIX and len(args) == 2:
        return f"{pseudo(args[0])} {INFIX[name]} {pseudo(args[1])}"
    if name in CROSS and len(args) == 2:
        return f"{pseudo(args[0])} {CROSS[name]} {pseudo(args[1])}"
    # keyword-style clauses, e.g. (:RSI-BELOW 10), (:CROSS-OVER :SMA 50 :SMA 200)
    if name and name.startswith(":"):
        label = name[1:].replace("-", " ").lower()
        rest = " ".join(pseudo(a) for a in args)
        return f"{label} {rest}".strip()
    # fallback: function-style
    return name + "(" + ", ".join(pseudo(a) for a in args) + ")" if name else serialize_lisp(node)


def unwrap_clause(node):
    """Library entries are sometimes double-wrapped: ((:RSI-BELOW 10)).
    Unwrap a single-element list-of-list to the inner clause for readability."""
    if isinstance(node, list) and len(node) == 1 and isinstance(node[0], list):
        return node[0]
    return node


def norm_indicators(node):
    out = []
    if not isinstance(node, list):
        return out
    for ind in node:
        if isinstance(ind, list):
            parts = []
            for p in ind:
                if isinstance(p, Sym):
                    parts.append(p.name.lstrip(":"))
                else:
                    parts.append(to_jsonable(p))
            out.append(parts)
        elif isinstance(ind, Sym):
            out.append([ind.name.lstrip(":")])
    return out


# --------------------------------------------------------------------------
# Field extraction
# --------------------------------------------------------------------------

def keyname(k):
    return k.lstrip(":").lower().replace("-", "_")


def getv(d, key, default=None):
    # Struct keys are stored verbatim from lisp (hyphenated, leading colon),
    # e.g. ":PROFIT-FACTOR". Accept underscore/hyphen and colon/no-colon forms.
    up = key.upper()
    for k in (":" + up.replace("_", "-"), ":" + up.replace("-", "_"),
              ":" + up, up.replace("_", "-"), up):
        if k in d:
            return d[k]
    return default


def lisp_time_to_iso(v):
    if not isinstance(v, int) or v <= 0:
        return None
    try:
        ts = v - LISP_EPOCH_OFFSET
        return datetime.fromtimestamp(ts, tz=timezone.utc).isoformat()
    except (OverflowError, OSError, ValueError):
        return None


def tf_label(minutes):
    if not isinstance(minutes, int):
        return None
    return TF_LABELS.get(minutes, f"M{minutes}")


TEST_MARKERS = ("TEST", "RECRUIT-RND", "TESTSTRAT", "REFRESH-RECON")


def is_test_artifact(name):
    up = (name or "").upper()
    return any(m in up for m in TEST_MARKERS)


def sym_to_str(node):
    if isinstance(node, Sym):
        return node.name.lstrip(":")
    if isinstance(node, Str):
        return node.value
    if node is None:
        return None
    return str(node)


def build_record(struct, tier, path):
    name = sym_to_str(getv(struct, "name")) or os.path.basename(path)[:-5]
    entry = unwrap_clause(getv(struct, "entry"))
    exit_ = unwrap_clause(getv(struct, "exit"))

    def num(key, default=None):
        v = getv(struct, key, default)
        return v if isinstance(v, (int, float)) else default

    rec = {
        "name": name,
        "source_tier": tier,
        "rank": sym_to_str(getv(struct, "rank")),
        "stored_tier": sym_to_str(getv(struct, "tier")),
        "status": sym_to_str(getv(struct, "status")),
        "status_reason": sym_to_str(getv(struct, "status_reason")) or "",
        "category": sym_to_str(getv(struct, "category")),
        "indicator_type": sym_to_str(getv(struct, "indicator_type")),
        "symbol": sym_to_str(getv(struct, "symbol")),
        "timeframe_minutes": getv(struct, "timeframe"),
        "timeframe_label": tf_label(getv(struct, "timeframe")),
        "direction": sym_to_str(getv(struct, "direction")),
        "indicators": norm_indicators(getv(struct, "indicators")),
        "entry_lisp": serialize_lisp(entry),
        "entry_ast": to_jsonable(entry),
        "entry_pseudo": pseudo(entry),
        "exit_lisp": serialize_lisp(exit_),
        "exit_ast": to_jsonable(exit_),
        "exit_pseudo": pseudo(exit_),
        "sl": num("sl"),
        "tp": num("tp"),
        "volume": num("volume"),
        "metrics": {
            "sharpe": num("sharpe"),
            "profit_factor": num("profit_factor"),
            "win_rate": num("win_rate"),
            "trades": num("trades"),
            "max_dd": num("max_dd"),
            "oos_sharpe": num("oos_sharpe"),
            "cpcv_pass_rate": num("cpcv_pass_rate"),
            "cpcv_median_sharpe": num("cpcv_median_sharpe"),
            "cpcv_median_pf": num("cpcv_median_pf"),
            "cpcv_median_wr": num("cpcv_median_wr"),
            "cpcv_median_maxdd": num("cpcv_median_maxdd"),
        },
        "provenance": {
            "generation": getv(struct, "generation"),
            "breeding_count": getv(struct, "breeding_count"),
            "parents": to_jsonable(getv(struct, "parents")),
            "hash": sym_to_str(getv(struct, "hash")),
            "immortal": bool(getv(struct, "immortal")),
            "creation_time_lisp": getv(struct, "creation_time"),
            "creation_time_iso": lisp_time_to_iso(getv(struct, "creation_time")),
            "confidence_estimator": sym_to_str(getv(struct, "confidence_estimator")),
        },
        "likely_test_artifact": is_test_artifact(name),
        "source_file": path,
    }
    return rec


def main():
    if not os.path.isdir(LIBRARY_DIR):
        print(f"[ERR] {LIBRARY_DIR} not found (run from repo root)", file=sys.stderr)
        return 1
    os.makedirs(OUT_DIR, exist_ok=True)

    records = []
    errors = []
    for tier in TIERS:
        d = os.path.join(LIBRARY_DIR, tier)
        if not os.path.isdir(d):
            continue
        for fn in sorted(os.listdir(d)):
            if not fn.endswith(".lisp"):
                continue
            path = os.path.join(d, fn)
            try:
                with open(path, "r", encoding="utf-8", errors="replace") as f:
                    text = f.read()
                node = Reader(text).read()
                if not isinstance(node, dict) or "__struct__" not in node:
                    errors.append((path, "not a struct"))
                    continue
                records.append(build_record(node, tier, path))
            except Exception as e:  # noqa: BLE001
                errors.append((path, repr(e)))

    by_tier = {}
    test_count = 0
    for r in records:
        by_tier[r["source_tier"]] = by_tier.get(r["source_tier"], 0) + 1
        if r["likely_test_artifact"]:
            test_count += 1

    header = {
        "bundle": "mt5_bundle_of_edges",
        "source_repo": "swimmy-fx-tribe",
        "generated_utc": datetime.now(timezone.utc).isoformat(),
        "total_strategies": len(records),
        "by_tier": by_tier,
        "likely_test_artifacts": test_count,
        "parse_errors": [{"file": p, "error": e} for p, e in errors],
        "tier_meaning": {
            "LEGEND": "protected/immortal strategies (never culled)",
            "S": "research-elite rank (Sharpe>=0.75, PF>=1.70, CPCV pass>=70%)",
            "A": "OOS-validated (Sharpe>=0.45, PF>=1.30, OOS Sharpe>=0.35)",
            "B": "Phase1 backtest passed (Sharpe>=0.15, PF>=1.05)",
            "RETIRED": "archived (low-weight learning data)",
        },
        "caveats": [
            "Metrics are Swimmy in-sample/backtest values; re-validate before treating as edge.",
            "Some A/S/B entries are test artifacts (see likely_test_artifact flag).",
            "timeframe_minutes for legacy reversion legends may be stored in seconds (e.g. 300=M5).",
            "entry/exit use Swimmy DSL; see README glossary for operator semantics.",
        ],
    }

    json_path = os.path.join(OUT_DIR, "edges_library.json")
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump({"meta": header, "strategies": records}, f,
                  ensure_ascii=False, indent=2)

    csv_path = os.path.join(OUT_DIR, "edges_index.csv")
    cols = ["name", "source_tier", "rank", "symbol", "timeframe_label",
            "direction", "category", "sharpe", "profit_factor", "win_rate",
            "trades", "max_dd", "oos_sharpe", "sl", "tp",
            "entry_pseudo", "exit_pseudo", "likely_test_artifact"]
    with open(csv_path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(cols)
        for r in records:
            m = r["metrics"]
            w.writerow([
                r["name"], r["source_tier"], r["rank"], r["symbol"],
                r["timeframe_label"], r["direction"], r["category"],
                m["sharpe"], m["profit_factor"], m["win_rate"], m["trades"],
                m["max_dd"], m["oos_sharpe"], r["sl"], r["tp"],
                r["entry_pseudo"], r["exit_pseudo"], r["likely_test_artifact"],
            ])

    print(f"[OK] {len(records)} strategies -> {json_path}")
    print(f"[OK] index -> {csv_path}")
    print(f"[OK] by_tier={by_tier} test_artifacts={test_count} errors={len(errors)}")
    for p, e in errors:
        print(f"  [ERR] {p}: {e}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
