#!/usr/bin/env python3
"""Build a kill_oos_cpcv manifest from the CURRENT SQL population (post-backbone-fix).

Faithfully ports school-backtest.lisp:extract-sma-params + detect-indicator-type
so the manifest reproduces exactly what guardian's real BACKTEST path would run
(entry/exit AST is NOT sent; guardian runs SMA(short/long) golden/death cross).

Population = feed-eligible survivors: rank NOT in {SCOUT, GRAVEYARD} (matches
tools/tribe/export.py default exclude). Emits the ManifestEntry shape consumed
by guardian/src/bin/kill_oos_cpcv.rs, plus reported IS metrics for fidelity +
the honest_gate IS floor.
"""
from __future__ import annotations
import sqlite3, json, re, sys, argparse

DB = "data/memory/swimmy.db"
EXCLUDE = {"SCOUT", "GRAVEYARD"}

# --- minimal Lisp reader (enough for the :INDICATORS forms) -----------------
def tokenize(s: str):
    return re.findall(r'\(|\)|"[^"]*"|[^\s()]+', s)

def parse(tokens, i=0):
    """Return (node, next_index). node is a list, or an atom (str)."""
    out = []
    while i < len(tokens):
        t = tokens[i]
        if t == '(':
            node, i = parse(tokens, i + 1)
            out.append(node)
        elif t == ')':
            return out, i + 1
        else:
            out.append(atom(t))
            i += 1
    return out, i

def atom(t: str):
    if t.startswith('"') and t.endswith('"'):
        return t[1:-1]            # string literal
    # integer?
    if re.fullmatch(r'[+-]?\d+', t):
        return int(t)
    # float?
    if re.fullmatch(r'[+-]?\d*\.\d+(e[+-]?\d+)?', t, re.I):
        return float(t)
    return t                      # bare symbol token (e.g. SMA-5, SMA, PSAR)

def parse_indicators(ind_str: str):
    """Parse the top-level indicator list into a python list of items."""
    if not ind_str:
        return []
    toks = tokenize(ind_str.strip())
    node, _ = parse(toks, 0)
    # node is [ <the-outer-list> ] because the string starts with '('
    if len(node) == 1 and isinstance(node[0], list):
        return node[0]
    return node

def is_int(x):
    return isinstance(x, int) and not isinstance(x, bool)

def extract_sma_params(indicators):
    """Faithful port of school-backtest.lisp:extract-sma-params."""
    all_params = []
    for ind in indicators:
        if isinstance(ind, list):
            for n in ind[1:]:          # (cdr ind)
                if is_int(n):
                    all_params.append(n)
    if len(all_params) >= 2:
        srt = sorted(set(all_params))
        return srt[0], srt[1]
    if len(all_params) == 1:
        p = all_params[0]
        return max(3, p // 2), p
    # no integer params -> default branch keyed on first indicator head
    first = indicators[0] if indicators else None
    head = None
    if isinstance(first, list) and first:
        head = str(first[0]).lower().split("::")[-1]
    if head == "rsi":   return 7, 14
    if head == "bb":    return 10, 20
    if head == "stoch": return 7, 14
    if head == "macd":  return 12, 26
    return 5, 20

def detect_indicator_type(indicators, fallback: str) -> str:
    """Only Sma vs Ema matter to build_strategy in the Rust bin."""
    if fallback and fallback.lower() in ("ema", "sma"):
        # data_sexp :INDICATOR-TYPE is authoritative when present
        return fallback.lower()
    first = indicators[0] if indicators else None
    name = None
    if isinstance(first, list) and first:
        name = str(first[0]).lower()
    elif isinstance(first, str):
        name = first.lower()
    if name and "ema" in name:
        return "ema"
    return "sma"

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", default=DB)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    c = sqlite3.connect(args.db); c.row_factory = sqlite3.Row
    rows = c.execute("SELECT * FROM strategies").fetchall()
    manifest = []
    skipped = {"excluded_rank": 0, "no_indicators": 0}
    rank_counts = {}
    for r in rows:
        rank = (r["rank"] or "NIL").strip().upper().lstrip(":")
        if rank in EXCLUDE:
            skipped["excluded_rank"] += 1
            continue
        rank_counts[rank] = rank_counts.get(rank, 0) + 1
        ind_str = r["indicators"]
        # prefer data_sexp :INDICATORS if column empty
        if not ind_str:
            ds = r["data_sexp"] or ""
            m = re.search(r":INDICATORS\s*(\([^)]*(?:\([^)]*\)[^)]*)*\))", ds)
            ind_str = m.group(1) if m else ""
        indicators = parse_indicators(ind_str)
        short, long = extract_sma_params(indicators)
        # indicator_type from data_sexp
        it = "sma"
        ds = r["data_sexp"] or ""
        mit = re.search(r':INDICATOR-TYPE\s+"?([A-Za-z]+)"?', ds)
        if mit:
            it = detect_indicator_type(indicators, mit.group(1))
        tf_min = int(r["timeframe"] or 1)
        manifest.append({
            "name": r["name"],
            "symbol": r["symbol"] or "USDJPY",
            "category": r["category"] or ":TREND",
            "indicator_type": it,
            "timeframe_min": tf_min,
            "tf_seconds": tf_min * 60,
            "sma_short": int(short),
            "sma_long": int(long),
            "sl": float(r["sl"] or 0.0),
            "tp": float(r["tp"] or 0.0),
            "volume": float(r["volume"]) if r["volume"] not in (None, "", "NIL") else 0.01,
            "indicators_raw": ind_str[:200],
            "rank": rank,
            "is_trades": int(r["trades"]) if r["trades"] is not None else None,
            "is_pf": float(r["profit_factor"]) if r["profit_factor"] is not None else None,
            "is_sharpe": float(r["sharpe"]) if r["sharpe"] is not None else None,
        })
    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(manifest, f, indent=1, ensure_ascii=False)
    print(f"wrote {args.out}: {len(manifest)} strategies")
    print("rank counts:", rank_counts)
    print("skipped:", skipped)
    # quick diversity + sma distribution
    from collections import Counter
    print("symbols:", Counter(m["symbol"] for m in manifest))
    print("categories:", Counter(m["category"] for m in manifest))
    print("sma pairs (top10):", Counter((m["sma_short"], m["sma_long"]) for m in manifest).most_common(10))
    print("IS trades>=200:", sum(1 for m in manifest if (m["is_trades"] or 0) >= 200))

if __name__ == "__main__":
    main()
