import os
import sys
from pathlib import Path


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))

import tempfile

from sexp_utils import load_sexp_alist, load_sexp_list, parse_sexp_alist, parse_sexp_list

# Alist parse
alist = parse_sexp_alist('((schema_version . 1) (name . "X") (value . 42))')
assert alist["schema_version"] == 1
assert alist["name"] == "X"
assert alist["value"] == 42

# List of alists parse
entries = parse_sexp_list('(((name . "A") (sharpe . 1.0)) ((name . "B") (sharpe . 2.0)))')
assert entries[0]["name"] == "A"
assert entries[1]["sharpe"] == 2.0

# Invalid input should raise
try:
    parse_sexp_alist("(")
    raise AssertionError("Expected parse error")
except Exception:
    pass

# File load - alist
with tempfile.TemporaryDirectory() as d:
    alist_path = os.path.join(d, "metrics.sexp")
    with open(alist_path, "w", encoding="utf-8") as f:
        f.write('((daily-pnl . 100) (goal-progress . 1.5))')
    loaded = load_sexp_alist(alist_path)
    assert loaded["daily_pnl"] == 100
    assert loaded["goal_progress"] == 1.5

# File load - list
with tempfile.TemporaryDirectory() as d:
    list_path = os.path.join(d, "backtest_cache.sexp")
    with open(list_path, "w", encoding="utf-8") as f:
        f.write('((schema_version . 1) (entries . (((name . "A") (sharpe . 1.0)))))')
    entries = load_sexp_list(list_path)
    assert entries[0]["name"] == "A"
    assert entries[0]["sharpe"] == 1.0

print("OK")
