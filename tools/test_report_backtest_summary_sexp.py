import os
import sys
import tempfile
from pathlib import Path


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "tools"))
sys.path.insert(0, str(BASE_DIR / "src" / "python"))

import report_backtest_summary as rbs


with tempfile.TemporaryDirectory() as d:
    path = os.path.join(d, "backtest_cache.sexp")
    with open(path, "w", encoding="utf-8") as f:
        f.write(
            "((schema_version . 1) (entries . (((name . \"StratA\") "
            "(result . ((sharpe . 1.23) (profit-factor . 1.5) "
            "(trades . 30) (win-rate . 0.5)))))))"
        )
    cache = rbs.load_backtest_cache(path)
    assert cache[0]["name"] == "StratA"
    result = cache[0]["result"]
    assert result["sharpe"] == 1.23
    assert result["profit_factor"] == 1.5
    assert result["trades"] == 30
    assert result["win_rate"] == 0.5

print("OK")
