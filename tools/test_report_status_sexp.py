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

import report_status as rs


with tempfile.TemporaryDirectory() as d:
    metrics_path = os.path.join(d, "system_metrics.sexp")
    with open(metrics_path, "w", encoding="utf-8") as f:
        f.write(
            "((schema_version . 1) (uptime-seconds . 3600) "
            "(heap-used-mb . 512.5) (strategy-count . 9))"
        )
    metrics = rs.load_system_metrics(metrics_path)
    assert metrics["uptime_seconds"] == 3600
    assert metrics["heap_used_mb"] == 512.5
    assert metrics["strategy_count"] == 9

with tempfile.TemporaryDirectory() as d:
    cache_path = os.path.join(d, "backtest_cache.sexp")
    with open(cache_path, "w", encoding="utf-8") as f:
        f.write(
            "((schema_version . 1) (entries . (((name . \"StratB\") "
            "(result . ((sharpe . 0.7) (profit-factor . 1.2) (trades . 10) "
            "(win-rate . 0.4)))))))"
        )
    cache = rs.load_backtest_cache(cache_path)
    assert cache[0]["name"] == "StratB"
    result = cache[0]["result"]
    assert result["sharpe"] == 0.7
    assert result["profit_factor"] == 1.2
    assert result["trades"] == 10
    assert result["win_rate"] == 0.4

print("OK")
