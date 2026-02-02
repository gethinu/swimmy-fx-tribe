import json
import os
import tempfile

from migrate_local_storage_to_sexp import migrate_file


with tempfile.TemporaryDirectory() as d:
    src = os.path.join(d, "backtest_cache.json")
    with open(src, "w", encoding="utf-8") as f:
        json.dump([{"name": "A", "timestamp": 1, "result": {"sharpe": 0.1}}], f)
    out = migrate_file(src)
    assert out.endswith(".sexp")
    assert os.path.exists(out)

print("OK")
