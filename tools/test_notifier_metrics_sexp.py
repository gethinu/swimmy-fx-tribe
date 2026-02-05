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

import notifier
from sexp_utils import load_sexp_alist


with tempfile.TemporaryDirectory() as d:
    metrics_path = Path(d) / "notifier_metrics.sexp"
    notifier.write_notifier_metrics_sexp(
        str(metrics_path),
        queue_len=5,
        drops=2,
        errors=1,
        timestamp=123,
    )
    data = load_sexp_alist(str(metrics_path))
    assert data["schema_version"] == 1
    assert data["timestamp"] == 123
    assert data["queue_len"] == 5
    assert data["drops"] == 2
    assert data["errors"] == 1

print("OK")

