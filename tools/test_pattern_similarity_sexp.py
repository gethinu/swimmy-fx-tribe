import csv
import shutil
import sys
import tempfile
from pathlib import Path
import numpy as np



def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))
sys.path.insert(0, str(BASE_DIR / "tools"))

from sexp_utils import parse_sexp_alist
import pattern_similarity_service as svc


def _make_h1_rows(count: int = 400):
    rows = []
    ts = 1700000000
    price = 140.0
    for i in range(count):
        drift = 0.02 if (i // 25) % 2 == 0 else -0.015
        open_ = price
        high = open_ + 0.08
        low = open_ - 0.07
        close = open_ + drift
        volume = 100 + (i % 30)
        rows.append([ts, open_, high, low, close, volume])
        ts += 3600
        price = close
    return rows


def _make_candles_from_rows(rows, n=120):
    out = []
    for r in rows[-n:]:
        out.append(
            {
                "timestamp": int(r[0]),
                "open": float(r[1]),
                "high": float(r[2]),
                "low": float(r[3]),
                "close": float(r[4]),
                "volume": int(r[5]),
            }
        )
    return out


def _seed_history(tmpdir: Path):
    hist_dir = tmpdir / "historical"
    hist_dir.mkdir(parents=True, exist_ok=True)
    rows = _make_h1_rows()
    path = hist_dir / "USDJPY_H1.csv"
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["timestamp", "open", "high", "low", "close", "volume"])
        writer.writerows(rows)
    return rows


def _make_training_samples(n=96, channels=5, window=120):
    rng = np.random.default_rng(7)
    samples = np.zeros((n, channels, window), dtype=np.float32)
    labels = np.zeros((n,), dtype=np.int32)
    for i in range(n):
        cls = i % 3
        labels[i] = cls
        x = np.linspace(0.0, 1.0, window, dtype=np.float32)
        if cls == 0:
            trend = x
        elif cls == 1:
            trend = -x
        else:
            trend = np.sin(np.linspace(0.0, np.pi * 4.0, window, dtype=np.float32)) * 0.2
        samples[i, 0] = trend + rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 1] = np.abs(rng.normal(0.03, 0.005, window)).astype(np.float32)
        samples[i, 2] = rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 3] = rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 4] = rng.normal(0.0, 0.01, window).astype(np.float32)
    return samples, labels


def test_status_contract():
    req = '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "STATUS"))'
    res = svc.handle_request_sexp(req)
    parsed = parse_sexp_alist(res)
    assert parsed["type"] == "PATTERN_SIMILARITY_RESULT"
    assert parsed["schema_version"] == 1
    assert parsed["status"] == "ok"


def test_build_index_and_query_contract():
    tmp = Path(tempfile.mkdtemp(prefix="pattern_svc_test_"))
    try:
        rows = _seed_history(tmp)
        svc.HISTORICAL_DIR = tmp / "historical"
        svc.PATTERN_ROOT = tmp / "patterns"
        svc._reset_runtime_state_for_tests()

        build_req = (
            '((type . "PATTERN_SIMILARITY") (schema_version . 1) '
            '(action . "BUILD_INDEX") (symbol . "USDJPY") (timeframes . ("H1")))'
        )
        build_res = parse_sexp_alist(svc.handle_request_sexp(build_req))
        assert build_res["status"] == "ok"

        candles = _make_candles_from_rows(rows, n=120)
        query_req = {
            "type": "PATTERN_SIMILARITY",
            "schema_version": 1,
            "action": "QUERY",
            "symbol": "USDJPY",
            "timeframe": "H1",
            "k": 5,
            "candles": candles,
        }
        from aux_sexp import sexp_request

        query_res = parse_sexp_alist(svc.handle_request_sexp(sexp_request(query_req)))
        assert query_res["status"] == "ok"
        result = query_res["result"]
        assert "p_up" in result
        assert "p_down" in result
        assert "p_flat" in result
        assert isinstance(result.get("top_k", []), list)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_invalid_type_returns_error():
    req = '((type . "WRONG") (schema_version . 1) (action . "STATUS"))'
    parsed = parse_sexp_alist(svc.handle_request_sexp(req))
    assert parsed["status"] == "error"


def test_query_rejects_wrong_window_length():
    candles = _make_candles_from_rows(_make_h1_rows(), n=10)
    from aux_sexp import sexp_request

    req = sexp_request(
        {
            "type": "PATTERN_SIMILARITY",
            "schema_version": 1,
            "action": "QUERY",
            "symbol": "USDJPY",
            "timeframe": "H1",
            "candles": candles,
        }
    )
    parsed = parse_sexp_alist(svc.handle_request_sexp(req))
    assert parsed["status"] == "error"


def test_status_reports_siamese_backend_when_checkpoint_exists():
    tmp = Path(tempfile.mkdtemp(prefix="pattern_svc_test_model_"))
    try:
        model_dir = tmp / "patterns" / "models"
        model_dir.mkdir(parents=True, exist_ok=True)
        model_path = model_dir / "vector_siamese_v1.pt"

        import pattern_vector_dl as dl

        samples, labels = _make_training_samples()
        train_meta = dl.train_siamese_triplet(
            samples=samples,
            labels=labels,
            output_path=model_path,
            epochs=1,
            batch_size=16,
            max_triplets=96,
            device="cpu",
            seed=7,
        )
        assert train_meta["backend"].startswith("vector-siamese")

        svc.HISTORICAL_DIR = tmp / "historical"
        svc.PATTERN_ROOT = tmp / "patterns"
        svc._reset_runtime_state_for_tests()
        svc._reload_embedder_for_tests(model_path)

        req = '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "STATUS"))'
        res = parse_sexp_alist(svc.handle_request_sexp(req))
        assert res["status"] == "ok"
        assert str(res["model"]).startswith("vector-siamese")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    test_status_contract()
    test_build_index_and_query_contract()
    test_invalid_type_returns_error()
    test_query_rejects_wrong_window_length()
    test_status_reports_siamese_backend_when_checkpoint_exists()
    print("OK")
