import sys
from pathlib import Path
import os
import json
from tempfile import TemporaryDirectory


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))
sys.path.insert(0, str(BASE_DIR / "tools"))

# Keep contract tests deterministic even if optional DL deps are installed.
os.environ.setdefault("SWIMMY_PATTERN_EMBED_BACKEND", "pixel")

from sexp_utils import parse_sexp_alist
from aux_sexp import sexp_request
from sexp_serialize import sexp_serialize

import pattern_similarity_service


def make_trend_candles(*, n: int, start_ts: int, step_sec: int, start_price: float, delta: float):
    candles = []
    price = start_price
    for i in range(n):
        ts = start_ts + i * step_sec
        o = price
        c = price + delta
        hi = max(o, c) + abs(delta) * 0.2
        lo = min(o, c) - abs(delta) * 0.2
        candles.append(
            {
                "timestamp": ts,
                "open": o,
                "high": hi,
                "low": lo,
                "close": c,
                "volume": 10,
            }
        )
        price = c
    return candles


def make_flat_candles(*, n: int, start_ts: int, step_sec: int, price: float, volume: int = 10):
    candles = []
    for i in range(n):
        ts = start_ts + i * step_sec
        candles.append(
            {
                "timestamp": ts,
                "open": price,
                "high": price,
                "low": price,
                "close": price,
                "volume": volume,
            }
        )
    return candles


def main():
    pattern_similarity_service._reset_state_for_tests()

    status = pattern_similarity_service.handle_request_sexp(
        '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "STATUS"))'
    )
    parsed = parse_sexp_alist(status)
    assert parsed["type"] == "PATTERN_SIMILARITY_RESULT"
    assert parsed["schema_version"] == 1
    assert parsed["status"] in ("ok", "error")
    assert "ensemble_default_vector_weight" in parsed
    assert "ensemble_weight_file" in parsed
    assert "query_metrics" in parsed
    assert "index_loader" in parsed
    assert "backend_loader" in parsed
    loader0 = parsed["index_loader"]
    backend_loader0 = parsed["backend_loader"]
    assert str(loader0["status"]) in ("idle", "running", "completed", "failed")
    assert int(loader0["started_at"]) >= 0
    assert int(loader0["finished_at"]) >= 0
    assert int(loader0["loaded"]) >= 0
    assert "error" in loader0
    assert str(backend_loader0["status"]) in ("idle", "running", "completed", "failed")
    assert int(backend_loader0["started_at"]) >= 0
    assert int(backend_loader0["finished_at"]) >= 0
    assert str(backend_loader0["clip_ready"]).lower() in ("true", "false")
    assert str(backend_loader0["vector_ready"]).lower() in ("true", "false")
    assert "error" in backend_loader0
    qm0 = parsed["query_metrics"]
    assert int(qm0["count"]) == 0
    assert int(qm0["window_count"]) == 0

    # No index yet -> QUERY should error (fail-open is on the Lisp side).
    query = pattern_similarity_service.handle_request_sexp(
        '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "QUERY") (symbol . "USDJPY") (timeframe . "H1") (candles . ()))'
    )
    parsed_q = parse_sexp_alist(query)
    assert parsed_q["status"] == "error"

    bad = pattern_similarity_service.handle_request_sexp('((schema_version . 1))')
    parsed_bad = parse_sexp_alist(bad)
    assert parsed_bad["status"] == "error"

    if pattern_similarity_service.np is None:
        print("SKIP: numpy not available")
        return

    # Build a tiny in-memory index and ensure QUERY returns probabilities.
    up = make_trend_candles(n=120, start_ts=1709230000, step_sec=3600, start_price=100.0, delta=0.1)
    down = make_trend_candles(n=120, start_ts=1709230000, step_sec=3600, start_price=200.0, delta=-0.1)
    pattern_similarity_service._reset_state_for_tests()
    pattern_similarity_service._build_index_from_inline_samples(
        symbol="USDJPY",
        timeframe="H1",
        samples=[
            {"id": "H1:USDJPY:1709230000", "label": "UP", "candles": up},
            {"id": "H1:USDJPY:1709230000:DOWN", "label": "DOWN", "candles": down},
        ],
    )

    query2_msg = sexp_request(
        {
            "type": "PATTERN_SIMILARITY",
            "schema_version": 1,
            "action": "QUERY",
            "symbol": "USDJPY",
            "timeframe": "H1",
            "intended_direction": "BUY",
            "k": 2,
            "candles": up,
        }
    )
    query2 = pattern_similarity_service.handle_request_sexp(query2_msg)
    parsed_q2 = parse_sexp_alist(query2)
    assert parsed_q2["status"] == "ok"
    result = parsed_q2["result"]
    assert 0.0 <= result["p_up"] <= 1.0
    assert 0.0 <= result["p_down"] <= 1.0
    assert 0.0 <= result["p_flat"] <= 1.0
    assert result["p_up"] > result["p_down"]
    assert len(result["top_k"]) == 2
    assert result["policy_mode"] in ("shadow", "soft-enforce", "full-enforce")
    assert result["decision_action"] in ("follow", "fade", "no-trade")
    assert isinstance(result["distortion_passed"], bool)
    assert "distortion_score" in result
    assert "backend_used" in result
    assert "vector_weight_applied" in result
    assert "weight_source" in result

    # Lisp shorthand alist entry form must be accepted for list-valued keys.
    candles_expr = sexp_serialize(up)
    candles_items = candles_expr[1:-1].strip()
    query2_shorthand_msg = (
        '((type . "PATTERN_SIMILARITY")'
        ' (schema_version . 1)'
        ' (action . "QUERY")'
        ' (symbol . "USDJPY")'
        ' (timeframe . "H1")'
        ' (intended_direction . "BUY")'
        ' (k . 2)'
        f" (candles {candles_items}))"
    )
    query2_shorthand = pattern_similarity_service.handle_request_sexp(query2_shorthand_msg)
    parsed_q2_shorthand = parse_sexp_alist(query2_shorthand)
    assert parsed_q2_shorthand["status"] == "ok"
    assert len(parsed_q2_shorthand["result"]["top_k"]) == 2

    # Flat/low-distortion query should yield no-trade decision in shadow policy.
    flat = make_flat_candles(n=120, start_ts=1709230000, step_sec=3600, price=123.45, volume=10)
    flat_query_msg = sexp_request(
        {
            "type": "PATTERN_SIMILARITY",
            "schema_version": 1,
            "action": "QUERY",
            "symbol": "USDJPY",
            "timeframe": "H1",
            "intended_direction": "BUY",
            "k": 2,
            "candles": flat,
        }
    )
    flat_query = pattern_similarity_service.handle_request_sexp(flat_query_msg)
    parsed_flat = parse_sexp_alist(flat_query)
    assert parsed_flat["status"] == "ok"
    flat_result = parsed_flat["result"]
    assert flat_result["decision_action"] == "no-trade"
    assert flat_result["distortion_passed"] is False
    assert flat_result["enforce_no_trade"] is False

    # Runtime ensemble weight file should override backend blend.
    with TemporaryDirectory() as tmpdir:
        weight_path = Path(tmpdir) / "ensemble_weight.json"
        original_weight_file = pattern_similarity_service.ENSEMBLE_WEIGHT_FILE
        original_resolve = pattern_similarity_service._resolve_query_indices
        original_query_single = pattern_similarity_service._query_single_index
        try:
            pattern_similarity_service.ENSEMBLE_WEIGHT_FILE = weight_path
            clip_idx = pattern_similarity_service.PatternIndex(
                symbol="USDJPY",
                timeframe="H1",
                model="clip-vit-b32",
                ids=["x"],
                labels=["UP"],
                embeddings=None,
                last_built=0,
                ann_index=None,
            )
            vec_idx = pattern_similarity_service.PatternIndex(
                symbol="USDJPY",
                timeframe="H1",
                model="vector-siamese-v1",
                ids=["y"],
                labels=["DOWN"],
                embeddings=None,
                last_built=0,
                ann_index=None,
            )

            def fake_resolve(_symbol, _tf):
                return [clip_idx, vec_idx]

            def fake_query_single(idx, _candles, _k):
                if idx.model == "clip-vit-b32":
                    return {"p_up": 0.9, "p_down": 0.1, "p_flat": 0.0}, [{"distance": 0.1, "label": "UP", "id": "x"}]
                return {"p_up": 0.1, "p_down": 0.9, "p_flat": 0.0}, [{"distance": 0.1, "label": "DOWN", "id": "y"}]

            pattern_similarity_service._resolve_query_indices = fake_resolve
            pattern_similarity_service._query_single_index = fake_query_single

            weight_path.write_text(json.dumps({"vector_weight": 0.0}), encoding="utf-8")
            pattern_similarity_service._weight_cache_mtime = None
            pattern_similarity_service._weight_cache_value = None
            q_clip = parse_sexp_alist(pattern_similarity_service.handle_request_sexp(query2_msg))["result"]
            probs_clip = {str(item["backend"]): float(item["weight"]) for item in q_clip["backend_probs"]}
            assert probs_clip.get("clip-vit-b32") == 1.0
            assert probs_clip.get("vector-siamese-v1") in (None, 0.0)
            assert q_clip["p_up"] > q_clip["p_down"]
            assert float(q_clip["vector_weight_applied"]) == 0.0
            assert str(q_clip["weight_source"]) == "file_global"

            weight_path.write_text(json.dumps({"vector_weight": 1.0}), encoding="utf-8")
            pattern_similarity_service._weight_cache_mtime = None
            pattern_similarity_service._weight_cache_value = None
            q_vec = parse_sexp_alist(pattern_similarity_service.handle_request_sexp(query2_msg))["result"]
            probs_vec = {str(item["backend"]): float(item["weight"]) for item in q_vec["backend_probs"]}
            assert probs_vec.get("clip-vit-b32") in (None, 0.0)
            assert probs_vec.get("vector-siamese-v1") == 1.0
            assert q_vec["p_up"] < q_vec["p_down"]
            assert float(q_vec["vector_weight_applied"]) == 1.0
            assert str(q_vec["weight_source"]) == "file_global"

            # symbol+timeframe override should take precedence over global weight.
            weight_path.write_text(
                json.dumps(
                    {
                        "vector_weight": 1.0,
                        "symbol_timeframe_weights": {"USDJPY:H1": 0.0},
                    }
                ),
                encoding="utf-8",
            )
            pattern_similarity_service._weight_cache_mtime = None
            pattern_similarity_service._weight_cache_value = None
            q_st = parse_sexp_alist(pattern_similarity_service.handle_request_sexp(query2_msg))["result"]
            probs_st = {str(item["backend"]): float(item["weight"]) for item in q_st["backend_probs"]}
            assert probs_st.get("clip-vit-b32") == 1.0
            assert probs_st.get("vector-siamese-v1") in (None, 0.0)
            assert float(q_st["vector_weight_applied"]) == 0.0
            assert str(q_st["weight_source"]) == "file_symbol_timeframe"

            # same symbol with unmatched timeframe should fallback to global value.
            query_h4_msg = sexp_request(
                {
                    "type": "PATTERN_SIMILARITY",
                    "schema_version": 1,
                    "action": "QUERY",
                    "symbol": "USDJPY",
                    "timeframe": "H4",
                    "intended_direction": "BUY",
                    "k": 2,
                    "candles": up,
                }
            )
            q_h4 = parse_sexp_alist(pattern_similarity_service.handle_request_sexp(query_h4_msg))["result"]
            assert float(q_h4["vector_weight_applied"]) == 1.0
            assert str(q_h4["weight_source"]) == "file_global"

            # If preferred backend is unavailable, QUERY should fail-soft to available backend.
            called_models = []

            def fake_query_single_degraded(idx, _candles, _k):
                called_models.append(str(idx.model))
                if idx.model == "clip-vit-b32":
                    raise RuntimeError("clip warmup")
                return {"p_up": 0.2, "p_down": 0.8, "p_flat": 0.0}, [{"distance": 0.1, "label": "DOWN", "id": "y"}]

            pattern_similarity_service._query_single_index = fake_query_single_degraded
            weight_path.write_text(json.dumps({"vector_weight": 0.0}), encoding="utf-8")
            pattern_similarity_service._weight_cache_mtime = None
            pattern_similarity_service._weight_cache_value = None

            degraded_resp = parse_sexp_alist(pattern_similarity_service.handle_request_sexp(query2_msg))
            assert degraded_resp["status"] == "ok"
            degraded_result = degraded_resp["result"]
            assert degraded_result["backend_used"] in ("vector-siamese-v1", "mixed")
            assert any(str(item["backend"]) == "vector-siamese-v1" for item in degraded_result["backend_probs"])
            assert "clip-vit-b32" in called_models
            assert "vector-siamese-v1" in called_models
        finally:
            pattern_similarity_service.ENSEMBLE_WEIGHT_FILE = original_weight_file
            pattern_similarity_service._resolve_query_indices = original_resolve
            pattern_similarity_service._query_single_index = original_query_single

    status_after = parse_sexp_alist(
        pattern_similarity_service.handle_request_sexp(
            '((type . "PATTERN_SIMILARITY") (schema_version . 1) (action . "STATUS"))'
        )
    )
    qm = status_after["query_metrics"]
    loader = status_after["index_loader"]
    backend_loader = status_after["backend_loader"]
    assert int(qm["count"]) >= 2
    assert int(qm["ok_count"]) >= 2
    assert int(qm["error_count"]) >= 0
    assert int(qm["window_count"]) >= 1
    assert int(qm["window_size"]) >= int(qm["window_count"])
    assert float(qm["p95_ms"]) >= float(qm["p50_ms"]) >= 0.0
    assert float(qm["max_ms"]) >= float(qm["p95_ms"])
    assert str(loader["status"]) in ("idle", "running", "completed", "failed")
    assert str(backend_loader["status"]) in ("idle", "running", "completed", "failed")


if __name__ == "__main__":
    main()
    print("OK")
