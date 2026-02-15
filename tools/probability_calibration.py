#!/usr/bin/env python3
"""Probability calibration utilities (lightweight, no external deps).

Currently supports isotonic regression via PAVA (pair-adjacent violators algorithm).

The resulting calibrator is serialized as a simple dict so it can be stored in JSON.
"""

from __future__ import annotations

import bisect
from datetime import datetime, timezone
from typing import Any, Dict, Iterable, List, Mapping, Sequence, Tuple


def _to_float(x: Any) -> float | None:
    try:
        return float(x)
    except (TypeError, ValueError):
        return None


def _clamp(x: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, x))


def _aggregate_by_x(samples: Iterable[Tuple[float, float]]) -> List[Tuple[float, float, float]]:
    """Return sorted unique x points as (x, y_mean, weight)."""
    buckets: Dict[float, Tuple[float, float]] = {}
    for x, y in samples:
        if x in buckets:
            sum_y, w = buckets[x]
            buckets[x] = (sum_y + float(y), w + 1.0)
        else:
            buckets[x] = (float(y), 1.0)
    out: List[Tuple[float, float, float]] = []
    for x in sorted(buckets.keys()):
        sum_y, w = buckets[x]
        out.append((float(x), float(sum_y) / float(w), float(w)))
    return out


def fit_isotonic_calibrator(*, samples: Sequence[Tuple[float, int | float]], created_at: str | None = None) -> Dict[str, Any]:
    """Fit a 1D isotonic calibrator mapping p->p_cal.

    samples: list of (p_pred, y) where y is 0/1 (or float in [0,1]).
    Returns a JSON-serializable dict:
      {"schema_version": 1, "method": "isotonic", "created_at": "...", "points": [{"x":..,"y":..}, ...]}
    """
    cleaned: List[Tuple[float, float]] = []
    for raw_p, raw_y in samples:
        p = _to_float(raw_p)
        y = _to_float(raw_y)
        if p is None or y is None:
            continue
        p = _clamp(float(p), 0.0, 1.0)
        y = _clamp(float(y), 0.0, 1.0)
        cleaned.append((p, y))

    if not cleaned:
        return {
            "schema_version": 1,
            "method": "isotonic",
            "created_at": created_at or datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
            "points": [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}],
        }

    # Aggregate duplicates and sort by x.
    agg = _aggregate_by_x(cleaned)

    # PAVA over block means.
    # Each block: x_min, x_max, sum_w, sum_y, mean
    blocks: List[Dict[str, float]] = []
    for x, y_mean, w in agg:
        blocks.append({"x_min": x, "x_max": x, "sum_w": float(w), "sum_y": float(y_mean) * float(w)})
        while len(blocks) >= 2:
            a = blocks[-2]
            b = blocks[-1]
            mean_a = a["sum_y"] / a["sum_w"] if a["sum_w"] else 0.0
            mean_b = b["sum_y"] / b["sum_w"] if b["sum_w"] else 0.0
            if mean_a <= mean_b:
                break
            merged = {
                "x_min": a["x_min"],
                "x_max": b["x_max"],
                "sum_w": a["sum_w"] + b["sum_w"],
                "sum_y": a["sum_y"] + b["sum_y"],
            }
            blocks[-2:] = [merged]

    # Convert blocks into (x, y) points. Use block boundaries so prediction can interpolate.
    points: List[Dict[str, float]] = []
    for block in blocks:
        mean = block["sum_y"] / block["sum_w"] if block["sum_w"] else 0.0
        x_min = float(block["x_min"])
        x_max = float(block["x_max"])
        mean = _clamp(float(mean), 0.0, 1.0)
        points.append({"x": x_min, "y": mean})
        if x_max != x_min:
            points.append({"x": x_max, "y": mean})
    points.sort(key=lambda p: float(p["x"]))

    # Deduplicate (shouldn't happen after aggregation, but keep it safe).
    dedup: List[Dict[str, float]] = []
    for pt in points:
        if not dedup or float(pt["x"]) > float(dedup[-1]["x"]):
            dedup.append({"x": float(pt["x"]), "y": float(pt["y"])})
        else:
            dedup[-1]["y"] = max(float(dedup[-1]["y"]), float(pt["y"]))

    # Ensure the mapping is defined on the full [0,1] domain and has >= 2 points.
    if not dedup:
        dedup = [{"x": 0.0, "y": 0.5}, {"x": 1.0, "y": 0.5}]
    if len(dedup) == 1:
        y0 = float(dedup[0]["y"])
        dedup = [{"x": 0.0, "y": y0}, {"x": 1.0, "y": y0}]
    else:
        if float(dedup[0]["x"]) > 0.0:
            dedup.insert(0, {"x": 0.0, "y": float(dedup[0]["y"])})
        if float(dedup[-1]["x"]) < 1.0:
            dedup.append({"x": 1.0, "y": float(dedup[-1]["y"])})

    return {
        "schema_version": 1,
        "method": "isotonic",
        "created_at": created_at or datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
        "points": dedup,
    }


def apply_probability_calibration(calibration: Mapping[str, Any] | None, p: float) -> float:
    """Apply a saved calibrator to a raw probability p."""
    if calibration is None:
        return float(p)
    method = str(calibration.get("method") or "").strip().lower()
    if method != "isotonic":
        return float(p)
    points = calibration.get("points")
    if not isinstance(points, list) or not points:
        return float(p)
    xs: List[float] = []
    ys: List[float] = []
    for row in points:
        if not isinstance(row, Mapping):
            continue
        x = _to_float(row.get("x"))
        y = _to_float(row.get("y"))
        if x is None or y is None:
            continue
        xs.append(float(x))
        ys.append(_clamp(float(y), 0.0, 1.0))
    if not xs:
        return float(p)
    # Ensure sorted by x.
    pairs = sorted(zip(xs, ys), key=lambda t: t[0])
    xs = [t[0] for t in pairs]
    ys = [t[1] for t in pairs]

    x = _clamp(float(p), 0.0, 1.0)
    if x <= xs[0]:
        return float(ys[0])
    if x >= xs[-1]:
        return float(ys[-1])
    idx = bisect.bisect_left(xs, x)
    if idx <= 0:
        return float(ys[0])
    x0, y0 = xs[idx - 1], ys[idx - 1]
    x1, y1 = xs[idx], ys[idx]
    if x1 <= x0:
        return float(y0)
    t = (x - x0) / (x1 - x0)
    return float(y0 + (y1 - y0) * t)


def validate_calibration(calibration: Mapping[str, Any]) -> None:
    method = str(calibration.get("method") or "").strip().lower()
    if method != "isotonic":
        raise ValueError(f"unsupported calibration method: {method}")
    points = calibration.get("points")
    if not isinstance(points, list) or len(points) < 2:
        raise ValueError("calibration.points must be a list with >= 2 items")
    last_x = -1.0
    last_y = -1.0
    for row in points:
        if not isinstance(row, Mapping):
            raise ValueError("calibration.points entries must be objects")
        x = _to_float(row.get("x"))
        y = _to_float(row.get("y"))
        if x is None or y is None:
            raise ValueError("calibration.points entries must include numeric x,y")
        x = float(x)
        y = float(y)
        if x < 0.0 or x > 1.0:
            raise ValueError("calibration.points.x must be in [0,1]")
        if y < 0.0 or y > 1.0:
            raise ValueError("calibration.points.y must be in [0,1]")
        if x < last_x:
            raise ValueError("calibration.points must be sorted by x")
        if y < last_y:
            raise ValueError("calibration.points must be non-decreasing in y")
        last_x = x
        last_y = y
