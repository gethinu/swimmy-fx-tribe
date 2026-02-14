#!/usr/bin/env python3
"""Utilities for temperature-range prediction markets (weather).

This module is intentionally lightweight and testable (no network calls).
Higher-level components can use it to fetch a point forecast from an official
source and translate that into a probability for an integer temperature bucket.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class TemperatureRangeQuestion:
    city: str
    month: int
    day: int
    low_f: int
    high_f: int


_MONTHS = {
    "jan": 1,
    "january": 1,
    "feb": 2,
    "february": 2,
    "mar": 3,
    "march": 3,
    "apr": 4,
    "april": 4,
    "may": 5,
    "jun": 6,
    "june": 6,
    "jul": 7,
    "july": 7,
    "aug": 8,
    "august": 8,
    "sep": 9,
    "sept": 9,
    "september": 9,
    "oct": 10,
    "october": 10,
    "nov": 11,
    "november": 11,
    "dec": 12,
    "december": 12,
}


_TEMP_RANGE_Q_RE = re.compile(
    r"highest\s+temperature\s+in\s+(?P<city>.+?)\s+be\s+between\s+"
    r"(?P<low>\d+)\s*-\s*(?P<high>\d+)\s*°?\s*f\s+on\s+"
    r"(?P<month>[A-Za-z]+)\s+(?P<day>\d{1,2})",
    re.IGNORECASE,
)


def parse_temperature_range_question(question: str) -> Optional[TemperatureRangeQuestion]:
    text = str(question or "").strip()
    if not text:
        return None
    match = _TEMP_RANGE_Q_RE.search(text)
    if not match:
        return None

    city = " ".join(match.group("city").strip().split())
    month_raw = match.group("month").strip().lower()
    day_raw = match.group("day").strip()
    low_raw = match.group("low").strip()
    high_raw = match.group("high").strip()

    month = _MONTHS.get(month_raw)
    if month is None:
        return None
    try:
        day = int(day_raw)
        low_f = int(low_raw)
        high_f = int(high_raw)
    except ValueError:
        return None
    if not city:
        return None
    if day < 1 or day > 31:
        return None
    if low_f > high_f:
        low_f, high_f = high_f, low_f

    return TemperatureRangeQuestion(
        city=city,
        month=month,
        day=day,
        low_f=low_f,
        high_f=high_f,
    )


def _normal_cdf(*, x: float, mu: float, sigma: float) -> float:
    if sigma <= 0.0:
        return 1.0 if x >= mu else 0.0
    z = (x - mu) / (sigma * math.sqrt(2.0))
    return 0.5 * (1.0 + math.erf(z))


def normal_range_probability(*, mu: float, sigma: float, low_f: int, high_f: int) -> float:
    """Probability that an integer max-temp falls within [low_f, high_f] inclusive.

    Uses a Gaussian approximation with continuity correction.
    """
    lo = float(min(low_f, high_f)) - 0.5
    hi = float(max(low_f, high_f)) + 0.5
    p = _normal_cdf(x=hi, mu=float(mu), sigma=float(sigma)) - _normal_cdf(x=lo, mu=float(mu), sigma=float(sigma))
    if p < 0.0:
        return 0.0
    if p > 1.0:
        return 1.0
    return float(p)

