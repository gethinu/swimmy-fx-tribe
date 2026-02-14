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


@dataclass(frozen=True)
class TemperatureBucketQuestion:
    """A single integer-temperature bucket for a max-temperature market.

    low/high are inclusive integer bounds in the given unit.
    If low is None => (-inf, high]
    If high is None => [low, +inf)
    """

    city: str
    month: int
    day: int
    unit: str  # "F" or "C"
    low: Optional[int]
    high: Optional[int]


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


_TEMP_BUCKET_BETWEEN_RE = re.compile(
    r"highest\s+temperature\s+in\s+(?P<city>.+?)\s+be\s+between\s+"
    r"(?P<low>-?\d+)\s*-\s*(?P<high>-?\d+)\s*°?\s*(?P<unit>[fc])\s+on\s+"
    r"(?P<month>[A-Za-z]+)\s+(?P<day>\d{1,2})",
    re.IGNORECASE,
)

_TEMP_BUCKET_OR_BELOW_RE = re.compile(
    r"highest\s+temperature\s+in\s+(?P<city>.+?)\s+be\s+"
    r"(?P<high>-?\d+)\s*°?\s*(?P<unit>[fc])\s+or\s+(?:below|lower)\s+on\s+"
    r"(?P<month>[A-Za-z]+)\s+(?P<day>\d{1,2})",
    re.IGNORECASE,
)

_TEMP_BUCKET_OR_HIGHER_RE = re.compile(
    r"highest\s+temperature\s+in\s+(?P<city>.+?)\s+be\s+"
    r"(?P<low>-?\d+)\s*°?\s*(?P<unit>[fc])\s+or\s+(?:higher|above|more)\s+on\s+"
    r"(?P<month>[A-Za-z]+)\s+(?P<day>\d{1,2})",
    re.IGNORECASE,
)

_TEMP_BUCKET_EXACT_RE = re.compile(
    r"highest\s+temperature\s+in\s+(?P<city>.+?)\s+be\s+"
    r"(?P<temp>-?\d+)\s*°?\s*(?P<unit>[fc])\s+on\s+"
    r"(?P<month>[A-Za-z]+)\s+(?P<day>\d{1,2})",
    re.IGNORECASE,
)


def parse_temperature_bucket_question(question: str) -> Optional[TemperatureBucketQuestion]:
    text = str(question or "").strip()
    if not text:
        return None

    match = _TEMP_BUCKET_BETWEEN_RE.search(text)
    bucket_type = "between"
    if not match:
        match = _TEMP_BUCKET_OR_BELOW_RE.search(text)
        bucket_type = "below"
    if not match:
        match = _TEMP_BUCKET_OR_HIGHER_RE.search(text)
        bucket_type = "higher"
    if not match:
        match = _TEMP_BUCKET_EXACT_RE.search(text)
        bucket_type = "exact"
    if not match:
        return None

    city = " ".join(match.group("city").strip().split())
    month_raw = match.group("month").strip().lower()
    day_raw = match.group("day").strip()
    unit = match.group("unit").strip().upper()

    month = _MONTHS.get(month_raw)
    if month is None:
        return None
    try:
        day = int(day_raw)
    except ValueError:
        return None
    if not city:
        return None
    if day < 1 or day > 31:
        return None
    if unit not in {"F", "C"}:
        return None

    low: Optional[int] = None
    high: Optional[int] = None
    try:
        if bucket_type == "between":
            low = int(match.group("low").strip())
            high = int(match.group("high").strip())
            if low > high:
                low, high = high, low
        elif bucket_type == "below":
            high = int(match.group("high").strip())
        elif bucket_type == "higher":
            low = int(match.group("low").strip())
        else:
            value = int(match.group("temp").strip())
            low = value
            high = value
    except ValueError:
        return None

    return TemperatureBucketQuestion(
        city=city,
        month=month,
        day=day,
        unit=unit,
        low=low,
        high=high,
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


def normal_bucket_probability(*, mu: float, sigma: float, low: Optional[int], high: Optional[int]) -> float:
    """Probability that an integer temperature falls within [low, high] inclusive.

    Bounds may be unbounded on either side (low/high is None).
    Uses a Gaussian approximation with continuity correction.
    """
    if low is None:
        lo = float("-inf")
    else:
        lo = float(low) - 0.5
    if high is None:
        hi = float("inf")
    else:
        hi = float(high) + 0.5

    if lo == float("-inf"):
        p_lo = 0.0
    else:
        p_lo = _normal_cdf(x=lo, mu=float(mu), sigma=float(sigma))
    if hi == float("inf"):
        p_hi = 1.0
    else:
        p_hi = _normal_cdf(x=hi, mu=float(mu), sigma=float(sigma))

    p = p_hi - p_lo
    if p < 0.0:
        return 0.0
    if p > 1.0:
        return 1.0
    return float(p)


def normal_range_probability(*, mu: float, sigma: float, low_f: int, high_f: int) -> float:
    """Probability that an integer max-temp falls within [low_f, high_f] inclusive.

    Uses a Gaussian approximation with continuity correction.
    """
    lo = int(min(low_f, high_f))
    hi = int(max(low_f, high_f))
    return normal_bucket_probability(mu=float(mu), sigma=float(sigma), low=lo, high=hi)
