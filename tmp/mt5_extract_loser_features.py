import csv
import json
from datetime import datetime, timedelta, timezone
from pathlib import Path

import MetaTrader5 as mt5

ROOT = Path(r"\\wsl.localhost\Ubuntu\home\swimmy\swimmy")
FORENSIC = ROOT / "data" / "reports" / "xau_mt5_forensic_20260226.json"
OUT_CSV = ROOT / "data" / "reports" / "xau_loser_entry_features_20260226.csv"
OUT_JSON = ROOT / "data" / "reports" / "xau_loser_entry_features_20260226.json"

SYMBOL = "XAUUSD"
EMA_FAST = 30
EMA_SLOW = 140
ATR_PERIOD = 14


def ema_series(values, period):
    alpha = 2.0 / (period + 1)
    out = []
    ema = values[0]
    out.append(ema)
    for v in values[1:]:
        ema = ema + alpha * (v - ema)
        out.append(ema)
    return out


def atr_series(highs, lows, closes, period):
    tr = []
    prev = None
    for h, l, c in zip(highs, lows, closes):
        if prev is None:
            t = h - l
        else:
            t = max(h - l, abs(h - prev), abs(l - prev))
        tr.append(t)
        prev = c
    if len(tr) < period:
        return []
    seed = sum(tr[:period]) / period
    out = [seed]
    prev_atr = seed
    for t in tr[period:]:
        nxt = ((prev_atr * (period - 1)) + t) / period
        out.append(nxt)
        prev_atr = nxt
    # align to closes length by padding front
    pad = [None] * (period - 1)
    return pad + out


def parse_utc(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00")).astimezone(timezone.utc)


def infer_side(position_id, start, end):
    deals = mt5.history_deals_get(start - timedelta(hours=6), end + timedelta(hours=6), group="*", position=position_id)
    if deals is None:
        return "UNKNOWN"
    for d in sorted(deals, key=lambda x: int(getattr(x, "time", 0) or 0)):
        entry = int(getattr(d, "entry", 0) or 0)
        # DEAL_ENTRY_IN == 0
        if entry == int(getattr(mt5, "DEAL_ENTRY_IN", 0)):
            t = int(getattr(d, "type", -1) or -1)
            if t == int(getattr(mt5, "DEAL_TYPE_BUY", 0)):
                return "BUY"
            if t == int(getattr(mt5, "DEAL_TYPE_SELL", 1)):
                return "SELL"
    return "UNKNOWN"


def nearest_spread_points(symbol, at_dt):
    info = mt5.symbol_info(symbol)
    if info is None:
        return None
    point = float(getattr(info, "point", 0.0) or 0.0)
    if point <= 0:
        return None
    ticks = mt5.copy_ticks_range(symbol, at_dt - timedelta(seconds=5), at_dt + timedelta(seconds=5), mt5.COPY_TICKS_ALL)
    if ticks is None or len(ticks) == 0:
        return None
    at_ts = at_dt.timestamp()
    best = None
    best_dist = 1e18
    for t in ticks:
        ts = float(t["time"]) if "time" in t.dtype.names else float(t["time_msc"]) / 1000.0
        dist = abs(ts - at_ts)
        if dist < best_dist:
            best_dist = dist
            best = t
    if best is None:
        return None
    ask = float(best["ask"])
    bid = float(best["bid"])
    if ask <= 0 or bid <= 0:
        return None
    return (ask - bid) / point


if not FORENSIC.exists():
    raise SystemExit(f"missing {FORENSIC}")

payload = json.loads(FORENSIC.read_text(encoding="utf-8"))
rows = payload.get("recent_positions", [])

losers = [r for r in rows if r.get("is_closed") and (float(r.get("profit") or 0.0) < 0.0)]
# latest 10 losses by entry time desc
losers = sorted(losers, key=lambda r: r.get("entry_time_utc", ""), reverse=True)[:10]

if not mt5.initialize():
    raise SystemExit(f"mt5.initialize failed: {mt5.last_error()}")

try:
    out = []
    for r in losers:
        entry_time = parse_utc(r["entry_time_utc"])
        et_epoch = int(entry_time.timestamp())

        side = infer_side(int(r.get("position_id", 0)), entry_time, entry_time)

        # pull enough bars up to entry
        rates = mt5.copy_rates_from(SYMBOL, mt5.TIMEFRAME_M5, entry_time + timedelta(minutes=5), 400)
        if rates is None or len(rates) < EMA_SLOW + 5:
            continue

        times = [int(x["time"]) for x in rates]
        closes = [float(x["close"]) for x in rates]
        highs = [float(x["high"]) for x in rates]
        lows = [float(x["low"]) for x in rates]

        # index of latest bar at/before entry
        idx = None
        for i, t in enumerate(times):
            if t <= et_epoch:
                idx = i
            else:
                break
        if idx is None or idx < EMA_SLOW:
            continue

        closes_u = closes[: idx + 1]
        highs_u = highs[: idx + 1]
        lows_u = lows[: idx + 1]

        ema_f = ema_series(closes_u, EMA_FAST)[-1]
        ema_s = ema_series(closes_u, EMA_SLOW)[-1]
        atr_vals = atr_series(highs_u, lows_u, closes_u, ATR_PERIOD)
        atr = atr_vals[-1] if atr_vals else None
        if atr is None or atr <= 0:
            continue

        ratio = abs(ema_f - ema_s) / atr
        spread_points = nearest_spread_points(SYMBOL, entry_time)

        out.append(
            {
                "position_id": int(r.get("position_id", 0)),
                "entry_time_utc": r.get("entry_time_utc"),
                "side": side,
                "last_close": closes_u[-1],
                "ema_fast_30": ema_f,
                "ema_slow_140": ema_s,
                "atr14": atr,
                "ema_gap_over_atr": ratio,
                "spread_points_entry": spread_points,
                "profit": float(r.get("profit") or 0.0),
                "close_reason": r.get("close_reason"),
            }
        )

    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    with OUT_CSV.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(
            f,
            fieldnames=[
                "position_id",
                "entry_time_utc",
                "side",
                "last_close",
                "ema_fast_30",
                "ema_slow_140",
                "atr14",
                "ema_gap_over_atr",
                "spread_points_entry",
                "profit",
                "close_reason",
            ],
        )
        w.writeheader()
        for row in out:
            w.writerow(row)

    OUT_JSON.write_text(json.dumps({"rows": out}, ensure_ascii=False, indent=2), encoding="utf-8")
    print(str(OUT_CSV))
    print(str(OUT_JSON))
finally:
    mt5.shutdown()
