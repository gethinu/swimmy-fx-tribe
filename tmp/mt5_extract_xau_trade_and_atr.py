import json
from datetime import datetime, timedelta, timezone
from pathlib import Path

import MetaTrader5 as mt5

ROOT = Path(r"\\wsl.localhost\Ubuntu\home\swimmy\swimmy")
OUT = ROOT / "data" / "reports" / "xau_mt5_forensic_20260226.json"

MAGIC_SET = {560081, 560083, 560070}
SYMBOL = "XAUUSD"

DEAL_REASON_MAP = {}
for k in dir(mt5):
    if k.startswith("DEAL_REASON_"):
        v = getattr(mt5, k)
        if isinstance(v, int):
            DEAL_REASON_MAP[v] = k

DEAL_ENTRY_MAP = {}
for k in dir(mt5):
    if k.startswith("DEAL_ENTRY_"):
        v = getattr(mt5, k)
        if isinstance(v, int):
            DEAL_ENTRY_MAP[v] = k

DEAL_TYPE_MAP = {}
for k in dir(mt5):
    if k.startswith("DEAL_TYPE_"):
        v = getattr(mt5, k)
        if isinstance(v, int):
            DEAL_TYPE_MAP[v] = k

if not mt5.initialize():
    raise SystemExit(f"mt5.initialize failed: {mt5.last_error()}")

try:
    end = datetime.now(timezone.utc)
    start = end - timedelta(days=30)

    deals = mt5.history_deals_get(start, end)
    if deals is None:
        raise RuntimeError(f"history_deals_get failed: {mt5.last_error()}")

    filtered = []
    for d in deals:
        if str(getattr(d, "symbol", "")).upper() != SYMBOL:
            continue
        magic = int(getattr(d, "magic", 0) or 0)
        if magic not in MAGIC_SET:
            continue
        filtered.append(d)

    by_pos = {}
    for d in filtered:
        pos = int(getattr(d, "position_id", 0) or 0)
        if pos <= 0:
            pos = int(getattr(d, "order", 0) or 0)
        by_pos.setdefault(pos, []).append(d)

    rows = []
    for pos, arr in by_pos.items():
        arr = sorted(arr, key=lambda x: int(getattr(x, "time", 0) or 0))
        entry = None
        exits = []
        for d in arr:
            entry_code = int(getattr(d, "entry", 0) or 0)
            if entry is None and entry_code == int(getattr(mt5, "DEAL_ENTRY_IN", 0)):
                entry = d
            if entry_code in {
                int(getattr(mt5, "DEAL_ENTRY_OUT", 1)),
                int(getattr(mt5, "DEAL_ENTRY_INOUT", 2)),
                int(getattr(mt5, "DEAL_ENTRY_OUT_BY", 3)),
            }:
                exits.append(d)

        if entry is None:
            entry = arr[0]

        exit_deal = exits[-1] if exits else None

        entry_time = int(getattr(entry, "time", 0) or 0)
        exit_time = int(getattr(exit_deal, "time", 0) or 0) if exit_deal else 0

        row = {
            "position_id": pos,
            "magic": int(getattr(entry, "magic", 0) or 0),
            "comment": str(getattr(entry, "comment", "")),
            "entry_time_utc": datetime.fromtimestamp(entry_time, tz=timezone.utc).isoformat() if entry_time else "",
            "entry_price": float(getattr(entry, "price", 0.0) or 0.0),
            "entry_volume": float(getattr(entry, "volume", 0.0) or 0.0),
            "entry_type": DEAL_TYPE_MAP.get(int(getattr(entry, "type", -1) or -1), int(getattr(entry, "type", -1) or -1)),
            "entry_sl": float(getattr(entry, "sl", 0.0) or 0.0),
            "entry_tp": float(getattr(entry, "tp", 0.0) or 0.0),
            "exit_time_utc": datetime.fromtimestamp(exit_time, tz=timezone.utc).isoformat() if exit_time else "",
            "exit_price": float(getattr(exit_deal, "price", 0.0) or 0.0) if exit_deal else None,
            "close_reason": DEAL_REASON_MAP.get(int(getattr(exit_deal, "reason", -1) or -1), None) if exit_deal else None,
            "exit_entry_code": DEAL_ENTRY_MAP.get(int(getattr(exit_deal, "entry", -1) or -1), None) if exit_deal else None,
            "profit": float(getattr(exit_deal, "profit", 0.0) or 0.0) if exit_deal else None,
            "swap": float(getattr(exit_deal, "swap", 0.0) or 0.0) if exit_deal else None,
            "commission": float(getattr(exit_deal, "commission", 0.0) or 0.0) if exit_deal else None,
            "is_closed": bool(exit_deal is not None),
            "deal_count": len(arr),
        }
        rows.append(row)

    rows.sort(key=lambda r: (r["exit_time_utc"] or r["entry_time_utc"]), reverse=True)

    # Open positions for current trial magics
    open_positions = mt5.positions_get(symbol=SYMBOL)
    open_rows = []
    if open_positions:
        for p in open_positions:
            magic = int(getattr(p, "magic", 0) or 0)
            if magic not in {560081, 560083}:
                continue
            t = int(getattr(p, "time", 0) or 0)
            open_rows.append({
                "ticket": int(getattr(p, "ticket", 0) or 0),
                "magic": magic,
                "comment": str(getattr(p, "comment", "")),
                "type": int(getattr(p, "type", -1) or -1),
                "volume": float(getattr(p, "volume", 0.0) or 0.0),
                "open_time_utc": datetime.fromtimestamp(t, tz=timezone.utc).isoformat() if t else "",
                "price_open": float(getattr(p, "price_open", 0.0) or 0.0),
                "sl": float(getattr(p, "sl", 0.0) or 0.0),
                "tp": float(getattr(p, "tp", 0.0) or 0.0),
                "profit": float(getattr(p, "profit", 0.0) or 0.0),
            })

    # ATR14 stats from latest M5 bars
    rates = mt5.copy_rates_from_pos(SYMBOL, mt5.TIMEFRAME_M5, 0, 260)
    atr_stats = {}
    if rates is not None and len(rates) >= 120:
        highs = [float(r["high"]) for r in rates]
        lows = [float(r["low"]) for r in rates]
        closes = [float(r["close"]) for r in rates]
        trs = []
        prev_close = None
        for h, l, c in zip(highs, lows, closes):
            if prev_close is None:
                tr = h - l
            else:
                tr = max(h - l, abs(h - prev_close), abs(l - prev_close))
            trs.append(tr)
            prev_close = c

        period = 14
        atr = []
        if len(trs) >= period:
            seed = sum(trs[:period]) / period
            atr.append(seed)
            prev = seed
            for tr in trs[period:]:
                nxt = ((prev * (period - 1)) + tr) / period
                atr.append(nxt)
                prev = nxt

        tail = atr[-100:] if len(atr) >= 100 else atr
        if tail:
            tail_sorted = sorted(tail)
            n = len(tail)
            median = tail_sorted[n // 2] if n % 2 == 1 else (tail_sorted[n // 2 - 1] + tail_sorted[n // 2]) / 2
            atr_stats = {
                "count": n,
                "mean": sum(tail) / n,
                "min": min(tail),
                "max": max(tail),
                "median": median,
                "last": tail[-1],
            }

    payload = {
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "symbol": SYMBOL,
        "window_days": 30,
        "magics": sorted(MAGIC_SET),
        "matched_position_count": len(rows),
        "recent_positions": rows[:30],
        "open_positions_current_trials": open_rows,
        "atr14_m5_last100": atr_stats,
    }

    OUT.parent.mkdir(parents=True, exist_ok=True)
    OUT.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    print(str(OUT))
finally:
    mt5.shutdown()
