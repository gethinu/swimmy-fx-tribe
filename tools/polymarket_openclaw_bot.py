#!/usr/bin/env python3
"""OpenClaw signal + Polymarket opportunity planner (dry-run by default)."""

from __future__ import annotations

import argparse
import json
import subprocess
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Iterable, List, Mapping, MutableMapping, Optional, Sequence, Set
from urllib.parse import urlencode
from urllib.request import Request, urlopen


def _to_float(value: Any) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def _is_gte_with_tol(value: float, threshold: float, tol: float = 1e-9) -> bool:
    return value >= (threshold - tol)


def _clamp_probability(value: float) -> float:
    if value < 0.0:
        return 0.0
    if value > 1.0:
        return 1.0
    return value


def _parse_maybe_json_array(value: Any) -> List[Any]:
    if isinstance(value, list):
        return value
    if isinstance(value, str):
        text = value.strip()
        if not text:
            return []
        try:
            parsed = json.loads(text)
            return parsed if isinstance(parsed, list) else []
        except json.JSONDecodeError:
            return []
    return []


def _normalize_outcome(value: Any) -> str:
    text = str(value or "").strip().upper()
    if text in {"YES", "Y", "TRUE", "1"}:
        return "YES"
    if text in {"NO", "N", "FALSE", "0"}:
        return "NO"
    return ""


@dataclass(frozen=True)
class BotConfig:
    bankroll_usd: float = 1000.0
    max_daily_loss_pct: float = 0.5
    max_trade_risk_pct: float = 0.25
    max_positions: int = 4
    max_open_positions: int = 12
    max_daily_entries: int = 12
    max_daily_loss_streak: int = 0
    max_daily_realized_loss_usd: float = 0.0
    min_price_sum: float = 0.90
    max_price_sum: float = 1.10
    min_market_price: float = 0.03
    max_market_price: float = 0.97
    min_liquidity_usd: float = 0.0
    min_volume_usd: float = 0.0
    min_edge: float = 0.02
    min_confidence: float = 0.0
    min_stake_usd: float = 1.0
    kelly_scale: float = 0.5
    fee_bps_per_side: float = 0.0
    slippage_bps_per_side: float = 0.0
    enable_contrarian_fade: bool = False
    contrarian_favorite_threshold: float = 0.60


@dataclass(frozen=True)
class OpenClawSignal:
    market_id: str
    p_yes: float
    confidence: float = 1.0


@dataclass(frozen=True)
class MarketSnapshot:
    market_id: str
    question: str
    yes_token_id: str
    no_token_id: str
    yes_price: float
    no_price: float
    liquidity_usd: float = 0.0
    volume_usd: float = 0.0


@dataclass
class TradePlanEntry:
    market_id: str
    question: str
    side: str
    token_id: str
    model_prob: float
    entry_price: float
    edge: float
    confidence: float
    kelly_fraction: float
    stake_usd: float
    expected_value_usd: float


@dataclass
class TradePlan:
    generated_at: str
    entries: List[TradePlanEntry]
    total_stake_usd: float
    daily_risk_budget_usd: float
    remaining_risk_budget_usd: float
    quality_filtered_markets: int = 0


def load_openclaw_signals(path: Path) -> Dict[str, OpenClawSignal]:
    with path.open("r", encoding="utf-8") as handle:
        return _parse_openclaw_signal_records(line for line in handle if line.strip())


def _parse_openclaw_signal_records(records: Iterable[str]) -> Dict[str, OpenClawSignal]:
    signals: Dict[str, OpenClawSignal] = {}
    for raw in records:
        try:
            payload = json.loads(raw)
        except json.JSONDecodeError:
            continue
        if not isinstance(payload, Mapping):
            continue
        market_id = str(
            payload.get("market_id")
            or payload.get("market")
            or payload.get("id")
            or ""
        ).strip()
        if not market_id:
            continue
        p_yes_value = payload.get("p_yes", payload.get("prob_yes", payload.get("probability_yes")))
        p_yes = _to_float(p_yes_value)
        if p_yes is None:
            continue
        confidence = _to_float(payload.get("confidence", 1.0))
        if confidence is None:
            confidence = 1.0
        signals[market_id] = OpenClawSignal(
            market_id=market_id,
            p_yes=_clamp_probability(p_yes),
            confidence=_clamp_probability(confidence),
        )
    return signals


def load_openclaw_signals_from_command(command: str) -> Dict[str, OpenClawSignal]:
    proc = subprocess.run(command, shell=True, capture_output=True, text=True, check=True)
    stdout = proc.stdout.strip()
    if not stdout:
        return {}

    try:
        payload = json.loads(stdout)
    except json.JSONDecodeError:
        payload = None
    if isinstance(payload, list):
        records = [json.dumps(item, ensure_ascii=False) for item in payload if isinstance(item, Mapping)]
        return _parse_openclaw_signal_records(records)

    lines = [line.strip() for line in stdout.splitlines() if line.strip()]
    for line in lines:
        if not line.startswith("["):
            continue
        try:
            payload = json.loads(line)
        except json.JSONDecodeError:
            continue
        if not isinstance(payload, list):
            continue
        records = [json.dumps(item, ensure_ascii=False) for item in payload if isinstance(item, Mapping)]
        return _parse_openclaw_signal_records(records)

    return _parse_openclaw_signal_records(lines)


def _extract_token_id(token: Mapping[str, Any]) -> str:
    return str(token.get("token_id") or token.get("tokenId") or token.get("id") or "").strip()


def parse_gamma_market(payload: Mapping[str, Any]) -> Optional[MarketSnapshot]:
    market_id = str(payload.get("id") or payload.get("conditionId") or "").strip()
    if not market_id:
        return None

    question = str(payload.get("question") or payload.get("title") or "").strip()
    yes_token_id = ""
    no_token_id = ""
    yes_price: Optional[float] = None
    no_price: Optional[float] = None

    tokens = payload.get("tokens")
    if isinstance(tokens, list):
        for token in tokens:
            if not isinstance(token, Mapping):
                continue
            outcome = str(token.get("outcome") or token.get("name") or "").strip().lower()
            price = _to_float(token.get("price", token.get("bestAsk")))
            token_id = _extract_token_id(token)
            if outcome == "yes":
                yes_price = price
                yes_token_id = token_id
            elif outcome == "no":
                no_price = price
                no_token_id = token_id

    if yes_price is None or no_price is None:
        outcomes = [str(item).strip().lower() for item in _parse_maybe_json_array(payload.get("outcomes"))]
        prices_raw = _parse_maybe_json_array(payload.get("outcomePrices"))
        token_ids = [str(item).strip() for item in _parse_maybe_json_array(payload.get("clobTokenIds"))]
        if len(outcomes) == len(prices_raw) and outcomes:
            for idx, outcome in enumerate(outcomes):
                price = _to_float(prices_raw[idx])
                token_id = token_ids[idx] if idx < len(token_ids) else ""
                if outcome == "yes":
                    yes_price = price
                    yes_token_id = token_id
                elif outcome == "no":
                    no_price = price
                    no_token_id = token_id

    if yes_price is None or no_price is None:
        return None
    if yes_price <= 0.0 or yes_price >= 1.0:
        return None
    if no_price <= 0.0 or no_price >= 1.0:
        return None

    liquidity = _to_float(payload.get("liquidity"))
    volume = _to_float(payload.get("volume"))

    return MarketSnapshot(
        market_id=market_id,
        question=question,
        yes_token_id=yes_token_id or f"{market_id}:YES",
        no_token_id=no_token_id or f"{market_id}:NO",
        yes_price=float(yes_price),
        no_price=float(no_price),
        liquidity_usd=float(liquidity) if liquidity is not None else 0.0,
        volume_usd=float(volume) if volume is not None else 0.0,
    )


def parse_gamma_markets(markets_payload: Sequence[Mapping[str, Any]]) -> List[MarketSnapshot]:
    parsed: List[MarketSnapshot] = []
    for raw in markets_payload:
        snap = parse_gamma_market(raw)
        if snap is not None:
            parsed.append(snap)
    return parsed


def filter_markets_by_keywords(markets: Sequence[MarketSnapshot], keywords: Sequence[str]) -> List[MarketSnapshot]:
    if not keywords:
        return list(markets)
    normalized = [keyword.strip().lower() for keyword in keywords if keyword and keyword.strip()]
    if not normalized:
        return list(markets)
    filtered: List[MarketSnapshot] = []
    for market in markets:
        question = market.question.lower()
        if any(keyword in question for keyword in normalized):
            filtered.append(market)
    return filtered


def merge_runtime_options(
    *,
    cli: Mapping[str, Any],
    defaults: Mapping[str, Any],
    config: Mapping[str, Any],
) -> Dict[str, Any]:
    merged: Dict[str, Any] = {}
    for key, default_value in defaults.items():
        cli_value = cli.get(key, default_value)
        has_cli_override = False
        if isinstance(default_value, list):
            has_cli_override = bool(cli_value)
        else:
            has_cli_override = cli_value != default_value

        if has_cli_override:
            merged[key] = cli_value
            continue

        if key == "question_keyword":
            cfg_value = config.get("question_keywords", default_value)
            if isinstance(cfg_value, list):
                merged[key] = [str(item) for item in cfg_value if str(item).strip()]
            else:
                merged[key] = default_value
            continue

        merged[key] = config.get(key, cli_value)
    return merged


def append_plan_to_journal(*, output: Mapping[str, Any], journal_path: Path, run_id: str = "") -> None:
    generated_at = str(output.get("generated_at", ""))
    summary = output.get("summary", {})
    entries = output.get("entries", [])
    if not isinstance(summary, Mapping):
        summary = {}
    if not isinstance(entries, list):
        entries = []

    date_text = generated_at[:10] if len(generated_at) >= 10 else ""
    journal_path.parent.mkdir(parents=True, exist_ok=True)

    run_record = {
        "type": "run_summary",
        "run_id": run_id,
        "generated_at": generated_at,
        "date": date_text,
        "summary": dict(summary),
    }
    with journal_path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(run_record, ensure_ascii=False) + "\n")
        for entry in entries:
            if not isinstance(entry, Mapping):
                continue
            row = {
                "type": "entry",
                "run_id": run_id,
                "generated_at": generated_at,
                "date": date_text,
                "market_id": str(entry.get("market_id", "")),
                "question": str(entry.get("question", "")),
                "side": str(entry.get("side", "")),
                "token_id": str(entry.get("token_id", "")),
                "entry_price": float(entry.get("entry_price", 0.0) or 0.0),
                "model_prob": float(entry.get("model_prob", 0.0) or 0.0),
                "stake_usd": float(entry.get("stake_usd", 0.0) or 0.0),
                "expected_value_usd": float(entry.get("expected_value_usd", 0.0) or 0.0),
                "edge": float(entry.get("edge", 0.0) or 0.0),
                "confidence": float(entry.get("confidence", 0.0) or 0.0),
            }
            handle.write(json.dumps(row, ensure_ascii=False) + "\n")


def fetch_gamma_markets(*, limit: int = 200, gamma_url: str = "https://gamma-api.polymarket.com/markets") -> List[Dict[str, Any]]:
    query = urlencode({"active": "true", "closed": "false", "limit": limit})
    url = f"{gamma_url}?{query}" if "?" not in gamma_url else gamma_url
    req = Request(url, headers={"Accept": "application/json", "User-Agent": "swimmy-openclaw-polymarket-bot/1.0"})
    with urlopen(req, timeout=15) as resp:
        body = resp.read().decode("utf-8")
    payload = json.loads(body)
    if isinstance(payload, list):
        return payload
    if isinstance(payload, MutableMapping) and isinstance(payload.get("markets"), list):
        return payload["markets"]
    raise RuntimeError("unexpected gamma payload shape")


def full_kelly_fraction(*, win_prob: float, price: float) -> float:
    if price <= 0.0 or price >= 1.0:
        return 0.0
    value = (win_prob - price) / (1.0 - price)
    return value if value > 0.0 else 0.0


def _build_candidate(
    *,
    signal: OpenClawSignal,
    market: MarketSnapshot,
    config: BotConfig,
) -> Optional[TradePlanEntry]:
    if signal.confidence < config.min_confidence:
        return None

    edge_yes = signal.p_yes - market.yes_price
    prob_no = 1.0 - signal.p_yes
    edge_no = prob_no - market.no_price

    if edge_yes <= 0.0 and edge_no <= 0.0:
        return None

    if edge_yes >= edge_no:
        side = "YES"
        price = market.yes_price
        model_prob = signal.p_yes
        token_id = market.yes_token_id
        edge = edge_yes
    else:
        side = "NO"
        price = market.no_price
        model_prob = prob_no
        token_id = market.no_token_id
        edge = edge_no

    if config.enable_contrarian_fade:
        favorite_side = "YES" if market.yes_price >= market.no_price else "NO"
        favorite_price = market.yes_price if favorite_side == "YES" else market.no_price
        underdog_edge = edge_no if favorite_side == "YES" else edge_yes
        if _is_gte_with_tol(favorite_price, config.contrarian_favorite_threshold) and _is_gte_with_tol(
            underdog_edge, config.min_edge
        ):
            if favorite_side == "YES":
                side = "NO"
                price = market.no_price
                model_prob = prob_no
                token_id = market.no_token_id
                edge = edge_no
            else:
                side = "YES"
                price = market.yes_price
                model_prob = signal.p_yes
                token_id = market.yes_token_id
                edge = edge_yes

    if not _is_gte_with_tol(edge, config.min_edge):
        return None

    kelly = full_kelly_fraction(win_prob=model_prob, price=price)
    if kelly <= 0.0:
        return None

    return TradePlanEntry(
        market_id=market.market_id,
        question=market.question,
        side=side,
        token_id=token_id,
        model_prob=model_prob,
        entry_price=price,
        edge=edge,
        confidence=signal.confidence,
        kelly_fraction=kelly,
        stake_usd=0.0,
        expected_value_usd=0.0,
    )


def build_trade_plan(
    *,
    signals: Mapping[str, OpenClawSignal],
    markets: Sequence[MarketSnapshot],
    config: BotConfig,
    blocked_market_ids: Sequence[str] = (),
    open_position_count: int = 0,
    daily_entry_count: int = 0,
    daily_loss_streak: int = 0,
    daily_realized_pnl_usd: float = 0.0,
) -> TradePlan:
    total_cost_rate = max(0.0, 2.0 * (config.fee_bps_per_side + config.slippage_bps_per_side) / 10000.0)
    side_min = max(0.0, min(float(config.min_market_price), float(config.max_market_price)))
    side_max = min(1.0, max(float(config.min_market_price), float(config.max_market_price)))
    sum_min = min(float(config.min_price_sum), float(config.max_price_sum))
    sum_max = max(float(config.min_price_sum), float(config.max_price_sum))
    min_liquidity = max(0.0, float(config.min_liquidity_usd))
    min_volume = max(0.0, float(config.min_volume_usd))
    blocked = {str(item).strip() for item in blocked_market_ids if str(item).strip()}
    quality_filtered = 0
    candidates: List[TradePlanEntry] = []
    for market in markets:
        if market.market_id in blocked:
            continue
        total_price = market.yes_price + market.no_price
        if not (
            side_min <= market.yes_price <= side_max
            and side_min <= market.no_price <= side_max
            and sum_min <= total_price <= sum_max
        ):
            quality_filtered += 1
            continue
        if market.liquidity_usd < min_liquidity:
            quality_filtered += 1
            continue
        if market.volume_usd < min_volume:
            quality_filtered += 1
            continue
        signal = signals.get(market.market_id)
        if signal is None:
            continue
        candidate = _build_candidate(signal=signal, market=market, config=config)
        if candidate is not None:
            candidates.append(candidate)

    candidates.sort(key=lambda item: (item.edge, item.confidence), reverse=True)

    daily_risk_budget = max(0.0, config.bankroll_usd * (config.max_daily_loss_pct / 100.0))
    per_trade_cap = max(0.0, config.bankroll_usd * (config.max_trade_risk_pct / 100.0))
    remaining_budget = daily_risk_budget
    max_entries_this_run = max(0, config.max_positions)
    if config.max_open_positions > 0:
        remaining_open_slots = max(0, config.max_open_positions - max(0, int(open_position_count)))
        max_entries_this_run = min(max_entries_this_run, remaining_open_slots)
    if config.max_daily_entries > 0:
        remaining_daily_slots = max(0, config.max_daily_entries - max(0, int(daily_entry_count)))
        max_entries_this_run = min(max_entries_this_run, remaining_daily_slots)
    if config.max_daily_loss_streak > 0 and int(daily_loss_streak) >= int(config.max_daily_loss_streak):
        max_entries_this_run = 0
    if config.max_daily_realized_loss_usd > 0.0 and float(daily_realized_pnl_usd) <= -float(config.max_daily_realized_loss_usd):
        max_entries_this_run = 0

    entries: List[TradePlanEntry] = []
    for candidate in candidates:
        if len(entries) >= max_entries_this_run:
            break
        if remaining_budget <= 0.0:
            break

        kelly_target = max(0.0, config.bankroll_usd * candidate.kelly_fraction * max(0.0, config.kelly_scale))
        stake = min(kelly_target, per_trade_cap, remaining_budget)
        if stake < config.min_stake_usd:
            continue

        expected_value_usd = 0.0
        if candidate.entry_price > 0.0:
            expected_value_usd = stake * ((candidate.edge / candidate.entry_price) - total_cost_rate)
        if expected_value_usd <= 0.0:
            continue

        candidate.stake_usd = round(stake, 2)
        candidate.expected_value_usd = round(expected_value_usd, 4)
        entries.append(candidate)
        remaining_budget -= stake

    total_stake = round(sum(item.stake_usd for item in entries), 2)
    remaining = round(max(0.0, daily_risk_budget - total_stake), 2)
    return TradePlan(
        generated_at=datetime.now(timezone.utc).isoformat(),
        entries=entries,
        total_stake_usd=total_stake,
        daily_risk_budget_usd=round(daily_risk_budget, 2),
        remaining_risk_budget_usd=remaining,
        quality_filtered_markets=quality_filtered,
    )


def _load_markets_from_file(path: Path) -> List[Dict[str, Any]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if isinstance(payload, list):
        return payload
    if isinstance(payload, dict) and isinstance(payload.get("markets"), list):
        return payload["markets"]
    raise RuntimeError(f"unsupported markets payload in {path}")


def load_settled_market_ids(path: Path) -> Set[str]:
    return set(load_settlements_map(path).keys())


def load_settlements_map(path: Path) -> Dict[str, str]:
    if not path.exists():
        return {}
    payload = json.loads(path.read_text(encoding="utf-8"))
    settled: Dict[str, str] = {}
    if isinstance(payload, dict):
        for market_id, outcome in payload.items():
            normalized = _normalize_outcome(outcome)
            if normalized:
                market_text = str(market_id).strip()
                if market_text:
                    settled[market_text] = normalized
        return settled
    if isinstance(payload, list):
        for item in payload:
            if not isinstance(item, Mapping):
                continue
            market_id = str(item.get("market_id") or item.get("id") or "").strip()
            normalized = _normalize_outcome(item.get("winner", item.get("outcome", item.get("result"))))
            if market_id and normalized:
                settled[market_id] = normalized
    return settled


def load_unresolved_market_ids(*, journal_path: Path, settled_market_ids: Sequence[str]) -> Set[str]:
    if not journal_path.exists():
        return set()
    settled = {str(item).strip() for item in settled_market_ids if str(item).strip()}
    opened: Set[str] = set()
    with journal_path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            text = raw.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, Mapping):
                continue
            if str(payload.get("type", "")).strip().lower() != "entry":
                continue
            market_id = str(payload.get("market_id", "")).strip()
            if market_id:
                opened.add(market_id)
    return {market_id for market_id in opened if market_id not in settled}


def load_daily_entry_count(*, journal_path: Path, target_date: str) -> int:
    if not journal_path.exists():
        return 0
    count = 0
    with journal_path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            text = raw.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, Mapping):
                continue
            if str(payload.get("type", "")).strip().lower() != "entry":
                continue
            if str(payload.get("date", "")).strip() == target_date:
                count += 1
    return count


def load_daily_loss_streak(*, journal_path: Path, target_date: str, settlements: Mapping[str, str]) -> int:
    if not journal_path.exists():
        return 0
    entries: List[Dict[str, str]] = []
    with journal_path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            text = raw.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, Mapping):
                continue
            if str(payload.get("type", "")).strip().lower() != "entry":
                continue
            if str(payload.get("date", "")).strip() != target_date:
                continue
            market_id = str(payload.get("market_id", "")).strip()
            side = _normalize_outcome(payload.get("side"))
            if market_id and side:
                entries.append({"market_id": market_id, "side": side})

    streak = 0
    for entry in reversed(entries):
        market_id = entry["market_id"]
        side = entry["side"]
        winner = _normalize_outcome(settlements.get(market_id))
        if not winner:
            continue
        if side == winner:
            break
        streak += 1
    return streak


def load_daily_realized_pnl_usd(
    *,
    journal_path: Path,
    target_date: str,
    settlements: Mapping[str, str],
    fee_bps_per_side: float = 0.0,
    slippage_bps_per_side: float = 0.0,
) -> float:
    if not journal_path.exists():
        return 0.0
    cost_rate = max(0.0, 2.0 * (float(fee_bps_per_side) + float(slippage_bps_per_side)) / 10000.0)
    total = 0.0
    with journal_path.open("r", encoding="utf-8") as handle:
        for raw in handle:
            text = raw.strip()
            if not text:
                continue
            try:
                payload = json.loads(text)
            except json.JSONDecodeError:
                continue
            if not isinstance(payload, Mapping):
                continue
            if str(payload.get("type", "")).strip().lower() != "entry":
                continue
            if str(payload.get("date", "")).strip() != target_date:
                continue
            market_id = str(payload.get("market_id", "")).strip()
            side = _normalize_outcome(payload.get("side"))
            winner = _normalize_outcome(settlements.get(market_id))
            stake = float(payload.get("stake_usd", 0.0) or 0.0)
            entry_price = float(payload.get("entry_price", 0.0) or 0.0)
            if not market_id or not side or not winner or stake <= 0.0 or entry_price <= 0.0 or entry_price >= 1.0:
                continue
            is_win = side == winner
            gross = (stake * ((1.0 / entry_price) - 1.0)) if is_win else -stake
            total += gross - (stake * cost_rate)
    return total


def main() -> None:
    parser = argparse.ArgumentParser(description="OpenClaw + Polymarket dry-run trade planner")
    parser.add_argument("--config-file", default="", help="Optional JSON file for runtime parameters")
    parser.add_argument("--signals-file", default="", help="OpenClaw JSONL signals path")
    parser.add_argument("--openclaw-cmd", default="", help="Command that prints OpenClaw signals JSONL/JSON array")
    parser.add_argument("--markets-file", default="", help="Polymarket markets JSON path (optional)")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument("--limit", type=int, default=200)
    parser.add_argument("--bankroll-usd", type=float, default=1000.0)
    parser.add_argument("--max-daily-loss-pct", type=float, default=0.5)
    parser.add_argument("--max-trade-risk-pct", type=float, default=0.25)
    parser.add_argument("--max-positions", type=int, default=4)
    parser.add_argument("--max-open-positions", type=int, default=12)
    parser.add_argument("--max-daily-entries", type=int, default=12)
    parser.add_argument("--max-daily-loss-streak", type=int, default=0)
    parser.add_argument("--max-daily-realized-loss-usd", type=float, default=0.0)
    parser.add_argument("--min-price-sum", type=float, default=0.90)
    parser.add_argument("--max-price-sum", type=float, default=1.10)
    parser.add_argument("--min-market-price", type=float, default=0.03)
    parser.add_argument("--max-market-price", type=float, default=0.97)
    parser.add_argument("--min-liquidity-usd", type=float, default=0.0)
    parser.add_argument("--min-volume-usd", type=float, default=0.0)
    parser.add_argument("--min-edge", type=float, default=0.02)
    parser.add_argument("--min-confidence", type=float, default=0.0)
    parser.add_argument("--min-stake-usd", type=float, default=1.0)
    parser.add_argument("--kelly-scale", type=float, default=0.5)
    parser.add_argument("--fee-bps-per-side", type=float, default=0.0)
    parser.add_argument("--slippage-bps-per-side", type=float, default=0.0)
    parser.add_argument("--enable-contrarian-fade", action="store_true")
    parser.add_argument("--contrarian-favorite-threshold", type=float, default=0.60)
    parser.add_argument(
        "--question-keyword",
        action="append",
        default=[],
        help="Filter markets whose question contains this keyword (repeatable)",
    )
    parser.add_argument("--write-plan", default="", help="Optional JSON output file")
    parser.add_argument("--journal-file", default="", help="Optional JSONL journal path for paper-trade tracking")
    parser.add_argument("--run-id", default="", help="Optional run identifier to include in journal records")
    parser.add_argument(
        "--settlements-file",
        default="",
        help="Optional resolved-outcome JSON used to avoid re-entering already-settled markets",
    )
    parser.add_argument(
        "--allow-duplicate-open-markets",
        action="store_true",
        help="Allow repeated entries for markets already present in journal entries",
    )
    args = parser.parse_args()

    runtime_keys = [
        "bankroll_usd",
        "max_daily_loss_pct",
        "max_trade_risk_pct",
        "max_positions",
        "max_open_positions",
        "max_daily_entries",
        "max_daily_loss_streak",
        "max_daily_realized_loss_usd",
        "min_price_sum",
        "max_price_sum",
        "min_market_price",
        "max_market_price",
        "min_liquidity_usd",
        "min_volume_usd",
        "min_edge",
        "min_confidence",
        "min_stake_usd",
        "kelly_scale",
        "fee_bps_per_side",
        "slippage_bps_per_side",
        "enable_contrarian_fade",
        "contrarian_favorite_threshold",
        "question_keyword",
    ]
    defaults = {key: parser.get_default(key) for key in runtime_keys}
    config_payload: Dict[str, Any] = {}
    if args.config_file:
        config_payload = json.loads(Path(args.config_file).read_text(encoding="utf-8"))
    merged = merge_runtime_options(cli=vars(args), defaults=defaults, config=config_payload)

    if not args.signals_file and not args.openclaw_cmd:
        raise SystemExit("provide --signals-file or --openclaw-cmd")

    if args.signals_file:
        signals = load_openclaw_signals(Path(args.signals_file))
    else:
        signals = load_openclaw_signals_from_command(args.openclaw_cmd)

    if args.markets_file:
        raw_markets = _load_markets_from_file(Path(args.markets_file))
    else:
        raw_markets = fetch_gamma_markets(limit=max(1, args.limit), gamma_url=args.gamma_url)

    markets = parse_gamma_markets(raw_markets)
    markets = filter_markets_by_keywords(markets, merged["question_keyword"])
    config = BotConfig(
        bankroll_usd=float(merged["bankroll_usd"]),
        max_daily_loss_pct=float(merged["max_daily_loss_pct"]),
        max_trade_risk_pct=float(merged["max_trade_risk_pct"]),
        max_positions=int(merged["max_positions"]),
        max_open_positions=int(merged["max_open_positions"]),
        max_daily_entries=int(merged["max_daily_entries"]),
        max_daily_loss_streak=int(merged["max_daily_loss_streak"]),
        max_daily_realized_loss_usd=float(merged["max_daily_realized_loss_usd"]),
        min_price_sum=float(merged["min_price_sum"]),
        max_price_sum=float(merged["max_price_sum"]),
        min_market_price=float(merged["min_market_price"]),
        max_market_price=float(merged["max_market_price"]),
        min_liquidity_usd=float(merged["min_liquidity_usd"]),
        min_volume_usd=float(merged["min_volume_usd"]),
        min_edge=float(merged["min_edge"]),
        min_confidence=float(merged["min_confidence"]),
        min_stake_usd=float(merged["min_stake_usd"]),
        kelly_scale=float(merged["kelly_scale"]),
        fee_bps_per_side=float(merged["fee_bps_per_side"]),
        slippage_bps_per_side=float(merged["slippage_bps_per_side"]),
        enable_contrarian_fade=bool(merged["enable_contrarian_fade"]),
        contrarian_favorite_threshold=float(merged["contrarian_favorite_threshold"]),
    )
    blocked_open_markets: Set[str] = set()
    daily_entry_count = 0
    daily_loss_streak = 0
    daily_realized_pnl_usd = 0.0
    settlements_map: Dict[str, str] = {}
    today_utc = datetime.now(timezone.utc).strftime("%Y-%m-%d")
    if args.settlements_file:
        settlements_map = load_settlements_map(Path(args.settlements_file))
    if args.journal_file and not args.allow_duplicate_open_markets:
        settled_market_ids: Set[str] = set(settlements_map.keys())
        blocked_open_markets = load_unresolved_market_ids(
            journal_path=Path(args.journal_file),
            settled_market_ids=settled_market_ids,
        )
    if args.journal_file:
        daily_entry_count = load_daily_entry_count(journal_path=Path(args.journal_file), target_date=today_utc)
        if settlements_map:
            daily_loss_streak = load_daily_loss_streak(
                journal_path=Path(args.journal_file),
                target_date=today_utc,
                settlements=settlements_map,
            )
            daily_realized_pnl_usd = load_daily_realized_pnl_usd(
                journal_path=Path(args.journal_file),
                target_date=today_utc,
                settlements=settlements_map,
                fee_bps_per_side=config.fee_bps_per_side,
                slippage_bps_per_side=config.slippage_bps_per_side,
            )

    plan = build_trade_plan(
        signals=signals,
        markets=markets,
        config=config,
        blocked_market_ids=blocked_open_markets,
        open_position_count=len(blocked_open_markets),
        daily_entry_count=daily_entry_count,
        daily_loss_streak=daily_loss_streak,
        daily_realized_pnl_usd=daily_realized_pnl_usd,
    )

    output = {
        "generated_at": plan.generated_at,
        "summary": {
            "signals": len(signals),
            "markets": len(markets),
            "open_markets": len(blocked_open_markets),
            "blocked_open_markets": len(blocked_open_markets),
            "max_open_positions": config.max_open_positions,
            "open_capacity_exhausted": config.max_open_positions > 0
            and len(blocked_open_markets) >= config.max_open_positions,
            "daily_entries": daily_entry_count,
            "max_daily_entries": config.max_daily_entries,
            "daily_entry_capacity_exhausted": config.max_daily_entries > 0
            and daily_entry_count >= config.max_daily_entries,
            "daily_loss_streak": daily_loss_streak,
            "max_daily_loss_streak": config.max_daily_loss_streak,
            "daily_loss_streak_capacity_exhausted": config.max_daily_loss_streak > 0
            and daily_loss_streak >= config.max_daily_loss_streak,
            "daily_realized_pnl_usd": round(daily_realized_pnl_usd, 4),
            "max_daily_realized_loss_usd": config.max_daily_realized_loss_usd,
            "daily_realized_loss_capacity_exhausted": config.max_daily_realized_loss_usd > 0.0
            and daily_realized_pnl_usd <= -config.max_daily_realized_loss_usd,
            "entries": len(plan.entries),
            "total_stake_usd": plan.total_stake_usd,
            "daily_risk_budget_usd": plan.daily_risk_budget_usd,
            "remaining_risk_budget_usd": plan.remaining_risk_budget_usd,
            "quality_filtered_markets": plan.quality_filtered_markets,
        },
        "entries": [asdict(item) for item in plan.entries],
    }
    text = json.dumps(output, ensure_ascii=False, indent=2)
    print(text)
    if args.write_plan:
        Path(args.write_plan).write_text(text + "\n", encoding="utf-8")
    if args.journal_file:
        append_plan_to_journal(output=output, journal_path=Path(args.journal_file), run_id=args.run_id)


if __name__ == "__main__":
    main()
