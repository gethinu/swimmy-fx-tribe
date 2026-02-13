#!/usr/bin/env python3
"""Execute live Polymarket limit orders from a generated plan file."""

from __future__ import annotations

import argparse
import json
import os
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Mapping, Sequence, Tuple


@dataclass
class LiveRuntimeConfig:
    host: str
    chain_id: int
    private_key: str
    signature_type: int
    funder: str
    api_key: str
    api_secret: str
    api_passphrase: str


@dataclass
class PlanEntry:
    market_id: str
    token_id: str
    side: str
    entry_price: float
    stake_usd: float
    expected_value_usd: float


def _to_float(value: Any, default: float = 0.0) -> float:
    try:
        return float(value)
    except (TypeError, ValueError):
        return float(default)


def _env_bool(env: Mapping[str, str], key: str, default: bool = False) -> bool:
    text = str(env.get(key, "")).strip().lower()
    if not text:
        return default
    return text in {"1", "true", "yes", "on"}


def _normalize_private_key(private_key: str) -> str:
    text = str(private_key or "").strip()
    if not text:
        return ""
    if text.lower() in {"0x...", "changeme", "change_me", "your_private_key"}:
        return ""
    if re.fullmatch(r"[0-9a-fA-F]{64}", text):
        return "0x" + text
    return text


def load_private_key(env: Mapping[str, str]) -> str:
    key_file = str(env.get("POLYCLAW_LIVE_PRIVATE_KEY_FILE", "")).strip()
    if key_file:
        path = Path(key_file).expanduser()
        try:
            if path.exists() and path.is_file():
                candidate = _normalize_private_key(path.read_text(encoding="utf-8").strip())
                if candidate:
                    return candidate
        except Exception:
            pass

    return _normalize_private_key(str(env.get("POLYCLAW_LIVE_PRIVATE_KEY", "")).strip())


def parse_plan_entries(plan_payload: Mapping[str, Any]) -> List[PlanEntry]:
    rows = plan_payload.get("entries", [])
    if not isinstance(rows, list):
        return []
    entries: List[PlanEntry] = []
    for item in rows:
        if not isinstance(item, Mapping):
            continue
        token_id = str(item.get("token_id") or "").strip()
        market_id = str(item.get("market_id") or "").strip()
        side = str(item.get("side") or "").strip().upper()
        entry_price = _to_float(item.get("entry_price"), 0.0)
        stake_usd = _to_float(item.get("stake_usd"), 0.0)
        expected_value_usd = _to_float(item.get("expected_value_usd"), 0.0)
        if not token_id or not market_id:
            continue
        if side not in {"YES", "NO"}:
            continue
        if entry_price <= 0.0 or entry_price >= 1.0:
            continue
        if stake_usd <= 0.0:
            continue
        entries.append(
            PlanEntry(
                market_id=market_id,
                token_id=token_id,
                side=side,
                entry_price=entry_price,
                stake_usd=stake_usd,
                expected_value_usd=expected_value_usd,
            )
        )
    return entries


def build_limit_order_specs(
    *,
    entries: Sequence[PlanEntry],
    max_orders: int,
    min_expected_value_usd: float,
    min_stake_usd: float,
) -> List[Dict[str, Any]]:
    specs: List[Dict[str, Any]] = []
    limit = max(0, int(max_orders))
    min_ev = max(0.0, float(min_expected_value_usd))
    min_stake = max(0.0, float(min_stake_usd))
    for entry in entries:
        if limit > 0 and len(specs) >= limit:
            break
        if float(entry.stake_usd) < min_stake:
            continue
        if float(entry.expected_value_usd) < min_ev:
            continue
        size = float(entry.stake_usd) / float(entry.entry_price)
        if size <= 0.0:
            continue
        specs.append(
            {
                "market_id": entry.market_id,
                "token_id": entry.token_id,
                "entry_side": entry.side,
                "entry_price": round(float(entry.entry_price), 8),
                "stake_usd": round(float(entry.stake_usd), 8),
                "size": round(size, 8),
                "expected_value_usd": round(float(entry.expected_value_usd), 8),
            }
        )
    return specs


def load_runtime_config(env: Mapping[str, str]) -> LiveRuntimeConfig:
    host = str(env.get("POLYCLAW_LIVE_HOST", "https://clob.polymarket.com")).strip()
    chain_id = int(str(env.get("POLYCLAW_LIVE_CHAIN_ID", "137")).strip() or "137")
    private_key = load_private_key(env)
    signature_type = int(str(env.get("POLYCLAW_LIVE_SIGNATURE_TYPE", "0")).strip() or "0")
    funder = str(env.get("POLYCLAW_LIVE_FUNDER", "")).strip()
    api_key = str(env.get("POLYCLAW_LIVE_API_KEY", "")).strip()
    api_secret = str(env.get("POLYCLAW_LIVE_API_SECRET", "")).strip()
    api_passphrase = str(env.get("POLYCLAW_LIVE_API_PASSPHRASE", "")).strip()
    if not private_key:
        raise ValueError("POLYCLAW_LIVE_PRIVATE_KEY (or POLYCLAW_LIVE_PRIVATE_KEY_FILE) is required for live execution")
    return LiveRuntimeConfig(
        host=host,
        chain_id=chain_id,
        private_key=private_key,
        signature_type=signature_type,
        funder=funder,
        api_key=api_key,
        api_secret=api_secret,
        api_passphrase=api_passphrase,
    )


def resolve_order_type(order_type_text: str, order_type_enum: Any) -> Any:
    normalized = str(order_type_text or "").strip().upper() or "GTC"
    if not hasattr(order_type_enum, normalized):
        raise ValueError(f"unsupported order type: {normalized}")
    return getattr(order_type_enum, normalized)


def initialize_live_client(config: LiveRuntimeConfig) -> Tuple[Any, Any, Any, Any]:
    try:
        from py_clob_client.client import ClobClient
        from py_clob_client.clob_types import ApiCreds, OrderArgs, OrderType
        from py_clob_client.order_builder.constants import BUY
    except Exception as exc:  # pragma: no cover
        raise RuntimeError(f"py-clob-client unavailable: {exc}") from exc

    client = ClobClient(
        config.host,
        chain_id=config.chain_id,
        key=config.private_key,
        signature_type=config.signature_type,
        funder=config.funder or None,
    )
    if config.api_key and config.api_secret and config.api_passphrase:
        client.set_api_creds(
            ApiCreds(
                api_key=config.api_key,
                api_secret=config.api_secret,
                api_passphrase=config.api_passphrase,
            )
        )
    else:
        client.set_api_creds(client.create_or_derive_api_creds())
    return client, OrderArgs, OrderType, BUY


def post_limit_orders(
    *,
    order_specs: Sequence[Mapping[str, Any]],
    client: Any,
    order_args_ctor: Any,
    order_type_enum: Any,
    buy_side_value: Any,
    order_type_text: str,
    dry_run: bool,
) -> Dict[str, Any]:
    attempted = len(order_specs)
    sent = 0
    failed = 0
    results: List[Dict[str, Any]] = []
    if dry_run:
        for spec in order_specs:
            row = {
                "market_id": str(spec.get("market_id") or ""),
                "token_id": str(spec.get("token_id") or ""),
                "entry_side": str(spec.get("entry_side") or ""),
                "entry_price": _to_float(spec.get("entry_price"), 0.0),
                "stake_usd": _to_float(spec.get("stake_usd"), 0.0),
                "size": _to_float(spec.get("size"), 0.0),
                "expected_value_usd": _to_float(spec.get("expected_value_usd"), 0.0),
                "ok": True,
                "error": "",
                "response": {"dry_run": True},
            }
            sent += 1
            results.append(row)
        return {
            "attempted": attempted,
            "sent": sent,
            "failed": failed,
            "results": results,
        }

    order_type_value = resolve_order_type(order_type_text, order_type_enum)
    for spec in order_specs:
        row = {
            "market_id": str(spec.get("market_id") or ""),
            "token_id": str(spec.get("token_id") or ""),
            "entry_side": str(spec.get("entry_side") or ""),
            "entry_price": _to_float(spec.get("entry_price"), 0.0),
            "stake_usd": _to_float(spec.get("stake_usd"), 0.0),
            "size": _to_float(spec.get("size"), 0.0),
            "expected_value_usd": _to_float(spec.get("expected_value_usd"), 0.0),
            "ok": False,
            "error": "",
            "response": {},
        }
        try:
            order_args = order_args_ctor(
                token_id=row["token_id"],
                price=float(row["entry_price"]),
                size=float(row["size"]),
                side=buy_side_value,
            )
            signed = client.create_order(order_args)
            response = client.post_order(signed, order_type_value)
            row["ok"] = True
            row["response"] = response if isinstance(response, Mapping) else {"raw": str(response)}
            sent += 1
        except Exception as exc:  # pragma: no cover - depends on remote API
            row["error"] = str(exc)
            failed += 1
        results.append(row)
    return {
        "attempted": attempted,
        "sent": sent,
        "failed": failed,
        "results": results,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description="Execute live Polymarket orders from plan JSON")
    parser.add_argument("--plan-file", required=True)
    parser.add_argument("--run-id", default="")
    parser.add_argument("--run-date", default="")
    parser.add_argument("--order-type", default=os.getenv("POLYCLAW_LIVE_ORDER_TYPE", "GTC"))
    parser.add_argument("--max-orders", type=int, default=int(os.getenv("POLYCLAW_LIVE_MAX_ORDERS_PER_RUN", "0") or "0"))
    parser.add_argument(
        "--min-expected-value-usd",
        type=float,
        default=float(os.getenv("POLYCLAW_LIVE_MIN_EXPECTED_VALUE_USD", "0") or "0"),
    )
    parser.add_argument(
        "--min-stake-usd",
        type=float,
        default=float(os.getenv("POLYCLAW_LIVE_MIN_STAKE_USD", "1.0") or "1.0"),
    )
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--write-report", default="")
    parser.add_argument("--fail-on-error", action="store_true")
    args = parser.parse_args()

    plan_path = Path(args.plan_file)
    if not plan_path.exists() or not plan_path.is_file():
        raise SystemExit(f"plan file not found: {plan_path}")

    payload = json.loads(plan_path.read_text(encoding="utf-8"))
    if not isinstance(payload, Mapping):
        raise SystemExit(f"unsupported plan payload: {plan_path}")
    entries = parse_plan_entries(payload)
    order_specs = build_limit_order_specs(
        entries=entries,
        max_orders=max(0, args.max_orders),
        min_expected_value_usd=max(0.0, args.min_expected_value_usd),
        min_stake_usd=max(0.0, args.min_stake_usd),
    )

    env = dict(os.environ)
    dry_run = bool(args.dry_run or _env_bool(env, "POLYCLAW_LIVE_DRY_RUN", False))
    result: Dict[str, Any] = {
        "ok": True,
        "run_id": args.run_id.strip(),
        "run_date": args.run_date.strip(),
        "plan_file": str(plan_path),
        "order_type": str(args.order_type).strip().upper() or "GTC",
        "dry_run": dry_run,
        "order_specs": order_specs,
        "attempted": 0,
        "sent": 0,
        "failed": 0,
        "results": [],
        "error": "",
    }

    if not order_specs:
        text = json.dumps(result, ensure_ascii=False, indent=2)
        print(text)
        if args.write_report:
            Path(args.write_report).write_text(text + "\n", encoding="utf-8")
        return

    try:
        if dry_run:
            post_result = post_limit_orders(
                order_specs=order_specs,
                client=None,
                order_args_ctor=None,
                order_type_enum=None,
                buy_side_value=None,
                order_type_text=result["order_type"],
                dry_run=True,
            )
        else:
            runtime = load_runtime_config(env)
            client, order_args_ctor, order_type_enum, buy_side_value = initialize_live_client(runtime)
            post_result = post_limit_orders(
                order_specs=order_specs,
                client=client,
                order_args_ctor=order_args_ctor,
                order_type_enum=order_type_enum,
                buy_side_value=buy_side_value,
                order_type_text=result["order_type"],
                dry_run=False,
            )
        result["attempted"] = int(post_result["attempted"])
        result["sent"] = int(post_result["sent"])
        result["failed"] = int(post_result["failed"])
        result["results"] = post_result["results"]
        result["ok"] = result["failed"] == 0
    except Exception as exc:
        result["ok"] = False
        result["error"] = str(exc)

    text = json.dumps(result, ensure_ascii=False, indent=2)
    print(text)
    if args.write_report:
        Path(args.write_report).write_text(text + "\n", encoding="utf-8")

    if args.fail_on_error and not result["ok"]:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
