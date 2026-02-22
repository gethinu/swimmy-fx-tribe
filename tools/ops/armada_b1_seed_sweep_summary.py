#!/usr/bin/env python3
"""
Build B1R seed sweep summary from armada_player_replica reports.
"""

from __future__ import annotations

import argparse
import json
import time
from datetime import datetime, timezone
from pathlib import Path


def parse_seed_list(raw: str) -> tuple[int, ...]:
    values: list[int] = []
    for token in str(raw).split(","):
        token = token.strip()
        if not token:
            continue
        values.append(int(token))
    if not values:
        raise ValueError("seed list is empty")
    return tuple(values)


def _as_int(value, default: int = 0) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _summarize_player(player_payload: dict, *, top_per_player: int) -> dict:
    selected = list(player_payload.get("selected", [])[:top_per_player])
    top_oos_ok = 0
    top_cpcv_ok = 0
    top_combo_ok = 0
    top_bt_ok = 0
    top_strong = 0
    timeframes: list[int] = []
    indicators: list[str] = []

    for row in selected:
        verdict = row.get("verdict", {}) if isinstance(row, dict) else {}
        candidate = row.get("candidate", {}) if isinstance(row, dict) else {}
        oos_ok = bool(verdict.get("oos_ok"))
        cpcv_ok = bool(verdict.get("cpcv_ok"))
        bt_ok = bool(verdict.get("bt_ok"))
        strong = bool(verdict.get("is_strong"))
        indicator = str(candidate.get("indicator_type", "")).strip()
        timeframe = _as_int(candidate.get("timeframe"), default=0)

        top_oos_ok += 1 if oos_ok else 0
        top_cpcv_ok += 1 if cpcv_ok else 0
        top_combo_ok += 1 if (oos_ok and cpcv_ok) else 0
        top_bt_ok += 1 if bt_ok else 0
        top_strong += 1 if strong else 0
        if timeframe > 0:
            timeframes.append(timeframe)
        if indicator:
            indicators.append(indicator)

    return {
        "top3_oos_ok": top_oos_ok,
        "top3_cpcv_ok": top_cpcv_ok,
        "top3_combo_ok": top_combo_ok,
        "top3_bt_ok": top_bt_ok,
        "top3_strong": top_strong,
        "top3_timeframes": timeframes,
        "top3_indicators": indicators,
        "gate_pass": top_oos_ok >= 1,
    }


def _player_map(report_payload: dict) -> dict[str, dict]:
    mapped: dict[str, dict] = {}
    for item in report_payload.get("players", []):
        if not isinstance(item, dict):
            continue
        profile = item.get("profile", {})
        if not isinstance(profile, dict):
            continue
        key = str(profile.get("key", "")).strip().lower()
        if not key:
            continue
        mapped[key] = item
    return mapped


def build_seed_sweep_summary(
    *,
    players: tuple[str, ...],
    seeds: tuple[int, ...],
    report_template: str,
    required_min_both_pass: int,
    top_per_player: int,
) -> dict:
    normalized_players = tuple(p.strip().lower() for p in players if p and p.strip())
    if not normalized_players:
        raise ValueError("players are required")
    if not seeds:
        raise ValueError("seeds are required")
    if "{seed}" not in report_template:
        raise ValueError("report template must include '{seed}' placeholder")

    player_pass_counts = {p: 0 for p in normalized_players}
    both_players_pass_count = 0
    missing: list[str] = []
    per_seed: list[dict] = []

    for seed in seeds:
        report_path = Path(report_template.format(seed=seed))
        if not report_path.exists():
            missing.append(str(report_path))
            per_seed.append(
                {
                    "seed": seed,
                    "report": str(report_path),
                    "status": "missing",
                    "players": {p: {"gate_pass": False, "status": "missing"} for p in normalized_players},
                }
            )
            continue

        payload = json.loads(report_path.read_text(encoding="utf-8"))
        player_lookup = _player_map(payload)
        seed_players: dict[str, dict] = {}
        all_pass = True

        for player in normalized_players:
            raw_player = player_lookup.get(player)
            if raw_player is None:
                seed_players[player] = {"gate_pass": False, "status": "missing_player"}
                all_pass = False
                continue
            summary = _summarize_player(raw_player, top_per_player=top_per_player)
            seed_players[player] = summary
            if summary["gate_pass"]:
                player_pass_counts[player] += 1
            else:
                all_pass = False

        if all_pass:
            both_players_pass_count += 1

        per_seed.append(
            {
                "seed": seed,
                "report": str(report_path),
                "status": "completed",
                "players": seed_players,
            }
        )

    return {
        "generated_at": time.time(),
        "report_type": "armada_b1_seed_sweep_summary",
        "date": datetime.now(timezone.utc).strftime("%Y-%m-%d"),
        "config": {
            "players": list(normalized_players),
            "seeds": list(seeds),
            "top_per_player": top_per_player,
            "gate_definition": "top3_oos_ok >= 1 for each player",
            "completion_rule": f">={required_min_both_pass}/{len(seeds)} seeds pass for both players",
        },
        "missing": missing,
        "per_seed": per_seed,
        "aggregate": {
            "player_pass_counts": player_pass_counts,
            "both_players_pass_count": both_players_pass_count,
            "required_min_both_pass": required_min_both_pass,
            "total_seeds": len(seeds),
            "b1r_completed": both_players_pass_count >= required_min_both_pass and len(missing) == 0,
        },
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build B1R seed sweep summary.")
    parser.add_argument(
        "--players",
        default="taiki,kojirin",
        help="Comma-separated players (default: taiki,kojirin)",
    )
    parser.add_argument(
        "--seeds",
        default="11,23,47,83,131",
        help="Comma-separated seeds (default: 11,23,47,83,131)",
    )
    parser.add_argument(
        "--report-template",
        required=True,
        help="Input report path template with {seed} placeholder.",
    )
    parser.add_argument(
        "--required-min-both-pass",
        type=int,
        default=4,
        help="Required seeds where all players pass (default: 4).",
    )
    parser.add_argument(
        "--top-per-player",
        type=int,
        default=3,
        help="Top candidates to evaluate per player (default: 3).",
    )
    parser.add_argument(
        "--output",
        required=True,
        help="Output JSON path.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    summary = build_seed_sweep_summary(
        players=tuple(p.strip() for p in str(args.players).split(",")),
        seeds=parse_seed_list(args.seeds),
        report_template=str(args.report_template),
        required_min_both_pass=max(0, int(args.required_min_both_pass)),
        top_per_player=max(1, int(args.top_per_player)),
    )

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(summary, ensure_ascii=False, indent=2), encoding="utf-8")

    aggregate = summary["aggregate"]
    print(f"[OK] saved summary: {out_path}")
    print(
        "[OK] b1r_completed="
        f"{aggregate['b1r_completed']} "
        f"both_players_pass_count={aggregate['both_players_pass_count']}/"
        f"{aggregate['total_seeds']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
