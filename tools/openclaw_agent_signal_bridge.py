#!/usr/bin/env python3
"""Generate OpenClaw-style signals via OpenClaw agent --local output."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence


def resolve_base_dir() -> Path:
    env = os.getenv("SWIMMY_HOME")
    if env:
        return Path(env)
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
if str(BASE_DIR) not in sys.path:
    sys.path.insert(0, str(BASE_DIR))

from tools.openclaw_signal_heuristic import (
    _clamp,
    build_signals_from_markets,
    fetch_markets,
    render_jsonl,
)


def _to_float(value: Any) -> Optional[float]:
    try:
        return float(value)
    except (TypeError, ValueError):
        return None


def extract_agent_json_from_stdout(stdout: str) -> Mapping[str, Any]:
    text = stdout.strip()
    if not text:
        return {}
    try:
        payload = json.loads(text)
        return payload if isinstance(payload, Mapping) else {}
    except json.JSONDecodeError:
        pass

    start = text.find("{")
    end = text.rfind("}")
    if start >= 0 and end > start:
        try:
            payload = json.loads(text[start : end + 1])
            return payload if isinstance(payload, Mapping) else {}
        except json.JSONDecodeError:
            return {}
    return {}


def _strip_code_fence(text: str) -> str:
    lines = text.strip().splitlines()
    if len(lines) >= 3 and lines[0].strip().startswith("```") and lines[-1].strip() == "```":
        return "\n".join(lines[1:-1]).strip()
    return text.strip()


def _extract_json_blob(text: str) -> str:
    stripped = _strip_code_fence(text)
    if stripped:
        return stripped
    return text.strip()


def parse_signal_payload(payload_text: str) -> Dict[str, Dict[str, float]]:
    text = _extract_json_blob(payload_text)
    if not text:
        return {}

    def _rows_from_json(candidate: str) -> List[Mapping[str, Any]]:
        try:
            payload = json.loads(candidate)
        except json.JSONDecodeError:
            return []
        if isinstance(payload, Mapping):
            return [payload]
        if isinstance(payload, list):
            return [item for item in payload if isinstance(item, Mapping)]
        return []

    rows = _rows_from_json(text)
    if not rows:
        left = text.find("[")
        right = text.rfind("]")
        if left >= 0 and right > left:
            rows = _rows_from_json(text[left : right + 1])
    if not rows:
        left = text.find("{")
        right = text.rfind("}")
        if left >= 0 and right > left:
            rows = _rows_from_json(text[left : right + 1])
    if not rows:
        rows = []
        for line in text.splitlines():
            line = line.strip()
            if not line:
                continue
            parsed_rows = _rows_from_json(line)
            rows.extend(parsed_rows)

    out: Dict[str, Dict[str, float]] = {}
    for row in rows:
        market_id = str(row.get("market_id") or row.get("id") or "").strip()
        if not market_id:
            continue
        p_yes = _to_float(row.get("p_yes", row.get("prob_yes", row.get("probability_yes"))))
        if p_yes is None:
            continue
        confidence = _to_float(row.get("confidence"))
        if confidence is None:
            confidence = 0.6
        out[market_id] = {
            "p_yes": _clamp(float(p_yes), 0.01, 0.99),
            "confidence": _clamp(float(confidence), 0.0, 1.0),
        }
    return out


def merge_agent_signals(
    fallback_signals: Sequence[Mapping[str, Any]],
    agent_signals: Mapping[str, Mapping[str, float]],
) -> List[Dict[str, Any]]:
    merged: List[Dict[str, Any]] = []
    for row in fallback_signals:
        out = dict(row)
        market_id = str(out.get("market_id", "")).strip()
        if market_id and market_id in agent_signals:
            signal = agent_signals[market_id]
            out["p_yes"] = round(float(signal["p_yes"]), 6)
            out["confidence"] = round(float(signal["confidence"]), 6)
            out["source"] = "openclaw_agent"
        else:
            out["source"] = out.get("source", "heuristic_fallback")
        merged.append(out)
    return merged


def _ps_quote_single(text: str) -> str:
    return "'" + text.replace("'", "''") + "'"


def _find_openclaw_entry(openclaw_cmd: str) -> str:
    cmd = str(openclaw_cmd or "").strip()
    executable_candidates: List[Path] = []
    if cmd:
        exe = shutil.which(cmd)
        if exe:
            executable_candidates.append(Path(exe))
        cmd_path = Path(cmd)
        if cmd_path.exists():
            executable_candidates.append(cmd_path)

    for exe_path in executable_candidates:
        candidate = exe_path.parent / "node_modules" / "openclaw" / "dist" / "index.js"
        if candidate.exists():
            return str(candidate)

    users_root = Path("/mnt/c/Users")
    if users_root.exists():
        for candidate in users_root.glob("*/AppData/Roaming/npm/node_modules/openclaw/dist/index.js"):
            if candidate.exists():
                return str(candidate)
    return ""


def _to_windows_path(path: str) -> str:
    text = str(path or "").strip()
    if text.startswith("/mnt/") and len(text) > 6:
        drive = text[5]
        rest = text[7:] if text[6] == "/" else text[6:]
        return f"{drive.upper()}:\\{rest.replace('/', '\\')}"
    return text


def _build_agent_prompt(rows: Sequence[Mapping[str, Any]]) -> str:
    lines = [
        "Return ONLY valid minified JSON array.",
        "Each item must be {\"market_id\":\"...\",\"p_yes\":0.0-1.0,\"confidence\":0.0-1.0}.",
        "No markdown, no explanation.",
        "Rows:",
    ]
    for row in rows:
        market_id = str(row.get("market_id", "")).strip()
        question = str(row.get("question", "")).strip().replace("\n", " ")
        yes_price = float(row.get("source_yes_price", 0.5) or 0.5)
        if len(question) > 160:
            question = question[:160]
        lines.append(json.dumps({"market_id": market_id, "yes_price": yes_price, "question": question}, ensure_ascii=False))
    return "\n".join(lines)


def run_openclaw_agent(
    *,
    node_exe: str,
    openclaw_entry: str,
    powershell_exe: str,
    openclaw_cmd: str,
    agent: str,
    prompt: str,
    timeout_seconds: int,
) -> Mapping[str, Any]:
    if node_exe and openclaw_entry and Path(node_exe).exists():
        entry_for_windows = _to_windows_path(openclaw_entry)
        proc = subprocess.run(
            [
                node_exe,
                entry_for_windows,
                "agent",
                "--local",
                "--agent",
                agent,
                "--json",
                "-m",
                prompt,
            ],
            capture_output=True,
            text=True,
            check=False,
            timeout=max(1, timeout_seconds),
        )
        if proc.returncode == 0:
            return extract_agent_json_from_stdout(proc.stdout)

    command = f"{openclaw_cmd} agent --local --agent {agent} --json -m {_ps_quote_single(prompt)}"
    proc2 = subprocess.run(
        [powershell_exe, "-NoProfile", "-Command", command],
        capture_output=True,
        text=True,
        check=False,
        timeout=max(1, timeout_seconds),
    )
    if proc2.returncode != 0:
        return {}
    return extract_agent_json_from_stdout(proc2.stdout)


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate signal JSONL via OpenClaw agent with heuristic fallback")
    parser.add_argument("--gamma-url", default="https://gamma-api.polymarket.com/markets")
    parser.add_argument("--limit", type=int, default=120)
    parser.add_argument("--favorite-threshold", type=float, default=0.60)
    parser.add_argument("--favorite-fade", type=float, default=0.03)
    parser.add_argument("--underdog-threshold", type=float, default=0.40)
    parser.add_argument("--underdog-lift", type=float, default=0.01)
    parser.add_argument("--question-keyword", action="append", default=[])
    parser.add_argument(
        "--powershell-exe",
        default="/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe",
        help="PowerShell executable used to run Windows OpenClaw",
    )
    parser.add_argument(
        "--node-exe",
        default="/mnt/c/Program Files/nodejs/node.exe",
        help="Windows node.exe for direct OpenClaw entry execution",
    )
    parser.add_argument(
        "--openclaw-entry",
        default="",
        help="Optional path to OpenClaw dist/index.js (auto-detected when empty)",
    )
    parser.add_argument("--openclaw-cmd", default="openclaw")
    parser.add_argument("--agent", default="main")
    parser.add_argument("--agent-market-cap", type=int, default=20)
    parser.add_argument("--timeout-seconds", type=int, default=90)
    parser.add_argument("--write-jsonl", default="")
    args = parser.parse_args()

    markets = fetch_markets(gamma_url=args.gamma_url, limit=max(1, args.limit))
    fallback = build_signals_from_markets(
        markets=markets,
        favorite_threshold=args.favorite_threshold,
        favorite_fade=args.favorite_fade,
        underdog_threshold=args.underdog_threshold,
        underdog_lift=args.underdog_lift,
        question_keywords=args.question_keyword,
    )

    cap = max(1, int(args.agent_market_cap))
    prompt_rows = fallback[:cap]
    agent_rows: Dict[str, Dict[str, float]] = {}
    if prompt_rows:
        prompt = _build_agent_prompt(prompt_rows)
        openclaw_entry = args.openclaw_entry.strip() or _find_openclaw_entry(args.openclaw_cmd.strip() or "openclaw")
        agent_json = run_openclaw_agent(
            node_exe=args.node_exe,
            openclaw_entry=openclaw_entry,
            powershell_exe=args.powershell_exe,
            openclaw_cmd=args.openclaw_cmd,
            agent=args.agent,
            prompt=prompt,
            timeout_seconds=max(1, args.timeout_seconds),
        )
        payloads = agent_json.get("payloads", []) if isinstance(agent_json, Mapping) else []
        if isinstance(payloads, list):
            texts = []
            for item in payloads:
                if isinstance(item, Mapping):
                    text = item.get("text")
                    if isinstance(text, str) and text.strip():
                        texts.append(text.strip())
            agent_rows = parse_signal_payload("\n".join(texts))

    merged = merge_agent_signals(fallback, agent_rows)
    text = render_jsonl(merged)
    print(text, end="")
    if args.write_jsonl:
        Path(args.write_jsonl).write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
