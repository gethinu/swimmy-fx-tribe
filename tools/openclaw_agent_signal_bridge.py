#!/usr/bin/env python3
"""Generate OpenClaw-style signals via OpenClaw agent --local output."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, Tuple


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

FATAL_AGENT_TEXT_PATTERNS = (
    "api error (429)",
    "rate limit",
    "exhausted your capacity",
    "quota will reset",
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


def extract_agent_signals(agent_json: Mapping[str, Any]) -> Dict[str, Dict[str, float]]:
    payloads = agent_json.get("payloads", []) if isinstance(agent_json, Mapping) else []
    if not isinstance(payloads, list):
        return {}
    texts: List[str] = []
    for item in payloads:
        if not isinstance(item, Mapping):
            continue
        text = item.get("text")
        if isinstance(text, str) and text.strip():
            texts.append(text.strip())
    if not texts:
        return {}
    return parse_signal_payload("\n".join(texts))


def extract_agent_fatal_error(agent_json: Mapping[str, Any]) -> str:
    payloads = agent_json.get("payloads", []) if isinstance(agent_json, Mapping) else []
    if not isinstance(payloads, list):
        return ""
    for item in payloads:
        if not isinstance(item, Mapping):
            continue
        text = item.get("text")
        if not isinstance(text, str):
            continue
        lowered = text.lower()
        if any(pattern in lowered for pattern in FATAL_AGENT_TEXT_PATTERNS):
            return text.strip()
    return ""


def collect_agent_signals_with_status(
    *,
    fetch_fn: Callable[[], Mapping[str, Any]],
    retries: int,
    retry_sleep_ms: int,
    sleep_fn: Callable[[float], None] = time.sleep,
) -> Tuple[Dict[str, Dict[str, float]], str]:
    attempts = max(1, int(retries) + 1)
    sleep_seconds = max(0, int(retry_sleep_ms)) / 1000.0
    for attempt in range(attempts):
        try:
            payload = fetch_fn()
        except subprocess.TimeoutExpired as exc:
            return {}, f"timeout: {exc}"
        except Exception:
            payload = {}
        fatal_error = extract_agent_fatal_error(payload)
        if fatal_error:
            return {}, fatal_error
        rows = extract_agent_signals(payload)
        if rows:
            return rows, ""
        if attempt < attempts - 1 and sleep_seconds > 0.0:
            sleep_fn(sleep_seconds)
    return {}, ""


def collect_agent_signals(
    *,
    fetch_fn: Callable[[], Mapping[str, Any]],
    retries: int,
    retry_sleep_ms: int,
    sleep_fn: Callable[[float], None] = time.sleep,
) -> Dict[str, Dict[str, float]]:
    rows, _fatal_error = collect_agent_signals_with_status(
        fetch_fn=fetch_fn,
        retries=retries,
        retry_sleep_ms=retry_sleep_ms,
        sleep_fn=sleep_fn,
    )
    return rows


def _parse_positive_int_csv(text: str) -> List[int]:
    out: List[int] = []
    for raw in str(text or "").split(","):
        token = raw.strip()
        if not token:
            continue
        try:
            value = int(token)
        except ValueError:
            continue
        if value > 0:
            out.append(value)
    return out


def build_agent_cap_attempts(*, primary_cap: int, fallback_caps: Sequence[int]) -> List[int]:
    cap = max(1, int(primary_cap))
    attempts: List[int] = [cap]
    seen = {cap}
    for raw in fallback_caps:
        try:
            candidate = int(raw)
        except (TypeError, ValueError):
            continue
        if candidate < 1 or candidate >= cap or candidate in seen:
            continue
        attempts.append(candidate)
        seen.add(candidate)
    return attempts


def collect_agent_signals_for_caps_with_status(
    *,
    fallback_signals: Sequence[Mapping[str, Any]],
    cap_attempts: Sequence[int],
    fetch_for_prompt: Callable[[str, int], Mapping[str, Any]],
    primary_retries: int,
    fallback_retries: int,
    retry_sleep_ms: int,
    sleep_fn: Callable[[float], None] = time.sleep,
) -> Tuple[Dict[str, Dict[str, float]], int, str]:
    selected_cap = int(cap_attempts[0]) if cap_attempts else 0
    for idx, cap in enumerate(cap_attempts):
        cap_value = max(1, int(cap))
        prompt_rows = list(fallback_signals[:cap_value])
        if not prompt_rows:
            selected_cap = cap_value
            continue
        prompt = _build_agent_prompt(prompt_rows)
        retries = max(0, primary_retries) if idx == 0 else max(0, fallback_retries)
        rows, fatal_error = collect_agent_signals_with_status(
            fetch_fn=lambda cap_arg=cap_value, prompt_arg=prompt: fetch_for_prompt(prompt_arg, cap_arg),
            retries=retries,
            retry_sleep_ms=max(0, retry_sleep_ms),
            sleep_fn=sleep_fn,
        )
        if rows:
            return rows, cap_value, ""
        if fatal_error:
            lowered = fatal_error.lower()
            if "timeout" in lowered or "timed out" in lowered:
                selected_cap = cap_value
                continue
            return {}, cap_value, fatal_error
        selected_cap = cap_value
    return {}, selected_cap, ""


def collect_agent_signals_for_caps(
    *,
    fallback_signals: Sequence[Mapping[str, Any]],
    cap_attempts: Sequence[int],
    fetch_for_prompt: Callable[[str, int], Mapping[str, Any]],
    primary_retries: int,
    fallback_retries: int,
    retry_sleep_ms: int,
    sleep_fn: Callable[[float], None] = time.sleep,
) -> Tuple[Dict[str, Dict[str, float]], int]:
    rows, selected_cap, _fatal_error = collect_agent_signals_for_caps_with_status(
        fallback_signals=fallback_signals,
        cap_attempts=cap_attempts,
        fetch_for_prompt=fetch_for_prompt,
        primary_retries=primary_retries,
        fallback_retries=fallback_retries,
        retry_sleep_ms=retry_sleep_ms,
        sleep_fn=sleep_fn,
    )
    return rows, selected_cap


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


def _resolve_windows_cmd_token(token: str, exists_fn: Optional[Callable[[Path], bool]] = None) -> str:
    text = str(token or "").strip()
    if not text:
        return text
    if not text.startswith("/mnt/"):
        return text
    path_exists = exists_fn or (lambda path: path.exists())
    base = Path(text)
    if base.suffix:
        return _to_windows_path(str(base))
    cmd_candidate = Path(str(base) + ".cmd")
    if path_exists(cmd_candidate):
        return _to_windows_path(str(cmd_candidate))
    return _to_windows_path(str(base))


def _split_openclaw_cmd_tokens(cmd_text: str) -> List[str]:
    text = str(cmd_text or "").strip()
    if not text:
        return []
    try:
        tokens = shlex.split(text)
    except ValueError:
        return [text]
    # Windows paths like C:\foo\bar are mangled by POSIX parsing. Retry with non-POSIX when detected.
    if "\\" in text and tokens:
        head = tokens[0]
        if ":" in head and "\\" not in head:
            try:
                win_tokens = shlex.split(text, posix=False)
            except ValueError:
                return tokens
            cleaned: List[str] = []
            for token in win_tokens:
                if len(token) >= 2 and token[0] == token[-1] and token[0] in {"'", '"'}:
                    cleaned.append(token[1:-1])
                else:
                    cleaned.append(token)
            return cleaned
    return tokens


def _build_powershell_agent_command(*, openclaw_cmd: str, agent: str, prompt: str) -> str:
    cmd_text = str(openclaw_cmd or "").strip()
    if cmd_text:
        base_tokens = _split_openclaw_cmd_tokens(cmd_text)
    else:
        base_tokens = ["openclaw"]
    if not base_tokens:
        base_tokens = ["openclaw"]
    base_tokens[0] = _resolve_windows_cmd_token(base_tokens[0])
    tokens = base_tokens + ["agent", "--local", "--agent", str(agent), "--json", "-m", str(prompt)]
    return "& " + " ".join(_ps_quote_single(token) for token in tokens)


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

    command = _build_powershell_agent_command(openclaw_cmd=openclaw_cmd, agent=agent, prompt=prompt)
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
    parser.add_argument(
        "--agent-market-cap-fallbacks",
        default="12,8,5",
        help="Comma-separated fallback caps when primary cap yields no agent rows",
    )
    parser.add_argument("--agent-retries", type=int, default=2)
    parser.add_argument("--agent-fallback-retries", type=int, default=0)
    parser.add_argument("--agent-retry-sleep-ms", type=int, default=300)
    parser.add_argument("--timeout-seconds", type=int, default=12)
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

    agent_rows: Dict[str, Dict[str, float]] = {}
    primary_cap = max(1, int(args.agent_market_cap))
    cap_fallbacks = _parse_positive_int_csv(args.agent_market_cap_fallbacks)
    cap_attempts = build_agent_cap_attempts(primary_cap=primary_cap, fallback_caps=cap_fallbacks)
    if fallback:
        openclaw_entry = args.openclaw_entry.strip() or _find_openclaw_entry(args.openclaw_cmd.strip() or "openclaw")
        agent_rows, _selected_cap, fatal_error = collect_agent_signals_for_caps_with_status(
            fallback_signals=fallback,
            cap_attempts=cap_attempts,
            fetch_for_prompt=lambda prompt_arg, _cap_arg: run_openclaw_agent(
                node_exe=args.node_exe,
                openclaw_entry=openclaw_entry,
                powershell_exe=args.powershell_exe,
                openclaw_cmd=args.openclaw_cmd,
                agent=args.agent,
                prompt=prompt_arg,
                timeout_seconds=max(1, args.timeout_seconds),
            ),
            primary_retries=max(0, args.agent_retries),
            fallback_retries=max(0, args.agent_fallback_retries),
            retry_sleep_ms=max(0, args.agent_retry_sleep_ms),
        )
        if fatal_error:
            print(f"[openclaw_agent_signal_bridge] fatal agent response: {fatal_error}", file=sys.stderr)

    merged = merge_agent_signals(fallback, agent_rows)
    text = render_jsonl(merged)
    print(text, end="")
    if args.write_jsonl:
        Path(args.write_jsonl).write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
