#!/usr/bin/env python3
"""
inference_worker.py - Dedicated LLM Service (S-exp)
===================================================
Phase 4 (Expert Panel Approved: 2026-01-07)

Purpose:
    Offload blocking Gemini API calls from the Lisp Brain.

Protocol:
    ZMQ REQ/REP + S-expression (alist), schema_version=1
    (ZMQでJSONは受理しない)

Port:
    SWIMMY_PORT_INFERENCE (default: 5565)
"""

import os
import sys
import json
from pathlib import Path

import zmq
import requests


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
PYTHON_SRC = BASE_DIR / "src" / "python"
if str(PYTHON_SRC) not in sys.path:
    sys.path.insert(0, str(PYTHON_SRC))

from aux_sexp import parse_aux_request, sexp_response


def _env_int(key: str, default: int) -> int:
    val = os.getenv(key, "").strip()
    if not val:
        return default
    try:
        return int(val)
    except ValueError:
        return default


ZMQ_PORT = _env_int("SWIMMY_PORT_INFERENCE", 5565)
GEMINI_API_KEY = os.environ.get("SWIMMY_GEMINI_API_KEY") or os.environ.get("GEMINI_API_KEY", "")
MODEL_URL = os.environ.get(
    "SWIMMY_GEMINI_MODEL_URL",
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent",
)


def _coerce_float(value, default: float):
    try:
        if value is None:
            return default
        return float(value)
    except Exception:
        return default


def _coerce_int(value, default: int):
    try:
        if value is None:
            return default
        return int(float(value))
    except Exception:
        return default


def call_gemini_api(prompt: str, *, temp: float = 0.5, max_tokens: int = 512, api_key: str | None = None) -> dict:
    key = (api_key or "").strip() or (GEMINI_API_KEY or "").strip()
    if not key:
        return {"status": "error", "error": "MISSING_API_KEY"}

    url = f"{MODEL_URL}?key={key}"
    headers = {"Content-Type": "application/json"}
    payload = {
        "contents": [{"parts": [{"text": prompt}]}],
        "generationConfig": {"temperature": temp, "maxOutputTokens": max_tokens},
    }

    try:
        response = requests.post(url, json=payload, headers=headers, timeout=30)
        if response.status_code == 200:
            data = response.json()
            try:
                text = data["candidates"][0]["content"]["parts"][0]["text"]
                return {"status": "ok", "text": text}
            except (KeyError, IndexError):
                return {"status": "error", "error": "INVALID_API_RESPONSE"}

        # Keep response body small in error to avoid blowing ZMQ payload limits elsewhere.
        body = response.text
        if isinstance(body, str) and len(body) > 500:
            body = body[:500] + "..."
        return {"status": "error", "error": f"HTTP_{response.status_code}: {body}"}

    except requests.exceptions.Timeout:
        return {"status": "error", "error": "TIMEOUT"}
    except Exception as e:
        return {"status": "error", "error": str(e)}


def _error_response(message: str) -> str:
    return sexp_response({"type": "INFERENCE_RESULT", "status": "error", "error": message})


def handle_request_sexp(message: str) -> str:
    try:
        data = parse_aux_request(message)
    except Exception as e:
        return _error_response(str(e))

    msg_type = str(data.get("type", "")).upper()
    if msg_type != "INFERENCE":
        return _error_response(f"Invalid type: {msg_type}")

    schema_version = data.get("schema_version")
    try:
        schema_version = int(float(schema_version))
    except Exception:
        schema_version = None
    if schema_version != 1:
        return _error_response("Unsupported schema_version")

    action = str(data.get("action", "")).upper()
    if not action:
        return _error_response("Missing action")

    if action == "STATUS":
        return sexp_response(
            {
                "type": "INFERENCE_RESULT",
                "status": "ok",
                "model_url": MODEL_URL,
                "key_present": bool((GEMINI_API_KEY or "").strip()),
            }
        )

    if action == "ASK":
        prompt = data.get("prompt")
        if not prompt:
            return _error_response("Missing prompt")
        prompt = str(prompt)
        temp = _coerce_float(data.get("temperature"), 0.5)
        max_tokens = _coerce_int(data.get("max_tokens"), 512)
        api_key = data.get("api_key")
        api_key = str(api_key) if api_key else None

        result = call_gemini_api(prompt, temp=temp, max_tokens=max_tokens, api_key=api_key)
        if result.get("status") != "ok":
            return _error_response(str(result.get("error", "ERROR")))
        return sexp_response({"type": "INFERENCE_RESULT", "status": "ok", "text": str(result.get("text", ""))})

    return _error_response(f"Unknown action: {action}")


def run_server() -> None:
    print("🧠 Swimmy Inference Worker (S-exp, REQ/REP)")
    print("==========================================")
    print(f"[INFERENCE] Port: {ZMQ_PORT}")
    print(f"[INFERENCE] Key Present: {'Yes' if (GEMINI_API_KEY or '').strip() else 'NO'}")

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[INFERENCE] Listening on port {ZMQ_PORT} (REP)")

    while True:
        try:
            msg_str = socket.recv_string()
            resp = handle_request_sexp(msg_str)
            socket.send_string(resp)
        except KeyboardInterrupt:
            print("\n[INFERENCE] Shutting down...")
            break
        except Exception as e:
            try:
                socket.send_string(_error_response(f"Unhandled error: {e}"))
            except Exception:
                pass

    socket.close()
    context.term()


if __name__ == "__main__":
    run_server()

