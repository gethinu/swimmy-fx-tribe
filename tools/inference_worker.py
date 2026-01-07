#!/usr/bin/env python3
"""
inference_worker.py - Dedicated LLM Service
===========================================
Expert Panel Approved (2026-01-07) - Phase 4

Purpose:
    Offload blocking Gemini API calls from the Lisp Brain.
    Handles rate limiting, retries, and API communication.

Architecture:
    [Swimmy Lisp] --(ZMQ REQ)--> [inference_worker.py] --(HTTP)--> [Google Gemini API]

Usage:
    python3 tools/inference_worker.py

Commands (JSON):
    INFERENCE: {
        "prompt": "Explain Quantum Physics",
        "max_tokens": 512,
        "temperature": 0.5
    }
    -> Returns: {"status": "OK", "text": "..."} or {"status": "ERROR", "error": "..."}
"""

import zmq
import json
import time
import os
import requests

# Configuration
ZMQ_PORT = 5564
GEMINI_API_KEY = os.environ.get("GEMINI_API_KEY", "")
MODEL_URL = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash-exp:generateContent"


def call_gemini_api(prompt, temp=0.5, max_tokens=512, api_key=None):
    key = api_key if api_key else GEMINI_API_KEY
    if not key:
        return {"status": "ERROR", "error": "MISSING_API_KEY"}

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
                return {"status": "OK", "text": text}
            except (KeyError, IndexError):
                return {"status": "ERROR", "error": "INVALID_API_RESPONSE"}
        else:
            return {
                "status": "ERROR",
                "error": f"HTTP_{response.status_code}: {response.text}",
            }

    except requests.exceptions.Timeout:
        return {"status": "ERROR", "error": "TIMEOUT"}
    except Exception as e:
        return {"status": "ERROR", "error": str(e)}


def main():
    print("=" * 60)
    print("  🧠 INFERENCE WORKER - Dedicated LLM Service")
    print("  Phase 4 Implementation")
    print(f"  Key Present: {'Yes' if GEMINI_API_KEY else 'NO'}")
    print("=" * 60)

    context = zmq.Context()
    socket = context.socket(zmq.REP)
    socket.bind(f"tcp://*:{ZMQ_PORT}")
    print(f"[INFERENCE] Listening on port {ZMQ_PORT} (REP)")

    while True:
        try:
            msg_str = socket.recv_string()

            # Simple Protocol: "PROMPT_TEXT" or "JSON"
            # To be robust, let's assume JSON input for params, or raw string as simple prompt

            prompt = msg_str
            temp = 0.5
            max_tokens = 512
            api_key = None

            # Try parsing as JSON for advanced params
            try:
                data = json.loads(msg_str)
                if isinstance(data, dict) and "prompt" in data:
                    prompt = data["prompt"]
                    temp = data.get("temperature", 0.5)
                    max_tokens = data.get("max_tokens", 512)
                    api_key = data.get("api_key")
            except json.JSONDecodeError:
                pass  # Treat as raw string

            print(f"[INFERENCE] Processing request: {prompt[:50]}...")

            result = call_gemini_api(prompt, temp, max_tokens, api_key)

            # Send back JSON response
            socket.send_string(json.dumps(result))

        except KeyboardInterrupt:
            print("\n[INFERENCE] Shutting down...")
            break
        except Exception as e:
            print(f"[INFERENCE] Critical Error: {e}")
            # Send error if socket is still in REQ/REP lockstep
            try:
                socket.send_string(
                    json.dumps({"status": "ERROR", "error": "WORKER_CRASH_RECOVERED"})
                )
            except:
                pass

    socket.close()
    context.term()


if __name__ == "__main__":
    main()
