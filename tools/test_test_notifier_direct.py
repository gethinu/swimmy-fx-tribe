#!/usr/bin/env python3
import importlib.util
import os
import sys
import tempfile
from pathlib import Path

root = Path(__file__).resolve().parent.parent
module_path = root / "tools" / "test_notifier_direct.py"

def load_module(name: str):
    spec = importlib.util.spec_from_file_location(name, module_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

os.environ["SWIMMY_DISCORD_RECRUIT"] = "https://example.com/webhook"
os.environ.pop("SWIMMY_HOME", None)

module = load_module("test_notifier_direct_env")
webhook = module.get_webhook()
if webhook != "https://example.com/webhook":
    print(f"Expected env webhook, got: {webhook}", file=sys.stderr)
    sys.exit(1)

os.environ.pop("SWIMMY_DISCORD_RECRUIT", None)

with tempfile.TemporaryDirectory() as tmp_dir:
    env_path = Path(tmp_dir) / ".env"
    env_path.write_text('SWIMMY_DISCORD_RECRUIT="https://from-env"\n')
    os.environ["SWIMMY_HOME"] = tmp_dir
    module = load_module("test_notifier_direct_envfile")
    webhook = module.get_webhook()
    if webhook != "https://from-env":
        print(f"Expected .env webhook, got: {webhook}", file=sys.stderr)
        sys.exit(1)

os.environ.pop("SWIMMY_HOME", None)

print("OK")
