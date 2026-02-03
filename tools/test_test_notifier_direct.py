#!/usr/bin/env python3
import os
import importlib.util
from pathlib import Path
import sys

root = Path(__file__).resolve().parent.parent
module_path = root / "tools" / "test_notifier_direct.py"

spec = importlib.util.spec_from_file_location("test_notifier_direct", module_path)
module = importlib.util.module_from_spec(spec)

os.environ["SWIMMY_DISCORD_RECRUIT"] = "https://example.com/webhook"

spec.loader.exec_module(module)

webhook = module.get_webhook()
if webhook != "https://example.com/webhook":
    print(f"Expected env webhook, got: {webhook}", file=sys.stderr)
    sys.exit(1)

print("OK")
