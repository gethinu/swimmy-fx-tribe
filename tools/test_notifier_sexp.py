import sys
from pathlib import Path


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))
sys.path.insert(0, str(BASE_DIR / "tools"))

import notifier


def main():
    msg = (
        '((type . "NOTIFIER") (schema_version . 1) (action . "SEND") '
        '(webhook . "https://discord.com/api/webhooks/dummy") '
        '(payload_json . "{\\\"embeds\\\":[{\\\"title\\\":\\\"Swimmy\\\"}] }"))'
    )
    webhook, payload = notifier.parse_notifier_message(msg)
    assert webhook.endswith("/dummy")
    assert payload["embeds"][0]["title"] == "Swimmy"

    msg_payload = (
        '((type . "NOTIFIER") (schema_version . 1) (action . "SEND") '
        '(webhook . "https://discord.com/api/webhooks/dummy") '
        '(payload . ((embeds . (((title . "Swimmy")))))))'
    )
    webhook, payload = notifier.parse_notifier_message(msg_payload)
    assert payload["embeds"][0]["title"] == "Swimmy"

    try:
        notifier.parse_notifier_message(
            '((type . "NOTIFIER") (schema_version . 1) (action . "SEND") '
            '(webhook . "https://discord.com/api/webhooks/dummy") '
            '(payload_json . "{}") (payload . ((embeds . ()))) )'
        )
    except Exception:
        pass
    else:
        raise AssertionError("expected conflict error")

    try:
        notifier.parse_notifier_message('((schema_version . 1))')
    except Exception:
        pass
    else:
        raise AssertionError("expected parse error")


if __name__ == "__main__":
    main()
    print("OK")
