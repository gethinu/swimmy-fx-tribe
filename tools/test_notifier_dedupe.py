import sys
from pathlib import Path


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "tools"))

import notifier


def main():
    payload = {
        "embeds": [
            {
                "title": "⚖️ 非相関 昇格通知",
                "description": "【理由】OOS+Stage2 validated: Sharpe=1.78 戦略: Bred-Bred--241-Gen518",
                "color": 3447003,
            }
        ]
    }

    original_window = notifier.DEDUPE_WINDOW_SEC
    try:
        notifier.DEDUPE_WINDOW_SEC = 10
        notifier._dedupe_cache.clear()

        assert notifier.should_enqueue_message("https://discord.example/webhook/A", payload, now=1000)
        assert not notifier.should_enqueue_message("https://discord.example/webhook/A", payload, now=1005)
        assert notifier.should_enqueue_message("https://discord.example/webhook/A", payload, now=1011)
        assert notifier.should_enqueue_message("https://discord.example/webhook/B", payload, now=1012)
        assert notifier.should_enqueue_message(
            "https://discord.example/webhook/A",
            {
                "embeds": [
                    {
                        "title": "⚖️ 非相関 昇格通知",
                        "description": "別戦略",
                        "color": 3447003,
                    }
                ]
            },
            now=1013,
        )

        notifier.DEDUPE_WINDOW_SEC = 0
        notifier._dedupe_cache.clear()
        assert notifier.should_enqueue_message("https://discord.example/webhook/A", payload, now=2000)
        assert notifier.should_enqueue_message("https://discord.example/webhook/A", payload, now=2001)
    finally:
        notifier.DEDUPE_WINDOW_SEC = original_window
        notifier._dedupe_cache.clear()


if __name__ == "__main__":
    main()
    print("OK")
