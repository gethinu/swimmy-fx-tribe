#!/usr/bin/env python3
"""
Train Pattern Similarity vector deep model (Siamese/Triplet).
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Iterable, List, Tuple

import numpy as np


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
if str(BASE_DIR / "tools") not in sys.path:
    sys.path.insert(0, str(BASE_DIR / "tools"))

import pattern_similarity_service as svc
import pattern_vector_dl as dl


def _coerce_tf_list(raw: Iterable[str]) -> List[str]:
    out = []
    for tf in raw:
        tf_norm = svc._normalize_tf(tf)  # pylint: disable=protected-access
        if tf_norm:
            out.append(tf_norm)
    return out


def build_training_samples(
    *,
    historical_dir: Path,
    symbols: Iterable[str],
    timeframes: Iterable[str],
    start_time: int | None = None,
    end_time: int | None = None,
    target_window: int = 120,
    max_samples: int = 60_000,
    seed: int = 42,
) -> Tuple[np.ndarray, np.ndarray, dict]:
    svc.HISTORICAL_DIR = Path(historical_dir)

    samples: list[np.ndarray] = []
    labels: list[int] = []
    stats = {
        "symbols": [],
        "timeframes": [],
        "collected": 0,
    }

    for symbol in symbols:
        symbol_norm = svc._normalize_symbol(symbol)  # pylint: disable=protected-access
        if not symbol_norm:
            continue
        if symbol_norm not in stats["symbols"]:
            stats["symbols"].append(symbol_norm)

        for tf in _coerce_tf_list(timeframes):
            if tf not in stats["timeframes"]:
                stats["timeframes"].append(tf)

            candles = svc._load_candles(  # pylint: disable=protected-access
                symbol_norm,
                tf,
                start_time=start_time,
                end_time=end_time,
            )
            window = svc.WINDOW_BARS[tf]
            stride = svc.STRIDE_BARS[tf]
            horizon = svc.HORIZON_BARS[tf]
            max_i = len(candles) - horizon

            if len(candles) < (window + horizon):
                continue

            for i in range(window - 1, max_i, stride):
                win = candles[i - window + 1 : i + 1]
                fut = candles[i + 1 : i + 1 + horizon]
                sample = dl.candles_to_feature_channels(win, out_len=target_window)
                label = svc._label_window(win, fut)  # pylint: disable=protected-access
                samples.append(sample)
                labels.append(int(label))

    if not samples:
        raise RuntimeError("No training samples built from historical data")

    x = np.stack(samples).astype(np.float32)
    y = np.array(labels, dtype=np.int64)

    if len(x) > max_samples:
        rng = np.random.default_rng(seed)
        picks = rng.choice(len(x), size=int(max_samples), replace=False)
        x = x[picks]
        y = y[picks]

    stats["collected"] = int(len(x))
    label_counts = {}
    for v in y.tolist():
        label_counts[str(v)] = label_counts.get(str(v), 0) + 1
    stats["label_counts"] = label_counts
    return x, y, stats


def train_from_history(
    *,
    historical_dir: Path,
    output_path: Path,
    symbols: Iterable[str],
    timeframes: Iterable[str],
    start_time: int | None = None,
    end_time: int | None = None,
    epochs: int = 4,
    batch_size: int = 64,
    lr: float = 1e-3,
    margin: float = 0.2,
    embedding_dim: int = 128,
    max_samples: int = 60_000,
    max_triplets: int = 30_000,
    device: str = "auto",
    seed: int = 42,
) -> dict:
    x, y, data_stats = build_training_samples(
        historical_dir=historical_dir,
        symbols=symbols,
        timeframes=timeframes,
        start_time=start_time,
        end_time=end_time,
        target_window=120,
        max_samples=max_samples,
        seed=seed,
    )

    train_meta = dl.train_siamese_triplet(
        samples=x,
        labels=y,
        output_path=output_path,
        epochs=epochs,
        batch_size=batch_size,
        lr=lr,
        margin=margin,
        max_triplets=max_triplets,
        embedding_dim=embedding_dim,
        device=device,
        seed=seed,
    )

    summary = {
        "backend": train_meta["backend"],
        "output_path": str(output_path),
        "data": data_stats,
        "train": train_meta,
    }

    meta_path = output_path.with_suffix(".meta.json")
    meta_path.parent.mkdir(parents=True, exist_ok=True)
    with meta_path.open("w", encoding="utf-8") as f:
        json.dump(summary, f, ensure_ascii=False, indent=2)
    summary["meta_path"] = str(meta_path)
    return summary


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Train pattern vector deep model")
    parser.add_argument(
        "--historical-dir",
        default=str(BASE_DIR / "data" / "historical"),
        help="CSV historical directory",
    )
    parser.add_argument(
        "--output",
        default=str(BASE_DIR / "data" / "patterns" / "models" / "vector_siamese_v1.pt"),
        help="Output checkpoint path",
    )
    parser.add_argument(
        "--symbols",
        default="USDJPY,EURUSD,GBPUSD",
        help="Comma separated symbols",
    )
    parser.add_argument(
        "--timeframes",
        default="M5,M15,H1,H4,D1,W1,MN1",
        help="Comma separated timeframes",
    )
    parser.add_argument("--start-time", type=int, default=None)
    parser.add_argument("--end-time", type=int, default=None)
    parser.add_argument("--epochs", type=int, default=4)
    parser.add_argument("--batch-size", type=int, default=64)
    parser.add_argument("--lr", type=float, default=1e-3)
    parser.add_argument("--margin", type=float, default=0.2)
    parser.add_argument("--embedding-dim", type=int, default=128)
    parser.add_argument("--max-samples", type=int, default=60000)
    parser.add_argument("--max-triplets", type=int, default=30000)
    parser.add_argument("--device", default="auto", choices=["auto", "cpu", "cuda"])
    parser.add_argument("--seed", type=int, default=42)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    symbols = [x.strip().upper() for x in str(args.symbols).split(",") if x.strip()]
    timeframes = [x.strip().upper() for x in str(args.timeframes).split(",") if x.strip()]

    summary = train_from_history(
        historical_dir=Path(args.historical_dir),
        output_path=Path(args.output),
        symbols=symbols,
        timeframes=timeframes,
        start_time=args.start_time,
        end_time=args.end_time,
        epochs=args.epochs,
        batch_size=args.batch_size,
        lr=args.lr,
        margin=args.margin,
        embedding_dim=args.embedding_dim,
        max_samples=args.max_samples,
        max_triplets=args.max_triplets,
        device=args.device,
        seed=args.seed,
    )
    print(json.dumps(summary, ensure_ascii=False))


if __name__ == "__main__":
    main()
