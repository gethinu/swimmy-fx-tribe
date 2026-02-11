import shutil
import sys
import tempfile
from pathlib import Path

import numpy as np


def resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "tools"))


def _make_samples(n=90, channels=5, window=120):
    rng = np.random.default_rng(42)
    samples = np.zeros((n, channels, window), dtype=np.float32)
    labels = np.zeros((n,), dtype=np.int32)
    x = np.linspace(0.0, 1.0, window, dtype=np.float32)
    for i in range(n):
        cls = i % 3
        labels[i] = cls
        if cls == 0:
            trend = x
        elif cls == 1:
            trend = -x
        else:
            trend = np.sin(np.linspace(0.0, np.pi * 3.0, window, dtype=np.float32)) * 0.2
        samples[i, 0] = trend + rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 1] = np.abs(rng.normal(0.03, 0.005, window)).astype(np.float32)
        samples[i, 2] = rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 3] = rng.normal(0.0, 0.01, window).astype(np.float32)
        samples[i, 4] = rng.normal(0.0, 0.01, window).astype(np.float32)
    return samples, labels


def _make_h1_rows(count: int = 360):
    rows = []
    ts = 1700000000
    price = 140.0
    for i in range(count):
        up_regime = (i // 18) % 2 == 0
        drift = 0.06 if up_regime else -0.07
        open_ = price
        high = open_ + (0.12 if up_regime else 0.02)
        low = open_ - (0.03 if up_regime else 0.15)
        close = open_ + drift
        volume = 100 + (i % 25)
        rows.append([ts, open_, high, low, close, volume])
        ts += 3600
        price = close
    return rows


def test_train_and_load_checkpoint():
    import pattern_vector_dl as dl

    tmp = Path(tempfile.mkdtemp(prefix="pattern_vector_dl_"))
    try:
        model_path = tmp / "vector_siamese_v1.pt"
        samples, labels = _make_samples()
        train_meta = dl.train_siamese_triplet(
            samples=samples,
            labels=labels,
            output_path=model_path,
            epochs=1,
            batch_size=16,
            max_triplets=120,
            device="cpu",
            seed=42,
        )
        assert train_meta["backend"].startswith("vector-siamese")
        assert model_path.exists()

        model, load_meta = dl.load_siamese_encoder(model_path, device="cpu")
        vec = dl.embed_sample(samples[0], model=model, device="cpu")
        assert vec.ndim == 1
        assert vec.shape[0] == int(load_meta["embedding_dim"])
        assert abs(float(np.linalg.norm(vec)) - 1.0) < 1e-4
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def test_train_from_history_writes_checkpoint_and_meta():
    import csv
    import train_pattern_vector_model as trainer

    tmp = Path(tempfile.mkdtemp(prefix="pattern_vector_train_"))
    try:
        hist_dir = tmp / "historical"
        hist_dir.mkdir(parents=True, exist_ok=True)
        with (hist_dir / "USDJPY_H1.csv").open("w", newline="", encoding="utf-8") as f:
            writer = csv.writer(f)
            writer.writerow(["timestamp", "open", "high", "low", "close", "volume"])
            writer.writerows(_make_h1_rows())

        output_path = tmp / "patterns" / "models" / "vector_siamese_v1.pt"
        summary = trainer.train_from_history(
            historical_dir=hist_dir,
            output_path=output_path,
            symbols=["USDJPY"],
            timeframes=["H1"],
            epochs=1,
            batch_size=16,
            max_samples=800,
            max_triplets=300,
            device="cpu",
            seed=13,
        )
        assert output_path.exists()
        assert Path(summary["meta_path"]).exists()
        assert str(summary["backend"]).startswith("vector-siamese")
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


if __name__ == "__main__":
    test_train_and_load_checkpoint()
    test_train_from_history_writes_checkpoint_and_meta()
    print("OK")
