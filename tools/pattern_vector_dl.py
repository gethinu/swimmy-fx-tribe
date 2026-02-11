#!/usr/bin/env python3
"""
Pattern Vector Deep Learning Utilities (Phase 2)
================================================
- Siamese encoder (Triplet loss)
- Candle -> feature tensor conversion
- Checkpoint save/load + embedding helpers
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, Optional, Tuple

import numpy as np

try:
    import torch
    import torch.nn as nn
    import torch.nn.functional as F
except Exception:  # pragma: no cover - guarded at runtime
    torch = None
    nn = None
    F = None


BACKEND_NAME = "vector-siamese-v1"


def _require_torch() -> None:
    if torch is None or nn is None or F is None:
        raise RuntimeError("PyTorch is required for vector DL backend")


def _resample(series: np.ndarray, out_len: int) -> np.ndarray:
    if len(series) == out_len:
        return series.astype(np.float32)
    if len(series) <= 1:
        return np.zeros(out_len, dtype=np.float32)
    x_old = np.linspace(0.0, 1.0, len(series), dtype=np.float64)
    x_new = np.linspace(0.0, 1.0, out_len, dtype=np.float64)
    return np.interp(x_new, x_old, series).astype(np.float32)


def candles_to_feature_channels(candles: list[dict], out_len: int = 120) -> np.ndarray:
    """
    Convert candles into 5 x out_len channels:
    [close_norm, spread, body, volume_norm, log_return]
    """
    if not candles:
        return np.zeros((5, out_len), dtype=np.float32)

    close = np.array([float(c["close"]) for c in candles], dtype=np.float64)
    open_ = np.array([float(c["open"]) for c in candles], dtype=np.float64)
    high = np.array([float(c["high"]) for c in candles], dtype=np.float64)
    low = np.array([float(c["low"]) for c in candles], dtype=np.float64)
    volume = np.array([float(c.get("volume", 0.0)) for c in candles], dtype=np.float64)

    base = max(abs(close[0]), 1e-9)
    close_norm = (close / base) - 1.0
    spread = (high - low) / np.maximum(np.abs(close), 1e-9)
    body = (close - open_) / np.maximum(np.abs(close), 1e-9)

    if np.std(volume) > 1e-9:
        volume_norm = (volume - np.mean(volume)) / np.std(volume)
    else:
        volume_norm = np.zeros_like(volume)

    rets = np.diff(np.log(np.maximum(close, 1e-9)))

    features = np.vstack(
        [
            _resample(close_norm, out_len),
            _resample(spread, out_len),
            _resample(body, out_len),
            _resample(volume_norm, out_len),
            _resample(rets, out_len),
        ]
    ).astype(np.float32)
    return features


class SiameseEncoder(nn.Module):
    def __init__(self, input_channels: int = 5, embedding_dim: int = 128):
        super().__init__()
        self.net = nn.Sequential(
            nn.Conv1d(input_channels, 32, kernel_size=5, padding=2),
            nn.ReLU(inplace=True),
            nn.Conv1d(32, 64, kernel_size=5, padding=2),
            nn.ReLU(inplace=True),
            nn.Conv1d(64, 64, kernel_size=3, padding=1),
            nn.ReLU(inplace=True),
            nn.AdaptiveAvgPool1d(1),
        )
        self.head = nn.Linear(64, embedding_dim)

    def forward(self, x):  # type: ignore[override]
        z = self.net(x)
        z = z.squeeze(-1)
        z = self.head(z)
        return F.normalize(z, p=2, dim=1)


def _resolve_device(device: Optional[str] = None) -> str:
    _require_torch()
    if device and device != "auto":
        if device == "cuda" and not torch.cuda.is_available():
            return "cpu"
        return device
    return "cuda" if torch.cuda.is_available() else "cpu"


def _build_triplets(labels: np.ndarray, max_triplets: int, seed: int) -> np.ndarray:
    label_to_idx: Dict[int, np.ndarray] = {}
    for label in np.unique(labels):
        label_to_idx[int(label)] = np.where(labels == label)[0]

    valid_anchor_labels = [
        label for label, idxs in label_to_idx.items() if len(idxs) >= 2
    ]
    if len(valid_anchor_labels) < 1 or len(label_to_idx) < 2:
        return np.zeros((0, 3), dtype=np.int64)

    rng = np.random.default_rng(seed)
    triplets = []
    max_attempts = max(max_triplets * 30, 1000)
    attempts = 0

    while len(triplets) < max_triplets and attempts < max_attempts:
        attempts += 1
        anchor_label = int(rng.choice(valid_anchor_labels))
        negative_candidates = [k for k in label_to_idx.keys() if k != anchor_label]
        if not negative_candidates:
            continue
        negative_label = int(rng.choice(negative_candidates))
        anchor_pos = rng.choice(label_to_idx[anchor_label], size=2, replace=False)
        negative = int(rng.choice(label_to_idx[negative_label]))
        triplets.append((int(anchor_pos[0]), int(anchor_pos[1]), negative))

    if not triplets:
        return np.zeros((0, 3), dtype=np.int64)
    return np.array(triplets, dtype=np.int64)


def train_siamese_triplet(
    samples: np.ndarray,
    labels: np.ndarray,
    output_path: Path,
    *,
    epochs: int = 3,
    batch_size: int = 64,
    lr: float = 1e-3,
    margin: float = 0.2,
    max_triplets: int = 20_000,
    embedding_dim: int = 128,
    device: Optional[str] = None,
    seed: int = 42,
) -> dict:
    """
    Train Siamese encoder with triplet loss.

    samples: [N, C, L]
    labels : [N]
    """
    _require_torch()

    if samples.ndim != 3:
        raise ValueError("samples must be 3D [N, C, L]")
    if labels.ndim != 1:
        raise ValueError("labels must be 1D [N]")
    if len(samples) != len(labels):
        raise ValueError("samples/labels length mismatch")
    if len(samples) < 10:
        raise ValueError("insufficient samples")

    resolved_device = _resolve_device(device)

    triplets = _build_triplets(labels.astype(np.int64), max_triplets=max_triplets, seed=seed)
    if len(triplets) == 0:
        raise ValueError("unable to build valid triplets from labels")

    model = SiameseEncoder(
        input_channels=int(samples.shape[1]),
        embedding_dim=int(embedding_dim),
    ).to(resolved_device)
    model.train()

    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    criterion = nn.TripletMarginLoss(margin=float(margin), p=2)

    rng = np.random.default_rng(seed)
    losses = []

    for _epoch in range(max(1, int(epochs))):
        order = rng.permutation(len(triplets))
        epoch_loss = 0.0
        steps = 0
        for start in range(0, len(order), max(1, int(batch_size))):
            chunk = order[start : start + max(1, int(batch_size))]
            tri = triplets[chunk]
            a_idx, p_idx, n_idx = tri[:, 0], tri[:, 1], tri[:, 2]

            anchor = torch.from_numpy(samples[a_idx]).to(resolved_device)
            positive = torch.from_numpy(samples[p_idx]).to(resolved_device)
            negative = torch.from_numpy(samples[n_idx]).to(resolved_device)

            optimizer.zero_grad(set_to_none=True)
            emb_a = model(anchor)
            emb_p = model(positive)
            emb_n = model(negative)
            loss = criterion(emb_a, emb_p, emb_n)
            loss.backward()
            optimizer.step()

            epoch_loss += float(loss.item())
            steps += 1

        losses.append(epoch_loss / max(steps, 1))

    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    checkpoint = {
        "backend": BACKEND_NAME,
        "channels": int(samples.shape[1]),
        "window": int(samples.shape[2]),
        "embedding_dim": int(embedding_dim),
        "state_dict": model.state_dict(),
        "train": {
            "epochs": int(max(1, int(epochs))),
            "batch_size": int(max(1, int(batch_size))),
            "lr": float(lr),
            "margin": float(margin),
            "triplets": int(len(triplets)),
            "losses": [float(x) for x in losses],
            "device": resolved_device,
        },
    }
    torch.save(checkpoint, output_path)

    return {
        "backend": BACKEND_NAME,
        "path": str(output_path),
        "samples": int(len(samples)),
        "triplets": int(len(triplets)),
        "embedding_dim": int(embedding_dim),
        "window": int(samples.shape[2]),
        "channels": int(samples.shape[1]),
        "device": resolved_device,
        "last_loss": float(losses[-1]) if losses else 0.0,
    }


def load_siamese_encoder(
    checkpoint_path: Path,
    *,
    device: Optional[str] = None,
) -> Tuple[SiameseEncoder, dict]:
    _require_torch()

    resolved_device = _resolve_device(device)
    payload = torch.load(
        Path(checkpoint_path),
        map_location=resolved_device,
    )

    backend = str(payload.get("backend", ""))
    if not backend.startswith("vector-siamese"):
        raise ValueError(f"unsupported backend in checkpoint: {backend}")

    channels = int(payload["channels"])
    embedding_dim = int(payload["embedding_dim"])

    model = SiameseEncoder(
        input_channels=channels,
        embedding_dim=embedding_dim,
    ).to(resolved_device)
    model.load_state_dict(payload["state_dict"], strict=True)
    model.eval()

    meta = {
        "backend": backend,
        "channels": channels,
        "window": int(payload["window"]),
        "embedding_dim": embedding_dim,
        "device": resolved_device,
    }
    return model, meta


def embed_sample(
    sample: np.ndarray,
    *,
    model: SiameseEncoder,
    device: Optional[str] = None,
) -> np.ndarray:
    _require_torch()
    resolved_device = _resolve_device(device)
    if sample.ndim != 2:
        raise ValueError("sample must be 2D [C, L]")
    x = torch.from_numpy(sample.astype(np.float32)).unsqueeze(0).to(resolved_device)
    model.eval()
    with torch.no_grad():
        vec = model(x).squeeze(0).detach().cpu().numpy().astype(np.float32)
    return vec


def embed_candles(
    candles: list[dict],
    *,
    model: SiameseEncoder,
    window: int,
    device: Optional[str] = None,
) -> np.ndarray:
    sample = candles_to_feature_channels(candles, out_len=int(window))
    return embed_sample(sample, model=model, device=device)
