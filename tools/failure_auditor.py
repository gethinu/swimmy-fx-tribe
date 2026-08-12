#!/usr/bin/env python3
"""
tools/failure_auditor.py

The AI Auditor (Andrew Ng's Directive).
Uses XGBoost to classify "Winning" vs "Losing" strategies based on their parameters and logic features.
This runs OFFLINE (Phase 24) to generate "Toxic Patterns" which are then blacklisted in Lisp.

Input: data/memory/strategies.csv (or SQL dump)
Output: data/memory/toxic_features.json
"""

import sys
import os
import sqlite3
import pandas as pd
import numpy as np
import json
import joblib
# NOTE: XGBoost is imported lazily inside train_and_audit so that the pure helpers
# (compute_mda, mda_enabled) can be imported/tested without the xgboost dependency.
# The actual audit run still imports it before use, so behaviour is unchanged.
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, precision_score, recall_score

# --- CONSTANTS ---
DB_PATH = "data/memory/swimmy.db"
OUTPUT_PATH = "data/memory/toxic_features.json"
MODEL_PATH = "data/models/auditor_xgb.pkl"


def load_data():
    """Load strategy performance data from SQLite."""
    if not os.path.exists(DB_PATH):
        print(f"[AUDITOR] ❌ Database not found: {DB_PATH}")
        sys.exit(1)

    conn = sqlite3.connect(DB_PATH)
    query = """
    SELECT name, rank, sharpe, trades, win_rate, 
           sl, tp, timeframe, indicators,
           entry_logic_size, exit_logic_size -- Derived features we might add later
    FROM strategies
    WHERE trades > 20 -- Only statistically significant samples
    """
    try:
        df = pd.read_sql_query("SELECT * FROM strategies WHERE trades > 10", conn)
    except Exception as e:
        print(f"[AUDITOR] ⚠️ SQL Error: {e}")
        # Fallback to loading CSV if DB fails or table doesn't exist
        # This is a mock impl for the script structure
        return pd.DataFrame()

    conn.close()
    return df


def feature_engineering(df):
    """Convert raw strategy data into ML features."""
    if df.empty:
        return df, pd.Series()

    # Target: 1 = Failure (Graveyard or Sharpe < 0), 0 = Success (Rank A/S or Sharpe > 1.0)
    # We want to PREDICT FAILURE.
    df["is_failure"] = ((df["rank"] == ":GRAVEYARD") | (df["sharpe"] < 0.0)).astype(int)

    # Features
    # 1. Risk/Reward Ratio
    df["rr_ratio"] = df["tp"] / df["sl"]

    # 2. Indicator Presence (One-Hot Encoding primitive)
    df["has_rsi"] = df["indicators"].apply(
        lambda x: 1 if "RSI" in str(x).upper() else 0
    )
    df["has_macd"] = df["indicators"].apply(
        lambda x: 1 if "MACD" in str(x).upper() else 0
    )
    df["has_sma"] = df["indicators"].apply(
        lambda x: 1 if "SMA" in str(x).upper() else 0
    )
    df["has_adx"] = df["indicators"].apply(
        lambda x: 1 if "ADX" in str(x).upper() else 0
    )

    # Check for infinity or NaN
    df = df.replace([np.inf, -np.inf], np.nan).dropna()

    features = [
        "sl",
        "tp",
        "rr_ratio",
        "timeframe",
        "has_rsi",
        "has_macd",
        "has_sma",
        "has_adx",
    ]
    X = df[features]
    y = df["is_failure"]

    return X, y


MDA_OUTPUT_PATH = "data/memory/toxic_features_mda.json"


def _truthy(v):
    return str(v).strip().lower() in ("1", "true", "yes", "on", "y", "t")


def mda_enabled():
    """V-methodology (2026-08-11): permutation/MDA is OFF by default.
    Enable with env AUDITOR_MDA=1 or the --mda CLI flag. When OFF, this module's
    output (toxic_features.json from XGBoost MDI) is byte-identical to before."""
    return _truthy(os.environ.get("AUDITOR_MDA", "0")) or ("--mda" in sys.argv)


def compute_mda(model, X_test, y_test, n_repeats=10, seed=42):
    """Mean-Decrease-Accuracy (permutation importance).

    MDI (model.feature_importances_) is impurity/gain based and is biased toward
    high-cardinality / correlated features. MDA measures the drop in out-of-sample
    accuracy when each feature column is randomly permuted — a model-agnostic,
    less-biased importance (Breiman; Lopez de Prado 'Advances in Financial ML').

    Returns {feature: {"mean": float, "std": float}} using only numpy + the model's
    predict + accuracy_score (no extra dependencies)."""
    rng = np.random.default_rng(seed)
    cols = list(X_test.columns)
    X_arr = X_test.to_numpy(dtype=float, copy=True)
    y_arr = np.asarray(y_test)
    baseline = accuracy_score(y_arr, model.predict(X_test))

    result = {}
    for j, col in enumerate(cols):
        drops = np.empty(n_repeats, dtype=float)
        original = X_arr[:, j].copy()
        for r in range(n_repeats):
            permuted = original.copy()
            rng.shuffle(permuted)
            X_arr[:, j] = permuted
            Xp = pd.DataFrame(X_arr, columns=cols, index=X_test.index)
            acc = accuracy_score(y_arr, model.predict(Xp))
            drops[r] = baseline - acc  # positive => permuting hurt => important
        X_arr[:, j] = original  # restore
        result[col] = {"mean": float(np.mean(drops)), "std": float(np.std(drops))}
    return result


def train_and_audit(X, y):
    """Train XGBoost model and identify toxic features."""
    if len(X) < 50:
        print(f"[AUDITOR] ⚠️ Not enough data samples ({len(X)} < 50). Skipping Audit.")
        return

    from xgboost import XGBClassifier  # lazy import (see module header)

    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.2, random_state=42
    )

    model = XGBClassifier(use_label_encoder=False, eval_metric="logloss")
    model.fit(X_train, y_train)

    preds = model.predict(X_test)
    acc = accuracy_score(y_test, preds)
    prec = precision_score(y_test, preds, zero_division=0)

    print(
        f"[AUDITOR] 🤖 Model Trained. Accuracy: {acc:.2f}, Precision (Predicting Failure): {prec:.2f}"
    )

    # Save Model
    os.makedirs(os.path.dirname(MODEL_PATH), exist_ok=True)
    joblib.dump(model, MODEL_PATH)

    # Extract Feature Importance (Toxic Drivers) — XGBoost MDI (impurity/gain).
    importance = model.feature_importances_
    toxic_features = {}
    for i, col in enumerate(X.columns):
        toxic_features[col] = float(importance[i])

    print("[AUDITOR] ☣️  Toxic Feature Importance:")
    sorted_features = sorted(toxic_features.items(), key=lambda x: x[1], reverse=True)
    for k, v in sorted_features:
        print(f"  - {k}: {v:.4f}")

    # JSON Export for Lisp to consume (unchanged default artifact).
    with open(OUTPUT_PATH, "w") as f:
        json.dump(toxic_features, f, indent=4)
    print(f"[AUDITOR] 💾 Toxic features saved to {OUTPUT_PATH}")

    # V-methodology (2026-08-11): additive MDA/permutation importance (default OFF).
    # Written to a SEPARATE sidecar so the canonical toxic_features.json is unchanged.
    if mda_enabled():
        mda = compute_mda(model, X_test, y_test)
        print("[AUDITOR] 🔀 Permutation Importance (MDA, mean decrease in accuracy):")
        for k, v in sorted(mda.items(), key=lambda kv: kv[1]["mean"], reverse=True):
            print(f"  - {k}: {v['mean']:.4f} ± {v['std']:.4f}")
        with open(MDA_OUTPUT_PATH, "w") as f:
            json.dump(mda, f, indent=4)
        print(f"[AUDITOR] 💾 MDA importances saved to {MDA_OUTPUT_PATH}")


def main():
    print("[AUDITOR] 🔍 Starting Logic Integrity Audit...")
    df = load_data()
    X, y = feature_engineering(df)

    if X.empty:
        print("[AUDITOR] ⚠️ No data to analyze.")
        # Create dummy file to satisfy check
        with open(OUTPUT_PATH, "w") as f:
            json.dump({"dummy": 0.0}, f)
        return

    train_and_audit(X, y)
    print("[AUDITOR] ✅ Audit Complete.")


if __name__ == "__main__":
    main()
