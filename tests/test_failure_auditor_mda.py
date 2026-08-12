"""V-methodology (2026-08-11): tests for the MDA / permutation-importance addition
to tools/failure_auditor.py. Skips cleanly where ML deps are unavailable."""
import os
import sys

import pytest

np = pytest.importorskip("numpy")
pd = pytest.importorskip("pandas")
pytest.importorskip("sklearn")
# NOTE: xgboost is intentionally NOT required — compute_mda is model-agnostic and
# failure_auditor imports xgboost lazily, so the MDA path is testable without it.

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "tools"))
import failure_auditor as fa  # noqa: E402


class _OneFeatureModel:
    """Predicts purely from the 'important' column; ignores everything else."""

    def predict(self, X):
        a = X["important"].to_numpy()
        return (a > 0.5).astype(int)


def _make_data(n=200, seed=0):
    rng = np.random.default_rng(seed)
    imp = rng.random(n)
    noise = rng.random(n)
    X = pd.DataFrame({"important": imp, "noise": noise})
    y = (imp > 0.5).astype(int)
    return X, y


def test_compute_mda_flags_important_feature():
    X, y = _make_data()
    res = fa.compute_mda(_OneFeatureModel(), X, y, n_repeats=10, seed=42)
    assert set(res.keys()) == {"important", "noise"}
    # Permuting the informative feature should sharply drop accuracy...
    assert res["important"]["mean"] > 0.2
    # ...while permuting the irrelevant one should not.
    assert abs(res["noise"]["mean"]) < 0.05


def test_mda_disabled_by_default(monkeypatch):
    monkeypatch.delenv("AUDITOR_MDA", raising=False)
    monkeypatch.setattr(sys, "argv", ["failure_auditor.py"])
    assert fa.mda_enabled() is False


def test_mda_enables_via_env(monkeypatch):
    monkeypatch.setenv("AUDITOR_MDA", "1")
    monkeypatch.setattr(sys, "argv", ["failure_auditor.py"])
    assert fa.mda_enabled() is True
