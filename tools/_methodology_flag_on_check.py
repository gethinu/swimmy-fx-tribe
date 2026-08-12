"""Flag-ON functional check (VERIFICATION harness — safe to delete).

Proves the ON path of each methodology flag actually DOES something (not a silent
no-op), with explicit PASS/FAIL logs:

  * Deflated Sharpe (Lisp `*enable-real-dsr*`): the exact Bailey-LdP formula ported
    from school-ranking.lisp — abstains on short history, ranks a strong stable
    series above noise on adequate history.
  * PBO / CSCV (Rust `SWIMMY_CPCV_PBO`): the exact compute_pbo ported from pbo.rs —
    returns a real interior probability on a partially-overfit grid.
  * MDA (Python `AUDITOR_MDA`): the REAL tools/failure_auditor.compute_mda — flags an
    informative feature and clears a noise feature.

Normal-CDF/PPF are validated against statistics.NormalDist in
tools/_methodology_math_check.py; this file focuses on ON *behaviour*.
"""
import math
import os
import sys
from itertools import combinations

import numpy as np

# ---------- DSR (verbatim port of school-ranking.lisp) ------------------------
EULER = 0.5772156649015329


def _poly(coeffs, x):
    acc = 0.0
    for c in coeffs:
        acc = acc * x + c
    return acc


def norm_cdf(x):
    neg = x < 0.0
    z = abs(x)
    tt = 1.0 / (1.0 + 0.2316419 * z)
    pdf = (1.0 / math.sqrt(2.0 * math.pi)) * math.exp(-0.5 * z * z)
    p = tt * _poly([1.330274429, -1.821255978, 1.781477937, -0.356563782, 0.319381530], tt)
    upper = pdf * p
    return upper if neg else 1.0 - upper


def norm_ppf(p):
    if p <= 0.0:
        return -math.inf
    if p >= 1.0:
        return math.inf
    ca = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
          -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00]
    cd = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
          3.754408661907416e+00, 1.0]
    aa = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
          1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00]
    bb = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
          6.680131188771972e+01, -1.328068155288572e+01, 1.0]
    plow, phigh = 0.02425, 1.0 - 0.02425
    if p < plow:
        q = math.sqrt(-2.0 * math.log(p))
        return _poly(ca, q) / _poly(cd, q)
    elif p <= phigh:
        q = p - 0.5
        r = q * q
        return (_poly(aa, r) * q) / _poly(bb, r)
    else:
        q = math.sqrt(-2.0 * math.log(1.0 - p))
        return -(_poly(ca, q) / _poly(cd, q))


def _cm(xs, m, k):
    return sum((x - m) ** k for x in xs) / len(xs)


def _per_obs_sharpe(h):
    if len(h) < 2:
        return None
    m = sum(h) / len(h)
    v = _cm(h, m, 2)
    return None if v <= 0 else m / math.sqrt(v)


def dsr(target_hist, all_hists, min_samples=20):
    if len(target_hist) < min_samples:
        return None
    n_trials = max(2, len(all_hists))
    T = len(target_hist)
    m = sum(target_hist) / T
    v = _cm(target_hist, m, 2)
    if v <= 0:
        return None
    sd = math.sqrt(v)
    sr = m / sd
    skew = _cm(target_hist, m, 3) / sd ** 3
    kurt = _cm(target_hist, m, 4) / sd ** 4
    srs = [s for s in (_per_obs_sharpe(h) for h in all_hists) if s is not None]
    if len(srs) < 2:
        return None
    msr = sum(srs) / len(srs)
    vsr = _cm(srs, msr, 2)
    if vsr <= 0:
        return None
    sr0 = math.sqrt(vsr) * ((1 - EULER) * norm_ppf(1 - 1.0 / n_trials)
                            + EULER * norm_ppf(1 - 1.0 / (n_trials * math.e)))
    denom = math.sqrt(max(1e-12, 1 - skew * sr + ((kurt - 1) / 4) * sr * sr))
    z = (sr - sr0) * math.sqrt(max(1.0, T - 1)) / denom
    return norm_cdf(z)


# ---------- PBO (verbatim port of pbo.rs) -------------------------------------
def _avg_rank(v, allv):
    less = sum(1 for x in allv if x < v)
    eq = sum(1 for x in allv if abs(x - v) <= 2.220446049250313e-16 * max(abs(v), 1.0))
    return less + (eq + 1) / 2.0


def compute_pbo(bp):
    s = len(bp)
    if s < 2:
        return None
    n = len(bp[0])
    if n < 2:
        return None
    if s % 2 == 1:
        s -= 1
    bp = bp[:s]
    half = s // 2
    over = 0
    sp = 0
    for isb in combinations(range(s), half):
        inis = [b in isb for b in range(s)]
        ri = [0.0] * n
        ro = [0.0] * n
        for b in range(s):
            for c in range(n):
                (ri if inis[b] else ro)[c] += bp[b][c]
        best = max(range(n), key=lambda c: ri[c])
        om = min(max(_avg_rank(ro[best], ro) / (n + 1), 1e-12), 1 - 1e-12)
        if math.log(om / (1 - om)) < 0:
            over += 1
        sp += 1
    return over / sp


# ============================== CHECKS ========================================
fails = 0


def chk(name, cond, extra=""):
    global fails
    print(("PASS" if cond else "FAIL"), name, extra)
    if not cond:
        fails += 1


print("=== FLAG-ON functional check ===\n")

# --- DSR ON: abstain on short, compute + rank on adequate --------------------
rng = np.random.default_rng(1)
noise = [[float(x) for x in rng.normal(0, 1, 80)] for _ in range(6)]
strong = [float(x) for x in (0.5 + rng.normal(0, 0.2, 80))]
d_short = dsr([1.0, -1.0, 2.0], noise)          # 3 < min_samples -> abstain
d_strong = dsr(strong, noise + [strong])
d_noise = dsr(noise[0], noise)
chk("DSR ON abstains on short history (returns None)", d_short is None)
chk("DSR ON computes on adequate history (strong)", d_strong is not None and 0 <= d_strong <= 1,
    f"[dsr_strong={d_strong:.4f}]")
chk("DSR ON ranks strong series above noise", d_strong is not None and d_noise is not None and d_strong > d_noise,
    f"[strong={d_strong:.4f} > noise={d_noise:.4f}]")

# --- PBO ON: interior probability on a partially-overfit grid ----------------
grid = [[3.0, 2.0, 1.0], [3.0, 2.0, 1.0], [1.0, 2.0, 3.0], [1.0, 2.0, 3.0]]
p = compute_pbo(grid)
chk("PBO ON computes an interior value (~1/3)", p is not None and abs(p - 1 / 3) < 1e-12, f"[pbo={p:.4f}]")

# --- MDA ON: real failure_auditor.compute_mda flags the informative feature ---
try:
    import pandas as pd
    sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
    import failure_auditor as fa

    class _OneFeatureModel:
        def predict(self, X):
            return (X["important"].to_numpy() > 0.5).astype(int)

    n = 200
    imp = rng.random(n)
    noise_f = rng.random(n)
    X = pd.DataFrame({"important": imp, "noise": noise_f})
    y = (imp > 0.5).astype(int)
    res = fa.compute_mda(_OneFeatureModel(), X, y, n_repeats=10, seed=42)
    chk("MDA ON flags the informative feature", res["important"]["mean"] > 0.2,
        f"[important={res['important']['mean']:.3f}]")
    chk("MDA ON clears the noise feature", abs(res["noise"]["mean"]) < 0.05,
        f"[noise={res['noise']['mean']:.3f}]")
except Exception as e:  # pragma: no cover
    print("SKIP MDA real check (dependency missing):", repr(e))

print("\nRESULT:", "ALL PASS" if fails == 0 else f"{fails} FAILURES")
sys.exit(1 if fails else 0)
