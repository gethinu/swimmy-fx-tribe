"""Numeric cross-check of the DSR (Lisp) and PBO (Rust) math ported verbatim.

Reference: Python's builtin statistics.NormalDist (no scipy needed).
This is a VERIFICATION harness, not shipped logic. Safe to delete.
"""
import math
from statistics import NormalDist
from itertools import combinations

ND = NormalDist()

# ---- ports of the Lisp %poly / %norm-cdf / %norm-ppf --------------------------
def poly(coeffs, x):
    acc = 0.0
    for c in coeffs:
        acc = acc * x + c
    return acc

def norm_cdf(x):
    neg = x < 0.0
    z = abs(x)
    tt = 1.0 / (1.0 + 0.2316419 * z)
    pdf = (1.0 / math.sqrt(2.0 * math.pi)) * math.exp(-0.5 * z * z)
    p = tt * poly([1.330274429, -1.821255978, 1.781477937, -0.356563782, 0.319381530], tt)
    upper = pdf * p
    return upper if neg else 1.0 - upper

def norm_ppf(p):
    if p <= 0.0: return -math.inf
    if p >= 1.0: return math.inf
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
        q = math.sqrt(-2.0 * math.log(p)); return poly(ca, q) / poly(cd, q)
    elif p <= phigh:
        q = p - 0.5; r = q * q; return (poly(aa, r) * q) / poly(bb, r)
    else:
        q = math.sqrt(-2.0 * math.log(1.0 - p)); return -(poly(ca, q) / poly(cd, q))

# ---- port of deflated-sharpe-ratio -------------------------------------------
EULER = 0.5772156649015329
def central_moment(xs, mean, k):
    return sum((x - mean) ** k for x in xs) / len(xs)

def per_obs_sharpe(h):
    if len(h) < 2: return None
    m = sum(h) / len(h)
    var = central_moment(h, m, 2)
    return None if var <= 0 else m / math.sqrt(var)

def dsr(target_hist, all_hists, min_samples=20):
    if len(target_hist) < min_samples: return None
    n_trials = max(2, len(all_hists))
    T = len(target_hist)
    m = sum(target_hist) / T
    var = central_moment(target_hist, m, 2)
    if var <= 0: return None
    sd = math.sqrt(var)
    sr_hat = m / sd
    skew = central_moment(target_hist, m, 3) / sd**3
    kurt = central_moment(target_hist, m, 4) / sd**4
    srs = [s for s in (per_obs_sharpe(h) for h in all_hists) if s is not None]
    if len(srs) < 2: return None
    msr = sum(srs) / len(srs)
    var_sr = central_moment(srs, msr, 2)
    if var_sr <= 0: return None
    sd_sr = math.sqrt(var_sr)
    e = math.e
    sr0 = sd_sr * ((1 - EULER) * norm_ppf(1 - 1.0/n_trials)
                   + EULER * norm_ppf(1 - 1.0/(n_trials*e)))
    denom = math.sqrt(max(1e-12, 1 - skew*sr_hat + ((kurt-1)/4)*sr_hat*sr_hat))
    z = (sr_hat - sr0) * math.sqrt(max(1.0, T-1)) / denom
    return norm_cdf(z)

# ---- port of Rust compute_pbo ------------------------------------------------
def average_rank(value, allv):
    less = sum(1 for v in allv if v < value)
    equal = sum(1 for v in allv if abs(v - value) <= 2.220446049250313e-16 * max(abs(value), 1.0))
    return less + (equal + 1) / 2.0

def compute_pbo(block_perf):
    s = len(block_perf)
    if s < 2: return None
    n = len(block_perf[0])
    if n < 2: return None
    if s % 2 == 1: s -= 1
    bp = block_perf[:s]
    half = s // 2
    overfit = 0; logit_sum = 0.0; splits = 0
    for is_blocks in combinations(range(s), half):
        in_is = [b in is_blocks for b in range(s)]
        r_is = [0.0]*n; r_oos = [0.0]*n
        for b in range(s):
            for cfg in range(n):
                if in_is[b]: r_is[cfg] += bp[b][cfg]
                else: r_oos[cfg] += bp[b][cfg]
        best = max(range(n), key=lambda c: r_is[c])
        rank = average_rank(r_oos[best], r_oos)
        omega = min(max(rank/(n+1), 1e-12), 1-1e-12)
        lam = math.log(omega/(1-omega))
        logit_sum += lam
        if lam < 0: overfit += 1
        splits += 1
    return overfit/splits, logit_sum/splits

# ============================== CHECKS ========================================
fails = 0
def chk(name, cond):
    global fails
    print(("PASS" if cond else "FAIL"), name)
    if not cond: fails += 1

# normal CDF vs reference
for x in [-3, -1.96, -1, 0, 1, 1.6448536269514722, 2, 3]:
    chk(f"norm_cdf({x})", abs(norm_cdf(x) - ND.cdf(x)) < 1e-6)
# inverse CDF vs reference
for p in [0.001, 0.025, 0.1, 0.5, 0.9, 0.975, 0.999]:
    chk(f"norm_ppf({p})", abs(norm_ppf(p) - ND.inv_cdf(p)) < 1e-6)

# DSR sanity: strong, stable positive returns among noisy peers -> high DSR;
# a target identical to noise peers -> lower DSR.
import random
random.seed(1)
noise = [[random.gauss(0.0, 1.0) for _ in range(80)] for _ in range(6)]
strong = [0.5 + random.gauss(0.0, 0.2) for _ in range(80)]
d_strong = dsr(strong, noise + [strong])
d_noise = dsr(noise[0], noise)
chk("dsr strong in [0,1]", d_strong is not None and 0.0 <= d_strong <= 1.0)
chk("dsr noise in [0,1]", d_noise is not None and 0.0 <= d_noise <= 1.0)
chk("dsr strong > noise", d_strong > d_noise)
chk("dsr short abstains", dsr([1.0,-1.0,2.0], noise) is None)
print(f"  (dsr strong={d_strong:.4f}, noise={d_noise:.4f})")

# PBO reference cases (mirror the Rust unit tests)
dom = [[3.0,2.0,1.0]]*4
chk("pbo dominant == 0", compute_pbo(dom)[0] == 0.0)
anti = [[3.0,2.0,1.0],[1.0,2.0,3.0]]
chk("pbo anti == 1", compute_pbo(anti)[0] == 1.0)
odd = [[3.0,2.0,1.0],[1.0,2.0,3.0],[9,9,9]]
chk("pbo odd drops last", compute_pbo(odd)[0] == 1.0)

print("\nRESULT:", "ALL PASS" if fails == 0 else f"{fails} FAILURES")
