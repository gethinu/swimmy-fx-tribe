"""Honest promotion gate for strategy candidates.

This is the rule that separates "survived the 蠱毒 jar" from "is a deployable
edge". It works on a flat dict of candidate metrics and emits a structured
verdict. It is intentionally provider-agnostic so it can score:

  * library survivors  (in-sample Sharpe/PF/WR/trades, plus OOS/CPCV slots)
  * MT5 optimisation candidates  (monthly_return_pct + drawdown_pct)

Two ideas are enforced:

1. Risk normalisation. Return and drawdown both scale (to first order) with
   position size, so a headline return at an out-of-budget drawdown is
   re-expressed at the firm's drawdown budget. A "month 5.9% / DD 45%" candidate
   is really "~1.6% at DD 12%" — the gate scores the budgeted number, not the
   headline.

2. Evidence, not luck. Tiny trade counts and missing out-of-sample / CPCV
   validation are flagged. A marginal IS-only survivor is never PASS; the best
   it earns is PROVISIONAL so a downstream consumer cannot mistake it for a
   verified edge.

CLI:
    python3 -m tools.tribe.honest_gate --monthly 5.9136 --dd 45.28 --trades 21 --pf 2.06
"""

from __future__ import annotations

import argparse
import json
import math
from dataclasses import dataclass, field, asdict
from typing import Any, Optional


@dataclass
class GateConfig:
    dd_budget_pct: float = 12.0      # firm drawdown budget
    kpi_monthly_pct: float = 3.0     # primary monthly-return KPI
    kpi_stretch_pct: float = 5.0     # stretch KPI
    min_trades: int = 200            # sample-size floor for an IS edge claim
    min_forward_trades: int = 300    # sample-size floor for forward evidence
    min_profit_factor: float = 1.10  # after-cost-aware PF floor
    min_sharpe: float = 0.10         # IS Sharpe floor


@dataclass
class Check:
    name: str
    status: str            # "pass" | "warn" | "fail" | "skip"
    detail: str
    value: Optional[float] = None


@dataclass
class Verdict:
    verdict: str           # "PASS" | "PROVISIONAL" | "REJECT"
    score: float
    checks: list = field(default_factory=list)
    normalized: dict = field(default_factory=dict)

    def to_dict(self) -> dict:
        d = asdict(self)
        return d


def _num(d: dict, *keys) -> Optional[float]:
    for k in keys:
        if k in d and d[k] is not None:
            try:
                return float(d[k])
            except (TypeError, ValueError):
                return None
    return None


def normalize_to_budget(monthly_pct: float, dd_pct: float, cfg: GateConfig) -> dict:
    """Re-express a headline return at the drawdown budget (linear leverage scale)."""
    if dd_pct is None or dd_pct <= 0:
        return {}
    k = cfg.dd_budget_pct / dd_pct
    budgeted_monthly = monthly_pct * k
    out = {
        "scale_to_budget": round(k, 4),
        "monthly_at_budget_pct": round(budgeted_monthly, 4),
        "meets_kpi_at_budget": budgeted_monthly >= cfg.kpi_monthly_pct,
        "dd_needed_for_kpi_pct": round(dd_pct * (cfg.kpi_monthly_pct / monthly_pct), 2)
        if monthly_pct > 0 else None,
        "recovery_gain_pct": round((1.0 / (1.0 - dd_pct / 100.0) - 1.0) * 100.0, 2)
        if dd_pct < 100 else None,
    }
    return out


def evaluate(candidate: dict, cfg: GateConfig | None = None) -> Verdict:
    """Score a candidate. ``candidate`` may carry any subset of:

    trades, profit_factor, sharpe, win_rate, max_dd,
    oos_sharpe, cpcv_pass_rate, monthly_return_pct, drawdown_pct, forward_trades
    """
    cfg = cfg or GateConfig()
    checks: list[Check] = []
    hard_fail = False
    warn = False

    trades = _num(candidate, "trades")
    pf = _num(candidate, "profit_factor", "profit-factor")
    sharpe = _num(candidate, "sharpe")
    oos_sharpe = _num(candidate, "oos_sharpe", "oos-sharpe")
    cpcv_pass = _num(candidate, "cpcv_pass_rate", "cpcv-pass-rate")
    cpcv_sharpe = _num(candidate, "cpcv_median_sharpe", "cpcv-median-sharpe")
    monthly = _num(candidate, "monthly_return_pct", "monthly_cagr_pct")
    dd = _num(candidate, "drawdown_pct", "equity_dd_pct")
    fwd_trades = _num(candidate, "forward_trades")

    # 1. not a placeholder / dummy
    has_logic = bool(candidate.get("entry"))
    if (trades is None or trades <= 0) and not has_logic:
        checks.append(Check("not_dummy", "fail", "no trades and no entry logic (placeholder)"))
        hard_fail = True
    else:
        checks.append(Check("not_dummy", "pass", "has trades and/or entry logic"))

    # 2. sample size (IS)
    if trades is not None:
        if trades >= cfg.min_trades:
            checks.append(Check("sample_size", "pass", f"{int(trades)} trades >= {cfg.min_trades}", trades))
        else:
            checks.append(Check("sample_size", "fail", f"{int(trades)} trades < {cfg.min_trades} (under-sampled)", trades))
            hard_fail = True

    # 3. profit factor
    if pf is not None:
        if pf >= cfg.min_profit_factor:
            checks.append(Check("profit_factor", "pass", f"PF {pf:.3f} >= {cfg.min_profit_factor}", pf))
        else:
            checks.append(Check("profit_factor", "fail", f"PF {pf:.3f} < {cfg.min_profit_factor}", pf))
            hard_fail = True

    # 4. sharpe
    if sharpe is not None:
        if sharpe >= cfg.min_sharpe:
            checks.append(Check("sharpe", "pass", f"Sharpe {sharpe:.3f} >= {cfg.min_sharpe}", sharpe))
        else:
            checks.append(Check("sharpe", "warn", f"Sharpe {sharpe:.3f} < {cfg.min_sharpe} (thin)", sharpe))
            warn = True

    # 5. out-of-sample / CPCV evidence
    validated = (oos_sharpe is not None and oos_sharpe > 0) or \
                (cpcv_pass is not None and cpcv_pass > 0) or \
                (cpcv_sharpe is not None and cpcv_sharpe > 0)
    if validated:
        checks.append(Check("out_of_sample", "pass", "has OOS/CPCV validation"))
    else:
        checks.append(Check("out_of_sample", "warn", "IS-only: never cleared OOS/CPCV (UNVALIDATED)"))
        warn = True

    # 6. drawdown-budget normalisation (only when a headline monthly return + DD exist)
    normalized: dict = {}
    if monthly is not None and dd is not None and dd > 0:
        normalized = normalize_to_budget(monthly, dd, cfg)
        if normalized.get("meets_kpi_at_budget"):
            checks.append(Check("dd_budget", "pass",
                                f"{normalized['monthly_at_budget_pct']:.2f}%/mo at DD{cfg.dd_budget_pct:.0f}% >= KPI {cfg.kpi_monthly_pct}%",
                                normalized["monthly_at_budget_pct"]))
        else:
            checks.append(Check("dd_budget", "fail",
                                f"only {normalized['monthly_at_budget_pct']:.2f}%/mo at DD{cfg.dd_budget_pct:.0f}% (< KPI {cfg.kpi_monthly_pct}%); headline {monthly:.2f}% needs DD{dd:.0f}%",
                                normalized["monthly_at_budget_pct"]))
            hard_fail = True
        # forward sample size, if present
        if fwd_trades is not None:
            if fwd_trades >= cfg.min_forward_trades:
                checks.append(Check("forward_sample", "pass", f"{int(fwd_trades)} forward trades >= {cfg.min_forward_trades}", fwd_trades))
            else:
                checks.append(Check("forward_sample", "fail", f"{int(fwd_trades)} forward trades < {cfg.min_forward_trades}", fwd_trades))
                hard_fail = True

    # ---- verdict ----
    if hard_fail:
        verdict = "REJECT"
    elif warn:
        verdict = "PROVISIONAL"
    else:
        verdict = "PASS"

    # ---- score (for ranking) ----
    # reward PF, Sharpe, log-trades; reward validation; penalise REJECT.
    score = 0.0
    if pf is not None:
        score += max(0.0, (pf - 1.0)) * 100.0
    if sharpe is not None:
        score += max(0.0, sharpe) * 50.0
    if trades is not None and trades > 0:
        score += math.log10(trades) * 5.0
    if validated:
        score += 25.0
    if normalized.get("monthly_at_budget_pct") is not None:
        score += max(0.0, normalized["monthly_at_budget_pct"]) * 5.0
    if verdict == "REJECT":
        score *= 0.25
    score = round(score, 3)

    return Verdict(verdict=verdict, score=score, checks=[asdict(c) for c in checks], normalized=normalized)


def _cli():
    ap = argparse.ArgumentParser(description="Honest promotion gate for a single candidate")
    ap.add_argument("--monthly", type=float, help="headline monthly return %%")
    ap.add_argument("--dd", type=float, help="drawdown %%")
    ap.add_argument("--trades", type=float)
    ap.add_argument("--forward-trades", type=float)
    ap.add_argument("--pf", type=float)
    ap.add_argument("--sharpe", type=float)
    ap.add_argument("--oos-sharpe", type=float)
    ap.add_argument("--cpcv-pass-rate", type=float)
    ap.add_argument("--dd-budget", type=float, default=12.0)
    ap.add_argument("--kpi-monthly", type=float, default=3.0)
    ap.add_argument("--json", action="store_true", help="emit JSON")
    args = ap.parse_args()

    cand = {
        "monthly_return_pct": args.monthly,
        "drawdown_pct": args.dd,
        "trades": args.trades,
        "forward_trades": args.forward_trades,
        "profit_factor": args.pf,
        "sharpe": args.sharpe,
        "oos_sharpe": args.oos_sharpe,
        "cpcv_pass_rate": args.cpcv_pass_rate,
    }
    cand = {k: v for k, v in cand.items() if v is not None}
    cfg = GateConfig(dd_budget_pct=args.dd_budget, kpi_monthly_pct=args.kpi_monthly)
    v = evaluate(cand, cfg)

    if args.json:
        print(json.dumps(v.to_dict(), indent=2))
        return
    print(f"VERDICT: {v.verdict}   score={v.score}")
    if v.normalized:
        n = v.normalized
        print(f"  risk-normalised: {n.get('monthly_at_budget_pct')}%/mo at DD{cfg.dd_budget_pct:.0f}% "
              f"(meets KPI: {n.get('meets_kpi_at_budget')})")
    for c in v.checks:
        mark = {"pass": "OK ", "warn": "!! ", "fail": "XX ", "skip": ".. "}.get(c["status"], "?")
        print(f"  [{mark}] {c['name']}: {c['detail']}")


if __name__ == "__main__":
    _cli()
