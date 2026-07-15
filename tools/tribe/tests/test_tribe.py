"""Tests for the 蠱毒 export toolkit. Run: python3 -m pytest tools/tribe/tests -q"""

from __future__ import annotations

import os
import sys

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..")))

from tools.tribe import sexpr
from tools.tribe.honest_gate import GateConfig, evaluate, normalize_to_budget


SAMPLE = '''#S(STRATEGY
   :NAME "Bred-Test-Strat"
   :INDICATORS ((SWIMMY.SCHOOL::SMA 86) (SWIMMY.SCHOOL::EMA 19))
   :ENTRY (AND (> CLOSE SWIMMY.SCHOOL::SMA-20) (> SWIMMY.SCHOOL::RSI 55))
   :EXIT (OR (> SWIMMY.SCHOOL::PNL SWIMMY.SCHOOL::TP))
   :SL 0.74
   :TP 0.50
   :SHARPE 0.207564
   :PROFIT-FACTOR 1.1561143
   :WIN-RATE 0.53395784
   :TRADES 1281
   :MAX-DD 0.0010407257
   :CATEGORY :TREND
   :OOS-SHARPE 0.0
   :CPCV-PASS-RATE 0.0
   :IMMORTAL NIL
   :SYMBOL "USDJPY"
   :PARENTS ("Parent-A" "Bladerunner"))'''


def test_parse_scalars():
    s = sexpr.parse_strategy(SAMPLE)
    assert s["name"] == "Bred-Test-Strat"
    assert abs(s["profit-factor"] - 1.1561143) < 1e-6
    assert s["trades"] == 1281
    assert s["category"] == ":TREND"
    assert s["immortal"] is None  # NIL -> None
    assert s["symbol"] == "USDJPY"


def test_parse_structural_slots_roundtrip():
    s = sexpr.parse_strategy(SAMPLE)
    entry = sexpr.serialize(s["entry"])
    assert entry.startswith("(AND")
    assert "CLOSE" in entry
    parents = s["parents"]
    assert parents == ["Parent-A", "Bladerunner"]


def test_parse_rejects_non_struct():
    try:
        sexpr.parse_strategy("(just a list)")
    except ValueError:
        return
    raise AssertionError("expected ValueError")


def test_gate_is_only_is_provisional_not_pass():
    # marginal IS survivor: clears floors but no OOS validation -> PROVISIONAL
    v = evaluate({
        "trades": 1281, "profit_factor": 1.156, "sharpe": 0.2076,
        "oos_sharpe": 0.0, "cpcv_pass_rate": 0.0, "entry": "(AND ...)",
    })
    assert v.verdict == "PROVISIONAL", v.verdict


def test_gate_validated_passes():
    v = evaluate({
        "trades": 1281, "profit_factor": 1.5, "sharpe": 0.6,
        "oos_sharpe": 0.4, "cpcv_pass_rate": 0.8, "entry": "(AND ...)",
    })
    assert v.verdict == "PASS", v.verdict


def test_gate_undersampled_rejects():
    v = evaluate({"trades": 21, "profit_factor": 2.06, "sharpe": 0.9, "entry": "(x)"})
    assert v.verdict == "REJECT"
    assert any(c["name"] == "sample_size" and c["status"] == "fail" for c in v.checks)


def test_gate_dummy_rejects():
    v = evaluate({"trades": 0})
    assert v.verdict == "REJECT"
    assert any(c["name"] == "not_dummy" and c["status"] == "fail" for c in v.checks)


def test_dd_budget_normalisation_eurusd():
    # the headline case: 5.9136%/mo at DD 45.28%
    n = normalize_to_budget(5.9136, 45.2789, GateConfig())
    assert abs(n["monthly_at_budget_pct"] - 1.57) < 0.05
    assert n["meets_kpi_at_budget"] is False
    # and the full gate rejects it on dd_budget + forward sample
    v = evaluate({"monthly_return_pct": 5.9136, "drawdown_pct": 45.2789,
                  "forward_trades": 21, "profit_factor": 2.06, "trades": 21, "entry": "(x)"})
    assert v.verdict == "REJECT"
    assert any(c["name"] == "dd_budget" and c["status"] == "fail" for c in v.checks)


if __name__ == "__main__":
    import subprocess
    raise SystemExit(subprocess.call([sys.executable, "-m", "pytest", os.path.dirname(__file__), "-q"]))
