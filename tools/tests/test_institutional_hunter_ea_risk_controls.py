from pathlib import Path


def test_ih_ea_has_atr_risk_controls_and_time_spread_filters() -> None:
    src = Path("src/mt5/InstitutionalHunterEA.mq5").read_text(encoding="utf-8")

    # Inputs added for ATR-based stop/target and trading session controls.
    assert "input int      InpATRPeriod" in src
    assert "input double   InpSL_ATR_Mult" in src
    assert "input double   InpTP_ATR_Mult" in src
    assert "input int      InpTradeStartHour" in src
    assert "input int      InpTradeEndHour" in src

    # Helper functions requested by the strategy update.
    assert "bool SpreadOK(" in src
    assert "bool TimeOK()" in src
    assert "double GetATR(" in src
    assert "double CalcLotByRisk(" in src

    # Guardrail usage in the execution flow.
    assert "if(!TimeOK())" in src
    assert "if(!SpreadOK(symbol))" in src
    assert "CalcLotByRisk(symbol, InpRiskPerTradePct, sl_points)" in src
