import os
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from tools.backtest_service import BacktestService


def test_backtest_service_propagates_request_id():
    svc = BacktestService(use_zmq=False)
    msg = '((action . "BACKTEST") (request_id . "RID-TEST") (strategy . ((name . "UT"))))'
    out = svc._handle_sexpr(msg)
    assert "RID-TEST" in str(out)


def test_backtest_service_missing_request_id_fails_fast():
    svc = BacktestService(use_zmq=False)
    msg = '((action . "BACKTEST") (strategy . ((name . "UT"))))'
    out = svc._handle_sexpr(msg)
    assert "missing request_id" in str(out)
    assert "MISSING" in str(out)
