import unittest
from unittest import mock

from tools import xau_autobot_backtest as backtest
from tools import xau_autobot_optimize as optimize
from tools import xau_autobot_readiness as readiness


class TestXauAutoBotOHLCWrappers(unittest.TestCase):
    def test_backtest_wrapper_calls_shared_loader_without_yf_module(self):
        expected = (["t"], [1.0], [1.0], [1.0], [1.0])
        with mock.patch.object(backtest, "load_ohlc", return_value=expected) as load_mock:
            got = backtest._load_ohlc("GC=F", "30d", "5m")
        self.assertEqual(got, expected)
        load_mock.assert_called_once_with(ticker="GC=F", period="30d", interval="5m")

    def test_readiness_wrapper_calls_shared_loader_without_yf_module(self):
        expected = (["t"], [2.0], [2.0], [2.0], [2.0])
        with mock.patch.object(readiness, "load_ohlc", return_value=expected) as load_mock:
            got = readiness._load_ohlc("GC=F", "60d", "5m")
        self.assertEqual(got, expected)
        load_mock.assert_called_once_with(ticker="GC=F", period="60d", interval="5m")

    def test_optimize_wrapper_calls_shared_loader_without_yf_module(self):
        expected = (["t"], [3.0], [3.0], [3.0], [3.0])
        with mock.patch.object(optimize, "load_ohlc", return_value=expected) as load_mock:
            got = optimize._load_ohlc("GC=F", "90d", "5m")
        self.assertEqual(got, expected)
        load_mock.assert_called_once_with(ticker="GC=F", period="90d", interval="5m")


if __name__ == "__main__":
    unittest.main()
