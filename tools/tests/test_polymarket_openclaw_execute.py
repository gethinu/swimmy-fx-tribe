import unittest

from tools import polymarket_openclaw_execute as live


class DummyOrderType:
    GTC = "GTC"
    FOK = "FOK"


class DummyClient:
    def __init__(self, *, should_fail: bool = False) -> None:
        self.should_fail = should_fail
        self.created = []
        self.posted = []

    def create_order(self, args):
        self.created.append(args)
        if self.should_fail:
            raise RuntimeError("create failed")
        return {"signed": True, "args": args}

    def post_order(self, signed, order_type):
        self.posted.append((signed, order_type))
        if self.should_fail:
            raise RuntimeError("post failed")
        return {"status": "accepted", "order_type": order_type}


class TestPolymarketOpenClawExecute(unittest.TestCase):
    def test_parse_plan_entries_filters_invalid_rows(self) -> None:
        payload = {
            "entries": [
                {
                    "market_id": "m1",
                    "token_id": "t1",
                    "side": "YES",
                    "entry_price": 0.5,
                    "stake_usd": 2.0,
                    "expected_value_usd": 0.1,
                },
                {"market_id": "m2", "token_id": "", "side": "NO", "entry_price": 0.5, "stake_usd": 1.0},
                {"market_id": "m3", "token_id": "t3", "side": "NO", "entry_price": 0.0, "stake_usd": 1.0},
            ]
        }
        rows = live.parse_plan_entries(payload)
        self.assertEqual(1, len(rows))
        self.assertEqual("m1", rows[0].market_id)

    def test_build_limit_order_specs_applies_filters(self) -> None:
        entries = [
            live.PlanEntry(
                market_id="m1",
                token_id="t1",
                side="YES",
                entry_price=0.5,
                stake_usd=2.0,
                expected_value_usd=0.2,
            ),
            live.PlanEntry(
                market_id="m2",
                token_id="t2",
                side="NO",
                entry_price=0.4,
                stake_usd=0.5,
                expected_value_usd=0.2,
            ),
            live.PlanEntry(
                market_id="m3",
                token_id="t3",
                side="YES",
                entry_price=0.25,
                stake_usd=1.5,
                expected_value_usd=0.05,
            ),
        ]
        specs = live.build_limit_order_specs(
            entries=entries,
            max_orders=2,
            min_expected_value_usd=0.1,
            min_stake_usd=1.0,
        )
        self.assertEqual(1, len(specs))
        self.assertEqual("m1", specs[0]["market_id"])
        self.assertAlmostEqual(4.0, specs[0]["size"])

    def test_resolve_order_type(self) -> None:
        value = live.resolve_order_type("gtc", DummyOrderType)
        self.assertEqual("GTC", value)
        with self.assertRaises(ValueError):
            live.resolve_order_type("bad", DummyOrderType)

    def test_post_limit_orders_success(self) -> None:
        client = DummyClient(should_fail=False)
        specs = [
            {
                "market_id": "m1",
                "token_id": "t1",
                "entry_side": "YES",
                "entry_price": 0.5,
                "stake_usd": 2.0,
                "size": 4.0,
                "expected_value_usd": 0.2,
            }
        ]

        def order_args_ctor(**kwargs):
            return kwargs

        result = live.post_limit_orders(
            order_specs=specs,
            client=client,
            order_args_ctor=order_args_ctor,
            order_type_enum=DummyOrderType,
            buy_side_value="BUY",
            order_type_text="GTC",
            dry_run=False,
        )
        self.assertEqual(1, result["attempted"])
        self.assertEqual(1, result["sent"])
        self.assertEqual(0, result["failed"])
        self.assertTrue(result["results"][0]["ok"])
        self.assertEqual("BUY", client.created[0]["side"])

    def test_post_limit_orders_dry_run(self) -> None:
        client = DummyClient(should_fail=True)
        specs = [
            {
                "market_id": "m1",
                "token_id": "t1",
                "entry_side": "YES",
                "entry_price": 0.5,
                "stake_usd": 2.0,
                "size": 4.0,
                "expected_value_usd": 0.2,
            }
        ]

        def order_args_ctor(**kwargs):
            return kwargs

        result = live.post_limit_orders(
            order_specs=specs,
            client=client,
            order_args_ctor=order_args_ctor,
            order_type_enum=DummyOrderType,
            buy_side_value="BUY",
            order_type_text="UNSUPPORTED_FOR_DRYRUN",
            dry_run=True,
        )
        self.assertEqual(1, result["attempted"])
        self.assertEqual(1, result["sent"])
        self.assertEqual(0, result["failed"])
        self.assertTrue(result["results"][0]["ok"])
        self.assertEqual([], client.created)

    def test_dry_run_without_private_key(self) -> None:
        specs = [
            {
                "market_id": "m1",
                "token_id": "t1",
                "entry_side": "YES",
                "entry_price": 0.5,
                "stake_usd": 2.0,
                "size": 4.0,
                "expected_value_usd": 0.2,
            }
        ]
        result = live.post_limit_orders(
            order_specs=specs,
            client=None,
            order_args_ctor=None,
            order_type_enum=None,
            buy_side_value=None,
            order_type_text="GTC",
            dry_run=True,
        )
        self.assertEqual(1, result["attempted"])
        self.assertEqual(1, result["sent"])
        self.assertEqual(0, result["failed"])
        self.assertTrue(result["results"][0]["ok"])
        self.assertEqual(True, result["results"][0]["response"]["dry_run"])

    def test_load_runtime_config_requires_private_key(self) -> None:
        with self.assertRaises(ValueError):
            live.load_runtime_config({})


if __name__ == "__main__":
    unittest.main()
