import importlib.util
import sys
import unittest
from pathlib import Path


MODULE_PATH = Path(__file__).with_name("probability_calibration.py")


def load_module():
    assert MODULE_PATH.exists(), f"missing module: {MODULE_PATH}"
    spec = importlib.util.spec_from_file_location("probability_calibration", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class TestProbabilityCalibration(unittest.TestCase):
    def test_module_exists(self) -> None:
        self.assertTrue(MODULE_PATH.exists(), f"{MODULE_PATH} should exist")

    def test_fit_isotonic_and_predict_interpolates(self) -> None:
        mod = load_module()
        calib = mod.fit_isotonic_calibrator(
            samples=[
                (0.1, 0),
                (0.2, 0),
                (0.8, 0),
                (0.9, 1),
            ]
        )
        self.assertEqual("isotonic", str(calib.get("method")))
        points = calib.get("points")
        self.assertIsInstance(points, list)
        self.assertGreaterEqual(len(points), 2)
        xs = [float(p.get("x")) for p in points]
        ys = [float(p.get("y")) for p in points]
        self.assertEqual(xs, sorted(xs))
        self.assertEqual(ys, sorted(ys))

        # Between (0.8 -> 0.0) and (0.9 -> 1.0), linear interpolation should give 0.5 at 0.85.
        self.assertAlmostEqual(0.5, float(mod.apply_probability_calibration(calib, 0.85)), places=6)

    def test_fit_isotonic_includes_endpoints(self) -> None:
        mod = load_module()
        calib = mod.fit_isotonic_calibrator(samples=[(0.3, 1)])
        points = calib.get("points")
        self.assertIsInstance(points, list)
        self.assertGreaterEqual(len(points), 2)
        self.assertEqual(0.0, float(points[0].get("x")))
        self.assertEqual(1.0, float(points[-1].get("x")))
        self.assertEqual(float(points[0].get("y")), float(points[-1].get("y")))


if __name__ == "__main__":
    unittest.main()
