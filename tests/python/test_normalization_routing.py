import importlib.util
import math
import tempfile
import unittest
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "normalization_routing", ROOT / "tools" / "normalization_routing.py")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class NormalizationRoutingTests(unittest.TestCase):
    def test_default_and_allowed_methods(self):
        self.assertEqual(MODULE.resolve_method({}), "spectrum_sum")
        self.assertEqual(MODULE.METHODS,
                         ("raw", "spectrum_sum", "rms", "median"))
        with self.assertRaises(ValueError):
            MODULE.resolve_method({"primary_normalization_method": "bad"})

    def test_each_method_routes_the_corresponding_columns(self):
        frame = pd.DataFrame({
            "raw_mean_all_valid_pixels": [1.0],
            "raw_mean_conservative_detected_pixels": [2.0],
            "normalized_mean_all_valid_pixels": [3.0],
            "normalized_mean_conservative_detected_pixels": [4.0],
            "rms_normalized_mean_all_valid_pixels": [5.0],
            "rms_normalized_mean_conservative_detected_pixels": [6.0],
            "median_normalized_mean_all_valid_pixels": [7.0],
            "median_normalized_mean_conservative_detected_pixels": [8.0],
        })
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "source.csv"
            frame.to_csv(source, index=False)
            expected = {"raw": (1, 2), "spectrum_sum": (3, 4),
                        "rms": (5, 6), "median": (7, 8)}
            for method, values in expected.items():
                output = Path(directory) / f"{method}.csv"
                MODULE.route_extraction_summary(source, output, method)
                observed = pd.read_csv(output)
                self.assertEqual(observed.normalized_mean_all_valid_pixels[0], values[0])
                self.assertEqual(observed.normalized_mean_conservative_detected_pixels[0], values[1])
                self.assertEqual(observed.primary_normalization_method[0], method)

    def test_provenance_is_method_explicit(self):
        for method in MODULE.METHODS:
            record = MODULE.provenance(method, 1_000_000)
            self.assertEqual(record["primary_normalization_method"], method)
            self.assertEqual(record["application_version"], "2.0.0-dev")
            self.assertEqual(record["normalization_scale"], 1_000_000)


if __name__ == "__main__":
    unittest.main()
