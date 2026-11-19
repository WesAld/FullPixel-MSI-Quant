import importlib.util
import hashlib
import json
import math
import subprocess
import sys
import unittest
import tempfile
from pathlib import Path

import pandas as pd
from openpyxl import Workbook, load_workbook

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "jackknife_analysis", ROOT / "tools" / "jackknife_analysis.py")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def mappings(n=3):
    rows = []
    for group in ("CTRL", "TRT"):
        for index in range(1, n + 1):
            rows.append({"internal_value": f"{group}_R{index}",
                         "original_replicate_id": f"R{index}"})
    return rows


class JackknifeAnalysisTests(unittest.TestCase):
    def test_three_pair_hand_reference_and_classical_se(self):
        pairs = MODULE.matched_replicate_pairs(mappings())
        frame = pd.DataFrame({"C1": [1.0], "C2": [2.0], "C3": [4.0],
                              "T1": [2.0], "T2": [8.0], "T3": [16.0]})
        columns = {f"CTRL_R{i}": f"C{i}" for i in range(1, 4)}
        columns.update({f"TRT_R{i}": f"T{i}" for i in range(1, 4)})
        full = pd.Series([math.log2((26 / 3) / (7 / 3))])
        result = MODULE.calculate_jackknife(frame, pairs, columns, full)
        expected = [math.log2(12 / 3), math.log2(9 / 2.5), math.log2(5 / 1.5)]
        observed = [result[f"JK_Omit_R{i}_Log2FC"][0] for i in range(1, 4)]
        for left, right in zip(observed, expected):
            self.assertAlmostEqual(left, right, places=12)
        mean = sum(expected) / 3
        se = math.sqrt((2 / 3) * sum((value - mean) ** 2 for value in expected))
        self.assertAlmostEqual(result.Jackknife_SE[0], se, places=12)
        self.assertTrue(result.Direction_Stable[0])

    def test_four_pairs_are_dynamic_and_pair_by_identity(self):
        pairs = MODULE.matched_replicate_pairs(list(reversed(mappings(4))))
        self.assertEqual([x["replicate_id"] for x in pairs],
                         ["R1", "R2", "R3", "R4"])
        self.assertEqual(len(pairs), 4)

    def test_unmatched_duplicate_missing_and_less_than_three_are_rejected(self):
        with self.assertRaisesRegex(ValueError, "identical matched"):
            MODULE.matched_replicate_pairs(mappings()[:-1])
        duplicate = mappings() + [{"internal_value": "CTRL_extra",
                                   "original_replicate_id": "R1"}]
        with self.assertRaisesRegex(ValueError, "Duplicate"):
            MODULE.matched_replicate_pairs(duplicate)
        with self.assertRaisesRegex(ValueError, "At least 3"):
            MODULE.matched_replicate_pairs(mappings(2))

    def test_missing_estimate_makes_summary_incomplete_without_imputation(self):
        pairs = MODULE.matched_replicate_pairs(mappings())
        frame = pd.DataFrame({"C1": [1.0], "C2": [float("nan")], "C3": [3.0],
                              "T1": [2.0], "T2": [4.0], "T3": [6.0]})
        columns = {f"CTRL_R{i}": f"C{i}" for i in range(1, 4)}
        columns.update({f"TRT_R{i}": f"T{i}" for i in range(1, 4)})
        result = MODULE.calculate_jackknife(frame, pairs, columns,
                                            pd.Series([float("nan")]))
        self.assertLess(result.N_Jackknife_Valid[0], 3)
        self.assertTrue(pd.isna(result.Jackknife_SE[0]))
        self.assertTrue(pd.isna(result.Direction_Stable[0]))

    def test_direction_stability_positive_negative_zero_and_change(self):
        pairs = MODULE.matched_replicate_pairs(mappings())
        columns = {f"CTRL_R{i}": f"C{i}" for i in range(1, 4)}
        columns.update({f"TRT_R{i}": f"T{i}" for i in range(1, 4)})
        stable = pd.DataFrame({**{f"C{i}": [2, 4, 5] for i in range(1, 4)},
                               **{f"T{i}": [4, 2, 5] for i in range(1, 4)}})
        result = MODULE.calculate_jackknife(
            stable, pairs, columns, pd.Series([1.0, -1.0, 0.0]))
        self.assertEqual(list(result.Direction_Stable.astype(object)),
                         [True, True, True])
        changing = pd.DataFrame({"C1": [10], "C2": [1], "C3": [1],
                                 "T1": [1], "T2": [3], "T3": [3]})
        changed = MODULE.calculate_jackknife(
            changing, pairs, columns, pd.Series([-0.5]))
        self.assertFalse(changed.Direction_Stable[0])

    def test_workbook_is_conditional_and_method_aware_for_all_four_methods(self):
        contracts = {
            "raw": ("Primary_RAW_Intensity", "All_Features_RAW", "_RAW_intensity"),
            "spectrum_sum": ("TIC_normalized_intensity",
                             "All_Features_TIC_Normalized",
                             "_TIC_normalized_intensity"),
            "rms": ("Primary_RMS_Intensity", "All_Features_RMS",
                    "_RMS_normalized_intensity"),
            "median": ("Primary_MEDIAN_Intensity", "All_Features_MEDIAN",
                       "_MEDIAN_normalized_intensity"),
        }
        map_rows = []
        for group in ("CTRL", "TRT"):
            for index in range(1, 4):
                map_rows.append({"internal_value": f"{group}_R{index}",
                                 "original_replicate_id": f"R{index}",
                                 "user_facing_label": f"{group} R{index}"})
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for method, (primary, all_features, suffix) in contracts.items():
                source = root / f"{method}.xlsx"
                workbook = Workbook()
                workbook.remove(workbook.active)
                for sheet in ("Mean_Log2FC", "All_Pixels", primary,
                              all_features, "QC", "README"):
                    ws = workbook.create_sheet(sheet)
                    if sheet == all_features:
                        headers = ["dataset_id", "ion_mode", "mzmine_row_id",
                                   "target_mz", "annotation_name"] + [
                            f"{group}_R{i}{suffix}" for group in ("CTRL", "TRT")
                            for i in range(1, 4)] + ["log2FC_TRT_vs_CTRL"]
                        ws.append(headers)
                        ws.append(["d", "NEG", "1", 100.0, "x",
                                   1, 2, 3, 2, 4, 6, 1.0])
                    elif sheet == "README":
                        ws.append(["Field", "Value"])
                        ws.append(["Primary normalization method", method])
                    else:
                        ws.append(["value"]); ws.append([1])
                workbook.save(source)
                source_hash = hashlib.sha256(source.read_bytes()).hexdigest().upper()
                for enabled in (False, True):
                    outdir = root / f"out-{method}-{enabled}"
                    request = {"workflow_id": "wf", "analysis_type": "compare_two_groups",
                               "primary_normalization_method": method,
                               "display_metadata": {"sample_mappings": map_rows},
                               "jackknife": {"enabled": enabled},
                               "source_workbook": {"path": str(source),
                                                   "sha256": source_hash},
                               "output_working_directory": str(outdir),
                               "output_filename": source.name}
                    request_path = root / f"request-{method}-{enabled}.json"
                    report_path = root / f"report-{method}-{enabled}.json"
                    request_path.write_text(json.dumps(request), encoding="utf-8")
                    completed = subprocess.run([
                        sys.executable, str(ROOT / "tools" /
                            "jackknife_workbook_adapter.py"), str(request_path),
                        "--report", str(report_path)], capture_output=True, text=True)
                    self.assertEqual(completed.returncode, 0, completed.stderr)
                    report = json.loads(report_path.read_text(encoding="utf-8"))
                    output = Path(report["output_workbook"]["path"])
                    checked = load_workbook(output, read_only=True, data_only=True)
                    expected = ["Mean_Log2FC"] + (["Jackknife_Log2FC"] if enabled else []) + [
                        "All_Pixels", primary, all_features, "QC", "README"]
                    self.assertEqual(checked.sheetnames, expected)
                    self.assertEqual(checked["Mean_Log2FC"]["A2"].value, 1)
                    if enabled:
                        headers = [cell.value for cell in
                                   next(checked["Jackknife_Log2FC"].iter_rows())]
                        self.assertIn("JK_Omit_R1_Log2FC", headers)
                        self.assertEqual(report["primary_normalization_method"], method)
                    checked.close()


if __name__ == "__main__":
    unittest.main()
