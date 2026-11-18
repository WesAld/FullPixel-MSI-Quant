import hashlib
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import pandas as pd
from openpyxl import Workbook, load_workbook


ROOT = Path(__file__).resolve().parents[2]
ADAPTER = ROOT / "tools" / "all_features_comparison_adapter.py"


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest().upper()


class AllFeaturesComparisonAdapterTests(unittest.TestCase):
    methods = {
        "raw": ("Primary_RAW_Intensity", "All_Features_RAW",
                "_RAW_intensity"),
        "spectrum_sum": ("TIC_normalized_intensity",
                         "All_Features_TIC_Normalized",
                         "_TIC_normalized_intensity"),
        "rms": ("Primary_RMS_Intensity", "All_Features_RMS",
                "_RMS_normalized_intensity"),
        "median": ("Primary_MEDIAN_Intensity", "All_Features_MEDIAN",
                   "_MEDIAN_normalized_intensity"),
    }

    def make_source(self, directory, intensity_sheet, method):
        path = directory / "source.xlsx"
        workbook = Workbook()
        workbook.remove(workbook.active)
        for name in ["Mean_Log2FC", "All_Pixels", intensity_sheet,
                     "QC", "README"]:
            worksheet = workbook.create_sheet(name)
            worksheet.append(["Field", "Value"])
            if name == "README":
                worksheet.append(["Reference condition", "CTRL"])
                worksheet.append(["Comparison condition", "TRT"])
                worksheet.append(["Primary normalization method", method])
            else:
                worksheet.append(["x", 1])
        workbook.save(path)
        return path

    def make_master(self, directory):
        path = directory / "master.csv"
        frame = pd.DataFrame({
            "dataset_id": ["dataset"],
            "ion_mode": ["POS"],
            "mzmine_row_id": ["1"],
            "target_mz": [300.1234],
            "annotation_name": ["Test feature"],
            "annotation_score": [0.9],
            "CTRL_R1_overall_normalized_signal": [10.0],
            "CTRL_R2_overall_normalized_signal": [20.0],
            "TRT_R1_overall_normalized_signal": [20.0],
            "TRT_R2_overall_normalized_signal": [40.0],
            "log2FC_overall_TRT_vs_CTRL": [1.0],
        })
        frame.to_csv(path, index=False)
        return path

    def run_adapter(self, method, intensity_sheet, output_name="output"):
        temporary = tempfile.TemporaryDirectory()
        directory = Path(temporary.name)
        source = self.make_source(directory, intensity_sheet, method)
        master = self.make_master(directory)
        request = {
            "workflow_id": "workflow-test",
            "analysis_type": "compare_two_groups",
            "display_metadata": {
                "reference_condition": "CTRL",
                "comparison_condition": "TRT",
                "normalization": {
                    "primary_normalization_method": method,
                    "normalization_scale": 1_000_000,
                },
            },
            "source_workbook": {"path": str(source),
                                "sha256": sha256(source)},
            "masters": [{"path": str(master), "sha256": sha256(master),
                         "ion_mode": "POS"}],
            "output_filename": "result.xlsx",
            "output_working_directory": str(directory / output_name),
        }
        request_path = directory / "request.json"
        report_path = directory / "report.json"
        request_path.write_text(json.dumps(request), encoding="utf-8")
        completed = subprocess.run(
            [sys.executable, str(ADAPTER), str(request_path), "--report",
             str(report_path)], capture_output=True, text=True, check=False)
        report = json.loads(report_path.read_text(encoding="utf-8"))
        return temporary, completed, report

    def test_all_four_methods_use_their_primary_sheet_and_labels(self):
        for method, (primary, all_features, suffix) in self.methods.items():
            with self.subTest(method=method):
                temporary, completed, report = self.run_adapter(method, primary)
                try:
                    self.assertEqual(completed.returncode, 0, report)
                    self.assertEqual(report["status"], "PASS")
                    self.assertEqual(report["primary_normalization_method"], method)
                    self.assertEqual(report["expected_primary_intensity_sheet"],
                                     primary)
                    self.assertEqual(report["sheet_name"], all_features)
                    workbook = load_workbook(
                        report["final_workbook"]["path"], read_only=True)
                    self.assertEqual(
                        workbook.sheetnames,
                        ["Mean_Log2FC", "All_Pixels", primary, all_features,
                         "QC", "README"])
                    headers = [cell.value for cell in next(
                        workbook[all_features].iter_rows(min_row=1, max_row=1))]
                    readme = {row[0].value: row[1].value for row in
                              workbook["README"].iter_rows(min_row=1,
                                                           max_col=2)}
                    workbook.close()
                    self.assertIn("CTRL_R1" + suffix, headers)
                    self.assertIn("mean_CTRL" + suffix, headers)
                    self.assertEqual(readme["Primary normalization method"],
                                     method)
                    if method != "spectrum_sum":
                        self.assertNotIn("TIC_normalized_intensity",
                                         " ".join(headers))
                finally:
                    temporary.cleanup()

    def test_missing_primary_sheet_reports_method_and_expected_sheet(self):
        temporary, completed, report = self.run_adapter(
            "raw", "TIC_normalized_intensity")
        try:
            self.assertNotEqual(completed.returncode, 0)
            self.assertEqual(report["status"], "FAIL")
            self.assertIn("normalization method 'raw'", report["error"])
            self.assertIn("Primary_RAW_Intensity", report["error"])
        finally:
            temporary.cleanup()

    def test_unsupported_method_has_clear_diagnostic(self):
        temporary, completed, report = self.run_adapter(
            "unsupported", "Primary_RAW_Intensity")
        try:
            self.assertNotEqual(completed.returncode, 0)
            self.assertEqual(report["status"], "FAIL")
            self.assertIn("Unsupported primary_normalization_method: unsupported",
                          report["error"])
        finally:
            temporary.cleanup()


if __name__ == "__main__":
    unittest.main()
