import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]


def load(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / "tools" / name)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


MULTI = load("multi_group_summary_adapter.py")
GENERIC = load("generic_script03_adapter.py")
STRICT = load("script03_adapter.py")


class Script03ExecutableResolutionTests(unittest.TestCase):
    def test_path_name_resolves_for_all_script03_routes(self):
        resolved = str(Path(tempfile.gettempdir()) / "Rscript.EXE")
        for adapter in (MULTI, GENERIC, STRICT):
            with self.subTest(adapter=adapter.__name__), \
                 mock.patch.object(adapter.shutil, "which", return_value=resolved):
                self.assertEqual(adapter.resolve_executable("Rscript"),
                                 str(Path(resolved).resolve()))

    def test_absolute_existing_path_is_preserved(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / "R Folder" / "Rscript.exe"
            executable.parent.mkdir()
            executable.touch()
            for adapter in (MULTI, GENERIC, STRICT):
                with self.subTest(adapter=adapter.__name__):
                    self.assertEqual(adapter.resolve_executable(str(executable)),
                                     str(executable.resolve()))

    def test_missing_executable_produces_environment_failure(self):
        for adapter in (MULTI, GENERIC, STRICT):
            with self.subTest(adapter=adapter.__name__), \
                 mock.patch.object(adapter.shutil, "which", return_value=None):
                problems = adapter.validate_request(
                    {"environment_requirements": {
                        "r_executable": "missing-Rscript"}},
                    r_executable=adapter.resolve_executable("missing-Rscript"))
                environment = [item for item in problems
                               if item["check_id"] == "environment.r"]
                self.assertEqual(len(environment), 1)
                self.assertEqual(environment[0]["message"],
                                 "Configured Rscript is unavailable.")

    def test_four_by_four_candidate_preflight_and_invalid_artifact(self):
        with tempfile.TemporaryDirectory() as directory:
            temporary = Path(directory)
            source = temporary / "summary.csv"
            header = [
                "dataset_id", "target_id", "mzmine_row_id", "target_mz",
                "sample_id", "condition", "normalized_mean_all_valid_pixels",
                "normalized_mean_conservative_detected_pixels",
                "conservative_detected_pixel_count",
                "conservative_detection_fraction",
                "primary_quantification_eligible", "primary_qc_status",
            ]
            rows = []
            for group in range(1, 5):
                for replicate in range(1, 5):
                    rows.append(",".join([
                        "dataset", "target-1", "1", "100.1",
                        f"G{group:03d}_R{replicate}", f"G{group:03d}",
                        "1", "1", "1", "1", "TRUE", "eligible",
                    ]))
            source.write_text(",".join(header) + "\n" + "\n".join(rows) + "\n")
            conditions = [{"condition_id": f"G{i:03d}",
                           "analytical_role": "reference" if i == 1 else "comparison"}
                          for i in range(1, 5)]
            samples = [{"sample_id": f"G{group:03d}_R{replicate}",
                        "condition_id": f"G{group:03d}"}
                       for group in range(1, 5) for replicate in range(1, 5)]
            comparisons = [{"reference_condition_id": "G001",
                            "comparison_condition_id": f"G{i:03d}"}
                           for i in range(2, 5)]
            request = {
                "stage_id": "summarize_replicate_readouts",
                "execution_adapter": "generic_multi_group",
                "conditions": conditions, "samples": samples,
                "comparisons": comparisons,
                "registered_inputs": {"pixel_intensity_artifact": {
                    "path": str(source), "sha256": MULTI.sha(source),
                    "active_status": "active", "validation_status": "PASS"}},
                "frozen_script": {"repository_relative_path":
                    "validated-source/03_three_readout_replicate_summary_PUBLICATION_READY.R"},
                "output_working_directory": str(temporary / "output"),
            }
            self.assertEqual(MULTI.validate_request(
                request, check_environment=False), [])
            request["registered_inputs"]["pixel_intensity_artifact"]["sha256"] = "0" * 64
            problems = MULTI.validate_request(request, check_environment=False)
            self.assertTrue(any(item["check_id"] == "input.hash"
                                for item in problems))

    def test_candidate_equivalence_contract_remains_not_evaluable(self):
        source = (ROOT / "tools" / "multi_group_summary_adapter.py").read_text()
        self.assertIn('"status": "NOT_EVALUABLE"', source)
        dispatcher = (ROOT / "R" / "script03_dispatch_service.R").read_text()
        self.assertIn('identical(report$equivalence$status %||% "", "NOT_EVALUABLE")',
                      dispatcher)

    def test_validated_route_still_requires_regression_equivalence(self):
        source = (ROOT / "tools" / "script03_adapter.py").read_text()
        self.assertIn('problems.append(issue("reference.missing"', source)
        self.assertIn('equivalence["status"] != "PASS"', source)

    def test_short_pair_execution_paths_are_independent_of_long_request_path(self):
        long_requested = Path("D:/") / ("long-project-name/" * 12) / (
            "summary-resume-20260831130732.007-683630")
        self.assertGreater(len(str(long_requested)), 246)
        with tempfile.TemporaryDirectory(prefix="fpq-s03-deadbeef-") as directory:
            root = Path(directory)
            pair = root / MULTI.pair_execution_name(1)
            dataset = MULTI.pair_execution_dataset_id(1)
            longest = (pair / "analysis" / "03_Replicate_Summaries" / dataset /
                       "ThreeReadout_ReplicateSummary_20260831_150000" /
                       f"{dataset}_ThreeReadout_Summary.xlsx")
            self.assertLess(len(str(longest)), 240)
            self.assertEqual(pair.name, "p001")
            self.assertEqual(dataset, "S03_P001")

    def test_short_pair_evidence_preserves_comparison_provenance(self):
        source = (ROOT / "tools" / "multi_group_summary_adapter.py").read_text()
        for field in ("comparison_id", "reference_condition_id",
                      "comparison_condition_id", "reference_token",
                      "comparison_token", "internal_execution_directory",
                      "internal_dataset_id"):
            self.assertIn(f'"{field}"', source)
        self.assertIn("pairwise_comparisons=pair_reports", source)


if __name__ == "__main__":
    unittest.main()
