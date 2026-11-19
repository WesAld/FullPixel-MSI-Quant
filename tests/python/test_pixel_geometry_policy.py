import copy
import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "generic_extraction_adapter", ROOT / "tools" / "generic_extraction_adapter.py")
ADAPTER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ADAPTER)


class PixelGeometryPolicyTests(unittest.TestCase):
    def test_missing_policy_defaults_to_strict(self):
        self.assertEqual(ADAPTER.pixel_geometry_policy({}), "strict_predefined")

    def test_sparse_policy_does_not_require_declared_count(self):
        request = {"pixel_metadata": {"pixel_geometry_policy": "sparse_observed"}}
        problems = ADAPTER.validate_request(request, check_environment=False)
        self.assertFalse(any(p["check_id"] == "acquisition.coordinates"
                             for p in problems))

    def test_strict_policy_requires_declared_count(self):
        request = {"pixel_metadata": {"pixel_geometry_policy": "strict_predefined"}}
        problems = ADAPTER.validate_request(request, check_environment=False)
        self.assertTrue(any(p["check_id"] == "acquisition.coordinates"
                            for p in problems))

    def test_generated_copy_keeps_guard_strict_only_and_writes_geometry_qc(self):
        request = json.loads((ROOT / "config" / "examples" /
            "pvl_single_negative_extraction.execution_request.json").read_text())
        with tempfile.TemporaryDirectory() as directory:
            temporary = Path(directory)
            target = temporary / "targets.csv"
            target.write_text("id,mz\nfeature-1,100.1\n", encoding="utf-8")
            request["registered_inputs"]["aligned_target_table"]["path"] = str(target)
            request["column_mappings"] = {"feature_id": "id", "target_mz": "mz"}
            for policy in ("strict_predefined", "sparse_observed"):
                candidate = copy.deepcopy(request)
                candidate["pixel_metadata"]["pixel_geometry_policy"] = policy
                workspace = temporary / policy
                script, transformations, _ = ADAPTER.make_copy(candidate, workspace)
                generated = script.read_text(encoding="utf-8")
                self.assertIn(
                    'if (identical(pixel_geometry_policy, "strict_predefined") &&\n'
                    "      inspect$n_coords != expected_coordinate_pixels)", generated)
                self.assertIn('file.path(dirs$run, "sample_geometry_qc.csv")', generated)
                self.assertIn("missing_positions_within_extent", generated)
                self.assertEqual(transformations["pixel_geometry_policy"], policy)

    def test_all_request_builders_write_policy(self):
        for relative in ("R/full_workflow_orchestration_service.R",
                         "R/zz_quantify_only_mode_service.R",
                         "R/zzzz_multi_group_mode_service.R"):
            source = (ROOT / relative).read_text(encoding="utf-8")
            self.assertIn("request$pixel_metadata$pixel_geometry_policy", source)


if __name__ == "__main__":
    unittest.main()
