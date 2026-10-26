import importlib.util
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = ROOT / "tools" / "generic_extraction_adapter.py"
SPEC = importlib.util.spec_from_file_location("generic_extraction_adapter", MODULE_PATH)
ADAPTER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ADAPTER)


class RExecutableResolutionTests(unittest.TestCase):
    def test_path_resolved_executable_name_passes_preflight(self):
        resolved = str(Path(tempfile.gettempdir()) / "Rscript.EXE")
        with mock.patch.object(ADAPTER.shutil, "which", return_value=resolved):
            result = ADAPTER.resolve_executable("Rscript")
        self.assertEqual(result, str(Path(resolved).resolve()))
        problems = ADAPTER.validate_request({}, r_executable=result)
        self.assertFalse(any(problem["check_id"] == "environment.r"
                             for problem in problems))

    def test_absolute_valid_executable_path_passes(self):
        with tempfile.TemporaryDirectory() as directory:
            executable = Path(directory) / "R Folder" / "Rscript.exe"
            executable.parent.mkdir()
            executable.touch()
            with mock.patch.object(
                    ADAPTER.shutil, "which", return_value=str(executable)) as lookup:
                result = ADAPTER.resolve_executable(str(executable))
        lookup.assert_called_once_with(str(executable))
        self.assertEqual(result, str(executable.resolve()))
        problems = ADAPTER.validate_request({}, r_executable=result)
        self.assertFalse(any(problem["check_id"] == "environment.r"
                             for problem in problems))

    def test_missing_executable_fails_cleanly(self):
        with mock.patch.object(ADAPTER.shutil, "which", return_value=None):
            result = ADAPTER.resolve_executable("missing-Rscript")
        self.assertIsNone(result)
        problems = ADAPTER.validate_request({}, r_executable=result)
        environment = [problem for problem in problems
                       if problem["check_id"] == "environment.r"]
        self.assertEqual(len(environment), 1)
        self.assertEqual(
            environment[0]["message"],
            "Configured Rscript executable could not be resolved.")

    def test_resolved_executable_is_used_by_subprocess(self):
        resolved = str(Path(tempfile.gettempdir()) / "Rscript.EXE")
        script = str(Path(tempfile.gettempdir()) / "execution copy.R")
        workspace = Path(tempfile.gettempdir()) / "workspace with spaces"
        with mock.patch.object(ADAPTER.shutil, "which", return_value=resolved), \
             mock.patch.object(ADAPTER.subprocess, "run",
                               return_value=subprocess.CompletedProcess([], 0)) as run:
            executable = ADAPTER.resolve_executable("Rscript")
            ADAPTER.run_rscript(executable, script, workspace,
                                mock.sentinel.stdout, mock.sentinel.stderr)
        run.assert_called_once_with(
            [str(Path(resolved).resolve()), script], cwd=workspace,
            stdout=mock.sentinel.stdout, stderr=mock.sentinel.stderr)


if __name__ == "__main__":
    unittest.main()
