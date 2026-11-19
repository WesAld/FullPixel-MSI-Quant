import importlib.util
import os
import subprocess
import sys
import tempfile
import time
import unittest
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
TOOLS = ROOT / "tools"
sys.path.insert(0, str(TOOLS))
from subprocess_runtime import (  # noqa: E402
    cleanup_stale_runtime_directories,
    create_runtime_cwd,
    run_external_process,
    runtime_root,
)


class SafeSubprocessRuntimeTests(unittest.TestCase):
    def test_deep_project_and_application_paths_use_short_runtime_cwd(self):
        with tempfile.TemporaryDirectory() as directory:
            base = Path(directory)
            deep_app = base
            deep_project = base
            while len(str(deep_app)) <= 290:
                deep_app = deep_app / "deep_application_install_segment"
            while len(str(deep_project)) <= 300:
                deep_project = deep_project / "deep_project_run_output_segment"
            deep_app.mkdir(parents=True)
            deep_project.mkdir(parents=True)
            script = deep_app / "absolute_worker.py"
            source = deep_project / "absolute_input.txt"
            output = deep_project / "absolute_output.txt"
            cwd_record = deep_project / "runtime_cwd.txt"
            source.write_text("absolute scientific input", encoding="utf-8")
            script.write_text(
                "import os, pathlib, sys\n"
                "src, out, cwd = map(pathlib.Path, sys.argv[1:])\n"
                "out.write_text(src.read_text(encoding='utf-8'), encoding='utf-8')\n"
                "cwd.write_text(os.getcwd(), encoding='utf-8')\n",
                encoding="utf-8",
            )
            result = run_external_process(
                [sys.executable, str(script), str(source), str(output),
                 str(cwd_record)],
                scientific_script=script, project_path=deep_project,
                check=False,
            )
            self.assertEqual(result.returncode, 0)
            self.assertEqual(output.read_text(encoding="utf-8"),
                             "absolute scientific input")
            observed = Path(cwd_record.read_text(encoding="utf-8"))
            self.assertEqual(observed.parent, runtime_root())
            self.assertNotEqual(observed, deep_project)
            self.assertLess(len(str(observed)), 260)
            self.assertFalse(observed.exists())

    def test_sequential_launches_get_unique_directories(self):
        observed = []

        def capture(command, cwd, **kwargs):
            observed.append(Path(cwd))
            return subprocess.CompletedProcess(command, 0)

        with mock.patch("subprocess_runtime.subprocess.run", side_effect=capture):
            run_external_process(["first"])
            run_external_process(["second"])
        self.assertEqual(len(observed), 2)
        self.assertNotEqual(observed[0], observed[1])
        self.assertTrue(all(path.parent == runtime_root() for path in observed))
        self.assertTrue(all(not path.exists() for path in observed))

    def test_concurrent_launches_do_not_collide(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            script = directory / "record_cwd.py"
            script.write_text(
                "import os, pathlib, sys, time\n"
                "time.sleep(0.2)\n"
                "pathlib.Path(sys.argv[1]).write_text(os.getcwd(), encoding='utf-8')\n",
                encoding="utf-8",
            )
            outputs = [directory / "cwd1.txt", directory / "cwd2.txt"]

            def launch(output):
                return run_external_process(
                    [sys.executable, str(script), str(output)],
                    scientific_script=script, project_path=directory,
                    check=True,
                )

            with ThreadPoolExecutor(max_workers=2) as pool:
                list(pool.map(launch, outputs))
            observed = [Path(path.read_text(encoding="utf-8"))
                        for path in outputs]
            self.assertNotEqual(observed[0], observed[1])
            self.assertTrue(all(path.parent == runtime_root()
                                for path in observed))
            self.assertTrue(all(not path.exists() for path in observed))

    def test_stale_cleanup_preserves_recent_runtime_directory(self):
        root = runtime_root()
        stale = root / "r-stale-test"
        recent = root / "r-recent-test"
        stale.mkdir(exist_ok=True)
        recent.mkdir(exist_ok=True)
        old = time.time() - 8 * 24 * 60 * 60
        os.utime(stale, (old, old))
        cleanup_stale_runtime_directories(root)
        self.assertFalse(stale.exists())
        self.assertTrue(recent.exists())
        recent.rmdir()

    def test_all_scientific_adapters_use_central_launcher(self):
        adapters = [
            "generic_extraction_adapter.py", "generic_script03_adapter.py",
            "generic_script04_adapter.py", "script07_adapter.py",
            "pvl_adapter.py", "script03_adapter.py", "script04_adapter.py",
            "multi_group_summary_adapter.py",
        ]
        for name in adapters:
            source = (TOOLS / name).read_text(encoding="utf-8")
            self.assertIn("run_external_process", source, name)
            self.assertNotIn("subprocess.run(", source, name)

    def test_launch_error_contains_technical_context(self):
        with mock.patch("subprocess_runtime.subprocess.run",
                        side_effect=OSError(267, "invalid directory")):
            with self.assertRaisesRegex(
                    RuntimeError,
                    "executable=.*safe_cwd=.*scientific_script=.*project_path=.*os_error"):
                run_external_process(
                    ["Rscript", "deep/script.R"],
                    scientific_script="deep/script.R",
                    project_path="deep/project/run",
                )


if __name__ == "__main__":
    unittest.main()
