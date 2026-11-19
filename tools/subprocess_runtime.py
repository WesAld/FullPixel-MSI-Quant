"""Safe operating-system working directories for external processes.

Scientific inputs, scripts, outputs, and reports remain absolute project paths.
Only the OS current working directory is isolated from project and install depth.
"""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import time
import uuid
from pathlib import Path


RUNTIME_ENV = "MALDIPIXELQUANT_RUNTIME_DIR"
STALE_AFTER_SECONDS = 7 * 24 * 60 * 60


def runtime_root() -> Path:
    configured = os.environ.get(RUNTIME_ENV, "").strip()
    if configured:
        root = Path(configured).expanduser()
    else:
        local = os.environ.get("LOCALAPPDATA", "").strip()
        base = Path(local) if local else Path(tempfile.gettempdir())
        root = base / "MALDIPixelQuant" / "runtime"
    root.mkdir(parents=True, exist_ok=True)
    if not root.is_dir():
        raise RuntimeError(f"MALDIPixelQuant runtime path is not a directory: {root}")
    return root.resolve()


def cleanup_stale_runtime_directories(root: Path | None = None) -> None:
    root = runtime_root() if root is None else Path(root)
    cutoff = time.time() - STALE_AFTER_SECONDS
    for candidate in root.glob("r*"):
        try:
            if candidate.is_dir() and candidate.stat().st_mtime < cutoff:
                shutil.rmtree(candidate)
        except OSError:
            # Active, locked, or otherwise unavailable runtime directories are
            # never forced. They can be reconsidered on a later launch.
            continue


def create_runtime_cwd() -> Path:
    root = runtime_root()
    cleanup_stale_runtime_directories(root)
    token = uuid.uuid4().hex[:8]
    cwd = root / f"r{os.getpid()}-{token}"
    cwd.mkdir()
    return cwd


def run_external_process(command, *, scientific_script=None,
                         project_path=None, **kwargs):
    """Run a command from a short unique cwd and remove that cwd afterward."""
    cwd = create_runtime_cwd()
    try:
        return subprocess.run(command, cwd=cwd, **kwargs)
    except OSError as exc:
        executable = str(command[0]) if command else ""
        details = (
            f"External process could not start: executable={executable!r}; "
            f"safe_cwd={str(cwd)!r}; scientific_script={str(scientific_script or '')!r}; "
            f"project_path={str(project_path or '')!r}; os_error={exc}"
        )
        raise RuntimeError(details) from exc
    finally:
        try:
            shutil.rmtree(cwd)
        except OSError:
            # A crashed child can temporarily retain a handle. The stale-age
            # cleanup policy removes abandoned directories later.
            pass
