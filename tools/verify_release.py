#!/usr/bin/env python3
"""Verify the public FullPixel MSI Quant release without scientific execution."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
import py_compile
import re
import sys
import tempfile

import yaml


ROOT = Path(__file__).resolve().parents[1]

EXPECTED_FROZEN = {
    "validated-source/01_extract_pixel_intensities_PUBLICATION_READY.R":
        "D846E8BB354B3BED88A321B8D61FE0BE2F5FE42943C3365F12AAFD6C47F96CB0",
    "validated-source/03_three_readout_replicate_summary_PUBLICATION_READY.R":
        "3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97",
    "validated-source/04_merge_annotations_three_readouts_PUBLICATION_READY.R":
        "1150E5F10565B7C25784A9DD875380ACC748C95A8D5BE7B0431C1C5767C587D1",
    "validated-source/07_export_three_graphs_to_graphpad_xlsx_VALIDATED_PUBLICATION_READY.R":
        "94055F3210D95D6AF0EA35D246D9F234A24FF429496E179DA91EE58A1FBBB0A0",
}

REQUIRED = [
    "README.md",
    "DESCRIPTION",
    "LICENSE",
    "CITATION.cff",
    "CHANGELOG.md",
    "VERSION",
    "requirements.txt",
    ".gitignore",
    ".gitattributes",
    "app/app.R",
    "tools/launch_app.R",
    "tools/fpq_dispatch_worker.R",
    "tools/fpq_full_workflow_worker.R",
    "config/scientific_core_manifest.yml",
]

FORBIDDEN_SUFFIXES = {
    ".imzml", ".ibd", ".raw", ".mzml", ".mzxml", ".mgf",
    ".xlsx", ".xls", ".zip", ".7z", ".rar", ".pyc", ".pyo",
}

TEXT_SUFFIXES = {
    ".r", ".rmd", ".md", ".txt", ".json", ".yaml", ".yml",
    ".toml", ".ini", ".cfg", ".ps1", ".bat", ".cmd", ".py",
    ".js", ".html", ".css", ".cff",
}

LOCAL_PATTERNS = [
    re.compile(r"[A-Za-z]:\\Users\\", re.IGNORECASE),
    re.compile(r"[A-Za-z]:/Users/", re.IGNORECASE),
    re.compile(r"D:\\Github\\", re.IGNORECASE),
    re.compile(r"D:/Github/", re.IGNORECASE),
    re.compile(r"D:\\FullPixel", re.IGNORECASE),
    re.compile(r"D:/FullPixel", re.IGNORECASE),
    re.compile(r"OneDrive", re.IGNORECASE),
]


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest().upper()


def main() -> int:
    failures: list[str] = []

    for relative in REQUIRED:
        path = ROOT / relative
        if not path.is_file():
            failures.append(f"Missing required release file: {relative}")

    version_path = ROOT / "VERSION"
    if version_path.is_file() and version_path.read_text(encoding="utf-8").strip() != "1.1.1":
        failures.append("VERSION is not 1.1.1.")

    description_path = ROOT / "DESCRIPTION"
    if description_path.is_file():
        description = description_path.read_text(encoding="utf-8")
        if not re.search(r"^Version:\s*1\.1\.1\s*$", description, re.MULTILINE):
            failures.append("DESCRIPTION does not declare Version 1.1.1.")
        if "maintainers@example.invalid" in description:
            failures.append("DESCRIPTION still contains the placeholder maintainer.")

    for relative, expected in EXPECTED_FROZEN.items():
        path = ROOT / relative
        if not path.is_file():
            failures.append(f"Missing frozen scientific source: {relative}")
            continue
        observed = sha256(path)
        if observed != expected:
            failures.append(
                f"Frozen scientific source hash mismatch: {relative} "
                f"(observed {observed}, expected {expected})"
            )

    for path in ROOT.rglob("*"):
        if not path.is_file():
            continue

        relative = path.relative_to(ROOT).as_posix()
        suffix = path.suffix.lower()

        if suffix in FORBIDDEN_SUFFIXES:
            failures.append(f"Forbidden generated/data file in release: {relative}")

        if "__pycache__" in path.parts:
            failures.append(f"Python cache in release: {relative}")

        if suffix == ".py":
            try:
                with tempfile.TemporaryDirectory(prefix="fpq-pycompile-") as directory:
                    target = Path(directory) / (path.stem + ".pyc")
                    py_compile.compile(str(path), cfile=str(target), doraise=True)
            except Exception as exc:
                failures.append(f"Python syntax failure {relative}: {exc}")

        if suffix == ".json":
            try:
                json.loads(path.read_text(encoding="utf-8"))
            except Exception as exc:
                failures.append(f"Invalid JSON {relative}: {exc}")

        if suffix in {".yml", ".yaml", ".cff"}:
            try:
                yaml.safe_load(path.read_text(encoding="utf-8"))
            except Exception as exc:
                failures.append(f"Invalid YAML {relative}: {exc}")

        if (
            suffix in TEXT_SUFFIXES
            and relative != "tools/verify_release.py"
            and not relative.startswith("validated-source/")
        ):
            text = path.read_text(encoding="utf-8", errors="replace")
            for pattern in LOCAL_PATTERNS:
                if pattern.search(text):
                    failures.append(
                        f"Machine-specific path pattern in public file: {relative}"
                    )
                    break

    manifest_path = ROOT / "config" / "scientific_core_manifest.yml"
    if manifest_path.is_file():
        manifest = yaml.safe_load(manifest_path.read_text(encoding="utf-8"))
        for entry in manifest.get("scripts", []):
            if "original_source_path" in entry:
                failures.append(
                    "Public scientific-core manifest contains original_source_path."
                )

    if failures:
        print("\n".join(f"FAIL: {item}" for item in failures), file=sys.stderr)
        return 1

    print("RELEASE_REQUIRED_FILES: PASS")
    print("FROZEN_SCIENTIFIC_HASHES: PASS")
    print("PYTHON_SYNTAX: PASS")
    print("JSON_YAML_SYNTAX: PASS")
    print("PUBLIC_PATH_PRIVACY_SCAN: PASS")
    print("FORBIDDEN_DATA_FILE_SCAN: PASS")
    print("FULLPIXEL MSI QUANT RELEASE 1.1.1: PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
