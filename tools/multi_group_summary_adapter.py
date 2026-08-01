#!/usr/bin/env python3
"""Run frozen Script 03 once per reference-versus-treatment comparison and merge outputs."""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import subprocess
from datetime import datetime
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
SCRIPT_HASH = "3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97"
BOUNDARY = '\n###############################################################################\n\nrequired_packages <- c("data.table", "openxlsx")'


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()


def sha_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest().upper()


def issue(code: str, message: str, category: str = "configuration") -> dict:
    return {
        "check_id": code,
        "status": "FAIL",
        "category": category,
        "message": message,
        "blocking": True,
    }


def rquote(value: object) -> str:
    return '"' + str(value).replace("\\", "/").replace('"', '\\"') + '"'


def rvec(values: list[str]) -> str:
    return "c(" + ", ".join(rquote(value) for value in values) + ")"


def safe(value: object, fallback: str = "Group") -> str:
    token = re.sub(r"[^A-Za-z0-9]+", "_", str(value or "")).strip("_")
    return (token or fallback)[:64]


def read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        return reader.fieldnames or [], list(reader)


def write_pair_input(
    pair: pd.DataFrame,
    pair_input: Path,
    pair_dataset_id: str,
) -> pd.DataFrame:
    """Write a Script 03 pair input with the same dataset ID as its config."""
    prepared = pair.copy()
    prepared["dataset_id"] = pair_dataset_id

    observed = {
        str(value).strip()
        for value in prepared["dataset_id"].dropna().tolist()
        if str(value).strip()
    }
    if observed != {pair_dataset_id}:
        raise RuntimeError(
            "Pair input dataset_id rewrite did not produce one "
            "pair-specific identity."
        )

    prepared.to_csv(pair_input, index=False)
    return prepared


def prepare_retry_workspace(workspace: Path) -> Path | None:
    """Create a clean stage workspace while preserving a failed attempt."""
    archived: Path | None = None

    if workspace.exists():
        if not workspace.is_dir():
            raise RuntimeError(
                f"Summary workspace exists but is not a directory: {workspace}"
            )

        if any(workspace.iterdir()):
            timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
            archived = workspace.with_name(
                f"{workspace.name}.failed-attempt-{timestamp}"
            )
            counter = 2
            while archived.exists():
                archived = workspace.with_name(
                    f"{workspace.name}.failed-attempt-{timestamp}-{counter}"
                )
                counter += 1
            workspace.rename(archived)
        else:
            workspace.rmdir()

    workspace.mkdir(parents=True, exist_ok=False)
    return archived


def validate_request(req: dict, check_environment: bool = True) -> list[dict]:
    problems: list[dict] = []
    if req.get("stage_id") != "summarize_replicate_readouts":
        problems.append(issue(
            "stage.unsupported",
            "Expected summarize_replicate_readouts.",
            "scientific",
        ))
    if req.get("execution_adapter") != "generic_multi_group":
        problems.append(issue(
            "adapter.unsupported",
            "Expected generic_multi_group execution adapter.",
            "scientific",
        ))

    conditions = req.get("conditions", [])
    references = [
        item for item in conditions
        if item.get("analytical_role") == "reference"
    ]
    treatments = [
        item for item in conditions
        if item.get("analytical_role") == "comparison"
    ]
    comparisons = req.get("comparisons", [])
    if len(conditions) < 3 or len(references) != 1 or len(treatments) < 2:
        problems.append(issue(
            "design.conditions",
            "One reference and at least two comparison conditions are required.",
            "scientific",
        ))
    if len(comparisons) != len(treatments):
        problems.append(issue(
            "design.comparisons",
            "Every comparison condition requires one reference comparison.",
            "scientific",
        ))

    samples = req.get("samples", [])
    sample_ids = [str(item.get("sample_id", "")).strip() for item in samples]
    if any(not value for value in sample_ids) or len(sample_ids) != len(set(sample_ids)):
        problems.append(issue(
            "design.sample_ids",
            "Sample identifiers must be nonblank and unique.",
            "scientific",
        ))
    grouped: dict[str, list[dict]] = {}
    for sample in samples:
        grouped.setdefault(sample.get("condition_id"), []).append(sample)
    configured_ids = {item.get("condition_id") for item in conditions}
    if set(grouped) != configured_ids or any(len(rows) < 2 for rows in grouped.values()):
        problems.append(issue(
            "design.replicates",
            "Every configured group requires at least two samples.",
            "scientific",
        ))

    reference_id = references[0].get("condition_id") if references else None
    treatment_ids = {item.get("condition_id") for item in treatments}
    seen_treatments: set[str] = set()
    for comparison in comparisons:
        if comparison.get("reference_condition_id") != reference_id:
            problems.append(issue(
                "design.reference",
                "Every comparison must use the configured reference group.",
                "scientific",
            ))
        treatment = comparison.get("comparison_condition_id")
        if treatment not in treatment_ids or treatment in seen_treatments:
            problems.append(issue(
                "design.treatment",
                "Comparison groups must be configured, distinct, and unique.",
                "scientific",
            ))
        seen_treatments.add(treatment)

    artifact = req.get("registered_inputs", {}).get(
        "pixel_intensity_artifact", {}
    )
    path = Path(artifact.get("path", ""))
    if not path.is_file():
        problems.append(issue(
            "input.summary",
            "Pixel-intensity artifact is missing.",
            "input",
        ))
    elif sha(path) != str(artifact.get("sha256", "")).upper():
        problems.append(issue(
            "input.hash",
            "Pixel-intensity artifact hash differs from its registered identity.",
            "scientific",
        ))
    elif artifact.get("active_status") != "active" or artifact.get(
        "validation_status"
    ) != "PASS":
        problems.append(issue(
            "input.inactive",
            "Pixel-intensity artifact must be active and validated.",
            "scientific",
        ))
    else:
        header, rows = read_csv(path)
        required = {
            "dataset_id",
            "target_id",
            "mzmine_row_id",
            "target_mz",
            "sample_id",
            "condition",
            "normalized_mean_all_valid_pixels",
            "normalized_mean_conservative_detected_pixels",
            "conservative_detected_pixel_count",
            "conservative_detection_fraction",
            "primary_quantification_eligible",
            "primary_qc_status",
        }
        missing = sorted(required - set(header))
        if missing:
            problems.append(issue(
                "input.schema",
                "Missing extraction columns: " + ", ".join(missing),
                "input",
            ))
        actual = {row.get("sample_id") for row in rows}
        if actual != set(sample_ids):
            problems.append(issue(
                "input.samples",
                "Extraction sample coverage does not match the request.",
                "scientific",
            ))

    frozen = ROOT / req.get("frozen_script", {}).get(
        "repository_relative_path", ""
    )
    if not frozen.is_file() or sha(frozen) != SCRIPT_HASH:
        problems.append(issue(
            "integrity.frozen_script",
            "Validated Script 03 identity failed.",
            "infrastructure",
        ))

    workspace = Path(req.get("output_working_directory", ""))
    if workspace.exists() and not workspace.is_dir():
        problems.append(issue(
            "workspace.invalid_type",
            "Summary workspace exists but is not a directory.",
            "infrastructure",
        ))

    if check_environment:
        rex = Path(req.get("environment_requirements", {}).get(
            "r_executable", ""
        ))
        if not rex.is_file():
            problems.append(issue(
                "environment.r",
                "Configured Rscript is unavailable.",
                "environment",
            ))
    return problems


def sample_display_tokens(req: dict) -> dict[str, str]:
    mappings = req.get("internal_role_mappings", [])
    raw: list[tuple[str, str]] = []
    for mapping in mappings:
        internal = str(mapping.get("internal_value", "")).strip()
        if not internal:
            continue
        label = (
            mapping.get("user_facing_label")
            or mapping.get("user_facing_value")
            or internal
        )
        raw.append((internal, safe(label, internal)))
    counts: dict[str, int] = {}
    for _, token in raw:
        counts[token] = counts.get(token, 0) + 1
    return {
        internal: (
            token
            if counts[token] == 1
            else safe(f"{token}_{internal}", internal)
        )
        for internal, token in raw
    }


def make_pair_copy(
    req: dict,
    pair_workspace: Path,
    pair_input: Path,
    reference_samples: list[str],
    treatment_samples: list[str],
    pair_dataset_id: str,
) -> tuple[Path, str]:
    source = ROOT / req["frozen_script"]["repository_relative_path"]
    original = source.read_text(encoding="utf-8")
    start = original.index('dataset_id <- "PVL_Single_Negative"')
    end = original.index(BOUNDARY)
    analysis = (pair_workspace / "analysis").resolve()

    panel = f'''dataset_id <- {rquote(pair_dataset_id)}

input_csv <-
  {rquote(pair_input.as_posix())}

analysis_root <- {rquote(analysis.as_posix())}

output_parent <- file.path(
  analysis_root,
  "03_Replicate_Summaries",
  dataset_id
)

control_sample_ids <- {rvec(reference_samples)}

treatment_sample_ids <- {rvec(treatment_samples)}

control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "TRT"

high_cv_threshold_percent <- 30
strong_signal_threshold <- 1000
minimum_strong_pixels <- 10L
minimum_supported_replicates <- 2L
absolute_log2fc_candidate_threshold <- 1

require_all_replicates_for_condition_statistics <- TRUE
write_excel_output <- TRUE

required_sample_ids <- c(
  control_sample_ids,
  treatment_sample_ids
)
'''
    generated = original[:start] + panel + original[end:]
    path = pair_workspace / "execution_copy_multi_group_script03.R"
    path.write_text(generated, encoding="utf-8", newline="\n")
    return path, sha(path)


def rename_pair_columns(
    frame: pd.DataFrame,
    sample_tokens: dict[str, str],
    reference_token: str,
    treatment_token: str,
) -> pd.DataFrame:
    renamed: dict[str, str] = {}
    sample_ids = sorted(sample_tokens, key=len, reverse=True)
    for column in frame.columns:
        new_name = column
        sample_column = False
        for sample_id in sample_ids:
            if new_name.startswith(sample_id + "_"):
                new_name = sample_tokens[sample_id] + new_name[len(sample_id):]
                sample_column = True
                break
        # Preserve the exact user-facing sample token. Only Script 03's
        # condition-level fields use the temporary CTRL/TRT labels.
        if not sample_column:
            new_name = new_name.replace("TRT", treatment_token)
            new_name = new_name.replace("CTRL", reference_token)
        renamed[column] = new_name
    return frame.rename(columns=renamed)


def equal_series(left: pd.Series, right: pd.Series) -> bool:
    """Compare aligned scientific columns without subtracting booleans."""
    if len(left) != len(right):
        return False

    lvalue = left.reset_index(drop=True)
    rvalue = right.reset_index(drop=True)

    left_missing = lvalue.isna()
    right_missing = rvalue.isna()
    if bool((left_missing ^ right_missing).any()):
        return False

    present = ~(left_missing | right_missing)
    if not bool(present.any()):
        return True

    # Script 03 emits logical QC/eligibility columns. pandas can preserve
    # those as bool, and NumPy explicitly forbids boolean subtraction.
    if (
        pd.api.types.is_bool_dtype(lvalue.dtype)
        or pd.api.types.is_bool_dtype(rvalue.dtype)
    ):
        left_bool = (
            lvalue[present].astype(str).str.strip().str.lower()
        )
        right_bool = (
            rvalue[present].astype(str).str.strip().str.lower()
        )
        return bool((left_bool == right_bool).all())

    lnum = pd.to_numeric(lvalue[present], errors="coerce")
    rnum = pd.to_numeric(rvalue[present], errors="coerce")
    numeric = lnum.notna() & rnum.notna()

    if bool(numeric.any()):
        left_numeric = lnum[numeric].astype(float)
        right_numeric = rnum[numeric].astype(float)
        close = (
            (left_numeric - right_numeric).abs()
            <= 1e-12 + 1e-12 * right_numeric.abs()
        )
        if not bool(close.all()):
            return False

    remaining = ~numeric
    if bool(remaining.any()):
        left_text = (
            lvalue[present][remaining]
            .fillna("")
            .astype(str)
        )
        right_text = (
            rvalue[present][remaining]
            .fillna("")
            .astype(str)
        )
        if not bool((left_text == right_text).all()):
            return False

    return True


def merge_pair(
    combined: pd.DataFrame | None,
    pair: pd.DataFrame,
    comparison_token: str,
    dataset_id: str,
) -> pd.DataFrame:
    keys = ["target_id", "mzmine_row_id", "target_mz"]
    pair = pair.copy()
    pair["dataset_id"] = dataset_id
    if combined is None:
        return pair

    left = combined.set_index("target_id", drop=False)
    right = pair.set_index("target_id", drop=False)
    if set(left.index) != set(right.index):
        raise RuntimeError("Pairwise Script 03 target identities disagree.")
    right = right.loc[left.index]

    for key in keys:
        if key in left.columns and key in right.columns:
            if not equal_series(left[key], right[key]):
                raise RuntimeError(
                    f"Pairwise Script 03 metadata differs for {key}."
                )

    for column in right.columns:
        if column in keys or column == "dataset_id":
            continue
        if column not in left.columns:
            left[column] = right[column]
            continue
        if equal_series(left[column], right[column]):
            continue
        candidate = f"{column}__{comparison_token}"
        counter = 2
        while candidate in left.columns:
            candidate = f"{column}__{comparison_token}_{counter}"
            counter += 1
        left[candidate] = right[column]

    return left.reset_index(drop=True)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("request")
    parser.add_argument("--report", required=True)
    args = parser.parse_args()

    request_path = Path(args.request)
    report_path = Path(args.report)
    req = json.loads(request_path.read_text(encoding="utf-8"))
    report: dict = {
        "schema_version": "1.0.0",
        "run_id": req.get("run_id"),
        "stage_id": req.get("stage_id"),
        "status": "preflight_failed",
        "validation_records": [],
        "produced_outputs": [],
    }

    problems = validate_request(req)
    report["validation_records"] = problems
    if problems:
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
        return 2

    workspace = Path(req["output_working_directory"]).resolve()
    archived_workspace: Path | None = None
    try:
        archived_workspace = prepare_retry_workspace(workspace)
        source_path = Path(
            req["registered_inputs"]["pixel_intensity_artifact"]["path"]
        )
        source = pd.read_csv(source_path, low_memory=False)
        sample_tokens = sample_display_tokens(req)
        conditions = {
            item["condition_id"]: item
            for item in req.get("conditions", [])
        }
        samples = req.get("samples", [])
        samples_by_condition: dict[str, list[str]] = {}
        for sample in samples:
            samples_by_condition.setdefault(
                sample["condition_id"], []
            ).append(sample["sample_id"])

        combined: pd.DataFrame | None = None
        pair_reports: list[dict] = []
        execution_hashes: list[str] = []
        aggregate_stdout = workspace / "stdout.log"
        aggregate_stderr = workspace / "stderr.log"

        with aggregate_stdout.open("w", encoding="utf-8") as all_out, \
             aggregate_stderr.open("w", encoding="utf-8") as all_err:
            for index, comparison in enumerate(
                req.get("comparisons", []), start=1
            ):
                ref_id = comparison["reference_condition_id"]
                cmp_id = comparison["comparison_condition_id"]
                ref_samples = samples_by_condition[ref_id]
                cmp_samples = samples_by_condition[cmp_id]
                ref_token = safe(
                    comparison.get("reference_token")
                    or comparison.get("reference_display_label")
                    or conditions[ref_id].get("display_label")
                    or ref_id,
                    "Reference",
                )
                cmp_token = safe(
                    comparison.get("comparison_token")
                    or comparison.get("comparison_display_label")
                    or conditions[cmp_id].get("display_label")
                    or cmp_id,
                    f"Treatment_{index}",
                )
                comparison_token = safe(
                    f"{cmp_token}_vs_{ref_token}",
                    f"Comparison_{index}",
                )

                pair_workspace = workspace / f"pair_{index:03d}_{comparison_token}"
                pair_workspace.mkdir(parents=True, exist_ok=False)
                pair_input = pair_workspace / "pair_input.csv"
                pair_samples = set(ref_samples + cmp_samples)
                pair = source[source["sample_id"].isin(pair_samples)].copy()
                expected_rows = len(pair_samples) * source["target_id"].nunique()
                if len(pair) != expected_rows:
                    raise RuntimeError(
                        f"Pair {comparison_token} extraction dimensions failed."
                    )
                pair.loc[
                    pair["sample_id"].isin(ref_samples), "condition"
                ] = "CTRL"
                pair.loc[
                    pair["sample_id"].isin(cmp_samples), "condition"
                ] = "TRT"

                pair_dataset_id = safe(
                    f"{req['dataset_id']}_P{index:03d}_{comparison_token}",
                    f"Pair_{index}",
                )
                pair = write_pair_input(
                    pair,
                    pair_input,
                    pair_dataset_id,
                )
                copy_path, copy_hash = make_pair_copy(
                    req,
                    pair_workspace,
                    pair_input,
                    ref_samples,
                    cmp_samples,
                    pair_dataset_id,
                )
                execution_hashes.append(copy_hash)
                stdout = pair_workspace / "script03.stdout.log"
                stderr = pair_workspace / "script03.stderr.log"
                rex = req["environment_requirements"]["r_executable"]
                with stdout.open("w", encoding="utf-8") as out, \
                     stderr.open("w", encoding="utf-8") as err:
                    process = subprocess.run(
                        [rex, str(copy_path)],
                        cwd=pair_workspace,
                        stdout=out,
                        stderr=err,
                        check=False,
                    )
                all_out.write(
                    f"\n===== {comparison_token} =====\n"
                    + stdout.read_text(encoding="utf-8", errors="replace")
                )
                all_err.write(
                    f"\n===== {comparison_token} =====\n"
                    + stderr.read_text(encoding="utf-8", errors="replace")
                )
                if process.returncode:
                    raise RuntimeError(
                        f"Frozen Script 03 failed for {comparison_token}."
                    )

                parent = (
                    pair_workspace
                    / "analysis"
                    / "03_Replicate_Summaries"
                    / pair_dataset_id
                )
                directories = [
                    item for item in parent.glob(
                        "ThreeReadout_ReplicateSummary_*"
                    )
                    if item.is_dir()
                ]
                if len(directories) != 1:
                    raise RuntimeError(
                        f"Expected one Script 03 output for {comparison_token}; "
                        f"found {len(directories)}."
                    )
                primary = directories[0] / (
                    f"{pair_dataset_id}_ThreeReadout_Summary.csv"
                )
                if not primary.is_file():
                    raise RuntimeError(
                        f"Script 03 summary is missing for {comparison_token}."
                    )
                pair_summary = pd.read_csv(primary, low_memory=False)
                if len(pair_summary) != source["target_id"].nunique():
                    raise RuntimeError(
                        f"Script 03 target retention failed for {comparison_token}."
                    )
                pair_summary = rename_pair_columns(
                    pair_summary,
                    sample_tokens,
                    ref_token,
                    cmp_token,
                )
                combined = merge_pair(
                    combined,
                    pair_summary,
                    comparison_token,
                    req["dataset_id"],
                )
                pair_reports.append({
                    "comparison_id": comparison.get("comparison_id"),
                    "reference_condition_id": ref_id,
                    "comparison_condition_id": cmp_id,
                    "reference_samples": ref_samples,
                    "comparison_samples": cmp_samples,
                    "reference_token": ref_token,
                    "comparison_token": cmp_token,
                    "execution_copy_hash": copy_hash,
                    "summary_path": str(primary),
                    "summary_sha256": sha(primary),
                    "rows": len(pair_summary),
                    "columns": len(pair_summary.columns),
                })

        if combined is None:
            raise RuntimeError("No multi-group comparisons were executed.")
        if "ion_mode" in combined.columns:
            combined["ion_mode"] = req.get("ion_mode")
        else:
            combined.insert(
                min(1, len(combined.columns)),
                "ion_mode",
                req.get("ion_mode"),
            )
        output_name = (
            f"{req['dataset_id']}_MultiGroup_ThreeReadout_Summary.csv"
        )
        output_path = workspace / output_name
        combined.to_csv(output_path, index=False)

        target_count = source["target_id"].nunique()
        if len(combined) != target_count:
            raise RuntimeError("Combined multi-group target retention failed.")
        required_patterns = [
            "log2FC_overall_",
            "local_intensity_log2FC_",
            "detection_fraction_difference_",
        ]
        for pattern in required_patterns:
            count = sum(
                str(column).startswith(pattern)
                for column in combined.columns
            )
            if count < len(req.get("comparisons", [])):
                raise RuntimeError(
                    f"Combined output is missing {pattern} comparison columns."
                )

        manifest_path = workspace / "multi_group_comparison_manifest.json"
        manifest_path.write_text(
            json.dumps({
                "dataset_id": req.get("dataset_id"),
                "ion_mode": req.get("ion_mode"),
                "comparisons": pair_reports,
            }, indent=2),
            encoding="utf-8",
        )
        aggregate_hash = sha_text(
            json.dumps(execution_hashes, sort_keys=True)
        )
        outputs = [
            {
                "artifact_role": output_name,
                "path": str(output_path),
                "sha256": sha(output_path),
                "bytes": output_path.stat().st_size,
                "rows": len(combined),
                "columns": len(combined.columns),
            },
            {
                "artifact_role": manifest_path.name,
                "path": str(manifest_path),
                "sha256": sha(manifest_path),
                "bytes": manifest_path.stat().st_size,
                "rows": len(pair_reports),
                "columns": None,
            },
        ]
        report.update(
            status="completed",
            execution_started=True,
            archived_previous_workspace=(
                str(archived_workspace)
                if archived_workspace is not None
                else None
            ),
            frozen_script_hash=SCRIPT_HASH,
            execution_copy_hash=aggregate_hash,
            pairwise_execution_copy_hashes=execution_hashes,
            pairwise_comparisons=pair_reports,
            stdout_path=str(aggregate_stdout),
            stderr_path=str(aggregate_stderr),
            produced_outputs=outputs,
            equivalence={
                "status": "NOT_EVALUABLE",
                "reason": (
                    "No dataset-specific multi-group historical oracle; "
                    "each pair was calculated by the unchanged frozen Script 03."
                ),
                "absolute_tolerance": 1e-10,
                "relative_tolerance": 1e-10,
                "maximum_absolute_difference": 0.0,
                "maximum_relative_difference": 0.0,
            },
            output_validation={
                "status": "PASS",
                "rows": len(combined),
                "columns": len(combined.columns),
                "targets": target_count,
                "comparisons": len(pair_reports),
                "sample_count": len(samples),
            },
            validation_records=[{
                "check_id": "multi_group.script03",
                "status": "PASS",
                "category": "scientific",
                "blocking": False,
                "message": (
                    "Every reference-versus-treatment comparison completed "
                    "with the unchanged frozen Script 03; all targets and "
                    "individual sample columns were retained."
                ),
            }],
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(
            json.dumps(report, indent=2),
            encoding="utf-8",
        )
        return 0
    except Exception as exc:
        report.update(
            status="failed",
            error=str(exc),
            archived_previous_workspace=(
                str(archived_workspace)
                if archived_workspace is not None
                else None
            ),
            recommended_correction=(
                "Inspect preserved pairwise Script 03 diagnostics; "
                "no multi-group summary was activated."
            ),
        )
        report_path.parent.mkdir(parents=True, exist_ok=True)
        report_path.write_text(
            json.dumps(report, indent=2),
            encoding="utf-8",
        )
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
