"""Pure biological-replicate jackknife calculations for FullPixel MSI Quant V3."""
from __future__ import annotations

import math
import re
from typing import Any

import pandas as pd


def safe_token(value: Any, fallback: str) -> str:
    token = re.sub(r"[^A-Za-z0-9]+", "_", str(value or "")).strip("_")
    return (token or fallback)[:64]


def unique_tokens(labels: list[str], internal_ids: list[str]) -> list[str]:
    base = [safe_token(label, internal)
            for label, internal in zip(labels, internal_ids)]
    counts = {token: base.count(token) for token in set(base)}
    return [token if counts[token] == 1 else
            safe_token(f"{token}_{internal}", internal)
            for token, internal in zip(base, internal_ids)]


def matched_replicate_pairs(mappings: list[dict[str, Any]]) -> list[dict[str, str]]:
    """Resolve CTRL/TRT pairs from explicit original replicate identifiers.

    Mode-repeated mappings are deduplicated by internal sample ID. Ambiguous,
    blank, duplicated, or unmatched replicate identities are rejected rather
    than paired by column position.
    """
    unique: dict[str, dict[str, Any]] = {}
    for item in mappings or []:
        internal = str(item.get("internal_value", "")).strip()
        if internal and internal not in unique:
            unique[internal] = item
    groups = {"CTRL": {}, "TRT": {}}
    for internal, item in unique.items():
        group = "CTRL" if internal.startswith("CTRL_") else (
            "TRT" if internal.startswith("TRT_") else None)
        if group is None:
            continue
        replicate = str(item.get("original_replicate_id", "")).strip()
        if not replicate:
            raise ValueError(f"Missing biological replicate ID for {internal}.")
        if replicate in groups[group]:
            raise ValueError(
                f"Duplicate biological replicate ID '{replicate}' in {group}.")
        groups[group][replicate] = internal
    if set(groups["CTRL"]) != set(groups["TRT"]):
        raise ValueError(
            "Reference and comparison samples do not have identical matched "
            "biological replicate IDs.")
    identifiers = sorted(groups["CTRL"], key=lambda x: [
        int(p) if p.isdigit() else p.lower() for p in re.split(r"(\d+)", x)])
    if len(identifiers) < 3:
        raise ValueError("At least 3 matched biological replicate pairs are required.")
    return [{"replicate_id": replicate,
             "reference_internal": groups["CTRL"][replicate],
             "comparison_internal": groups["TRT"][replicate]}
            for replicate in identifiers]


def _complete_mean(frame: pd.DataFrame, columns: list[str]) -> pd.Series:
    return frame[columns].apply(pd.to_numeric, errors="coerce").mean(
        axis=1, skipna=False)


def calculate_log2fc(frame: pd.DataFrame, reference: list[str],
                     comparison: list[str]) -> pd.Series:
    ref = _complete_mean(frame, reference)
    cmp = _complete_mean(frame, comparison)
    result = pd.Series(float("nan"), index=frame.index, dtype="float64")
    valid = ref.notna() & cmp.notna() & (ref > 0) & (cmp > 0)
    result.loc[valid] = (cmp.loc[valid] / ref.loc[valid]).map(math.log2)
    return result


def calculate_jackknife(frame: pd.DataFrame, pairs: list[dict[str, str]],
                        column_by_internal: dict[str, str],
                        full_log2fc: pd.Series) -> pd.DataFrame:
    """Calculate matched leave-one-pair-out estimates and classical SE."""
    if len(pairs) < 3:
        raise ValueError("At least 3 matched biological replicate pairs are required.")
    result = pd.DataFrame(index=frame.index)
    full = pd.to_numeric(full_log2fc, errors="coerce")
    result["Full_Log2FC"] = full
    jk_columns: list[str] = []
    for omitted in pairs:
        retained = [pair for pair in pairs
                    if pair["replicate_id"] != omitted["replicate_id"]]
        reference = [column_by_internal[pair["reference_internal"]]
                     for pair in retained]
        comparison = [column_by_internal[pair["comparison_internal"]]
                      for pair in retained]
        name = f"JK_Omit_{safe_token(omitted['replicate_id'], 'replicate')}_Log2FC"
        result[name] = calculate_log2fc(frame, reference, comparison)
        jk_columns.append(name)

    estimates = result[jk_columns]
    valid_count = estimates.notna().sum(axis=1)
    complete = valid_count == len(pairs)
    mean = estimates.mean(axis=1, skipna=False)
    squared = estimates.sub(mean, axis=0).pow(2).sum(axis=1, skipna=False)
    se = (((len(pairs) - 1) / len(pairs)) * squared).pow(0.5)
    result["Jackknife_Mean_Log2FC"] = mean.where(complete)
    result["Jackknife_SE"] = se.where(complete)
    result["Jackknife_Min_Log2FC"] = estimates.min(axis=1, skipna=False).where(complete)
    result["Jackknife_Max_Log2FC"] = estimates.max(axis=1, skipna=False).where(complete)
    result["Jackknife_Range"] = (
        result["Jackknife_Max_Log2FC"] - result["Jackknife_Min_Log2FC"])

    stable = pd.Series(pd.NA, index=frame.index, dtype="boolean")
    positive = complete & full.notna() & (full > 0)
    negative = complete & full.notna() & (full < 0)
    zero = complete & full.notna() & (full == 0)
    stable.loc[positive] = estimates.loc[positive].gt(0).all(axis=1)
    stable.loc[negative] = estimates.loc[negative].lt(0).all(axis=1)
    # Exact zero is a separate class and is stable only when every estimate is zero.
    stable.loc[zero] = estimates.loc[zero].eq(0).all(axis=1)
    result["Direction_Stable"] = stable
    result["N_Jackknife_Requested"] = len(pairs)
    result["N_Jackknife_Valid"] = valid_count.astype("int64")
    return result
