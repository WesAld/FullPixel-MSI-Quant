"""V2 normalization configuration and downstream column routing."""
from __future__ import annotations

from pathlib import Path
import pandas as pd

METHODS = ("raw", "spectrum_sum", "rms", "median")
DEFINITION_VERSION = "2.0.0-dev-1"


def resolve_method(request: dict) -> str:
    method = str(request.get("primary_normalization_method") or
                 request.get("approved_scientific_configuration", {}).get(
                     "primary_normalization_method") or
                 request.get("display_metadata", {}).get("normalization", {}).get(
                     "primary_normalization_method") or
                 "spectrum_sum").strip().lower()
    if method not in METHODS:
        raise ValueError(f"Unsupported primary_normalization_method: {method}")
    return method


def summary_columns(method: str) -> tuple[str, str]:
    mapping = {
        "raw": ("raw_mean_all_valid_pixels", "raw_mean_conservative_detected_pixels"),
        "spectrum_sum": ("normalized_mean_all_valid_pixels", "normalized_mean_conservative_detected_pixels"),
        "rms": ("rms_normalized_mean_all_valid_pixels", "rms_normalized_mean_conservative_detected_pixels"),
        "median": ("median_normalized_mean_all_valid_pixels", "median_normalized_mean_conservative_detected_pixels"),
    }
    if method not in mapping:
        raise ValueError(f"Unsupported primary_normalization_method: {method}")
    return mapping[method]


def route_extraction_summary(source: Path, destination: Path, method: str) -> Path:
    overall, local = summary_columns(method)
    frame = pd.read_csv(source, low_memory=False)
    missing = sorted({overall, local} - set(frame.columns))
    if missing:
        raise ValueError("Selected normalization columns are missing: " + ", ".join(missing))
    frame["normalized_mean_all_valid_pixels"] = frame[overall]
    frame["normalized_mean_conservative_detected_pixels"] = frame[local]
    frame["primary_normalization_method"] = method
    destination.parent.mkdir(parents=True, exist_ok=True)
    frame.to_csv(destination, index=False)
    return destination


def provenance(method: str, scale: float) -> dict:
    definitions = {
        "raw": "No per-pixel spectral normalization.",
        "spectrum_sum": "Existing FullPixel legacy validated spectrum-sum implementation.",
        "rms": "Positive finite stored centroid intensities (I > 0); sqrt(mean(I^2)); zero denominator invalid and normalized value NA.",
        "median": "Positive finite stored centroid intensities (I > 0); median(I); zero denominator invalid and normalized value NA.",
    }
    return {"application_version": "2.0.0-dev",
            "primary_normalization_method": method,
            "normalization_scale": scale,
            "normalization_definition_version": DEFINITION_VERSION,
            "normalization_definition": definitions[method]}
