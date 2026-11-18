# Changelog

All notable changes to FullPixel MSI Quant are documented here.

## [Unreleased]

### V4 validated result export

- Adds a researcher-facing **Freeze / Export Result** action to completed
  Workflow and Run History records.
- Creates normalization-aware, byte-identical workbook copies only after the
  selected historical run's validated artifact and SHA-256 have been verified.
- Prevents silent overwrite and records the export destination, hashes,
  normalization, jackknife state, application version, and software commit in
  project provenance without creating a new scientific analysis.

### V3 jackknife development

- Identifies the application as FullPixel MSI Quant V3 `3.0.0-dev`.
- Adds an optional, default-off matched biological-replicate-pair jackknife for
  two-group log2 fold changes across Raw, spectrum-sum, RMS, and Median.
- Adds deliberate Include/Remove controls, eligibility reporting, project
  persistence, conditional Results presentation, provenance, and the optional
  `Jackknife_Log2FC` worksheet.
- Preserves the full-data log2 fold change, normalization formulas, detection
  calculations, and hash-locked validated scientific source.

### V2 normalization development

- Identifies the application as FullPixel MSI Quant V2 `2.0.0-dev`.
- Adds selectable Raw, legacy spectrum-sum, RMS, and Median primary intensity
  strategies while keeping the frozen V1 scientific source unchanged.
- RMS and Median remain development-stage Technical Validation Candidate
  methods and are not claimed as publication-validated.
- Validates GUI selection, persistence, execution, and final export for Raw,
  TIC/spectrum-sum, RMS, and Median using the two-group POS+NEG validation
  project.
- Routes each method to explicit primary and all-features workbook sheets while
  preserving the legacy `TIC_normalized_intensity` and
  `All_Features_TIC_Normalized` spectrum-sum contract.
- Prevents explicit Raw, RMS, or Median selections from silently defaulting to
  spectrum-sum during final export.
- Records the completed generic export stage before all-features finalization,
  preventing a later final-workbook failure from leaving stale running state.

## [1.1.1] - 2026-08-01

### Fixed

- Windows launcher now locates `Rscript.exe` automatically through `PATH` and
  common system and user R installation directories.
- Launching from PowerShell now opens the Shiny application in the default
  browser automatically.
- The R launch script now accepts an explicit browser-launch argument.

## [1.1.0] - 2026-08-01

### Added

- GUI control for selecting either the validated v1.0.0 tolerance or a custom
  strict combined ppm/Da tolerance.
- Custom strict mode accepts both a ppm value and an absolute Da half-window and
  uses whichever produces the narrower window for each target m/z.
- Configuration validation, execution-request propagation, provenance display,
  and focused automated tests for combined mass tolerances.

### Fixed

- Interactive mass-tolerance settings are saved from the GUI and persist in the
  project configuration.
- Windows workflow requests resolve and record the concrete `Rscript.exe` path
  before adapter execution.

### Validation

- End-to-end custom strict workflows completed with external PVL positive- and
  negative-mode data using 5 ppm and an absolute half-window of 0.0015 Da.
- Request metadata and executed extraction copies confirmed the strict
  `pmin()` extraction rule.
- Custom strict combined settings remain classified as Technical Validation
  Candidate overrides. The four frozen scientific scripts remain unchanged.

## [1.0.0] - 2026-08-01

### Added

- Local R Shiny application for full-pixel MALDI-MSI quantification.
- Registration of centroided imzML/IBD sample pairs.
- Registration and column mapping of aligned and annotated MZmine tables.
- Positive, negative, or combined ion-mode workflows.
- Quantify Samples Only mode with individual sample outputs.
- Generic two-group comparison mode with equal replicate counts.
- Multi-group mode with one reference, multiple treatments, and unequal
  replicate counts.
- Per-pixel spectrum-sum normalization at a scale of 1,000,000.
- Full-pixel, strong-pixel intensity, and detection-fraction readouts.
- Putative annotation integration with quantitative-preservation checks.
- All-features comparison export retaining annotated and unannotated features.
- Six-sheet comparison workbooks and five-sheet quantify-only workbooks.
- Artifact hashing, run records, validation records, logs, and provenance.
- Retry-safe workflow execution and result-download validation.
- Frozen scientific source verification using registered SHA-256 hashes.

### Validation

- End-to-end quantify-only workflow tested with external PVL Positive-mode data.
- End-to-end generic two-group workflow tested with external PVL Positive and
  Negative data.
- End-to-end multi-group workflow tested with external PVL Positive-mode data.
- Workbook structure, feature retention, sample columns, comparison columns,
  MZmine scores, QC records, and Excel formula integrity reviewed.
- Release remains classified as a Technical Validation Candidate because an
  independent scientific reference oracle has not been established.

[Unreleased]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.1.1...HEAD
[1.1.1]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/WesAld/FullPixel-MSI-Quant/releases/tag/v1.0.0
