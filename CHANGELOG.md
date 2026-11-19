# Changelog

All notable changes to MALDIPixelQuant are documented here.

## [Unreleased]

## [2.0.0] - 2026-11-19

### Added

- Selectable Raw, TIC/spectrum-sum, RMS, and Median primary intensity
  strategies with method-aware provenance and workbook sheet contracts.
- Optional matched biological-replicate-pair jackknife robustness analysis for
  eligible two-group projects, including the `Jackknife_Log2FC` worksheet.
- Redesigned MALDIPixelQuant interface for project creation, dataset
  configuration, validation, workflow monitoring, results, and provenance.
- Researcher-facing **Freeze / Export Result** for validated completed runs,
  with normalization-aware filenames, hash verification, overwrite protection,
  and provenance linkage.

### Changed

- Hardened input validation, normalization selection persistence, workflow
  state recovery, method-aware all-features export, and Windows subprocess
  handling for deep application and project paths.
- Optional unused dependency warnings remain available in technical details but
  no longer make an otherwise compatible selected workflow appear unready in
  Standard Mode.
- Public application, package, documentation, and citation identity is now
  MALDIPixelQuant V2 / 2.0.0.

### Compatibility

- Legacy spectrum-sum calculations and TIC-named workbook contracts remain
  backward-compatible.
- The frozen scientific source under `validated-source/` is unchanged.
- V1 tags, releases, and Git history remain available.

### Validation

- Raw, TIC/spectrum-sum, RMS, and Median GUI selection, persistence, execution,
  final workbook generation, and export were validated end to end with external
  Positive- and Negative-mode PVL MALDI-MSI data.
- The V2 spectrum-sum end-to-end regression was backward-compatible with V1
  within the established numerical tolerance.
- The automated R and Python suites and frozen scientific-core integrity checks
  pass for the release candidate.
- RMS and Median are not claimed to be scientifically superior to
  spectrum-sum.

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

[Unreleased]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v2.0.0...HEAD
[2.0.0]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.1.1...v2.0.0
[1.1.1]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.1.0...v1.1.1
[1.1.0]: https://github.com/WesAld/FullPixel-MSI-Quant/compare/v1.0.0...v1.1.0
[1.0.0]: https://github.com/WesAld/FullPixel-MSI-Quant/releases/tag/v1.0.0
