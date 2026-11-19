# MALDIPixelQuant V2

MALDIPixelQuant V2 is the second public release of the local R Shiny workflow
for pixel-level quantitative analysis of full-pixel MALDI-MSI data. It uses an
aligned MZmine feature list as the target m/z list and extracts quantitative
signals directly from centroided imzML/IBD spectra.

## Major changes

- Redesigned researcher-facing GUI for project creation, MSI and MZmine input
  registration, dataset configuration, validation, workflow monitoring,
  results, logs, artifacts, and provenance.
- Selectable Raw, TIC/spectrum-sum, RMS, and Median primary intensity
  strategies with method-aware routing, persistence, provenance, and final
  workbook exports.
- Optional matched biological-replicate-pair jackknife robustness analysis for
  eligible two-group projects, reported in an additional `Jackknife_Log2FC`
  worksheet.
- Publication-ready, normalization-aware final workbooks that retain complete
  feature tables and available putative MZmine annotation metadata.
- Validated result freeze/export with SHA-256 verification, overwrite
  protection, and provenance linkage.
- Improved validation, failure diagnostics, workflow-state recovery, and
  Windows handling for deeply nested application and project paths.

## Validation

- Raw, TIC/spectrum-sum, RMS, and Median GUI selection, persistence, execution,
  workbook generation, and export were validated end to end with external
  Positive- and Negative-mode PVL MALDI-MSI datasets.
- V2 spectrum-sum results were backward-compatible with V1 within the
  established numerical tolerance.
- The release candidate passes the automated R and Python suites and frozen
  scientific-core integrity verification.
- The four registered scripts under `validated-source/` remain byte-identical
  to their recorded SHA-256 hashes.

The software retains the **Technical Validation Candidate** scientific
classification. RMS and Median are robustness/sensitivity strategies and are
not claimed to be scientifically superior to spectrum-sum. Molecular
annotations remain putative accurate-mass matches. MALDIPixelQuant is not
intended for clinical or diagnostic use.

## Compatibility and inputs

- Existing spectrum-sum workflows and legacy TIC-named workbook contracts are
  preserved.
- Inputs are centroided full-pixel imzML/IBD sample pairs and aligned MZmine
  feature tables containing feature identifiers and target m/z values.
- Optional annotated MZmine tables can be integrated after quantification.
- V1 tags and releases remain available in the same repository.

## Citation

No DOI is currently declared. Cite the tagged repository release:

> Alderden, W. (2026). *MALDIPixelQuant* (Version 2.0.0) [Computer software].
> GitHub. https://github.com/WesAld/FullPixel-MSI-Quant
