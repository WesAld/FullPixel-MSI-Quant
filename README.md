# MALDIPixelQuant V2

**Current public release: 2.0.1**<br>

MALDIPixelQuant is a local R Shiny application for pixel-level quantitative
analysis of full-pixel MALDI mass spectrometry imaging data. An aligned MZmine
feature list supplies feature identifiers and target m/z values; quantitative
signals are extracted directly from the original centroided imzML/IBD spectra.
Putative annotation metadata are integrated only after quantitative
summarization.

V2 supports Raw, TIC/spectrum-sum, RMS, and Median primary intensity strategies,
optional matched biological-replicate jackknife analysis, validation and
provenance records, and publication-ready Excel exports.

## What MALDIPixelQuant does

MALDIPixelQuant supports three analysis designs:

| Analysis design | Supported structure | Main output |
|---|---|---|
| Quantify samples only | One or more independent samples | Individual sample readouts; no group comparison |
| Compare two groups | Exactly two groups, at least two equal-count biological replicates per group | Reference-versus-comparison descriptive readouts |
| Compare multiple groups | One reference plus at least two non-reference groups; at least two replicates per group; unequal group sizes allowed | Every non-reference group compared with the same reference |

Positive ion mode, negative ion mode, or both modes can be registered. Project
names, exposure labels, sample identifiers, pixel size, and coordinate counts
are configuration-driven. The declared coordinate count is checked against the
imzML coordinates.

For every valid pixel and target feature, the workflow selects the nearest
centroid inside the configured mass tolerance, applies the selected primary
intensity strategy, summarizes full-pixel and detection readouts, aggregates
biological replicates where applicable, and finally merges putative MZmine
annotations without changing the quantitative values.

The workflow uses no pseudocount, no imputation, no paired inference, and no
p-values. Missing values remain missing. See
[`docs/SCIENTIFIC_METHOD.md`](docs/SCIENTIFIC_METHOD.md).

## Normalization strategies

| Display name | Internal value | Behavior |
|---|---|---|
| Raw | `raw` | Measured target intensity without per-pixel spectral normalization |
| TIC / spectrum-sum | `spectrum_sum` | Target intensity divided by the sum of stored centroid intensities in the same pixel, scaled to 1,000,000 |
| RMS | `rms` | Target intensity divided by the root-mean-square of positive finite stored centroid intensities, scaled to 1,000,000 |
| Median | `median` | Target intensity divided by the median of positive finite stored centroid intensities, scaled to 1,000,000 |

Legacy TIC-named workbook fields remain spectrum-sum-only. Raw, RMS, and Median
exports use method-explicit names. Spectrum-sum remains the primary manuscript
method in the validated PVL workflow; RMS and Median are robustness/sensitivity
methods and are not claimed to be scientifically superior.

## Optional jackknife robustness analysis

Eligible two-group projects with at least three explicitly matched biological
replicate pairs can include a leave-one-pair-out jackknife. Each replicate ID is
omitted from both conditions in turn; MSI pixels are never treated as biological
replicates. The ordinary full-data log2 fold change remains the primary effect
estimate. The optional `Jackknife_Log2FC` worksheet reports the leave-one-pair-out
estimates, jackknife standard error, range, completeness, and direction stability.

## Required inputs

For each selected ion mode:

- centroided full-pixel `.imzML` and matching `.ibd` files for every sample;
- an aligned MZmine feature table containing a feature identifier and target m/z;
- optionally, an annotated MZmine feature table for annotation integration.

MZmine intensity columns are not used as the source of pixel-level
quantification. Raw scientific data are not copied into this repository.
Projects and outputs are stored in user-selected external folders.

## System requirements

- Windows 10 or 11 (the primary tested platform is Windows 11 x64)
- R 4.6.0 recommended for the validated environment
- Python 3.10 or newer
- CRAN/Bioconductor and Python dependencies installed by the supplied installer

Other operating systems are not part of the primary validation claim.

## Installation and start

Clone or download this repository, open PowerShell in the repository root, and
run:

```powershell
powershell.exe -ExecutionPolicy Bypass -File .\Install-Dependencies.ps1
```

The installer adds the required R and Python packages. It does not install R or
Python themselves.

Verify the frozen scientific core:

```powershell
Rscript .\tools\verify_scientific_core.R
```

Launch MALDIPixelQuant:

```powershell
powershell.exe -ExecutionPolicy Bypass -File .\Launch-FullPixel-MSI-Quant.ps1
```

Or launch directly:

```powershell
Rscript .\tools\launch_app.R
```

The application runs locally; scientific files are not uploaded to an external
service.

## Basic workflow

1. Create or open a project.
2. Choose the analysis design and ion mode.
3. Register all imzML/IBD sample pairs.
4. Register and map the aligned MZmine target table.
5. Optionally register and map the annotated MZmine table.
6. Configure mass tolerance, normalization, comparisons, and optional jackknife.
7. Review structural, compatibility, environment, and pre-execution validation.
8. Run the workflow and review the completed run record.
9. Download or freeze/export the validated final workbook.

See [`docs/USER_GUIDE.md`](docs/USER_GUIDE.md) for detailed instructions.

## Generated outputs

Depending on the analysis design and selected method, final workbooks contain:

- full-pixel and detected-pixel sample readouts;
- detection counts and fractions;
- reference and comparison group means;
- fold change and log2 fold change for comparison workflows;
- method-aware all-features quantitative tables;
- integrated putative MZmine annotations and available annotation scores;
- QC and provenance metadata;
- an optional `Jackknife_Log2FC` worksheet for eligible two-group projects.

See [`docs/OUTPUTS.md`](docs/OUTPUTS.md) for the method-specific sheet contracts.

## Repository structure

```text
app/                 Shiny entry point and static assets
R/                   Application, workflow, validation, and presentation services
tools/               Workers, adapters, launch, and verification utilities
config/              Profiles, contracts, schemas, and examples
validated-source/    Immutable scientific reference scripts and checksums
tests/                R and Python contract/application tests
docs/                 User, scientific, output, validation, and release documentation
```

Files under `validated-source/` must not be reformatted or edited in place. A
scientific change requires a new scientific-core version and new validation
evidence.

## Version history

- **V2 / 2.0.1** — current public release: redesigned GUI, selectable
  normalization, optional jackknife robustness analysis, hardened validation and
  workflow handling, publication-ready exports, and validated result freeze/export.
- **V1** — original public release series (`v1.0.0` through `v1.1.1`).

Internal development iterations between the original release and V2 remain in
Git history but were not separate public releases.

See [`CHANGELOG.md`](CHANGELOG.md) and
[`docs/RELEASE_NOTES_V2.0.1.md`](docs/RELEASE_NOTES_V2.0.1.md).

## Citation

Citation metadata are provided in [`CITATION.cff`](CITATION.cff). No software or
manuscript DOI is currently declared. Cite the repository release as:

> Alderden, W. (2026). *MALDIPixelQuant* (Version 2.0.1) [Computer software].
> GitHub. https://github.com/WesAld/FullPixel-MSI-Quant

## Contributing, support, and license

Open a GitHub issue for reproducible bug reports, feature proposals, or
documentation corrections. Scientific-method changes require explicit review
and a new scientific-core version. See [`CONTRIBUTING.md`](CONTRIBUTING.md) and
[`SECURITY.md`](SECURITY.md).

MALDIPixelQuant is released under the MIT License. Copyright © 2026 Wessel
Alderden.
