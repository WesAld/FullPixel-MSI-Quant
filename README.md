# FullPixel MSI Quant

**FullPixel MSI Quant** is a local R Shiny workflow for spectrum-sum-normalized
full-pixel MALDI mass spectrometry imaging quantification from aligned MZmine
features.

The aligned MZmine table defines the feature identifiers and target m/z values.
Quantitative intensities are extracted from the original centroided imzML/IBD
spectra. Putative annotation metadata is integrated only after quantitative
summarization.

> **Release:** 1.0.0  
> **Validation classification:** Technical Validation Candidate  
> **Primary tested platform:** Windows 11 with R 4.6.0

## What the application does

FullPixel MSI Quant supports three analysis designs:

| Analysis design | Supported structure | Main output |
|---|---|---|
| Quantify samples only | One or more independent samples | Individual sample readouts; no group comparison |
| Compare two groups | Exactly two groups, at least two equal-count biological replicates per group | Reference-versus-comparison descriptive readouts |
| Compare multiple groups | One reference plus at least two non-reference groups; at least two replicates per group; unequal group sizes allowed | Every non-reference group compared with the same reference |

Positive ion mode, negative ion mode, or both modes can be registered. Exposure
labels, project names, sample identifiers, pixel size, and coordinate counts are
request-driven. The declared coordinate count is checked against the imzML
coordinates.

## Scientific method

For every valid pixel and target feature:

1. The nearest centroid within the configured tolerance is selected.
2. The target intensity is divided by the sum of all stored centroid
   intensities in that pixel.
3. The result is multiplied by 1,000,000.
4. Full-pixel and detection readouts are summarized for each sample.
5. Group means, fold change, and log2 fold change are calculated only for
   comparison workflows.
6. Putative MZmine annotations are merged without changing upstream
   quantitative values.

The workflow uses no pseudocount, no imputation, no pairing, and no p-values.
Missing values remain missing. See
[`docs/SCIENTIFIC_METHOD.md`](docs/SCIENTIFIC_METHOD.md).

## Required inputs

For each selected ion mode:

- centroided full-pixel `.imzML` and matching `.ibd` files for every sample;
- an aligned MZmine feature table containing a feature identifier and target
  m/z column;
- optionally, an annotated MZmine feature table for annotation integration.

Raw scientific data are not copied into this repository. Projects and outputs
are stored in user-selected external folders.

## Installation

### 1. Install prerequisites

- Windows 10 or 11
- R 4.6.0 recommended for the validated environment
- Python 3.10 or newer

From PowerShell in the repository root:

```powershell
powershell.exe -ExecutionPolicy Bypass -File .\Install-Dependencies.ps1
```

The installer adds the required CRAN/Bioconductor and Python packages. It does
not install R or Python themselves.

### 2. Verify the frozen scientific core

```powershell
Rscript .\tools\verify_scientific_core.R
```

The command must end with:

```text
Scientific-core integrity verification PASSED.
```

### 3. Launch the application

```powershell
powershell.exe -ExecutionPolicy Bypass -File .\Launch-FullPixel-MSI-Quant.ps1
```

Or directly:

```powershell
Rscript .\tools\launch_app.R
```

The application runs locally. Scientific files are not uploaded to an external
service.

## Basic workflow

1. Create or open a project.
2. Select the analysis design and ion mode.
3. Register all imzML/IBD sample pairs.
4. register and map the aligned MZmine target table.
5. Optionally register and map the annotated MZmine table.
6. Review structural, profile, environment, and pre-execution validation.
7. Run the workflow.
8. Download the final workbook from **Results**.

See [`docs/USER_GUIDE.md`](docs/USER_GUIDE.md) for detailed instructions.

## Workbook outputs

Comparison workbooks contain:

- `Mean_Log2FC`
- `All_Pixels`
- `TIC_normalized_intensity`
- `All_Features_TIC_Normalized`
- `QC`
- `README`

Quantify-only workbooks contain:

- `Normalized_Intensity`
- `Detected_Pixel_Intensity`
- `Detection`
- `QC`
- `README`

The all-features comparison sheet preserves every registered/extracted feature,
including unannotated features and available MZmine annotation scores. See
[`docs/OUTPUTS.md`](docs/OUTPUTS.md).

## Validation status and limitations

Version 1.0.0 passed end-to-end technical workflow tests using external PVL
MALDI-MSI datasets for:

- quantify samples only;
- two-group comparison; and
- multi-group reference-versus-treatment comparison.

The frozen scientific scripts are distributed with registered SHA-256 hashes.
The application checks these hashes before scientific execution.

This release is not independently scientifically validated against a separate
software pipeline or external reference oracle. Annotation identities are
putative accurate-mass matches. The software is not intended for clinical or
diagnostic use. See [`docs/VALIDATION.md`](docs/VALIDATION.md).

## Repository structure

```text
app/                 Shiny entry point
R/                   Application and workflow services
tools/               Workers, adapters, launch and validation utilities
config/              Profiles, contracts, schemas and examples
validated-source/    Immutable scientific reference scripts and checksums
tests/                Contract and application tests
docs/                 User, scientific, output and validation documentation
```

The files under `validated-source/` must not be reformatted or edited in place.
A scientific change requires a new scientific-core version and new validation
evidence.

## Citation

Citation metadata are provided in [`CITATION.cff`](CITATION.cff). Until a
software paper or archived DOI is available, cite the repository release:

> Alderden, W. (2026). *FullPixel MSI Quant* (Version 1.0.0) [Computer
> software]. GitHub.

## Contributing and support

Open a GitHub issue for reproducible bug reports, feature proposals, or
documentation corrections. Scientific-method changes require explicit review
and a new scientific-core version.

See [`CONTRIBUTING.md`](CONTRIBUTING.md) and [`SECURITY.md`](SECURITY.md).

## License

MIT License. Copyright © 2026 Wessel Alderden.
