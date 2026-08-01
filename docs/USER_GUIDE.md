# User Guide

## 1. Launch

From the repository root:

```powershell
powershell.exe -ExecutionPolicy Bypass -File .\Launch-FullPixel-MSI-Quant.ps1
```

The application opens locally in the default browser. Closing the browser does
not automatically terminate every background scientific worker; use the
application controls and allow active stages to reach a terminal state.

## 2. Create a project

Choose **Create Project** and provide:

- a project name;
- an external project folder;
- an analysis design;
- an exposure label;
- Positive, Negative, or both ion modes.

The project folder is separate from the software repository. It contains
configuration, run records, logs, intermediate artifacts, provenance, and
final outputs.

### Analysis designs

**Quantify samples only**

- one or more samples;
- no conditions or group comparison required;
- one output column per registered sample.

**Compare two groups**

- exactly two groups;
- at least two biological replicates per group;
- equal replicate counts required;
- one group is the reference.

**Compare multiple groups**

- one reference group;
- at least two non-reference groups;
- at least two biological replicates per group;
- replicate counts may differ;
- every non-reference group is compared with the same reference.

## 3. Select the extraction mass tolerance

Open **Dataset Configuration** and use **Extraction mass tolerance**.

- **Validated v1.0.0 default** uses `max(10 ppm, 0.002 Da)`.
- **Custom strict combined** accepts both a ppm value and an absolute m/z
  tolerance expressed as a `±` Da half-window. For every target m/z, the ppm
  value is converted to Da and the smaller of the ppm-derived and absolute Da
  half-windows is used.

For example, with 10 ppm and 0.002 Da, the effective half-window is 0.001 Da
at m/z 100 and 0.002 Da at m/z 500.

Click **Apply Mass Tolerance**, then click **Save Configuration**. Custom strict
settings are retained as Technical Validation Candidate settings and must be
assessed for suitability for the acquisition platform and dataset.

## 4. Register MSI samples

For every sample and ion mode, register:

- sample identifier;
- condition and replicate identifier where applicable;
- `.imzML` path;
- matching `.ibd` path;
- pixel size;
- declared coordinate count;
- optional acquisition metadata.

The application checks that each imzML/IBD pair exists and that identifiers are
unique. The actual imzML coordinate count must match the declared count.

Do not rename or move registered inputs during an active workflow. A changed
path or hash can invalidate dependent artifacts.

## 5. Register the aligned MZmine table

The aligned feature table supplies extraction targets, not quantitative
intensities.

Map at least:

- feature identifier;
- target m/z.

Column names do not need to follow one fixed naming convention because the
mapping is explicit. Review the preview before confirming.

## 6. Register the annotated MZmine table

The annotated table is optional until annotation integration. Map the available
annotation fields, including compound name, formula, adduct, score, compound
score, and database where present.

Annotations remain putative. Quantitative values must be preserved exactly
during the merge.

## 7. Review validation

Before execution, review:

- structural validation;
- analysis-design compatibility;
- profile compatibility;
- environment checks;
- frozen scientific-source integrity;
- registered input availability;
- sample and column mappings.

A critical failure blocks execution. Warnings remain visible in the project
record and output workbook.

## 8. Run the workflow

The workflow stages are:

1. **Extract Pixel Intensities**
2. **Summarize Replicate Readouts**
3. **Integrate MZmine Annotations**
4. **Create Combined Publication Export**
5. **Create and validate the final workbook**

Scientific execution runs in external workers. The browser shows persisted
stage states rather than an invented progress percentage.

### Resume after a failure

After correcting the reported cause, use **Resume Failed Run**. Valid completed
checkpoints are reused. The application creates a new retry workspace for the
failed stage and retains the previous failed attempt for provenance.

## 9. Download results

Open **Results** after the workflow completes.

- Quantify-only projects show **Download Quantified Samples Workbook**.
- Comparison projects show **Download Final Results Workbook**.

The download is validated against the project analysis type and expected sheet
contract before it is offered.

## 10. Interpret the results

The workbooks contain descriptive quantitative readouts. They do not contain
p-values or statistical-significance claims.

- `log2FC > 0` indicates a larger comparison-group mean.
- `log2FC < 0` indicates a larger reference-group mean.
- Missing log2 fold change means one or both group means were nonpositive,
  nonfinite, or unavailable.
- Detection change is a difference in conservative detection fraction.
- Annotation identities are putative accurate-mass matches.

## 11. Project safety

- Keep raw data outside the software repository.
- Back up the project folder before manual filesystem changes.
- Do not edit registered artifacts in place.
- Do not edit files under `validated-source/`.
- Do not interpret the software's technical-validation label as biological or
  clinical validation.
