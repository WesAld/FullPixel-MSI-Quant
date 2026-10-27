# Scientific Method

## Quantitative source

Aligned MZmine features provide feature identifiers and target m/z values.
Centroided full-pixel imzML/IBD spectra provide quantitative intensities.
Annotated MZmine tables provide putative metadata after quantification.

MZmine intensity columns are not used as the source of pixel-level
quantification.

## Target extraction

For each target m/z and valid pixel:

- under the validated v1.0.0 setting, the extraction half-window is the larger
  of 0.002 Da and 10 ppm of the target m/z;
- under a custom strict combined setting, the user enters both a ppm tolerance
  and an absolute Da half-window, and the extraction half-window is the smaller
  of the two for that target m/z;
- the nearest centroid inside the resulting tolerance is selected;
- greater intensity resolves equal mass-error ties;
- a missing centroid remains missing or undetected according to the registered
  output field; it is not imputed.

Custom ppm values must be between 0.1 and 100. Custom absolute half-windows
must be between 0.000001 and 1 Da. Both values and the combination rule are
recorded in the execution request and analysis configuration. Custom strict
runs are classified as **Technical Validation Candidates**. The four frozen
scientific source files remain byte-identical; the strict rule is applied only
in the generated execution copy.

## Spectrum-sum normalization

For target \(j\) in pixel \(p\):

\[
I_{j,p}^{norm} =
\frac{I_{j,p}^{raw}}
{\sum_k I_{k,p}^{raw}}
\times 10^6
\]

The denominator is the sum of all stored centroid intensities in the same valid
pixel. The method is described as **spectrum-sum normalization**.

## V3 selectable primary intensity strategies

FullPixel MSI Quant V3 3.0.0-dev retains multiple signal families from the
same stored centroid spectra. `primary_normalization_method` selects the family
used downstream; a missing field resolves to `spectrum_sum` for compatibility.

- **Raw (`raw`)** uses `raw_target_intensity` without scaling or transformation.
- **TIC / spectrum-sum (`spectrum_sum`)** preserves the V1 formula above.
- **RMS (`rms`)** uses positive finite stored centroid intensities only:
  `sqrt(mean(I_positive^2))`, then divides and scales by 1,000,000.
- **Median (`median`)** uses `median(I_positive)` for the same positive finite
  population, then divides and scales by 1,000,000.

RMS and Median do not create dense bins, include explicit zeros or negative
intensities, impute missing peaks, or add pseudocounts. Existing spectrum
integrity rejection remains authoritative. An empty positive population gives
a zero denominator, an invalid-denominator flag, and a missing normalized
value. RMS and Median are V2 Technical Validation Candidate methods, not
publication-validated methods.

## Detection definitions

- Inclusive detection: raw target intensity greater than 0.
- Conservative detection: raw target intensity greater than or equal to 1,000.
- Local strong-pixel intensity requires at least 10 conservative detected
  pixels.

The declared full-pixel coordinate count must match the coordinates read from
the imzML data.

## Sample readouts

Each registered sample contributes:

- overall normalized signal across the complete valid full-pixel image;
- local strong-pixel intensity;
- conservative detected-pixel count;
- conservative detection fraction;
- eligibility and QC fields.

## Group comparisons

For comparison workflows, group means are arithmetic means of registered
biological-replicate values.

For comparison group \(T\) and reference group \(R\):

\[
FC = \frac{\bar{T}}{\bar{R}}
\]

\[
log_2FC = \log_2\left(\frac{\bar{T}}{\bar{R}}\right)
\]

Official log2 fold change remains missing when either group mean is
nonpositive, nonfinite, or unavailable.

Multi-group analysis performs one descriptive comparison for each
non-reference group against the single configured reference.

## Missing values and inference

The workflow uses:

- no pseudocount;
- no imputation;
- no paired analysis;
- no p-values;
- no automatic significance threshold.

Missing values remain missing. The application produces descriptive
quantitative summaries, not inferential statistical conclusions.

## Annotation integration

The annotated MZmine table is joined after quantitative summarization.

- Quantitative columns must be preserved value-for-value.
- Annotation scores are retained where available.
- The high-priority annotation-score threshold is 0.6.
- Annotation identities are putative accurate-mass database matches.

Accepted reference-profile adducts include `[M-H]-`, `[M+H]+`, `[M+Na]+`, and
`[M+K]+`. Generic projects may retain additional annotation metadata but must
not convert it into a confirmed identity claim.

## Frozen scientific sources

The four reference scripts under `validated-source/` define the registered
scientific core. Their SHA-256 hashes are listed in
`validated-source/SHA256SUMS` and
`config/scientific_core_manifest.yml`.

Run:

```powershell
Rscript .\tools\verify_scientific_core.R
```

A changed or missing frozen source blocks scientific execution.
## Optional matched biological-replicate-pair jackknife

For an eligible two-group comparison with at least three explicitly matched
biological replicate IDs, V3 can calculate a leave-one-pair-out jackknife after
the ordinary FullPixel workflow. For each replicate ID, its sample is omitted
from both groups. Pixels are never treated as replicates. The chosen primary
intensity family has already been normalized (if applicable) and is not
normalized again.

Each leave-one-out estimate uses the same complete-replicate group mean,
positive-denominator rule, missing-value behavior, and
`log2(mean(comparison) / mean(reference))` formula as the full estimate. For
`n` matched-pair estimates, the classical standard error is
`sqrt(((n - 1) / n) * sum((theta_i - mean(theta))^2))`. If any estimate needed
for a feature is invalid, summary statistics remain missing and requested/valid
counts expose the incompleteness; no imputation or pseudocount is introduced.

Direction stability requires all estimates to be strictly positive for a
positive full estimate, strictly negative for a negative full estimate, and
exactly zero for an exact-zero full estimate. The full-data log2 fold change
remains the primary point estimate.
