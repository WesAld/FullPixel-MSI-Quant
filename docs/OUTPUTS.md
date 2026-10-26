# Output Workbooks

## V2 normalization fields

Pixel output preserves all V1 fields and adds `rms_denominator`,
`valid_rms_denominator`, `rms_normalized_per_million`, `median_denominator`,
`valid_median_denominator`, `median_normalized_per_million`,
`primary_normalization_method`, and `primary_intensity`.

Feature summaries retain legacy `normalized_*` names exclusively for
spectrum-sum values. RMS uses `rms_normalized_*`; Median uses
`median_normalized_*`; Raw retains established `raw_*` fields. Explicit
`primary_mean_all_valid_pixels` and
`primary_mean_conservative_detected_pixels` fields route the selected family.

Legacy TIC-named sheets and columns remain spectrum-sum-only. Non-spectrum-sum
V2 exports use method-explicit primary names and include normalization
provenance.

## Quantify Samples Only

The five-sheet workbook contains:

### `Normalized_Intensity`

One spectrum-sum-normalized overall-signal column per registered sample.

### `Detected_Pixel_Intensity`

One local strong-pixel intensity column per registered sample.

### `Detection`

Detected-pixel counts, detection fractions, and related individual-sample
fields.

### `QC`

Workbook, feature-retention, sample-column, numerical, and provenance checks.

### `README`

Project identity, exposure, ion mode, sample list, normalization, detection
thresholds, interpretation, and validation classification.

Quantify-only workbooks contain no group means, fold changes, p-values,
pairing, pseudocounts, or imputation.

## Compare Two Groups

The six-sheet workbook contains:

### `Mean_Log2FC`

Overall, strong-pixel, and detection-change comparison readouts.

### `All_Pixels`

Full-pixel and detection-oriented sample and group fields.

### `TIC_normalized_intensity`

Spectrum-sum-normalized individual sample values, group means, fold change, and
log2 fold change.

### `All_Features_TIC_Normalized`

The complete feature audit table. It retains all registered/extracted features,
including unannotated features, individual sample intensities, group means,
fold change, log2 fold change, and available MZmine scores.

### `QC`

Workbook and numerical audit checks.

### `README`

Project and interpretation metadata.

The current generic two-group export preserves all feature rows across the four
data sheets.

## Compare Multiple Groups

The six-sheet names are the same as the two-group workbook.

- `Mean_Log2FC`, `All_Pixels`, and `TIC_normalized_intensity` are
  annotation-focused tables.
- `All_Features_TIC_Normalized` retains every registered/extracted feature.
- Each treatment has its own reference-versus-treatment group means, fold
  change, and log2 fold change.
- All registered individual samples are retained.
- The `QC` sheet records the number of conditions, samples, comparisons,
  feature rows, and preservation checks.

## General interpretation

- Positive log2 fold change: comparison mean exceeds reference mean.
- Negative log2 fold change: reference mean exceeds comparison mean.
- Missing log2 fold change: ratio could not be calculated under the no-
  pseudocount policy.
- Detection-fraction change is an arithmetic difference, not a log ratio.
- MZmine annotation scores and names remain metadata, not confirmed compound
  identities.

Excel workbooks are validated before activation and download. A workbook whose
sheet contract, row retention, sample columns, comparison columns, or numerical
audit fails is not activated as the final result.
