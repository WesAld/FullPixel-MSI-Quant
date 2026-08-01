# Validation Status

## Classification

FullPixel MSI Quant 1.0.0 is released as a **Technical Validation Candidate**.

This means the tested workflows completed end to end with structural,
numerical, preservation, workbook, and provenance checks. It does not mean the
software has been independently scientifically validated against a separate
software pipeline, certified reference material, or external reference oracle.

## Completed workflow tests

### Quantify samples only

External PVL Positive-mode test project:

- 6 individual samples;
- 2,558 features;
- correct five-sheet workbook;
- individual sample columns retained;
- no group statistics or fold-change columns;
- QC and formula checks passed.

### Compare two groups

External PVL Positive and Negative test project:

- 3 reference and 3 comparison replicates;
- 3,802 total features;
- 428 annotated and 3,374 unannotated features;
- available MZmine scores retained;
- correct six-sheet workbook;
- all feature rows retained in the generic comparison data sheets;
- group means, fold change, and log2 fold change retained;
- QC and formula checks passed.

### Compare multiple groups

External PVL Positive-mode software test:

- 3 configured conditions;
- 6 individual samples;
- 2 reference-versus-treatment comparisons;
- 2,558 total features in the all-features sheet;
- 338 annotation-focused rows;
- all individual sample columns and both comparisons retained;
- correct six-sheet workbook;
- QC and formula checks passed.

The third group in this software test deliberately combined samples from
different original biological conditions. It validated software behavior only
and must not be biologically interpreted.

## Frozen scientific-core integrity

Registered SHA-256 values:

- Script 01:
  `D846E8BB354B3BED88A321B8D61FE0BE2F5FE42943C3365F12AAFD6C47F96CB0`
- Script 03:
  `3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97`
- Script 04:
  `1150E5F10565B7C25784A9DD875380ACC748C95A8D5BE7B0431C1C5767C587D1`
- Script 07:
  `94055F3210D95D6AF0EA35D246D9F234A24FF429496E179DA91EE58A1FBBB0A0`

The release package preserves these files byte-for-byte. `.gitattributes`
disables text normalization for the frozen R files.

## Validated environment

The principal validation environment was:

- Windows 11 x64;
- R 4.6.0 UCRT;
- Cardinal 3.14.0;
- data.table 1.18.4;
- stringr 1.6.0;
- matrixStats 1.5.0;
- openxlsx 4.2.8.1;
- Shiny 1.14.0.

Python adapters require pandas and openpyxl. The release uses version ranges
rather than claiming one fully locked clean-room environment.

## Configurable mass-tolerance status

The validated v1.0.0 extraction default remains the larger of 10 ppm and
0.002 Da. A custom strict combined control is available for technical
evaluation. It accepts both ppm and absolute Da half-window values and uses the
smaller result for each target m/z. Custom strict runs are explicitly
classified as Technical Validation Candidates and require dataset-specific
review. The frozen scientific source hashes are unchanged; the strict rule is
applied only to the generated execution copy.

## Remaining limitations

- No independent cross-software quantitative oracle.
- No clinical or diagnostic validation.
- No confirmed molecular identities without orthogonal evidence.
- Other operating systems are not part of the primary validation claim.
- A fresh combined Positive-plus-Negative multi-group run is additional
  robustness testing rather than part of the completed v1 test set.
- Large raw MSI data and private project outputs are not distributed with the
  repository.

## Appropriate claims

Appropriate:

> FullPixel MSI Quant 1.0.0 passed technical workflow validation using PVL
> MALDI-MSI test datasets.

Not appropriate:

> FullPixel MSI Quant is universally scientifically validated or approved for
> diagnostic use.
