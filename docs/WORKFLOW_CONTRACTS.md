# Workflow Contracts

## Stage chain

```text
registered imzML/IBD + aligned MZmine targets
    -> Extract Pixel Intensities
    -> Summarize Replicate Readouts
    -> Integrate MZmine Annotations
    -> Create Combined Publication Export
    -> Validate Final Workbook
```

Each stage consumes exact registered parent artifacts and creates new
artifacts. A stage never selects an arbitrary newest file.

## Run identity

Every attempt has an immutable run identifier, input hashes, configuration
snapshot, adapter identity, scientific-core version, log paths, output paths,
validation records, and terminal state.

Retries reuse only validated completed checkpoints. The failed stage receives a
clean retry workspace; the previous failed workspace is retained or archived.

## Activation

An output becomes active only after:

- the worker exits successfully;
- required files exist;
- output hashes are recorded;
- row and column contracts pass;
- quantitative-preservation checks pass where applicable;
- the workbook contract passes for final results.

Replacing an active upstream artifact marks dependent downstream artifacts
stale rather than deleting history.

## Analysis-design contracts

### Quantify samples only

- one or more samples;
- no comparison;
- no group statistics.

### Compare two groups

- exactly two groups;
- one reference and one comparison;
- at least two replicates per group;
- equal replicate counts.

### Compare multiple groups

- at least three groups;
- one shared reference;
- every non-reference group appears in one comparison;
- at least two replicates per group;
- unequal replicate counts allowed.

## Scientific preservation

Annotation and export stages must not redefine upstream quantitative values.
Boolean QC fields are compared as Boolean values, numeric fields use a strict
floating-point tolerance, and text fields are compared directly.
