# Subprocess Boundary

Scientific stages run outside the interactive Shiny process.

## R responsibilities

The R application:

- validates project and analysis configuration;
- prepares immutable JSON requests;
- starts and observes workers;
- persists run state, logs, artifacts, validation records, and provenance;
- activates outputs only after validation.

## Worker responsibilities

R or Python workers:

- operate in a run-specific workspace;
- invoke the required adapter and frozen scientific source;
- write stdout and stderr to registered log files;
- produce a machine-readable report;
- never update the frozen source hashes;
- never select unregistered inputs.

## Failure behavior

A nonzero exit status, missing report, hash mismatch, missing output, row loss,
column mismatch, quantitative-preservation failure, or workbook-contract
failure prevents output activation.

Cancellation and retry decisions are based on persisted worker identity and
run state, not browser state.
