# Synthetic Fixtures

Synthetic fixtures may test schemas, failure modes, and infrastructure. They
must be labelled `synthetic_non_scientific` and must never be presented as
scientific validation or expected biological results.

Generalization cases are generated in memory by
`tools/validate_generalization.py`. They exercise multiple treatments, more
than three replicates, unequal replicate counts, custom identifiers, invalid
mappings, and the PVL reference boundary. They contain no quantitative MSI
results and are not scientific regression evidence.
