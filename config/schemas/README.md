# Contract schemas

These JSON Schema 2020-12 documents define orchestration records only. They do
not define or execute scientific calculations. Example records in
`../examples/` are explicitly non-scientific. Run
`python tools/validate_contracts.py` from the repository root to check schema
syntax, valid examples, controlled vocabularies, required fields, and expected
failure of invalid examples.

Generic schemas validate record structure. Cross-record uniqueness,
referential integrity, and selected-profile compatibility are separate semantic
checks. Run `python tools/validate_generalization.py` to test dynamic
conditions, arbitrary and unequal replicate counts, multiple comparisons,
custom names, invalid mappings, and the PVL compatibility boundary.
