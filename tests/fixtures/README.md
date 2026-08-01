# Regression Fixtures

This structure separates fixture roles:

- `external/`: metadata for real files retained outside Git;
- `expected/`: approved immutable expected outputs only;
- `synthetic/`: clearly labelled non-scientific schema/failure fixtures;
- `metadata/`: fixture manifests and checksums; and
- `reports/`: generated local regression reports, ignored except explanatory
  files.

No real scientific data are committed during M0. External candidate outputs
are not golden results until provenance, access, and scientific approval are
resolved. Synthetic records test infrastructure and schemas only; they are not
scientific validation.

Register local restricted fixtures by adding a local metadata file derived from
`metadata/external_fixture_registry.example.yml`. Do not commit private paths or
data without authorization.
