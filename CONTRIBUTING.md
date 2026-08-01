# Contributing

Thank you for helping improve FullPixel MSI Quant.

## Bug reports

Open a GitHub issue and include:

- application version;
- operating system, R version, and Python version;
- analysis design and ion mode;
- the exact workflow stage that failed;
- the displayed error message;
- a minimal reproducible example that does not contain private or restricted
  scientific data.

Do not upload raw human-derived data, confidential project files, access
tokens, or personal filesystem paths unless they are essential and safely
redacted.

## Code changes

1. Create a branch from the current release.
2. Keep repository paths portable and relative.
3. Add or update tests for behavioral changes.
4. Run:

```powershell
python .	oolserify_release.py
python .	oolsalidate_contracts.py
Rscript .	oolserify_scientific_core.R
```

5. Explain the reason, scope, and validation evidence in the pull request.

## Scientific-method changes

The files under `validated-source/` are immutable reference sources. Do not
edit, reformat, or replace them in a normal code change.

A proposed scientific change must:

- identify the affected formula, threshold, rule, or output;
- provide a scientific rationale;
- create a new scientific-core version;
- preserve the previous frozen version;
- update profiles and contracts;
- include regression evidence and a difference report; and
- undergo explicit scientific review.

Changing a registered hash without explaining and validating the scientific
difference is not an acceptable repair.
