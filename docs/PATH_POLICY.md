# Path Policy

The repository root is resolved dynamically from the launch script,
`FULLPIXEL_MSI_QUANT_ROOT`, or the current repository context.

## Repository files

Repository-internal files use relative paths. Source code, profiles, schemas,
documentation, frozen scientific scripts, and tests must not depend on one
developer's drive letter, username, cloud-synced folder, or project directory.

## External scientific files

User-selected raw data, aligned MZmine tables, annotated tables, project
folders, and output locations may be absolute paths. Exact paths and hashes are
retained in the external project record for reproducibility.

## Public repository exclusions

Do not commit:

- raw imzML/IBD or vendor data;
- project folders and run workspaces;
- generated workbooks and CSV outputs;
- logs containing personal paths;
- backups, caches, lock files, or compiled Python files;
- credentials or private identifiers.

The `.gitignore` file encodes these exclusions.
