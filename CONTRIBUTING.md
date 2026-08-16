# Contributing to gp3tools

Thank you for helping improve gp3tools.

## Reporting bugs

Please include a minimal reproducible example, the function call that
failed, the exact error message, and
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).

Do not upload private Gazepoint participant exports. If an import
problem depends on file structure, share column names, synthetic rows,
or a fully anonymised minimal example.

## Feature requests

Please describe the Gazepoint workflow, the expected input columns, and
the desired returned object or output table.

## Pull requests

Pull requests should:

- avoid committing private data;
- include tests for new user-facing behaviour;
- update documentation when arguments or outputs change;
- keep optional external-package workflows optional;
- pass `devtools::check()` with 0 errors, 0 warnings, and 0 notes.

## Data privacy

Gazepoint exports may contain participant-level behavioural data. Use
synthetic, anonymised, or aggregated examples in public issues and pull
requests.
