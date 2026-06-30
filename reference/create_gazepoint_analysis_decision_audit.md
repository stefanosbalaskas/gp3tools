# Create a final Gazepoint analysis-decision audit

Create a final audit table for a Gazepoint analysis workflow. The
function records which analysis branches were run, classifies them as
confirmatory, sensitivity, exploratory, diagnostic, preprocessing, or
reporting branches, summarises available diagnostics, flags
interpretation cautions, and creates a final analysis-readiness table.

## Usage

``` r
create_gazepoint_analysis_decision_audit(
  ...,
  results = NULL,
  branch_roles = NULL,
  required_confirmatory = character(),
  diagnostics_required = TRUE,
  require_clean_diagnostics = FALSE
)
```

## Arguments

- ...:

  Named analysis result objects. Each named object is treated as one
  analysis branch.

- results:

  Optional named list of analysis result objects. This can be used
  instead of, or together with, `...`.

- branch_roles:

  Optional data frame describing branch roles. It must contain
  `branch_name` and `decision_type`. Optional columns include
  `analysis_family`, `interpretation_scope`, and `notes`.

- required_confirmatory:

  Character vector of confirmatory branches that must be present for the
  analysis to be considered complete.

- diagnostics_required:

  Logical. If `TRUE`, confirmatory model branches without extractable
  diagnostics are flagged with a caution.

- require_clean_diagnostics:

  Logical. If `TRUE`, diagnostic warnings in required confirmatory
  branches make the final readiness status `not_ready`.

## Value

A list with class `gp3_analysis_decision_audit` containing overview,
branch audit, diagnostics summary, interpretation cautions, readiness,
and settings tables.
