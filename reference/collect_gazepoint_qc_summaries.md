# Collect Gazepoint QC summaries

Collects compact overview/status information from gp3tools audit,
workflow, checklist, readiness, diagnostic, or reporting objects. The
helper is intended to make existing QC outputs easier to review
together; it does not rerun checks or define exclusion rules.

## Usage

``` r
collect_gazepoint_qc_summaries(
  objects,
  object_names = NULL,
  name = "gazepoint_qc_summary_bundle",
  include_overview_rows = TRUE
)
```

## Arguments

- objects:

  A list of gp3tools objects, audit objects, overview data frames, or a
  single such object.

- object_names:

  Optional character names for unnamed objects.

- name:

  Character label stored in the returned object.

- include_overview_rows:

  Logical. If `TRUE`, returns a combined long-form table of
  interpretable overview rows.

## Value

A list with `overview`, `object_summary`, `overview_rows`, and
`settings`.

## Examples

``` r
audit <- list(
  overview = data.frame(
    audit_status = "ok",
    message = "Example audit passed."
  )
)
collect_gazepoint_qc_summaries(list(example_audit = audit))
#> $overview
#>                   object_name n_objects n_overview_rows n_pass n_warn n_fail
#> 1 gazepoint_qc_summary_bundle         1               1      1      0      0
#>   n_info n_unknown qc_bundle_status
#> 1      0         0             pass
#> 
#> $object_summary
#>     object_name object_index object_class overview_available n_overview_rows
#> 1 example_audit            1         list               TRUE               1
#>   status_columns message_columns qc_status            qc_message
#> 1   audit_status         message      pass Example audit passed.
#> 
#> $status_counts
#>         qc_status n_objects
#> pass         pass         1
#> warn         warn         0
#> fail         fail         0
#> info         info         0
#> unknown   unknown         0
#> 
#> $overview_rows
#>   .gp3_qc_object_name .gp3_qc_object_index .gp3_qc_row audit_status
#> 1       example_audit                    1           1           ok
#>                 message
#> 1 Example audit passed.
#> 
#> $settings
#>                 setting                       value
#> 1                  name gazepoint_qc_summary_bundle
#> 2 include_overview_rows                        TRUE
#> 
#> attr(,"class")
#> [1] "gp3_qc_summary_bundle" "list"                 
```
