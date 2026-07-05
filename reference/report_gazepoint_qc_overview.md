# Report Gazepoint QC overview

Produces compact, cautious text from a QC summary bundle. The report
describes available QC outputs and status patterns, but it does not
replace the underlying audit, readiness-gate, checklist, or
exclusion-recommendation functions.

## Usage

``` r
report_gazepoint_qc_overview(qc_bundle, max_objects = 5)
```

## Arguments

- qc_bundle:

  A `gp3_qc_summary_bundle`, object-summary data frame, or list of
  objects that can be passed to
  [`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md).

- max_objects:

  Maximum number of non-pass objects to name in the report.

## Value

A list with `summary`, `object_summary`, and `report_text`.

## Examples

``` r
x <- list(overview = data.frame(audit_status = "review", message = "Check coverage."))
report_gazepoint_qc_overview(list(example = x))
#> $summary
#> $overview
#>   n_objects n_pass n_warn n_fail n_info n_unknown qc_overview_status
#> 1         1      0      1      0      0         0               warn
#> 
#> $status_counts
#>         qc_status n_objects
#> pass         pass         0
#> warn         warn         1
#> fail         fail         0
#> info         info         0
#> unknown   unknown         0
#> 
#> $object_summary
#>   object_name object_index object_class overview_available n_overview_rows
#> 1     example            1         list               TRUE               1
#>   status_columns message_columns qc_status      qc_message
#> 1   audit_status         message      warn Check coverage.
#> 
#> attr(,"class")
#> [1] "gp3_qc_status_summary" "list"                 
#> 
#> $object_summary
#>   object_name object_index object_class overview_available n_overview_rows
#> 1     example            1         list               TRUE               1
#>   status_columns message_columns qc_status      qc_message
#> 1   audit_status         message      warn Check coverage.
#> 
#> $report_text
#> [1] "QC overview collected 1 object(s): 0 pass, 1 warn, 0 fail, 0 info, and 0 unknown. Overall QC overview status was 'warn'. Object(s) needing review or interpretation: example. This overview is a reporting aid only; it does not replace the underlying audit outputs, readiness gates, or exclusion decisions."
#> 
#> attr(,"class")
#> [1] "gp3_qc_overview_report" "list"                  
```
