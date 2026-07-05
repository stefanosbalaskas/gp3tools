# Summarize Gazepoint QC status

Summarizes pass/warn/fail/info/unknown status counts from a QC bundle or
an object-summary table produced by
[`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md).

## Usage

``` r
summarize_gazepoint_qc_status(qc_bundle)

summarise_gazepoint_qc_status(qc_bundle)
```

## Arguments

- qc_bundle:

  A `gp3_qc_summary_bundle`, object-summary data frame, or list of
  objects that can be passed to
  [`collect_gazepoint_qc_summaries()`](https://stefanosbalaskas.github.io/gp3tools/reference/collect_gazepoint_qc_summaries.md).

## Value

A list with `overview`, `status_counts`, and `object_summary`.

## Examples

``` r
x <- list(overview = data.frame(audit_status = "ok"))
summarize_gazepoint_qc_status(list(x))
#> $overview
#>   n_objects n_pass n_warn n_fail n_info n_unknown qc_overview_status
#> 1         1      1      0      0      0         0               pass
#> 
#> $status_counts
#>         qc_status n_objects
#> pass         pass         1
#> warn         warn         0
#> fail         fail         0
#> info         info         0
#> unknown   unknown         0
#> 
#> $object_summary
#>   object_name object_index object_class overview_available n_overview_rows
#> 1    object_1            1         list               TRUE               1
#>   status_columns message_columns qc_status                       qc_message
#> 1   audit_status            <NA>      pass QC status interpreted as 'pass'.
#> 
#> attr(,"class")
#> [1] "gp3_qc_status_summary" "list"                 
```
