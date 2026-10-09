# Large-export performance benchmarking

## Purpose

This workflow measures elapsed time, approximate R-heap growth, output
size, and empirical scaling across increasing Gazepoint export sizes. It
is a regression framework rather than a hardware-independent speed
claim.

``` r

gp3tools_performance_limits()
#>   operation max_seconds_per_million_rows max_heap_delta_mb_per_million_rows
#> 1  generate                           90                               1200
#> 2    import                          240                               1800
#> 3    master                          240                               1800
#> 4  sampling                          180                               1200
#> 5   quality                          180                               1200
#>   max_scaling_exponent
#> 1                  1.6
#> 2                  1.6
#> 3                  1.6
#> 4                  1.6
#> 5                  1.6
```

The default limits are deliberately conservative. Projects can supply a
machine-specific baseline and stricter ratio limits after collecting
stable measurements on the same system.

## Fast contract run

A small run verifies the complete benchmark contract without slowing
package checks.

``` r

small <- benchmark_gazepoint_export_performance(
  scales = data.frame(
    total_rows = c(1000L, 5000L),
    n_files = c(1L, 2L)
  ),
  operations = "generate",
  trials = 1L
)

small$summary
#>   scale_id total_rows n_files rows_per_file operation n_trials n_success
#> 1        1       1000       1          1000  generate        1         1
#> 2        2       5000       2          2500  generate        1         1
#>   median_elapsed_s minimum_elapsed_s maximum_elapsed_s median_heap_delta_mb
#> 1            0.004             0.004             0.004             1.949677
#> 2            0.007             0.007             0.007             3.578438
#>   maximum_heap_delta_mb median_output_size_mb
#> 1              1.949677             0.1873322
#> 2              3.578438             0.9200592
small$regression$overall
#>    pass n_checks n_pass n_fail
#> 1 FALSE       12     11      1
```

## Large-export profile

The installed script uses 60,000, 240,000, and 960,000 rows distributed
across 1, 4, and 16 files, respectively. Run it outside ordinary R CMD
checks:

``` r

source(
  system.file(
    "benchmarks",
    "run-large-export-performance.R",
    package = "gp3tools"
  )
)
```

On Windows CMD, set `GP3TOOLS_BENCHMARK_OUTPUT` to an external folder
before running the script. Benchmark outputs should not be committed
when they contain machine-specific paths or private export data.

## Baseline-relative regression checks

A saved result can be used as a baseline:

``` r

current_audit <- check_gazepoint_performance_regression(
  current_benchmark,
  baseline = previous_benchmark,
  elapsed_ratio_limit = 1.25,
  memory_ratio_limit = 1.25
)
```

Absolute and baseline-relative failures remain visible in the audit
table.
