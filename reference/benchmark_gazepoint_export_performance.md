# Benchmark gp3tools on increasingly large Gazepoint exports

Generates deterministic Gazepoint-like all-gaze exports and benchmarks
selected package operations across increasing row and file counts.
Ordinary unit tests should use small scales. The script installed under
`inst/benchmarks/` runs the large-export profile.

## Usage

``` r
benchmark_gazepoint_export_performance(
  scales = data.frame(total_rows = c(10000L, 50000L, 200000L), n_files = c(1L, 4L, 8L)),
  operations = c("generate", "import", "master", "sampling", "quality"),
  trials = 3L,
  seed = 20260717L,
  limits = gp3tools_performance_limits(),
  stop_on_regression = FALSE,
  output_dir = NULL,
  keep_exports = FALSE,
  on_error = c("record", "stop")
)
```

## Arguments

- scales:

  Data frame with integer `total_rows` and `n_files` columns.

- operations:

  Any of `"generate"`, `"import"`, `"master"`, `"sampling"`, and
  `"quality"`.

- trials:

  Number of repetitions per scale.

- seed:

  Integer random seed.

- limits:

  Performance limits returned by
  [`gp3tools_performance_limits()`](https://stefanosbalaskas.github.io/gp3tools/reference/gp3tools_performance_limits.md)
  or a compatible data frame.

- stop_on_regression:

  Stop when a completed benchmark exceeds a limit.

- output_dir:

  Optional directory used for generated exports.

- keep_exports:

  Retain generated CSV exports.

- on_error:

  Whether operation errors are recorded or stop the benchmark.

## Value

A `"gazepoint_performance_benchmark"` object containing trial-level
measurements, aggregated summaries, regression checks, settings, and
session metadata.
