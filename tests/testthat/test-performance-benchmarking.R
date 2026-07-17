test_that("performance limits are explicit and complete", {
  limits <- gp3tools_performance_limits()

  expect_s3_class(limits, "data.frame")
  expect_equal(
    limits$operation,
    c("generate", "import", "master", "sampling", "quality")
  )
  expect_true(all(limits$max_seconds_per_million_rows > 0))
  expect_true(all(limits$max_heap_delta_mb_per_million_rows > 0))
  expect_true(all(limits$max_scaling_exponent > 1))
})

test_that("small generation benchmark returns auditable measurements", {
  out <- benchmark_gazepoint_export_performance(
    scales = data.frame(total_rows = 500L, n_files = 2L),
    operations = "generate",
    trials = 1L,
    stop_on_regression = FALSE
  )

  expect_s3_class(out, "gazepoint_performance_benchmark")
  expect_equal(nrow(out$trials), 1)
  expect_equal(out$trials$status, "ok")
  expect_equal(out$trials$total_rows, 500)
  expect_true(out$trials$elapsed_s >= 0)
  expect_true(out$trials$output_size_mb > 0)
  expect_true(is.logical(out$regression$overall$pass))
})

test_that("performance regression checks detect explicit failures", {
  summary <- data.frame(
    scale_id = c(1L, 2L),
    total_rows = c(100000L, 200000L),
    n_files = c(1L, 2L),
    rows_per_file = c(100000L, 100000L),
    operation = c("import", "import"),
    n_trials = c(3L, 3L),
    n_success = c(3L, 3L),
    median_elapsed_s = c(30, 300),
    minimum_elapsed_s = c(28, 290),
    maximum_elapsed_s = c(32, 310),
    median_heap_delta_mb = c(20, 500),
    maximum_heap_delta_mb = c(22, 520),
    median_output_size_mb = c(10, 20)
  )

  strict_limits <- data.frame(
    operation = "import",
    max_seconds_per_million_rows = 500,
    max_heap_delta_mb_per_million_rows = 500,
    max_scaling_exponent = 1.2
  )

  audit <- check_gazepoint_performance_regression(
    summary,
    limits = strict_limits
  )

  expect_s3_class(audit, "gazepoint_performance_regression")
  expect_false(audit$overall$pass)
  expect_true(any(audit$checks$status == "fail"))
})

test_that("baseline-relative performance checks are supported", {
  current <- data.frame(
    scale_id = 1L,
    total_rows = 100000L,
    n_files = 1L,
    rows_per_file = 100000L,
    operation = "generate",
    n_trials = 3L,
    n_success = 3L,
    median_elapsed_s = 2,
    minimum_elapsed_s = 1.8,
    maximum_elapsed_s = 2.2,
    median_heap_delta_mb = 20,
    maximum_heap_delta_mb = 22,
    median_output_size_mb = 10
  )
  baseline <- current
  baseline$median_elapsed_s <- 1
  baseline$median_heap_delta_mb <- 10

  audit <- check_gazepoint_performance_regression(
    current,
    baseline = baseline,
    elapsed_ratio_limit = 1.5,
    memory_ratio_limit = 1.5
  )

  expect_false(audit$overall$pass)
  expect_equal(unique(audit$evaluated$elapsed_ratio), 2)
  expect_equal(unique(audit$evaluated$memory_ratio), 2)
})

test_that("performance benchmark tables can be written", {
  out <- benchmark_gazepoint_export_performance(
    scales = data.frame(total_rows = 200L, n_files = 1L),
    operations = "generate",
    trials = 1L
  )
  directory <- tempfile("gp3tools-performance-output-")

  files <- write_gazepoint_performance_benchmark(
    out,
    directory
  )

  expect_true(all(file.exists(files)))
  expect_named(files, c("trials", "summary", "checks", "evaluated"))
})

test_that("operation completion requires every trial to succeed", {
  summary <- data.frame(
    scale_id = 1L,
    total_rows = 100000L,
    n_files = 1L,
    rows_per_file = 100000L,
    operation = "generate",
    n_trials = 3L,
    n_success = 2L,
    median_elapsed_s = 1,
    minimum_elapsed_s = 0.9,
    maximum_elapsed_s = 1.1,
    median_heap_delta_mb = 10,
    maximum_heap_delta_mb = 12,
    median_output_size_mb = 5
  )

  audit <- check_gazepoint_performance_regression(summary)
  completion <- audit$checks[
    audit$checks$check == "operation_completed",
    ,
    drop = FALSE
  ]

  expect_equal(completion$status, "fail")
  expect_false(audit$overall$pass)
})
