make_test_multiverse_plot_summary <- function() {
  branch_summary <- tibble::tibble(
    result_name = c(
      "pupil",
      "pupil",
      "aoi",
      "aoi"
    ),
    multiverse_family = c(
      "pupil",
      "pupil",
      "aoi",
      "aoi"
    ),
    branch_id = c(
      "pupil_1",
      "pupil_2",
      "aoi_1",
      "aoi_2"
    ),
    branch_label = c(
      "pupil_gap75_smooth3",
      "pupil_gap150_smooth5",
      "aoi_valid_min1",
      "aoi_aoi_only_min3"
    ),
    branch_status = c(
      "completed",
      "completed",
      "completed",
      "failed"
    ),
    output_rows = c(
      24L,
      24L,
      NA_integer_,
      NA_integer_
    ),
    output_cols = c(
      20L,
      20L,
      NA_integer_,
      NA_integer_
    ),
    max_gap_ms = c(
      75,
      150,
      NA,
      NA
    ),
    smoothing_window_samples = c(
      3L,
      5L,
      NA_integer_,
      NA_integer_
    ),
    denominator = c(
      NA_character_,
      NA_character_,
      "valid",
      "aoi_only"
    ),
    min_denominator_samples = c(
      NA_integer_,
      NA_integer_,
      1L,
      3L
    ),
    aoi_window_rows = c(
      NA_integer_,
      NA_integer_,
      8L,
      NA_integer_
    ),
    aoi_glmm_rows = c(
      NA_integer_,
      NA_integer_,
      8L,
      NA_integer_
    ),
    message = c(
      NA_character_,
      NA_character_,
      NA_character_,
      "Example branch failure"
    )
  )

  overview <- tibble::tibble(
    result_name = c("pupil", "aoi", "overall"),
    multiverse_family = c("pupil", "aoi", "combined"),
    n_defined_branches = c(2L, 2L, 4L),
    n_requested_branches = c(2L, 2L, 4L),
    n_completed_branches = c(2L, 1L, 3L),
    n_failed_branches = c(0L, 1L, 1L),
    n_skipped_branches = c(0L, 0L, 0L),
    multiverse_status = c(
      "completed",
      "completed_with_errors",
      "completed_with_errors"
    )
  )

  out <- list(
    overview = overview,
    branch_summary = branch_summary,
    failure_summary = branch_summary[
      branch_summary$branch_status == "failed",
      c(
        "result_name",
        "multiverse_family",
        "branch_id",
        "branch_label",
        "branch_status",
        "message"
      )
    ],
    settings = tibble::tibble(
      setting = "n_result_objects",
      value = "2"
    )
  )

  class(out) <- c("gp3_multiverse_summary_results", "list")

  out
}

make_test_plot_pupil_result <- function() {
  out <- list(
    overview = tibble::tibble(
      multiverse_family = "pupil",
      n_defined_branches = 2L,
      n_requested_branches = 2L,
      n_completed_branches = 2L,
      n_failed_branches = 0L,
      n_skipped_branches = 0L,
      multiverse_status = "completed"
    ),
    branch_results = tibble::tibble(
      branch_id = c("pupil_1", "pupil_2"),
      branch_label = c("pupil_gap75_smooth3", "pupil_gap150_smooth5"),
      preprocessing_family = "pupil",
      artifact_padding_ms = c(0, 0),
      max_gap_ms = c(75, 150),
      smoothing_window_samples = c(3L, 5L),
      baseline_window_start_ms = c(0, 0),
      baseline_window_end_ms = c(100, 100),
      baseline_window_label = c("0_to_100ms", "0_to_100ms"),
      branch_status = c("completed", "completed"),
      output_class = c("tbl_df, tbl, data.frame", "tbl_df, tbl, data.frame"),
      output_rows = c(24L, 24L),
      output_cols = c(20L, 20L),
      message = c(NA_character_, NA_character_)
    ),
    branch_outputs = list(),
    settings = tibble::tibble(
      setting = "keep_outputs",
      value = "FALSE"
    )
  )

  class(out) <- c("gp3_pupil_multiverse_results", "list")

  out
}

test_that("plot_gazepoint_multiverse_results creates status plots", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "status"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results creates row-count plots", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "rows"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results creates pupil parameter plots", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "pupil_parameters",
    family = "pupil"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results creates AOI denominator plots", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "aoi_denominators",
    family = "aoi"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results supports custom titles and branch IDs", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "status",
    title = "Custom multiverse status",
    show_labels = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Custom multiverse status")
})

test_that("plot_gazepoint_multiverse_results accepts direct pupil multiverse results", {
  x <- make_test_plot_pupil_result()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "status"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results filters by family", {
  x <- make_test_multiverse_plot_summary()

  p <- plot_gazepoint_multiverse_results(
    x,
    plot = "status",
    family = "pupil"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_multiverse_results checks invalid inputs", {
  x <- make_test_multiverse_plot_summary()

  expect_error(
    plot_gazepoint_multiverse_results(
      list(),
      plot = "status"
    ),
    "`x` must be a multiverse summary, pupil multiverse result, or AOI multiverse result object",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      plot = "bad_plot"
    )
  )

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      family = "bad_family"
    )
  )

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      show_labels = NA
    ),
    "`show_labels` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_multiverse_results checks empty branch summaries", {
  x <- list(
    overview = tibble::tibble(),
    branch_summary = tibble::tibble(),
    failure_summary = tibble::tibble(),
    settings = tibble::tibble()
  )

  class(x) <- c("gp3_multiverse_summary_results", "list")

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      plot = "status"
    ),
    "`x` does not contain branch-level multiverse results",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_multiverse_results checks unavailable families", {
  x <- make_test_multiverse_plot_summary()

  pupil_only <- x
  pupil_only$branch_summary <- pupil_only$branch_summary[
    pupil_only$branch_summary$multiverse_family == "pupil",
    ,
    drop = FALSE
  ]

  expect_error(
    plot_gazepoint_multiverse_results(
      pupil_only,
      plot = "status",
      family = "aoi"
    ),
    "No branches are available for the requested `family`",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_multiverse_results checks missing row-count columns", {
  x <- make_test_multiverse_plot_summary()

  x$branch_summary$output_rows <- NULL
  x$branch_summary$aoi_glmm_rows <- NULL
  x$branch_summary$aoi_window_rows <- NULL

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      plot = "rows"
    ),
    "No row-count columns are available for the requested multiverse results",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_multiverse_results checks missing pupil plot columns", {
  x <- make_test_multiverse_plot_summary()

  x$branch_summary$max_gap_ms <- NULL

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      plot = "pupil_parameters",
      family = "pupil"
    ),
    "Pupil-parameter plots require columns",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_multiverse_results checks missing AOI plot columns", {
  x <- make_test_multiverse_plot_summary()

  x$branch_summary$denominator <- NULL

  expect_error(
    plot_gazepoint_multiverse_results(
      x,
      plot = "aoi_denominators",
      family = "aoi"
    ),
    "AOI-denominator plots require columns",
    fixed = TRUE
  )
})
