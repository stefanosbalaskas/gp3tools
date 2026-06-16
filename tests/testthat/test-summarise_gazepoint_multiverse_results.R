make_test_summary_pupil_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 12),
    MEDIA_ID = rep(1, 24),
    trial_global = rep(rep(1:2, each = 6), 2),
    time = rep(c(0, 50, 100, 150, 200, 250), 4),
    pupil = c(
      100, 101, NA, 103, 104, 105,
      99, 100, 101, NA, 103, 104,
      110, 111, NA, 113, 114, 115,
      108, 109, 110, NA, 112, 113
    )
  )
}

make_test_summary_aoi_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 12),
    MEDIA_ID = rep(1, 24),
    trial_global = rep(rep(1:2, each = 6), 2),
    condition = rep(c("A", "B", "A", "B"), each = 6),
    time = rep(c(0, 50, 100, 150, 200, 250), 4),
    aoi_current = c(
      "target", "target", "distractor", "none", "target", "none",
      "distractor", "target", "target", "none", "target", "distractor",
      "target", "none", "target", "distractor", "target", "none",
      "distractor", "target", "none", "target", "target", "none"
    )
  )
}

make_test_summary_pupil_results <- function() {
  toy_pupil <- make_test_summary_pupil_data()

  mv <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(75, 150),
    pupil_smoothing_window_samples = 3,
    pupil_baseline_windows = list(c(0, 100)),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "toy"
  )

  run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    keep_outputs = FALSE
  )
}

make_test_summary_aoi_results <- function() {
  toy_aoi <- make_test_summary_aoi_data()

  mv <- create_gazepoint_preprocessing_multiverse(
    aoi_denominators = c("valid", "aoi_only"),
    aoi_min_denominator_samples = c(1, 3),
    include_pupil = FALSE,
    include_aoi = TRUE,
    label_prefix = "toy"
  )

  run_gazepoint_aoi_multiverse(
    toy_aoi,
    mv,
    windows = c(0, 150, 300),
    time_col = "time",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "target",
    distractor_aoi_values = "distractor",
    keep_outputs = FALSE
  )
}

test_that("summarise_gazepoint_multiverse_results summarises pupil and AOI results", {
  pupil_results <- make_test_summary_pupil_results()
  aoi_results <- make_test_summary_aoi_results()

  out <- summarise_gazepoint_multiverse_results(
    pupil = pupil_results,
    aoi = aoi_results
  )

  expect_s3_class(out, "gp3_multiverse_summary_results")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "branch_summary",
      "failure_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$branch_summary, "tbl_df")
  expect_s3_class(out$failure_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(nrow(out$overview), 3)
  expect_equal(out$overview$result_name, c("pupil", "aoi", "overall"))

  overall <- out$overview[out$overview$result_name == "overall", ]

  expect_equal(overall$n_defined_branches, 6)
  expect_equal(overall$n_requested_branches, 6)
  expect_equal(overall$n_completed_branches, 6)
  expect_equal(overall$n_failed_branches, 0)
  expect_equal(overall$n_skipped_branches, 0)
  expect_equal(overall$multiverse_status, "completed")

  expect_equal(nrow(out$branch_summary), 6)
  expect_true(all(out$branch_summary$branch_status == "completed"))
  expect_equal(nrow(out$failure_summary), 0)
})

test_that("summarise_gazepoint_multiverse_results accepts a results list", {
  pupil_results <- make_test_summary_pupil_results()
  aoi_results <- make_test_summary_aoi_results()

  out <- summarise_gazepoint_multiverse_results(
    results = list(
      pupil_run = pupil_results,
      aoi_run = aoi_results
    )
  )

  expect_s3_class(out, "gp3_multiverse_summary_results")

  expect_equal(
    out$overview$result_name,
    c("pupil_run", "aoi_run", "overall")
  )

  expect_equal(
    out$settings$value[out$settings$setting == "n_result_objects"],
    "2"
  )

  expect_equal(
    out$settings$value[out$settings$setting == "result_names"],
    "pupil_run, aoi_run"
  )
})

test_that("summarise_gazepoint_multiverse_results creates default names when missing", {
  pupil_results <- make_test_summary_pupil_results()

  out <- summarise_gazepoint_multiverse_results(pupil_results)

  expect_s3_class(out, "gp3_multiverse_summary_results")
  expect_equal(
    out$overview$result_name,
    c("pupil_multiverse_1", "overall")
  )
})

test_that("summarise_gazepoint_multiverse_results reports failures", {
  toy_pupil <- make_test_summary_pupil_data()

  mv <- create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(75, 150),
    pupil_smoothing_window_samples = 3,
    pupil_baseline_windows = list(c(0, 100)),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "toy"
  )

  failed_pupil <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    summarise_windows = TRUE,
    windows = NULL,
    keep_outputs = FALSE,
    stop_on_error = FALSE
  )

  out <- summarise_gazepoint_multiverse_results(
    pupil_failed = failed_pupil
  )

  expect_s3_class(out, "gp3_multiverse_summary_results")

  overall <- out$overview[out$overview$result_name == "overall", ]

  expect_equal(overall$n_requested_branches, 2)
  expect_equal(overall$n_completed_branches, 0)
  expect_equal(overall$n_failed_branches, 2)
  expect_equal(overall$multiverse_status, "completed_with_errors")

  expect_equal(nrow(out$failure_summary), 2)
  expect_true(all(out$failure_summary$branch_status == "failed"))
  expect_true(all(grepl(
    "`windows` must be supplied when `summarise_windows = TRUE`",
    out$failure_summary$message,
    fixed = TRUE
  )))
})

test_that("summarise_gazepoint_multiverse_results handles one result object", {
  pupil_results <- make_test_summary_pupil_results()

  out <- summarise_gazepoint_multiverse_results(
    pupil = pupil_results
  )

  expect_equal(nrow(out$overview), 2)
  expect_equal(out$overview$result_name, c("pupil", "overall"))

  overall <- out$overview[out$overview$result_name == "overall", ]

  expect_equal(overall$n_requested_branches, 2)
  expect_equal(overall$n_completed_branches, 2)
  expect_equal(overall$n_failed_branches, 0)
  expect_equal(overall$multiverse_status, "completed")

  expect_equal(nrow(out$branch_summary), 2)
})

test_that("summarise_gazepoint_multiverse_results checks invalid inputs", {
  expect_error(
    summarise_gazepoint_multiverse_results(),
    "At least one multiverse result object must be supplied",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_multiverse_results(
      results = data.frame(x = 1)
    ),
    "`results` must be a named list of multiverse result objects",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_multiverse_results(
      results = list(bad = list())
    ),
    "All supplied objects must be pupil or AOI multiverse result objects",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_multiverse_results(
      bad = list()
    ),
    "All supplied objects must be pupil or AOI multiverse result objects",
    fixed = TRUE
  )
})
