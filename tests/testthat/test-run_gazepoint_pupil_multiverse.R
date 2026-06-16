make_test_pupil_multiverse_data <- function() {
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

make_test_pupil_multiverse <- function() {
  create_gazepoint_preprocessing_multiverse(
    pupil_max_gap_ms = c(75, 150),
    pupil_smoothing_window_samples = 3,
    pupil_baseline_windows = list(c(0, 100)),
    pupil_artifact_padding_ms = 0,
    include_pupil = TRUE,
    include_aoi = FALSE,
    label_prefix = "toy"
  )
}

test_that("run_gazepoint_pupil_multiverse runs all pupil branches", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  out <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    keep_outputs = TRUE
  )

  expect_s3_class(out, "gp3_pupil_multiverse_results")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "branch_results",
      "branch_outputs",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$branch_results, "tbl_df")
  expect_type(out$branch_outputs, "list")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$multiverse_family, "pupil")
  expect_equal(out$overview$n_defined_branches, 2)
  expect_equal(out$overview$n_requested_branches, 2)
  expect_equal(out$overview$n_completed_branches, 2)
  expect_equal(out$overview$n_failed_branches, 0)
  expect_equal(out$overview$multiverse_status, "completed")

  expect_equal(nrow(out$branch_results), 2)
  expect_true(all(out$branch_results$branch_status == "completed"))
  expect_true(all(out$branch_results$output_rows == nrow(toy_pupil)))
  expect_true(all(out$branch_results$output_cols >= ncol(toy_pupil)))
  expect_true(all(is.na(out$branch_results$message)))

  expect_equal(
    names(out$branch_outputs),
    c("toy_pupil_1", "toy_pupil_2")
  )

  expect_true(all(vapply(out$branch_outputs, is.data.frame, logical(1))))
})

test_that("run_gazepoint_pupil_multiverse can run selected branch IDs", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  out <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    branch_ids = "toy_pupil_2",
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    keep_outputs = TRUE
  )

  expect_equal(out$overview$n_requested_branches, 1)
  expect_equal(out$overview$n_completed_branches, 1)
  expect_equal(out$overview$n_failed_branches, 0)

  expect_equal(out$branch_results$branch_id, "toy_pupil_2")
  expect_equal(names(out$branch_outputs), "toy_pupil_2")
})

test_that("run_gazepoint_pupil_multiverse can omit branch outputs", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  out <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    keep_outputs = FALSE
  )

  expect_equal(out$overview$n_completed_branches, 2)
  expect_equal(length(out$branch_outputs), 0)
})

test_that("run_gazepoint_pupil_multiverse standardises common Gazepoint group aliases", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  out <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    keep_outputs = TRUE
  )

  expect_equal(out$overview$n_completed_branches, 2)
  expect_true(all(out$branch_results$branch_status == "completed"))

  first_output <- out$branch_outputs[[1]]

  expect_true("media_id" %in% names(first_output))
  expect_true("MEDIA_ID" %in% names(first_output))
})

test_that("run_gazepoint_pupil_multiverse records branch failures when stop_on_error is FALSE", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  out <- run_gazepoint_pupil_multiverse(
    toy_pupil,
    mv,
    pupil_col = "pupil",
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    summarise_windows = TRUE,
    windows = NULL,
    keep_outputs = TRUE,
    stop_on_error = FALSE
  )

  expect_equal(out$overview$n_completed_branches, 0)
  expect_equal(out$overview$n_failed_branches, 2)
  expect_equal(out$overview$multiverse_status, "completed_with_errors")

  expect_true(all(out$branch_results$branch_status == "failed"))
  expect_true(all(grepl(
    "`windows` must be supplied when `summarise_windows = TRUE`",
    out$branch_results$message,
    fixed = TRUE
  )))

  expect_equal(length(out$branch_outputs), 0)
})

test_that("run_gazepoint_pupil_multiverse stops on branch failure when requested", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      pupil_col = "pupil",
      time_col = "time",
      group_cols = c("subject", "MEDIA_ID", "trial_global"),
      summarise_windows = TRUE,
      windows = NULL,
      stop_on_error = TRUE
    ),
    "`windows` must be supplied when `summarise_windows = TRUE`",
    fixed = TRUE
  )
})

test_that("run_gazepoint_pupil_multiverse checks invalid inputs", {
  toy_pupil <- make_test_pupil_multiverse_data()
  mv <- make_test_pupil_multiverse()

  expect_error(
    run_gazepoint_pupil_multiverse(
      data = NULL,
      multiverse = mv
    ),
    "`data` must be supplied",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      multiverse = list()
    ),
    "`multiverse` must be created by `create_gazepoint_preprocessing_multiverse()`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      branch_ids = "unknown_branch"
    ),
    "`branch_ids` contains unknown pupil branch ID",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      branch_ids = NA_character_
    ),
    "`branch_ids` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      summarise_windows = NA
    ),
    "`summarise_windows` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      keep_outputs = NA
    ),
    "`keep_outputs` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv,
      stop_on_error = NA
    ),
    "`stop_on_error` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("run_gazepoint_pupil_multiverse requires pupil branches", {
  toy_pupil <- make_test_pupil_multiverse_data()

  mv_aoi_only <- create_gazepoint_preprocessing_multiverse(
    include_pupil = FALSE,
    include_aoi = TRUE,
    aoi_denominators = "valid",
    aoi_min_denominator_samples = 1,
    label_prefix = "aoi_only"
  )

  expect_error(
    run_gazepoint_pupil_multiverse(
      toy_pupil,
      mv_aoi_only
    ),
    "`multiverse` does not contain pupil branches",
    fixed = TRUE
  )
})
