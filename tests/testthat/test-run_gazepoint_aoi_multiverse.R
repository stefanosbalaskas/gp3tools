make_test_aoi_multiverse_data <- function() {
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

make_test_aoi_multiverse <- function() {
  create_gazepoint_preprocessing_multiverse(
    aoi_denominators = c("valid", "aoi_only"),
    aoi_min_denominator_samples = c(1, 3),
    include_pupil = FALSE,
    include_aoi = TRUE,
    label_prefix = "toy"
  )
}

test_that("run_gazepoint_aoi_multiverse runs all AOI branches", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
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
    keep_outputs = TRUE
  )

  expect_s3_class(out, "gp3_aoi_multiverse_results")
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

  expect_equal(out$overview$multiverse_family, "aoi")
  expect_equal(out$overview$n_defined_branches, 4)
  expect_equal(out$overview$n_requested_branches, 4)
  expect_equal(out$overview$n_completed_branches, 4)
  expect_equal(out$overview$n_failed_branches, 0)
  expect_equal(out$overview$multiverse_status, "completed")

  expect_equal(nrow(out$branch_results), 4)
  expect_true(all(out$branch_results$branch_status == "completed"))
  expect_true(all(out$branch_results$aoi_window_rows == 8))
  expect_true(all(out$branch_results$aoi_glmm_rows >= 1))
  expect_true(all(is.na(out$branch_results$message)))

  expect_equal(
    names(out$branch_outputs),
    c("toy_aoi_1", "toy_aoi_2", "toy_aoi_3", "toy_aoi_4")
  )

  expect_true(all(vapply(out$branch_outputs, is.list, logical(1))))
  expect_s3_class(out$branch_outputs[[1]], "gp3_aoi_multiverse_branch_output")
  expect_true("aoi_windows" %in% names(out$branch_outputs[[1]]))
  expect_true("aoi_glmm_data" %in% names(out$branch_outputs[[1]]))
})

test_that("run_gazepoint_aoi_multiverse maps aoi_only denominator internally", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
    toy_aoi,
    mv,
    branch_ids = "toy_aoi_3",
    windows = c(0, 150, 300),
    time_col = "time",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "target",
    distractor_aoi_values = "distractor",
    keep_outputs = TRUE
  )

  expect_equal(out$overview$n_requested_branches, 1)
  expect_equal(out$overview$n_completed_branches, 1)
  expect_equal(out$overview$n_failed_branches, 0)

  expect_equal(out$branch_results$denominator, "aoi_only")
  expect_equal(out$branch_results$branch_status, "completed")
  expect_equal(names(out$branch_outputs), "toy_aoi_3")
})

test_that("run_gazepoint_aoi_multiverse can run selected branch IDs", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
    toy_aoi,
    mv,
    branch_ids = "toy_aoi_2",
    windows = c(0, 150, 300),
    time_col = "time",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "target",
    distractor_aoi_values = "distractor",
    keep_outputs = TRUE
  )

  expect_equal(out$overview$n_requested_branches, 1)
  expect_equal(out$overview$n_completed_branches, 1)
  expect_equal(out$overview$n_failed_branches, 0)

  expect_equal(out$branch_results$branch_id, "toy_aoi_2")
  expect_equal(names(out$branch_outputs), "toy_aoi_2")
})

test_that("run_gazepoint_aoi_multiverse can omit branch outputs", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
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

  expect_equal(out$overview$n_completed_branches, 4)
  expect_equal(length(out$branch_outputs), 0)
})

test_that("run_gazepoint_aoi_multiverse standardises common Gazepoint group aliases", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
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
    keep_outputs = TRUE
  )

  expect_equal(out$overview$n_completed_branches, 4)

  first_windows <- out$branch_outputs[[1]]$aoi_windows

  expect_true("media_id" %in% names(first_windows))
})

test_that("run_gazepoint_aoi_multiverse records branch failures when stop_on_error is FALSE", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  out <- run_gazepoint_aoi_multiverse(
    toy_aoi,
    mv,
    windows = c(0, 150, 300),
    time_col = "bad_time_column",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "target",
    distractor_aoi_values = "distractor",
    keep_outputs = TRUE,
    stop_on_error = FALSE
  )

  expect_equal(out$overview$n_completed_branches, 0)
  expect_equal(out$overview$n_failed_branches, 4)
  expect_equal(out$overview$multiverse_status, "completed_with_errors")

  expect_true(all(out$branch_results$branch_status == "failed"))
  expect_true(all(!is.na(out$branch_results$message)))
  expect_equal(length(out$branch_outputs), 0)
})

test_that("run_gazepoint_aoi_multiverse stops on branch failure when requested", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      windows = c(0, 150, 300),
      time_col = "bad_time_column",
      aoi_col = "aoi_current",
      subject_col = "subject",
      condition_col = "condition",
      group_cols = c("subject", "MEDIA_ID", "trial_global"),
      target_aoi_values = "target",
      distractor_aoi_values = "distractor",
      stop_on_error = TRUE
    )
  )
})

test_that("run_gazepoint_aoi_multiverse checks invalid inputs", {
  toy_aoi <- make_test_aoi_multiverse_data()
  mv <- make_test_aoi_multiverse()

  expect_error(
    run_gazepoint_aoi_multiverse(
      data = NULL,
      multiverse = mv,
      windows = c(0, 150, 300),
      target_aoi_values = "target"
    ),
    "`data` must be supplied",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      multiverse = list(),
      windows = c(0, 150, 300),
      target_aoi_values = "target"
    ),
    "`multiverse` must be created by `create_gazepoint_preprocessing_multiverse()`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      target_aoi_values = "target"
    ),
    "`windows` must be supplied",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      windows = c(0, 150, 300)
    ),
    "`target_aoi_values` must be supplied",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      branch_ids = "unknown_branch",
      windows = c(0, 150, 300),
      target_aoi_values = "target"
    ),
    "`branch_ids` contains unknown AOI branch ID",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      branch_ids = NA_character_,
      windows = c(0, 150, 300),
      target_aoi_values = "target"
    ),
    "`branch_ids` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      windows = c(0, 150, 300),
      target_aoi_values = "target",
      keep_outputs = NA
    ),
    "`keep_outputs` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv,
      windows = c(0, 150, 300),
      target_aoi_values = "target",
      stop_on_error = NA
    ),
    "`stop_on_error` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("run_gazepoint_aoi_multiverse requires AOI branches", {
  toy_aoi <- make_test_aoi_multiverse_data()

  mv_pupil_only <- create_gazepoint_preprocessing_multiverse(
    include_pupil = TRUE,
    include_aoi = FALSE,
    pupil_max_gap_ms = 100,
    pupil_smoothing_window_samples = 3,
    pupil_baseline_windows = list(c(0, 100)),
    pupil_artifact_padding_ms = 0,
    label_prefix = "pupil_only"
  )

  expect_error(
    run_gazepoint_aoi_multiverse(
      toy_aoi,
      mv_pupil_only,
      windows = c(0, 150, 300),
      target_aoi_values = "target"
    ),
    "`multiverse` does not contain AOI branches",
    fixed = TRUE
  )
})

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}
