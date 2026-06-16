make_test_aoi_gamm_fit_data <- function(two_conditions = TRUE) {
  subjects <- paste0("S", 1:8)
  conditions <- if (two_conditions) c("control", "treatment") else "control"
  trials <- paste0("T", 1:3)
  times <- seq(0, 950, by = 50)

  dat <- expand.grid(
    subject = subjects,
    condition = conditions,
    trial_global = trials,
    time = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat <- tibble::as_tibble(dat)

  subject_num <- as.numeric(factor(dat$subject))
  time_scaled <- dat$time / max(dat$time)

  base_prob <- stats::plogis(-2 + 2 * sin(time_scaled * pi))
  treatment_boost <- ifelse(
    dat$condition == "treatment" & dat$time >= 300 & dat$time <= 650,
    0.25,
    0
  )
  subject_shift <- (subject_num - mean(subject_num)) * 0.015

  prob <- pmin(pmax(base_prob + treatment_boost + subject_shift, 0.02), 0.98)

  dat$target_aoi <- prob > 0.35

  prepare_gazepoint_aoi_gamm_data(
    dat,
    outcome_col = "target_aoi",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 950),
    bin_size_ms = 50,
    min_denominator_samples = 1,
    outcome_label = "target_aoi"
  )
}

test_that("fit_gazepoint_aoi_gamm fits a single-condition AOI GAMM", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = FALSE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    include_condition = TRUE,
    condition_smooths = TRUE,
    random_subject = TRUE,
    random_subject_time = FALSE,
    time_k = 6
  )

  expect_s3_class(fit, "gp3_aoi_gamm_fit")
  expect_equal(fit$model_status, "ok")
  expect_equal(fit$condition_status, "less_than_two_conditions")
  expect_s3_class(fit$model, "bam")
  expect_true(inherits(fit$model, "gam"))

  expect_true(grepl("cbind", fit$formula_text, fixed = TRUE))
  expect_true(grepl("s(.gp3_aoi_gamm_time_bin", fit$formula_text, fixed = TRUE))
  expect_true(grepl("s(.gp3_aoi_gamm_subject", fit$formula_text, fixed = TRUE))
  expect_false(grepl(".gp3_aoi_gamm_condition +", fit$formula_text, fixed = TRUE))

  expect_s3_class(fit$diagnostics, "tbl_df")
  expect_equal(fit$diagnostics$model_status, "ok")
  expect_false(fit$diagnostics$used_condition)
  expect_false(fit$diagnostics$used_condition_smooths)
  expect_true(fit$diagnostics$used_random_subject)

  expect_s3_class(fit$parametric_table, "tbl_df")
  expect_s3_class(fit$smooth_table, "tbl_df")
  expect_true(nrow(fit$smooth_table) >= 1)
})

test_that("fit_gazepoint_aoi_gamm fits a two-condition AOI GAMM", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    include_condition = TRUE,
    condition_smooths = TRUE,
    random_subject = TRUE,
    random_subject_time = FALSE,
    time_k = 6
  )

  expect_s3_class(fit, "gp3_aoi_gamm_fit")
  expect_equal(fit$model_status, "ok")
  expect_equal(fit$condition_status, "two_conditions")
  expect_s3_class(fit$model, "bam")

  expect_true(grepl(".gp3_aoi_gamm_condition", fit$formula_text, fixed = TRUE))
  expect_true(grepl("by = .gp3_aoi_gamm_condition", fit$formula_text, fixed = TRUE))

  expect_true(fit$diagnostics$used_condition)
  expect_true(fit$diagnostics$used_condition_smooths)
  expect_true(fit$diagnostics$used_random_subject)
})

test_that("fit_gazepoint_aoi_gamm can disable condition terms", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    include_condition = FALSE,
    condition_smooths = FALSE,
    random_subject = TRUE,
    time_k = 6
  )

  expect_equal(fit$model_status, "ok")
  expect_false(fit$diagnostics$used_condition)
  expect_false(fit$diagnostics$used_condition_smooths)
  expect_false(grepl("by = .gp3_aoi_gamm_condition", fit$formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_gamm can disable random subject effects", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    include_condition = TRUE,
    condition_smooths = FALSE,
    random_subject = FALSE,
    random_subject_time = FALSE,
    time_k = 6
  )

  expect_equal(fit$model_status, "ok")
  expect_false(fit$diagnostics$used_random_subject)
  expect_false(grepl("bs = 're'", fit$formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_gamm supports subject-specific time smooths", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    include_condition = FALSE,
    condition_smooths = FALSE,
    random_subject = TRUE,
    random_subject_time = TRUE,
    time_k = 6,
    subject_time_k = 4
  )

  expect_equal(fit$model_status, "ok")
  expect_true(fit$diagnostics$used_random_subject_time)
  expect_true(grepl("bs = 'fs'", fit$formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_gamm respects basis-size limits", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = FALSE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    random_subject = TRUE,
    time_k = 100,
    subject_time_k = 100
  )

  expect_equal(fit$model_status, "ok")
  expect_lte(fit$diagnostics$effective_time_k, fit$diagnostics$n_time_bins)
  expect_lte(
    fit$diagnostics$effective_subject_time_k,
    fit$diagnostics$n_time_bins
  )
})

test_that("fit_gazepoint_aoi_gamm checks required columns", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)
  dat$.gp3_aoi_gamm_success <- NULL

  expect_error(
    fit_gazepoint_aoi_gamm(dat),
    "`data` is missing required column(s): .gp3_aoi_gamm_success",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_gamm rejects too few rows", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = FALSE)
  dat <- dat[1:5, , drop = FALSE]

  expect_error(
    fit_gazepoint_aoi_gamm(dat, min_rows = 10),
    "Not enough valid rows are available for AOI-GAMM fitting",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_gamm rejects too few subjects", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = FALSE)
  dat <- dat[dat$.gp3_aoi_gamm_subject == "S1", , drop = FALSE]

  expect_error(
    fit_gazepoint_aoi_gamm(dat, min_subjects = 2),
    "Not enough subjects are available for AOI-GAMM fitting",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_gamm rejects too few time bins", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = FALSE)
  dat <- dat[dat$.gp3_aoi_gamm_time_bin %in% c(0, 50), , drop = FALSE]

  expect_error(
    fit_gazepoint_aoi_gamm(dat, min_time_bins = 4),
    "Not enough time bins are available for AOI-GAMM fitting",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_gamm checks scalar arguments", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  expect_error(
    fit_gazepoint_aoi_gamm(dat, include_condition = NA),
    "`include_condition` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_gamm(dat, condition_smooths = NA),
    "`condition_smooths` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_gamm(dat, random_subject = NA),
    "`random_subject` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_gamm(dat, random_subject_time = NA),
    "`random_subject_time` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_gamm(dat, time_k = 0),
    "`time_k` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_gamm(dat, subject_time_k = 0),
    "`subject_time_k` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_gamm returns status error when mgcv fitting fails", {
  dat <- make_test_aoi_gamm_fit_data(two_conditions = TRUE)

  fit <- fit_gazepoint_aoi_gamm(
    dat,
    family = stats::gaussian(),
    time_k = 6
  )

  expect_s3_class(fit, "gp3_aoi_gamm_fit")
  expect_equal(fit$model_status, "error")
  expect_null(fit$model)
  expect_true(is.character(fit$error_message))
})
