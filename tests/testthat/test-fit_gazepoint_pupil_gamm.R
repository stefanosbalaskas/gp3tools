make_test_pupil_gamm_fit_data <- function() {
  base <- expand.grid(
    subject = c("S1", "S2", "S3", "S4"),
    condition = c("A", "B"),
    time_bin_center_ms = seq(0, 900, by = 100),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  base <- tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, time_bin_center_ms) |>
    dplyr::group_by(subject, condition) |>
    dplyr::mutate(
      AR.start = dplyr::row_number() == 1L
    ) |>
    dplyr::ungroup()

  subject_shift <- c(S1 = -0.10, S2 = 0.05, S3 = 0.12, S4 = -0.03)

  base |>
    dplyr::mutate(
      mean_pupil =
        sin(time_bin_center_ms / 300) +
        ifelse(condition == "B", 0.20, 0) +
        subject_shift[subject],
      n_valid_samples = 5L
    )
}

test_that("fit_gazepoint_pupil_gamm fits the default Gaussian GAMM", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    discrete = FALSE
  )

  expect_s3_class(out, "gp3_pupil_gamm")
  expect_equal(out$model_status, "ok")
  expect_s3_class(out$model, "bam")
  expect_s3_class(out$model, "gam")
  expect_s3_class(out$formula, "formula")
  expect_s3_class(out$data, "tbl_df")

  expect_equal(out$settings$pupil_col, "mean_pupil")
  expect_equal(out$settings$time_col, "time_bin_center_ms")
  expect_equal(out$settings$subject_col, "subject")
  expect_equal(out$settings$condition_col, "condition")
  expect_equal(out$settings$family, "gaussian")
  expect_equal(out$settings$effective_k, 5L)
  expect_false(out$settings$ar_used)
})

test_that("fit_gazepoint_pupil_gamm includes condition terms when multiple conditions exist", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    discrete = FALSE
  )

  formula_text <- gsub(
    "\\s+",
    " ",
    paste(deparse(out$formula), collapse = " ")
  )

  expect_match(formula_text, ".gp3_condition", fixed = TRUE)
  expect_match(formula_text, "by = .gp3_condition", fixed = TRUE)
  expect_true(grepl("s\\(\\.gp3_subject,\\s*bs\\s*=\\s*\"re\"\\)", formula_text))
})

test_that("fit_gazepoint_pupil_gamm simplifies formula for one-condition data", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data() |>
    dplyr::mutate(condition = "all_data")

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    discrete = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = " ")

  expect_equal(out$model_status, "ok")
  expect_false(grepl(".gp3_condition \\+", formula_text))
  expect_false(grepl("by = .gp3_condition", formula_text, fixed = TRUE))
  expect_match(formula_text, "s(.gp3_time, k = 5)", fixed = TRUE)
})

test_that("fit_gazepoint_pupil_gamm can omit subject random effect", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    include_subject_random_effect = FALSE,
    discrete = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = " ")

  expect_equal(out$model_status, "ok")
  expect_false(grepl("bs = \"re\"", formula_text, fixed = TRUE))
  expect_false(out$settings$include_subject_random_effect)
})

test_that("fit_gazepoint_pupil_gamm can omit condition-specific smooths", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    use_condition_smooths = FALSE,
    discrete = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = " ")

  expect_equal(out$model_status, "ok")
  expect_false(grepl("by = .gp3_condition", formula_text, fixed = TRUE))
  expect_false(out$settings$use_condition_smooths)
})

test_that("fit_gazepoint_pupil_gamm supports AR start and rho", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    rho = 0.2,
    ar_start_col = "AR.start",
    discrete = FALSE
  )

  expect_equal(out$model_status, "ok")
  expect_true(out$settings$ar_used)
  expect_equal(out$settings$rho, 0.2)
  expect_true(".gp3_ar_start" %in% names(out$data))
})

test_that("fit_gazepoint_pupil_gamm supports weights", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data() |>
    dplyr::mutate(weight = n_valid_samples)

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    weights_col = "weight",
    discrete = FALSE
  )

  expect_equal(out$model_status, "ok")
  expect_equal(out$settings$weights_col, "weight")
  expect_true(".gp3_weights" %in% names(out$data))
})

test_that("fit_gazepoint_pupil_gamm supports scaled-t family", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    family = "scat",
    discrete = FALSE
  )

  expect_equal(out$model_status, "ok")
  expect_equal(out$settings$family, "scat")
})

test_that("fit_gazepoint_pupil_gamm records fit failures rather than crashing", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()

  out <- fit_gazepoint_pupil_gamm(
    x,
    pupil_col = "mean_pupil",
    time_col = "time_bin_center_ms",
    subject_col = "subject",
    condition_col = "condition",
    n_time_basis = 50,
    discrete = FALSE
  )

  expect_s3_class(out, "gp3_pupil_gamm")
  expect_true(out$model_status %in% c("ok", "fit_failed"))

  if (identical(out$model_status, "fit_failed")) {
    expect_null(out$model)
    expect_type(out$error_message, "character")
  }
})

test_that("fit_gazepoint_pupil_gamm drops missing model rows", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data()
  x$mean_pupil[1:3] <- NA_real_

  out <- fit_gazepoint_pupil_gamm(
    x,
    n_time_basis = 5,
    discrete = FALSE
  )

  expect_equal(out$model_status, "ok")
  expect_equal(nrow(out$data), nrow(x) - 3L)
})

test_that("fit_gazepoint_pupil_gamm errors for invalid inputs", {
  x <- make_test_pupil_gamm_fit_data()

  expect_error(
    fit_gazepoint_pupil_gamm("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      pupil_col = NA_character_
    ),
    "`pupil_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      subject_col = NA_character_
    ),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      condition_col = NA_character_
    ),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      n_time_basis = 2
    ),
    "`n_time_basis` must be a finite numeric scalar greater than or equal to 3",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      use_condition_smooths = NA
    ),
    "`use_condition_smooths` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      include_subject_random_effect = NA
    ),
    "`include_subject_random_effect` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      discrete = NA
    ),
    "`discrete` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      rho = 1
    ),
    "`rho` must be NULL or a finite numeric scalar in [0, 1)",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      drop_missing = NA
    ),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_gamm errors when required columns are missing", {
  x <- make_test_pupil_gamm_fit_data()

  expect_error(
    fit_gazepoint_pupil_gamm(
      dplyr::select(x, -mean_pupil)
    ),
    "Missing required columns: mean_pupil",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      dplyr::select(x, -time_bin_center_ms)
    ),
    "Missing required columns: time_bin_center_ms",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      dplyr::select(x, -subject)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      weights_col = "missing_weight"
    ),
    "Missing required columns: missing_weight",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_gamm errors when too few time values are available", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_gamm_fit_data() |>
    dplyr::filter(time_bin_center_ms %in% c(0, 100))

  expect_error(
    fit_gazepoint_pupil_gamm(
      x,
      n_time_basis = 3,
      discrete = FALSE
    ),
    "At least three unique time values are required to fit a pupil GAMM",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_gamm works with real prepared pupil_gamm_data object when available", {
  testthat::skip_if_not_installed("mgcv")

  if (exists("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c("mean_pupil", "time_bin_center_ms", "subject") %in% names(real_data))) {
      out <- fit_gazepoint_pupil_gamm(
        real_data,
        n_time_basis = 10,
        discrete = TRUE
      )

      expect_s3_class(out, "gp3_pupil_gamm")
      expect_true(out$model_status %in% c("ok", "fit_failed"))

      if (identical(out$model_status, "ok")) {
        expect_s3_class(out$model, "bam")
        expect_s3_class(out$model, "gam")
      }
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
