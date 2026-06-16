make_test_pupil_pfe_gamm_data <- function() {
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
      mean_x =
        0.25 +
        0.00035 * time_bin_center_ms +
        ifelse(condition == "B", 0.05, 0) +
        as.numeric(factor(subject)) * 0.015,
      mean_y =
        0.70 -
        0.00025 * time_bin_center_ms +
        ifelse(condition == "B", -0.03, 0) +
        as.numeric(factor(subject)) * 0.010,
      mean_pupil =
        sin(time_bin_center_ms / 300) +
        ifelse(condition == "B", 0.20, 0) +
        subject_shift[subject] +
        0.40 * mean_x -
        0.30 * mean_y,
      n_valid_samples = 5L
    )
}

normalise_formula_text <- function(x) {
  gsub(
    "\\s+",
    " ",
    paste(deparse(x), collapse = " ")
  )
}

test_that("fit_gazepoint_pupil_pfe_gamm fits main and PFE models", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  expect_s3_class(out, "gp3_pupil_pfe_gamm")
  expect_equal(out$sensitivity_status, "ok")
  expect_s3_class(out$main_model, "bam")
  expect_s3_class(out$main_model, "gam")
  expect_s3_class(out$pfe_model, "bam")
  expect_s3_class(out$pfe_model, "gam")
  expect_s3_class(out$main_fit, "gp3_pupil_gamm")
  expect_s3_class(out$pfe_formula, "formula")
  expect_s3_class(out$data, "tbl_df")
})

test_that("fit_gazepoint_pupil_pfe_gamm adds tensor gaze-position smooth", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  formula_text <- normalise_formula_text(out$pfe_formula)

  expect_match(formula_text, "te(.gp3_x, .gp3_y", fixed = TRUE)
  expect_match(formula_text, "k = c(4, 4)", fixed = TRUE)
})

test_that("fit_gazepoint_pupil_pfe_gamm returns comparison table", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  expect_s3_class(out$comparison, "tbl_df")
  expect_equal(out$comparison$model_type, c("main_gamm", "pfe_gamm"))
  expect_true(all(c(
    "AIC",
    "BIC",
    "deviance_explained",
    "adj_r_squared",
    "delta_AIC_from_main",
    "delta_BIC_from_main",
    "delta_deviance_explained_from_main"
  ) %in% names(out$comparison)))

  expect_equal(
    out$comparison$delta_AIC_from_main[
      out$comparison$model_type == "main_gamm"
    ],
    0
  )
})

test_that("fit_gazepoint_pupil_pfe_gamm simplifies condition terms for one-condition data", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data() |>
    dplyr::mutate(condition = "all_data")

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  formula_text <- normalise_formula_text(out$pfe_formula)

  expect_equal(out$sensitivity_status, "ok")
  expect_false(grepl(".gp3_condition \\+", formula_text))
  expect_false(grepl("by = .gp3_condition", formula_text, fixed = TRUE))
  expect_match(formula_text, "te(.gp3_x, .gp3_y", fixed = TRUE)
})

test_that("fit_gazepoint_pupil_pfe_gamm handles insufficient gaze-position variation", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data() |>
    dplyr::mutate(
      mean_x = 0.5,
      mean_y = 0.5
    )

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  expect_s3_class(out, "gp3_pupil_pfe_gamm")
  expect_equal(out$sensitivity_status, "insufficient_gaze_position_variation")
  expect_s3_class(out$main_fit, "gp3_pupil_gamm")
  expect_null(out$pfe_model)
  expect_null(out$pfe_formula)
  expect_equal(nrow(out$comparison), 0L)
})

test_that("fit_gazepoint_pupil_pfe_gamm supports AR start and rho", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    rho = 0.2,
    ar_start_col = "AR.start",
    discrete = FALSE
  )

  expect_equal(out$sensitivity_status, "ok")
  expect_true(out$settings$ar_used)
  expect_equal(out$settings$rho, 0.2)
  expect_true(".gp3_ar_start" %in% names(out$data))
})

test_that("fit_gazepoint_pupil_pfe_gamm supports weights", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data() |>
    dplyr::mutate(weight = n_valid_samples)

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    weights_col = "weight",
    discrete = FALSE
  )

  expect_equal(out$sensitivity_status, "ok")
  expect_equal(out$settings$weights_col, "weight")
  expect_true(".gp3_weights" %in% names(out$data))
})

test_that("fit_gazepoint_pupil_pfe_gamm supports scaled-t family", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    family = "scat",
    discrete = FALSE
  )

  expect_true(out$sensitivity_status %in% c("ok", "pfe_model_failed"))
  expect_equal(out$settings$family, "scat")
})

test_that("fit_gazepoint_pupil_pfe_gamm drops missing gaze-position rows", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data()
  x$mean_x[1:3] <- NA_real_
  x$mean_y[4:5] <- NA_real_

  out <- fit_gazepoint_pupil_pfe_gamm(
    x,
    n_time_basis = 5,
    n_position_basis = 4,
    discrete = FALSE
  )

  expect_equal(out$sensitivity_status, "ok")
  expect_equal(nrow(out$data), nrow(x) - 5L)
})

test_that("fit_gazepoint_pupil_pfe_gamm errors for invalid inputs", {
  x <- make_test_pupil_pfe_gamm_data()

  expect_error(
    fit_gazepoint_pupil_pfe_gamm("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      pupil_col = NA_character_
    ),
    "`pupil_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      subject_col = NA_character_
    ),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      condition_col = NA_character_
    ),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      x_col = NA_character_
    ),
    "`x_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      y_col = NA_character_
    ),
    "`y_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      n_time_basis = 2
    ),
    "`n_time_basis` must be a finite numeric scalar greater than or equal to 3",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      n_position_basis = 2
    ),
    "`n_position_basis` must be a finite numeric scalar greater than or equal to 3",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      use_condition_smooths = NA
    ),
    "`use_condition_smooths` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      include_subject_random_effect = NA
    ),
    "`include_subject_random_effect` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      discrete = NA
    ),
    "`discrete` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      rho = 1
    ),
    "`rho` must be NULL or a finite numeric scalar in [0, 1)",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      drop_missing = NA
    ),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_pfe_gamm errors when required columns are missing", {
  x <- make_test_pupil_pfe_gamm_data()

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      dplyr::select(x, -mean_pupil)
    ),
    "Missing required columns: mean_pupil",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      dplyr::select(x, -time_bin_center_ms)
    ),
    "Missing required columns: time_bin_center_ms",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      dplyr::select(x, -subject)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      dplyr::select(x, -mean_x)
    ),
    "Missing required columns: mean_x",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      dplyr::select(x, -mean_y)
    ),
    "Missing required columns: mean_y",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      weights_col = "missing_weight"
    ),
    "Missing required columns: missing_weight",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_pfe_gamm errors when too few time values are available", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data() |>
    dplyr::filter(time_bin_center_ms %in% c(0, 100))

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      n_time_basis = 3,
      n_position_basis = 4,
      discrete = FALSE
    ),
    "At least three unique time values are required to fit a pupil GAMM",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_pfe_gamm errors when all rows are missing model variables", {
  testthat::skip_if_not_installed("mgcv")

  x <- make_test_pupil_pfe_gamm_data() |>
    dplyr::mutate(mean_x = NA_real_)

  expect_error(
    fit_gazepoint_pupil_pfe_gamm(
      x,
      n_time_basis = 5,
      n_position_basis = 4,
      discrete = FALSE
    ),
    "No rows remain after removing missing model or gaze-position variables",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_pfe_gamm works with real pupil_gamm_data object when available", {
  testthat::skip_if_not_installed("mgcv")

  if (exists("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "mean_pupil",
      "time_bin_center_ms",
      "subject",
      "mean_x",
      "mean_y"
    ) %in% names(real_data))) {
      out <- fit_gazepoint_pupil_pfe_gamm(
        real_data,
        n_time_basis = 10,
        n_position_basis = 8,
        discrete = TRUE
      )

      expect_s3_class(out, "gp3_pupil_pfe_gamm")
      expect_true(out$sensitivity_status %in% c(
        "ok",
        "main_model_failed",
        "pfe_model_failed",
        "insufficient_gaze_position_variation"
      ))

      if (identical(out$sensitivity_status, "ok")) {
        expect_s3_class(out$main_model, "bam")
        expect_s3_class(out$pfe_model, "bam")
        expect_s3_class(out$comparison, "tbl_df")
      }
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
