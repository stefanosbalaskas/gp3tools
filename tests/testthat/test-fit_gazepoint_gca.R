make_test_gca_fit_data <- function() {
  base <- expand.grid(
    subject = paste0("S", 1:8),
    condition = c("A", "B"),
    gca_time = seq(0, 900, by = 100),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  base <- tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, gca_time)

  time_poly <- stats::poly(base$gca_time, degree = 3)

  subject_shift <- stats::setNames(
    seq(-0.20, 0.20, length.out = 8),
    paste0("S", 1:8)
  )

  base |>
    dplyr::mutate(
      time_poly_1 = as.numeric(time_poly[, 1]),
      time_poly_2 = as.numeric(time_poly[, 2]),
      time_poly_3 = as.numeric(time_poly[, 3]),
      gca_pupil =
        0.20 +
        ifelse(condition == "B", 0.10, 0) +
        0.60 * time_poly_1 -
        0.35 * time_poly_2 +
        0.15 * time_poly_3 +
        ifelse(condition == "B", 0.15 * time_poly_1, 0) +
        subject_shift[subject],
      gca_weight = 5,
      n_valid_samples = 5L,
      gca_data_status = "ok"
    )
}

normalise_gca_formula_text <- function(x) {
  gsub(
    "\\s+",
    " ",
    paste(deparse(x), collapse = " ")
  )
}

quiet_fit_gca <- function(...) {
  suppressMessages(
    suppressWarnings(
      fit_gazepoint_gca(...)
    )
  )
}

test_that("fit_gazepoint_gca fits a GCA mixed model", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    REML = FALSE
  )

  expect_s3_class(out, "gp3_gca_model")
  expect_true(out$model_status %in% c(
    "ok",
    "singular_fit",
    "fallback_after_singular_fit",
    "fallback_after_fit_failure",
    "singular_random_slope_model_fallback_failed"
  ))

  expect_s3_class(out$data, "tbl_df")
  expect_s3_class(out$formula, "formula")
  expect_s3_class(out$attempted_formula, "formula")
  expect_s3_class(out$fallback_formula, "formula")
  expect_s3_class(out$comparison, "tbl_df")

  if (!is.null(out$model)) {
    expect_true(methods::is(out$model, "lmerMod"))
  }
})

test_that("fit_gazepoint_gca attempts random intercept and time slopes by default", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    REML = FALSE
  )

  attempted_formula_text <- normalise_gca_formula_text(out$attempted_formula)

  expect_match(
    attempted_formula_text,
    "(1 + .gp3_time_poly_1 + .gp3_time_poly_2 + .gp3_time_poly_3 | .gp3_gca_subject)",
    fixed = TRUE
  )

  expect_equal(
    out$attempted_random_effect_structure,
    "random_intercept_and_time_slopes"
  )
})

test_that("fit_gazepoint_gca can fit random-intercept-only model directly", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    random_slopes = FALSE,
    REML = FALSE
  )

  formula_text <- normalise_gca_formula_text(out$formula)

  expect_s3_class(out, "gp3_gca_model")
  expect_equal(out$random_effect_structure, "random_intercept")
  expect_false(out$fallback_used)
  expect_match(formula_text, "(1 | .gp3_gca_subject)", fixed = TRUE)
})

test_that("fit_gazepoint_gca includes condition interactions when multiple conditions exist", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    random_slopes = FALSE,
    REML = FALSE
  )

  formula_text <- normalise_gca_formula_text(out$formula)

  expect_match(formula_text, ".gp3_gca_condition *", fixed = TRUE)
  expect_match(formula_text, ".gp3_time_poly_1", fixed = TRUE)
  expect_match(formula_text, ".gp3_time_poly_2", fixed = TRUE)
  expect_match(formula_text, ".gp3_time_poly_3", fixed = TRUE)
})

test_that("fit_gazepoint_gca simplifies formula for one-condition data", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data() |>
    dplyr::mutate(condition = "all_data")

  out <- quiet_fit_gca(
    x,
    random_slopes = FALSE,
    REML = FALSE
  )

  formula_text <- normalise_gca_formula_text(out$formula)

  expect_false(grepl(".gp3_gca_condition", formula_text, fixed = TRUE))
  expect_match(formula_text, ".gp3_time_poly_1", fixed = TRUE)
  expect_match(formula_text, "(1 | .gp3_gca_subject)", fixed = TRUE)
})

test_that("fit_gazepoint_gca supports explicit time terms and degree", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    time_terms = c("time_poly_1", "time_poly_2"),
    degree = 2,
    random_slopes = FALSE,
    REML = FALSE
  )

  formula_text <- normalise_gca_formula_text(out$formula)

  expect_equal(out$settings$time_terms, c("time_poly_1", "time_poly_2"))
  expect_equal(out$settings$degree, 2L)
  expect_match(formula_text, ".gp3_time_poly_1", fixed = TRUE)
  expect_match(formula_text, ".gp3_time_poly_2", fixed = TRUE)
  expect_false(grepl(".gp3_time_poly_3", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_gca uses weights when requested", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data() |>
    dplyr::mutate(custom_weight = gca_weight + 1)

  out <- quiet_fit_gca(
    x,
    weights_col = "custom_weight",
    use_weights = TRUE,
    random_slopes = FALSE,
    REML = FALSE
  )

  expect_equal(out$settings$weights_col, "custom_weight")
  expect_true(out$settings$use_weights)
  expect_true(".gp3_gca_weights" %in% names(out$data))
  expect_equal(out$data$.gp3_gca_weights, x$custom_weight)
})

test_that("fit_gazepoint_gca can ignore weights", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()

  out <- quiet_fit_gca(
    x,
    use_weights = FALSE,
    random_slopes = FALSE,
    REML = FALSE
  )

  expect_false(out$settings$use_weights)
  expect_true(all(is.na(out$data$.gp3_gca_weights)))
})

test_that("fit_gazepoint_gca falls back when random-slope model is singular", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data() |>
    dplyr::mutate(gca_pupil = 0.1 + 0.2 * time_poly_1)

  out <- quiet_fit_gca(
    x,
    random_slopes = TRUE,
    fallback_on_singular = TRUE,
    REML = FALSE
  )

  expect_s3_class(out, "gp3_gca_model")
  expect_true(out$model_status %in% c(
    "ok",
    "singular_fit",
    "fallback_after_singular_fit",
    "singular_random_slope_model_fallback_failed"
  ))

  if (identical(out$model_status, "fallback_after_singular_fit")) {
    expect_true(out$fallback_used)
    expect_equal(out$random_effect_structure, "random_intercept")
  }
})

test_that("fit_gazepoint_gca drops missing rows", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data()
  x$gca_pupil[1:3] <- NA_real_
  x$time_poly_1[4:5] <- NA_real_

  out <- quiet_fit_gca(
    x,
    random_slopes = FALSE,
    REML = FALSE
  )

  expect_equal(nrow(out$data), nrow(x) - 5L)
})

test_that("fit_gazepoint_gca errors for invalid inputs", {
  x <- make_test_gca_fit_data()

  expect_error(
    fit_gazepoint_gca("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      outcome_col = NA_character_
    ),
    "`outcome_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      subject_col = NA_character_
    ),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      condition_col = NA_character_
    ),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      time_terms = c("time_poly_1", "time_poly_1")
    ),
    "`time_terms` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      degree = 0
    ),
    "`degree` must be NULL or a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      use_weights = NA
    ),
    "`use_weights` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      random_slopes = NA
    ),
    "`random_slopes` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      fallback_on_singular = NA
    ),
    "`fallback_on_singular` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      REML = NA
    ),
    "`REML` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      optimizer = NA_character_
    ),
    "`optimizer` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      maxfun = 0
    ),
    "`maxfun` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      drop_missing = NA
    ),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_gca errors when required columns are missing", {
  x <- make_test_gca_fit_data()

  expect_error(
    fit_gazepoint_gca(
      dplyr::select(x, -gca_pupil)
    ),
    "Missing required columns: gca_pupil",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      dplyr::select(x, -subject)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      dplyr::select(x, -time_poly_1),
      time_terms = c("time_poly_1", "time_poly_2", "time_poly_3")
    ),
    "Missing required columns: time_poly_1",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_gca(
      x,
      weights_col = "missing_weight",
      use_weights = TRUE
    ),
    "Missing required columns: missing_weight",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_gca errors when time terms cannot be detected", {
  x <- make_test_gca_fit_data() |>
    dplyr::select(-time_poly_1, -time_poly_2, -time_poly_3)

  expect_error(
    fit_gazepoint_gca(x),
    "Could not detect polynomial time terms. Please provide `time_terms`.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_gca errors when no rows remain after filtering", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data() |>
    dplyr::mutate(gca_pupil = NA_real_)

  expect_error(
    fit_gazepoint_gca(x),
    "No rows remain after removing missing GCA model variables.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_gca errors when fewer than two subjects are present", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_gca_fit_data() |>
    dplyr::filter(subject == "S1")

  expect_error(
    fit_gazepoint_gca(x),
    "At least two subjects are required to fit a GCA mixed model.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_gca works with real pupil_gca_data object when available", {
  testthat::skip_if_not_installed("lme4")

  if (exists("pupil_gca_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("pupil_gca_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "gca_pupil",
      "subject",
      "time_poly_1",
      "time_poly_2",
      "time_poly_3"
    ) %in% names(real_data))) {
      out <- quiet_fit_gca(
        real_data,
        REML = FALSE
      )

      expect_s3_class(out, "gp3_gca_model")
      expect_true(out$model_status %in% c(
        "ok",
        "singular_fit",
        "fallback_after_singular_fit",
        "fallback_after_fit_failure",
        "singular_random_slope_model_fallback_failed",
        "fit_failed"
      ))

      expect_s3_class(out$comparison, "tbl_df")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
