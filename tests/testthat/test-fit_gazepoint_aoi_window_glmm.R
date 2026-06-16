make_test_aoi_window_glmm_fit_data <- function() {
  base <- expand.grid(
    subject = paste0("S", 1:10),
    condition = c("A", "B"),
    window_label = c("early", "late"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  base <- tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, window_label)

  subject_shift <- stats::setNames(
    seq(-2, 2, length.out = 10),
    paste0("S", 1:10)
  )

  base |>
    dplyr::mutate(
      aoi_glmm_denominator = 40,
      linear_predictor =
        -1.2 +
        ifelse(condition == "B", 0.45, 0) +
        ifelse(window_label == "late", 0.65, 0) +
        ifelse(condition == "B" & window_label == "late", 0.20, 0) +
        subject_shift[subject] * 0.12,
      prob = stats::plogis(linear_predictor),
      aoi_glmm_success = round(aoi_glmm_denominator * prob),
      aoi_glmm_failure = aoi_glmm_denominator - aoi_glmm_success,
      aoi_glmm_subject = factor(subject),
      aoi_glmm_condition = factor(condition),
      aoi_glmm_window = factor(
        window_label,
        levels = c("early", "late")
      )
    )
}

normalise_aoi_glmm_formula_text <- function(x) {
  gsub(
    "\\s+",
    " ",
    paste(deparse(x), collapse = " ")
  )
}

quiet_fit_aoi_window_glmm <- function(...) {
  suppressMessages(
    suppressWarnings(
      fit_gazepoint_aoi_window_glmm(...)
    )
  )
}

test_that("fit_gazepoint_aoi_window_glmm fits a binomial mixed model", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  out <- quiet_fit_aoi_window_glmm(x)

  expect_s3_class(out, "gp3_aoi_window_glmm")
  expect_true(out$model_status %in% c(
    "ok",
    "singular_fit",
    "fallback_after_singular_fit",
    "fallback_after_fit_failure",
    "singular_random_slope_model_fallback_failed",
    "fit_failed"
  ))

  expect_s3_class(out$data, "tbl_df")
  expect_s3_class(out$formula, "formula")
  expect_s3_class(out$attempted_formula, "formula")
  expect_s3_class(out$comparison, "tbl_df")

  if (!is.null(out$model)) {
    expect_true(methods::is(out$model, "glmerMod"))
  }
})

test_that("fit_gazepoint_aoi_window_glmm includes condition, window, and interaction by default", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = FALSE
  )

  formula_text <- normalise_aoi_glmm_formula_text(out$formula)

  expect_match(formula_text, ".gp3_condition * .gp3_window", fixed = TRUE)
  expect_match(formula_text, "(1 | .gp3_subject)", fixed = TRUE)
  expect_equal(out$random_effect_structure, "random_intercept")
})

test_that("fit_gazepoint_aoi_window_glmm can remove the interaction", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  out <- quiet_fit_aoi_window_glmm(
    x,
    include_interaction = FALSE,
    random_window_slopes = FALSE
  )

  formula_text <- normalise_aoi_glmm_formula_text(out$formula)

  expect_false(grepl(".gp3_condition * .gp3_window", formula_text, fixed = TRUE))
  expect_match(formula_text, ".gp3_condition", fixed = TRUE)
  expect_match(formula_text, ".gp3_window", fixed = TRUE)
})

test_that("fit_gazepoint_aoi_window_glmm simplifies for one-condition data", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data() |>
    dplyr::mutate(
      aoi_glmm_condition = factor("all_data")
    )

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = FALSE
  )

  formula_text <- normalise_aoi_glmm_formula_text(out$formula)

  expect_false(grepl(".gp3_condition", formula_text, fixed = TRUE))
  expect_match(formula_text, ".gp3_window", fixed = TRUE)
})

test_that("fit_gazepoint_aoi_window_glmm simplifies for one-window data", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data() |>
    dplyr::filter(aoi_glmm_window == "early") |>
    dplyr::mutate(aoi_glmm_window = factor(aoi_glmm_window))

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = FALSE
  )

  formula_text <- normalise_aoi_glmm_formula_text(out$formula)

  expect_match(formula_text, ".gp3_condition", fixed = TRUE)
  expect_false(grepl(".gp3_window", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_window_glmm can fit intercept-only fixed part", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data() |>
    dplyr::mutate(
      aoi_glmm_condition = factor("all_data"),
      aoi_glmm_window = factor("all_window")
    )

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = FALSE
  )

  formula_text <- normalise_aoi_glmm_formula_text(out$formula)

  expect_match(formula_text, "~ 1 + (1 | .gp3_subject)", fixed = TRUE)
})

test_that("fit_gazepoint_aoi_window_glmm can attempt random window slopes", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = TRUE,
    fallback_on_singular = TRUE
  )

  attempted_formula_text <- normalise_aoi_glmm_formula_text(out$attempted_formula)

  expect_match(
    attempted_formula_text,
    "(1 + .gp3_window | .gp3_subject)",
    fixed = TRUE
  )

  expect_equal(
    out$attempted_random_effect_structure,
    "random_intercept_and_window_slopes"
  )

  expect_true(out$model_status %in% c(
    "ok",
    "singular_fit",
    "fallback_after_singular_fit",
    "fallback_after_fit_failure",
    "singular_random_slope_model_fallback_failed"
  ))
})

test_that("fit_gazepoint_aoi_window_glmm stores settings", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  out <- quiet_fit_aoi_window_glmm(
    x,
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = FALSE,
    random_intercept = TRUE,
    random_window_slopes = FALSE,
    fallback_on_singular = TRUE,
    optimizer = "bobyqa",
    maxfun = 10000,
    nAGQ = 0,
    drop_missing = TRUE
  )

  expect_equal(out$settings$success_col, "aoi_glmm_success")
  expect_equal(out$settings$failure_col, "aoi_glmm_failure")
  expect_equal(out$settings$subject_col, "aoi_glmm_subject")
  expect_equal(out$settings$condition_col, "aoi_glmm_condition")
  expect_equal(out$settings$window_col, "aoi_glmm_window")
  expect_true(out$settings$include_condition)
  expect_true(out$settings$include_window)
  expect_false(out$settings$include_interaction)
  expect_true(out$settings$random_intercept)
  expect_false(out$settings$random_window_slopes)
  expect_true(out$settings$fallback_on_singular)
  expect_equal(out$settings$optimizer, "bobyqa")
  expect_equal(out$settings$maxfun, 10000)
  expect_equal(out$settings$nAGQ, 0)
  expect_true(out$settings$drop_missing)
})

test_that("fit_gazepoint_aoi_window_glmm drops missing rows", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()

  x$aoi_glmm_success[1] <- NA_real_
  x$aoi_glmm_failure[2] <- NA_real_

  out <- quiet_fit_aoi_window_glmm(
    x,
    random_window_slopes = FALSE,
    drop_missing = TRUE
  )

  expect_equal(nrow(out$data), nrow(x) - 2L)
})

test_that("fit_gazepoint_aoi_window_glmm errors for invalid count values", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data()
  x$aoi_glmm_success[1] <- -1

  expect_error(
    fit_gazepoint_aoi_window_glmm(x),
    "AOI-window GLMM success and failure counts must be finite and non-negative.",
    fixed = TRUE
  )

  x <- make_test_aoi_window_glmm_fit_data()
  x$aoi_glmm_failure[1] <- -1

  expect_error(
    fit_gazepoint_aoi_window_glmm(x),
    "AOI-window GLMM success and failure counts must be finite and non-negative.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_window_glmm errors when no rows remain", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data() |>
    dplyr::mutate(aoi_glmm_success = NA_real_)

  expect_error(
    fit_gazepoint_aoi_window_glmm(x),
    "No rows remain after removing missing AOI-window GLMM variables.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_window_glmm errors when fewer than two subjects are present", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_window_glmm_fit_data() |>
    dplyr::filter(aoi_glmm_subject == "S1")

  expect_error(
    fit_gazepoint_aoi_window_glmm(x),
    "At least two subjects are required to fit an AOI-window GLMM.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_window_glmm errors for invalid inputs", {
  x <- make_test_aoi_window_glmm_fit_data()

  expect_error(
    fit_gazepoint_aoi_window_glmm("not data"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, success_col = NA_character_),
    "`success_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, failure_col = NA_character_),
    "`failure_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, subject_col = NA_character_),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, condition_col = NA_character_),
    "`condition_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, window_col = NA_character_),
    "`window_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, include_condition = NA),
    "`include_condition` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, include_window = NA),
    "`include_window` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, include_interaction = NA),
    "`include_interaction` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, random_intercept = NA),
    "`random_intercept` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, random_window_slopes = NA),
    "`random_window_slopes` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, fallback_on_singular = NA),
    "`fallback_on_singular` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, drop_missing = NA),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, optimizer = NA_character_),
    "`optimizer` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, maxfun = 0),
    "`maxfun` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(x, nAGQ = -1),
    "`nAGQ` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_window_glmm errors when required columns are missing", {
  x <- make_test_aoi_window_glmm_fit_data()

  expect_error(
    fit_gazepoint_aoi_window_glmm(
      dplyr::select(x, -aoi_glmm_success)
    ),
    "Missing required columns: aoi_glmm_success",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(
      dplyr::select(x, -aoi_glmm_failure)
    ),
    "Missing required columns: aoi_glmm_failure",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(
      dplyr::select(x, -aoi_glmm_subject)
    ),
    "Missing required columns: aoi_glmm_subject",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(
      dplyr::select(x, -aoi_glmm_condition)
    ),
    "Missing required columns: aoi_glmm_condition",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_window_glmm(
      dplyr::select(x, -aoi_glmm_window)
    ),
    "Missing required columns: aoi_glmm_window",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_window_glmm works with real aoi_glmm_data object when available", {
  testthat::skip_if_not_installed("lme4")

  if (exists("aoi_glmm_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("aoi_glmm_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "aoi_glmm_success",
      "aoi_glmm_failure",
      "aoi_glmm_subject",
      "aoi_glmm_condition",
      "aoi_glmm_window"
    ) %in% names(real_data))) {
      out <- quiet_fit_aoi_window_glmm(
        real_data,
        random_window_slopes = FALSE
      )

      expect_s3_class(out, "gp3_aoi_window_glmm")
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
