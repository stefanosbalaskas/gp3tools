make_test_aoi_sensitivity_data <- function() {
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
      aoi_glmm_prop = aoi_glmm_success / aoi_glmm_denominator,
      aoi_glmm_subject = factor(subject),
      aoi_glmm_condition = factor(condition),
      aoi_glmm_window = factor(
        window_label,
        levels = c("early", "late")
      )
    )
}

normalise_aoi_sensitivity_formula_text <- function(x) {
  gsub(
    "\\s+",
    " ",
    paste(deparse(x), collapse = " ")
  )
}

quiet_fit_aoi_sensitivity <- function(...) {
  suppressMessages(
    suppressWarnings(
      fit_gazepoint_aoi_model_sensitivity(...)
    )
  )
}

test_that("fit_gazepoint_aoi_model_sensitivity fits all supported model types", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(x)

  expect_s3_class(out, "gp3_aoi_model_sensitivity")
  expect_type(out, "list")

  expect_true(all(c(
    "models",
    "formulas",
    "data",
    "comparison",
    "fixed_effects",
    "settings",
    "model_status",
    "error_message"
  ) %in% names(out)))

  expect_equal(
    names(out$models),
    c(
      "binomial_glmm",
      "empirical_logit_lmm",
      "proportion_lmm",
      "quasibinomial_glm"
    )
  )

  expect_equal(
    out$comparison$model_type,
    c(
      "binomial_glmm",
      "empirical_logit_lmm",
      "proportion_lmm",
      "quasibinomial_glm"
    )
  )

  expect_true(all(out$comparison$model_status %in% c(
    "ok",
    "singular_fit",
    "fit_failed"
  )))

  expect_s3_class(out$comparison, "tbl_df")
  expect_s3_class(out$fixed_effects, "tbl_df")
  expect_s3_class(out$data, "tbl_df")
})

test_that("fit_gazepoint_aoi_model_sensitivity can fit only quasibinomial GLM without mixed models", {
  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "quasibinomial_glm"
  )

  expect_equal(names(out$models), "quasibinomial_glm")
  expect_equal(out$comparison$model_type, "quasibinomial_glm")
  expect_true(out$comparison$model_status %in% c("ok", "fit_failed"))

  if (!is.null(out$models$quasibinomial_glm)) {
    expect_s3_class(out$models$quasibinomial_glm, "glm")
  }
})

test_that("fit_gazepoint_aoi_model_sensitivity formulas include condition-window interaction by default", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = c("binomial_glmm", "quasibinomial_glm"),
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = TRUE,
    random_intercept = TRUE
  )

  binomial_formula <- normalise_aoi_sensitivity_formula_text(
    out$formulas$binomial_glmm
  )

  quasi_formula <- normalise_aoi_sensitivity_formula_text(
    out$formulas$quasibinomial_glm
  )

  expect_match(binomial_formula, ".gp3_condition * .gp3_window", fixed = TRUE)
  expect_match(binomial_formula, "(1 | .gp3_subject)", fixed = TRUE)
  expect_match(quasi_formula, ".gp3_condition * .gp3_window", fixed = TRUE)
  expect_false(grepl(".gp3_subject", quasi_formula, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_model_sensitivity can remove the interaction", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "binomial_glmm",
    include_interaction = FALSE
  )

  formula_text <- normalise_aoi_sensitivity_formula_text(
    out$formulas$binomial_glmm
  )

  expect_false(grepl(".gp3_condition * .gp3_window", formula_text, fixed = TRUE))
  expect_match(formula_text, ".gp3_condition", fixed = TRUE)
  expect_match(formula_text, ".gp3_window", fixed = TRUE)
})

test_that("fit_gazepoint_aoi_model_sensitivity simplifies for one-condition data", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data() |>
    dplyr::mutate(aoi_glmm_condition = factor("all_data"))

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "binomial_glmm"
  )

  formula_text <- normalise_aoi_sensitivity_formula_text(
    out$formulas$binomial_glmm
  )

  expect_false(grepl(".gp3_condition", formula_text, fixed = TRUE))
  expect_match(formula_text, ".gp3_window", fixed = TRUE)
})

test_that("fit_gazepoint_aoi_model_sensitivity simplifies for one-window data", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data() |>
    dplyr::filter(aoi_glmm_window == "early") |>
    dplyr::mutate(aoi_glmm_window = factor(aoi_glmm_window))

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "binomial_glmm"
  )

  formula_text <- normalise_aoi_sensitivity_formula_text(
    out$formulas$binomial_glmm
  )

  expect_match(formula_text, ".gp3_condition", fixed = TRUE)
  expect_false(grepl(".gp3_window", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_aoi_model_sensitivity can fit fixed-effects-only sensitivity models", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = c("empirical_logit_lmm", "proportion_lmm"),
    random_intercept = FALSE
  )

  empirical_formula <- normalise_aoi_sensitivity_formula_text(
    out$formulas$empirical_logit_lmm
  )

  proportion_formula <- normalise_aoi_sensitivity_formula_text(
    out$formulas$proportion_lmm
  )

  expect_false(grepl(".gp3_subject", empirical_formula, fixed = TRUE))
  expect_false(grepl(".gp3_subject", proportion_formula, fixed = TRUE))
  expect_true(all(out$comparison$model_status %in% c("ok", "singular_fit", "fit_failed")))
})

test_that("fit_gazepoint_aoi_model_sensitivity returns fixed effects", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = c("binomial_glmm", "quasibinomial_glm")
  )

  expect_true(all(c(
    "model_type",
    "term",
    "estimate",
    "std_error",
    "statistic",
    "p_value"
  ) %in% names(out$fixed_effects)))

  expect_true("binomial_glmm" %in% out$fixed_effects$model_type)
  expect_true("quasibinomial_glm" %in% out$fixed_effects$model_type)
  expect_true("(Intercept)" %in% out$fixed_effects$term)
})

test_that("fit_gazepoint_aoi_model_sensitivity stores settings", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = c("binomial_glmm", "quasibinomial_glm"),
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = FALSE,
    random_intercept = TRUE,
    optimizer = "bobyqa",
    maxfun = 10000,
    nAGQ = 0,
    empirical_logit_correction = 0.25,
    drop_missing = TRUE
  )

  expect_equal(out$settings$success_col, "aoi_glmm_success")
  expect_equal(out$settings$failure_col, "aoi_glmm_failure")
  expect_equal(out$settings$denominator_col, "aoi_glmm_denominator")
  expect_equal(out$settings$proportion_col, "aoi_glmm_prop")
  expect_equal(out$settings$subject_col, "aoi_glmm_subject")
  expect_equal(out$settings$condition_col, "aoi_glmm_condition")
  expect_equal(out$settings$window_col, "aoi_glmm_window")
  expect_equal(out$settings$model_types, c("binomial_glmm", "quasibinomial_glm"))
  expect_true(out$settings$include_condition)
  expect_true(out$settings$include_window)
  expect_false(out$settings$include_interaction)
  expect_true(out$settings$random_intercept)
  expect_equal(out$settings$optimizer, "bobyqa")
  expect_equal(out$settings$maxfun, 10000)
  expect_equal(out$settings$nAGQ, 0)
  expect_equal(out$settings$empirical_logit_correction, 0.25)
  expect_true(out$settings$drop_missing)
})

test_that("fit_gazepoint_aoi_model_sensitivity drops missing rows", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  x$aoi_glmm_success[1] <- NA_real_
  x$aoi_glmm_prop[2] <- NA_real_

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "binomial_glmm",
    drop_missing = TRUE
  )

  expect_equal(nrow(out$data), nrow(x) - 2L)
})

test_that("fit_gazepoint_aoi_model_sensitivity errors for invalid count values", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()
  x$aoi_glmm_success[1] <- -1

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x),
    "AOI sensitivity-model counts must be finite and non-negative, with positive denominators.",
    fixed = TRUE
  )

  x <- make_test_aoi_sensitivity_data()
  x$aoi_glmm_failure[1] <- -1

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x),
    "AOI sensitivity-model counts must be finite and non-negative, with positive denominators.",
    fixed = TRUE
  )

  x <- make_test_aoi_sensitivity_data()
  x$aoi_glmm_denominator[1] <- 0

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x),
    "AOI sensitivity-model counts must be finite and non-negative, with positive denominators.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity errors when success plus failure does not equal denominator", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()
  x$aoi_glmm_failure[1] <- x$aoi_glmm_failure[1] + 1

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x),
    "For AOI sensitivity models, success + failure must equal the denominator.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity errors when no rows remain", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data() |>
    dplyr::mutate(aoi_glmm_success = NA_real_)

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x),
    "No rows remain after removing missing AOI sensitivity-model variables.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity errors when fewer than two subjects are used for mixed models", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data() |>
    dplyr::filter(aoi_glmm_subject == "S1")

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      x,
      model_types = "binomial_glmm"
    ),
    "At least two subjects are required for mixed AOI sensitivity models.",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity does not require two subjects for quasibinomial-only model", {
  x <- make_test_aoi_sensitivity_data() |>
    dplyr::filter(aoi_glmm_subject == "S1")

  out <- quiet_fit_aoi_sensitivity(
    x,
    model_types = "quasibinomial_glm"
  )

  expect_s3_class(out, "gp3_aoi_model_sensitivity")
  expect_equal(out$comparison$model_type, "quasibinomial_glm")
})

test_that("fit_gazepoint_aoi_model_sensitivity errors for invalid inputs", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  expect_error(
    fit_gazepoint_aoi_model_sensitivity("not data"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, success_col = NA_character_),
    "`success_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, failure_col = NA_character_),
    "`failure_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, denominator_col = NA_character_),
    "`denominator_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, proportion_col = NA_character_),
    "`proportion_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, subject_col = NA_character_),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, condition_col = NA_character_),
    "`condition_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, window_col = NA_character_),
    "`window_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, model_types = character(0)),
    "`model_types` must be a non-empty character vector.",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, model_types = "bad_model"),
    "Unsupported model type(s): bad_model",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, include_condition = NA),
    "`include_condition` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, include_window = NA),
    "`include_window` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, include_interaction = NA),
    "`include_interaction` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, random_intercept = NA),
    "`random_intercept` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, drop_missing = NA),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, optimizer = NA_character_),
    "`optimizer` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, maxfun = 0),
    "`maxfun` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, nAGQ = -1),
    "`nAGQ` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(x, empirical_logit_correction = 0),
    "`empirical_logit_correction` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity errors when required columns are missing", {
  testthat::skip_if_not_installed("lme4")

  x <- make_test_aoi_sensitivity_data()

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_success)
    ),
    "Missing required columns: aoi_glmm_success",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_failure)
    ),
    "Missing required columns: aoi_glmm_failure",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_denominator)
    ),
    "Missing required columns: aoi_glmm_denominator",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_prop)
    ),
    "Missing required columns: aoi_glmm_prop",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_subject)
    ),
    "Missing required columns: aoi_glmm_subject",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_condition)
    ),
    "Missing required columns: aoi_glmm_condition",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_aoi_model_sensitivity(
      dplyr::select(x, -aoi_glmm_window)
    ),
    "Missing required columns: aoi_glmm_window",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_aoi_model_sensitivity works with real aoi_glmm_data object when available", {
  testthat::skip_if_not_installed("lme4")

  if (exists("aoi_glmm_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("aoi_glmm_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "aoi_glmm_success",
      "aoi_glmm_failure",
      "aoi_glmm_denominator",
      "aoi_glmm_prop",
      "aoi_glmm_subject",
      "aoi_glmm_condition",
      "aoi_glmm_window"
    ) %in% names(real_data))) {
      out <- quiet_fit_aoi_sensitivity(real_data)

      expect_s3_class(out, "gp3_aoi_model_sensitivity")
      expect_s3_class(out$comparison, "tbl_df")
      expect_s3_class(out$fixed_effects, "tbl_df")
      expect_true(all(out$comparison$model_status %in% c(
        "ok",
        "singular_fit",
        "fit_failed"
      )))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
