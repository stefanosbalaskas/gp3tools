make_test_loo_data <- function(seed = 123) {
  set.seed(seed)

  tibble::tibble(
    subject = rep(paste0("S", 1:12), each = 20),
    item = rep(paste0("I", 1:6), length.out = 240),
    condition = rep(rep(c("control", "treatment"), each = 10), times = 12),
    outcome = 1 +
      ifelse(condition == "treatment", 0.35, 0) +
      rep(stats::rnorm(12, 0, 0.15), each = 20) +
      stats::rnorm(240, 0, 0.10)
  )
}

make_test_loo_fit <- function(d) {
  stats::lm(outcome ~ condition, data = d)
}

make_test_loo_extract <- function(model) {
  coef_summary <- summary(model)$coefficients

  tibble::tibble(
    term = rownames(coef_summary),
    estimate = coef_summary[, "Estimate"],
    std_error = coef_summary[, "Std. Error"],
    statistic = coef_summary[, "t value"],
    p_value = coef_summary[, "Pr(>|t|)"]
  )
}

test_that("run_gazepoint_model_leave_one_out creates a complete subject-level sensitivity object", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = make_test_loo_fit,
    effect_terms = "conditiontreatment",
    keep_models = FALSE,
    name = "toy_loso"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "full_effects",
      "leave_one_results",
      "leave_one_effects",
      "effect_summary",
      "full_model",
      "refit_models",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$full_effects, "tbl_df")
  expect_s3_class(out$leave_one_results, "tbl_df")
  expect_s3_class(out$leave_one_effects, "tbl_df")
  expect_s3_class(out$effect_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_loso")
  expect_equal(out$overview$sensitivity_status, "complete")
  expect_equal(out$overview$unit_col, "subject")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_units, 12)
  expect_equal(out$overview$n_complete, 12)
  expect_equal(out$overview$n_fit_error, 0)
  expect_equal(out$overview$n_extract_error, 0)
  expect_equal(out$overview$n_skipped, 0)
  expect_equal(out$overview$n_effect_terms, 1)
  expect_false(out$overview$keep_models)

  expect_equal(nrow(out$full_effects), 1)
  expect_equal(out$full_effects$term, "conditiontreatment")
  expect_equal(out$full_effects$model_scope, "full_data")
  expect_true(is.na(out$full_effects$left_out_unit))
  expect_equal(out$full_effects$n_rows_used, nrow(toy_data))

  expect_equal(nrow(out$leave_one_results), 12)
  expect_true(all(out$leave_one_results$model_status == "complete"))
  expect_equal(nrow(out$leave_one_effects), 12)
  expect_true(all(out$leave_one_effects$model_scope == "leave_one_out"))
  expect_true(all(out$leave_one_effects$term == "conditiontreatment"))

  expect_equal(nrow(out$effect_summary), 1)
  expect_equal(out$effect_summary$term, "conditiontreatment")
  expect_equal(out$effect_summary$n_refits_complete, 12)
  expect_true(out$effect_summary$max_abs_change >= 0)
  expect_false(out$effect_summary$sign_flip)

  expect_null(out$full_model)
  expect_null(out$refit_models)
})

test_that("run_gazepoint_model_leave_one_out supports custom extract functions", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = make_test_loo_fit,
    extract_function = make_test_loo_extract,
    effect_terms = "conditiontreatment",
    name = "custom_extract_loso"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$sensitivity_status, "complete")
  expect_equal(out$overview$n_complete, 12)

  expect_true(all(c(
    "term",
    "estimate",
    "std_error",
    "statistic",
    "p_value",
    "conf_low",
    "conf_high"
  ) %in% names(out$leave_one_effects)))

  expect_equal(out$effect_summary$n_refits_complete, 12)
})

test_that("run_gazepoint_model_leave_one_out can retain fitted models", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = make_test_loo_fit,
    effect_terms = "conditiontreatment",
    keep_models = TRUE,
    name = "keep_models_loso"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$sensitivity_status, "complete")
  expect_true(out$overview$keep_models)

  expect_s3_class(out$full_model, "lm")
  expect_type(out$refit_models, "list")
  expect_equal(length(out$refit_models), 12)
  expect_true(all(vapply(out$refit_models, inherits, logical(1), what = "lm")))
})

test_that("run_gazepoint_model_leave_one_out supports item-level leave-one-out", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "item",
    fit_function = make_test_loo_fit,
    effect_terms = "conditiontreatment",
    name = "toy_loio"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$unit_col, "item")
  expect_equal(out$overview$sensitivity_status, "complete")
  expect_equal(out$overview$n_units, 6)
  expect_equal(out$overview$n_complete, 6)
  expect_equal(nrow(out$leave_one_results), 6)
  expect_equal(nrow(out$leave_one_effects), 6)
  expect_equal(out$effect_summary$n_refits_complete, 6)
})

test_that("run_gazepoint_model_leave_one_out supports multiple effect terms", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = make_test_loo_fit,
    effect_terms = c("(Intercept)", "conditiontreatment")
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$n_effect_terms, 2)
  expect_equal(nrow(out$full_effects), 2)
  expect_equal(nrow(out$effect_summary), 2)
  expect_true(all(c("(Intercept)", "conditiontreatment") %in% out$effect_summary$term))
})

test_that("run_gazepoint_model_leave_one_out supports no effect term filter", {
  toy_data <- make_test_loo_data()

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = make_test_loo_fit
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_true(out$overview$n_effect_terms >= 2)
  expect_true("(Intercept)" %in% out$full_effects$term)
  expect_true("conditiontreatment" %in% out$full_effects$term)
})

test_that("run_gazepoint_model_leave_one_out records fit errors cleanly", {
  toy_data <- make_test_loo_data()

  fit_with_error <- function(d) {
    if (!"S1" %in% d$subject) {
      stop("mock fit error after removing S1", call. = FALSE)
    }

    stats::lm(outcome ~ condition, data = d)
  }

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = fit_with_error,
    effect_terms = "conditiontreatment"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$sensitivity_status, "partial_complete")
  expect_equal(out$overview$n_fit_error, 1)
  expect_equal(out$overview$n_complete, 11)

  error_row <- out$leave_one_results[out$leave_one_results$model_status == "fit_error", ]

  expect_equal(error_row$left_out_unit, "S1")
  expect_match(error_row$message, "mock fit error after removing S1", fixed = TRUE)
})

test_that("run_gazepoint_model_leave_one_out records extraction errors cleanly", {
  toy_data <- make_test_loo_data()

  fit_with_subject_marker <- function(d) {
    fit <- stats::lm(outcome ~ condition, data = d)
    attr(fit, "has_s1") <- "S1" %in% d$subject
    fit
  }

  extract_with_error <- function(model) {
    if (!isTRUE(attr(model, "has_s1"))) {
      stop("mock extraction error after removing S1", call. = FALSE)
    }

    coefs <- stats::coef(model)

    tibble::tibble(
      term = "conditiontreatment",
      estimate = coefs[["conditiontreatment"]]
    )
  }

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = fit_with_subject_marker,
    extract_function = extract_with_error
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$sensitivity_status, "partial_complete")
  expect_equal(out$overview$n_extract_error, 1)
  expect_equal(out$overview$n_complete, 11)

  error_row <- out$leave_one_results[out$leave_one_results$model_status == "extract_error", ]

  expect_equal(error_row$left_out_unit, "S1")
  expect_match(error_row$message, "mock extraction error after removing S1", fixed = TRUE)
})

test_that("run_gazepoint_model_leave_one_out can skip units with too few rows", {
  toy_data <- tibble::tibble(
    subject = c("S1", "S1", "S2", "S2"),
    condition = c("control", "treatment", "control", "treatment"),
    outcome = c(1, 2, 1.1, 2.1)
  )

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = function(d) stats::lm(outcome ~ condition, data = d),
    min_rows = 3
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_equal(out$overview$sensitivity_status, "failed")
  expect_equal(out$overview$n_skipped, 2)
  expect_true(all(out$leave_one_results$model_status == "skipped_too_few_rows"))
  expect_equal(nrow(out$leave_one_effects), 0)
})

test_that("run_gazepoint_model_leave_one_out detects sign flips", {
  toy_data <- tibble::tibble(
    subject = rep(c("S1", "S2", "S3", "S4"), each = 10),
    condition = rep(rep(c("control", "treatment"), each = 5), times = 4),
    outcome = c(
      rep(1, 5), rep(3.5, 5),
      rep(1, 5), rep(1.1, 5),
      rep(1, 5), rep(1.1, 5),
      rep(1, 5), rep(0.1, 5)
    )
  )

  out <- run_gazepoint_model_leave_one_out(
    toy_data,
    unit_col = "subject",
    fit_function = function(d) stats::lm(outcome ~ condition, data = d),
    effect_terms = "conditiontreatment"
  )

  expect_s3_class(out, "gp3_model_leave_one_out_sensitivity")
  expect_true(is.logical(out$effect_summary$sign_flip))
  expect_true(out$effect_summary$max_abs_change >= 0)
})

test_that("run_gazepoint_model_leave_one_out handles full model fit failure", {
  toy_data <- make_test_loo_data()

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = function(d) stop("full model failed", call. = FALSE)
    ),
    "The full-data model could not be fitted: full model failed",
    fixed = TRUE
  )
})

test_that("run_gazepoint_model_leave_one_out handles full model extraction failure", {
  toy_data <- make_test_loo_data()

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      extract_function = function(model) stop("full extraction failed", call. = FALSE)
    ),
    "Effects could not be extracted from the full-data model: full extraction failed",
    fixed = TRUE
  )
})

test_that("run_gazepoint_model_leave_one_out checks invalid inputs", {
  toy_data <- make_test_loo_data()

  expect_error(
    run_gazepoint_model_leave_one_out(
      list(),
      unit_col = "subject",
      fit_function = make_test_loo_fit
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data[0, ],
      unit_col = "subject",
      fit_function = make_test_loo_fit
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "bad_subject",
      fit_function = make_test_loo_fit
    ),
    "`unit_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = "bad"
    ),
    "`fit_function` must be a function",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      extract_function = "bad"
    ),
    "`extract_function` must be a function",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      effect_terms = NA_character_
    ),
    "`effect_terms` must be NULL or a character vector",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      min_rows = 0
    ),
    "`min_rows` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      keep_models = NA
    ),
    "`keep_models` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    run_gazepoint_model_leave_one_out(
      toy_data,
      unit_col = "subject",
      fit_function = make_test_loo_fit,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )

  one_unit <- toy_data |>
    dplyr::filter(.data$subject == "S1")

  expect_error(
    run_gazepoint_model_leave_one_out(
      one_unit,
      unit_col = "subject",
      fit_function = make_test_loo_fit
    ),
    "`unit_col` must contain at least two non-missing units",
    fixed = TRUE
  )
})
