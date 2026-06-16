make_test_nested_data <- function(seed = 123) {
  set.seed(seed)

  tidyr::expand_grid(
    subject = paste0("S", 1:25),
    condition = c("control", "treatment"),
    time = seq(0, 1000, by = 100)
  ) |>
    dplyr::mutate(
      outcome = 1 +
        0.001 * .data$time +
        ifelse(.data$condition == "treatment", 0.25 + 0.0004 * .data$time, 0) +
        stats::rnorm(dplyr::n(), 0, 0.12)
    )
}

make_test_nested_models <- function() {
  toy_data <- make_test_nested_data()

  list(
    null = stats::lm(outcome ~ 1, data = toy_data),
    time = stats::lm(outcome ~ time, data = toy_data),
    condition = stats::lm(outcome ~ time + condition, data = toy_data),
    interaction = stats::lm(outcome ~ time * condition, data = toy_data)
  )
}

make_fake_nested_model <- function(logLik, df, AIC, BIC, nobs = 100) {
  structure(
    list(
      logLik = logLik,
      df = df,
      AIC = AIC,
      BIC = BIC,
      nobs = nobs
    ),
    class = "gp3_test_nested_model"
  )
}

logLik.gp3_test_nested_model <- function(object, ...) {
  structure(
    as.numeric(object$logLik),
    df = as.numeric(object$df),
    nobs = as.numeric(object$nobs),
    class = "logLik"
  )
}

AIC.gp3_test_nested_model <- function(object, ..., k = 2) {
  as.numeric(object$AIC)
}

BIC.gp3_test_nested_model <- function(object, ...) {
  as.numeric(object$BIC)
}

nobs.gp3_test_nested_model <- function(object, ...) {
  as.numeric(object$nobs)
}

test_that("compare_gazepoint_nested_models compares sequential lm models", {
  models <- make_test_nested_models()

  out <- compare_gazepoint_nested_models(
    models = models,
    comparison = "sequential",
    name = "toy_nested_comparison"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "model_table",
      "lrt_table",
      "ranking_table",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$model_table, "tbl_df")
  expect_s3_class(out$lrt_table, "tbl_df")
  expect_s3_class(out$ranking_table, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_nested_comparison")
  expect_equal(out$overview$comparison_status, "complete")
  expect_equal(out$overview$comparison, "sequential")
  expect_equal(out$overview$n_models, 4)
  expect_equal(out$overview$n_complete_models, 4)
  expect_equal(out$overview$n_model_extraction_errors, 0)
  expect_equal(out$overview$n_lrt_comparisons, 3)
  expect_equal(out$overview$n_lrt_complete, 3)
  expect_equal(out$overview$n_lrt_problem, 0)
  expect_equal(out$overview$best_aic_model, "interaction")
  expect_equal(out$overview$best_bic_model, "interaction")

  expect_equal(out$model_table$model_name, c("null", "time", "condition", "interaction"))
  expect_true(all(out$model_table$extraction_status == "complete"))
  expect_true(all(is.finite(out$model_table$logLik)))
  expect_true(all(is.finite(out$model_table$AIC)))
  expect_true(all(is.finite(out$model_table$BIC)))
  expect_true(all(is.finite(out$model_table$df)))
  expect_true(all(is.finite(out$model_table$nobs)))

  expect_equal(out$lrt_table$model_0, c("null", "time", "condition"))
  expect_equal(out$lrt_table$model_1, c("time", "condition", "interaction"))
  expect_true(all(out$lrt_table$comparison_status == "complete"))
  expect_true(all(out$lrt_table$df_diff > 0))
  expect_true(all(out$lrt_table$chisq >= 0))
  expect_true(all(out$lrt_table$p_value >= 0))
  expect_true(all(out$lrt_table$p_value <= 1))

  expect_equal(out$ranking_table$model_name[1], "interaction")
  expect_equal(out$ranking_table$aic_rank[out$ranking_table$model_name == "interaction"], 1)
  expect_equal(out$ranking_table$bic_rank[out$ranking_table$model_name == "interaction"], 1)
  expect_equal(out$ranking_table$delta_AIC[out$ranking_table$model_name == "interaction"], 0)
  expect_equal(out$ranking_table$delta_BIC[out$ranking_table$model_name == "interaction"], 0)

  expect_equal(
    out$settings$value[out$settings$setting == "comparison"],
    "sequential"
  )
})

test_that("compare_gazepoint_nested_models supports against-first comparisons", {
  models <- make_test_nested_models()

  out <- compare_gazepoint_nested_models(
    models = models,
    comparison = "against_first",
    name = "against_first_comparison"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$overview$comparison_status, "complete")
  expect_equal(out$overview$comparison, "against_first")
  expect_equal(out$overview$n_lrt_comparisons, 3)

  expect_equal(out$lrt_table$model_0, rep("null", 3))
  expect_equal(out$lrt_table$model_1, c("time", "condition", "interaction"))
  expect_true(all(out$lrt_table$comparison_status == "complete"))
  expect_true(all(out$lrt_table$df_diff > 0))
})

test_that("compare_gazepoint_nested_models supports supplied model names", {
  models <- unname(make_test_nested_models())

  out <- compare_gazepoint_nested_models(
    models = models,
    model_names = c("m0_null", "m1_time", "m2_condition", "m3_interaction"),
    comparison = "sequential",
    name = "custom_names"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(
    out$model_table$model_name,
    c("m0_null", "m1_time", "m2_condition", "m3_interaction")
  )
  expect_equal(out$lrt_table$model_0, c("m0_null", "m1_time", "m2_condition"))
  expect_equal(out$lrt_table$model_1, c("m1_time", "m2_condition", "m3_interaction"))
  expect_equal(
    out$settings$value[out$settings$setting == "model_names"],
    "m0_null, m1_time, m2_condition, m3_interaction"
  )
})

test_that("compare_gazepoint_nested_models generates names for unnamed models", {
  models <- unname(make_test_nested_models())

  out <- compare_gazepoint_nested_models(
    models = models,
    comparison = "sequential"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$model_table$model_name, c("model_1", "model_2", "model_3", "model_4"))
  expect_equal(out$overview$comparison_status, "complete")
})

test_that("compare_gazepoint_nested_models handles one model", {
  models <- make_test_nested_models()

  out <- compare_gazepoint_nested_models(
    models = list(null = models$null),
    name = "one_model_only"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$overview$comparison_status, "not_enough_models")
  expect_equal(out$overview$n_models, 1)
  expect_equal(out$overview$n_complete_models, 1)
  expect_equal(out$overview$n_lrt_comparisons, 0)
  expect_equal(nrow(out$lrt_table), 0)
  expect_equal(nrow(out$model_table), 1)
})

test_that("compare_gazepoint_nested_models records model extraction errors", {
  models <- make_test_nested_models()

  bad_model <- structure(
    list(x = 1),
    class = "gp3_bad_model_without_methods"
  )

  out <- compare_gazepoint_nested_models(
    models = list(
      null = models$null,
      bad = bad_model,
      interaction = models$interaction
    ),
    comparison = "sequential",
    name = "partial_extraction"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$overview$comparison_status, "partial_complete")
  expect_equal(out$overview$n_models, 3)
  expect_equal(out$overview$n_complete_models, 2)
  expect_equal(out$overview$n_model_extraction_errors, 1)
  expect_true("extraction_error" %in% out$model_table$extraction_status)
  expect_true(any(out$lrt_table$comparison_status == "model_extraction_error"))
  expect_true(any(!is.na(out$model_table$message)))
})

test_that("compare_gazepoint_nested_models detects nonpositive df differences", {
  models <- make_test_nested_models()

  out <- compare_gazepoint_nested_models(
    models = list(
      interaction = models$interaction,
      condition = models$condition
    ),
    comparison = "sequential",
    name = "reversed_models"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$overview$comparison_status, "partial_complete")
  expect_equal(out$lrt_table$comparison_status, "nonpositive_df_difference")
  expect_true(is.na(out$lrt_table$p_value))
  expect_match(
    out$lrt_table$message,
    "did not have more degrees of freedom",
    fixed = TRUE
  )
})

test_that("compare_gazepoint_nested_models detects negative LRT statistics", {
  m0 <- make_fake_nested_model(
    logLik = 100,
    df = 2,
    AIC = -196,
    BIC = -190
  )

  m1 <- make_fake_nested_model(
    logLik = 90,
    df = 3,
    AIC = -174,
    BIC = -165
  )

  out <- compare_gazepoint_nested_models(
    models = list(reference = m0, worse_complex = m1),
    comparison = "sequential",
    name = "negative_lrt"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$model_table$extraction_status, c("complete", "complete"))
  expect_equal(out$overview$comparison_status, "partial_complete")
  expect_equal(out$lrt_table$comparison_status, "negative_lrt_statistic")
  expect_true(out$lrt_table$chisq < 0)
  expect_true(is.na(out$lrt_table$p_value))
})

test_that("compare_gazepoint_nested_models ranks AIC and BIC correctly", {
  m0 <- make_fake_nested_model(
    logLik = -100,
    df = 2,
    AIC = 204,
    BIC = 210
  )

  m1 <- make_fake_nested_model(
    logLik = -80,
    df = 3,
    AIC = 166,
    BIC = 176
  )

  m2 <- make_fake_nested_model(
    logLik = -75,
    df = 6,
    AIC = 162,
    BIC = 185
  )

  out <- compare_gazepoint_nested_models(
    models = list(simple = m0, middle = m1, complex = m2),
    comparison = "sequential",
    name = "ranking_check"
  )

  expect_s3_class(out, "gp3_nested_model_comparison")
  expect_equal(out$overview$best_aic_model, "complex")
  expect_equal(out$overview$best_bic_model, "middle")

  expect_equal(out$ranking_table$aic_rank[out$ranking_table$model_name == "complex"], 1)
  expect_equal(out$ranking_table$bic_rank[out$ranking_table$model_name == "middle"], 1)
  expect_equal(out$ranking_table$delta_AIC[out$ranking_table$model_name == "complex"], 0)
  expect_equal(out$ranking_table$delta_BIC[out$ranking_table$model_name == "middle"], 0)
})

test_that("compare_gazepoint_nested_models checks invalid inputs", {
  models <- make_test_nested_models()

  expect_error(
    compare_gazepoint_nested_models(
      models = NULL
    ),
    "`models` must be a non-empty list of fitted model objects",
    fixed = TRUE
  )

  expect_error(
    compare_gazepoint_nested_models(
      models = list()
    ),
    "`models` must be a non-empty list of fitted model objects",
    fixed = TRUE
  )

  expect_error(
    compare_gazepoint_nested_models(
      models = models,
      model_names = c("a", "b")
    ),
    "`model_names` must be NULL or a non-missing character vector with one name per model",
    fixed = TRUE
  )

  expect_error(
    compare_gazepoint_nested_models(
      models = models,
      model_names = c("a", "b", "c", NA_character_)
    ),
    "`model_names` must be NULL or a non-missing character vector with one name per model",
    fixed = TRUE
  )

  expect_error(
    compare_gazepoint_nested_models(
      models = models,
      model_names = c("a", "b", "c", "c")
    ),
    "`model_names` must be unique",
    fixed = TRUE
  )

  duplicated_names <- models
  names(duplicated_names) <- c("m", "m", "m2", "m3")

  expect_error(
    compare_gazepoint_nested_models(
      models = duplicated_names
    ),
    "Model names must be unique. Supply `model_names` to disambiguate them.",
    fixed = TRUE
  )

  expect_error(
    compare_gazepoint_nested_models(
      models = models,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
