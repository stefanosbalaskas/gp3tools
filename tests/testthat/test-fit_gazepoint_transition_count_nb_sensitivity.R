make_test_transition_nb_data <- function() {
  set.seed(123)

  tidyr::expand_grid(
    subject = paste0("S", 1:12),
    condition = c("A", "B"),
    from_aoi = c("logo", "claim", "product"),
    to_aoi = c("logo", "claim", "product")
  ) |>
    dplyr::filter(.data$from_aoi != .data$to_aoi) |>
    dplyr::mutate(
      subject_effect = rep(seq(0.2, 1.1, length.out = 12), each = 12),
      exposure = 100,
      offset_value = log(.data$exposure),
      transition_count = stats::rpois(
        dplyr::n(),
        lambda = exp(
          0.7 +
            0.25 * (.data$condition == "B") +
            0.15 * (.data$from_aoi == "claim") +
            .data$subject_effect
        )
      )
    ) |>
    dplyr::select(-dplyr::all_of("subject_effect"))
}

test_that("fit_gazepoint_transition_count_nb_sensitivity skips cleanly when glmmTMB is unavailable", {
  testthat::local_mocked_bindings(
    .gp3_transition_nb_namespace_available = function() FALSE
  )

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = "subject",
    name = "toy_transition_nb"
  )

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "model",
      "model_summary",
      "fixed_effects",
      "random_effects",
      "model_data",
      "settings"
    )
  )

  expect_null(out$model)
  expect_null(out$model_summary)
  expect_null(out$fixed_effects)
  expect_null(out$random_effects)

  expect_equal(out$overview$model_status, "skipped_missing_package")
  expect_match(out$overview$message, "Optional package 'glmmTMB' is not installed", fixed = TRUE)

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$model_data, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_true(".gp3_transition_pair" %in% names(out$model_data))
  expect_true(".gp3_transition_count_nb_response" %in% names(out$model_data))
})

test_that("fit_gazepoint_transition_count_nb_sensitivity fits nbinom2 model when glmmTMB is available", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    family = "nbinom2",
    name = "toy_transition_nb"
  )

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_s3_class(out$model, "glmmTMB")

  expect_equal(out$overview$object_name, "toy_transition_nb")
  expect_equal(out$overview$n_rows, nrow(toy_data))
  expect_equal(out$overview$family, "nbinom2")
  expect_equal(out$overview$model_status, "complete")
  expect_match(out$overview$message, "Negative-binomial transition-count sensitivity model fitted", fixed = TRUE)

  expect_true(is.finite(out$overview$AIC))
  expect_true(is.finite(out$overview$BIC))
  expect_true(is.finite(out$overview$logLik))

  expect_s3_class(out$fixed_effects, "tbl_df")
  expect_true("term" %in% names(out$fixed_effects))
  expect_true("Estimate" %in% names(out$fixed_effects))
  expect_true(any(out$fixed_effects$term == "conditionB"))

  expect_true(".gp3_transition_pair" %in% names(out$model_data))
  expect_false(anyNA(out$model_data$.gp3_transition_count_nb_response))
})

test_that("fit_gazepoint_transition_count_nb_sensitivity fits nbinom1 sensitivity when glmmTMB is available", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    family = "nbinom1",
    name = "toy_transition_nb_nbinom1"
  )

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_s3_class(out$model, "glmmTMB")
  expect_equal(out$overview$family, "nbinom1")
  expect_equal(out$overview$model_status, "complete")
  expect_s3_class(out$fixed_effects, "tbl_df")
})

test_that("fit_gazepoint_transition_count_nb_sensitivity auto-detects common columns", {
  testthat::local_mocked_bindings(
    .gp3_transition_nb_namespace_available = function() FALSE
  )

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(toy_data)

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_equal(out$overview$model_status, "skipped_missing_package")

  expect_equal(out$settings$value[out$settings$setting == "count_col"], "transition_count")
  expect_equal(out$settings$value[out$settings$setting == "from_col"], "from_aoi")
  expect_equal(out$settings$value[out$settings$setting == "to_col"], "to_aoi")
  expect_equal(out$settings$value[out$settings$setting == "random_effect_cols"], "subject")
})

test_that("fit_gazepoint_transition_count_nb_sensitivity supports exposure offsets", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    exposure_col = "exposure",
    family = "nbinom2"
  )

  expect_equal(out$overview$model_status, "complete")
  expect_true(".gp3_transition_nb_offset" %in% names(out$model_data))
  expect_true(all(is.finite(out$model_data$.gp3_transition_nb_offset)))
  expect_match(out$overview$formula, "offset(.gp3_transition_nb_offset)", fixed = TRUE)
})

test_that("fit_gazepoint_transition_count_nb_sensitivity supports numeric offsets", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    offset_col = "offset_value",
    family = "nbinom2"
  )

  expect_equal(out$overview$model_status, "complete")
  expect_true(".gp3_transition_nb_offset" %in% names(out$model_data))
  expect_true(all(is.finite(out$model_data$.gp3_transition_nb_offset)))
  expect_match(out$overview$formula, "offset(.gp3_transition_nb_offset)", fixed = TRUE)
})

test_that("fit_gazepoint_transition_count_nb_sensitivity supports supplied formulas", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    formula = .gp3_transition_count_nb_response ~ condition,
    family = "nbinom2"
  )

  expect_equal(out$overview$model_status, "complete")
  expect_match(out$overview$formula, "~ condition", fixed = TRUE)
  expect_s3_class(out$model, "glmmTMB")
})

test_that("fit_gazepoint_transition_count_nb_sensitivity supports zero-inflation setting", {
  testthat::skip_if_not_installed("glmmTMB")

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = character(0),
    zero_inflation = TRUE,
    family = "nbinom2"
  )

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_s3_class(out$model, "glmmTMB")
  expect_equal(out$overview$model_status, "complete")
  expect_equal(out$settings$value[out$settings$setting == "zero_inflation"], "TRUE")
})

test_that("fit_gazepoint_transition_count_nb_sensitivity records model-fit errors cleanly", {
  testthat::local_mocked_bindings(
    .gp3_transition_nb_namespace_available = function() TRUE,
    .gp3_transition_nb_get_export = function(function_name) {
      if (identical(function_name, "glmmTMB")) {
        return(function(...) stop("mock glmmTMB fit error", call. = FALSE))
      }

      if (identical(function_name, "nbinom2")) {
        return(function(link = "log") stats::poisson(link = link))
      }

      if (identical(function_name, "nbinom1")) {
        return(function(link = "log") stats::poisson(link = link))
      }

      stop("Unexpected function requested.", call. = FALSE)
    }
  )

  toy_data <- make_test_transition_nb_data()

  out <- fit_gazepoint_transition_count_nb_sensitivity(
    toy_data,
    count_col = "transition_count",
    from_col = "from_aoi",
    to_col = "to_aoi",
    condition_cols = "condition",
    random_effect_cols = "subject"
  )

  expect_s3_class(out, "gp3_transition_count_nb_sensitivity")
  expect_equal(out$overview$model_status, "error_model_fit")
  expect_match(out$overview$message, "mock glmmTMB fit error", fixed = TRUE)
  expect_null(out$model)
  expect_null(out$model_summary)
  expect_null(out$fixed_effects)
})

test_that("fit_gazepoint_transition_count_nb_sensitivity checks invalid inputs", {
  toy_data <- make_test_transition_nb_data()

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      count_col = "bad_count"
    ),
    "`count_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      from_col = "bad_from"
    ),
    "`from_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      to_col = "bad_to"
    ),
    "`to_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      condition_cols = "bad_condition"
    ),
    "All `condition_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      random_effect_cols = "bad_subject"
    ),
    "All `random_effect_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      exposure_col = "exposure",
      offset_col = "offset_value"
    ),
    "Use either `exposure_col` or `offset_col`, not both",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      formula = "bad_formula"
    ),
    "`formula` must be a formula when supplied",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      zero_inflation = NA
    ),
    "`zero_inflation` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_transition_count_nb_sensitivity validates count outcomes", {
  toy_data <- make_test_transition_nb_data()

  missing_count <- toy_data
  missing_count$transition_count[1] <- NA_real_

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      missing_count,
      count_col = "transition_count",
      from_col = "from_aoi",
      to_col = "to_aoi"
    ),
    "`count_col` must contain finite non-missing counts",
    fixed = TRUE
  )

  negative_count <- toy_data
  negative_count$transition_count[1] <- -1

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      negative_count,
      count_col = "transition_count",
      from_col = "from_aoi",
      to_col = "to_aoi"
    ),
    "`count_col` must contain non-negative counts",
    fixed = TRUE
  )

  decimal_count <- toy_data
  decimal_count$transition_count[1] <- 1.5

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      decimal_count,
      count_col = "transition_count",
      from_col = "from_aoi",
      to_col = "to_aoi"
    ),
    "`count_col` must contain integer-valued counts",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_transition_count_nb_sensitivity validates exposure and offset values", {
  toy_data <- make_test_transition_nb_data()

  bad_exposure <- toy_data
  bad_exposure$exposure[1] <- 0

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      bad_exposure,
      count_col = "transition_count",
      from_col = "from_aoi",
      to_col = "to_aoi",
      exposure_col = "exposure"
    ),
    "`exposure_col` must contain finite positive values",
    fixed = TRUE
  )

  bad_offset <- toy_data
  bad_offset$offset_value[1] <- NA_real_

  expect_error(
    fit_gazepoint_transition_count_nb_sensitivity(
      bad_offset,
      count_col = "transition_count",
      from_col = "from_aoi",
      to_col = "to_aoi",
      offset_col = "offset_value"
    ),
    "`offset_col` must contain finite numeric values",
    fixed = TRUE
  )
})
