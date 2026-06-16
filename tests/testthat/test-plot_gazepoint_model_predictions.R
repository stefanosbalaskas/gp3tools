make_test_prediction_data <- function(seed = 123) {
  set.seed(seed)

  tidyr::expand_grid(
    subject = paste0("S", 1:20),
    condition = c("control", "treatment"),
    time = seq(0, 1000, by = 100)
  ) |>
    dplyr::mutate(
      subject_shift = stats::rnorm(20)[match(.data$subject, paste0("S", 1:20))],
      outcome = 1 +
        0.001 * .data$time +
        ifelse(.data$condition == "treatment", 0.25 + 0.0004 * .data$time, 0) +
        .data$subject_shift * 0.08 +
        stats::rnorm(dplyr::n(), 0, 0.10)
    )
}

test_that("plot_gazepoint_model_predictions plots observed summaries and lm predictions", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    prediction_type = "response",
    name = "toy_prediction_plot"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")
  expect_s3_class(p, "ggplot")

  overview <- attr(p, "gp3_model_prediction_overview")
  observed_summary <- attr(p, "gp3_model_prediction_observed_summary")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")
  settings <- attr(p, "gp3_model_prediction_settings")

  expect_s3_class(overview, "tbl_df")
  expect_s3_class(observed_summary, "tbl_df")
  expect_s3_class(prediction_summary, "tbl_df")
  expect_s3_class(settings, "tbl_df")

  expect_equal(overview$object_name, "toy_prediction_plot")
  expect_equal(overview$plot_type, "model_predictions")
  expect_equal(overview$prediction_status, "complete")
  expect_true(is.na(overview$prediction_error))
  expect_equal(overview$x_col, "time")
  expect_equal(overview$outcome_col, "outcome")
  expect_equal(overview$condition_col, "condition")
  expect_equal(overview$n_input_rows, nrow(toy_data))
  expect_equal(overview$n_observed_summary_rows, 22)
  expect_equal(overview$n_prediction_summary_rows, 22)
  expect_equal(overview$prediction_type, "response")
  expect_false(overview$include_random_effects)
  expect_equal(overview$observed_summary_function, "mean")
  expect_equal(overview$ci, 0.95)

  expect_true(all(c(
    ".gp3_x",
    ".gp3_plot_group",
    "observed",
    "observed_sd",
    "observed_n",
    "observed_se",
    "observed_lower",
    "observed_upper"
  ) %in% names(observed_summary)))

  expect_true(all(c(
    ".gp3_x",
    ".gp3_plot_group",
    "predicted",
    "predicted_lower",
    "predicted_upper",
    "prediction_n"
  ) %in% names(prediction_summary)))

  expect_equal(sort(unique(observed_summary$.gp3_plot_group)), c("control", "treatment"))
  expect_equal(sort(unique(prediction_summary$.gp3_plot_group)), c("control", "treatment"))
  expect_true(all(is.finite(prediction_summary$predicted)))
  expect_true(any(is.finite(prediction_summary$predicted_lower)))
  expect_true(any(is.finite(prediction_summary$predicted_upper)))

  expect_equal(settings$value[settings$setting == "name"], "toy_prediction_plot")
})

test_that("plot_gazepoint_model_predictions supports observed-only plots", {
  toy_data <- make_test_prediction_data()

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = NULL,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    show_predictions = FALSE,
    name = "observed_only"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")
  expect_s3_class(p, "ggplot")

  overview <- attr(p, "gp3_model_prediction_overview")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")

  expect_equal(overview$prediction_status, "no_model_supplied")
  expect_equal(overview$n_prediction_summary_rows, 0)
  expect_null(prediction_summary)
})

test_that("plot_gazepoint_model_predictions supports no condition column", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ time,
    data = toy_data
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    name = "single_series_prediction"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  observed_summary <- attr(p, "gp3_model_prediction_observed_summary")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")

  expect_true(is.na(overview$condition_col))
  expect_equal(unique(observed_summary$.gp3_plot_group), "All observations")
  expect_equal(unique(prediction_summary$.gp3_plot_group), "All observations")
  expect_equal(nrow(observed_summary), length(unique(toy_data$time)))
  expect_equal(nrow(prediction_summary), length(unique(toy_data$time)))
})

test_that("plot_gazepoint_model_predictions supports supplied newdata", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  newdata <- tidyr::expand_grid(
    condition = c("control", "treatment"),
    time = seq(0, 1000, by = 250)
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    newdata = newdata,
    name = "newdata_prediction"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")
  settings <- attr(p, "gp3_model_prediction_settings")

  expect_equal(overview$prediction_status, "complete")
  expect_equal(nrow(prediction_summary), nrow(newdata))
  expect_equal(sort(unique(prediction_summary$.gp3_x)), seq(0, 1000, by = 250))
  expect_equal(settings$value[settings$setting == "newdata"], "supplied_newdata")
})

test_that("plot_gazepoint_model_predictions supports GLM response predictions", {
  set.seed(123)

  toy_data <- tidyr::expand_grid(
    subject = paste0("S", 1:25),
    condition = c("control", "treatment"),
    time = seq(0, 1000, by = 200)
  ) |>
    dplyr::mutate(
      eta = -1 + 0.001 * .data$time + ifelse(.data$condition == "treatment", 0.8, 0),
      p = stats::plogis(.data$eta),
      outcome = stats::rbinom(dplyr::n(), size = 1, prob = .data$p)
    )

  model <- stats::glm(
    outcome ~ condition + time,
    data = toy_data,
    family = stats::binomial()
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    prediction_type = "response",
    name = "glm_prediction_plot"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")

  expect_equal(overview$prediction_status, "complete")
  expect_true(all(prediction_summary$predicted >= 0))
  expect_true(all(prediction_summary$predicted <= 1))
  expect_true(any(is.finite(prediction_summary$predicted_lower)))
  expect_true(any(is.finite(prediction_summary$predicted_upper)))
})

test_that("plot_gazepoint_model_predictions supports median observed summaries", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    observed_summary_function = "median",
    name = "median_observed_prediction"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  settings <- attr(p, "gp3_model_prediction_settings")

  expect_equal(overview$observed_summary_function, "median")
  expect_equal(settings$value[settings$setting == "observed_summary_function"], "median")
})

test_that("plot_gazepoint_model_predictions supports grouping and faceting", {
  toy_data <- make_test_prediction_data() |>
    dplyr::mutate(
      block = dplyr::if_else(.data$time <= 500, "early", "late"),
      stimulus = rep(c("A", "B"), length.out = dplyr::n())
    )

  model <- stats::lm(
    outcome ~ condition * time + stimulus,
    data = toy_data
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    group_cols = "stimulus",
    facet_cols = "block",
    name = "grouped_faceted_prediction"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  observed_summary <- attr(p, "gp3_model_prediction_observed_summary")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")

  expect_equal(overview$group_cols, "stimulus")
  expect_equal(overview$facet_cols, "block")
  expect_true("block" %in% names(observed_summary))
  expect_true("block" %in% names(prediction_summary))
  expect_true(any(grepl("control", observed_summary$.gp3_plot_group)))
  expect_true(any(grepl("A", observed_summary$.gp3_plot_group)))
})

test_that("plot_gazepoint_model_predictions records prediction errors", {
  toy_data <- make_test_prediction_data()

  bad_model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  newdata <- tibble::tibble(
    time = seq(0, 1000, by = 100),
    condition = "new_condition"
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = bad_model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    newdata = newdata,
    name = "prediction_error_plot"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  prediction_summary <- attr(p, "gp3_model_prediction_prediction_summary")

  expect_equal(overview$prediction_status, "prediction_error")
  expect_false(is.na(overview$prediction_error))
  expect_equal(nrow(prediction_summary), 0)
})

test_that("plot_gazepoint_model_predictions can hide observed summaries or intervals", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  p <- plot_gazepoint_model_predictions(
    data = toy_data,
    model = model,
    x_col = "time",
    outcome_col = "outcome",
    condition_col = "condition",
    show_observed = FALSE,
    show_observed_ci = FALSE,
    show_prediction_ci = FALSE,
    name = "hidden_observed_plot"
  )

  expect_s3_class(p, "gp3_model_prediction_plot")

  overview <- attr(p, "gp3_model_prediction_overview")
  settings <- attr(p, "gp3_model_prediction_settings")

  expect_equal(overview$prediction_status, "complete")
  expect_equal(settings$value[settings$setting == "show_observed"], "FALSE")
  expect_equal(settings$value[settings$setting == "show_observed_ci"], "FALSE")
  expect_equal(settings$value[settings$setting == "show_prediction_ci"], "FALSE")
})

test_that("plot_gazepoint_model_predictions checks invalid inputs", {
  toy_data <- make_test_prediction_data()

  model <- stats::lm(
    outcome ~ condition * time,
    data = toy_data
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = list(),
      model = model,
      x_col = "time",
      outcome_col = "outcome"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data[0, ],
      model = model,
      x_col = "time",
      outcome_col = "outcome"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "bad_time",
      outcome_col = "outcome"
    ),
    "`x_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "bad_outcome"
    ),
    "`outcome_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      group_cols = "bad_group"
    ),
    "`group_cols` contains column(s) not present in the relevant data",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      newdata = list()
    ),
    "`newdata` must be NULL or a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      newdata = toy_data[0, ]
    ),
    "`newdata` must contain at least one row",
    fixed = TRUE
  )

  bad_newdata <- tibble::tibble(condition = "control")

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      condition_col = "condition",
      newdata = bad_newdata
    ),
    "`x_col in prediction data` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      ci = 1
    ),
    "`ci` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      show_observed = NA
    ),
    "`show_observed` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      point_alpha = -0.1
    ),
    "`point_alpha` must be a finite non-negative number",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      line_width = 0
    ),
    "`line_width` must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_model_predictions(
      data = toy_data,
      model = model,
      x_col = "time",
      outcome_col = "outcome",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
