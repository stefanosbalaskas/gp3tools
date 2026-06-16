make_test_cluster_plot_result <- function(effect = TRUE) {
  subjects <- paste0("S", 1:12)
  conditions <- c("control", "treatment")
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
  trial_num <- as.numeric(factor(dat$trial_global))

  dat$cluster_window <- dat$time >= 300 & dat$time <= 550
  dat$subject_shift <- subject_num * 0.01
  dat$time_shift <- dat$time / 10000

  subject_cluster_effect <- 0.34 + subject_num * 0.01

  dat$outcome <- dat$subject_shift +
    dat$time_shift +
    trial_num * 0.002 +
    ifelse(
      effect & dat$condition == "treatment" & dat$cluster_window,
      subject_cluster_effect,
      0
    )

  cluster_data <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "outcome",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 950),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE,
    outcome_label = "toy_outcome"
  )

  run_gazepoint_cluster_permutation(
    cluster_data,
    condition_order = c("control", "treatment"),
    n_permutations = 25,
    cluster_threshold = 2,
    tail = "two_sided",
    cluster_stat = "sum_abs_t",
    min_time_bins = 1,
    seed = 123
  )
}

test_that("plot_gazepoint_cluster_results returns a ggplot for both panels", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  p <- plot_gazepoint_cluster_results(
    result,
    plot_type = "both",
    alpha = 0.05
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(
    attr(p, "gp3_cluster_plot_settings")$plot_type,
    "both"
  )
  expect_true(attr(p, "gp3_cluster_plot_settings")$show_clusters)
  expect_true(attr(p, "gp3_cluster_plot_settings")$show_candidates)
  expect_true(attr(p, "gp3_cluster_plot_settings")$show_threshold)
})

test_that("plot_gazepoint_cluster_results supports difference and statistic panels", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  p_difference <- plot_gazepoint_cluster_results(
    result,
    plot_type = "difference",
    alpha = 0.05
  )

  p_statistic <- plot_gazepoint_cluster_results(
    result,
    plot_type = "statistic",
    alpha = 0.05
  )

  expect_true(inherits(p_difference, "ggplot"))
  expect_true(inherits(p_statistic, "ggplot"))

  expect_equal(
    attr(p_difference, "gp3_cluster_plot_settings")$plot_type,
    "difference"
  )

  expect_equal(
    attr(p_statistic, "gp3_cluster_plot_settings")$plot_type,
    "statistic"
  )
})

test_that("plot_gazepoint_cluster_results supports custom labels", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  p <- plot_gazepoint_cluster_results(
    result,
    plot_type = "difference",
    title = "Custom title",
    subtitle = "Custom subtitle",
    x_label = "Custom time",
    y_label = "Custom difference"
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$labels$title, "Custom title")
  expect_equal(p$labels$subtitle, "Custom subtitle")
  expect_equal(p$labels$x, "Custom time")
  expect_equal(p$labels$y, "Custom difference")
})

test_that("plot_gazepoint_cluster_results handles no-cluster results", {
  result <- make_test_cluster_plot_result(effect = FALSE)

  p <- plot_gazepoint_cluster_results(
    result,
    plot_type = "both",
    alpha = 0.05
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(
    attr(p, "gp3_cluster_plot_settings")$plot_type,
    "both"
  )
})

test_that("plot_gazepoint_cluster_results can hide optional plot elements", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  p <- plot_gazepoint_cluster_results(
    result,
    plot_type = "statistic",
    show_clusters = FALSE,
    show_candidates = FALSE,
    show_threshold = FALSE,
    show_zero_line = FALSE
  )

  expect_true(inherits(p, "ggplot"))
  expect_false(attr(p, "gp3_cluster_plot_settings")$show_clusters)
  expect_false(attr(p, "gp3_cluster_plot_settings")$show_candidates)
  expect_false(attr(p, "gp3_cluster_plot_settings")$show_threshold)
  expect_false(attr(p, "gp3_cluster_plot_settings")$show_zero_line)
})

test_that("plot_gazepoint_cluster_results can show all clusters, not only significant clusters", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  p <- plot_gazepoint_cluster_results(
    result,
    plot_type = "both",
    significant_only = FALSE
  )

  expect_true(inherits(p, "ggplot"))
  expect_false(attr(p, "gp3_cluster_plot_settings")$significant_only)
})

test_that("plot_gazepoint_cluster_results checks result structure", {
  expect_error(
    plot_gazepoint_cluster_results(data.frame(x = 1)),
    "`result` is missing required element\\(s\\)"
  )

  result <- make_test_cluster_plot_result(effect = TRUE)
  result$timecourse <- NULL

  expect_error(
    plot_gazepoint_cluster_results(result),
    "`result` is missing required element\\(s\\): timecourse"
  )
})

test_that("plot_gazepoint_cluster_results checks timecourse columns", {
  result <- make_test_cluster_plot_result(effect = TRUE)
  result$timecourse$statistic <- NULL

  expect_error(
    plot_gazepoint_cluster_results(result),
    "`result\\$timecourse` is missing required column\\(s\\): statistic"
  )
})

test_that("plot_gazepoint_cluster_results checks cluster columns", {
  result <- make_test_cluster_plot_result(effect = TRUE)
  result$clusters$p_value <- NULL

  expect_error(
    plot_gazepoint_cluster_results(result),
    "`result\\$clusters` is missing required column\\(s\\): p_value"
  )
})

test_that("plot_gazepoint_cluster_results checks scalar arguments", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  expect_error(
    plot_gazepoint_cluster_results(result, alpha = 0),
    "`alpha` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, significant_only = NA),
    "`significant_only` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, show_clusters = NA),
    "`show_clusters` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, line_width = 0),
    "`line_width` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, point_size = 0),
    "`point_size` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, cluster_alpha = -0.1),
    "`cluster_alpha` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, cluster_alpha = 1.1),
    "`cluster_alpha` must be between 0 and 1",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_cluster_results checks label arguments", {
  result <- make_test_cluster_plot_result(effect = TRUE)

  expect_error(
    plot_gazepoint_cluster_results(result, title = NA_character_),
    "`title` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, subtitle = NA_character_),
    "`subtitle` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, x_label = ""),
    "`x_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_cluster_results(result, y_label = NA_character_),
    "`y_label` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )
})
