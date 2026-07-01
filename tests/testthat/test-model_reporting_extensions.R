test_that("plot_gazepoint_time_varying_effect returns a ggplot", {
  d <- data.frame(
    time = 1:5,
    estimate = c(-0.2, -0.1, 0, 0.1, 0.2),
    lower = c(-0.3, -0.2, -0.1, 0, 0.1),
    upper = c(-0.1, 0, 0.1, 0.2, 0.3)
  )
  p <- plot_gazepoint_time_varying_effect(
    d, time_col = "time", estimate_col = "estimate",
    lower_col = "lower", upper_col = "upper"
  )
  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_model_residuals works with data frames", {
  d <- data.frame(fitted = c(1, 2, 3), residual = c(-0.1, 0.0, 0.1))
  p <- plot_gazepoint_model_residuals(data = d)
  expect_s3_class(p, "ggplot")
  q <- plot_gazepoint_model_residuals(data = d, type = "qq")
  expect_s3_class(q, "ggplot")
})

test_that("plot_gazepoint_model_residuals works with lm objects", {
  d <- data.frame(x = 1:10, y = 1:10 + c(-1, 1, -1, 1, -1, 1, -1, 1, -1, 1))
  fit <- stats::lm(y ~ x, data = d)
  p <- plot_gazepoint_model_residuals(model = fit)
  expect_s3_class(p, "ggplot")
})

test_that("report_gazepoint_multiverse summarises tidy data", {
  d <- data.frame(
    branch = c("a", "a", "b", "b"),
    term = c("x", "z", "x", "z"),
    estimate = c(0.2, -0.1, 0.3, -0.2),
    p.value = c(0.01, 0.20, 0.04, 0.50),
    status = c("ok", "ok", "ok", "ok")
  )
  out <- report_gazepoint_multiverse(d)
  expect_s3_class(out, "gp3_multiverse_report")
  expect_equal(nrow(out$branch_summary), 2)
  expect_equal(nrow(out$term_summary), 2)
  expect_true(out$term_summary$prop_significant[out$term_summary$term == "x"] == 1)
})

test_that("report_gazepoint_multiverse accepts lists of data frames", {
  x <- list(
    branch_a = data.frame(term = "x", estimate = 1, p.value = 0.01),
    branch_b = data.frame(term = "x", estimate = 2, p.value = 0.20)
  )
  out <- report_gazepoint_multiverse(x)
  expect_s3_class(out, "gp3_multiverse_report")
  expect_equal(nrow(out$branch_summary), 2)
})
