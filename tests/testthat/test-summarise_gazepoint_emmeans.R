test_that("summarise_gazepoint_emmeans summarises lm models when emmeans is available", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 1.3, 2.1, 3.1, 4.2, 5.2, 6.1),
    condition = factor(rep(c("A", "B"), each = 6)),
    x = rep(c(1, 2, 3, 4, 5, 6), times = 2)
  )

  mod <- stats::lm(y ~ condition, data = dat)

  out <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    model_name = "lm_emm",
    type = "response"
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$emmeans, "tbl_df")
  expect_s3_class(out$contrasts, "tbl_df")

  expect_equal(out$overview$model_name, "lm_emm")
  expect_equal(out$overview$emmeans_status, "ok")
  expect_equal(out$overview$contrasts_status, "ok")
  expect_equal(out$overview$summary_status, "ok")
  expect_equal(nrow(out$emmeans), 2)
  expect_true(nrow(out$contrasts) >= 1)
  expect_true(all(out$emmeans$diagnostic_status == "ok"))
  expect_true(all(out$contrasts$diagnostic_status == "ok"))
})

test_that("summarise_gazepoint_emmeans can skip contrasts", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 1.3, 2.1, 3.1, 4.2, 5.2, 6.1),
    condition = factor(rep(c("A", "B"), each = 6))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  out <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    include_contrasts = FALSE
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_equal(out$overview$emmeans_status, "ok")
  expect_equal(out$overview$contrasts_status, "skipped_disabled")
  expect_equal(out$contrasts$diagnostic_status, "skipped_disabled")
})

test_that("summarise_gazepoint_emmeans works with glm models", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0),
    condition = factor(rep(c("A", "B"), each = 6))
  )

  mod <- stats::glm(y ~ condition, data = dat, family = stats::binomial())

  out <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    model_name = "glm_emm",
    type = "response"
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_equal(out$overview$model_name, "glm_emm")
  expect_equal(out$overview$emmeans_status, "ok")
  expect_true(nrow(out$emmeans) == 2)
  expect_true(all(is.finite(out$emmeans$estimate)))
})

test_that("summarise_gazepoint_emmeans extracts model from gp3tools fit object", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 1.3, 2.1, 3.1, 4.2, 5.2, 6.1),
    condition = factor(rep(c("A", "B"), each = 6))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  fit <- list(
    model = mod,
    model_name = "wrapped_lm"
  )

  out <- summarise_gazepoint_emmeans(
    fit,
    specs = "condition"
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_equal(out$overview$model_name, "wrapped_lm")
  expect_equal(out$emmeans$model_name[[1]], "wrapped_lm")
})

test_that("summarise_gazepoint_emmeans handles by groups", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(
      1.1, 1.9, 3.2, 3.8,
      1.3, 2.1, 3.1, 4.2,
      2.1, 2.9, 4.2, 4.8,
      2.3, 3.1, 4.1, 5.2
    ),
    condition = factor(rep(c("A", "B"), each = 8)),
    window = factor(rep(c("early", "late"), times = 8))
  )

  mod <- stats::lm(y ~ condition * window, data = dat)

  out <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    by = "window",
    model_name = "by_lm"
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_equal(out$overview$by, "window")
  expect_equal(out$overview$emmeans_status, "ok")
  expect_true(nrow(out$emmeans) >= 4)
})

test_that("summarise_gazepoint_emmeans reports emmeans errors cleanly", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8),
    condition = factor(c("A", "A", "B", "B"))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  out <- summarise_gazepoint_emmeans(
    mod,
    specs = "not_a_model_term",
    model_name = "bad_specs"
  )

  expect_s3_class(out, "gp3_emmeans_summary")
  expect_equal(out$overview$emmeans_status, "error")
  expect_equal(out$overview$summary_status, "error")
  expect_equal(out$emmeans$diagnostic_status, "error")
})

test_that("summarise_gazepoint_emmeans checks invalid inputs", {
  expect_error(
    summarise_gazepoint_emmeans(NULL, specs = "condition"),
    "`model` must not be NULL",
    fixed = TRUE
  )

  dat <- tibble::tibble(
    y = c(1, 2, 3, 4),
    condition = factor(c("A", "A", "B", "B"))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  expect_error(
    summarise_gazepoint_emmeans(mod),
    "`specs` must be supplied",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(data.frame(x = 1), specs = "condition"),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(1:3, specs = "condition"),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(list(model = NULL), specs = "condition"),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", by = NA_character_),
    "`by` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", type = NA_character_),
    "`type` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", contrast_method = NA_character_),
    "`contrast_method` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", adjust = NA_character_),
    "`adjust` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", conf_level = 1),
    "`conf_level` must be a finite numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_emmeans(mod, specs = "condition", include_contrasts = NA),
    "`include_contrasts` must be TRUE or FALSE",
    fixed = TRUE
  )
})
