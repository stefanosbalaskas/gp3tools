test_that("tidy_gazepoint_model_summary summarises glm models", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "glm_test",
    exponentiate = TRUE,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_true(is.list(out))

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$model_info, "tbl_df")
  expect_s3_class(out$fixed_effects, "tbl_df")
  expect_true(is.list(out$diagnostics))

  expect_equal(out$overview$model_name, "glm_test")
  expect_equal(out$model_info$model_name, "glm_test")
  expect_equal(out$model_info$model_family, "binomial")
  expect_equal(out$model_info$model_link, "logit")
  expect_true(out$overview$n_fixed_effects >= 1)
  expect_equal(out$overview$fixed_effects_status, "ok")
  expect_true(out$overview$summary_status %in% c("ok", "diagnostic_warning"))

  expect_true("(Intercept)" %in% out$fixed_effects$term)
  expect_true("x" %in% out$fixed_effects$term)
  expect_equal(unique(out$fixed_effects$response_scale), "exponentiated")
})

test_that("tidy_gazepoint_model_summary can disable diagnostics", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "glm_no_diag",
    include_diagnostics = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "glm_no_diag")
  expect_equal(out$overview$fixed_effects_status, "ok")
  expect_equal(out$overview$diagnostics_status, "skipped_disabled")
  expect_equal(out$overview$summary_status, "ok")
  expect_equal(out$diagnostics$overview$diagnostic_status, "skipped_disabled")
})

test_that("tidy_gazepoint_model_summary extracts model from gp3tools fit object", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  fit <- list(
    model = mod,
    model_name = "wrapped_glm"
  )

  out <- tidy_gazepoint_model_summary(
    fit,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "wrapped_glm")
  expect_equal(out$model_info$model_name, "wrapped_glm")
  expect_equal(out$fixed_effects$model_name[[1]], "wrapped_glm")
})

test_that("tidy_gazepoint_model_summary summarises lm models", {
  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9),
    x = c(1, 2, 3, 4, 5, 6)
  )

  mod <- stats::lm(y ~ x, data = dat)

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "lm_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "lm_test")
  expect_true(out$model_info$model_family %in% c("gaussian", NA_character_))
  expect_true(out$model_info$model_link %in% c("identity", NA_character_))
  expect_equal(out$overview$fixed_effects_status, "ok")
  expect_true("x" %in% out$fixed_effects$term)
})

test_that("tidy_gazepoint_model_summary summarises lme4 models when available", {
  testthat::skip_if_not_installed("lme4")

  set.seed(123)

  dat <- tibble::tibble(
    subject = factor(rep(paste0("S", 1:10), each = 5)),
    x = rep(seq(-1, 1, length.out = 5), times = 10)
  )

  subject_effect <- stats::rnorm(10, 0, 0.5)

  dat$y <- 1 +
    0.4 * dat$x +
    subject_effect[as.integer(dat$subject)] +
    stats::rnorm(nrow(dat), 0, 0.2)

  mod <- suppressWarnings(
    suppressMessages(
      lme4::lmer(y ~ x + (1 | subject), data = dat)
    )
  )

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "lmer_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "lmer_test")
  expect_equal(out$overview$fixed_effects_status, "ok")
  expect_true(out$overview$summary_status %in% c("ok", "diagnostic_warning"))
  expect_true("x" %in% out$fixed_effects$term)
})

test_that("tidy_gazepoint_model_summary summarises mgcv gam models when available", {
  testthat::skip_if_not_installed("mgcv")

  set.seed(123)

  dat <- tibble::tibble(
    time = seq(0, 1, length.out = 80)
  )

  dat$y <- sin(dat$time * 2 * pi) + stats::rnorm(nrow(dat), 0, 0.1)

  mod <- mgcv::gam(
    y ~ s(time, k = 6),
    data = dat,
    method = "REML"
  )

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "gam_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "gam_test")
  expect_equal(out$overview$fixed_effects_status, "ok")
  expect_true(out$overview$diagnostics_status %in% c("ok", "diagnostic_warning"))
  expect_true("(Intercept)" %in% out$fixed_effects$term)
  expect_s3_class(out$diagnostics$basis, "tbl_df")
})

test_that("tidy_gazepoint_model_summary reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- tidy_gazepoint_model_summary(
    mod,
    model_name = "unsupported"
  )

  expect_s3_class(out, "gp3_model_summary")
  expect_equal(out$overview$model_name, "unsupported")
  expect_equal(out$overview$fixed_effects_status, "error")
  expect_equal(out$overview$summary_status, "error")
  expect_equal(out$fixed_effects$diagnostic_status[[1]], "unsupported_model_class")
})

test_that("tidy_gazepoint_model_summary checks invalid inputs", {
  expect_error(
    tidy_gazepoint_model_summary(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(list(model = NULL)),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8),
    x = c(1, 2, 3, 4)
  )

  mod <- stats::lm(y ~ x, data = dat)

  expect_error(
    tidy_gazepoint_model_summary(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, conf_level = 1),
    "`conf_level` must be a finite numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, exponentiate = NA),
    "`exponentiate` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, drop_intercept = NA),
    "`drop_intercept` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, include_diagnostics = NA),
    "`include_diagnostics` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, use_dharma = NA),
    "`use_dharma` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, dharma_simulations = 0),
    "`dharma_simulations` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    tidy_gazepoint_model_summary(mod, seed = NA_real_),
    "`seed` must be a finite numeric scalar",
    fixed = TRUE
  )
})
