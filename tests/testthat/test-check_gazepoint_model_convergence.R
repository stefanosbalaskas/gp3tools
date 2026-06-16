test_that("check_gazepoint_model_convergence checks glm convergence", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- check_gazepoint_model_convergence(mod, model_name = "glm_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$model_name, "glm_test")
  expect_equal(out$diagnostic, "convergence")
  expect_true(out$converged)
  expect_equal(out$diagnostic_status, "ok")
  expect_true(grepl("glm", out$model_class))
})

test_that("check_gazepoint_model_convergence detects glm non-convergence flag", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())
  mod$converged <- FALSE

  out <- check_gazepoint_model_convergence(mod)

  expect_false(out$converged)
  expect_equal(out$diagnostic_status, "convergence_warning")
})

test_that("check_gazepoint_model_convergence handles lm as not applicable", {
  dat <- tibble::tibble(
    y = c(1, 2, 3, 4, 5),
    x = c(1, 2, 3, 4, 5)
  )

  mod <- stats::lm(y ~ x, data = dat)

  out <- check_gazepoint_model_convergence(mod, model_name = "lm_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "lm_test")
  expect_equal(out$diagnostic_status, "not_applicable")
  expect_true(is.na(out$converged))
})

test_that("check_gazepoint_model_convergence checks lme4 models when available", {
  testthat::skip_if_not_installed("lme4")

  set.seed(123)

  dat <- tibble::tibble(
    subject = factor(rep(paste0("S", 1:10), each = 5)),
    x = rep(seq(-1, 1, length.out = 5), times = 10)
  )

  subject_effect <- stats::rnorm(10, 0, 0.5)
  dat$y <- 1 + 0.4 * dat$x + subject_effect[as.integer(dat$subject)] +
    stats::rnorm(nrow(dat), 0, 0.2)

  mod <- lme4::lmer(y ~ x + (1 | subject), data = dat)

  out <- check_gazepoint_model_convergence(mod, model_name = "lmer_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "lmer_test")
  expect_equal(out$diagnostic, "convergence")
  expect_true(out$diagnostic_status %in% c("ok", "convergence_warning"))
  expect_true(is.logical(out$converged) || is.na(out$converged))
  expect_true(grepl("merMod", out$model_class))
})

test_that("check_gazepoint_model_convergence checks mgcv models when available", {
  testthat::skip_if_not_installed("mgcv")

  dat <- tibble::tibble(
    y = sin(seq(0, 2 * pi, length.out = 60)) + stats::rnorm(60, 0, 0.1),
    time = seq(0, 1000, length.out = 60)
  )

  mod <- mgcv::gam(y ~ s(time, k = 6), data = dat, method = "REML")

  out <- check_gazepoint_model_convergence(mod, model_name = "gam_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "gam_test")
  expect_equal(out$diagnostic, "convergence")
  expect_true(out$diagnostic_status %in% c("ok", "not_available", "convergence_warning"))
  expect_true(grepl("gam", out$model_class))
})

test_that("check_gazepoint_model_convergence extracts model from gp3tools fit object", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  fit <- list(
    model = mod,
    model_name = "wrapped_glm"
  )

  out <- check_gazepoint_model_convergence(fit)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "wrapped_glm")
  expect_true(out$converged)
  expect_equal(out$diagnostic_status, "ok")
})

test_that("check_gazepoint_model_convergence reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- check_gazepoint_model_convergence(mod, model_name = "unsupported")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "unsupported")
  expect_equal(out$diagnostic_status, "unsupported_model_class")
  expect_true(is.na(out$converged))
})

test_that("check_gazepoint_model_convergence checks invalid inputs", {
  expect_error(
    check_gazepoint_model_convergence(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_convergence(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_convergence(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_convergence(
      list(model = NULL)
    ),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  dat <- tibble::tibble(
    y = c(0, 1, 0, 1),
    x = c(0, 0, 1, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  expect_error(
    check_gazepoint_model_convergence(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
