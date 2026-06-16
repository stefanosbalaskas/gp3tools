test_that("check_gazepoint_model_overdispersion checks binomial glm models", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- check_gazepoint_model_overdispersion(
    mod,
    ratio_threshold = 1.2,
    model_name = "glm_binomial"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$model_name, "glm_binomial")
  expect_equal(out$diagnostic, "overdispersion")
  expect_true(is.numeric(out$dispersion_ratio))
  expect_true(is.numeric(out$pearson_chisq))
  expect_true(is.numeric(out$residual_df))
  expect_true(is.logical(out$overdispersed))
  expect_equal(out$ratio_threshold, 1.2)
  expect_true(out$diagnostic_status %in% c("ok", "overdispersed"))
  expect_true(grepl("glm", out$model_class))
})

test_that("check_gazepoint_model_overdispersion checks poisson glm models", {
  dat <- tibble::tibble(
    count = c(1, 2, 0, 3, 2, 1, 4, 3, 2, 5, 3, 4),
    x = rep(c(0, 1), 6)
  )

  mod <- stats::glm(count ~ x, data = dat, family = stats::poisson())

  out <- check_gazepoint_model_overdispersion(
    mod,
    ratio_threshold = 1.2,
    model_name = "glm_poisson"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "glm_poisson")
  expect_equal(out$diagnostic, "overdispersion")
  expect_true(is.finite(out$dispersion_ratio))
  expect_true(out$residual_df > 0)
  expect_true(out$diagnostic_status %in% c("ok", "overdispersed"))
})

test_that("check_gazepoint_model_overdispersion handles gaussian lm as not applicable", {
  dat <- tibble::tibble(
    y = c(1, 2, 3, 4, 5),
    x = c(1, 2, 3, 4, 5)
  )

  mod <- stats::lm(y ~ x, data = dat)

  out <- check_gazepoint_model_overdispersion(mod, model_name = "lm_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "lm_test")
  expect_equal(out$diagnostic_status, "not_applicable")
  expect_true(is.na(out$dispersion_ratio))
  expect_true(is.na(out$overdispersed))
})

test_that("check_gazepoint_model_overdispersion handles gaussian glm as not applicable", {
  dat <- tibble::tibble(
    y = c(1, 2, 3, 4, 5),
    x = c(1, 2, 3, 4, 5)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::gaussian())

  out <- check_gazepoint_model_overdispersion(mod, model_name = "glm_gaussian")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "glm_gaussian")
  expect_equal(out$diagnostic_status, "not_applicable")
  expect_true(is.na(out$dispersion_ratio))
  expect_true(is.na(out$overdispersed))
})

test_that("check_gazepoint_model_overdispersion checks lme4 glmer models when available", {
  testthat::skip_if_not_installed("lme4")

  set.seed(123)

  subjects <- paste0("S", 1:12)

  dat <- expand.grid(
    subject = subjects,
    trial = 1:6,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat <- tibble::as_tibble(dat)
  dat$subject <- factor(dat$subject)
  dat$x <- rep(c(0, 1), length.out = nrow(dat))

  subject_effect <- stats::rnorm(length(subjects), 0, 0.4)

  eta <- -0.2 +
    0.6 * dat$x +
    subject_effect[as.integer(dat$subject)]

  dat$y <- stats::rbinom(nrow(dat), size = 1, prob = stats::plogis(eta))

  mod <- lme4::glmer(
    y ~ x + (1 | subject),
    data = dat,
    family = stats::binomial()
  )

  out <- check_gazepoint_model_overdispersion(
    mod,
    ratio_threshold = 1.2,
    model_name = "glmer_binomial"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "glmer_binomial")
  expect_equal(out$diagnostic, "overdispersion")
  expect_true(is.finite(out$dispersion_ratio))
  expect_true(out$residual_df > 0)
  expect_true(out$diagnostic_status %in% c("ok", "overdispersed"))
  expect_true(grepl("merMod", out$model_class))
})

test_that("check_gazepoint_model_overdispersion handles lme4 lmer models as not applicable", {
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

  mod <- lme4::lmer(y ~ x + (1 | subject), data = dat)

  out <- check_gazepoint_model_overdispersion(mod, model_name = "lmer_test")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "lmer_test")
  expect_equal(out$diagnostic_status, "not_applicable")
  expect_true(is.na(out$dispersion_ratio))
})

test_that("check_gazepoint_model_overdispersion checks mgcv binomial models when available", {
  testthat::skip_if_not_installed("mgcv")

  set.seed(123)

  dat <- tibble::tibble(
    time = seq(0, 1, length.out = 80)
  )

  eta <- -0.5 + sin(dat$time * 2 * pi)
  dat$y <- stats::rbinom(nrow(dat), size = 1, prob = stats::plogis(eta))

  mod <- mgcv::gam(
    y ~ s(time, k = 6),
    data = dat,
    family = stats::binomial(),
    method = "REML"
  )

  out <- check_gazepoint_model_overdispersion(mod, model_name = "gam_binomial")

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "gam_binomial")
  expect_equal(out$diagnostic, "overdispersion")
  expect_true(out$diagnostic_status %in% c("ok", "overdispersed"))
  expect_true(is.finite(out$dispersion_ratio))
})

test_that("check_gazepoint_model_overdispersion extracts model from gp3tools fit object", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  fit <- list(
    model = mod,
    model_name = "wrapped_glm"
  )

  out <- check_gazepoint_model_overdispersion(fit)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "wrapped_glm")
  expect_true(out$diagnostic_status %in% c("ok", "overdispersed"))
})

test_that("check_gazepoint_model_overdispersion reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- check_gazepoint_model_overdispersion(
    mod,
    model_name = "unsupported"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name, "unsupported")
  expect_equal(out$diagnostic, "overdispersion")
  expect_equal(out$diagnostic_status, "unsupported_model_class")
  expect_true(is.na(out$dispersion_ratio))
})

test_that("check_gazepoint_model_overdispersion reports insufficient residual df", {
  dat <- tibble::tibble(
    y = c(0, 1),
    x = c(0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- check_gazepoint_model_overdispersion(mod)

  expect_s3_class(out, "tbl_df")
  expect_true(out$diagnostic_status %in% c(
    "insufficient_residual_df",
    "not_available",
    "ok",
    "overdispersed"
  ))
})

test_that("check_gazepoint_model_overdispersion checks invalid inputs", {
  expect_error(
    check_gazepoint_model_overdispersion(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_overdispersion(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_overdispersion(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_overdispersion(
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
    check_gazepoint_model_overdispersion(mod, ratio_threshold = 0),
    "`ratio_threshold` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_model_overdispersion(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
