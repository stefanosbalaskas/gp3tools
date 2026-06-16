test_that("summarise_gazepoint_fixed_effects summarises lm models", {
  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9),
    x = c(1, 2, 3, 4, 5, 6)
  )

  mod <- stats::lm(y ~ x, data = dat)

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "lm_test"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "lm_test")
  expect_true(all(c(
    "model_name",
    "model_class",
    "term",
    "estimate",
    "std_error",
    "statistic",
    "statistic_type",
    "df",
    "p_value",
    "conf_low",
    "conf_high",
    "response_scale",
    "significance",
    "diagnostic_status",
    "message"
  ) %in% names(out)))
  expect_true("(Intercept)" %in% out$term)
  expect_true("x" %in% out$term)
  expect_equal(unique(out$diagnostic_status), "ok")
  expect_equal(unique(out$statistic_type), "t")
  expect_true(all(is.finite(out$estimate)))
})

test_that("summarise_gazepoint_fixed_effects can drop intercept", {
  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9),
    x = c(1, 2, 3, 4, 5, 6)
  )

  mod <- stats::lm(y ~ x, data = dat)

  out <- summarise_gazepoint_fixed_effects(
    mod,
    drop_intercept = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_false("(Intercept)" %in% out$term)
  expect_true("x" %in% out$term)
})

test_that("summarise_gazepoint_fixed_effects summarises glm models", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "glm_test"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "glm_test")
  expect_true("(Intercept)" %in% out$term)
  expect_true("x" %in% out$term)
  expect_equal(unique(out$diagnostic_status), "ok")
  expect_equal(unique(out$statistic_type), "z")
  expect_true(all(is.finite(out$estimate)))
})

test_that("summarise_gazepoint_fixed_effects can exponentiate glm estimates", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  raw <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "glm_raw",
    exponentiate = FALSE
  )

  exp_out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "glm_exp",
    exponentiate = TRUE
  )

  expect_s3_class(exp_out, "tbl_df")
  expect_equal(unique(exp_out$response_scale), "exponentiated")
  expect_equal(
    exp_out$estimate,
    exp(raw$estimate),
    tolerance = 1e-8
  )
})

test_that("summarise_gazepoint_fixed_effects extracts model from gp3tools fit object", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  fit <- list(
    model = mod,
    model_name = "wrapped_glm"
  )

  out <- summarise_gazepoint_fixed_effects(fit)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "wrapped_glm")
  expect_equal(unique(out$diagnostic_status), "ok")
})

test_that("summarise_gazepoint_fixed_effects summarises lme4 lmer models when available", {
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

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "lmer_test"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "lmer_test")
  expect_true("(Intercept)" %in% out$term)
  expect_true("x" %in% out$term)
  expect_equal(unique(out$diagnostic_status), "ok")
  expect_true(all(is.finite(out$estimate)))
})

test_that("summarise_gazepoint_fixed_effects summarises lme4 glmer models when available", {
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

  mod <- suppressWarnings(
    suppressMessages(
      lme4::glmer(
        y ~ x + (1 | subject),
        data = dat,
        family = stats::binomial()
      )
    )
  )

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "glmer_test",
    exponentiate = TRUE
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "glmer_test")
  expect_true("(Intercept)" %in% out$term)
  expect_true("x" %in% out$term)
  expect_equal(unique(out$diagnostic_status), "ok")
  expect_equal(unique(out$response_scale), "exponentiated")
})

test_that("summarise_gazepoint_fixed_effects summarises mgcv gam models when available", {
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

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "gam_test"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "gam_test")
  expect_true("(Intercept)" %in% out$term)
  expect_equal(unique(out$diagnostic_status), "ok")
  expect_true(all(is.finite(out$estimate)))
})

test_that("summarise_gazepoint_fixed_effects reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- summarise_gazepoint_fixed_effects(
    mod,
    model_name = "unsupported"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$model_name[[1]], "unsupported")
  expect_equal(out$diagnostic_status[[1]], "unsupported_model_class")
  expect_true(is.na(out$term[[1]]))
})

test_that("summarise_gazepoint_fixed_effects checks invalid inputs", {
  expect_error(
    summarise_gazepoint_fixed_effects(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(list(model = NULL)),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  dat <- tibble::tibble(
    y = c(1, 2, 3, 4),
    x = c(1, 2, 3, 4)
  )

  mod <- stats::lm(y ~ x, data = dat)

  expect_error(
    summarise_gazepoint_fixed_effects(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(mod, conf_level = 1),
    "`conf_level` must be a finite numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(mod, exponentiate = NA),
    "`exponentiate` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixed_effects(mod, drop_intercept = NA),
    "`drop_intercept` must be TRUE or FALSE",
    fixed = TRUE
  )
})
