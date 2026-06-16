make_test_gam_for_diagnostics <- function() {
  testthat::skip_if_not_installed("mgcv")

  set.seed(123)

  dat <- tibble::tibble(
    time = seq(0, 1, length.out = 80)
  )

  dat$y <- sin(dat$time * 2 * pi) + stats::rnorm(nrow(dat), 0, 0.1)

  mgcv::gam(
    y ~ s(time, k = 6),
    data = dat,
    method = "REML"
  )
}

make_test_binomial_gam_for_diagnostics <- function() {
  testthat::skip_if_not_installed("mgcv")

  set.seed(123)

  dat <- tibble::tibble(
    time = seq(0, 1, length.out = 80)
  )

  eta <- -0.5 + sin(dat$time * 2 * pi)
  dat$y <- stats::rbinom(nrow(dat), size = 1, prob = stats::plogis(eta))

  mgcv::gam(
    y ~ s(time, k = 6),
    data = dat,
    family = stats::binomial(),
    method = "REML"
  )
}

test_that("diagnose_gazepoint_gamm diagnoses mgcv gam objects", {
  mod <- make_test_gam_for_diagnostics()

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "gam_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_true(is.list(out))

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$convergence, "tbl_df")
  expect_s3_class(out$basis, "tbl_df")
  expect_s3_class(out$overdispersion, "tbl_df")
  expect_s3_class(out$dharma, "tbl_df")

  expect_equal(nrow(out$overview), 1)
  expect_equal(out$overview$model_name, "gam_test")
  expect_true(out$overview$diagnostic_status %in% c("ok", "diagnostic_warning"))
  expect_true(out$overview$converged)
  expect_true(out$overview$basis_status %in% c("ok", "basis_warning"))
  expect_true(is.na(out$overview$overdispersed))
  expect_equal(out$overview$dharma_status, "skipped_disabled")

  expect_equal(out$convergence$diagnostic_status, "ok")
  expect_true(all(out$basis$diagnostic_status %in% c("ok", "basis_warning")))
  expect_equal(out$overdispersion$diagnostic_status, "not_applicable")
  expect_equal(out$dharma$diagnostic_status, "skipped_disabled")
})

test_that("diagnose_gazepoint_gamm diagnoses binomial mgcv gam objects", {
  mod <- make_test_binomial_gam_for_diagnostics()

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "binomial_gam_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "binomial_gam_test")
  expect_true(out$overview$basis_status %in% c("ok", "basis_warning"))
  expect_true(is.logical(out$overview$overdispersed))
  expect_true(out$overdispersion$diagnostic_status %in% c("ok", "overdispersed"))
})

test_that("diagnose_gazepoint_gamm extracts model from gp3tools fit objects", {
  mod <- make_test_gam_for_diagnostics()

  fit <- list(
    model = mod,
    model_name = "wrapped_gam"
  )

  out <- diagnose_gazepoint_gamm(
    fit,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "wrapped_gam")
  expect_true(out$overview$converged)
  expect_equal(out$dharma$dharma_status, "skipped_disabled")
})

test_that("diagnose_gazepoint_gamm handles named lists of models", {
  mod_1 <- make_test_gam_for_diagnostics()
  mod_2 <- make_test_binomial_gam_for_diagnostics()

  out <- diagnose_gazepoint_gamm(
    list(gaussian = mod_1, binomial = mod_2),
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(nrow(out$overview), 2)
  expect_equal(sort(out$overview$model_name), c("binomial", "gaussian"))
  expect_equal(nrow(out$convergence), 2)
  expect_true(nrow(out$basis) >= 2)
  expect_equal(nrow(out$overdispersion), 2)
  expect_equal(nrow(out$dharma), 2)
  expect_equal(out$settings$n_models, 2)
})

test_that("diagnose_gazepoint_gamm can disable individual diagnostics", {
  mod <- make_test_gam_for_diagnostics()

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "gam_disabled",
    check_convergence = FALSE,
    check_basis = FALSE,
    check_overdispersion = FALSE,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$convergence$diagnostic_status, "skipped_disabled")
  expect_equal(out$basis$diagnostic_status, "skipped_disabled")
  expect_equal(out$overdispersion$diagnostic_status, "skipped_disabled")
  expect_equal(out$dharma$diagnostic_status, "skipped_disabled")
  expect_equal(out$overview$diagnostic_status, "ok")
})

test_that("diagnose_gazepoint_gamm handles optional DHARMa when requested", {
  mod <- make_test_binomial_gam_for_diagnostics()

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "gam_dharma_optional",
    use_dharma = TRUE,
    dharma_simulations = 20,
    seed = 123
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_s3_class(out$dharma, "tbl_df")
  expect_equal(out$dharma$diagnostic, "dharma")
  expect_true(out$dharma$dharma_status %in% c(
    "ok",
    "diagnostic_warning",
    "partial_error",
    "error",
    "skipped_missing_package"
  ))
})

test_that("diagnose_gazepoint_gamm handles mgcv bam objects", {
  testthat::skip_if_not_installed("mgcv")

  set.seed(123)

  dat <- tibble::tibble(
    time = seq(0, 1, length.out = 100)
  )

  dat$y <- sin(dat$time * 2 * pi) + stats::rnorm(nrow(dat), 0, 0.1)

  mod <- mgcv::bam(
    y ~ s(time, k = 6),
    data = dat,
    method = "REML"
  )

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "bam_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "bam_test")
  expect_true(out$overview$converged)
  expect_true(out$overview$basis_status %in% c("ok", "basis_warning"))
})

test_that("diagnose_gazepoint_gamm reports non-gam model classes as not applicable or unsupported", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1),
    x = c(0, 0, 1, 1, 0, 1)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "glm_in_gamm_diagnostics",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "glm_in_gamm_diagnostics")
  expect_equal(out$basis$diagnostic_status, "not_applicable")
  expect_true(out$convergence$diagnostic_status %in% c("ok", "convergence_warning"))
})

test_that("diagnose_gazepoint_gamm reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- diagnose_gazepoint_gamm(
    mod,
    model_name = "unsupported",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "unsupported")
  expect_equal(out$overview$diagnostic_status, "error")
  expect_equal(out$convergence$diagnostic_status, "unsupported_model_class")
  expect_equal(out$basis$diagnostic_status, "not_applicable")
  expect_equal(out$overdispersion$diagnostic_status, "unsupported_model_class")
})

test_that("diagnose_gazepoint_gamm checks invalid inputs", {
  expect_error(
    diagnose_gazepoint_gamm(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(list(model = NULL)),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  mod <- make_test_gam_for_diagnostics()

  expect_error(
    diagnose_gazepoint_gamm(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, check_convergence = NA),
    "`check_convergence` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, check_basis = NA),
    "`check_basis` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, check_overdispersion = NA),
    "`check_overdispersion` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, use_dharma = NA),
    "`use_dharma` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, dharma_simulations = 0),
    "`dharma_simulations` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_gamm(mod, seed = NA_real_),
    "`seed` must be a finite numeric scalar",
    fixed = TRUE
  )
})
