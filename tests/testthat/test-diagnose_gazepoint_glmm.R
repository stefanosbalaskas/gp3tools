make_test_glm_for_diagnostics <- function() {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  stats::glm(y ~ x, data = dat, family = stats::binomial())
}

test_that("diagnose_gazepoint_glmm diagnoses glm objects", {
  mod <- make_test_glm_for_diagnostics()

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "glm_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_true(is.list(out))

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$convergence, "tbl_df")
  expect_s3_class(out$singularity, "tbl_df")
  expect_s3_class(out$overdispersion, "tbl_df")
  expect_s3_class(out$dharma, "tbl_df")

  expect_equal(nrow(out$overview), 1)
  expect_equal(out$overview$model_name, "glm_test")
  expect_equal(out$overview$diagnostic_status, "ok")
  expect_true(out$overview$converged)
  expect_true(is.na(out$overview$singular_fit))
  expect_true(is.logical(out$overview$overdispersed))
  expect_equal(out$overview$dharma_status, "skipped_disabled")

  expect_equal(out$convergence$diagnostic_status, "ok")
  expect_equal(out$singularity$diagnostic_status, "not_applicable")
  expect_true(out$overdispersion$diagnostic_status %in% c("ok", "overdispersed"))
  expect_equal(out$dharma$diagnostic_status, "skipped_disabled")
})

test_that("diagnose_gazepoint_glmm extracts model from gp3tools fit objects", {
  mod <- make_test_glm_for_diagnostics()

  fit <- list(
    model = mod,
    model_name = "wrapped_glm"
  )

  out <- diagnose_gazepoint_glmm(
    fit,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "wrapped_glm")
  expect_true(out$overview$converged)
  expect_equal(out$dharma$dharma_status, "skipped_disabled")
})

test_that("diagnose_gazepoint_glmm handles named lists of models", {
  mod_1 <- make_test_glm_for_diagnostics()
  mod_2 <- make_test_glm_for_diagnostics()

  out <- diagnose_gazepoint_glmm(
    list(main = mod_1, sensitivity = mod_2),
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(nrow(out$overview), 2)
  expect_equal(sort(out$overview$model_name), c("main", "sensitivity"))
  expect_equal(nrow(out$convergence), 2)
  expect_equal(nrow(out$singularity), 2)
  expect_equal(nrow(out$overdispersion), 2)
  expect_equal(nrow(out$dharma), 2)
  expect_equal(out$settings$n_models, 2)
})

test_that("diagnose_gazepoint_glmm can disable individual diagnostics", {
  mod <- make_test_glm_for_diagnostics()

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "glm_disabled",
    check_convergence = FALSE,
    check_singularity = FALSE,
    check_overdispersion = FALSE,
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$convergence$diagnostic_status, "skipped_disabled")
  expect_equal(out$singularity$diagnostic_status, "skipped_disabled")
  expect_equal(out$overdispersion$diagnostic_status, "skipped_disabled")
  expect_equal(out$dharma$diagnostic_status, "skipped_disabled")
  expect_equal(out$overview$diagnostic_status, "ok")
})

test_that("diagnose_gazepoint_glmm handles optional DHARMa when requested", {
  mod <- make_test_glm_for_diagnostics()

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "glm_dharma_optional",
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

test_that("diagnose_gazepoint_glmm diagnoses lme4 glmer models when available", {
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

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "glmer_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "glmer_test")
  expect_true(out$convergence$diagnostic_status %in% c(
    "ok",
    "convergence_warning"
  ))
  expect_true(out$singularity$diagnostic_status %in% c(
    "ok",
    "singular_fit"
  ))
  expect_true(out$overdispersion$diagnostic_status %in% c(
    "ok",
    "overdispersed"
  ))
})

test_that("diagnose_gazepoint_glmm diagnoses lme4 lmer models when available", {
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

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "lmer_test",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "lmer_test")
  expect_true(out$singularity$diagnostic_status %in% c("ok", "singular_fit"))
  expect_equal(out$overdispersion$diagnostic_status, "not_applicable")
})

test_that("diagnose_gazepoint_glmm reports unsupported model classes", {
  mod <- structure(
    list(a = 1),
    class = "unsupported_test_model"
  )

  out <- diagnose_gazepoint_glmm(
    mod,
    model_name = "unsupported",
    use_dharma = FALSE
  )

  expect_s3_class(out, "gp3_model_diagnostics")
  expect_equal(out$overview$model_name, "unsupported")
  expect_equal(out$overview$diagnostic_status, "error")
  expect_equal(out$convergence$diagnostic_status, "unsupported_model_class")
  expect_equal(out$singularity$diagnostic_status, "unsupported_model_class")
  expect_equal(out$overdispersion$diagnostic_status, "unsupported_model_class")
})

test_that("diagnose_gazepoint_glmm checks invalid inputs", {
  expect_error(
    diagnose_gazepoint_glmm(NULL),
    "`model` must not be NULL",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(data.frame(x = 1)),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(1:3),
    "`model` must be a fitted model object or a gp3tools fit object",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(list(model = NULL)),
    "`model$model` must not be NULL",
    fixed = TRUE
  )

  mod <- make_test_glm_for_diagnostics()

  expect_error(
    diagnose_gazepoint_glmm(mod, model_name = NA_character_),
    "`model_name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, check_convergence = NA),
    "`check_convergence` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, check_singularity = NA),
    "`check_singularity` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, check_overdispersion = NA),
    "`check_overdispersion` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, use_dharma = NA),
    "`use_dharma` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, dharma_simulations = 0),
    "`dharma_simulations` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    diagnose_gazepoint_glmm(mod, seed = NA_real_),
    "`seed` must be a finite numeric scalar",
    fixed = TRUE
  )
})
