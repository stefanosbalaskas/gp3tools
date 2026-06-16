make_pupil_window_sensitivity_toy <- function() {
  subjects <- sprintf("S%02d", 1:12)
  conditions <- c("control", "treatment")
  windows <- c("0_500ms", "500_1000ms")

  dat <- expand.grid(
    subject = subjects,
    condition = conditions,
    window_label = windows,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat$window_start_ms <- ifelse(dat$window_label == "0_500ms", 0, 500)
  dat$window_end_ms <- ifelse(dat$window_label == "0_500ms", 500, 1000)
  dat$media_id <- rep(c("0", "1"), length.out = nrow(dat))
  dat$trial_global <- paste(dat$subject, dat$media_id, sep = "_M")

  subject_num <- as.numeric(factor(dat$subject))
  condition_effect <- ifelse(dat$condition == "treatment", 0.20, 0)
  window_effect <- ifelse(dat$window_label == "500_1000ms", -0.10, 0)
  interaction_effect <- ifelse(
    dat$condition == "treatment" &
      dat$window_label == "500_1000ms",
    0.05,
    0
  )

  dat$mean_pupil <- 0.50 +
    condition_effect +
    window_effect +
    interaction_effect +
    subject_num * 0.02

  dat$n_samples <- ifelse(dat$window_label == "0_500ms", 30, 60)
  dat$n_valid_pupil <- dat$n_samples - rep(c(0, 1, 0, 2), length.out = nrow(dat))
  dat$pupil_window_status <- "valid"

  prepare_gazepoint_pupil_window_model_data(
    tibble::as_tibble(dat),
    outcome_col = "mean_pupil",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    trial_col = "trial_global",
    media_col = "media_id",
    valid_samples_col = "n_valid_pupil",
    total_samples_col = "n_samples",
    min_valid_samples = 5,
    min_valid_prop = 0.70,
    outcome_label = "mean_pupil"
  )
}

test_that("fit_gazepoint_pupil_window_sensitivity fits all default model types", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(dat)

  expect_s3_class(out, "gp3_pupil_window_sensitivity")
  expect_equal(names(out$models), c("lmm", "weighted_lmm", "lm", "weighted_lm"))
  expect_equal(names(out$fits), c("lmm", "weighted_lmm", "lm", "weighted_lm"))
  expect_equal(names(out$formulas), c("lmm", "weighted_lmm", "lm", "weighted_lm"))

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_true(all(out$model_status_by_type %in% c("ok", "singular_fit")))

  expect_equal(nrow(out$comparison), 4)
  expect_true(all(c(
    "model_type",
    "model",
    "formula",
    "engine",
    "model_status",
    "final_model_status",
    "singular_fit",
    "fallback_used",
    "random_effect_structure",
    "n",
    "n_subjects",
    "n_conditions",
    "n_windows",
    "AIC",
    "BIC",
    "logLik",
    "warnings",
    "error_message"
  ) %in% names(out$comparison)))

  expect_true(nrow(out$fixed_effects) > 0)
  expect_true(all(c(
    "model_type",
    "term",
    "estimate",
    "std_error",
    "statistic",
    "p_value"
  ) %in% names(out$fixed_effects)))
})

test_that("fit_gazepoint_pupil_window_sensitivity uses expected engines", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(dat)

  engines <- stats::setNames(
    out$comparison$engine,
    out$comparison$model_type
  )

  expect_equal(engines[["lmm"]], "lme4::lmer")
  expect_equal(engines[["weighted_lmm"]], "lme4::lmer")
  expect_equal(engines[["lm"]], "stats::lm")
  expect_equal(engines[["weighted_lm"]], "stats::lm")
})

test_that("fit_gazepoint_pupil_window_sensitivity returns fitted model objects", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(dat)

  expect_true(inherits(out$models$lmm, "lmerMod"))
  expect_true(inherits(out$models$weighted_lmm, "lmerMod"))
  expect_s3_class(out$models$lm, "lm")
  expect_s3_class(out$models$weighted_lm, "lm")
})

test_that("fit_gazepoint_pupil_window_sensitivity respects selected model types", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lm", "weighted_lm")
  )

  expect_equal(names(out$models), c("lm", "weighted_lm"))
  expect_equal(nrow(out$comparison), 2)
  expect_true(all(out$comparison$model_type %in% c("lm", "weighted_lm")))
  expect_true(all(out$comparison$engine == "stats::lm"))
})

test_that("fit_gazepoint_pupil_window_sensitivity removes duplicate model types", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "lmm", "lm")
  )

  expect_equal(names(out$models), c("lmm", "lm"))
  expect_equal(nrow(out$comparison), 2)
})

test_that("fit_gazepoint_pupil_window_sensitivity handles single-condition fallback", {
  dat <- make_pupil_window_sensitivity_toy()
  dat$pupil_model_condition <- factor("all_data")

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "lm")
  )

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_true(all(grepl(".gp3_window", out$comparison$formula, fixed = TRUE)))
  expect_false(any(grepl(".gp3_condition", out$comparison$formula, fixed = TRUE)))
})

test_that("fit_gazepoint_pupil_window_sensitivity handles single-window fallback", {
  dat <- make_pupil_window_sensitivity_toy()
  dat <- dat[dat$pupil_model_window == "0_500ms", , drop = FALSE]
  dat$pupil_model_window <- droplevels(dat$pupil_model_window)

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "lm")
  )

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_true(all(grepl(".gp3_condition", out$comparison$formula, fixed = TRUE)))
  expect_false(any(grepl(".gp3_window", out$comparison$formula, fixed = TRUE)))
})

test_that("fit_gazepoint_pupil_window_sensitivity handles intercept-only fallback", {
  dat <- make_pupil_window_sensitivity_toy()
  dat <- dat[dat$pupil_model_window == "0_500ms", , drop = FALSE]
  dat$pupil_model_window <- droplevels(dat$pupil_model_window)
  dat$pupil_model_condition <- factor("all_data")

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "lm")
  )

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_true(all(grepl(".gp3_outcome ~ 1", out$comparison$formula, fixed = TRUE)))
})

test_that("fit_gazepoint_pupil_window_sensitivity records settings", {
  dat <- make_pupil_window_sensitivity_toy()

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "weighted_lmm"),
    include_condition = TRUE,
    include_window = TRUE,
    include_interaction = FALSE,
    random_intercept = TRUE,
    random_window_slopes = FALSE,
    fallback_on_singular = TRUE,
    REML = TRUE,
    optimizer = "bobyqa",
    maxfun = 10000,
    drop_missing = TRUE
  )

  settings <- out$settings

  expect_equal(settings$model_types, c("lmm", "weighted_lmm"))
  expect_true(settings$include_condition)
  expect_true(settings$include_window)
  expect_false(settings$include_interaction)
  expect_true(settings$random_intercept)
  expect_false(settings$random_window_slopes)
  expect_true(settings$fallback_on_singular)
  expect_true(settings$REML)
  expect_equal(settings$optimizer, "bobyqa")
  expect_equal(settings$maxfun, 10000)
  expect_true(settings$drop_missing)
})

test_that("fit_gazepoint_pupil_window_sensitivity reports unsupported model types", {
  dat <- make_pupil_window_sensitivity_toy()

  expect_error(
    fit_gazepoint_pupil_window_sensitivity(
      dat,
      model_types = c("lmm", "robust_lmm")
    ),
    "Unsupported model type\\(s\\): robust_lmm"
  )
})

test_that("fit_gazepoint_pupil_window_sensitivity checks scalar arguments", {
  dat <- make_pupil_window_sensitivity_toy()

  expect_error(
    fit_gazepoint_pupil_window_sensitivity(dat, outcome_col = NA_character_),
    "`outcome_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_window_sensitivity(dat, include_condition = NA),
    "`include_condition` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_window_sensitivity(dat, maxfun = 0),
    "`maxfun` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_window_sensitivity captures model errors", {
  dat <- make_pupil_window_sensitivity_toy()
  dat$pupil_model_outcome <- NA_real_

  out <- fit_gazepoint_pupil_window_sensitivity(
    dat,
    model_types = c("lmm", "lm")
  )

  expect_equal(out$model_status, "error")
  expect_true(all(out$model_status_by_type == "error"))
  expect_true(all(out$comparison$final_model_status == "error"))
  expect_match(
    out$error_message,
    "No rows are available for pupil-window LMM fitting"
  )
})
