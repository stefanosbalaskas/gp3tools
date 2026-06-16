make_pupil_window_lmm_toy <- function() {
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

  dat$mean_pupil <- 0.50 +
    condition_effect +
    window_effect +
    subject_num * 0.02

  dat$n_samples <- ifelse(dat$window_label == "0_500ms", 30, 60)
  dat$n_valid_pupil <- dat$n_samples
  dat$pupil_window_status <- "valid"

  prep <- prepare_gazepoint_pupil_window_model_data(
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

  prep
}

test_that("fit_gazepoint_pupil_window_lmm fits a condition by window LMM", {
  dat <- make_pupil_window_lmm_toy()

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    random_window_slopes = FALSE
  )

  expect_s3_class(out, "gp3_pupil_window_lmm")
  expect_true(inherits(out$model, "lmerMod"))
  expect_equal(out$model_status, "ok")
  expect_false(out$fallback_used)
  expect_false(isTRUE(out$singular_fit))

  expect_match(
    paste(deparse(out$formula), collapse = ""),
    ".gp3_condition \\* .gp3_window",
    perl = TRUE
  )

  expect_equal(out$random_effect_structure, "1 | .gp3_subject")
  expect_equal(nrow(out$comparison), 1)
  expect_equal(out$comparison$model_status, "ok")
  expect_equal(out$comparison$engine, "lme4::lmer")

  expect_true(nrow(out$fixed_effects) >= 4)
  expect_true(all(c("term", "estimate", "std_error", "statistic", "p_value") %in%
                    names(out$fixed_effects)))
})

test_that("fit_gazepoint_pupil_window_lmm drops condition when one condition level exists", {
  dat <- make_pupil_window_lmm_toy()
  dat$pupil_model_condition <- factor("all_data")

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    random_window_slopes = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = "")

  expect_equal(out$model_status, "ok")
  expect_false(grepl(".gp3_condition", formula_text, fixed = TRUE))
  expect_true(grepl(".gp3_window", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_pupil_window_lmm drops window when one window level exists", {
  dat <- make_pupil_window_lmm_toy()
  dat <- dat[dat$pupil_model_window == "0_500ms", , drop = FALSE]
  dat$pupil_model_window <- droplevels(dat$pupil_model_window)

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    random_window_slopes = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = "")

  expect_equal(out$model_status, "ok")
  expect_true(grepl(".gp3_condition", formula_text, fixed = TRUE))
  expect_false(grepl(".gp3_window", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_pupil_window_lmm uses intercept-only fixed part when needed", {
  dat <- make_pupil_window_lmm_toy()
  dat <- dat[dat$pupil_model_window == "0_500ms", , drop = FALSE]
  dat$pupil_model_window <- droplevels(dat$pupil_model_window)
  dat$pupil_model_condition <- factor("all_data")

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    random_window_slopes = FALSE
  )

  formula_text <- paste(deparse(out$formula), collapse = "")

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_match(formula_text, ".gp3_outcome ~ 1")
  expect_false(grepl(".gp3_condition", formula_text, fixed = TRUE))
  expect_false(grepl(".gp3_window", formula_text, fixed = TRUE))
})

test_that("fit_gazepoint_pupil_window_lmm can fit a weighted LMM", {
  dat <- make_pupil_window_lmm_toy()

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    use_weights = TRUE,
    random_window_slopes = FALSE
  )

  expect_equal(out$model_status, "ok")
  expect_true(out$settings$use_weights)
  expect_equal(out$settings$weights_col, "pupil_model_weight")
  expect_true(inherits(out$model, "lmerMod"))
})

test_that("fit_gazepoint_pupil_window_lmm falls back to lm when no random effect is feasible", {
  dat <- make_pupil_window_lmm_toy()
  dat$pupil_model_subject <- factor("S01")

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    random_intercept = TRUE
  )

  expect_true(out$model_status %in% c("ok", "singular_fit"))
  expect_s3_class(out$model, "lm")
  expect_equal(out$comparison$engine, "stats::lm")
  expect_equal(out$random_effect_structure, "none")
})

test_that("fit_gazepoint_pupil_window_lmm can use a supplied formula", {
  dat <- make_pupil_window_lmm_toy()

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    formula = .gp3_outcome ~ .gp3_window + (1 | .gp3_subject)
  )

  expect_equal(out$model_status, "ok")
  expect_match(
    paste(deparse(out$formula), collapse = ""),
    ".gp3_window",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_window_lmm records settings", {
  dat <- make_pupil_window_lmm_toy()

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    use_weights = TRUE,
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

  expect_true(settings$use_weights)
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

test_that("fit_gazepoint_pupil_window_lmm removes invalid rows when requested", {
  dat <- make_pupil_window_lmm_toy()

  dat$pupil_model_outcome[1] <- NA_real_
  dat$pupil_model_weight[2] <- NA_real_
  dat$pupil_model_status[3] <- "missing_outcome"

  out <- fit_gazepoint_pupil_window_lmm(
    dat,
    use_weights = TRUE,
    random_window_slopes = FALSE,
    drop_missing = TRUE
  )

  expect_equal(out$model_status, "ok")
  expect_equal(nrow(out$data), nrow(dat) - 3)
})

test_that("fit_gazepoint_pupil_window_lmm checks required columns", {
  dat <- make_pupil_window_lmm_toy()
  dat$pupil_model_outcome <- NULL

  expect_error(
    fit_gazepoint_pupil_window_lmm(dat),
    "Missing required columns: pupil_model_outcome"
  )
})

test_that("fit_gazepoint_pupil_window_lmm checks scalar arguments", {
  dat <- make_pupil_window_lmm_toy()

  expect_error(
    fit_gazepoint_pupil_window_lmm(dat, outcome_col = NA_character_),
    "`outcome_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_window_lmm(dat, use_weights = NA),
    "`use_weights` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    fit_gazepoint_pupil_window_lmm(dat, maxfun = 0),
    "`maxfun` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("fit_gazepoint_pupil_window_lmm errors when no rows are available", {
  dat <- make_pupil_window_lmm_toy()
  dat$pupil_model_outcome <- NA_real_

  expect_error(
    fit_gazepoint_pupil_window_lmm(dat),
    "No rows are available for pupil-window LMM fitting"
  )
})
