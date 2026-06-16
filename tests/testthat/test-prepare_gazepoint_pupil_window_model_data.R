make_pupil_window_model_toy <- function() {
  subjects <- sprintf("S%02d", 1:10)
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
  condition_effect <- ifelse(dat$condition == "treatment", 0.15, 0)
  window_effect <- ifelse(dat$window_label == "500_1000ms", -0.05, 0)

  dat$mean_pupil <- 0.10 +
    condition_effect +
    window_effect +
    subject_num * 0.01

  dat$n_samples <- ifelse(dat$window_label == "0_500ms", 30, 60)
  dat$n_valid_pupil <- dat$n_samples - rep(c(0, 1, 2, 0), length.out = nrow(dat))
  dat$pupil_window_status <- "valid"

  tibble::as_tibble(dat)
}

test_that("prepare_gazepoint_pupil_window_model_data prepares valid pupil-window data", {
  dat <- make_pupil_window_model_toy()

  out <- prepare_gazepoint_pupil_window_model_data(dat)

  expect_s3_class(out, "gp3_pupil_window_model_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(dat))

  expect_true(all(out$pupil_model_status == "ok"))
  expect_true(is.factor(out$pupil_model_subject))
  expect_true(is.factor(out$pupil_model_condition))
  expect_true(is.factor(out$pupil_model_window))

  expect_equal(
    levels(out$pupil_model_window),
    c("0_500ms", "500_1000ms")
  )

  expect_equal(out$pupil_model_outcome, out$mean_pupil)
  expect_equal(out$pupil_model_valid_samples, out$n_valid_pupil)
  expect_equal(out$pupil_model_total_samples, out$n_samples)
  expect_equal(
    out$pupil_model_valid_prop,
    out$n_valid_pupil / out$n_samples
  )
  expect_equal(out$pupil_model_weight, out$n_valid_pupil)

  expect_equal(unique(out$pupil_model_outcome_label), "pupil")
  expect_equal(unique(out$pupil_model_outcome_col), "mean_pupil")
  expect_equal(unique(out$pupil_model_valid_samples_col), "n_valid_pupil")
  expect_equal(unique(out$pupil_model_total_samples_col), "n_samples")
})

test_that("prepare_gazepoint_pupil_window_model_data records settings", {
  dat <- make_pupil_window_model_toy()

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
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
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "mean_pupil"
  )

  settings <- attr(out, "settings")

  expect_equal(settings$outcome_col, "mean_pupil")
  expect_equal(settings$subject_col, "subject")
  expect_equal(settings$condition_col, "condition")
  expect_equal(settings$window_col, "window_label")
  expect_equal(settings$window_start_col, "window_start_ms")
  expect_equal(settings$window_end_col, "window_end_ms")
  expect_equal(settings$trial_col, "trial_global")
  expect_equal(settings$media_col, "media_id")
  expect_equal(settings$valid_samples_col, "n_valid_pupil")
  expect_equal(settings$total_samples_col, "n_samples")
  expect_equal(settings$min_valid_samples, 5)
  expect_equal(settings$min_valid_prop, 0.70)
  expect_true(settings$drop_invalid)
  expect_equal(settings$missing_condition_label, "all_data")
  expect_equal(settings$outcome_label, "mean_pupil")
})

test_that("prepare_gazepoint_pupil_window_model_data keeps optional trial and media identifiers", {
  dat <- make_pupil_window_model_toy()

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
    trial_col = "trial_global",
    media_col = "media_id"
  )

  expected <- dat |>
    dplyr::arrange(
      .data[["subject"]],
      .data[["condition"]],
      .data[["window_start_ms"]],
      .data[["window_label"]]
    )

  expect_equal(out$pupil_model_trial, expected$trial_global)
  expect_equal(out$pupil_model_media, expected$media_id)
})

test_that("prepare_gazepoint_pupil_window_model_data falls back when condition is missing", {
  dat <- make_pupil_window_model_toy()
  dat$condition <- NULL

  out <- prepare_gazepoint_pupil_window_model_data(dat)

  expect_equal(nrow(out), nrow(dat))
  expect_equal(levels(out$pupil_model_condition), "all_data")
  expect_true(all(as.character(out$pupil_model_condition) == "all_data"))
})

test_that("prepare_gazepoint_pupil_window_model_data keeps optional trial and media identifiers", {
  dat <- make_pupil_window_model_toy()

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
    trial_col = "trial_global",
    media_col = "media_id"
  )

  expected <- dat |>
    dplyr::arrange(
      .data[["subject"]],
      .data[["condition"]],
      .data[["window_start_ms"]],
      .data[["window_label"]]
    )

  expect_equal(out$pupil_model_trial, expected$trial_global)
  expect_equal(out$pupil_model_media, expected$media_id)
})

test_that("prepare_gazepoint_pupil_window_model_data handles missing optional identifiers", {
  dat <- make_pupil_window_model_toy()
  dat$media_id <- NULL
  dat$trial_global <- NULL

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
    trial_col = NULL,
    media_col = NULL
  )

  expect_equal(nrow(out), nrow(dat))
  expect_true(all(is.na(out$pupil_model_trial)))
  expect_true(all(is.na(out$pupil_model_media)))
})

test_that("prepare_gazepoint_pupil_window_model_data reports invalid row statuses", {
  dat <- make_pupil_window_model_toy()[1:8, ]

  dat$mean_pupil[1] <- NA_real_
  dat$mean_pupil[2] <- Inf
  dat$n_valid_pupil[3] <- NA_real_
  dat$n_samples[4] <- NA_real_
  dat$n_valid_pupil[5] <- -1
  dat$n_samples[6] <- -1
  dat$n_samples[7] <- 0
  dat$n_valid_pupil[8] <- 10
  dat$n_samples[8] <- 30

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
    drop_invalid = FALSE
  )

  expect_equal(out$pupil_model_status[1], "missing_outcome")
  expect_equal(out$pupil_model_status[2], "non_finite_outcome")
  expect_equal(out$pupil_model_status[3], "missing_valid_samples")
  expect_equal(out$pupil_model_status[4], "missing_total_samples")
  expect_equal(out$pupil_model_status[5], "negative_valid_samples")
  expect_equal(out$pupil_model_status[6], "negative_total_samples")
  expect_equal(out$pupil_model_status[7], "zero_total_samples")
  expect_equal(out$pupil_model_status[8], "low_valid_prop")
})

test_that("prepare_gazepoint_pupil_window_model_data drops invalid rows", {
  dat <- make_pupil_window_model_toy()[1:6, ]

  dat$mean_pupil[1] <- NA_real_
  dat$n_valid_pupil[2] <- 1
  dat$n_valid_pupil[3] <- 5
  dat$n_samples[3] <- 30

  out <- prepare_gazepoint_pupil_window_model_data(
    dat,
    min_valid_samples = 5,
    min_valid_prop = 0.70,
    drop_invalid = TRUE
  )

  expect_equal(nrow(out), 3)
  expect_true(all(out$pupil_model_status == "ok"))
})

test_that("prepare_gazepoint_pupil_window_model_data errors when no valid rows remain", {
  dat <- make_pupil_window_model_toy()[1:3, ]
  dat$mean_pupil <- NA_real_

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat),
    "No rows remain after preparing pupil-window model data"
  )
})

test_that("prepare_gazepoint_pupil_window_model_data checks required columns", {
  dat <- make_pupil_window_model_toy()
  dat$mean_pupil <- NULL

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat),
    "Missing required columns: mean_pupil"
  )
})

test_that("prepare_gazepoint_pupil_window_model_data checks scalar arguments", {
  dat <- make_pupil_window_model_toy()

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat, outcome_col = NA_character_),
    "`outcome_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat, min_valid_samples = -1),
    "`min_valid_samples` must be a non-negative finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat, min_valid_prop = 1.5),
    "`min_valid_prop` must be a finite numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_pupil_window_model_data(dat, drop_invalid = NA),
    "`drop_invalid` must be TRUE or FALSE",
    fixed = TRUE
  )
})
