make_test_aoi_gamm_data <- function() {
  subjects <- paste0("S", 1:6)
  conditions <- c("control", "treatment")
  trials <- paste0("T", 1:2)
  times <- seq(0, 190, by = 10)

  dat <- expand.grid(
    subject = subjects,
    condition = conditions,
    trial_global = trials,
    time = times,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat <- tibble::as_tibble(dat)

  dat$aoi_current <- "non_aoi"
  dat$aoi_current[dat$time >= 50 & dat$time < 100] <- "AOI 1"
  dat$aoi_current[
    dat$condition == "treatment" &
      dat$time >= 100 &
      dat$time < 150
  ] <- "AOI 2"
  dat$aoi_current[dat$time == 190] <- "missing"

  dat$time_bin_50 <- floor(dat$time / 50) * 50
  dat$target_aoi <- dat$aoi_current == "AOI 2"

  dat
}

test_that("prepare_gazepoint_aoi_gamm_data prepares AOI-column data", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "valid",
    min_denominator_samples = 1,
    outcome_label = "target_aoi"
  )

  expect_s3_class(out, "gp3_aoi_gamm_data")
  expect_equal(nrow(out), 6 * 2 * 4)
  expect_equal(dplyr::n_distinct(out$.gp3_aoi_gamm_subject), 6)
  expect_equal(dplyr::n_distinct(out$.gp3_aoi_gamm_condition), 2)
  expect_equal(dplyr::n_distinct(out$.gp3_aoi_gamm_time_bin), 4)

  expect_true(all(out$.gp3_aoi_gamm_status == "ok"))
  expect_true(all(out$.gp3_aoi_gamm_condition_status == "two_conditions"))
  expect_true(all(out$.gp3_aoi_gamm_denominator_type == "valid"))
  expect_true(all(out$.gp3_aoi_gamm_outcome_label == "target_aoi"))
  expect_true(all(out$.gp3_aoi_gamm_target_label == "AOI 2"))

  expect_true(all(out$.gp3_aoi_gamm_success >= 0))
  expect_true(all(out$.gp3_aoi_gamm_failure >= 0))
  expect_equal(
    out$.gp3_aoi_gamm_success + out$.gp3_aoi_gamm_failure,
    out$.gp3_aoi_gamm_denominator
  )
  expect_true(all(out$.gp3_aoi_gamm_proportion >= 0))
  expect_true(all(out$.gp3_aoi_gamm_proportion <= 1))

  summary <- attr(out, "summary")
  expect_equal(summary$n_subjects, 6)
  expect_equal(summary$n_conditions, 2)
  expect_equal(summary$n_time_bins, 4)
  expect_equal(summary$condition_status, "two_conditions")
  expect_equal(summary$denominator_type, "valid")
  expect_equal(summary$outcome_label, "target_aoi")

  settings <- attr(out, "settings")
  expect_equal(settings$aoi_col, "aoi_current")
  expect_equal(settings$target_aoi_values, "AOI 2")
  expect_equal(settings$denominator, "valid")
})

test_that("prepare_gazepoint_aoi_gamm_data prepares logical indicator data", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    outcome_col = "target_aoi",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    min_denominator_samples = 1,
    outcome_label = "target_aoi"
  )

  expect_s3_class(out, "gp3_aoi_gamm_data")
  expect_equal(nrow(out), 6 * 2 * 4)
  expect_true(all(out$.gp3_aoi_gamm_status == "ok"))
  expect_true(all(out$.gp3_aoi_gamm_target_label == "target_aoi"))
  expect_true(all(out$.gp3_aoi_gamm_success >= 0))
  expect_true(all(out$.gp3_aoi_gamm_failure >= 0))
})

test_that("prepare_gazepoint_aoi_gamm_data supports numeric 0/1 indicator data", {
  dat <- make_test_aoi_gamm_data()
  dat$target_numeric <- as.numeric(dat$target_aoi)

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    outcome_col = "target_numeric",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    min_denominator_samples = 1,
    outcome_label = "target_numeric"
  )

  expect_s3_class(out, "gp3_aoi_gamm_data")
  expect_true(all(out$.gp3_aoi_gamm_status == "ok"))
  expect_true(all(out$.gp3_aoi_gamm_proportion >= 0))
  expect_true(all(out$.gp3_aoi_gamm_proportion <= 1))
})

test_that("prepare_gazepoint_aoi_gamm_data uses an existing time-bin column", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_bin_col = "time_bin_50",
    denominator = "valid"
  )

  expect_equal(sort(unique(out$.gp3_aoi_gamm_time_bin)), c(0, 50, 100, 150))
  expect_equal(attr(out, "settings")$time_bin_col, "time_bin_50")
})

test_that("prepare_gazepoint_aoi_gamm_data filters conditions and preserves order", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    conditions = c("treatment", "control"),
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "valid"
  )

  expect_equal(
    levels(out$.gp3_aoi_gamm_condition),
    c("treatment", "control")
  )

  expect_equal(
    attr(out, "summary")$condition_levels,
    c("treatment", "control")
  )
})

test_that("prepare_gazepoint_aoi_gamm_data filters the requested time window", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(50, 140),
    bin_size_ms = 50,
    denominator = "valid"
  )

  expect_equal(sort(unique(out$.gp3_aoi_gamm_time_bin)), c(50, 100))
})

test_that("prepare_gazepoint_aoi_gamm_data supports denominator choices", {
  dat <- make_test_aoi_gamm_data()

  out_all <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "all",
    drop_invalid = FALSE
  )

  out_aoi_only <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "aoi_only",
    drop_invalid = FALSE
  )

  expect_true(all(out_all$.gp3_aoi_gamm_denominator_type == "all"))
  expect_true(all(out_aoi_only$.gp3_aoi_gamm_denominator_type == "aoi_only"))

  key_cols <- c(
    ".gp3_aoi_gamm_subject",
    ".gp3_aoi_gamm_condition",
    ".gp3_aoi_gamm_time_bin"
  )

  comparison <- dplyr::inner_join(
    out_all[
      ,
      c(key_cols, ".gp3_aoi_gamm_denominator"),
      drop = FALSE
    ],
    out_aoi_only[
      ,
      c(key_cols, ".gp3_aoi_gamm_denominator"),
      drop = FALSE
    ],
    by = key_cols,
    suffix = c("_all", "_aoi_only")
  )

  expect_equal(nrow(comparison), nrow(out_all))
  expect_true(all(
    comparison$.gp3_aoi_gamm_denominator_all >=
      comparison$.gp3_aoi_gamm_denominator_aoi_only
  ))
})

test_that("prepare_gazepoint_aoi_gamm_data supports custom valid AOI values", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "aoi_only",
    valid_aoi_values = c("AOI 1", "AOI 2"),
    drop_invalid = FALSE
  )

  expect_s3_class(out, "gp3_aoi_gamm_data")
  expect_true(all(out$.gp3_aoi_gamm_denominator_type == "aoi_only"))
})

test_that("prepare_gazepoint_aoi_gamm_data can retain low denominator bins", {
  dat <- make_test_aoi_gamm_data()

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "valid",
    min_denominator_samples = 100,
    drop_invalid = FALSE
  )

  expect_true(any(out$.gp3_aoi_gamm_status == "low_denominator"))
})

test_that("prepare_gazepoint_aoi_gamm_data uses all_data when condition is unavailable", {
  dat <- make_test_aoi_gamm_data()
  dat$condition <- NULL

  out <- prepare_gazepoint_aoi_gamm_data(
    dat,
    aoi_col = "aoi_current",
    target_aoi_values = "AOI 2",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    denominator = "valid"
  )

  expect_equal(levels(out$.gp3_aoi_gamm_condition), "all_data")
  expect_true(all(out$.gp3_aoi_gamm_condition_status ==
                    "less_than_two_conditions"))
  expect_equal(attr(out, "summary")$n_conditions, 1)
})

test_that("prepare_gazepoint_aoi_gamm_data checks required columns", {
  dat <- make_test_aoi_gamm_data()
  dat$aoi_current <- NULL

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time"
    ),
    "Missing required columns: aoi_current"
  )
})

test_that("prepare_gazepoint_aoi_gamm_data requires target values without outcome_col", {
  dat <- make_test_aoi_gamm_data()

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time"
    ),
    "`target_aoi_values` must be a non-empty character vector",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_gamm_data checks scalar arguments", {
  dat <- make_test_aoi_gamm_data()

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = NA_character_,
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      time_col = "time"
    ),
    "`aoi_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      time_col = "time",
      bin_size_ms = 0
    ),
    "`bin_size_ms` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      time_col = "time",
      drop_invalid = NA
    ),
    "`drop_invalid` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_gamm_data checks time window validity", {
  dat <- make_test_aoi_gamm_data()

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time",
      time_window = c(100, 0)
    ),
    "`time_window` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_gamm_data rejects non-binary outcome data", {
  dat <- make_test_aoi_gamm_data()
  dat$bad_outcome <- 2

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      outcome_col = "bad_outcome",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time"
    ),
    "No rows are available after preparing AOI-GAMM input rows"
  )
})

test_that("prepare_gazepoint_aoi_gamm_data errors when no rows remain", {
  dat <- make_test_aoi_gamm_data()

  expect_error(
    prepare_gazepoint_aoi_gamm_data(
      dat,
      aoi_col = "aoi_current",
      target_aoi_values = "AOI 2",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time",
      time_window = c(1000, 2000)
    ),
    "No rows are available after applying condition/time filters"
  )
})
