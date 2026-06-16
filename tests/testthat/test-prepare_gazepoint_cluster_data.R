make_test_cluster_data <- function() {
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

  dat$time_bin_50 <- floor(dat$time / 50) * 50

  subject_num <- as.numeric(factor(dat$subject))
  condition_effect <- ifelse(dat$condition == "treatment", 0.20, 0)
  time_effect <- dat$time / 1000

  dat$pupil_value <- 1 +
    condition_effect +
    time_effect +
    subject_num * 0.01

  dat$target_aoi <- dat$condition == "treatment" &
    dat$time >= 50 &
    dat$time < 150

  tibble::as_tibble(dat)
}

test_that("prepare_gazepoint_cluster_data prepares logical AOI proportion data", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "target_aoi",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "proportion",
    paired = TRUE,
    outcome_label = "target_aoi"
  )

  expect_s3_class(out, "gp3_cluster_data")
  expect_equal(nrow(out), 6 * 2 * 4)
  expect_equal(dplyr::n_distinct(out$.gp3_cluster_subject), 6)
  expect_equal(dplyr::n_distinct(out$.gp3_cluster_condition), 2)
  expect_equal(dplyr::n_distinct(out$.gp3_cluster_time_bin), 4)
  expect_true(all(out$.gp3_cluster_status == "ok"))
  expect_true(all(out$.gp3_cluster_condition_status == "two_conditions"))
  expect_true(all(out$.gp3_cluster_aggregation == "proportion"))
  expect_true(all(out$.gp3_cluster_outcome_label == "target_aoi"))

  expect_true(all(out$.gp3_cluster_outcome >= 0))
  expect_true(all(out$.gp3_cluster_outcome <= 1))

  summary <- attr(out, "summary")
  expect_equal(summary$n_subjects, 6)
  expect_equal(summary$n_conditions, 2)
  expect_equal(summary$n_time_bins, 4)
  expect_equal(summary$condition_status, "two_conditions")

  settings <- attr(out, "settings")
  expect_equal(settings$outcome_col, "target_aoi")
  expect_equal(settings$aggregation, "proportion")
  expect_true(settings$paired)
})

test_that("prepare_gazepoint_cluster_data prepares numeric mean data", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE,
    outcome_label = "pupil_value"
  )

  expect_s3_class(out, "gp3_cluster_data")
  expect_equal(nrow(out), 6 * 2 * 4)
  expect_true(all(is.finite(out$.gp3_cluster_outcome)))
  expect_true(all(out$.gp3_cluster_aggregation == "mean"))
  expect_true(all(out$.gp3_cluster_outcome_col == "pupil_value"))
})

test_that("prepare_gazepoint_cluster_data uses an existing time-bin column", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_bin_col = "time_bin_50",
    aggregation = "mean",
    paired = TRUE
  )

  expect_equal(sort(unique(out$.gp3_cluster_time_bin)), c(0, 50, 100, 150))
  expect_equal(attr(out, "settings")$time_bin_col, "time_bin_50")
})

test_that("prepare_gazepoint_cluster_data filters conditions and preserves order", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    conditions = c("treatment", "control"),
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE
  )

  expect_equal(
    levels(out$.gp3_cluster_condition),
    c("treatment", "control")
  )

  expect_equal(
    attr(out, "summary")$condition_levels,
    c("treatment", "control")
  )
})

test_that("prepare_gazepoint_cluster_data filters the requested time window", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(50, 140),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE
  )

  expect_equal(sort(unique(out$.gp3_cluster_time_bin)), c(50, 100))
})

test_that("prepare_gazepoint_cluster_data paired filtering drops incomplete subjects", {
  dat <- make_test_cluster_data()

  dat <- dat[
    !(dat$subject == "S6" & dat$condition == "treatment"),
    ,
    drop = FALSE
  ]

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE
  )

  expect_false("S6" %in% as.character(out$.gp3_cluster_subject))
  expect_equal(dplyr::n_distinct(out$.gp3_cluster_subject), 5)
})

test_that("prepare_gazepoint_cluster_data can keep incomplete subjects when unpaired", {
  dat <- make_test_cluster_data()

  dat <- dat[
    !(dat$subject == "S6" & dat$condition == "treatment"),
    ,
    drop = FALSE
  ]

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = FALSE
  )

  expect_true("S6" %in% as.character(out$.gp3_cluster_subject))
  expect_equal(dplyr::n_distinct(out$.gp3_cluster_subject), 6)
})

test_that("prepare_gazepoint_cluster_data can retain low-sample bins when requested", {
  dat <- make_test_cluster_data()

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    min_samples_per_bin = 20,
    paired = TRUE,
    drop_invalid = FALSE
  )

  expect_true(any(out$.gp3_cluster_status == "low_samples"))
  expect_equal(nrow(out), 6 * 2 * 4)
})

test_that("prepare_gazepoint_cluster_data supports sum and median aggregation", {
  dat <- make_test_cluster_data()

  out_sum <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "target_aoi",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "sum",
    paired = TRUE
  )

  out_median <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "median",
    paired = TRUE
  )

  expect_true(all(out_sum$.gp3_cluster_aggregation == "sum"))
  expect_true(all(out_median$.gp3_cluster_aggregation == "median"))
  expect_true(all(is.finite(out_sum$.gp3_cluster_outcome)))
  expect_true(all(is.finite(out_median$.gp3_cluster_outcome)))
})

test_that("prepare_gazepoint_cluster_data uses all_data when condition is unavailable", {
  dat <- make_test_cluster_data()
  dat$condition <- NULL

  out <- prepare_gazepoint_cluster_data(
    dat,
    outcome_col = "pupil_value",
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = "trial_global",
    time_window = c(0, 190),
    bin_size_ms = 50,
    aggregation = "mean",
    paired = TRUE
  )

  expect_equal(levels(out$.gp3_cluster_condition), "all_data")
  expect_true(all(out$.gp3_cluster_condition_status == "less_than_two_conditions"))
  expect_equal(attr(out, "summary")$n_conditions, 1)
})

test_that("prepare_gazepoint_cluster_data checks required columns", {
  dat <- make_test_cluster_data()
  dat$pupil_value <- NULL

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = "pupil_value",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time"
    ),
    "Missing required columns: pupil_value"
  )
})

test_that("prepare_gazepoint_cluster_data checks scalar arguments", {
  dat <- make_test_cluster_data()

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = NA_character_,
      subject_col = "subject",
      time_col = "time"
    ),
    "`outcome_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = "pupil_value",
      subject_col = "subject",
      time_col = "time",
      paired = NA
    ),
    "`paired` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = "pupil_value",
      subject_col = "subject",
      time_col = "time",
      bin_size_ms = 0
    ),
    "`bin_size_ms` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_cluster_data checks time window validity", {
  dat <- make_test_cluster_data()

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = "pupil_value",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time",
      time_window = c(100, 0)
    ),
    "`time_window` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_cluster_data errors when no rows remain", {
  dat <- make_test_cluster_data()

  expect_error(
    prepare_gazepoint_cluster_data(
      dat,
      outcome_col = "pupil_value",
      subject_col = "subject",
      condition_col = "condition",
      time_col = "time",
      time_window = c(1000, 2000)
    ),
    "No rows are available after preparing cluster-test input rows"
  )
})
