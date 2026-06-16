make_test_overlap_data <- function() {
  tibble::tibble(
    subject = c(rep("S1", 5), rep("S2", 5)),
    trial_global = c(rep("S1_T1", 5), rep("S2_T1", 5)),
    time = c(
      0, 500, 1000, 1500, 2000,
      0, 1000, 2000, 3000, 4000
    ),
    stimulus_onset_time = c(rep(0, 5), rep(0, 5)),
    target_onset_time = c(rep(1000, 5), rep(3000, 5)),
    response_time = c(rep(3500, 5), rep(4500, 5)),
    excluded_trial = FALSE
  )
}

test_that("audit_gazepoint_pupil_overlap_risk returns named audit tables", {
  x <- make_test_overlap_data()

  out <- audit_gazepoint_pupil_overlap_risk(x)

  expect_type(out, "list")
  expect_named(out, c("events", "event_gaps", "by_trial", "summary"))

  expect_s3_class(out$events, "tbl_df")
  expect_s3_class(out$event_gaps, "tbl_df")
  expect_s3_class(out$by_trial, "tbl_df")
  expect_s3_class(out$summary, "tbl_df")
})

test_that("audit_gazepoint_pupil_overlap_risk creates distinct event table", {
  x <- make_test_overlap_data()

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    group_cols = "subject",
    trial_col = "trial_global",
    time_col = "time",
    event_time_cols = c(
      "stimulus_onset_time",
      "target_onset_time",
      "response_time"
    ),
    window_start_ms = 0,
    window_end_ms = 2000,
    min_event_gap_ms = 1500
  )

  expect_equal(nrow(out$events), 6L)
  expect_true(all(c("S1", "S2") %in% out$events$subject))
  expect_true(all(c("stimulus_onset_time", "target_onset_time", "response_time") %in% out$events$event_name))

  s1_target <- out$events[
    out$events$subject == "S1" &
      out$events$event_name == "target_onset_time",
  ]

  expect_equal(s1_target$event_time_ms, 1000)
  expect_equal(s1_target$response_window_start_ms, 1000)
  expect_equal(s1_target$response_window_end_ms, 3000)
  expect_equal(s1_target$response_window_duration_ms, 2000)
})

test_that("audit_gazepoint_pupil_overlap_risk detects short gaps and overlapping windows", {
  x <- make_test_overlap_data()

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    group_cols = "subject",
    trial_col = "trial_global",
    time_col = "time",
    event_time_cols = c(
      "stimulus_onset_time",
      "target_onset_time",
      "response_time"
    ),
    window_start_ms = 0,
    window_end_ms = 2000,
    min_event_gap_ms = 1500
  )

  s1_target <- out$event_gaps[
    out$event_gaps$subject == "S1" &
      out$event_gaps$event_name == "target_onset_time",
  ]

  expect_equal(s1_target$previous_event_name, "stimulus_onset_time")
  expect_equal(s1_target$event_gap_ms, 1000)
  expect_true(s1_target$short_event_gap)
  expect_true(s1_target$response_window_overlap)
  expect_equal(s1_target$overlap_amount_ms, 1000)
  expect_equal(s1_target$event_gap_status, "overlap_and_short_gap")
})

test_that("audit_gazepoint_pupil_overlap_risk summarises by trial", {
  x <- make_test_overlap_data()

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    group_cols = "subject",
    trial_col = "trial_global",
    time_col = "time",
    event_time_cols = c(
      "stimulus_onset_time",
      "target_onset_time",
      "response_time"
    ),
    window_start_ms = 0,
    window_end_ms = 2000,
    min_event_gap_ms = 1500
  )

  expect_equal(nrow(out$by_trial), 2L)

  s1 <- out$by_trial[out$by_trial$trial_global == "S1_T1", ]
  s2 <- out$by_trial[out$by_trial$trial_global == "S2_T1", ]

  expect_equal(s1$n_events, 3L)
  expect_equal(s1$n_event_gaps, 2L)
  expect_equal(s1$n_short_event_gaps, 1L)
  expect_equal(s1$n_overlapping_response_windows, 1L)
  expect_true(s1$overlap_risk_warning)
  expect_equal(
    s1$overlap_risk_reason,
    "short_event_gap;overlapping_response_window"
  )

  expect_equal(s2$n_events, 3L)
  expect_equal(s2$n_event_gaps, 2L)
  expect_equal(s2$n_short_event_gaps, 0L)
  expect_equal(s2$n_overlapping_response_windows, 1L)
  expect_true(s2$overlap_risk_warning)
  expect_equal(s2$overlap_risk_reason, "overlapping_response_window")
})

test_that("audit_gazepoint_pupil_overlap_risk reports summary status", {
  x <- make_test_overlap_data()

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    window_start_ms = 0,
    window_end_ms = 2000,
    min_event_gap_ms = 1500
  )

  expect_equal(out$summary$n_trials, 2L)
  expect_equal(out$summary$n_events, 6L)
  expect_equal(out$summary$n_trials_with_events, 2L)
  expect_equal(out$summary$n_trials_without_events, 0L)
  expect_equal(out$summary$n_trials_with_short_event_gaps, 1L)
  expect_equal(out$summary$n_trials_with_overlapping_windows, 2L)
  expect_equal(out$summary$n_overlap_risk_trials, 2L)
  expect_equal(out$summary$pct_overlap_risk_trials, 100)
  expect_equal(out$summary$overlap_assessment_status, "possible_overlap_risk")
})

test_that("audit_gazepoint_pupil_overlap_risk handles no usable event times", {
  x <- make_test_overlap_data() |>
    dplyr::mutate(
      stimulus_onset_time = NA_real_,
      target_onset_time = NA_real_,
      response_time = NA_real_
    )

  out <- audit_gazepoint_pupil_overlap_risk(x)

  expect_equal(nrow(out$events), 0L)
  expect_equal(nrow(out$event_gaps), 0L)
  expect_equal(nrow(out$by_trial), 2L)
  expect_equal(out$summary$n_trials, 2L)
  expect_equal(out$summary$n_events, 0L)
  expect_equal(out$summary$n_trials_with_events, 0L)
  expect_equal(out$summary$n_trials_without_events, 2L)
  expect_equal(out$summary$overlap_assessment_status, "no_usable_event_times")
  expect_true(all(out$by_trial$overlap_risk_reason == "no_usable_event_times"))
})

test_that("audit_gazepoint_pupil_overlap_risk respects excluded rows", {
  x <- make_test_overlap_data() |>
    dplyr::mutate(excluded_trial = subject == "S2")

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    include_excluded = FALSE
  )

  expect_equal(nrow(out$by_trial), 1L)
  expect_equal(out$by_trial$subject, "S1")
  expect_equal(out$summary$n_trials, 1L)
})

test_that("audit_gazepoint_pupil_overlap_risk can include excluded rows", {
  x <- make_test_overlap_data() |>
    dplyr::mutate(excluded_trial = subject == "S2")

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    include_excluded = TRUE
  )

  expect_equal(nrow(out$by_trial), 2L)
  expect_equal(out$summary$n_trials, 2L)
})

test_that("audit_gazepoint_pupil_overlap_risk supports custom grouping columns", {
  x <- make_test_overlap_data() |>
    dplyr::mutate(condition = c(rep("A", 5), rep("B", 5)))

  out <- audit_gazepoint_pupil_overlap_risk(
    x,
    group_cols = c("subject", "condition"),
    trial_col = "trial_global"
  )

  expect_true("condition" %in% names(out$events))
  expect_true("condition" %in% names(out$by_trial))
  expect_equal(nrow(out$by_trial), 2L)
})

test_that("audit_gazepoint_pupil_overlap_risk errors when required columns are missing", {
  x <- make_test_overlap_data()

  expect_error(
    audit_gazepoint_pupil_overlap_risk(
      dplyr::select(x, -time)
    ),
    "Missing required columns"
  )

  expect_error(
    audit_gazepoint_pupil_overlap_risk(
      x,
      group_cols = "missing_subject"
    ),
    "Missing required columns"
  )
})

test_that("audit_gazepoint_pupil_overlap_risk errors for invalid inputs", {
  x <- make_test_overlap_data()

  expect_error(
    audit_gazepoint_pupil_overlap_risk("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_overlap_risk(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_overlap_risk(
      x,
      event_time_cols = character(0)
    ),
    "`event_time_cols` must be a non-empty character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_pupil_overlap_risk(
      x,
      window_start_ms = 2000,
      window_end_ms = 1000
    ),
    "`window_end_ms` must be greater than `window_start_ms`",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_pupil_overlap_risk works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "subject",
      "trial_global",
      "time",
      "stimulus_onset_time",
      "target_onset_time",
      "response_time"
    )

    if (all(required_cols %in% names(real_data))) {
      out <- audit_gazepoint_pupil_overlap_risk(
        real_data,
        group_cols = "subject",
        trial_col = "trial_global",
        time_col = "time",
        event_time_cols = c(
          "stimulus_onset_time",
          "target_onset_time",
          "response_time"
        )
      )

      expect_type(out, "list")
      expect_s3_class(out$by_trial, "tbl_df")
      expect_s3_class(out$summary, "tbl_df")
      expect_true("overlap_assessment_status" %in% names(out$summary))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
