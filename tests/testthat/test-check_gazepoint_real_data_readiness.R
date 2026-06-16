make_test_real_ready_data <- function() {
  set.seed(123)

  tibble::tibble(
    subject = rep(paste0("S", 1:6), each = 20),
    trial = rep(rep(1:4, each = 5), times = 6),
    time = rep(seq(0, 400, by = 100), times = 24),
    condition = rep(c("A", "B"), each = 60),
    stimulus = rep(c("stim_1", "stim_2"), each = 60),
    aoi = rep(c("logo", "claim", "product", "none"), length.out = 120),
    pupil_clean = stats::rnorm(120, mean = 1000, sd = 40),
    gaze_x = stats::runif(120, 0, 1),
    gaze_y = stats::runif(120, 0, 1),
    tracking_valid = TRUE
  )
}

test_that("check_gazepoint_real_data_readiness creates a passing combined readiness gate", {
  toy_data <- make_test_real_ready_data()

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "combined",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    condition_col = "condition",
    stimulus_col = "stimulus",
    aoi_col = "aoi",
    pupil_col = "pupil_clean",
    gaze_x_col = "gaze_x",
    gaze_y_col = "gaze_y",
    tracking_valid_col = "tracking_valid",
    min_participants = 6,
    min_trials = 24,
    name = "toy_real_gate"
  )

  expect_s3_class(out, "gp3_real_data_readiness_gate")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "gate_decision",
      "checks",
      "detected_columns",
      "data_summary",
      "condition_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$gate_decision, "tbl_df")
  expect_s3_class(out$checks, "tbl_df")
  expect_s3_class(out$detected_columns, "tbl_df")
  expect_s3_class(out$data_summary, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_real_gate")
  expect_equal(out$overview$analysis_type, "combined")
  expect_equal(out$overview$readiness_status, "pass")
  expect_true(out$overview$ready_for_real_data_analysis)
  expect_equal(out$overview$n_rows, nrow(toy_data))
  expect_equal(out$overview$n_participants, 6)
  expect_equal(out$overview$n_trial_units, 24)
  expect_equal(out$overview$n_fail, 0)
  expect_equal(out$overview$n_warn, 0)

  expect_equal(out$gate_decision$readiness_status, "pass")
  expect_true(out$gate_decision$ready_for_real_data_analysis)
  expect_match(out$gate_decision$decision_message, "Ready for real-data analysis", fixed = TRUE)

  expect_true(all(out$checks$status %in% c("pass", "warn", "fail", "info")))
  expect_true(any(out$checks$check_id == "pupil_missingness"))
  expect_true(any(out$checks$check_id == "gaze_coordinate_missingness"))
  expect_true(any(out$checks$check_id == "tracking_validity"))
})

test_that("check_gazepoint_real_data_readiness auto-detects common columns", {
  toy_data <- make_test_real_ready_data()

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "combined",
    min_participants = 6,
    min_trials = 24
  )

  expect_s3_class(out, "gp3_real_data_readiness_gate")
  expect_equal(out$overview$readiness_status, "pass")

  expect_equal(out$detected_columns$column[out$detected_columns$role == "participant_col"], "subject")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "trial_col"], "trial")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "time_col"], "time")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "condition_col"], "condition")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "stimulus_col"], "stimulus")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "aoi_col"], "aoi")
  expect_equal(out$detected_columns$column[out$detected_columns$role == "pupil_col"], "pupil_clean")
})

test_that("check_gazepoint_real_data_readiness supports general analysis without pupil or AOI columns", {
  toy_data <- make_test_real_ready_data() |>
    dplyr::select(
      "subject",
      "trial",
      "condition"
    )

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    min_participants = 6,
    min_trials = 24
  )

  expect_s3_class(out, "gp3_real_data_readiness_gate")
  expect_equal(out$overview$readiness_status, "pass")
  expect_true(out$overview$ready_for_real_data_analysis)

  expect_equal(
    out$checks$status[out$checks$check_id == "required_participant_col"],
    "pass"
  )
  expect_equal(
    out$checks$status[out$checks$check_id == "required_trial_col"],
    "pass"
  )
  expect_equal(
    out$checks$status[out$checks$check_id == "required_aoi_col"],
    "info"
  )
  expect_equal(
    out$checks$status[out$checks$check_id == "required_pupil_col"],
    "info"
  )
})

test_that("check_gazepoint_real_data_readiness fails when required combined columns are missing", {
  toy_data <- make_test_real_ready_data() |>
    dplyr::select(
      -"time",
      -"aoi",
      -"pupil_clean"
    )

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "combined",
    participant_col = "subject",
    trial_col = "trial",
    condition_col = "condition",
    min_participants = 6,
    min_trials = 24
  )

  expect_s3_class(out, "gp3_real_data_readiness_gate")
  expect_equal(out$overview$readiness_status, "fail")
  expect_false(out$overview$ready_for_real_data_analysis)
  expect_true(out$overview$n_fail >= 3)

  failed_ids <- out$checks$check_id[out$checks$status == "fail"]

  expect_true("required_time_col" %in% failed_ids)
  expect_true("required_aoi_col" %in% failed_ids)
  expect_true("required_pupil_col" %in% failed_ids)

  expect_match(
    out$gate_decision$decision_message,
    "Not ready for real-data analysis",
    fixed = TRUE
  )
})

test_that("check_gazepoint_real_data_readiness fails minimum sample thresholds", {
  toy_data <- make_test_real_ready_data()

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    min_rows = 200,
    min_participants = 10,
    min_trials = 50
  )

  expect_equal(out$overview$readiness_status, "fail")
  expect_false(out$overview$ready_for_real_data_analysis)

  failed_ids <- out$checks$check_id[out$checks$status == "fail"]

  expect_true("minimum_rows" %in% failed_ids)
  expect_true("minimum_participants" %in% failed_ids)
  expect_true("minimum_trials" %in% failed_ids)
})

test_that("check_gazepoint_real_data_readiness fails pupil analysis with high pupil missingness", {
  toy_data <- make_test_real_ready_data()
  toy_data$pupil_clean[1:80] <- NA_real_

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "pupil",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean",
    max_missing_pupil_prop = 0.40
  )

  expect_equal(out$overview$readiness_status, "fail")
  expect_false(out$overview$ready_for_real_data_analysis)

  pupil_check <- out$checks[out$checks$check_id == "pupil_missingness", ]

  expect_equal(pupil_check$status, "fail")
  expect_equal(pupil_check$severity, "blocking")
  expect_true(pupil_check$observed > pupil_check$threshold)
})

test_that("check_gazepoint_real_data_readiness warns for high gaze missingness in non-blocking signal-quality checks", {
  toy_data <- make_test_real_ready_data()
  toy_data$gaze_x[1:80] <- NA_real_

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    gaze_x_col = "gaze_x",
    gaze_y_col = "gaze_y",
    max_missing_gaze_prop = 0.40
  )

  expect_equal(out$overview$readiness_status, "warn")
  expect_true(out$overview$ready_for_real_data_analysis)
  expect_true(out$overview$n_warn >= 1)

  gaze_check <- out$checks[out$checks$check_id == "gaze_coordinate_missingness", ]

  expect_equal(gaze_check$status, "warn")
  expect_equal(gaze_check$severity, "warning")
})

test_that("check_gazepoint_real_data_readiness warns for duplicated participant-trial-time keys", {
  toy_data <- make_test_real_ready_data()
  toy_data$time[2] <- toy_data$time[1]

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "pupil",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    pupil_col = "pupil_clean"
  )

  expect_equal(out$overview$readiness_status, "warn")
  expect_true(out$overview$ready_for_real_data_analysis)

  duplicate_check <- out$checks[out$checks$check_id == "duplicate_participant_trial_time", ]

  expect_equal(duplicate_check$status, "warn")
  expect_true(duplicate_check$observed > 0)
})

test_that("check_gazepoint_real_data_readiness warns for condition imbalance and single conditions", {
  imbalanced_data <- make_test_real_ready_data()
  imbalanced_data$condition <- c(rep("A", 110), rep("B", 10))

  imbalanced_gate <- check_gazepoint_real_data_readiness(
    imbalanced_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    condition_col = "condition",
    max_condition_imbalance_ratio = 3
  )

  expect_equal(imbalanced_gate$overview$readiness_status, "warn")

  imbalance_check <- imbalanced_gate$checks[
    imbalanced_gate$checks$check_id == "condition_imbalance",
  ]

  expect_equal(imbalance_check$status, "warn")
  expect_true(imbalance_check$observed > imbalance_check$threshold)

  single_condition_data <- make_test_real_ready_data()
  single_condition_data$condition <- "A"

  single_gate <- check_gazepoint_real_data_readiness(
    single_condition_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    condition_col = "condition"
  )

  expect_equal(single_gate$overview$readiness_status, "warn")
  expect_equal(
    single_gate$checks$status[single_gate$checks$check_id == "condition_count"],
    "warn"
  )
})

test_that("check_gazepoint_real_data_readiness handles upstream audit objects", {
  toy_data <- make_test_real_ready_data()

  pass_audit <- list(
    overview = tibble::tibble(audit_status = "complete")
  )
  class(pass_audit) <- c("mock_pass_audit", "list")

  warn_audit <- list(
    overview = tibble::tibble(audit_status = "review_needed")
  )
  class(warn_audit) <- c("mock_warn_audit", "list")

  fail_audit <- list(
    overview = tibble::tibble(audit_status = "fail")
  )
  class(fail_audit) <- c("mock_fail_audit", "list")

  warn_gate <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    audit_objects = list(pass_audit, warn_audit)
  )

  expect_equal(warn_gate$overview$readiness_status, "warn")
  expect_true(warn_gate$overview$ready_for_real_data_analysis)
  expect_true(any(warn_gate$checks$check_area == "upstream_audits"))
  expect_true(any(warn_gate$checks$status == "warn"))

  fail_gate <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    audit_objects = list(fail_audit)
  )

  expect_equal(fail_gate$overview$readiness_status, "fail")
  expect_false(fail_gate$overview$ready_for_real_data_analysis)
  expect_true(any(fail_gate$checks$status == "fail"))
})

test_that("check_gazepoint_real_data_readiness handles uninterpretable audit objects", {
  toy_data <- make_test_real_ready_data()

  out <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    audit_objects = list(list(a = 1))
  )

  audit_check <- out$checks[out$checks$check_area == "upstream_audits", ]

  expect_equal(audit_check$status, "info")
  expect_equal(audit_check$severity, "informational")
  expect_match(audit_check$message, "could not be interpreted", fixed = TRUE)
})

test_that("check_gazepoint_real_data_readiness checks user-required columns", {
  toy_data <- make_test_real_ready_data()

  pass_gate <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    required_cols = c("condition", "stimulus")
  )

  expect_equal(pass_gate$overview$readiness_status, "pass")
  expect_equal(
    pass_gate$checks$status[pass_gate$checks$check_id == "user_required_columns"],
    "pass"
  )

  fail_gate <- check_gazepoint_real_data_readiness(
    toy_data,
    analysis_type = "general",
    participant_col = "subject",
    trial_col = "trial",
    required_cols = c("condition", "missing_column")
  )

  expect_equal(fail_gate$overview$readiness_status, "fail")
  expect_equal(
    fail_gate$checks$status[fail_gate$checks$check_id == "user_required_columns"],
    "fail"
  )
})

test_that("check_gazepoint_real_data_readiness checks invalid inputs", {
  toy_data <- make_test_real_ready_data()

  expect_error(
    check_gazepoint_real_data_readiness(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      trial_col = "bad_trial"
    ),
    "`trial_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      required_cols = NA_character_
    ),
    "`required_cols` must be a character vector",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      audit_objects = "bad"
    ),
    "`audit_objects` must be NULL, a data frame, or a list",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      min_rows = 0
    ),
    "`min_rows` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      min_participants = 1.5
    ),
    "`min_participants` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      max_missing_pupil_prop = 1.1
    ),
    "`max_missing_pupil_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      max_missing_gaze_prop = -0.1
    ),
    "`max_missing_gaze_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      max_condition_imbalance_ratio = 0
    ),
    "`max_condition_imbalance_ratio` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    check_gazepoint_real_data_readiness(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
