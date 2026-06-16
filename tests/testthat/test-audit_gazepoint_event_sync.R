make_test_event_sync_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 8),
    MEDIA_ID = rep(1, 16),
    trial_global = rep(rep(1:2, each = 4), 2),
    condition = rep(c("A", "B", "A", "B"), each = 4),
    time = rep(c(0, 50, 100, 150), 4),
    event_label = c(
      "onset", "", "response", "",
      "onset", "", "response", "",
      "onset", "", "response", "",
      "onset", "", "", ""
    )
  )
}

test_that("audit_gazepoint_event_sync creates a complete event-sync audit", {
  toy_event <- make_test_event_sync_data()

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    condition_col = "condition",
    expected_event_labels = c("onset", "response"),
    onset_event_label = "onset",
    response_event_label = "response",
    min_samples_per_unit = 2,
    max_time_gap_ms = 75
  )

  expect_s3_class(out, "gp3_event_sync_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "unit_summary",
      "event_summary",
      "expected_event_summary",
      "flagged_units",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_true(is.data.frame(out$unit_summary))
  expect_s3_class(out$event_summary, "tbl_df")
  expect_s3_class(out$expected_event_summary, "tbl_df")
  expect_true(is.data.frame(out$flagged_units))
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 16)
  expect_equal(out$overview$n_units, 4)
  expect_equal(out$overview$n_flagged_units, 1)
  expect_equal(out$overview$event_col, "event_label")
  expect_equal(out$overview$has_event_col, TRUE)
  expect_equal(out$overview$has_expected_events, TRUE)
  expect_equal(out$overview$audit_status, "review")

  expect_equal(nrow(out$unit_summary), 4)
  expect_equal(sum(out$unit_summary$event_sync_status == "ok"), 3)
  expect_equal(sum(out$unit_summary$event_sync_status == "missing_expected_events"), 1)

  expect_equal(nrow(out$flagged_units), 1)
  expect_equal(out$flagged_units$subject, "S2")
  expect_equal(out$flagged_units$trial_global, 2)
  expect_equal(out$flagged_units$missing_expected_events, "response")

  response_row <- out$expected_event_summary[
    out$expected_event_summary$expected_event_label == "response",
    ,
    drop = FALSE
  ]

  expect_equal(response_row$n_units_missing, 1)
  expect_equal(response_row$expected_event_status, "missing_in_some_units")
})

test_that("audit_gazepoint_event_sync reports ok when expected events are present", {
  toy_event <- make_test_event_sync_data()
  toy_event$event_label[15] <- "response"

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    condition_col = "condition",
    expected_event_labels = c("onset", "response"),
    onset_event_label = "onset",
    response_event_label = "response",
    min_samples_per_unit = 2,
    max_time_gap_ms = 75
  )

  expect_equal(out$overview$n_flagged_units, 0)
  expect_equal(out$overview$audit_status, "ok")
  expect_equal(nrow(out$flagged_units), 0)
  expect_true(all(out$unit_summary$event_sync_status == "ok"))
  expect_true(all(out$expected_event_summary$expected_event_status == "ok"))
})

test_that("audit_gazepoint_event_sync detects event columns automatically", {
  toy_event <- make_test_event_sync_data()

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    condition_col = "condition",
    expected_event_labels = c("onset", "response")
  )

  expect_equal(out$overview$event_col, "event_label")
  expect_equal(out$overview$has_event_col, TRUE)
  expect_true("media_id" %in% names(out$unit_summary))

  expect_equal(
    out$settings$value[out$settings$setting == "group_cols"],
    "subject, media_id, trial_global"
  )
})

test_that("audit_gazepoint_event_sync flags duplicate time values", {
  toy_event <- make_test_event_sync_data()
  toy_event$time[2] <- 0

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    expected_event_labels = c("onset", "response"),
    min_samples_per_unit = 2,
    max_time_gap_ms = 75
  )

  expect_true("duplicate_time_values" %in% out$unit_summary$event_sync_status)
  expect_true("duplicate_time_values" %in% out$flagged_units$event_sync_status)
  expect_equal(out$overview$audit_status, "review")
})

test_that("audit_gazepoint_event_sync flags large time gaps", {
  toy_event <- make_test_event_sync_data()
  toy_event$time[3] <- 300

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    expected_event_labels = c("onset", "response"),
    min_samples_per_unit = 2,
    max_time_gap_ms = 75
  )

  expect_true("large_time_gap" %in% out$unit_summary$event_sync_status)
  expect_true("large_time_gap" %in% out$flagged_units$event_sync_status)
  expect_equal(out$overview$audit_status, "review")
})

test_that("audit_gazepoint_event_sync flags units with too few samples", {
  toy_event <- make_test_event_sync_data()

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    expected_event_labels = c("onset", "response"),
    min_samples_per_unit = 5
  )

  expect_true(all(out$unit_summary$event_sync_status == "too_few_samples"))
  expect_equal(out$overview$n_flagged_units, 4)
  expect_equal(out$overview$audit_status, "review")
})

test_that("audit_gazepoint_event_sync summarises events by condition", {
  toy_event <- make_test_event_sync_data()

  out <- audit_gazepoint_event_sync(
    toy_event,
    time_col = "time",
    event_col = "event_label",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    condition_col = "condition",
    expected_event_labels = c("onset", "response")
  )

  expect_true("condition" %in% names(out$event_summary))
  expect_true("event_label" %in% names(out$event_summary))
  expect_true("n_event_samples" %in% names(out$event_summary))

  response_b <- out$event_summary[
    out$event_summary$condition == "B" &
      out$event_summary$event_label == "response",
    ,
    drop = FALSE
  ]

  expect_equal(response_b$n_event_samples, 1)
})

test_that("audit_gazepoint_event_sync checks invalid inputs", {
  toy_event <- make_test_event_sync_data()

  expect_error(
    audit_gazepoint_event_sync(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      event_col = "bad_event"
    ),
    "`event_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      group_cols = "bad_group"
    ),
    "At least one usable `group_cols` column must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      min_samples_per_unit = 0
    ),
    "`min_samples_per_unit` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      max_time_gap_ms = 0
    ),
    "`max_time_gap_ms` must be a positive numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      expected_event_labels = character()
    ),
    "`expected_event_labels` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      onset_event_label = NA_character_
    ),
    "`onset_event_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_event_sync(
      toy_event,
      response_event_label = NA_character_
    ),
    "`response_event_label` must be a non-missing character scalar",
    fixed = TRUE
  )
})
