make_artifact_pupil_data <- function() {
  tibble::tibble(
    subject = rep("P1", 6),
    MEDIA_ID = rep("M1", 6),
    time = c(0, 10, 20, 30, 40, 50),
    mean_pupil = c(3.0, 3.1, NA, 3.2, 3.3, 3.4),
    left_pupil = c(3.0, 3.1, NA, 3.2, 3.3, 3.4),
    right_pupil = c(3.0, 3.1, NA, 3.2, 3.3, 3.4),
    blink = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    Trackloss = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
    pupil_unit = rep("diameter_mm", 6)
  )
}

test_that("flag_gazepoint_pupil_artifacts returns expected columns", {
  data <- make_artifact_pupil_data()

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), nrow(data))

  expected_cols <- c(
    "pupil_artifact_raw_value",
    "left_pupil_artifact_raw_value",
    "right_pupil_artifact_raw_value",
    "pupil_unit_text",
    "pupil_unit_is_mm",
    "pupil_artifact_nonfinite",
    "pupil_artifact_nonpositive",
    "pupil_physio_outlier",
    "pupil_physio_outlier_candidate",
    "pupil_physio_candidate_prop",
    "pupil_physio_rule_suppressed",
    "pupil_lr_absdiff",
    "pupil_binocular_disagreement_threshold",
    "pupil_binocular_disagreement",
    "pupil_speed",
    "pupil_speed_abs",
    "pupil_speed_threshold",
    "pupil_speed_outlier",
    "pupil_flag_missing_source",
    "pupil_flag_blink_source",
    "pupil_flag_trackloss_source",
    "pupil_flag_prior_invalid_source",
    "pupil_bad_sample_basic",
    "pupil_artifact_padding_flag",
    "pupil_artifact_flag",
    "pupil_artifact_reason",
    "pupil_clean",
    "pupil_artifact_pupil_column",
    "pupil_artifact_left_pupil_column",
    "pupil_artifact_right_pupil_column",
    "pupil_artifact_time_column",
    "pupil_artifact_unit_column",
    "pupil_artifact_blink_column",
    "pupil_artifact_trackloss_column",
    "pupil_artifact_missing_pupil_column",
    "pupil_artifact_padding_pre_ms",
    "pupil_artifact_padding_post_ms",
    "pupil_artifact_min_mm",
    "pupil_artifact_max_mm",
    "pupil_artifact_speed_mad_k",
    "pupil_artifact_binocular_mad_k",
    "pupil_artifact_max_physio_outlier_prop"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_equal(result$pupil_artifact_pupil_column[1], "mean_pupil")
  expect_equal(result$pupil_artifact_time_column[1], "time")
  expect_equal(result$pupil_artifact_unit_column[1], "pupil_unit")
})

test_that("flag_gazepoint_pupil_artifacts preserves original columns", {
  data <- make_artifact_pupil_data()

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0
  )

  expect_equal(result$mean_pupil, data$mean_pupil)
  expect_equal(result$left_pupil, data$left_pupil)
  expect_equal(result$right_pupil, data$right_pupil)
  expect_equal(result$time, data$time)
})

test_that("flag_gazepoint_pupil_artifacts creates clean pupil values", {
  data <- make_artifact_pupil_data()

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0
  )

  expect_true(is.na(result$pupil_clean[3]))
  expect_false(is.na(result$pupil_clean[1]))
  expect_true(result$pupil_flag_missing_source[3])
  expect_true(result$pupil_flag_blink_source[3])
  expect_true(result$pupil_flag_trackloss_source[3])
  expect_true(result$pupil_artifact_flag[3])
  expect_true(grepl("missing_pupil", result$pupil_artifact_reason[3]))
})

test_that("flag_gazepoint_pupil_artifacts applies blink padding", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 50, 100, 150, 200),
    mean_pupil = rep(3, 5),
    blink = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    Trackloss = rep(FALSE, 5),
    missing_pupil = rep(FALSE, 5),
    pupil_unit = rep("diameter_mm", 5)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 60,
    blink_padding_post_ms = 60,
    flag_speed_outliers = FALSE
  )

  expect_equal(
    result$pupil_artifact_padding_flag,
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )

  expect_true(all(is.na(result$pupil_clean[c(2, 3, 4)])))
  expect_false(is.na(result$pupil_clean[1]))
  expect_false(is.na(result$pupil_clean[5]))
})

test_that("flag_gazepoint_pupil_artifacts keeps padding within groups", {
  data <- tibble::tibble(
    subject = c("P1", "P2"),
    MEDIA_ID = c("M1", "M1"),
    time = c(100, 100),
    mean_pupil = c(3, 3),
    blink = c(TRUE, FALSE),
    Trackloss = c(FALSE, FALSE),
    missing_pupil = c(FALSE, FALSE),
    pupil_unit = c("diameter_mm", "diameter_mm")
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 50,
    blink_padding_post_ms = 50,
    flag_speed_outliers = FALSE
  )

  expect_true(result$pupil_artifact_padding_flag[1])
  expect_false(result$pupil_artifact_padding_flag[2])
})

test_that("flag_gazepoint_pupil_artifacts can use global padding", {
  data <- tibble::tibble(
    subject = c("P1", "P2"),
    MEDIA_ID = c("M1", "M1"),
    time = c(100, 100),
    mean_pupil = c(3, 3),
    blink = c(TRUE, FALSE),
    Trackloss = c(FALSE, FALSE),
    missing_pupil = c(FALSE, FALSE),
    pupil_unit = c("diameter_mm", "diameter_mm")
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    group_cols = character(0),
    blink_padding_pre_ms = 50,
    blink_padding_post_ms = 50,
    flag_speed_outliers = FALSE
  )

  expect_true(result$pupil_artifact_padding_flag[1])
  expect_true(result$pupil_artifact_padding_flag[2])
})

test_that("flag_gazepoint_pupil_artifacts detects pupil speed outliers", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    mean_pupil = c(3, 3, 3, 10, 3),
    blink = rep(FALSE, 5),
    Trackloss = rep(FALSE, 5),
    missing_pupil = rep(FALSE, 5),
    pupil_unit = rep("arbitrary_units", 5)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0,
    pupil_speed_mad_k = 0,
    flag_physiological_outliers = FALSE
  )

  expect_true(any(result$pupil_speed_outlier, na.rm = TRUE))
  expect_true(any(grepl("pupil_speed_outlier", result$pupil_artifact_reason)))
})

test_that("flag_gazepoint_pupil_artifacts applies physiological rule when appropriate", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    mean_pupil = c(3, 4, 5, 20, 6),
    blink = rep(FALSE, 5),
    Trackloss = rep(FALSE, 5),
    missing_pupil = rep(FALSE, 5),
    pupil_unit = rep("diameter_mm", 5)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0,
    flag_speed_outliers = FALSE
  )

  expect_equal(sum(result$pupil_physio_outlier_candidate), 1)
  expect_equal(sum(result$pupil_physio_outlier), 1)
  expect_false(unique(result$pupil_physio_rule_suppressed))
  expect_true(is.na(result$pupil_clean[4]))
})

test_that("flag_gazepoint_pupil_artifacts suppresses physiological rule when it would wipe the series", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    mean_pupil = c(10, 11, 12, 13, 14),
    blink = rep(FALSE, 5),
    Trackloss = rep(FALSE, 5),
    missing_pupil = rep(FALSE, 5),
    pupil_unit = rep("diameter_mm", 5)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0,
    flag_speed_outliers = FALSE
  )

  expect_equal(sum(result$pupil_physio_outlier_candidate), 5)
  expect_equal(sum(result$pupil_physio_outlier), 0)
  expect_true(unique(result$pupil_physio_rule_suppressed))
  expect_equal(unique(result$pupil_physio_candidate_prop), 1)
  expect_equal(sum(!is.na(result$pupil_clean)), 5)
})

test_that("flag_gazepoint_pupil_artifacts detects binocular disagreement", {
  data <- tibble::tibble(
    subject = rep("P1", 5),
    MEDIA_ID = rep("M1", 5),
    time = c(0, 10, 20, 30, 40),
    mean_pupil = rep(3, 5),
    left_pupil = c(3, 3, 8, 3, 3),
    right_pupil = c(3.01, 3.01, 3, 3.01, 3.01),
    blink = rep(FALSE, 5),
    Trackloss = rep(FALSE, 5),
    missing_pupil = rep(FALSE, 5),
    pupil_unit = rep("diameter_mm", 5)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0,
    flag_speed_outliers = FALSE
  )

  expect_true(any(result$pupil_binocular_disagreement, na.rm = TRUE))
  expect_true(any(grepl("binocular_pupil_disagreement", result$pupil_artifact_reason)))
})

test_that("flag_gazepoint_pupil_artifacts uses registry values", {
  data <- make_artifact_pupil_data()

  registry <- create_gazepoint_preprocessing_registry(
    blink_padding_pre_ms = 10,
    blink_padding_post_ms = 20,
    pupil_physiological_min = 1.5,
    pupil_physiological_max = 8.5,
    pupil_speed_mad_k = 5,
    binocular_mad_k = 7
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    registry = registry
  )

  expect_equal(result$pupil_artifact_padding_pre_ms[1], 10)
  expect_equal(result$pupil_artifact_padding_post_ms[1], 20)
  expect_equal(result$pupil_artifact_min_mm[1], 1.5)
  expect_equal(result$pupil_artifact_max_mm[1], 8.5)
  expect_equal(result$pupil_artifact_speed_mad_k[1], 5)
  expect_equal(result$pupil_artifact_binocular_mad_k[1], 7)
})

test_that("flag_gazepoint_pupil_artifacts supports explicit columns", {
  data <- tibble::tibble(
    participant = rep("P1", 3),
    media_id = rep("M1", 3),
    custom_time = c(0, 10, 20),
    custom_pupil = c(3, NA, 3.2),
    custom_left = c(3, NA, 3.2),
    custom_right = c(3, NA, 3.2),
    custom_missing = c(FALSE, TRUE, FALSE),
    custom_blink = c(FALSE, TRUE, FALSE),
    custom_trackloss = c(FALSE, TRUE, FALSE),
    custom_unit = rep("diameter_mm", 3)
  )

  result <- flag_gazepoint_pupil_artifacts(
    data,
    pupil_col = "custom_pupil",
    left_pupil_col = "custom_left",
    right_pupil_col = "custom_right",
    time_col = "custom_time",
    blink_col = "custom_blink",
    trackloss_col = "custom_trackloss",
    missing_pupil_col = "custom_missing",
    pupil_unit_col = "custom_unit",
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0
  )

  expect_equal(result$pupil_artifact_pupil_column[1], "custom_pupil")
  expect_equal(result$pupil_artifact_time_column[1], "custom_time")
  expect_equal(result$pupil_artifact_blink_column[1], "custom_blink")
  expect_true(result$pupil_artifact_flag[2])
})

test_that("flag_gazepoint_pupil_artifacts replaces pre-existing output columns", {
  data <- make_artifact_pupil_data()
  data$pupil_clean <- -999
  data$pupil_artifact_reason <- "old"

  result <- flag_gazepoint_pupil_artifacts(
    data,
    blink_padding_pre_ms = 0,
    blink_padding_post_ms = 0
  )

  expect_equal(sum(names(result) == "pupil_clean"), 1)
  expect_equal(sum(names(result) == "pupil_artifact_reason"), 1)
  expect_false(any(result$pupil_artifact_reason == "old"))
  expect_false(any(result$pupil_clean == -999, na.rm = TRUE))
})

test_that("flag_gazepoint_pupil_artifacts validates arguments", {
  data <- make_artifact_pupil_data()

  expect_error(
    flag_gazepoint_pupil_artifacts("not a data frame"),
    "`data` must be a data frame"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, pupil_col = c("a", "b")),
    "`pupil_col` must be `NULL` or a single character string"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, group_cols = 1),
    "`group_cols` must be a character vector"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, flag_speed_outliers = c(TRUE, FALSE)),
    "`flag_speed_outliers` must be `TRUE` or `FALSE`"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, blink_padding_pre_ms = NA_real_),
    "`blink_padding_pre_ms` must be a single non-missing numeric value"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, blink_padding_pre_ms = -1),
    "`blink_padding_pre_ms` must be greater than or equal to 0"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, pupil_max_mm = 1, pupil_min_mm = 9),
    "`pupil_max_mm` must be greater than `pupil_min_mm`"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, max_physio_outlier_prop = 1.5),
    "`max_physio_outlier_prop` must be between 0 and 1"
  )
})

test_that("flag_gazepoint_pupil_artifacts errors when required columns are missing", {
  data <- make_artifact_pupil_data()

  no_pupil <- data
  no_pupil$mean_pupil <- NULL
  no_pupil$left_pupil <- NULL
  no_pupil$right_pupil <- NULL

  expect_error(
    flag_gazepoint_pupil_artifacts(no_pupil),
    "No pupil column was found"
  )

  no_time <- data
  no_time$time <- NULL

  expect_error(
    flag_gazepoint_pupil_artifacts(no_time),
    "No time column was found"
  )

  no_subject <- data
  no_subject$subject <- NULL

  expect_error(
    flag_gazepoint_pupil_artifacts(no_subject),
    "requested but not found"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, group_cols = "condition"),
    "requested but not found"
  )

  expect_error(
    flag_gazepoint_pupil_artifacts(data, blink_col = "bad_blink_col"),
    "`blink_col` was not found in `data`"
  )
})
