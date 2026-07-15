test_that("integrated signal preprocessing returns structured outputs", {
  n <- 80L

  dat <- data.frame(
    USER_ID = rep("P01", n),
    trial = rep("T01", n),
    TIME = seq(0, by = 0.01, length.out = n),
    FPOGX = c(
      rep(0.25, 35),
      seq(0.25, 0.75, length.out = 10),
      rep(0.75, 35)
    ),
    FPOGY = rep(0.50, n),
    LPupil = rep(3.2, n),
    RPupil = rep(3.1, n),
    stringsAsFactors = FALSE
  )

  dat$LPupil[20:23] <- NA_real_
  dat$RPupil[20:23] <- NA_real_

  result <- preprocess_gazepoint_signals(
    dat,
    group_cols = "trial",
    pupil_mode = "mean",
    downsample_factor = 2,
    blink_args = list(
      min_duration = 20,
      include_rapid_changes = FALSE
    ),
    fixation_args = list(
      vmax = 5,
      min_duration = 40
    )
  )

  expect_s3_class(
    result,
    "gp3_signal_preprocessing_result"
  )

  expect_true(
    all(
      names(dat) %in% names(result$data)
    )
  )

  expect_true(
    all(
      c(
        "gp3_pupil_fused",
        "gp3_pupil_fused_blink_interp",
        "pupil_smoothed",
        "FPOGX_smooth",
        "FPOGY_smooth"
      ) %in% names(result$data)
    )
  )

  expect_true(is.data.frame(result$blinks))
  expect_true(is.data.frame(result$fixations))
  expect_true(is.list(result$diagnostics))
  expect_true(is.data.frame(result$decision_log))
  expect_true(is.list(result$settings))
  expect_equal(nrow(result$data), 40L)

  expect_equal(
    result$decision_log$operation,
    c(
      "binocular_pupil_mean",
      "blink_detection",
      "blink_interpolation",
      "pupil_smoothing",
      "coordinate_smoothing",
      "velocity_fixation_detection",
      "downsampling"
    )
  )

  expect_true(
    all(result$decision_log$status %in% c("applied", "skipped"))
  )

  expect_equal(
    result$diagnostics$overview$workflow_status,
    "ok"
  )
})

test_that("regression pupil fusion is supported", {
  n <- 30L

  dat <- data.frame(
    USER_ID = rep("P01", n),
    TIME = seq(0, by = 0.01, length.out = n),
    FPOGX = rep(0.4, n),
    FPOGY = rep(0.6, n),
    LPupil = seq(3, 4, length.out = n),
    RPupil = seq(3.1, 4.1, length.out = n),
    stringsAsFactors = FALSE
  )

  result <- preprocess_gazepoint_signals(
    dat,
    pupil_mode = "regression",
    detect_blinks = FALSE,
    interpolate_blinks = FALSE,
    smooth_pupil = FALSE,
    smooth_coordinates = FALSE,
    detect_fixations = FALSE
  )

  expect_true(
    "gp3_pupil_fused" %in% names(result$data)
  )

  expect_equal(
    nrow(result$data),
    nrow(dat)
  )

  expect_equal(
    result$settings$pupil_mode,
    "regression"
  )
})

test_that("an existing pupil column can be used without fusion", {
  dat <- data.frame(
    USER_ID = rep("P01", 20),
    TIME = seq(0, by = 0.01, length.out = 20),
    FPOGX = rep(0.3, 20),
    FPOGY = rep(0.4, 20),
    pupil = seq(3, 3.5, length.out = 20),
    stringsAsFactors = FALSE
  )

  result <- preprocess_gazepoint_signals(
    dat,
    pupil_mode = "none",
    pupil_col = "pupil",
    detect_blinks = FALSE,
    interpolate_blinks = FALSE,
    smooth_pupil = FALSE,
    smooth_coordinates = FALSE,
    detect_fixations = FALSE
  )

  expect_equal(
    result$settings$input_pupil_col,
    "pupil"
  )

  expect_equal(
    result$data$pupil,
    dat$pupil
  )
})

test_that("blink interpolation requires blink detection", {
  dat <- data.frame(
    USER_ID = rep("P01", 10),
    TIME = seq(0, by = 0.01, length.out = 10),
    FPOGX = rep(0.3, 10),
    FPOGY = rep(0.4, 10),
    pupil = rep(3.2, 10),
    stringsAsFactors = FALSE
  )

  expect_error(
    preprocess_gazepoint_signals(
      dat,
      pupil_mode = "none",
      pupil_col = "pupil",
      detect_blinks = FALSE,
      interpolate_blinks = TRUE,
      smooth_pupil = FALSE,
      smooth_coordinates = FALSE,
      detect_fixations = FALSE
    ),
    "requires"
  )
})

test_that("workflow-managed arguments cannot be overridden", {
  dat <- data.frame(
    USER_ID = rep("P01", 10),
    TIME = seq(0, by = 0.01, length.out = 10),
    FPOGX = rep(0.3, 10),
    FPOGY = rep(0.4, 10),
    LPupil = rep(3.2, 10),
    RPupil = rep(3.1, 10),
    stringsAsFactors = FALSE
  )

  expect_error(
    preprocess_gazepoint_signals(
      dat,
      pupil_args = list(
        output_col = "different_name"
      ),
      detect_blinks = FALSE,
      interpolate_blinks = FALSE,
      smooth_pupil = FALSE,
      smooth_coordinates = FALSE,
      detect_fixations = FALSE
    ),
    "cannot be overridden"
  )
})

test_that("invalid downsampling factors are rejected", {
  dat <- data.frame(
    USER_ID = "P01",
    TIME = 0,
    FPOGX = 0.3,
    FPOGY = 0.4,
    LPupil = 3.2,
    RPupil = 3.1
  )

  expect_error(
    preprocess_gazepoint_signals(
      dat,
      downsample_factor = 0
    ),
    "positive integer"
  )
})
