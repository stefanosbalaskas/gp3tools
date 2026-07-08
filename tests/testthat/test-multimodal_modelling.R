test_that("prepare_gazepoint_multimodal_data joins response data and scales predictors", {
  face_windows <- data.frame(
    participant_id = c("P001", "P002", "P003"),
    trial_id = c(1, 1, 1),
    AU12_r_mean = c(0.1, 0.2, 0.3),
    AU04_r_mean = c(0.4, 0.5, 0.6),
    stringsAsFactors = FALSE
  )

  responses <- data.frame(
    participant_id = c("P001", "P002", "P003"),
    trial_id = c(1, 1, 1),
    rating = c(3, 4, 5),
    stringsAsFactors = FALSE
  )

  out <- prepare_gazepoint_multimodal_data(
    face_windows,
    response_data = responses,
    by = c("participant_id", "trial_id"),
    outcome_cols = "rating",
    predictor_cols = c("AU12_r_mean", "AU04_r_mean")
  )

  expect_s3_class(out, "gp3_multimodal_data")
  expect_equal(nrow(out), 3)
  expect_true("rating" %in% names(out))
  expect_true("AU12_r_mean_z" %in% names(out))
  expect_true("AU04_r_mean_z" %in% names(out))
  expect_s3_class(attr(out, "gp3_multimodal_scaling"), "tbl_df")
})


test_that("prepare_gazepoint_multimodal_data joins gaze and response data", {
  face_windows <- data.frame(
    participant_id = c("P001", "P002"),
    trial_id = c(1, 1),
    AU12_r_mean = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  gaze <- data.frame(
    participant_id = c("P001", "P002"),
    trial_id = c(1, 1),
    dwell_time = c(1.2, 1.5),
    stringsAsFactors = FALSE
  )

  responses <- data.frame(
    participant_id = c("P001", "P002"),
    trial_id = c(1, 1),
    rating = c(3, 4),
    stringsAsFactors = FALSE
  )

  out <- prepare_gazepoint_multimodal_data(
    face_windows,
    gaze_data = gaze,
    response_data = responses,
    by = c("participant_id", "trial_id"),
    outcome_cols = "rating",
    predictor_cols = c("AU12_r_mean", "dwell_time")
  )

  expect_true("dwell_time" %in% names(out))
  expect_true("dwell_time_z" %in% names(out))
  expect_equal(out$rating, c(3, 4))
})


test_that("prepare_gazepoint_multimodal_data supports named join mappings", {
  face_windows <- data.frame(
    participant_id = c("P001", "P002"),
    trial_id = c(1, 1),
    AU12_r_mean = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  responses <- data.frame(
    subject = c("P001", "P002"),
    trial = c(1, 1),
    rating = c(3, 4),
    stringsAsFactors = FALSE
  )

  out <- prepare_gazepoint_multimodal_data(
    face_windows,
    response_data = responses,
    by = c("participant_id", "trial_id"),
    response_by = c(participant_id = "subject", trial_id = "trial"),
    outcome_cols = "rating",
    predictor_cols = "AU12_r_mean"
  )

  expect_equal(out$rating, c(3, 4))
})


test_that("fit_gazepoint_face_window_lmm fits lm without random effects", {
  dat <- data.frame(
    participant_id = c("P001", "P002", "P003", "P004"),
    AU12_r_mean = c(0.1, 0.2, 0.3, 0.4),
    rating = c(3, 4, 5, 6),
    stringsAsFactors = FALSE
  )

  fit <- fit_gazepoint_face_window_lmm(
    dat,
    outcome = "rating",
    predictors = "AU12_r_mean"
  )

  expect_s3_class(fit, "gp3_face_window_lmm")
  expect_s3_class(fit$model, "lm")
  expect_equal(fit$settings$outcome, "rating")
  expect_equal(fit$settings$n_rows_model, 4)
})


test_that("fit_gazepoint_multimodal_response_model fits lm", {
  dat <- data.frame(
    participant_id = c("P001", "P002", "P003", "P004"),
    AU12_r_mean = c(0.1, 0.2, 0.3, 0.4),
    dwell_time = c(1.0, 1.2, 1.3, 1.5),
    rating = c(3, 4, 5, 6),
    stringsAsFactors = FALSE
  )

  fit <- fit_gazepoint_multimodal_response_model(
    dat,
    outcome = "rating",
    predictors = c("AU12_r_mean", "dwell_time")
  )

  expect_s3_class(fit, "gp3_multimodal_response_model")
  expect_s3_class(fit$model, "lm")
  expect_true(grepl("AU12_r_mean", deparse(fit$formula)))
  expect_true(grepl("dwell_time", deparse(fit$formula)))
})

test_that("fit_gazepoint_multimodal_response_model fits glm when family supplied", {
  dat <- data.frame(
    AU12_r_mean = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8),
    response = c(0, 1, 0, 1, 0, 1, 0, 1)
  )

  fit <- fit_gazepoint_multimodal_response_model(
    dat,
    outcome = "response",
    predictors = "AU12_r_mean",
    family = stats::binomial()
  )

  expect_s3_class(fit$model, "glm")
  expect_equal(fit$settings$outcome, "response")
})

test_that("multimodal modelling helpers validate inputs", {
  face_windows <- data.frame(
    participant_id = "P001",
    AU12_r_mean = 0.1
  )

  expect_error(
    prepare_gazepoint_multimodal_data(1:3),
    "must be a data frame"
  )

  expect_error(
    prepare_gazepoint_multimodal_data(
      face_windows,
      response_data = data.frame(id = "P001"),
      by = "participant_id"
    ),
    "right column"
  )

  expect_error(
    prepare_gazepoint_multimodal_data(
      face_windows,
      predictor_cols = "missing"
    ),
    "predictor_cols"
  )

  expect_error(
    fit_gazepoint_face_window_lmm(
      face_windows,
      outcome = "missing",
      predictors = "AU12_r_mean"
    ),
    "outcome"
  )

  expect_error(
    fit_gazepoint_multimodal_response_model(
      face_windows,
      outcome = "AU12_r_mean",
      predictors = "missing"
    ),
    "predictors"
  )
})


test_that("multimodal modelling drops incomplete model rows", {
  dat <- data.frame(
    AU12_r_mean = c(0.1, NA, 0.3),
    rating = c(3, 4, 5)
  )

  fit <- fit_gazepoint_face_window_lmm(
    dat,
    outcome = "rating",
    predictors = "AU12_r_mean"
  )

  expect_equal(fit$settings$n_rows_input, 3)
  expect_equal(fit$settings$n_rows_model, 2)
})
