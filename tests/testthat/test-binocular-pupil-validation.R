test_that("pseudo-missing validation is deterministic and measures held-out error", {
  dat <- make_binocular_test_data(n = 180)
  a <- validate_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject",
    mask_prop = 0.10, repeats = 2, min_pairs = 20, seed = 101
  )
  b <- validate_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject",
    mask_prop = 0.10, repeats = 2, min_pairs = 20, seed = 101
  )

  expect_s3_class(a, "gp3_binocular_validation")
  expect_equal(a$metrics, b$metrics)
  expect_equal(a$predictions, b$predictions)
  expect_true(all(a$summary$prediction_rate > 0))
  expect_true(all(a$summary$rmse < 1e-8, na.rm = TRUE))
})

test_that("contiguous artificial loss records gap durations", {
  dat <- make_binocular_test_data(n = 180)
  val <- validate_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject",
    mask_prop = 0.10, mask_mode = "contiguous", block_size = 4,
    repeats = 1, min_pairs = 20, seed = 102
  )
  expect_true(nrow(val$predictions) > 0)
  expect_true(any(is.finite(val$predictions$gap_ms)))
})

test_that("stress testing covers declared missingness cells", {
  dat <- make_binocular_test_data(n = 180)
  out <- stress_test_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
    group_cols = "subject", missingness = c(0.05, 0.15),
    mask_modes = "random", repeats = 1, min_pairs = 20, seed = 103
  )
  expect_s3_class(out, "gp3_binocular_stress_test")
  expect_setequal(unique(out$results$missingness), c(0.05, 0.15))
  expect_true(all(out$results$mask_mode == "random"))
})

test_that("audit reports burden, shift, and condition imbalance", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_left[dat$condition == "A" & seq_len(nrow(dat)) %% 5 == 0] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
  )
  aud <- audit_gazepoint_binocular_reconstruction(rec, by = "condition")

  expect_s3_class(aud, "gp3_binocular_audit")
  expect_true(aud$overall$n_reconstructed[[1]] > 0)
  expect_equal(nrow(aud$by_group), 2)
  expect_true("mean_absolute_reconstruction_shift" %in% names(aud$reconstruction_shift))
  expect_true(aud$audit$status[[1]] %in% c("descriptive", "ok", "review"))
})

test_that("sensitivity compares retention and policy disagreement descriptively", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_right[25:35] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
  )
  sens <- analyse_gazepoint_binocular_sensitivity(
    rec, "pupil_left", "pupil_right",
    policies = c("complete_case", "available_eye", "reconstructed_mean"),
    condition_col = "condition"
  )

  expect_s3_class(sens, "gp3_binocular_sensitivity")
  expect_setequal(unique(sens$summary$policy),
                  c("complete_case", "available_eye", "reconstructed_mean"))
  expect_true(nrow(sens$correlations) == 3)
  expect_true(nrow(sens$condition_summary) > 0)
  expect_true(nrow(sens$condition_contrasts) > 0)
})

test_that("reporting remains explicit about predicted rather than measured values", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_left[30:34] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
  )
  val <- validate_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right", group_cols = "subject",
    mask_prop = 0.05, repeats = 1, min_pairs = 20, seed = 104
  )
  rep <- summarise_gazepoint_binocular_reporting(rec, validation = val)

  expect_s3_class(rep, "gp3_binocular_reporting")
  expect_match(rep$text, "model-based cross-eye reconstruction")
  expect_match(rep$text, "predicted values")
  expect_match(rep$text, "not treated as independently measured")
  expect_true(length(rep$limitations) >= 3)
})

test_that("validation fails clearly without bilateral ground truth", {
  dat <- make_binocular_test_data(n = 50)
  dat$pupil_left[] <- NA_real_
  expect_error(
    validate_gazepoint_binocular_reconstruction(
      dat, "pupil_left", "pupil_right", min_pairs = 10
    ),
    "bilateral observations"
  )
})
