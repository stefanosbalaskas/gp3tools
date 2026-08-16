test_that("diagnostics report binocular availability and agreement", {
  dat <- make_binocular_test_data()
  dat$pupil_left[10:12] <- NA_real_
  dat$pupil_right[30:31] <- NA_real_

  out <- diagnose_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject", min_pairs = 20
  )

  expect_s3_class(out, "gp3_binocular_diagnostics")
  expect_true(all(c("n_bilateral", "correlation", "rmse_between_eyes",
                    "longest_left_gap_ms", "calibration_eligible") %in%
                  names(out$summary)))
  expect_equal(sum(out$summary$n_left_only), 2)
  expect_equal(sum(out$summary$n_right_only), 3)
  expect_true(nrow(out$gaps) >= 2)
})

test_that("calibration fits separate directions and preserves fallback provenance", {
  dat <- make_binocular_test_data()
  fit <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right",
    group_cols = "subject", min_pairs = 20
  )

  expect_s3_class(fit, "gp3_binocular_calibration")
  expect_true(all(c("left_from_right", "right_from_left") %in% fit$models$direction))
  expect_true(any(fit$models$calibration_level == "pooled"))
  local <- fit$models[
    fit$models$calibration_level == "subject" &
      fit$models$direction == "left_from_right", , drop = FALSE
  ]
  expect_true(all(local$eligible))
  expect_equal(local$slope, rep(1.1, nrow(local)), tolerance = 1e-10)
  expect_equal(local$intercept, rep(0.4, nrow(local)), tolerance = 1e-10)
})

test_that("reconstruction is bidirectional, explicit, and non-destructive", {
  dat <- make_binocular_test_data()
  original <- dat
  dat$pupil_left[20:22] <- NA_real_
  dat$pupil_right[70:72] <- NA_real_

  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject",
    min_pairs = 20, max_gap_ms = 100
  )

  expect_equal(rec$pupil_left, dat$pupil_left)
  expect_equal(rec$pupil_right, dat$pupil_right)
  expect_equal(rec$unrelated, original$unrelated)
  expect_true(all(rec$gp3_binocular_left_reconstructed[20:22]))
  expect_true(all(rec$gp3_binocular_right_reconstructed[70:72]))
  expect_true(all(rec$gp3_binocular_status[20:22] == "left_reconstructed"))
  expect_true(all(rec$gp3_binocular_status[70:72] == "right_reconstructed"))
  expect_equal(
    rec$gp3_binocular_left_final[20:22],
    0.4 + 1.1 * rec$pupil_right[20:22],
    tolerance = 1e-10
  )
  expect_true(all(!is.na(rec$gp3_binocular_model_id[c(20:22, 70:72)])))
})

test_that("gap and edge rules can block otherwise eligible reconstruction", {
  dat <- make_binocular_test_data()
  dat$pupil_left[20:30] <- NA_real_

  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject",
    min_pairs = 20, max_gap_ms = 40
  )
  expect_false(any(rec$gp3_binocular_left_reconstructed[20:30]))
  expect_true(all(rec$gp3_binocular_status[20:30] == "reconstruction_blocked_gap"))

  edge_dat <- make_binocular_test_data()
  edge_dat$pupil_left[1:2] <- NA_real_
  edge <- reconstruct_gazepoint_binocular_pupil(
    edge_dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", min_pairs = 20,
    allow_edge_gaps = FALSE
  )
  expect_true(all(edge$gp3_binocular_status[1:2] == "reconstruction_blocked_edge"))
})

test_that("extrapolation, exclusion flags, and prediction bounds are explicit", {
  dat <- make_binocular_test_data()
  dat$pupil_left[15] <- NA_real_
  dat$pupil_right[15] <- 9
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 20,
    allow_extrapolation = FALSE
  )
  expect_identical(rec$gp3_binocular_status[[15]], "reconstruction_blocked_extrapolation")
  expect_false(rec$gp3_binocular_left_reconstructed[[15]])

  dat2 <- make_binocular_test_data()
  dat2$do_not_use <- FALSE
  dat2$do_not_use[25] <- TRUE
  dat2$pupil_left[25] <- NA_real_
  rec2 <- reconstruct_gazepoint_binocular_pupil(
    dat2, "pupil_left", "pupil_right", min_pairs = 20,
    exclude_flag_cols = "do_not_use"
  )
  expect_identical(rec2$gp3_binocular_status[[25]], "reconstruction_blocked_exclusion")

  dat3 <- make_binocular_test_data()
  dat3$pupil_left[40] <- NA_real_
  dat3$pupil_right[40] <- 4.9
  rec3 <- reconstruct_gazepoint_binocular_pupil(
    dat3, "pupil_left", "pupil_right", min_pairs = 20,
    valid_max = 5.2, allow_extrapolation = TRUE
  )
  expect_identical(
    rec3$gp3_binocular_status[[40]],
    "reconstruction_blocked_bounds"
  )
  expect_false(rec3$gp3_binocular_left_reconstructed[[40]])
})

test_that("insufficient calibration is returned as a status rather than silent imputation", {
  dat <- make_binocular_test_data(n = 20)
  dat$pupil_left[5] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 50,
    fallback_group_cols = list()
  )
  expect_identical(rec$gp3_binocular_status[[5]], "reconstruction_ineligible")
  expect_true(is.na(rec$gp3_binocular_left_final[[5]]))
})

test_that("available-eye and none policies never synthesize a missing eye", {
  dat <- make_binocular_test_data()
  dat$pupil_left[10] <- NA_real_
  for (method in c("available_eye", "none")) {
    rec <- reconstruct_gazepoint_binocular_pupil(
      dat, "pupil_left", "pupil_right", method = method
    )
    expect_false(rec$gp3_binocular_left_reconstructed[[10]])
    expect_true(is.na(rec$gp3_binocular_left_final[[10]]))
    expect_identical(rec$gp3_binocular_status[[10]], "right_only_observed")
  }
})

test_that("combined pupil policies preserve provenance", {
  dat <- make_binocular_test_data()
  dat$pupil_left[10] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 20
  )
  combined <- construct_gazepoint_combined_pupil(
    rec, "pupil_left", "pupil_right", policy = "reconstructed_mean"
  )
  expect_true(is.finite(combined$pupil_binocular[[10]]))
  expect_identical(combined$pupil_binocular_status[[10]], "bilateral_with_reconstruction")

  complete <- construct_gazepoint_combined_pupil(
    dat, "pupil_left", "pupil_right", policy = "complete_case"
  )
  expect_true(is.na(complete$pupil_binocular[[10]]))
})

test_that("input validation catches unsafe configurations", {
  dat <- make_binocular_test_data()
  expect_error(
    reconstruct_gazepoint_binocular_pupil(
      dat, "pupil_left", "pupil_right", max_gap_ms = 100
    ),
    "time_col"
  )
  expect_error(
    diagnose_gazepoint_binocular_pupil(dat, "missing", "pupil_right"),
    "Missing"
  )
  dat$gp3_binocular_status <- "existing"
  expect_error(
    reconstruct_gazepoint_binocular_pupil(dat, "pupil_left", "pupil_right"),
    "already exist"
  )
})


test_that("diagnostics handle all-missing, one-eye-only, and time-quality edge cases", {
  all_missing <- make_binocular_test_data(n = 30)
  all_missing$pupil_left[] <- NA_real_
  all_missing$pupil_right[] <- NA_real_
  out <- diagnose_gazepoint_binocular_pupil(
    all_missing, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", min_pairs = 10
  )
  expect_equal(out$summary$n_bilateral[[1]], 0)
  expect_equal(out$summary$n_both_missing[[1]], 30)
  expect_false(out$summary$calibration_eligible[[1]])

  one_eye <- make_binocular_test_data(n = 30)
  one_eye$pupil_left[] <- NA_real_
  out_one <- diagnose_gazepoint_binocular_pupil(
    one_eye, "pupil_left", "pupil_right", min_pairs = 10
  )
  expect_equal(out_one$summary$n_right_only[[1]], 30)
  expect_equal(out_one$summary$n_left[[1]], 0)

  time_bad <- make_binocular_test_data(n = 30)
  time_bad$timestamp_ms[10] <- time_bad$timestamp_ms[9]
  time_bad$timestamp_ms[c(20, 21)] <- time_bad$timestamp_ms[c(21, 20)]
  out_time <- diagnose_gazepoint_binocular_pupil(
    time_bad, "pupil_left", "pupil_right", time_col = "timestamp_ms"
  )
  expect_true(out_time$summary$duplicate_time_count[[1]] >= 1)
  expect_true(out_time$summary$time_unsorted[[1]])
})

test_that("zero-variance calibration is explicitly ineligible", {
  dat <- make_binocular_test_data(n = 60)
  dat$pupil_right <- 4
  fit <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right", min_pairs = 20
  )
  expect_false(any(fit$models$eligible))
  expect_true(any(grepl("variance|unique", fit$models$reason)))
})

test_that("gap grouping is independent from calibration grouping", {
  n_each <- 60L
  dat <- data.frame(
    subject = "S1",
    trial = rep(c("T1", "T2"), each = n_each),
    timestamp_ms = rep(seq(0, by = 1000 / 60, length.out = n_each), 2),
    pupil_right = seq(3, 5, length.out = 2 * n_each),
    stringsAsFactors = FALSE
  )
  dat$pupil_left <- 0.4 + 1.1 * dat$pupil_right
  dat$pupil_left[c(20:22, 80:82)] <- NA_real_

  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms",
    group_cols = "subject",
    gap_group_cols = c("subject", "trial"),
    min_pairs = 20, max_gap_ms = 60
  )

  masked <- c(20:22, 80:82)
  expect_true(all(rec$gp3_binocular_left_reconstructed[masked]))
  expect_true(all(rec$gp3_binocular_gap_ms[masked] <= 60))
})

test_that("numeric-channel validation and non-finite observations are conservative", {
  dat <- make_binocular_test_data(n = 50)
  bad <- dat
  bad$pupil_left <- as.character(bad$pupil_left)
  expect_error(
    diagnose_gazepoint_binocular_pupil(bad, "pupil_left", "pupil_right"),
    "numeric"
  )

  dat$pupil_left[10] <- Inf
  out <- diagnose_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 20
  )
  expect_equal(out$summary$n_right_only[[1]], 1)
})

test_that("invalid reconstruction method is rejected", {
  dat <- make_binocular_test_data()
  expect_error(
    reconstruct_gazepoint_binocular_pupil(
      dat, "pupil_left", "pupil_right", method = "magic"
    ),
    "arg"
  )
})

test_that("prefit calibration carries grouping provenance and rejects channel mismatch", {
  dat <- make_binocular_test_data()
  fit <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
  )
  dat$pupil_left[20:22] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", calibration = fit,
    time_col = "timestamp_ms", max_gap_ms = 100
  )
  meta <- attr(rec, "gp3_binocular_reconstruction")
  expect_identical(meta$group_cols, "subject")
  expect_identical(meta$gap_group_cols, "subject")

  renamed <- dat
  renamed$left_other <- renamed$pupil_left
  expect_error(
    reconstruct_gazepoint_binocular_pupil(
      renamed, "left_other", "pupil_right", calibration = fit
    ),
    "different left/right"
  )
})

test_that("only-left availability and NA grouping identifiers remain diagnosable", {
  dat <- make_binocular_test_data(n = 40)
  dat$pupil_right[] <- NA_real_
  dat$subject[1:5] <- NA_character_
  out <- diagnose_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 10
  )
  expect_equal(sum(out$summary$n_left_only), 40)
  expect_equal(sum(out$summary$n_right), 0)
  expect_true(any(out$summary$group_key == "subject=<NA>"))
})

test_that("groups without local bilateral data require an explicit fallback", {
  dat <- make_binocular_test_data(n = 120, groups = 2)
  s1 <- dat$subject == "S1"
  dat$pupil_left[s1] <- NA_real_

  no_fallback <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right", group_cols = "subject",
    fallback_group_cols = list(), min_pairs = 20
  )
  rec_local <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", calibration = no_fallback
  )
  expect_false(any(rec_local$gp3_binocular_left_reconstructed[s1]))

  with_fallback <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right", group_cols = "subject",
    fallback_group_cols = list(character(0)), min_pairs = 20
  )
  rec_fallback <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", calibration = with_fallback
  )
  expect_true(any(rec_fallback$gp3_binocular_left_reconstructed[s1]))
  expect_true(all(
    rec_fallback$gp3_binocular_calibration_level[
      s1 & rec_fallback$gp3_binocular_left_reconstructed
    ] == "pooled"
  ))
})

test_that("missing timestamps make finite gap gates fail closed", {
  dat <- make_binocular_test_data(n = 80)
  dat$pupil_left[20:22] <- NA_real_
  dat$timestamp_ms[20:22] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
    min_pairs = 20, max_gap_ms = 100
  )
  expect_true(all(
    rec$gp3_binocular_status[20:22] == "reconstruction_blocked_gap"
  ))
})

test_that("prior interpolation can be declared ineligible as predictor support", {
  dat <- make_binocular_test_data(n = 80)
  dat$pupil_left[25] <- NA_real_
  dat$predictor_interpolated <- FALSE
  dat$predictor_interpolated[25] <- TRUE
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 20,
    exclude_flag_cols = "predictor_interpolated"
  )
  expect_identical(
    rec$gp3_binocular_status[[25]],
    "reconstruction_blocked_exclusion"
  )
})

test_that("one-row calibration groups and invalid thresholds fail transparently", {
  dat <- make_binocular_test_data(n = 40)
  dat$singleton <- paste0("row", seq_len(nrow(dat)))
  fit <- fit_gazepoint_binocular_calibration(
    dat, "pupil_left", "pupil_right", group_cols = "singleton",
    fallback_group_cols = list(), min_pairs = 2
  )
  expect_false(any(fit$models$eligible))

  expect_error(
    fit_gazepoint_binocular_calibration(
      dat, "pupil_left", "pupil_right", min_r2 = 1.5
    ),
    "permitted range"
  )
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", min_pairs = 20
  )
  expect_error(
    audit_gazepoint_binocular_reconstruction(
      rec, max_reconstruction_prop = -0.1
    ),
    "permitted range"
  )
})
