test_that("data-level binocular plots return ggplot objects", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_left[30:34] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right",
    time_col = "timestamp_ms", group_cols = "subject", min_pairs = 20
  )

  for (type in c("trace", "agreement", "bland_altman", "timeline", "gaps")) {
    p <- plot_gazepoint_binocular_diagnostics(rec, type = type)
    expect_s3_class(p, "ggplot")
  }
})

test_that("validation plots return ggplot objects", {
  dat <- make_binocular_test_data(n = 160)
  val <- validate_gazepoint_binocular_reconstruction(
    dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
    group_cols = "subject", mask_prop = 0.08, repeats = 1,
    min_pairs = 20, seed = 105
  )
  expect_s3_class(plot_gazepoint_binocular_diagnostics(val, "validation"), "ggplot")
  expect_s3_class(plot_gazepoint_binocular_diagnostics(val, "residuals"), "ggplot")
})

test_that("audit and sensitivity plots return ggplot objects", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_right[25:35] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
  )
  aud <- audit_gazepoint_binocular_reconstruction(rec, by = "condition")
  sens <- analyse_gazepoint_binocular_sensitivity(
    rec, "pupil_left", "pupil_right",
    policies = c("complete_case", "available_eye", "reconstructed_mean")
  )
  expect_s3_class(plot_gazepoint_binocular_diagnostics(aud, "burden"), "ggplot")
  expect_s3_class(plot_gazepoint_binocular_diagnostics(sens, "sensitivity"), "ggplot")
})

test_that("dashboard returns a list without adding a layout dependency", {
  dat <- make_binocular_test_data(n = 160)
  dat$pupil_left[40:43] <- NA_real_
  rec <- reconstruct_gazepoint_binocular_pupil(
    dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
    group_cols = "subject", min_pairs = 20
  )
  dash <- plot_gazepoint_binocular_diagnostics(rec, "dashboard")
  expect_type(dash, "list")
  expect_true(all(vapply(dash, inherits, logical(1), what = "ggplot")))
})
