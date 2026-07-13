test_that("create_gazepoint_hddm_fit_script writes a Python script", {
  tmp_csv <- tempfile(fileext = ".csv")
  tmp_py <- tempfile(fileext = ".py")

  dat <- data.frame(
    subj_idx = c(1, 1, 2, 2),
    rt = c(.7, .8, .9, 1.0),
    response = c(1, 0, 1, 0),
    target_dwell_ms_z = c(-.5, .5, -.2, .2),
    pupil_peak_z = c(.1, .2, -.1, -.2)
  )
  utils::write.csv(dat, tmp_csv, row.names = FALSE)

  out <- create_gazepoint_hddm_fit_script(
    data_file = tmp_csv,
    output_file = tmp_py,
    regressions = c(v = "target_dwell_ms_z", a = "pupil_peak_z")
  )

  expect_true(file.exists(out))
  txt <- readLines(out, warn = FALSE)
  expect_true(any(grepl("HDDMRegressor", txt)))
  expect_true(any(grepl("v ~ 1 \\+ target_dwell_ms_z", txt)))
})

test_that("select_gazepoint_adaptive_trial selects candidate", {
  candidates <- data.frame(
    stimulus = c("A", "B", "C"),
    posterior_mean = c(.2, .4, .3),
    posterior_sd = c(.1, .5, .2)
  )

  out <- select_gazepoint_adaptive_trial(
    candidates,
    mean = "posterior_mean",
    sd = "posterior_sd",
    acquisition = "ucb",
    kappa = 2
  )

  expect_equal(nrow(out), 1)
  expect_true("acquisition_score" %in% names(out))
  expect_equal(out$stimulus, "B")
})

test_that("classify_gazepoint_events_hmm returns states", {
  dat <- data.frame(
    subject = rep("S1", 30),
    time = seq_len(30),
    x = cumsum(c(rep(.01, 10), rep(5, 10), rep(.2, 10))),
    y = cumsum(c(rep(.01, 10), rep(5, 10), rep(.2, 10)))
  )

  out <- classify_gazepoint_events_hmm(
    dat,
    x = "x",
    y = "y",
    time = "time",
    subject = "subject",
    n_states = 3,
    state_labels = c("fixation_like", "pursuit_like", "saccade_like")
  )

  expect_equal(nrow(out), nrow(dat))
  expect_true("hmm_event" %in% names(out))
  expect_true(any(!is.na(out$hmm_event)))
})

test_that("impute_gazepoint_pupil_gp imputes missing pupil values", {
  dat <- data.frame(
    subject = rep("S1", 20),
    trial = rep(1, 20),
    time = seq_len(20),
    pupil = sin(seq_len(20) / 5) + 3
  )
  dat$pupil[8:10] <- NA_real_

  out <- impute_gazepoint_pupil_gp(
    dat,
    pupil = "pupil",
    time = "time",
    subject = "subject",
    trial = "trial",
    max_train = 20
  )

  expect_true("pupil_gp_imputed" %in% names(out))
  expect_true(all(is.finite(out$pupil_gp_imputed[8:10])))
  expect_true(all(out$pupil_was_gp_imputed[8:10]))
})

test_that("filter_gazepoint_cnn_uncertainty creates weights and validity flags", {
  dat <- data.frame(
    frame = 1:4,
    x = c(100, 101, NA, 103),
    y = c(200, 201, 202, 203),
    uncertainty = c(.1, .2, .3, 10)
  )

  out <- filter_gazepoint_cnn_uncertainty(
    dat,
    x = "x",
    y = "y",
    uncertainty = "uncertainty",
    max_uncertainty = 1
  )

  expect_true("cnn_uncertainty_weight" %in% names(out))
  expect_true("cnn_valid_frame" %in% names(out))
  expect_false(out$cnn_valid_frame[3])
  expect_false(out$cnn_valid_frame[4])
})

test_that("fit_gazepoint_brms_model reports missing optional backend", {
  if (requireNamespace("brms", quietly = TRUE)) {
    skip("brms is installed; backend availability test not applicable.")
  }

  dat <- data.frame(y = rnorm(5), x = rnorm(5))

  expect_error(
    fit_gazepoint_brms_model(dat, y ~ x),
    "Package 'brms' is required"
  )
})
