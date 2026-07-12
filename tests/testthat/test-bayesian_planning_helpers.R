test_that("recommend_gazepoint_model_family returns known metrics", {
  rec <- recommend_gazepoint_model_family("fixation_duration")

  expect_s3_class(rec, "data.frame")
  expect_equal(rec$metric, "fixation_duration")
  expect_true(grepl("lognormal", rec$recommended_family))
})

test_that("check_gazepoint_bayesian_readiness flags basic readiness", {
  dat <- data.frame(
    subject = rep(c("S1", "S2"), each = 12),
    trial = rep(1:6, times = 4),
    time = rep(seq(-200, 900, by = 100), length.out = 24),
    condition = rep(c("A", "B"), each = 12),
    pupil_bc = rnorm(24)
  )

  chk <- check_gazepoint_bayesian_readiness(
    data = dat,
    outcome = "pupil_bc",
    subject = "subject",
    trial = "trial",
    time = "time",
    condition = "condition",
    metric_type = "pupil_timecourse",
    baseline_window = c(-200, 0)
  )

  expect_s3_class(chk, "data.frame")
  expect_true("required_columns" %in% chk$check)
  expect_true(any(chk$status == "pass"))
})

test_that("create_gazepoint_bayesian_sap returns checklist", {
  sap <- create_gazepoint_bayesian_sap(
    outcome = "pupil",
    design = "within_subject",
    primary_model = "bayesian_gamm",
    baseline_window = c(-200, 0),
    analysis_window = c(0, 2000)
  )

  expect_s3_class(sap, "data.frame")
  expect_true("MCMC diagnostics" %in% sap$section)
})

test_that("prepare_gazepoint_hddm_export creates HDDM-style columns", {
  dat <- data.frame(
    subject = rep(c("S1", "S2"), each = 4),
    rt = c(.7, .8, .9, 1.0, .6, .7, .8, .9),
    response = c(1, 0, 1, 0, 1, 1, 0, 0),
    dwell = c(200, 250, 230, 210, 190, 220, 240, 260)
  )

  out <- prepare_gazepoint_hddm_export(
    data = dat,
    subject = "subject",
    rt = "rt",
    response = "response",
    predictors = "dwell",
    zscore_within_subject = TRUE
  )

  expect_true(all(c("subj_idx", "rt", "response", "dwell_z") %in% names(out)))
  expect_equal(nrow(out), nrow(dat))
})

test_that("create_gazepoint_brms_template returns pupil formula", {
  tpl <- create_gazepoint_brms_template(
    metric_type = "pupil_timecourse",
    outcome = "pupil_bc",
    time = "time",
    condition = "condition",
    subject = "subject"
  )

  expect_true(grepl("s\\(time", tpl$formula))
  expect_equal(tpl$family, "gaussian()")
})

test_that("summarize_gazepoint_pupil_response_features extracts trial features", {
  dat <- data.frame(
    subject = rep("S1", 8),
    trial = rep(1, 8),
    time = c(-200, -100, 0, 100, 200, 300, 400, 500),
    pupil = c(3, 3.1, 3.0, 3.2, 3.4, 3.3, 3.1, 3.0),
    condition = "A",
    interpolated = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )

  out <- summarize_gazepoint_pupil_response_features(
    data = dat,
    pupil = "pupil",
    time = "time",
    subject = "subject",
    trial = "trial",
    baseline_window = c(-200, 0),
    response_window = c(100, 500),
    condition = "condition",
    interpolated = "interpolated"
  )

  expect_equal(nrow(out), 1)
  expect_true("peak_dilation" %in% names(out))
  expect_true(out$peak_dilation > 0)
})

test_that("compute_gazepoint_scanpath_geometry computes geometry features", {
  dat <- data.frame(
    subject = rep("S1", 4),
    trial = rep(1, 4),
    time = 1:4,
    x = c(0, 1, 1, 0),
    y = c(0, 0, 1, 1),
    condition = "A"
  )

  out <- compute_gazepoint_scanpath_geometry(
    data = dat,
    x = "x",
    y = "y",
    subject = "subject",
    trial = "trial",
    time = "time",
    condition = "condition"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$n_points, 4)
  expect_true(out$scanpath_length > 0)
  expect_true(out$convex_hull_area > 0)
})
