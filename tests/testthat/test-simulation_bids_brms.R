test_that("simulate_gazepoint_data returns expected tables", {
  sim <- simulate_gazepoint_data(
    n_subjects = 3, n_trials = 2, trial_duration_ms = 100,
    sampling_rate_hz = 10, seed = 1
  )
  expect_s3_class(sim, "gp3_simulated_data")
  expect_true(all(c("all_gaze", "aoi_windows", "metadata") %in% names(sim)))
  expect_equal(length(unique(sim$all_gaze$subject_id)), 3)
  expect_equal(nrow(sim$aoi_windows), 6)
  expect_true(is.data.frame(sim$fixations))
})

test_that("simulate_gazepoint_data is reproducible with seed", {
  a <- simulate_gazepoint_data(n_subjects = 2, n_trials = 2, seed = 123)
  b <- simulate_gazepoint_data(n_subjects = 2, n_trials = 2, seed = 123)
  expect_equal(a$all_gaze$aoi, b$all_gaze$aoi)
})

test_that("export_gazepoint_to_bids writes expected files", {
  sim <- simulate_gazepoint_data(n_subjects = 2, n_trials = 1, trial_duration_ms = 100, sampling_rate_hz = 10, seed = 1)
  outdir <- tempfile("gp3_bids_")
  written <- export_gazepoint_to_bids(
    sim$all_gaze, outdir = outdir, subject_col = "subject_id",
    task = "demo", time_col = "time_ms", x_col = "x", y_col = "y",
    pupil_col = "pupil", trial_col = "trial_id", aoi_col = "aoi"
  )
  expect_true(file.exists(file.path(outdir, "dataset_description.json")))
  expect_true(file.exists(file.path(outdir, "participants.tsv")))
  expect_true(any(written$file_type == "eyetrack_tsv"))
  expect_true(all(file.exists(written$file)))
})

test_that("export_gazepoint_to_bids protects existing directories", {
  sim <- simulate_gazepoint_data(n_subjects = 1, n_trials = 1, trial_duration_ms = 100, sampling_rate_hz = 10, seed = 1)
  outdir <- tempfile("gp3_bids_existing_")
  dir.create(outdir)
  expect_error(
    export_gazepoint_to_bids(sim$all_gaze, outdir, "subject_id", time_col = "time_ms", x_col = "x", y_col = "y"),
    "outdir already exists"
  )
})

test_that("fit_gazepoint_aoi_brms returns a dry-run specification", {
  d <- data.frame(
    y = c(0, 1, 0, 1),
    condition = c("a", "a", "b", "b"),
    subject = c("s1", "s2", "s1", "s2")
  )
  spec <- fit_gazepoint_aoi_brms(
    d, response = "y", predictors = "condition",
    subject_col = "subject", dry_run = TRUE
  )
  expect_s3_class(spec, "gp3_brms_spec")
  expect_true(spec$dry_run)
  expect_true(inherits(spec$formula, "formula"))
})
