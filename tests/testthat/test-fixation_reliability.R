
test_that("audit_gazepoint_fixation_reliability computes duration reliability", {
  dat <- expand.grid(
    subject = paste0("S", 1:8),
    trial = paste0("T", 1:4),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dat$duration <- rep(seq_len(8), each = 4) + rep(c(0, 0.1, 0, 0.1), 8)

  out <- audit_gazepoint_fixation_reliability(
    dat,
    subject_col = "subject",
    trial_col = "trial",
    metric = "total_fixation_duration",
    duration_col = "duration"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$reliability_status, "ok")
  expect_true(out$split_half_r > 0.99)
  expect_true(out$spearman_brown > 0.99)
})

test_that("audit_gazepoint_fixation_reliability handles AOI transition metric", {
  dat <- data.frame(
    subject = rep(paste0("S", 1:4), each = 8),
    trial = rep(rep(paste0("T", 1:4), each = 2), 4),
    time = rep(1:2, 16),
    AOI = rep(c("A", "B"), 16),
    stringsAsFactors = FALSE
  )

  out <- audit_gazepoint_fixation_reliability(
    dat,
    subject_col = "subject",
    trial_col = "trial",
    metric = "transition_count",
    aoi_col = "AOI",
    time_col = "time",
    min_trials = 4
  )

  expect_equal(out$reliability_status, "no_variance")
})
