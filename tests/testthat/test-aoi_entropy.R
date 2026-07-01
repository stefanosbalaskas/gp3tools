
test_that("compute_gazepoint_aoi_entropy computes spatial and transition entropy", {
  dat <- data.frame(
    subject = "S01",
    trial = "T01",
    time = 1:4,
    AOI = c("A", "A", "B", "B")
  )

  out <- compute_gazepoint_aoi_entropy(
    dat,
    aoi_col = "AOI",
    group_cols = c("subject", "trial"),
    time_col = "time"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$n_observations, 4)
  expect_equal(out$n_aoi, 2)
  expect_equal(out$spatial_entropy, 1)
  expect_equal(out$spatial_entropy_norm, 1)
  expect_equal(out$n_transitions, 3)
  expect_equal(out$n_transition_types, 3)
  expect_equal(out$entropy_status, "ok")
})

test_that("compute_gazepoint_aoi_entropy handles repeat collapse", {
  dat <- data.frame(
    trial = "T01",
    time = 1:4,
    AOI = c("A", "A", "B", "B")
  )

  out <- compute_gazepoint_aoi_entropy(
    dat,
    aoi_col = "AOI",
    group_cols = "trial",
    time_col = "time",
    collapse_repeats = TRUE
  )

  expect_equal(out$n_observations, 2)
  expect_equal(out$n_transitions, 1)
  expect_equal(out$n_transition_types, 1)
})
