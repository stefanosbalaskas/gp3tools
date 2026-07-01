
test_that("compute_gazepoint_aoi_sequence_metrics computes expected metrics", {
  dat <- data.frame(
    subject = "S01",
    trial = "T01",
    time = 1:6,
    AOI = c("A", "A", "B", "A", "C", "C")
  )

  out <- compute_gazepoint_aoi_sequence_metrics(
    dat,
    aoi_col = "AOI",
    group_cols = c("subject", "trial"),
    time_col = "time"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$sequence_length, 6)
  expect_equal(out$n_aoi_visits, 4)
  expect_equal(out$n_unique_aoi, 3)
  expect_equal(out$transition_count, 3)
  expect_equal(out$revisit_count, 1)
  expect_equal(out$first_aoi, "A")
  expect_equal(out$last_aoi, "C")
  expect_equal(out$max_run_length, 2)
  expect_equal(out$sequence_status, "ok")
})

test_that("compute_gazepoint_aoi_sequence_metrics handles empty AOI sequences", {
  dat <- data.frame(
    trial = "T01",
    AOI = c(NA, "")
  )

  out <- compute_gazepoint_aoi_sequence_metrics(
    dat,
    aoi_col = "AOI",
    group_cols = "trial"
  )

  expect_equal(out$sequence_status, "no_valid_aoi")
  expect_equal(out$sequence_length, 0)
})
