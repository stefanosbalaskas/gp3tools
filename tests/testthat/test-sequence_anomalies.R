test_that("flag_gazepoint_sequence_anomalies flags short and missing sequences", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    aoi = c("A", "B", "C", NA, NA, "A")
  )
  out <- flag_gazepoint_sequence_anomalies(
    d, aoi_col = "aoi", group_cols = "id", time_col = "time",
    min_length = 2, max_missing_prop = 0.5
  )
  expect_equal(nrow(out), 2)
  expect_false(out$anomaly_flag[out$id == 1])
  expect_true(out$anomaly_flag[out$id == 2])
})
