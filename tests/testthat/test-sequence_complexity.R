test_that("compute_gazepoint_sequence_complexity works for a vector", {
  out <- compute_gazepoint_sequence_complexity(sequence = c("A", "A", "B", "C"))
  expect_equal(out$sequence_length, 4)
  expect_equal(out$n_unique_aoi, 3)
  expect_true(out$complexity_index > 0)
})

test_that("compute_gazepoint_sequence_complexity works by group", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    aoi = c("A", "A", "A", "A", "B", "C")
  )
  out <- compute_gazepoint_sequence_complexity(d, aoi_col = "aoi", group_cols = "id", time_col = "time")
  expect_equal(nrow(out), 2)
  expect_true(out$complexity_index[out$id == 2] > out$complexity_index[out$id == 1])
})
