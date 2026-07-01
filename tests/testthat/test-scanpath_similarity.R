test_that("compute_gazepoint_scanpath_similarity returns self similarity of one", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    aoi = c("A", "B", "C", "A", "B", "C")
  )
  out <- compute_gazepoint_scanpath_similarity(d, aoi_col = "aoi", group_cols = "id", time_col = "time")
  self <- out[out$sequence_a == out$sequence_b, ]
  expect_true(all(self$similarity == 1))
})

test_that("compute_gazepoint_scanpath_similarity distinguishes different sequences", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    aoi = c("A", "B", "C", "A", "C", "B")
  )
  out <- compute_gazepoint_scanpath_similarity(d, aoi_col = "aoi", group_cols = "id", time_col = "time")
  between <- out[out$sequence_a != out$sequence_b, ]
  expect_true(all(between$similarity < 1))
})
