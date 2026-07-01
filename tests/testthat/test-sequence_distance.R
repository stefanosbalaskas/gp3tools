
test_that("compute_gazepoint_sequence_distance returns zero for identical sequences", {
  out <- compute_gazepoint_sequence_distance(
    c("A", "B", "C"),
    c("A", "B", "C")
  )

  expect_equal(out$edit_distance, 0)
  expect_equal(out$normalized_distance, 0)
  expect_equal(out$sequence_a_length, 3)
  expect_equal(out$sequence_b_length, 3)
})

test_that("compute_gazepoint_sequence_distance detects deletion distance", {
  out <- compute_gazepoint_sequence_distance(
    c("A", "B", "C"),
    c("A", "C")
  )

  expect_equal(out$edit_distance, 1)
  expect_equal(out$normalized_distance, 1 / 3)
})

test_that("compute_gazepoint_sequence_distance can collapse repeats", {
  out <- compute_gazepoint_sequence_distance(
    c("A", "A", "B"),
    c("A", "B"),
    collapse_repeats = TRUE
  )

  expect_equal(out$edit_distance, 0)
})
