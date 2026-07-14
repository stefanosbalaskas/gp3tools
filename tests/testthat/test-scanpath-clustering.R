test_that("hierarchical scanpath clustering separates clear groups", {
  distance_matrix <- matrix(
    c(
      0, 1, 5, 6,
      1, 0, 6, 5,
      5, 6, 0, 1,
      6, 5, 1, 0
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      c("A", "B", "C", "D"),
      c("A", "B", "C", "D")
    )
  )

  result <- cluster_gazepoint_scanpaths(
    distance_matrix,
    k = 2,
    method = "hierarchical"
  )

  expect_s3_class(result, "gp3_scanpath_clusters")
  expect_s3_class(result$model, "hclust")
  expect_equal(nrow(result$assignments), 4L)
  expect_equal(result$assignments$cluster[[1L]],
               result$assignments$cluster[[2L]])
  expect_equal(result$assignments$cluster[[3L]],
               result$assignments$cluster[[4L]])
  expect_false(
    result$assignments$cluster[[1L]] ==
      result$assignments$cluster[[3L]]
  )
  expect_equal(result$distance_source, "distance_matrix")
  expect_equal(result$clustering_status, "ok")
})

test_that("long AOI data can be clustered directly", {
  dat <- data.frame(
    subject = rep(c("S1", "S2", "S3", "S4"), each = 3L),
    time = rep(1:3, times = 4L),
    AOI = c(
      "A", "B", "C",
      "A", "B", "C",
      "X", "Y", "Z",
      "X", "Y", "Z"
    ),
    stringsAsFactors = FALSE
  )

  result <- cluster_gazepoint_scanpaths(
    dat,
    aoi_col = "AOI",
    group_cols = "subject",
    time_col = "time",
    k = 2,
    method = "hierarchical"
  )

  expect_s3_class(result, "gp3_scanpath_clusters")
  expect_equal(nrow(result$assignments), 4L)
  expect_equal(length(unique(result$assignments$cluster)), 2L)
  expect_equal(result$distance_source, "long_aoi_data")
  expect_true(is.data.frame(result$pairwise_distances))
})

test_that("pairwise distance tables are supported", {
  pairwise <- data.frame(
    sequence_a = c("A", "B", "C", "D", "A", "A", "A", "B", "B", "C"),
    sequence_b = c("A", "B", "C", "D", "B", "C", "D", "C", "D", "D"),
    normalized_distance = c(0, 0, 0, 0, 0.1, 0.9, 0.9, 0.9, 0.9, 0.1),
    stringsAsFactors = FALSE
  )

  result <- cluster_gazepoint_scanpaths(
    pairwise,
    k = 2,
    method = "hierarchical"
  )

  expect_s3_class(result, "gp3_scanpath_clusters")
  expect_equal(result$distance_source, "pairwise_distance_table")
  expect_equal(nrow(result$assignments), 4L)
})

test_that("incomplete pairwise distances are rejected", {
  incomplete <- data.frame(
    sequence_a = c("A", "A", "B", "C"),
    sequence_b = c("A", "B", "B", "C"),
    normalized_distance = c(0, 0.5, 0, 0),
    stringsAsFactors = FALSE
  )

  expect_error(
    cluster_gazepoint_scanpaths(
      incomplete,
      k = 2,
      method = "hierarchical"
    ),
    "incomplete"
  )
})

test_that("invalid cluster counts are rejected", {
  distance_matrix <- matrix(
    c(
      0, 1, 2,
      1, 0, 1,
      2, 1, 0
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_error(
    cluster_gazepoint_scanpaths(
      distance_matrix,
      k = 3,
      method = "hierarchical"
    ),
    "smaller than the number"
  )
})

test_that("PAM clustering works when cluster is installed", {
  skip_if_not_installed("cluster")

  distance_matrix <- matrix(
    c(
      0, 1, 5, 6,
      1, 0, 6, 5,
      5, 6, 0, 1,
      6, 5, 1, 0
    ),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(
      c("A", "B", "C", "D"),
      c("A", "B", "C", "D")
    )
  )

  result <- cluster_gazepoint_scanpaths(
    distance_matrix,
    k = 2,
    method = "pam"
  )

  expect_s3_class(result, "gp3_scanpath_clusters")
  expect_s3_class(result$model, "pam")
  expect_length(result$medoids, 2L)
  expect_equal(nrow(result$assignments), 4L)
})

