.make_scanpath_workflow_distance <- function() {
  latent_cluster <- rep(1:3, each = 2L)

  distance_matrix <- outer(
    latent_cluster,
    latent_cluster,
    FUN = function(x, y) {
      ifelse(x == y, 0.1, 1)
    }
  )

  diag(distance_matrix) <- 0

  dimnames(distance_matrix) <- list(
    LETTERS[1:6],
    LETTERS[1:6]
  )

  distance_matrix
}

test_that("cluster-number selection identifies clear groups", {
  skip_if_not_installed("cluster")

  selection <- select_gazepoint_scanpath_clusters(
    .make_scanpath_workflow_distance(),
    k_values = 2:4,
    method = "hierarchical"
  )

  expect_s3_class(
    selection,
    "gp3_scanpath_cluster_selection"
  )

  expect_equal(selection$recommended_k, 3L)
  expect_equal(selection$diagnostics$k, 2:4)

  expect_s3_class(
    selection$recommended_fit,
    "gp3_scanpath_clusters"
  )

  selected_width <- selection$diagnostics$mean_silhouette_width[
    selection$diagnostics$k == selection$recommended_k
  ]

  expect_equal(
    selected_width,
    max(selection$diagnostics$mean_silhouette_width)
  )
})

test_that("PAM cluster-number selection is supported", {
  skip_if_not_installed("cluster")

  selection <- select_gazepoint_scanpath_clusters(
    .make_scanpath_workflow_distance(),
    k_values = 2:4,
    method = "pam"
  )

  expect_equal(selection$recommended_k, 3L)
  expect_equal(selection$method, "pam")
  expect_s3_class(selection$recommended_fit$model, "pam")
})

test_that("invalid cluster candidates are rejected", {
  skip_if_not_installed("cluster")

  expect_error(
    select_gazepoint_scanpath_clusters(
      .make_scanpath_workflow_distance(),
      k_values = c(1, 2, 6)
    ),
    "Invalid value"
  )
})

test_that("representatives minimize within-cluster distance", {
  fit <- cluster_gazepoint_scanpaths(
    .make_scanpath_workflow_distance(),
    k = 3,
    method = "hierarchical"
  )

  representatives <-
    extract_gazepoint_representative_scanpaths(fit)

  expect_s3_class(
    representatives,
    "gp3_scanpath_representatives"
  )

  expect_equal(nrow(representatives), 3L)

  expect_setequal(
    representatives$sequence_id,
    c("A", "C", "E")
  )

  expect_true(
    all(representatives$cluster_size == 2L)
  )

  expect_equal(
    representatives$mean_within_cluster_distance,
    rep(0.1, 3L),
    tolerance = 1e-12
  )
})

test_that("multiple representatives can be returned", {
  fit <- cluster_gazepoint_scanpaths(
    .make_scanpath_workflow_distance(),
    k = 3
  )

  representatives <-
    extract_gazepoint_representative_scanpaths(
      fit,
      n_per_cluster = 2
    )

  expect_equal(nrow(representatives), 6L)

  expect_equal(
    as.integer(table(representatives$cluster)),
    rep(2L, 3L)
  )
})

test_that("MDS and dendrogram plot data are returned", {
  fit <- cluster_gazepoint_scanpaths(
    .make_scanpath_workflow_distance(),
    k = 3,
    method = "hierarchical"
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit(
    if (grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    },
    add = TRUE
  )

  mds_result <- plot_gazepoint_scanpath_clusters(
    fit,
    plot = "mds"
  )

  dendrogram_result <- plot_gazepoint_scanpath_clusters(
    fit,
    plot = "dendrogram"
  )

  grDevices::dev.off()

  expect_equal(mds_result$plot, "mds")
  expect_equal(nrow(mds_result$data), 6L)

  expect_true(
    all(
      c(
        "mds_dimension_1",
        "mds_dimension_2",
        "cluster"
      ) %in% names(mds_result$data)
    )
  )

  expect_equal(
    dendrogram_result$plot,
    "dendrogram"
  )

  expect_true(file.exists(plot_file))
})

test_that("silhouette plot data are returned", {
  skip_if_not_installed("cluster")

  fit <- cluster_gazepoint_scanpaths(
    .make_scanpath_workflow_distance(),
    k = 3
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit(
    if (grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    },
    add = TRUE
  )

  result <- plot_gazepoint_scanpath_clusters(
    fit,
    plot = "silhouette"
  )

  grDevices::dev.off()

  expect_equal(result$plot, "silhouette")
  expect_equal(nrow(result$data), 6L)

  expect_true(
    all(is.finite(result$data$silhouette_width))
  )
})

test_that("dendrogram rejects PAM results", {
  skip_if_not_installed("cluster")

  fit <- cluster_gazepoint_scanpaths(
    .make_scanpath_workflow_distance(),
    k = 3,
    method = "pam"
  )

  expect_error(
    plot_gazepoint_scanpath_clusters(
      fit,
      plot = "dendrogram"
    ),
    "requires a hierarchical"
  )
})

test_that("workflow helpers reject unrelated objects", {
  expect_error(
    extract_gazepoint_representative_scanpaths(list()),
    "cluster_gazepoint_scanpaths"
  )

  expect_error(
    plot_gazepoint_scanpath_clusters(list()),
    "cluster_gazepoint_scanpaths"
  )
})
