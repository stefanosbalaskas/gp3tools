.make_scanpath_stability_distance <- function() {
  latent_cluster <- rep(1:3, each = 2L)

  distance_matrix <- outer(
    latent_cluster,
    latent_cluster,
    FUN = function(x, y) {
      ifelse(x == y, 0.05, 1)
    }
  )

  diag(distance_matrix) <- 0

  dimnames(distance_matrix) <- list(
    LETTERS[1:6],
    LETTERS[1:6]
  )

  distance_matrix
}

test_that("bootstrap stability returns expected structures", {
  result <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 20,
    sample_fraction = 0.8,
    linkages = c("average", "complete"),
    seed = 42
  )

  expect_s3_class(
    result,
    "gp3_scanpath_cluster_bootstrap"
  )

  expect_equal(
    result$specifications$specification,
    c(
      "hierarchical_average",
      "hierarchical_complete"
    )
  )

  expect_equal(
    nrow(result$iteration_summary),
    40L
  )

  expect_equal(
    dim(result$co_clustering$hierarchical_average),
    c(6L, 6L)
  )

  expect_equal(
    unname(diag(result$co_clustering$hierarchical_average)),
    rep(1, 6L)
  )

  expect_equal(
    nrow(result$representative_stability),
    12L
  )

  expect_equal(result$bootstrap_status, "ok")
})

test_that("bootstrap stability is deterministic with a seed", {
  first <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 15,
    seed = 123
  )

  second <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 15,
    seed = 123
  )

  expect_equal(
    first$iteration_summary,
    second$iteration_summary
  )

  expect_equal(
    first$co_clustering,
    second$co_clustering
  )

  expect_equal(
    first$representative_stability,
    second$representative_stability
  )
})

test_that("clear clusters have strong co-clustering separation", {
  result <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 30,
    seed = 9
  )

  summary_result <-
    summarise_gazepoint_scanpath_cluster_stability(
      result,
      min_pair_coverage = 0.25,
      stable_threshold = 0.75
    )

  expect_s3_class(
    summary_result,
    "gp3_scanpath_cluster_stability_summary"
  )

  expect_equal(nrow(summary_result$overview), 1L)
  expect_equal(
    nrow(summary_result$sequence_summary),
    6L
  )

  expect_true(
    summary_result$overview$
      mean_within_cluster_coclustering >
      summary_result$overview$
        mean_between_cluster_coclustering
  )

  expect_true(all(
    summary_result$sequence_summary$
      within_cluster_stability >= 0.75
  ))

  expect_true(all(
    c(
      "representative_rate_when_included",
      "reference_cluster"
    ) %in%
      names(
        summary_result$representative_stability
      )
  ))
})

test_that("PAM stability is available when cluster is installed", {
  skip_if_not_installed("cluster")

  result <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 10,
    method = "pam",
    seed = 7
  )

  expect_equal(
    result$specifications$specification,
    "pam"
  )

  expect_s3_class(
    result$reference_fits$pam$model,
    "pam"
  )
})

test_that("stability plots return plot data", {
  result <- bootstrap_gazepoint_scanpath_clusters(
    .make_scanpath_stability_distance(),
    k = 3,
    n_boot = 15,
    seed = 11
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit(
    if (grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    },
    add = TRUE
  )

  co_result <-
    plot_gazepoint_scanpath_cluster_stability(
      result,
      plot = "coclustering"
    )

  ari_result <-
    plot_gazepoint_scanpath_cluster_stability(
      result,
      plot = "ari"
    )

  sequence_result <-
    plot_gazepoint_scanpath_cluster_stability(
      result,
      plot = "sequence",
      min_pair_coverage = 0.25
    )

  grDevices::dev.off()

  expect_equal(co_result$plot, "coclustering")
  expect_equal(dim(co_result$data), c(6L, 6L))
  expect_equal(ari_result$plot, "ari")
  expect_equal(nrow(ari_result$data), 15L)
  expect_equal(sequence_result$plot, "sequence")
  expect_equal(nrow(sequence_result$data), 6L)
  expect_true(file.exists(plot_file))
})

test_that("invalid stability settings are rejected", {
  distance_matrix <-
    .make_scanpath_stability_distance()

  expect_error(
    bootstrap_gazepoint_scanpath_clusters(
      distance_matrix,
      k = 6
    ),
    "smaller than the number"
  )

  expect_error(
    bootstrap_gazepoint_scanpath_clusters(
      distance_matrix,
      sample_fraction = 0
    ),
    "greater than 0"
  )

  expect_error(
    bootstrap_gazepoint_scanpath_clusters(
      distance_matrix,
      linkages = "unsupported"
    ),
    "Unsupported linkage"
  )

  expect_error(
    summarise_gazepoint_scanpath_cluster_stability(
      list()
    ),
    "bootstrap_gazepoint_scanpath_clusters"
  )
})
