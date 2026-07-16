#' Bootstrap scanpath-cluster stability
#'
#' Evaluate the stability of scanpath clustering by repeatedly subsampling
#' scanpaths, refitting the clustering solution, and recording co-clustering,
#' adjusted Rand agreement, and representative-scanpath selection.
#'
#' The routine reuses the distance formats accepted by
#' [cluster_gazepoint_scanpaths()]. Hierarchical solutions can be compared
#' across multiple linkage methods. PAM remains optional through \pkg{cluster}.
#'
#' @param x Long-format AOI data, pairwise distance data, a square numeric
#'   distance matrix, or a `"dist"` object.
#' @param k Number of clusters.
#' @param n_boot Number of subsampling iterations per specification.
#' @param sample_fraction Proportion of scanpaths retained in each iteration.
#'   The resolved sample size is always at least `k + 1`.
#' @param method Clustering method: `"hierarchical"` or `"pam"`.
#' @param linkages Hierarchical linkage methods to compare. Ignored for PAM.
#' @param seed Optional integer seed. The caller's random-number state is
#'   restored on exit.
#' @param aoi_col AOI column when `x` is long-format AOI data.
#' @param group_cols Columns identifying independent scanpaths.
#' @param time_col Optional ordering column.
#' @param distance_col Distance column for pairwise-distance data.
#' @param include_missing Should missing AOI labels be retained?
#' @param missing_label Label used for retained missing AOIs.
#' @param collapse_repeats Should consecutive repeated AOIs be collapsed?
#' @param max_sequences Maximum number of scanpaths permitted when pairwise
#'   distances must be calculated.
#'
#' @return An object of class `"gp3_scanpath_cluster_bootstrap"` containing
#'   full-data reference fits, co-clustering and pair-coverage matrices,
#'   iteration-level adjusted Rand results, representative stability, the
#'   reusable distance object, and resolved settings.
#'
#' @export
#'
#' @examples
#' latent <- rep(1:3, each = 2)
#' d <- outer(
#'   latent,
#'   latent,
#'   FUN = function(x, y) ifelse(x == y, 0.1, 1)
#' )
#' diag(d) <- 0
#' dimnames(d) <- list(LETTERS[1:6], LETTERS[1:6])
#'
#' stability <- bootstrap_gazepoint_scanpath_clusters(
#'   d,
#'   k = 3,
#'   n_boot = 10,
#'   seed = 1
#' )
#'
#' stability$iteration_summary
bootstrap_gazepoint_scanpath_clusters <- function(
    x,
    k = 3L,
    n_boot = 200L,
    sample_fraction = 0.8,
    method = c("hierarchical", "pam"),
    linkages = "average",
    seed = NULL,
    aoi_col = NULL,
    group_cols = NULL,
    time_col = NULL,
    distance_col = "normalized_distance",
    include_missing = FALSE,
    missing_label = "missing",
    collapse_repeats = FALSE,
    max_sequences = 200) {

  method <- match.arg(method)

  .gp3_stability_check_positive_integer(
    k,
    "k"
  )

  .gp3_stability_check_positive_integer(
    n_boot,
    "n_boot"
  )

  k <- as.integer(k)
  n_boot <- as.integer(n_boot)

  if (!is.numeric(sample_fraction) ||
      length(sample_fraction) != 1L ||
      is.na(sample_fraction) ||
      !is.finite(sample_fraction) ||
      sample_fraction <= 0 ||
      sample_fraction > 1) {
    stop(
      "`sample_fraction` must be greater than 0 and at most 1.",
      call. = FALSE
    )
  }

  specifications <- .gp3_stability_specifications(
    method = method,
    linkages = linkages
  )

  preparation <- cluster_gazepoint_scanpaths(
    x = x,
    k = 2L,
    method = "hierarchical",
    linkage = "average",
    aoi_col = aoi_col,
    group_cols = group_cols,
    time_col = time_col,
    distance_col = distance_col,
    include_missing = include_missing,
    missing_label = missing_label,
    collapse_repeats = collapse_repeats,
    max_sequences = max_sequences
  )

  distance <- preparation$distance
  distance_matrix <- as.matrix(distance)
  sequence_ids <- attr(distance, "Labels")

  if (is.null(sequence_ids)) {
    sequence_ids <- rownames(distance_matrix)
  }

  n_sequences <- length(sequence_ids)

  if (k < 2L || k >= n_sequences) {
    stop(
      "`k` must be at least 2 and smaller than the number of scanpaths.",
      call. = FALSE
    )
  }

  sample_size <- max(
    k + 1L,
    as.integer(ceiling(
      n_sequences * sample_fraction
    ))
  )

  sample_size <- min(
    sample_size,
    n_sequences
  )

  .gp3_stability_with_seed(
    seed,
    {
      reference_fits <- vector(
        "list",
        nrow(specifications)
      )

      names(reference_fits) <-
        specifications$specification

      co_clustering <- vector(
        "list",
        nrow(specifications)
      )

      pair_coverage <- vector(
        "list",
        nrow(specifications)
      )

      same_counts <- vector(
        "list",
        nrow(specifications)
      )

      seen_counts <- vector(
        "list",
        nrow(specifications)
      )

      inclusion_counts <- vector(
        "list",
        nrow(specifications)
      )

      names(co_clustering) <-
        specifications$specification

      names(pair_coverage) <-
        specifications$specification

      names(same_counts) <-
        specifications$specification

      names(seen_counts) <-
        specifications$specification

      names(inclusion_counts) <-
        specifications$specification

      iteration_rows <- vector(
        "list",
        nrow(specifications) * n_boot
      )

      representative_rows <- list()
      iteration_row_index <- 0L
      representative_row_index <- 0L

      for (spec_index in seq_len(
        nrow(specifications)
      )) {
        specification <-
          specifications$specification[[spec_index]]

        specification_method <-
          specifications$method[[spec_index]]

        specification_linkage <-
          specifications$linkage[[spec_index]]

        reference_fit <- cluster_gazepoint_scanpaths(
          x = distance,
          k = k,
          method = specification_method,
          linkage = if (
            identical(
              specification_method,
              "hierarchical"
            )
          ) {
            specification_linkage
          } else {
            "average"
          }
        )

        reference_fits[[specification]] <-
          reference_fit

        reference_cluster <- reference_fit$assignments$cluster
        names(reference_cluster) <-
          reference_fit$assignments$sequence_id

        same_matrix <- matrix(
          0L,
          nrow = n_sequences,
          ncol = n_sequences,
          dimnames = list(
            sequence_ids,
            sequence_ids
          )
        )

        seen_matrix <- same_matrix

        included <- stats::setNames(
          integer(n_sequences),
          sequence_ids
        )

        for (iteration in seq_len(n_boot)) {
          sampled_ids <- sample(
            sequence_ids,
            size = sample_size,
            replace = FALSE
          )

          included[sampled_ids] <-
            included[sampled_ids] + 1L

          sampled_distance <- stats::as.dist(
            distance_matrix[
              sampled_ids,
              sampled_ids,
              drop = FALSE
            ]
          )

          fit <- cluster_gazepoint_scanpaths(
            x = sampled_distance,
            k = k,
            method = specification_method,
            linkage = if (
              identical(
                specification_method,
                "hierarchical"
              )
            ) {
              specification_linkage
            } else {
              "average"
            }
          )

          sampled_cluster <-
            fit$assignments$cluster

          names(sampled_cluster) <-
            fit$assignments$sequence_id

          sampled_cluster <- sampled_cluster[
            sampled_ids
          ]

          sampled_reference <-
            reference_cluster[sampled_ids]

          seen_matrix[
            sampled_ids,
            sampled_ids
          ] <- seen_matrix[
            sampled_ids,
            sampled_ids
          ] + 1L

          same_iteration <- outer(
            sampled_cluster,
            sampled_cluster,
            FUN = "=="
          )

          same_matrix[
            sampled_ids,
            sampled_ids
          ] <- same_matrix[
            sampled_ids,
            sampled_ids
          ] + same_iteration

          ari <- .gp3_adjusted_rand_index(
            sampled_reference,
            sampled_cluster
          )

          iteration_row_index <-
            iteration_row_index + 1L

          iteration_rows[[iteration_row_index]] <-
            data.frame(
              specification = specification,
              method = specification_method,
              linkage = specification_linkage,
              iteration = iteration,
              n_sampled = sample_size,
              adjusted_rand_index = ari,
              mean_silhouette_width =
                fit$mean_silhouette_width,
              stringsAsFactors = FALSE
            )

          cluster_map <-
            .gp3_map_resampled_clusters(
              reference_cluster =
                sampled_reference,
              resampled_cluster =
                sampled_cluster
            )

          representatives <-
            extract_gazepoint_representative_scanpaths(
              fit,
              n_per_cluster = 1L
            )

          if (nrow(representatives)) {
            for (representative_index in seq_len(
              nrow(representatives)
            )) {
              resampled_cluster_id <-
                as.character(
                  representatives$cluster[[representative_index]]
                )

              mapped_reference_cluster <-
                cluster_map[[resampled_cluster_id]]

              representative_row_index <-
                representative_row_index + 1L

              representative_rows[[representative_row_index]] <-
                data.frame(
                specification = specification,
                iteration = iteration,
                sequence_id =
                  representatives$sequence_id[[representative_index]],
                resampled_cluster =
                  representatives$cluster[[representative_index]],
                reference_cluster =
                  as.integer(
                    mapped_reference_cluster
                  ),
                stringsAsFactors = FALSE
              )
            }
          }
        }

        co_matrix <- same_matrix / seen_matrix
        co_matrix[seen_matrix == 0L] <- NA_real_
        diag(co_matrix) <- 1

        coverage_matrix <- seen_matrix / n_boot

        co_clustering[[specification]] <-
          co_matrix

        pair_coverage[[specification]] <-
          coverage_matrix

        same_counts[[specification]] <-
          same_matrix

        seen_counts[[specification]] <-
          seen_matrix

        inclusion_counts[[specification]] <-
          included
      }

      iteration_summary <- do.call(
        rbind,
        iteration_rows[
          seq_len(iteration_row_index)
        ]
      )

      rownames(iteration_summary) <- NULL

      representative_events <- if (
        representative_row_index > 0L
      ) {
        do.call(
          rbind,
          representative_rows[
            seq_len(representative_row_index)
          ]
        )
      } else {
        data.frame(
          specification = character(),
          iteration = integer(),
          sequence_id = character(),
          resampled_cluster = integer(),
          reference_cluster = integer(),
          stringsAsFactors = FALSE
        )
      }

      representative_stability <-
        .gp3_summarise_representative_events(
          representative_events =
            representative_events,
          inclusion_counts =
            inclusion_counts,
          reference_fits =
            reference_fits,
          specifications =
            specifications
        )

      out <- list(
        reference_fits = reference_fits,
        co_clustering = co_clustering,
        pair_coverage = pair_coverage,
        same_counts = same_counts,
        seen_counts = seen_counts,
        inclusion_counts = inclusion_counts,
        iteration_summary = iteration_summary,
        representative_events =
          representative_events,
        representative_stability =
          representative_stability,
        distance = distance,
        specifications = specifications,
        settings = list(
          k = k,
          n_boot = n_boot,
          sample_fraction = sample_fraction,
          sample_size = sample_size,
          method = method,
          linkages = specifications$linkage,
          seed = seed,
          distance_source =
            preparation$distance_source
        ),
        bootstrap_status = "ok",
        call = match.call()
      )

      class(out) <- c(
        "gp3_scanpath_cluster_bootstrap",
        "list"
      )

      out
    }
  )
}

#' Summarise scanpath-cluster stability
#'
#' Convert bootstrap scanpath-clustering output into overview, sequence-level,
#' pairwise, and representative-stability tables.
#'
#' @param x An object returned by
#'   [bootstrap_gazepoint_scanpath_clusters()].
#' @param min_pair_coverage Minimum proportion of iterations in which a pair
#'   must co-occur before it contributes to summaries.
#' @param stable_threshold Within-reference-cluster co-clustering threshold
#'   used to count stable scanpaths.
#'
#' @return An object of class `"gp3_scanpath_cluster_stability_summary"`
#'   containing overview, sequence, pairwise, and representative tables.
#'
#' @export
#'
#' @examples
#' latent <- rep(1:3, each = 2)
#' d <- outer(
#'   latent,
#'   latent,
#'   FUN = function(x, y) ifelse(x == y, 0.1, 1)
#' )
#' diag(d) <- 0
#' dimnames(d) <- list(LETTERS[1:6], LETTERS[1:6])
#'
#' stability <- bootstrap_gazepoint_scanpath_clusters(
#'   d,
#'   k = 3,
#'   n_boot = 10,
#'   seed = 1
#' )
#'
#' summarise_gazepoint_scanpath_cluster_stability(
#'   stability
#' )
summarise_gazepoint_scanpath_cluster_stability <- function(
    x,
    min_pair_coverage = 0.5,
    stable_threshold = 0.75) {

  .gp3_check_scanpath_bootstrap(x)

  for (argument in c(
    "min_pair_coverage",
    "stable_threshold"
  )) {
    value <- get(argument)

    if (!is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0 ||
        value > 1) {
      stop(
        paste0(
          "`",
          argument,
          "` must be between 0 and 1."
        ),
        call. = FALSE
      )
    }
  }

  overview_rows <- vector(
    "list",
    nrow(x$specifications)
  )

  sequence_rows <- vector(
    "list",
    nrow(x$specifications)
  )

  pair_rows <- vector(
    "list",
    nrow(x$specifications)
  )

  for (spec_index in seq_len(
    nrow(x$specifications)
  )) {
    specification <-
      x$specifications$specification[[spec_index]]

    fit <- x$reference_fits[[specification]]
    co_matrix <- x$co_clustering[[specification]]
    coverage_matrix <-
      x$pair_coverage[[specification]]

    reference_cluster <- fit$assignments$cluster
    names(reference_cluster) <-
      fit$assignments$sequence_id

    sequence_ids <- names(reference_cluster)

    upper_index <- which(
      upper.tri(co_matrix),
      arr.ind = TRUE
    )

    pair_table <- data.frame(
      specification = specification,
      sequence_a =
        rownames(co_matrix)[upper_index[, 1L]],
      sequence_b =
        colnames(co_matrix)[upper_index[, 2L]],
      co_clustering_probability =
        co_matrix[upper_index],
      pair_coverage =
        coverage_matrix[upper_index],
      same_reference_cluster =
        reference_cluster[
          rownames(co_matrix)[upper_index[, 1L]]
        ] ==
        reference_cluster[
          colnames(co_matrix)[upper_index[, 2L]]
        ],
      stringsAsFactors = FALSE
    )

    pair_table$included_in_summary <-
      is.finite(
        pair_table$co_clustering_probability
      ) &
      pair_table$pair_coverage >=
        min_pair_coverage

    pair_rows[[spec_index]] <- pair_table

    sequence_table <- do.call(
      rbind,
      lapply(
        sequence_ids,
        function(sequence_id) {
          other_ids <- setdiff(
            sequence_ids,
            sequence_id
          )

          same_ids <- other_ids[
            reference_cluster[other_ids] ==
              reference_cluster[[sequence_id]]
          ]

          different_ids <- other_ids[
            reference_cluster[other_ids] !=
              reference_cluster[[sequence_id]]
          ]

          within_values <- co_matrix[
            sequence_id,
            same_ids,
            drop = TRUE
          ]

          within_coverage <- coverage_matrix[
            sequence_id,
            same_ids,
            drop = TRUE
          ]

          between_values <- co_matrix[
            sequence_id,
            different_ids,
            drop = TRUE
          ]

          between_coverage <- coverage_matrix[
            sequence_id,
            different_ids,
            drop = TRUE
          ]

          valid_within <- is.finite(
            within_values
          ) &
            within_coverage >=
              min_pair_coverage

          valid_between <- is.finite(
            between_values
          ) &
            between_coverage >=
              min_pair_coverage

          within_mean <- if (
            any(valid_within)
          ) {
            mean(within_values[valid_within])
          } else {
            NA_real_
          }

          between_mean <- if (
            any(valid_between)
          ) {
            mean(between_values[valid_between])
          } else {
            NA_real_
          }

          data.frame(
            specification = specification,
            sequence_id = sequence_id,
            reference_cluster =
              reference_cluster[[sequence_id]],
            within_cluster_stability =
              within_mean,
            between_cluster_coclustering =
              between_mean,
            stability_separation = if (
              is.finite(within_mean) &&
                is.finite(between_mean)
            ) {
              within_mean - between_mean
            } else {
              NA_real_
            },
            n_within_pairs =
              sum(valid_within),
            n_between_pairs =
              sum(valid_between),
            mean_pair_coverage = mean(
              coverage_matrix[
                sequence_id,
                other_ids,
                drop = TRUE
              ],
              na.rm = TRUE
            ),
            stable = is.finite(within_mean) &&
              within_mean >= stable_threshold,
            stringsAsFactors = FALSE
          )
        }
      )
    )

    rownames(sequence_table) <- NULL
    sequence_rows[[spec_index]] <-
      sequence_table

    iteration_table <- x$iteration_summary[
      x$iteration_summary$specification ==
        specification,
      ,
      drop = FALSE
    ]

    valid_pairs <- pair_table[
      pair_table$included_in_summary,
      ,
      drop = FALSE
    ]

    within_pair_values <-
      valid_pairs$co_clustering_probability[
        valid_pairs$same_reference_cluster
      ]

    between_pair_values <-
      valid_pairs$co_clustering_probability[
        !valid_pairs$same_reference_cluster
      ]

    overview_rows[[spec_index]] <- data.frame(
      specification = specification,
      method =
        x$specifications$method[[spec_index]],
      linkage =
        x$specifications$linkage[[spec_index]],
      k = x$settings$k,
      n_boot = x$settings$n_boot,
      sample_size = x$settings$sample_size,
      mean_adjusted_rand_index = mean(
        iteration_table$adjusted_rand_index,
        na.rm = TRUE
      ),
      sd_adjusted_rand_index = stats::sd(
        iteration_table$adjusted_rand_index,
        na.rm = TRUE
      ),
      min_adjusted_rand_index = min(
        iteration_table$adjusted_rand_index,
        na.rm = TRUE
      ),
      mean_within_cluster_coclustering =
        .gp3_mean_or_na(within_pair_values),
      mean_between_cluster_coclustering =
        .gp3_mean_or_na(between_pair_values),
      mean_sequence_stability =
        .gp3_mean_or_na(
          sequence_table$within_cluster_stability
        ),
      min_sequence_stability = if (
        any(is.finite(
          sequence_table$within_cluster_stability
        ))
      ) {
        min(
          sequence_table$within_cluster_stability,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      pct_sequences_stable = 100 *
        mean(sequence_table$stable),
      stability_status = if (
        all(
          sequence_table$stable |
            !is.finite(
              sequence_table$within_cluster_stability
            )
        )
      ) {
        "stable"
      } else {
        "review"
      },
      stringsAsFactors = FALSE
    )
  }

  overview <- do.call(
    rbind,
    overview_rows
  )

  sequence_summary <- do.call(
    rbind,
    sequence_rows
  )

  pairwise_summary <- do.call(
    rbind,
    pair_rows
  )

  rownames(overview) <- NULL
  rownames(sequence_summary) <- NULL
  rownames(pairwise_summary) <- NULL

  out <- list(
    overview = overview,
    sequence_summary = sequence_summary,
    pairwise_summary = pairwise_summary,
    representative_stability =
      x$representative_stability,
    settings = list(
      min_pair_coverage = min_pair_coverage,
      stable_threshold = stable_threshold
    )
  )

  class(out) <- c(
    "gp3_scanpath_cluster_stability_summary",
    "list"
  )

  out
}

#' Plot scanpath-cluster stability
#'
#' Create base-R diagnostics for co-clustering probabilities,
#' iteration-level adjusted Rand indices, or sequence-level stability.
#'
#' @param x An object returned by
#'   [bootstrap_gazepoint_scanpath_clusters()].
#' @param plot Plot type: `"coclustering"`, `"ari"`, or `"sequence"`.
#' @param specification Optional specification name. The first specification
#'   is used by default.
#' @param min_pair_coverage Pair-coverage threshold used in sequence summaries.
#' @param stable_threshold Stability threshold used in sequence summaries.
#' @param main Optional plot title.
#' @param label_cex Axis-label size multiplier.
#'
#' @return Invisibly returns a list containing the plot type,
#'   specification, and plotted data.
#'
#' @export
#'
#' @examples
#' latent <- rep(1:3, each = 2)
#' d <- outer(
#'   latent,
#'   latent,
#'   FUN = function(x, y) ifelse(x == y, 0.1, 1)
#' )
#' diag(d) <- 0
#' dimnames(d) <- list(LETTERS[1:6], LETTERS[1:6])
#'
#' stability <- bootstrap_gazepoint_scanpath_clusters(
#'   d,
#'   k = 3,
#'   n_boot = 10,
#'   seed = 1
#' )
#'
#' plot_gazepoint_scanpath_cluster_stability(
#'   stability,
#'   plot = "coclustering"
#' )
plot_gazepoint_scanpath_cluster_stability <- function(
    x,
    plot = c("coclustering", "ari", "sequence"),
    specification = NULL,
    min_pair_coverage = 0.5,
    stable_threshold = 0.75,
    main = NULL,
    label_cex = 0.8) {

  .gp3_check_scanpath_bootstrap(x)

  plot <- match.arg(plot)

  if (!is.numeric(label_cex) ||
      length(label_cex) != 1L ||
      is.na(label_cex) ||
      !is.finite(label_cex) ||
      label_cex <= 0) {
    stop(
      "`label_cex` must be one positive finite number.",
      call. = FALSE
    )
  }

  available_specifications <-
    x$specifications$specification

  if (is.null(specification)) {
    specification <-
      available_specifications[[1L]]
  }

  if (!is.character(specification) ||
      length(specification) != 1L ||
      is.na(specification) ||
      !specification %in%
        available_specifications) {
    stop(
      paste0(
        "`specification` must be one of: ",
        paste(
          available_specifications,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  fit <- x$reference_fits[[specification]]

  reference_cluster <- fit$assignments$cluster
  names(reference_cluster) <-
    fit$assignments$sequence_id

  if (identical(plot, "coclustering")) {
    co_matrix <- x$co_clustering[[specification]]

    order_index <- order(
      reference_cluster[
        rownames(co_matrix)
      ],
      rownames(co_matrix)
    )

    plot_data <- co_matrix[
      order_index,
      order_index,
      drop = FALSE
    ]

    if (is.null(main)) {
      main <- paste0(
        "Scanpath co-clustering: ",
        specification
      )
    }

    graphics::image(
      x = seq_len(nrow(plot_data)),
      y = seq_len(ncol(plot_data)),
      z = plot_data,
      zlim = c(0, 1),
      axes = FALSE,
      xlab = "",
      ylab = "",
      main = main,
      col = grDevices::gray.colors(
        20,
        start = 1,
        end = 0
      )
    )

    graphics::axis(
      1,
      at = seq_len(nrow(plot_data)),
      labels = rownames(plot_data),
      las = 2,
      cex.axis = label_cex
    )

    graphics::axis(
      2,
      at = seq_len(ncol(plot_data)),
      labels = colnames(plot_data),
      las = 2,
      cex.axis = label_cex
    )

    graphics::box()
  }

  if (identical(plot, "ari")) {
    plot_data <- x$iteration_summary[
      x$iteration_summary$specification ==
        specification,
      ,
      drop = FALSE
    ]

    if (is.null(main)) {
      main <- paste0(
        "Adjusted Rand stability: ",
        specification
      )
    }

    graphics::hist(
      plot_data$adjusted_rand_index,
      breaks = "FD",
      xlim = c(-1, 1),
      xlab = "Adjusted Rand index",
      main = main
    )

    graphics::abline(
      v = mean(
        plot_data$adjusted_rand_index,
        na.rm = TRUE
      ),
      lty = 2
    )
  }

  if (identical(plot, "sequence")) {
    summary_object <-
      summarise_gazepoint_scanpath_cluster_stability(
        x,
        min_pair_coverage =
          min_pair_coverage,
        stable_threshold =
          stable_threshold
      )

    plot_data <- summary_object$sequence_summary[
      summary_object$sequence_summary$specification ==
        specification,
      ,
      drop = FALSE
    ]

    plot_data <- plot_data[
      order(
        plot_data$reference_cluster,
        plot_data$within_cluster_stability
      ),
      ,
      drop = FALSE
    ]

    if (is.null(main)) {
      main <- paste0(
        "Sequence stability: ",
        specification
      )
    }

    graphics::barplot(
      plot_data$within_cluster_stability,
      names.arg = plot_data$sequence_id,
      ylim = c(0, 1),
      las = 2,
      cex.names = label_cex,
      ylab = "Within-cluster co-clustering",
      main = main
    )

    graphics::abline(
      h = stable_threshold,
      lty = 2
    )
  }

  invisible(
    list(
      plot = plot,
      specification = specification,
      data = plot_data
    )
  )
}

.gp3_stability_specifications <- function(
    method,
    linkages) {

  if (identical(method, "pam")) {
    if (!requireNamespace(
      "cluster",
      quietly = TRUE
    )) {
      stop(
        "Package 'cluster' is required for PAM stability analysis.",
        call. = FALSE
      )
    }

    return(
      data.frame(
        specification = "pam",
        method = "pam",
        linkage = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }

  allowed_linkages <- c(
    "average",
    "complete",
    "single",
    "ward.D2",
    "ward.D",
    "mcquitty",
    "median",
    "centroid"
  )

  if (!is.character(linkages) ||
      length(linkages) < 1L ||
      anyNA(linkages) ||
      any(!nzchar(linkages))) {
    stop(
      "`linkages` must contain one or more linkage names.",
      call. = FALSE
    )
  }

  linkages <- unique(linkages)
  invalid <- setdiff(
    linkages,
    allowed_linkages
  )

  if (length(invalid)) {
    stop(
      paste0(
        "Unsupported linkage method(s): ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  data.frame(
    specification = paste0(
      "hierarchical_",
      linkages
    ),
    method = "hierarchical",
    linkage = linkages,
    stringsAsFactors = FALSE
  )
}

.gp3_stability_with_seed <- function(
    seed,
    code) {

  if (is.null(seed)) {
    return(force(code))
  }

  if (!is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      !is.finite(seed) ||
      seed != as.integer(seed)) {
    stop(
      "`seed` must be NULL or one finite integer.",
      call. = FALSE
    )
  }

  seed_exists <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (seed_exists) {
    old_seed <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }

  on.exit(
    {
      if (seed_exists) {
        assign(
          ".Random.seed",
          old_seed,
          envir = .GlobalEnv
        )
      } else if (exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  set.seed(as.integer(seed))
  force(code)
}

.gp3_adjusted_rand_index <- function(
    reference,
    candidate) {

  if (length(reference) != length(candidate)) {
    stop(
      "Partitions must have the same length.",
      call. = FALSE
    )
  }

  keep <- !is.na(reference) &
    !is.na(candidate)

  reference <- reference[keep]
  candidate <- candidate[keep]

  n <- length(reference)

  if (n < 2L) {
    return(NA_real_)
  }

  contingency <- table(
    reference,
    candidate
  )

  choose_two <- function(value) {
    value * (value - 1) / 2
  }

  index <- sum(
    choose_two(contingency)
  )

  row_index <- sum(
    choose_two(
      rowSums(contingency)
    )
  )

  column_index <- sum(
    choose_two(
      colSums(contingency)
    )
  )

  total_pairs <- choose_two(n)

  expected <- row_index *
    column_index /
    total_pairs

  maximum <- (
    row_index +
      column_index
  ) / 2

  denominator <- maximum - expected

  if (abs(denominator) <=
      sqrt(.Machine$double.eps)) {
    same_partition <- outer(
      reference,
      reference,
      FUN = "=="
    ) == outer(
      candidate,
      candidate,
      FUN = "=="
    )

    return(
      if (all(same_partition)) {
        1
      } else {
        0
      }
    )
  }

  (index - expected) / denominator
}

.gp3_map_resampled_clusters <- function(
    reference_cluster,
    resampled_cluster) {

  resampled_levels <- sort(
    unique(resampled_cluster)
  )

  out <- stats::setNames(
    integer(length(resampled_levels)),
    as.character(resampled_levels)
  )

  for (cluster_id in resampled_levels) {
    reference_values <- reference_cluster[
      resampled_cluster == cluster_id
    ]

    counts <- table(reference_values)
    maximum <- max(counts)

    candidates <- as.integer(
      names(counts)[counts == maximum]
    )

    out[[as.character(cluster_id)]] <-
      min(candidates)
  }

  out
}

.gp3_summarise_representative_events <- function(
    representative_events,
    inclusion_counts,
    reference_fits,
    specifications) {

  rows <- list()
  row_index <- 0L

  for (spec_index in seq_len(
    nrow(specifications)
  )) {
    specification <-
      specifications$specification[[spec_index]]

    reference_fit <-
      reference_fits[[specification]]

    reference_cluster <-
      reference_fit$assignments$cluster

    names(reference_cluster) <-
      reference_fit$assignments$sequence_id

    counts <- inclusion_counts[[specification]]

    events <- representative_events[
      representative_events$specification ==
        specification,
      ,
      drop = FALSE
    ]

    for (sequence_id in names(reference_cluster)) {
      cluster_id <-
        reference_cluster[[sequence_id]]

      selected <- sum(
        events$sequence_id == sequence_id &
          events$reference_cluster ==
            cluster_id
      )

      included <- counts[[sequence_id]]

      row_index <- row_index + 1L

      rows[[row_index]] <- data.frame(
        specification = specification,
        sequence_id = sequence_id,
        reference_cluster = cluster_id,
        n_included = included,
        n_selected_as_representative =
          selected,
        representative_rate_when_included =
          if (included > 0L) {
            selected / included
          } else {
            NA_real_
          },
        stringsAsFactors = FALSE
      )
    }
  }

  out <- do.call(
    rbind,
    rows
  )

  rownames(out) <- NULL
  out
}

.gp3_check_scanpath_bootstrap <- function(x) {
  if (!inherits(
    x,
    "gp3_scanpath_cluster_bootstrap"
  )) {
    stop(
      paste0(
        "`x` must be returned by ",
        "`bootstrap_gazepoint_scanpath_clusters()`."
      ),
      call. = FALSE
    )
  }

  required <- c(
    "reference_fits",
    "co_clustering",
    "pair_coverage",
    "iteration_summary",
    "representative_stability",
    "specifications",
    "settings"
  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing)) {
    stop(
      paste0(
        "The bootstrap result is missing component(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_stability_check_positive_integer <- function(
    x,
    argument) {

  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x != as.integer(x) ||
      x < 1L) {
    stop(
      paste0(
        "`",
        argument,
        "` must be one positive integer."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_mean_or_na <- function(x) {
  x <- x[is.finite(x)]

  if (!length(x)) {
    return(NA_real_)
  }

  mean(x)
}
