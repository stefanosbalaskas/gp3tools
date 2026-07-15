#' Select the number of scanpath clusters
#'
#' Compare candidate cluster counts using mean silhouette width.
#' Input formats and sequence-preparation arguments match
#' [cluster_gazepoint_scanpaths()].
#'
#' @param x Long-format AOI data, a pairwise-distance data frame, a
#'   square numeric distance matrix, or a `dist` object.
#' @param k_values Candidate cluster counts. When `NULL`, values from
#'   2 through the smaller of 6 or one fewer than the number of
#'   scanpaths are evaluated.
#' @param method Clustering method: `"hierarchical"` or `"pam"`.
#' @param linkage Hierarchical linkage method passed to
#'   [stats::hclust()].
#' @param aoi_col AOI column when `x` is long-format AOI data.
#' @param group_cols Columns identifying independent scanpaths.
#' @param time_col Optional ordering column.
#' @param distance_col Distance column for pairwise-distance data.
#' @param include_missing Should missing AOI labels be retained?
#' @param missing_label Label used for retained missing AOIs.
#' @param collapse_repeats Should consecutive repeated AOIs be collapsed?
#' @param max_sequences Maximum number of scanpaths permitted when
#'   pairwise distances must be calculated.
#'
#' @return An object of class `gp3_scanpath_cluster_selection` containing
#'   candidate diagnostics, the recommended number of clusters, all
#'   fitted solutions, the recommended fit, and the distance object.
#'
#' @export
#'
#' @examples
#' d <- matrix(
#'   c(
#'     0, 0.1, 1, 1,
#'     0.1, 0, 1, 1,
#'     1, 1, 0, 0.1,
#'     1, 1, 0.1, 0
#'   ),
#'   nrow = 4,
#'   byrow = TRUE,
#'   dimnames = list(LETTERS[1:4], LETTERS[1:4])
#' )
#'
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   result <- select_gazepoint_scanpath_clusters(
#'     d,
#'     k_values = 2:3
#'   )
#'   result$diagnostics
#' }
select_gazepoint_scanpath_clusters <- function(
    x,
    k_values = NULL,
    method = c("hierarchical", "pam"),
    linkage = c(
      "average",
      "complete",
      "single",
      "ward.D2",
      "ward.D",
      "mcquitty",
      "median",
      "centroid"
    ),
    aoi_col = NULL,
    group_cols = NULL,
    time_col = NULL,
    distance_col = "normalized_distance",
    include_missing = FALSE,
    missing_label = "missing",
    collapse_repeats = FALSE,
    max_sequences = 200) {

  method <- match.arg(method)
  linkage <- match.arg(linkage)

  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop(
      paste0(
        "Package 'cluster' is required to compare candidate ",
        "cluster solutions using silhouette widths."
      ),
      call. = FALSE
    )
  }

  prepared <- cluster_gazepoint_scanpaths(
    x = x,
    k = 2L,
    method = "hierarchical",
    linkage = linkage,
    aoi_col = aoi_col,
    group_cols = group_cols,
    time_col = time_col,
    distance_col = distance_col,
    include_missing = include_missing,
    missing_label = missing_label,
    collapse_repeats = collapse_repeats,
    max_sequences = max_sequences
  )

  distance <- prepared$distance
  n_sequences <- attr(distance, "Size")

  if (is.null(k_values)) {
    k_values <- seq.int(
      from = 2L,
      to = min(6L, n_sequences - 1L)
    )
  }

  k_values <- .gp3_validate_scanpath_k_values(
    k_values,
    n_sequences = n_sequences
  )

  fits <- lapply(
    k_values,
    function(k_value) {
      cluster_gazepoint_scanpaths(
        x = distance,
        k = k_value,
        method = method,
        linkage = linkage
      )
    }
  )

  names(fits) <- as.character(k_values)

  mean_widths <- vapply(
    fits,
    function(fit) fit$mean_silhouette_width,
    numeric(1)
  )

  if (any(!is.finite(mean_widths))) {
    stop(
      "One or more candidate solutions lacked finite silhouette widths.",
      call. = FALSE
    )
  }

  diagnostics <- data.frame(
    k = k_values,
    mean_silhouette_width = unname(mean_widths),
    n_clusters = vapply(
      fits,
      function(fit) length(unique(fit$assignments$cluster)),
      integer(1)
    ),
    method = rep(method, length(k_values)),
    stringsAsFactors = FALSE
  )

  best_index <- which.max(
    diagnostics$mean_silhouette_width
  )

  recommended_k <- diagnostics$k[[best_index]]

  out <- list(
    diagnostics = diagnostics,
    recommended_k = recommended_k,
    recommended_fit = fits[[best_index]],
    fits = fits,
    distance = distance,
    method = method,
    linkage = if (identical(method, "hierarchical")) {
      linkage
    } else {
      NA_character_
    },
    distance_source = prepared$distance_source,
    criterion = "mean_silhouette_width",
    selection_status = "ok",
    call = match.call()
  )

  class(out) <- c(
    "gp3_scanpath_cluster_selection",
    "list"
  )

  out
}

#' Extract representative scanpaths
#'
#' Select representative observed scanpaths by minimizing mean distance
#' to other members of the same fitted cluster.
#'
#' @param x An object returned by [cluster_gazepoint_scanpaths()].
#' @param n_per_cluster Number of representatives to return per cluster.
#'
#' @return A data frame containing cluster, representative rank, sequence
#'   identifier, mean within-cluster distance, cluster size, and PAM
#'   medoid status.
#'
#' @export
#'
#' @examples
#' d <- matrix(
#'   c(
#'     0, 0.1, 1, 1,
#'     0.1, 0, 1, 1,
#'     1, 1, 0, 0.1,
#'     1, 1, 0.1, 0
#'   ),
#'   nrow = 4,
#'   byrow = TRUE,
#'   dimnames = list(LETTERS[1:4], LETTERS[1:4])
#' )
#'
#' fit <- cluster_gazepoint_scanpaths(d, k = 2)
#' extract_gazepoint_representative_scanpaths(fit)
extract_gazepoint_representative_scanpaths <- function(
    x,
    n_per_cluster = 1L) {

  .gp3_check_scanpath_cluster_result(x)

  if (!is.numeric(n_per_cluster) ||
      length(n_per_cluster) != 1L ||
      is.na(n_per_cluster) ||
      !is.finite(n_per_cluster) ||
      n_per_cluster != as.integer(n_per_cluster) ||
      n_per_cluster < 1L) {
    stop(
      "`n_per_cluster` must be one positive integer.",
      call. = FALSE
    )
  }

  n_per_cluster <- as.integer(n_per_cluster)

  distance_matrix <- as.matrix(x$distance)
  sequence_ids <- rownames(distance_matrix)

  assignments <- x$assignments
  assignments$sequence_id <- as.character(
    assignments$sequence_id
  )
  assignments$cluster <- as.integer(
    assignments$cluster
  )

  if (anyDuplicated(assignments$sequence_id)) {
    stop(
      "Cluster assignments contain duplicated sequence identifiers.",
      call. = FALSE
    )
  }

  missing_ids <- setdiff(
    sequence_ids,
    assignments$sequence_id
  )

  if (length(missing_ids) > 0L) {
    stop(
      paste0(
        "Cluster assignments are missing sequence identifier(s): ",
        paste(missing_ids, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  assignments <- assignments[
    match(sequence_ids, assignments$sequence_id),
    ,
    drop = FALSE
  ]

  model_medoids <- if (is.null(x$medoids)) {
    character(0)
  } else {
    as.character(x$medoids)
  }

  cluster_ids <- sort(unique(assignments$cluster))
  result_rows <- vector("list", length(cluster_ids))

  for (i in seq_along(cluster_ids)) {
    cluster_id <- cluster_ids[[i]]

    member_ids <- assignments$sequence_id[
      assignments$cluster == cluster_id
    ]

    cluster_size <- length(member_ids)

    within_matrix <- distance_matrix[
      member_ids,
      member_ids,
      drop = FALSE
    ]

    if (cluster_size == 1L) {
      mean_distance <- 0
      names(mean_distance) <- member_ids
    } else {
      mean_distance <- rowSums(within_matrix) /
        (cluster_size - 1L)
    }

    ordered_members <- order(
      mean_distance,
      names(mean_distance)
    )

    n_take <- min(
      n_per_cluster,
      length(ordered_members)
    )

    selected <- ordered_members[seq_len(n_take)]
    selected_ids <- names(mean_distance)[selected]

    result_rows[[i]] <- data.frame(
      cluster = rep(cluster_id, n_take),
      representative_rank = seq_len(n_take),
      sequence_id = selected_ids,
      mean_within_cluster_distance = as.numeric(
        mean_distance[selected]
      ),
      cluster_size = rep(cluster_size, n_take),
      is_model_medoid = selected_ids %in% model_medoids,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(
    what = rbind,
    args = result_rows
  )

  rownames(out) <- NULL

  class(out) <- c(
    "gp3_scanpath_representatives",
    "data.frame"
  )

  out
}

#' Plot fitted scanpath clusters
#'
#' Create base-R MDS, dendrogram, or silhouette diagnostics for an
#' object returned by [cluster_gazepoint_scanpaths()].
#'
#' These displays describe distance structure and cluster separation.
#' They do not establish distinct cognitive or psychological strategies.
#'
#' @param x An object returned by [cluster_gazepoint_scanpaths()].
#' @param plot Plot type: `"mds"`, `"dendrogram"`, or
#'   `"silhouette"`.
#' @param labels Should scanpath identifiers be displayed?
#' @param main Optional plot title.
#' @param xlab Optional horizontal-axis label.
#' @param ylab Optional vertical-axis label.
#' @param point_cex Point-size multiplier for the MDS display.
#' @param label_cex Label-size multiplier.
#'
#' @return Invisibly returns a list containing the plot type, plot data,
#'   clustering method, and number of clusters.
#'
#' @export
#'
#' @examples
#' d <- matrix(
#'   c(
#'     0, 0.1, 1, 1,
#'     0.1, 0, 1, 1,
#'     1, 1, 0, 0.1,
#'     1, 1, 0.1, 0
#'   ),
#'   nrow = 4,
#'   byrow = TRUE,
#'   dimnames = list(LETTERS[1:4], LETTERS[1:4])
#' )
#'
#' fit <- cluster_gazepoint_scanpaths(d, k = 2)
#' plot_gazepoint_scanpath_clusters(fit, plot = "mds")
plot_gazepoint_scanpath_clusters <- function(
    x,
    plot = c("mds", "dendrogram", "silhouette"),
    labels = TRUE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    point_cex = 1.2,
    label_cex = 0.8) {

  .gp3_check_scanpath_cluster_result(x)

  plot <- match.arg(plot)

  if (!is.logical(labels) ||
      length(labels) != 1L ||
      is.na(labels)) {
    stop(
      "`labels` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  .gp3_check_positive_plot_number(
    point_cex,
    "point_cex"
  )

  .gp3_check_positive_plot_number(
    label_cex,
    "label_cex"
  )

  assignments <- x$assignments
  assignments$sequence_id <- as.character(
    assignments$sequence_id
  )
  assignments$cluster <- as.integer(
    assignments$cluster
  )

  cluster_levels <- sort(unique(assignments$cluster))
  cluster_colours <- seq_along(cluster_levels)

  if (identical(plot, "dendrogram")) {
    if (!identical(x$method, "hierarchical") ||
        !inherits(x$model, "hclust")) {
      stop(
        paste0(
          "The dendrogram display requires a hierarchical ",
          "clustering result."
        ),
        call. = FALSE
      )
    }

    if (is.null(main)) {
      main <- "Hierarchical scanpath clustering"
    }

    if (is.null(xlab)) {
      xlab <- "Scanpath"
    }

    if (is.null(ylab)) {
      ylab <- "Dissimilarity"
    }

    dendrogram_labels <- if (labels) {
      x$model$labels
    } else {
      FALSE
    }

    graphics::plot(
      x$model,
      labels = dendrogram_labels,
      hang = -1,
      main = main,
      xlab = xlab,
      ylab = ylab,
      sub = ""
    )

    stats::rect.hclust(
      x$model,
      k = x$k,
      border = cluster_colours
    )

    plot_data <- assignments
  }

  if (identical(plot, "mds")) {
    if (is.null(main)) {
      main <- "Scanpath-cluster distance structure"
    }

    if (is.null(xlab)) {
      xlab <- "MDS dimension 1"
    }

    if (is.null(ylab)) {
      ylab <- "MDS dimension 2"
    }

    mds <- tryCatch(
      stats::cmdscale(
        d = x$distance,
        k = 2L,
        eig = TRUE,
        add = TRUE
      ),
      error = identity
    )

    if (inherits(mds, "error")) {
      stop(
        paste0(
          "Could not compute the MDS representation: ",
          conditionMessage(mds)
        ),
        call. = FALSE
      )
    }

    coordinates <- as.matrix(mds$points)

    if (is.null(rownames(coordinates))) {
      rownames(coordinates) <- attr(
        x$distance,
        "Labels"
      )
    }

    assignment_index <- match(
      rownames(coordinates),
      assignments$sequence_id
    )

    if (anyNA(assignment_index)) {
      stop(
        paste0(
          "MDS sequence identifiers could not be matched to ",
          "cluster assignments."
        ),
        call. = FALSE
      )
    }

    point_clusters <- assignments$cluster[assignment_index]
    point_colours <- match(
      point_clusters,
      cluster_levels
    )

    medoid_ids <- if (is.null(x$medoids)) {
      character(0)
    } else {
      as.character(x$medoids)
    }

    plot_data <- data.frame(
      sequence_id = rownames(coordinates),
      mds_dimension_1 = coordinates[, 1L],
      mds_dimension_2 = coordinates[, 2L],
      cluster = point_clusters,
      is_medoid = rownames(coordinates) %in% medoid_ids,
      stringsAsFactors = FALSE
    )

    graphics::plot(
      plot_data$mds_dimension_1,
      plot_data$mds_dimension_2,
      type = "n",
      asp = 1,
      main = main,
      xlab = xlab,
      ylab = ylab
    )

    graphics::points(
      plot_data$mds_dimension_1,
      plot_data$mds_dimension_2,
      pch = 19,
      cex = point_cex,
      col = point_colours
    )

    if (labels) {
      graphics::text(
        plot_data$mds_dimension_1,
        plot_data$mds_dimension_2,
        labels = plot_data$sequence_id,
        pos = 3,
        cex = label_cex
      )
    }

    if (any(plot_data$is_medoid)) {
      graphics::points(
        plot_data$mds_dimension_1[plot_data$is_medoid],
        plot_data$mds_dimension_2[plot_data$is_medoid],
        pch = 8,
        cex = point_cex * 1.4
      )
    }

    graphics::legend(
      "topright",
      legend = paste("Cluster", cluster_levels),
      pch = 19,
      col = cluster_colours,
      bty = "n"
    )
  }

  if (identical(plot, "silhouette")) {
    if (is.null(x$silhouette)) {
      stop(
        paste0(
          "Silhouette diagnostics are unavailable. Install the ",
          "optional 'cluster' package and refit the result."
        ),
        call. = FALSE
      )
    }

    if (is.null(main)) {
      main <- "Scanpath-cluster silhouette widths"
    }

    if (is.null(xlab)) {
      xlab <- "Silhouette width"
    }

    order_index <- order(
      x$silhouette$cluster,
      x$silhouette$silhouette_width
    )

    plot_data <- x$silhouette[
      order_index,
      ,
      drop = FALSE
    ]

    silhouette_colours <- match(
      plot_data$cluster,
      cluster_levels
    )

    name_labels <- if (labels) {
      plot_data$sequence_id
    } else {
      rep("", nrow(plot_data))
    }

    graphics::barplot(
      plot_data$silhouette_width,
      horiz = TRUE,
      names.arg = name_labels,
      las = 1,
      col = silhouette_colours,
      border = NA,
      cex.names = label_cex,
      main = main,
      xlab = xlab
    )

    graphics::abline(
      v = 0,
      lty = 2
    )

    graphics::legend(
      "bottomright",
      legend = paste("Cluster", cluster_levels),
      fill = cluster_colours,
      bty = "n"
    )
  }

  invisible(
    list(
      plot = plot,
      data = plot_data,
      method = x$method,
      k = x$k
    )
  )
}

.gp3_validate_scanpath_k_values <- function(
    k_values,
    n_sequences) {

  if (!is.numeric(k_values) ||
      length(k_values) < 1L ||
      anyNA(k_values) ||
      any(!is.finite(k_values)) ||
      any(k_values != as.integer(k_values))) {
    stop(
      "`k_values` must contain finite integers.",
      call. = FALSE
    )
  }

  k_values <- sort(unique(as.integer(k_values)))

  invalid <- k_values < 2L |
    k_values >= n_sequences

  if (any(invalid)) {
    stop(
      paste0(
        "Every candidate in `k_values` must be at least 2 and ",
        "smaller than the number of scanpaths (",
        n_sequences,
        "). Invalid value(s): ",
        paste(k_values[invalid], collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  k_values
}

.gp3_check_scanpath_cluster_result <- function(x) {
  if (!inherits(x, "gp3_scanpath_clusters")) {
    stop(
      "`x` must be returned by `cluster_gazepoint_scanpaths()`.",
      call. = FALSE
    )
  }

  required_components <- c(
    "assignments",
    "distance",
    "model",
    "k",
    "method"
  )

  missing_components <- setdiff(
    required_components,
    names(x)
  )

  if (length(missing_components) > 0L) {
    stop(
      paste0(
        "The clustering result is missing component(s): ",
        paste(missing_components, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!inherits(x$distance, "dist")) {
    stop(
      "The clustering result lacks a valid `dist` object.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "sequence_id",
    "cluster"
  )

  if (!is.data.frame(x$assignments) ||
      !all(required_columns %in% names(x$assignments))) {
    stop(
      paste0(
        "The clustering result must contain an assignments data ",
        "frame with `sequence_id` and `cluster` columns."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_check_positive_plot_number <- function(
    x,
    argument_name) {

  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop(
      paste0(
        "`",
        argument_name,
        "` must be one positive finite number."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
