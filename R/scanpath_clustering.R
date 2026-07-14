#' Cluster Gazepoint AOI scanpaths
#'
#' Cluster scanpaths from long-format AOI observations, pairwise scanpath
#' distances, a numeric distance matrix, or a \code{"dist"} object.
#'
#' Long-format AOI data are converted to pairwise normalized edit distances
#' with \code{\link{compute_gazepoint_scanpath_similarity}}. Hierarchical
#' clustering uses base R. Partitioning around medoids requires the optional
#' \pkg{cluster} package.
#'
#' @param x One of:
#' \itemize{
#'   \item a long-format AOI data frame;
#'   \item output from \code{\link{compute_gazepoint_scanpath_similarity}};
#'   \item a square numeric distance matrix; or
#'   \item a \code{"dist"} object.
#' }
#' @param k Integer number of clusters. Must be at least 2 and smaller than
#' the number of scanpaths.
#' @param method Clustering method: \code{"hierarchical"} or \code{"pam"}.
#' @param linkage Hierarchical linkage method passed to
#' \code{\link[stats]{hclust}}.
#' @param aoi_col AOI column when \code{x} is long-format AOI data.
#' @param group_cols Columns identifying independent scanpaths when \code{x}
#' is long-format AOI data.
#' @param time_col Optional ordering column for long-format AOI data.
#' @param distance_col Distance column when \code{x} is a pairwise-distance
#' data frame. Defaults to \code{"normalized_distance"}.
#' @param include_missing Should missing AOI labels be retained as a state?
#' @param missing_label Label used when retaining missing AOIs.
#' @param collapse_repeats Should consecutive repeated AOI labels be collapsed
#' before pairwise distances are calculated?
#' @param max_sequences Maximum number of grouped scanpaths permitted when
#' pairwise distances must be calculated.
#'
#' @return An object of class \code{"gp3_scanpath_clusters"} containing:
#' \itemize{
#'   \item \code{assignments}: scanpath identifiers and cluster assignments;
#'   \item \code{distance}: the clustering distance object;
#'   \item \code{model}: the fitted hierarchical or PAM model;
#'   \item \code{medoids}: PAM medoid identifiers, when applicable;
#'   \item \code{silhouette}: scanpath-level silhouette diagnostics when
#'   \pkg{cluster} is available;
#'   \item clustering settings and status fields.
#' }
#'
#' @export
#'
#' @examples
#' distance_matrix <- matrix(
#'   c(
#'     0, 1, 5, 6,
#'     1, 0, 6, 5,
#'     5, 6, 0, 1,
#'     6, 5, 1, 0
#'   ),
#'   nrow = 4,
#'   byrow = TRUE,
#'   dimnames = list(
#'     c("scanpath_1", "scanpath_2", "scanpath_3", "scanpath_4"),
#'     c("scanpath_1", "scanpath_2", "scanpath_3", "scanpath_4")
#'   )
#' )
#'
#' result <- cluster_gazepoint_scanpaths(
#'   distance_matrix,
#'   k = 2,
#'   method = "hierarchical"
#' )
#'
#' result$assignments
cluster_gazepoint_scanpaths <- function(
    x,
    k = 3,
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

  if (!is.numeric(k) ||
      length(k) != 1L ||
      is.na(k) ||
      !is.finite(k) ||
      k != as.integer(k)) {
    stop("`k` must be one finite integer.", call. = FALSE)
  }

  k <- as.integer(k)

  .gp3_sequence_check_scalar_string(distance_col, "distance_col")

  pairwise_distances <- NULL
  distance_source <- NULL

  if (inherits(x, "dist")) {
    distance <- .gp3_scanpath_validate_distance_matrix(as.matrix(x))
    distance_source <- "dist_object"
  } else if (is.matrix(x)) {
    distance <- .gp3_scanpath_validate_distance_matrix(x)
    distance_source <- "distance_matrix"
  } else if (
    is.data.frame(x) &&
      all(c("sequence_a", "sequence_b", distance_col) %in% names(x))
  ) {
    pairwise_distances <- x
    distance <- .gp3_scanpath_pairs_to_dist(
      x,
      distance_col = distance_col
    )
    distance_source <- "pairwise_distance_table"
  } else if (is.data.frame(x)) {
    if (is.null(aoi_col) || is.null(group_cols)) {
      stop(
        paste0(
          "Supply `aoi_col` and `group_cols` when `x` is long-format ",
          "AOI data."
        ),
        call. = FALSE
      )
    }

    pairwise_distances <- compute_gazepoint_scanpath_similarity(
      data = x,
      aoi_col = aoi_col,
      group_cols = group_cols,
      time_col = time_col,
      include_missing = include_missing,
      missing_label = missing_label,
      collapse_repeats = collapse_repeats,
      max_sequences = max_sequences
    )

    distance <- .gp3_scanpath_pairs_to_dist(
      pairwise_distances,
      distance_col = distance_col
    )

    distance_source <- "long_aoi_data"
  } else {
    stop(
      paste0(
        "`x` must be long-format AOI data, a pairwise-distance data ",
        "frame, a numeric distance matrix, or a `dist` object."
      ),
      call. = FALSE
    )
  }

  n_sequences <- attr(distance, "Size")
  sequence_ids <- attr(distance, "Labels")

  if (is.null(sequence_ids)) {
    sequence_ids <- paste0("sequence_", seq_len(n_sequences))
    attr(distance, "Labels") <- sequence_ids
  }

  if (n_sequences < 3L) {
    stop(
      "At least three scanpaths are required for clustering.",
      call. = FALSE
    )
  }

  if (k < 2L || k >= n_sequences) {
    stop(
      "`k` must be at least 2 and smaller than the number of scanpaths.",
      call. = FALSE
    )
  }

  medoids <- NULL

  if (identical(method, "hierarchical")) {
    model <- stats::hclust(
      distance,
      method = linkage
    )

    cluster_vector <- stats::cutree(
      model,
      k = k
    )
  } else {
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop(
        paste0(
          "Package 'cluster' is required for `method = \"pam\"`. ",
          "Use `method = \"hierarchical\"` for the base-R implementation."
        ),
        call. = FALSE
      )
    }

    model <- cluster::pam(
      distance,
      k = k,
      diss = TRUE
    )

    cluster_vector <- as.integer(model$clustering)
    names(cluster_vector) <- names(model$clustering)

    if (is.null(names(cluster_vector))) {
      names(cluster_vector) <- sequence_ids
    }

    medoids <- sequence_ids[as.integer(model$id.med)]
  }

  if (is.null(names(cluster_vector))) {
    names(cluster_vector) <- sequence_ids
  }

  assignments <- data.frame(
    sequence_id = names(cluster_vector),
    cluster = unname(as.integer(cluster_vector)),
    stringsAsFactors = FALSE
  )

  silhouette <- NULL
  mean_silhouette_width <- NA_real_

  if (requireNamespace("cluster", quietly = TRUE)) {
    silhouette_object <- cluster::silhouette(
      as.integer(cluster_vector),
      distance
    )

    silhouette <- data.frame(
      sequence_id = sequence_ids,
      cluster = as.integer(silhouette_object[, 1L]),
      neighbor_cluster = as.integer(silhouette_object[, 2L]),
      silhouette_width = as.numeric(silhouette_object[, 3L]),
      stringsAsFactors = FALSE
    )

    mean_silhouette_width <- mean(
      silhouette$silhouette_width,
      na.rm = TRUE
    )
  }

  out <- list(
    assignments = assignments,
    distance = distance,
    model = model,
    medoids = medoids,
    silhouette = silhouette,
    mean_silhouette_width = mean_silhouette_width,
    pairwise_distances = pairwise_distances,
    k = k,
    method = method,
    linkage = if (identical(method, "hierarchical")) linkage else NA_character_,
    distance_source = distance_source,
    clustering_status = "ok",
    call = match.call()
  )

  class(out) <- c("gp3_scanpath_clusters", "list")
  out
}

.gp3_scanpath_validate_distance_matrix <- function(x) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop(
      "The distance matrix must be a numeric matrix.",
      call. = FALSE
    )
  }

  if (nrow(x) != ncol(x)) {
    stop(
      "The distance matrix must be square.",
      call. = FALSE
    )
  }

  if (nrow(x) < 2L) {
    stop(
      "The distance matrix must contain at least two scanpaths.",
      call. = FALSE
    )
  }

  if (any(!is.finite(x))) {
    stop(
      "The distance matrix contains non-finite values.",
      call. = FALSE
    )
  }

  if (any(x < 0)) {
    stop(
      "Distances must be non-negative.",
      call. = FALSE
    )
  }

  row_ids <- rownames(x)
  col_ids <- colnames(x)

  if (is.null(row_ids) && is.null(col_ids)) {
    ids <- paste0("sequence_", seq_len(nrow(x)))
    dimnames(x) <- list(ids, ids)
  } else if (is.null(row_ids)) {
    rownames(x) <- col_ids
  } else if (is.null(col_ids)) {
    colnames(x) <- row_ids
  }

  if (!identical(rownames(x), colnames(x))) {
    stop(
      "Distance-matrix row and column names must be identical.",
      call. = FALSE
    )
  }

  tolerance <- sqrt(.Machine$double.eps) *
    max(1, max(abs(x)))

  if (max(abs(x - t(x))) > tolerance) {
    stop(
      "The distance matrix must be symmetric.",
      call. = FALSE
    )
  }

  if (any(abs(diag(x)) > tolerance)) {
    stop(
      "The distance-matrix diagonal must contain zeros.",
      call. = FALSE
    )
  }

  x <- (x + t(x)) / 2
  diag(x) <- 0

  stats::as.dist(x)
}

.gp3_scanpath_pairs_to_dist <- function(x, distance_col) {
  required_cols <- c("sequence_a", "sequence_b", distance_col)
  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "Pairwise-distance data are missing column(s): ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(x[[distance_col]])) {
    stop(
      paste0("`", distance_col, "` must be numeric."),
      call. = FALSE
    )
  }

  sequence_a <- as.character(x$sequence_a)
  sequence_b <- as.character(x$sequence_b)
  distance_values <- as.numeric(x[[distance_col]])

  invalid_ids <- is.na(sequence_a) |
    !nzchar(sequence_a) |
    is.na(sequence_b) |
    !nzchar(sequence_b)

  if (any(invalid_ids)) {
    stop(
      "Pairwise sequence identifiers must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  if (any(!is.finite(distance_values))) {
    stop(
      "Pairwise distances must be finite.",
      call. = FALSE
    )
  }

  if (any(distance_values < 0)) {
    stop(
      "Pairwise distances must be non-negative.",
      call. = FALSE
    )
  }

  sequence_ids <- unique(c(sequence_a, sequence_b))

  distance_matrix <- matrix(
    NA_real_,
    nrow = length(sequence_ids),
    ncol = length(sequence_ids),
    dimnames = list(sequence_ids, sequence_ids)
  )

  diag(distance_matrix) <- 0

  for (i in seq_along(distance_values)) {
    id_a <- sequence_a[[i]]
    id_b <- sequence_b[[i]]
    value <- distance_values[[i]]
    previous <- distance_matrix[id_a, id_b]

    if (
      !is.na(previous) &&
        abs(previous - value) >
          sqrt(.Machine$double.eps) *
            max(1, abs(previous), abs(value))
    ) {
      stop(
        paste0(
          "Conflicting distances were supplied for pair `",
          id_a,
          "` and `",
          id_b,
          "`."
        ),
        call. = FALSE
      )
    }

    distance_matrix[id_a, id_b] <- value
    distance_matrix[id_b, id_a] <- value
  }

  missing_pairs <- which(
    is.na(distance_matrix) & upper.tri(distance_matrix),
    arr.ind = TRUE
  )

  if (nrow(missing_pairs) > 0L) {
    first_missing <- missing_pairs[1L, ]

    stop(
      paste0(
        "The pairwise-distance table is incomplete; missing pair `",
        rownames(distance_matrix)[first_missing[[1L]]],
        "` and `",
        colnames(distance_matrix)[first_missing[[2L]]],
        "`."
      ),
      call. = FALSE
    )
  }

  .gp3_scanpath_validate_distance_matrix(distance_matrix)
}

