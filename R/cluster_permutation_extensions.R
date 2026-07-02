#' Prepare time-course data for Gazepoint cluster-permutation testing
#'
#' `prepare_gazepoint_timecourse_test_data()` converts a long-format
#' subject-by-condition-by-time data frame into the internal column contract used
#' by `run_gazepoint_cluster_permutation()`. It is intended for conservative
#' two-condition, within-subject, one-dimensional time-course workflows.
#'
#' @param data A long-format data frame.
#' @param subject_col Column identifying participants or paired units.
#' @param condition_col Column identifying the two within-subject conditions.
#' @param time_col Numeric time or time-bin column.
#' @param outcome_col Numeric outcome column.
#' @param condition_order Optional character vector of length two giving the
#'   condition order used for contrasts.
#' @param aggregate_fun Function used when duplicate subject-condition-time rows
#'   are present. Defaults to `mean`.
#' @param complete_only If `TRUE`, keep only subject-by-time cells with both
#'   conditions present.
#'
#' @return A data frame with internal cluster columns:
#'   `.gp3_cluster_subject`, `.gp3_cluster_condition`,
#'   `.gp3_cluster_time_bin`, and `.gp3_cluster_outcome`.
#' @export
#'
#' @examples
#' d <- data.frame(
#'   subject = rep(1:4, each = 6),
#'   condition = rep(rep(c("A", "B"), each = 3), 4),
#'   time = rep(1:3, 8),
#'   value = rnorm(24)
#' )
#'
#' prepare_gazepoint_timecourse_test_data(
#'   d,
#'   subject_col = "subject",
#'   condition_col = "condition",
#'   time_col = "time",
#'   outcome_col = "value"
#' )
prepare_gazepoint_timecourse_test_data <- function(data,
                                                   subject_col,
                                                   condition_col,
                                                   time_col,
                                                   outcome_col,
                                                   condition_order = NULL,
                                                   aggregate_fun = mean,
                                                   complete_only = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  subject_col <- .gp3_cluster_ext_scalar_string(subject_col, "subject_col")
  condition_col <- .gp3_cluster_ext_scalar_string(condition_col, "condition_col")
  time_col <- .gp3_cluster_ext_scalar_string(time_col, "time_col")
  outcome_col <- .gp3_cluster_ext_scalar_string(outcome_col, "outcome_col")

  .gp3_cluster_ext_check_columns(
    data,
    c(subject_col, condition_col, time_col, outcome_col)
  )

  if (!is.function(aggregate_fun)) {
    stop("`aggregate_fun` must be a function.", call. = FALSE)
  }

  .gp3_cluster_ext_logical_scalar(complete_only, "complete_only")

  dat <- data.frame(
    .gp3_cluster_subject = as.character(data[[subject_col]]),
    .gp3_cluster_condition = as.character(data[[condition_col]]),
    .gp3_cluster_time_bin = suppressWarnings(as.numeric(data[[time_col]])),
    .gp3_cluster_outcome = suppressWarnings(as.numeric(data[[outcome_col]])),
    stringsAsFactors = FALSE
  )

  dat <- dat[
    !is.na(dat$.gp3_cluster_subject) &
      !is.na(dat$.gp3_cluster_condition) &
      is.finite(dat$.gp3_cluster_time_bin) &
      is.finite(dat$.gp3_cluster_outcome),
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop("No valid time-course rows remained after preparation.", call. = FALSE)
  }

  available_conditions <- unique(dat$.gp3_cluster_condition)

  if (is.null(condition_order)) {
    condition_order <- available_conditions
  } else {
    if (!is.character(condition_order) ||
        length(condition_order) != 2L ||
        any(is.na(condition_order)) ||
        any(!nzchar(condition_order))) {
      stop("`condition_order` must be NULL or a character vector of length two.", call. = FALSE)
    }
  }

  condition_order <- unique(condition_order)

  if (length(condition_order) != 2L) {
    stop(
      "Cluster-permutation preparation requires exactly two conditions. Found: ",
      paste(available_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  missing_conditions <- setdiff(condition_order, available_conditions)

  if (length(missing_conditions) > 0L) {
    stop(
      "Requested condition(s) not found in `data`: ",
      paste(missing_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- dat[
    dat$.gp3_cluster_condition %in% condition_order,
    ,
    drop = FALSE
  ]

  dat$.gp3_cluster_condition <- factor(
    dat$.gp3_cluster_condition,
    levels = condition_order
  )

  dat <- stats::aggregate(
    .gp3_cluster_outcome ~
      .gp3_cluster_subject +
      .gp3_cluster_condition +
      .gp3_cluster_time_bin,
    data = dat,
    FUN = function(x) aggregate_fun(x, na.rm = TRUE)
  )

  if (isTRUE(complete_only)) {
    cell_key <- paste(
      dat$.gp3_cluster_subject,
      dat$.gp3_cluster_time_bin,
      sep = "\r"
    )

    cell_n <- stats::ave(
      as.character(dat$.gp3_cluster_condition),
      cell_key,
      FUN = function(x) length(unique(x))
    )

    dat <- dat[cell_n == 2L, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No complete paired subject-by-time cells remained after preparation.",
      call. = FALSE
    )
  }

  dat$.gp3_cluster_status <- "ok"

  dat <- dat[order(
    dat$.gp3_cluster_subject,
    dat$.gp3_cluster_time_bin,
    dat$.gp3_cluster_condition
  ), , drop = FALSE]

  rownames(dat) <- NULL
  class(dat) <- unique(c("gp3_timecourse_test_data", class(dat)))

  dat
}


#' Summarize Gazepoint time clusters
#'
#' `summarize_gazepoint_time_clusters()` provides a compact US-spelling summary
#' of cluster-permutation output from `run_gazepoint_cluster_permutation()`.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param alpha Significance threshold used for the descriptive
#'   `cluster_significant` flag.
#'
#' @return A data frame with one row per cluster. If no clusters are present, an
#'   empty data frame with the expected columns is returned.
#' @export
#'
#' @examples
#' # See run_gazepoint_cluster_permutation() for the inferential workflow.
summarize_gazepoint_time_clusters <- function(result, alpha = 0.05) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  if (!"clusters" %in% names(result)) {
    stop("`result` must contain a `clusters` element.", call. = FALSE)
  }

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  clusters <- as.data.frame(result$clusters, stringsAsFactors = FALSE)

  out_cols <- c(
    "cluster_id",
    "cluster_direction",
    "start_time_bin",
    "end_time_bin",
    "n_time_bins",
    "cluster_statistic",
    "p_value",
    "cluster_significant",
    "cluster_summary_status"
  )

  if (nrow(clusters) == 0L) {
    out <- data.frame(
      cluster_id = integer(0),
      cluster_direction = character(0),
      start_time_bin = numeric(0),
      end_time_bin = numeric(0),
      n_time_bins = integer(0),
      cluster_statistic = numeric(0),
      p_value = numeric(0),
      cluster_significant = logical(0),
      cluster_summary_status = character(0),
      stringsAsFactors = FALSE
    )
    return(out[, out_cols])
  }

  required <- c("cluster_id", "start_time_bin", "end_time_bin", "p_value")
  .gp3_cluster_ext_check_columns(clusters, required)

  get_col <- function(name, default) {
    if (name %in% names(clusters)) clusters[[name]] else default
  }

  start_time <- as.numeric(clusters$start_time_bin)
  end_time <- as.numeric(clusters$end_time_bin)

  out <- data.frame(
    cluster_id = clusters$cluster_id,
    cluster_direction = as.character(
      get_col("cluster_direction", rep(NA_character_, nrow(clusters)))
    ),
    start_time_bin = start_time,
    end_time_bin = end_time,
    n_time_bins = as.integer(end_time - start_time + 1),
    cluster_statistic = as.numeric(
      get_col("cluster_statistic", rep(NA_real_, nrow(clusters)))
    ),
    p_value = as.numeric(clusters$p_value),
    cluster_significant = as.numeric(clusters$p_value) < alpha,
    cluster_summary_status = "ok",
    stringsAsFactors = FALSE
  )

  out[, out_cols]
}


#' Plot a Gazepoint cluster-permutation result
#'
#' `plot_gazepoint_cluster_permutation()` is a compatibility wrapper around
#' `plot_gazepoint_cluster_results()`. It uses the existing validated gp3tools
#' plotting engine while providing a name aligned with the cluster-permutation
#' workflow.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param ... Additional arguments passed to
#'   `plot_gazepoint_cluster_results()`.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_cluster_permutation <- function(result, ...) {
  plot_gazepoint_cluster_results(result, ...)
}


.gp3_cluster_ext_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a non-empty character scalar.", call. = FALSE)
  }

  x
}


.gp3_cluster_ext_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(x)
}


.gp3_cluster_ext_check_columns <- function(data, cols) {
  cols <- cols[!is.na(cols) & nzchar(cols)]
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
