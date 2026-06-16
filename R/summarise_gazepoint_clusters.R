#' Summarise cluster-based permutation results
#'
#' Create compact reporting tables from the output of
#' `run_gazepoint_cluster_permutation()`. The function returns an overview
#' table, all observed clusters, significant clusters, time-course summary,
#' permutation-distribution summary, settings table, and circularity warning.
#'
#' Cluster-based permutation tests are intended for time-course inference.
#' They should not be used to discover a confirmatory time window and then test
#' that same window again in a second confirmatory model.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param alpha Cluster-level significance threshold.
#' @param round_digits Optional number of digits for rounding numeric reporting
#'   columns. If `NULL`, no rounding is applied.
#' @param include_timecourse Logical. If `TRUE`, include the full observed
#'   time-course table in the returned object.
#'
#' @return A list of summary tables.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_clusters <- function(
    result,
    alpha = 0.05,
    round_digits = NULL,
    include_timecourse = TRUE
) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  required_elements <- c(
    "timecourse",
    "clusters",
    "permutation_distribution",
    "settings",
    "model_status"
  )

  missing_elements <- setdiff(required_elements, names(result))

  if (length(missing_elements) > 0L) {
    stop(
      "`result` is missing required element(s): ",
      paste(missing_elements, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  if (!is.null(round_digits)) {
    if (!is.numeric(round_digits) ||
        length(round_digits) != 1L ||
        is.na(round_digits) ||
        !is.finite(round_digits) ||
        round_digits < 0) {
      stop(
        "`round_digits` must be NULL or a non-negative numeric scalar.",
        call. = FALSE
      )
    }


    round_digits <- as.integer(round_digits)


  }

  if (!is.logical(include_timecourse) ||
      length(include_timecourse) != 1L ||
      is.na(include_timecourse)) {
    stop("`include_timecourse` must be TRUE or FALSE.", call. = FALSE)
  }

  timecourse <- tibble::as_tibble(result$timecourse)
  clusters <- tibble::as_tibble(result$clusters)
  permutation_distribution <- tibble::as_tibble(
    result$permutation_distribution
  )
  settings <- result$settings

  required_timecourse_cols <- c(
    ".gp3_cluster_time_bin",
    "n_subjects",
    "mean_difference",
    "statistic",
    "cluster_id",
    "point_candidate"
  )

  missing_timecourse_cols <- setdiff(required_timecourse_cols, names(timecourse))

  if (length(missing_timecourse_cols) > 0L) {
    stop(
      "`result$timecourse` is missing required column(s): ",
      paste(missing_timecourse_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_perm_cols <- c("permutation", "max_cluster_statistic")
  missing_perm_cols <- setdiff(required_perm_cols, names(permutation_distribution))

  if (length(missing_perm_cols) > 0L) {
    stop(
      "`result$permutation_distribution` is missing required column(s): ",
      paste(missing_perm_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(clusters) > 0L) {
    required_cluster_cols <- c(
      "cluster_id",
      "cluster_direction",
      "start_time_bin",
      "end_time_bin",
      "n_time_bins",
      "cluster_statistic",
      "max_abs_statistic",
      "mean_difference",
      "p_value"
    )


    missing_cluster_cols <- setdiff(required_cluster_cols, names(clusters))

    if (length(missing_cluster_cols) > 0L) {
      stop(
        "`result$clusters` is missing required column(s): ",
        paste(missing_cluster_cols, collapse = ", "),
        call. = FALSE
      )
    }


  }

  get_setting <- function(name, default = NA_character_) {
    if (!is.null(settings[[name]])) {
      settings[[name]]
    } else {
      default
    }
  }

  time_bins <- sort(unique(timecourse$.gp3_cluster_time_bin))

  bin_step_ms <- if (length(time_bins) >= 2L) {
    stats::median(diff(time_bins), na.rm = TRUE)
  } else {
    NA_real_
  }

  n_observed_clusters <- nrow(clusters)

  if (nrow(clusters) > 0L) {
    clusters$cluster_label <- paste0("Cluster ", clusters$cluster_id)


    if (is.finite(bin_step_ms)) {
      clusters$cluster_duration_ms <-
        clusters$end_time_bin - clusters$start_time_bin + bin_step_ms
    } else {
      clusters$cluster_duration_ms <-
        clusters$end_time_bin - clusters$start_time_bin
    }

    clusters$significant_alpha <- clusters$p_value < alpha

    clusters$report_status <- ifelse(
      clusters$significant_alpha,
      "significant",
      "not_significant"
    )

    preferred_cluster_cols <- c(
      "cluster_id",
      "cluster_label",
      "cluster_direction",
      "start_time_bin",
      "end_time_bin",
      "cluster_duration_ms",
      "n_time_bins",
      "cluster_statistic",
      "max_abs_statistic",
      "mean_difference",
      "p_value",
      "significant_alpha",
      "report_status"
    )

    clusters <- clusters[
      ,
      c(
        preferred_cluster_cols,
        setdiff(names(clusters), preferred_cluster_cols)
      ),
      drop = FALSE
    ]

    clusters <- tibble::as_tibble(clusters)


  } else {
    clusters <- tibble::tibble(
      cluster_id = integer(0),
      cluster_label = character(0),
      cluster_direction = character(0),
      start_time_bin = numeric(0),
      end_time_bin = numeric(0),
      cluster_duration_ms = numeric(0),
      n_time_bins = integer(0),
      cluster_statistic = numeric(0),
      max_abs_statistic = numeric(0),
      mean_difference = numeric(0),
      p_value = numeric(0),
      significant_alpha = logical(0),
      report_status = character(0)
    )
  }

  significant_clusters <- clusters[
    clusters$significant_alpha,
    ,
    drop = FALSE
  ]

  n_significant_clusters <- nrow(significant_clusters)

  overview <- tibble::tibble(
    model_status = as.character(result$model_status),
    report_status = dplyr::case_when(
      n_observed_clusters == 0L ~ "no_observed_clusters",
      n_significant_clusters > 0L ~ "significant_cluster_evidence",
      TRUE ~ "observed_clusters_not_significant"
    ),
    alpha = alpha,
    n_subjects = if (!is.null(result$n_subjects)) {
      as.integer(result$n_subjects)
    } else {
      dplyr::n_distinct(timecourse$n_subjects)
    },
    n_time_bins = if (!is.null(result$n_time_bins)) {
      as.integer(result$n_time_bins)
    } else {
      dplyr::n_distinct(timecourse$.gp3_cluster_time_bin)
    },
    bin_step_ms = bin_step_ms,
    n_permutations = as.integer(get_setting("n_permutations", NA_integer_)),
    condition_1 = as.character(get_setting("condition_1", NA_character_)),
    condition_2 = as.character(get_setting("condition_2", NA_character_)),
    difference = as.character(get_setting("difference", NA_character_)),
    cluster_threshold = as.numeric(get_setting("cluster_threshold", NA_real_)),
    tail = as.character(get_setting("tail", NA_character_)),
    cluster_stat = as.character(get_setting("cluster_stat", NA_character_)),
    min_time_bins = as.integer(get_setting("min_time_bins", NA_integer_)),
    n_observed_clusters = n_observed_clusters,
    n_significant_clusters = n_significant_clusters
  )

  timecourse_summary <- timecourse |>
    dplyr::summarise(
      n_time_bins = dplyr::n(),
      start_time_bin = min(.data[[".gp3_cluster_time_bin"]], na.rm = TRUE),
      end_time_bin = max(.data[[".gp3_cluster_time_bin"]], na.rm = TRUE),
      min_n_subjects = min(.data[["n_subjects"]], na.rm = TRUE),
      max_n_subjects = max(.data[["n_subjects"]], na.rm = TRUE),
      mean_difference_min = min(.data[["mean_difference"]], na.rm = TRUE),
      mean_difference_max = max(.data[["mean_difference"]], na.rm = TRUE),
      mean_difference_mean = mean(.data[["mean_difference"]], na.rm = TRUE),
      max_abs_statistic = max(abs(.data[["statistic"]]), na.rm = TRUE),
      n_candidate_time_bins = sum(.data[["point_candidate"]], na.rm = TRUE),
      n_clustered_time_bins = sum(!is.na(.data[["cluster_id"]])),
      .groups = "drop"
    )

  permutation_summary <- permutation_distribution |>
    dplyr::summarise(
      n_permutations = dplyr::n(),
      min_max_cluster_statistic = min(
        .data[["max_cluster_statistic"]],
        na.rm = TRUE
      ),
      median_max_cluster_statistic = stats::median(
        .data[["max_cluster_statistic"]],
        na.rm = TRUE
      ),
      mean_max_cluster_statistic = mean(
        .data[["max_cluster_statistic"]],
        na.rm = TRUE
      ),
      p95_max_cluster_statistic = as.numeric(stats::quantile(
        .data[["max_cluster_statistic"]],
        0.95,
        na.rm = TRUE
      )),
      max_max_cluster_statistic = max(
        .data[["max_cluster_statistic"]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  settings_table <- tibble::tibble(
    parameter = names(settings),
    value = vapply(
      settings,
      function(x) paste(as.character(x), collapse = ", "),
      character(1)
    )
  )

  warning_table <- tibble::tibble(
    warning = if (!is.null(result$warning)) {
      as.character(result$warning)
    } else {
      paste(
        "Cluster-based permutation tests are for time-course inference;",
        "do not use them to select a confirmatory window and then retest that same window."
      )
    }
  )

  round_table <- function(tbl) {
    if (is.null(round_digits)) {
      return(tbl)
    }

    numeric_cols <- vapply(tbl, is.numeric, logical(1))

    tbl[numeric_cols] <- lapply(
      tbl[numeric_cols],
      round,
      digits = round_digits
    )

    tbl
  }

  overview <- round_table(overview)
  clusters <- round_table(clusters)
  significant_clusters <- round_table(significant_clusters)
  timecourse_summary <- round_table(timecourse_summary)
  permutation_summary <- round_table(permutation_summary)

  out <- list(
    overview = overview,
    clusters = clusters,
    significant_clusters = significant_clusters,
    timecourse_summary = timecourse_summary,
    permutation_summary = permutation_summary,
    settings = settings_table,
    warning = warning_table,
    model_status = as.character(result$model_status)
  )

  if (include_timecourse) {
    out$timecourse <- round_table(timecourse)
  }

  class(out) <- c("gp3_cluster_summary", class(out))

  out
}
