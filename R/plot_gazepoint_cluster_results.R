#' Plot cluster-based permutation results
#'
#' Create a publication-ready time-course plot from the output of
#' `run_gazepoint_cluster_permutation()`. The plot can show the mean condition
#' difference, the time-wise test statistic, or both. Candidate time bins and
#' cluster-level significant windows can be highlighted.
#'
#' Cluster-based permutation tests are intended for time-course inference.
#' They should not be used to discover a confirmatory time window and then test
#' that same window again in a second confirmatory model.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param plot_type Character. One of `"both"`, `"difference"`, or
#'   `"statistic"`.
#' @param alpha Cluster-level significance threshold used to decide which
#'   clusters are significant for plotting.
#' @param significant_only Logical. If `TRUE`, only significant clusters are
#'   shaded. If `FALSE`, all observed clusters are shaded.
#' @param show_clusters Logical. If `TRUE`, shade cluster windows.
#' @param show_candidates Logical. If `TRUE`, mark time bins exceeding the
#'   cluster-forming threshold.
#' @param show_threshold Logical. If `TRUE`, show the cluster-forming threshold
#'   on the statistic panel.
#' @param show_zero_line Logical. If `TRUE`, add a horizontal zero reference
#'   line.
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @param x_label X-axis label.
#' @param y_label Optional y-axis label. If `NULL`, a label is chosen
#'   automatically.
#' @param line_width Width of the time-course line.
#' @param point_size Size of candidate-bin points.
#' @param cluster_alpha Transparency for shaded cluster windows.
#'
#' @return A `ggplot` object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_cluster_results <- function(
    result,
    plot_type = c("both", "difference", "statistic"),
    alpha = 0.05,
    significant_only = TRUE,
    show_clusters = TRUE,
    show_candidates = TRUE,
    show_threshold = TRUE,
    show_zero_line = TRUE,
    title = NULL,
    subtitle = NULL,
    x_label = "Time (ms)",
    y_label = NULL,
    line_width = 0.7,
    point_size = 1.8,
    cluster_alpha = 0.12
) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  required_elements <- c(
    "timecourse",
    "clusters",
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

  plot_type <- match.arg(plot_type)

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  check_logical_scalar(significant_only, "significant_only")
  check_logical_scalar(show_clusters, "show_clusters")
  check_logical_scalar(show_candidates, "show_candidates")
  check_logical_scalar(show_threshold, "show_threshold")
  check_logical_scalar(show_zero_line, "show_zero_line")

  check_positive_numeric <- function(x, arg, allow_zero = FALSE) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        (!allow_zero && x <= 0) ||
        (allow_zero && x < 0)) {
      if (allow_zero) {
        stop(
          "`", arg, "` must be a non-negative finite numeric scalar.",
          call. = FALSE
        )
      } else {
        stop(
          "`", arg, "` must be a positive finite numeric scalar.",
          call. = FALSE
        )
      }
    }
  }

  check_positive_numeric(line_width, "line_width")
  check_positive_numeric(point_size, "point_size")
  check_positive_numeric(cluster_alpha, "cluster_alpha", allow_zero = TRUE)

  if (cluster_alpha > 1) {
    stop("`cluster_alpha` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.null(title) &&
      (!is.character(title) || length(title) != 1L || is.na(title))) {
    stop(
      "`title` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.null(subtitle) &&
      (!is.character(subtitle) || length(subtitle) != 1L || is.na(subtitle))) {
    stop(
      "`subtitle` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(x_label) ||
      length(x_label) != 1L ||
      is.na(x_label) ||
      !nzchar(x_label)) {
    stop("`x_label` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!is.null(y_label) &&
      (!is.character(y_label) || length(y_label) != 1L || is.na(y_label))) {
    stop(
      "`y_label` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  timecourse <- tibble::as_tibble(result$timecourse)
  clusters <- tibble::as_tibble(result$clusters)
  settings <- result$settings

  required_timecourse_cols <- c(
    ".gp3_cluster_time_bin",
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

  if (nrow(clusters) > 0L) {
    required_cluster_cols <- c(
      "cluster_id",
      "cluster_direction",
      "start_time_bin",
      "end_time_bin",
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

  cluster_threshold <- as.numeric(get_setting("cluster_threshold", NA_real_))
  tail <- as.character(get_setting("tail", "two_sided"))
  difference_label <- as.character(
    get_setting("difference", "condition difference")
  )

  time_bins <- sort(unique(timecourse$.gp3_cluster_time_bin))

  bin_step_ms <- if (length(time_bins) >= 2L) {
    stats::median(diff(time_bins), na.rm = TRUE)
  } else {
    NA_real_
  }

  if (is.null(title)) {
    title <- "Cluster-based permutation result"
  }

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Difference: ",
      difference_label,
      "; status: ",
      as.character(result$model_status)
    )
  }

  difference_data <- tibble::tibble(
    time_bin = timecourse$.gp3_cluster_time_bin,
    value = timecourse$mean_difference,
    metric = "Mean difference",
    point_candidate = timecourse$point_candidate,
    cluster_id = timecourse$cluster_id
  )

  statistic_data <- tibble::tibble(
    time_bin = timecourse$.gp3_cluster_time_bin,
    value = timecourse$statistic,
    metric = "Test statistic",
    point_candidate = timecourse$point_candidate,
    cluster_id = timecourse$cluster_id
  )

  plot_data <- if (plot_type == "difference") {
    difference_data
  } else if (plot_type == "statistic") {
    statistic_data
  } else {
    dplyr::bind_rows(difference_data, statistic_data)
  }

  plot_data$metric <- factor(
    plot_data$metric,
    levels = c("Mean difference", "Test statistic")
  )

  cluster_windows <- clusters

  if (nrow(cluster_windows) > 0L) {
    cluster_windows$significant_alpha <- cluster_windows$p_value < alpha


    if (significant_only) {
      cluster_windows <- cluster_windows[
        cluster_windows$significant_alpha,
        ,
        drop = FALSE
      ]
    }

    if (nrow(cluster_windows) > 0L) {
      if (is.finite(bin_step_ms)) {
        cluster_windows$xmax_plot <-
          cluster_windows$end_time_bin + bin_step_ms
      } else {
        cluster_windows$xmax_plot <- cluster_windows$end_time_bin
      }

      cluster_windows$xmin_plot <- cluster_windows$start_time_bin
    }


  }

  y_label_final <- y_label

  if (is.null(y_label_final)) {
    y_label_final <- if (plot_type == "difference") {
      "Mean condition difference"
    } else if (plot_type == "statistic") {
      "Test statistic"
    } else {
      "Value"
    }
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["time_bin"]],
      y = .data[["value"]]
    )
  )

  if (show_clusters && nrow(cluster_windows) > 0L) {
    p <- p +
      ggplot2::geom_rect(
        data = cluster_windows,
        ggplot2::aes(
          xmin = .data[["xmin_plot"]],
          xmax = .data[["xmax_plot"]],
          ymin = -Inf,
          ymax = Inf
        ),
        inherit.aes = FALSE,
        alpha = cluster_alpha
      )
  }

  if (show_zero_line) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        linewidth = 0.3
      )
  }

  p <- p +
    ggplot2::geom_line(linewidth = line_width)

  if (show_candidates) {
    candidate_data <- plot_data[
      plot_data$point_candidate %in% TRUE,
      ,
      drop = FALSE
    ]


    if (plot_type == "both") {
      candidate_data <- candidate_data[
        candidate_data$metric == "Test statistic",
        ,
        drop = FALSE
      ]
    }

    if (nrow(candidate_data) > 0L) {
      p <- p +
        ggplot2::geom_point(
          data = candidate_data,
          ggplot2::aes(
            x = .data[["time_bin"]],
            y = .data[["value"]]
          ),
          size = point_size
        )
    }


  }

  if (show_threshold &&
      is.finite(cluster_threshold) &&
      plot_type %in% c("both", "statistic")) {
    threshold_values <- if (identical(tail, "greater")) {
      cluster_threshold
    } else if (identical(tail, "less")) {
      -cluster_threshold
    } else {
      c(-cluster_threshold, cluster_threshold)
    }


    threshold_data <- tibble::tibble(
      metric = factor(
        "Test statistic",
        levels = c("Mean difference", "Test statistic")
      ),
      threshold = threshold_values
    )

    p <- p +
      ggplot2::geom_hline(
        data = threshold_data,
        ggplot2::aes(yintercept = .data[["threshold"]]),
        linetype = "dashed",
        inherit.aes = FALSE
      )


  }

  if (plot_type == "both") {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[["metric"]]),
        ncol = 1,
        scales = "free_y"
      )
  }

  p <- p +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label_final
    ) +
    ggplot2::theme_minimal()

  attr(p, "gp3_cluster_plot_settings") <- list(
    plot_type = plot_type,
    alpha = alpha,
    significant_only = significant_only,
    show_clusters = show_clusters,
    show_candidates = show_candidates,
    show_threshold = show_threshold,
    show_zero_line = show_zero_line
  )

  p
}
