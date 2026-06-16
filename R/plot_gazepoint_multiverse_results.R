#' Plot Gazepoint preprocessing multiverse results
#'
#' Create diagnostic plots from preprocessing multiverse summaries or from
#' pupil/AOI multiverse result objects.
#'
#' @param x A `gp3_multiverse_summary_results`,
#'   `gp3_pupil_multiverse_results`, or `gp3_aoi_multiverse_results` object.
#' @param plot Character. Plot type. One of `"status"`, `"rows"`,
#'   `"pupil_parameters"`, or `"aoi_denominators"`.
#' @param family Character. Which family to show. One of `"all"`, `"pupil"`,
#'   or `"aoi"`.
#' @param title Optional plot title.
#' @param show_labels Logical. If `TRUE`, show branch labels on the y-axis.
#'
#' @return A `ggplot` object.
#' @export
plot_gazepoint_multiverse_results <- function(
    x,
    plot = c("status", "rows", "pupil_parameters", "aoi_denominators"),
    family = c("all", "pupil", "aoi"),
    title = NULL,
    show_labels = TRUE
) {
  plot <- match.arg(plot)
  family <- match.arg(family)

  .gp3_multiverse_check_logical_scalar(show_labels, "show_labels")

  summary <- .gp3_multiverse_as_summary(x)
  branch_summary <- summary$branch_summary

  if (!is.data.frame(branch_summary) || nrow(branch_summary) == 0L) {
    stop("`x` does not contain branch-level multiverse results.", call. = FALSE)
  }

  if (!"multiverse_family" %in% names(branch_summary)) {
    stop("`x` does not contain a `multiverse_family` column.", call. = FALSE)
  }

  if (family != "all") {
    branch_summary <- branch_summary[
      branch_summary$multiverse_family == family,
      ,
      drop = FALSE
    ]
  }

  if (nrow(branch_summary) == 0L) {
    stop("No branches are available for the requested `family`.", call. = FALSE)
  }

  if (plot == "status") {
    return(.gp3_plot_multiverse_status(
      branch_summary,
      title = title,
      show_labels = show_labels
    ))
  }

  if (plot == "rows") {
    return(.gp3_plot_multiverse_rows(
      branch_summary,
      title = title,
      show_labels = show_labels
    ))
  }

  if (plot == "pupil_parameters") {
    return(.gp3_plot_multiverse_pupil_parameters(
      branch_summary,
      title = title
    ))
  }

  if (plot == "aoi_denominators") {
    return(.gp3_plot_multiverse_aoi_denominators(
      branch_summary,
      title = title
    ))
  }

  stop("Unsupported plot type.", call. = FALSE)
}

.gp3_multiverse_as_summary <- function(x) {
  if (inherits(x, "gp3_multiverse_summary_results")) {
    return(x)
  }

  if (inherits(x, "gp3_pupil_multiverse_results") ||
      inherits(x, "gp3_aoi_multiverse_results")) {
    return(summarise_gazepoint_multiverse_results(x))
  }

  stop(
    "`x` must be a multiverse summary, pupil multiverse result, or AOI multiverse result object.",
    call. = FALSE
  )
}

.gp3_plot_multiverse_status <- function(branch_summary, title, show_labels) {
  plot_data <- branch_summary

  plot_data$plot_value <- 1L
  plot_data$plot_label <- .gp3_multiverse_plot_branch_labels(
    plot_data,
    show_labels = show_labels
  )

  plot_data$plot_label <- factor(
    plot_data$plot_label,
    levels = rev(unique(plot_data$plot_label))
  )

  if (is.null(title)) {
    title <- "Preprocessing multiverse branch status"
  }

  p <- ggplot2::ggplot(
    plot_data,
    .gp3_multiverse_aes(
      x = "plot_label",
      y = "plot_value",
      fill = "branch_status"
    )
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      stats::as.formula("~ multiverse_family"),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = title,
      x = "Branch",
      y = "Status indicator",
      fill = "Branch status"
    ) +
    ggplot2::theme_minimal()

  p
}

.gp3_plot_multiverse_rows <- function(branch_summary, title, show_labels) {
  plot_data <- branch_summary

  plot_data$plot_value <- .gp3_multiverse_result_rows(plot_data)

  plot_data <- plot_data[!is.na(plot_data$plot_value), , drop = FALSE]

  if (nrow(plot_data) == 0L) {
    stop(
      "No row-count columns are available for the requested multiverse results.",
      call. = FALSE
    )
  }

  plot_data$plot_label <- .gp3_multiverse_plot_branch_labels(
    plot_data,
    show_labels = show_labels
  )

  plot_data$plot_label <- factor(
    plot_data$plot_label,
    levels = rev(unique(plot_data$plot_label))
  )

  if (is.null(title)) {
    title <- "Rows retained across preprocessing multiverse branches"
  }

  p <- ggplot2::ggplot(
    plot_data,
    .gp3_multiverse_aes(
      x = "plot_label",
      y = "plot_value",
      fill = "branch_status"
    )
  ) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      stats::as.formula("~ multiverse_family"),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = title,
      x = "Branch",
      y = "Rows",
      fill = "Branch status"
    ) +
    ggplot2::theme_minimal()

  p
}

.gp3_plot_multiverse_pupil_parameters <- function(branch_summary, title) {
  required <- c(
    "multiverse_family",
    "max_gap_ms",
    "smoothing_window_samples",
    "branch_status"
  )

  missing_cols <- setdiff(required, names(branch_summary))

  if (length(missing_cols) > 0L) {
    stop(
      "Pupil-parameter plots require columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  plot_data <- branch_summary[
    branch_summary$multiverse_family == "pupil",
    ,
    drop = FALSE
  ]

  if (nrow(plot_data) == 0L) {
    stop("No pupil branches are available for plotting.", call. = FALSE)
  }

  plot_data$max_gap_ms <- as.factor(plot_data$max_gap_ms)
  plot_data$smoothing_window_samples <- as.factor(
    plot_data$smoothing_window_samples
  )

  if (is.null(title)) {
    title <- "Pupil preprocessing multiverse settings"
  }

  p <- ggplot2::ggplot(
    plot_data,
    .gp3_multiverse_aes(
      x = "max_gap_ms",
      fill = "branch_status"
    )
  ) +
    ggplot2::geom_bar(width = 0.75) +
    ggplot2::facet_wrap(
      stats::as.formula("~ smoothing_window_samples"),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = title,
      x = "Maximum interpolation gap (ms)",
      y = "Branches",
      fill = "Branch status"
    ) +
    ggplot2::theme_minimal()

  p
}

.gp3_plot_multiverse_aoi_denominators <- function(branch_summary, title) {
  required <- c(
    "multiverse_family",
    "denominator",
    "min_denominator_samples",
    "branch_status"
  )

  missing_cols <- setdiff(required, names(branch_summary))

  if (length(missing_cols) > 0L) {
    stop(
      "AOI-denominator plots require columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  plot_data <- branch_summary[
    branch_summary$multiverse_family == "aoi",
    ,
    drop = FALSE
  ]

  if (nrow(plot_data) == 0L) {
    stop("No AOI branches are available for plotting.", call. = FALSE)
  }

  plot_data$min_denominator_samples <- as.factor(
    plot_data$min_denominator_samples
  )

  if (is.null(title)) {
    title <- "AOI preprocessing multiverse denominator settings"
  }

  p <- ggplot2::ggplot(
    plot_data,
    .gp3_multiverse_aes(
      x = "denominator",
      fill = "branch_status"
    )
  ) +
    ggplot2::geom_bar(width = 0.75) +
    ggplot2::facet_wrap(
      stats::as.formula("~ min_denominator_samples"),
      scales = "free_y"
    ) +
    ggplot2::labs(
      title = title,
      x = "AOI denominator",
      y = "Branches",
      fill = "Branch status"
    ) +
    ggplot2::theme_minimal()

  p
}

.gp3_multiverse_result_rows <- function(x) {
  if ("output_rows" %in% names(x)) {
    rows <- x$output_rows
  } else {
    rows <- rep(NA_integer_, nrow(x))
  }

  if ("aoi_glmm_rows" %in% names(x)) {
    use_aoi <- x$multiverse_family == "aoi" & !is.na(x$aoi_glmm_rows)
    rows[use_aoi] <- x$aoi_glmm_rows[use_aoi]
  }

  if ("aoi_window_rows" %in% names(x)) {
    use_aoi_windows <- x$multiverse_family == "aoi" &
      is.na(rows) &
      !is.na(x$aoi_window_rows)
    rows[use_aoi_windows] <- x$aoi_window_rows[use_aoi_windows]
  }

  rows
}

.gp3_multiverse_plot_branch_labels <- function(x, show_labels = TRUE) {
  if (isTRUE(show_labels) && "branch_label" %in% names(x)) {
    return(x$branch_label)
  }

  x$branch_id
}

.gp3_multiverse_aes <- function(x, y = NULL, fill = NULL) {
  mapping <- list(x = as.name(x))

  if (!is.null(y)) {
    mapping$y <- as.name(y)
  }

  if (!is.null(fill)) {
    mapping$fill <- as.name(fill)
  }

  do.call(ggplot2::aes, mapping)
}
