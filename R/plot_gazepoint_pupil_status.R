#' Plot Gazepoint pupil preprocessing status
#'
#' Visualise observed, interpolated, missing, artifact, and other pupil-sample
#' statuses over time or as grouped percentages.
#'
#' @param data A Gazepoint pupil data frame.
#' @param time_col Name of the time column.
#' @param pupil_col Optional pupil column used to detect remaining missing
#'   samples. If `NULL`, the function tries `pupil_smoothed`,
#'   `pupil_baseline_corrected`, `pupil_interpolated`, `pupil_clean`, and
#'   `pupil`.
#' @param status_col Optional interpolation-status column.
#' @param interpolated_col Optional logical interpolation flag column.
#' @param artifact_col Optional artifact flag column. If `NULL`, the function
#'   tries `pupil_artifact_flag`, `pupil_flag_invalid`, and `artifact_flag`.
#' @param artifact_reason_col Optional artifact-reason column. If `NULL`, the
#'   function tries `pupil_artifact_reason`, `pupil_flag_reason`, and
#'   `artifact_reason`.
#' @param group_cols Character vector used to define timeline rows or summary
#'   groups.
#' @param facet_cols Optional character vector of columns used for faceting.
#' @param plot_type Either `"timeline"` or `"summary"`.
#' @param point_size Point size for timeline plots.
#' @param alpha Point/column transparency.
#' @param max_points Maximum number of rows to plot in timeline mode. If the
#'   input has more rows, rows are evenly thinned for plotting only.
#'
#' @return A `ggplot2` plot object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_pupil_status <- function(
    data,
    time_col = "time",
    pupil_col = NULL,
    status_col = "pupil_interpolation_status",
    interpolated_col = "pupil_was_interpolated",
    artifact_col = NULL,
    artifact_reason_col = NULL,
    group_cols = c("subject", "trial_global"),
    facet_cols = NULL,
    plot_type = c("timeline", "summary"),
    point_size = 0.7,
    alpha = 0.8,
    max_points = 50000
) {
  plot_type <- match.arg(plot_type)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(group_cols) ||
      any(is.na(group_cols)) ||
      any(!nzchar(group_cols)) ||
      anyDuplicated(group_cols)) {
    stop(
      "`group_cols` must be a character vector of unique column names.",
      call. = FALSE
    )
  }

  valid_optional_column <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      )
  }

  optional_column_args <- list(
    pupil_col = pupil_col,
    status_col = status_col,
    interpolated_col = interpolated_col,
    artifact_col = artifact_col,
    artifact_reason_col = artifact_reason_col
  )

  valid_optional_args <- vapply(
    optional_column_args,
    valid_optional_column,
    logical(1)
  )

  if (any(!valid_optional_args)) {
    stop(
      "Optional column-name arguments must be NULL or non-missing character scalars: ",
      paste(names(valid_optional_args)[!valid_optional_args], collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(facet_cols) &&
      (
        !is.character(facet_cols) ||
        any(is.na(facet_cols)) ||
        any(!nzchar(facet_cols)) ||
        anyDuplicated(facet_cols)
      )) {
    stop(
      "`facet_cols` must be NULL or a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.character(time_col) ||
      length(time_col) != 1L ||
      is.na(time_col) ||
      !nzchar(time_col)) {
    stop("`time_col` must be a non-missing character scalar.", call. = FALSE)
  }

  numeric_args <- c(
    point_size = point_size,
    alpha = alpha,
    max_points = max_points
  )

  valid_numeric_arg <- vapply(
    numeric_args,
    function(x) {
      is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x)
    },
    logical(1)
  )

  if (any(!valid_numeric_arg)) {
    stop(
      "Plot-control arguments must be finite numeric scalars: ",
      paste(names(numeric_args)[!valid_numeric_arg], collapse = ", "),
      call. = FALSE
    )
  }

  if (max_points < 1) {
    stop("`max_points` must be at least 1.", call. = FALSE)
  }

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
  }

  if (is.null(pupil_col)) {
    pupil_col <- auto_detect_col(
      c(
        "pupil_smoothed",
        "pupil_baseline_corrected",
        "pupil_interpolated",
        "pupil_clean",
        "pupil_for_preprocessing",
        "pupil"
      )
    )
  }

  if (is.null(artifact_col)) {
    artifact_col <- auto_detect_col(
      c("pupil_artifact_flag", "pupil_flag_invalid", "artifact_flag")
    )
  }

  if (is.null(artifact_reason_col)) {
    artifact_reason_col <- auto_detect_col(
      c("pupil_artifact_reason", "pupil_flag_reason", "artifact_reason")
    )
  }

  required_cols <- unique(c(time_col, group_cols, facet_cols))

  if (!is.null(pupil_col)) {
    required_cols <- unique(c(required_cols, pupil_col))
  }

  if (!is.null(status_col) && status_col %in% names(data)) {
    required_cols <- unique(c(required_cols, status_col))
  }

  if (!is.null(interpolated_col) && interpolated_col %in% names(data)) {
    required_cols <- unique(c(required_cols, interpolated_col))
  }

  if (!is.null(artifact_col) && artifact_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_col))
  }

  if (!is.null(artifact_reason_col) && artifact_reason_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_reason_col))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(pupil_col) &&
      (is.null(status_col) || !status_col %in% names(data)) &&
      is.null(artifact_col) &&
      is.null(artifact_reason_col)) {
    stop(
      "Could not detect a pupil, status, or artifact column to plot.",
      call. = FALSE
    )
  }

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  as_logical_flag <- function(x) {
    if (is.logical(x)) {
      return(dplyr::coalesce(x, FALSE))
    }

    if (is.numeric(x) || is.integer(x)) {
      return(!is.na(x) & x != 0)
    }

    x_chr <- tolower(trimws(as.character(x)))

    x_chr %in% c("true", "t", "1", "yes", "y")
  }

  make_group_label <- function(x, cols) {
    if (length(cols) == 0L) {
      return(rep("all", nrow(x)))
    }

    label_data <- x[, cols, drop = FALSE]

    apply(
      label_data,
      1,
      function(row) {
        paste(row, collapse = " | ")
      }
    )
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_status_time = as_numeric_safe(.data[[time_col]])
    )

  working$.gp3_status_group <- make_group_label(working, group_cols)

  if (!is.null(pupil_col) && pupil_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_status_pupil_missing = is.na(as_numeric_safe(.data[[pupil_col]]))
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_status_pupil_missing = FALSE
      )
  }

  if (!is.null(status_col) && status_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_status_raw = as.character(.data[[status_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_status_raw = NA_character_
      )
  }

  if (!is.null(interpolated_col) && interpolated_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_status_interpolated =
          as_logical_flag(.data[[interpolated_col]])
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_status_interpolated = FALSE
      )
  }

  if (!is.null(artifact_col) && artifact_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_status_artifact =
          as_logical_flag(.data[[artifact_col]])
      )
  } else if (!is.null(artifact_reason_col) &&
             artifact_reason_col %in% names(working)) {
    working <- working |>
      dplyr::mutate(
        .gp3_status_artifact =
          !is.na(.data[[artifact_reason_col]]) &
          as.character(.data[[artifact_reason_col]]) != "valid" &
          as.character(.data[[artifact_reason_col]]) != ""
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_status_artifact = FALSE
      )
  }

  working <- working |>
    dplyr::mutate(
      pupil_sample_status = dplyr::case_when(
        .data[[".gp3_status_artifact"]] ~ "artifact",
        .data[[".gp3_status_interpolated"]] |
          .data[[".gp3_status_raw"]] == "interpolated" ~ "interpolated",
        .data[[".gp3_status_pupil_missing"]] |
          grepl(
            "^missing|missing_|_missing|long_gap|edge_gap|unfilled",
            .data[[".gp3_status_raw"]],
            ignore.case = TRUE
          ) ~ "missing",
        .data[[".gp3_status_raw"]] == "observed" |
          !.data[[".gp3_status_pupil_missing"]] ~ "observed",
        TRUE ~ "other"
      ),
      pupil_sample_status = factor(
        .data$pupil_sample_status,
        levels = c(
          "observed",
          "interpolated",
          "missing",
          "artifact",
          "other"
        )
      )
    )

  if (plot_type == "timeline") {
    plot_data <- working |>
      dplyr::filter(!is.na(.data[[".gp3_status_time"]]))

    if (nrow(plot_data) > max_points) {
      keep <- unique(round(seq(1, nrow(plot_data), length.out = max_points)))
      plot_data <- plot_data[keep, , drop = FALSE]
    }

    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[".gp3_status_time"]],
        y = .data[[".gp3_status_group"]],
        colour = .data$pupil_sample_status
      )
    ) +
      ggplot2::geom_point(
        size = point_size,
        alpha = alpha,
        na.rm = TRUE
      ) +
      ggplot2::labs(
        title = "Gazepoint pupil preprocessing status over time",
        x = time_col,
        y = paste(group_cols, collapse = " | "),
        colour = "Status"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank()
      )

    if (!is.null(facet_cols) && length(facet_cols) > 0L) {
      p <- p +
        ggplot2::facet_wrap(
          stats::as.formula(
            paste("~", paste(facet_cols, collapse = " + "))
          ),
          scales = "free_y"
        )
    }

    return(p)
  }

  summary_group_cols <- unique(c(facet_cols, group_cols))

  summary_data <- working |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(summary_group_cols)),
      .data$pupil_sample_status
    ) |>
    dplyr::summarise(
      n_samples = dplyr::n(),
      .groups = "drop_last"
    ) |>
    dplyr::mutate(
      total_samples = sum(.data$n_samples, na.rm = TRUE),
      sample_pct = dplyr::if_else(
        .data$total_samples > 0L,
        100 * .data$n_samples / .data$total_samples,
        NA_real_
      )
    ) |>
    dplyr::ungroup()

  summary_data$.gp3_status_group <- make_group_label(summary_data, group_cols)

  p <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(
      x = .data[[".gp3_status_group"]],
      y = .data$sample_pct,
      fill = .data$pupil_sample_status
    )
  ) +
    ggplot2::geom_col(alpha = alpha) +
    ggplot2::labs(
      title = "Gazepoint pupil preprocessing status summary",
      x = paste(group_cols, collapse = " | "),
      y = "Samples (%)",
      fill = "Status"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (!is.null(facet_cols) && length(facet_cols) > 0L) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula(
          paste("~", paste(facet_cols, collapse = " + "))
        ),
        scales = "free_x"
      )
  }

  p
}
