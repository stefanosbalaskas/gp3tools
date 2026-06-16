#' Plot Gazepoint pupil preprocessing for one trial
#'
#' Create a visual audit plot for one selected subject, media item, trial, or
#' trial-global identifier. The plot can show raw pupil, cleaned pupil,
#' interpolated pupil, baseline-corrected pupil, smoothed pupil, and artifact
#' flags.
#'
#' @param data A Gazepoint pupil data frame.
#' @param subject Optional subject value to filter.
#' @param media_id Optional media identifier value to filter.
#' @param trial Optional trial value to filter.
#' @param trial_global Optional global trial identifier value to filter.
#' @param condition Optional condition value to filter.
#' @param subject_col Name of the subject column.
#' @param media_col Name of the media identifier column.
#' @param trial_col Name of the trial column.
#' @param trial_global_col Name of the global trial identifier column.
#' @param condition_col Name of the condition column.
#' @param time_col Name of the time column.
#' @param raw_pupil_col Optional raw pupil column.
#' @param clean_pupil_col Optional cleaned pupil column.
#' @param interpolated_pupil_col Optional interpolated pupil column.
#' @param baseline_pupil_col Optional baseline-corrected pupil column.
#' @param smoothed_pupil_col Optional smoothed pupil column.
#' @param artifact_col Optional artifact flag column. If `NULL`, the function
#'   tries `pupil_artifact_flag`, `pupil_flag_invalid`, and `artifact_flag`.
#' @param artifact_reason_col Optional artifact-reason column. If `NULL`, the
#'   function tries `pupil_artifact_reason`, `pupil_flag_reason`, and
#'   `artifact_reason`.
#' @param status_col Optional interpolation-status column.
#' @param plot_style Either `"faceted"` or `"overlaid"`.
#' @param bin_width_ms Width of time bins in milliseconds. This is used only
#'   for visual smoothing of dense sample-level traces.
#' @param max_event_marks Maximum number of artifact/interpolation rug marks
#'   to draw. Event marks are evenly thinned if there are more events.
#' @param point_size Size control for artifact/interpolation rug marks.
#' @param line_width Line width for pupil series.
#' @param alpha Line and marker transparency.
#'
#' @return A `ggplot2` plot object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_pupil_preprocessing <- function(
    data,
    subject = NULL,
    media_id = NULL,
    trial = NULL,
    trial_global = NULL,
    condition = NULL,
    subject_col = "subject",
    media_col = "MEDIA_ID",
    trial_col = "trial",
    trial_global_col = "trial_global",
    condition_col = "condition",
    time_col = "time",
    raw_pupil_col = "pupil",
    clean_pupil_col = "pupil_clean",
    interpolated_pupil_col = "pupil_interpolated",
    baseline_pupil_col = "pupil_baseline_corrected",
    smoothed_pupil_col = "pupil_smoothed",
    artifact_col = NULL,
    artifact_reason_col = NULL,
    status_col = "pupil_interpolation_status",
    plot_style = c("faceted", "overlaid"),
    bin_width_ms = 50,
    max_event_marks = 150,
    point_size = 0.8,
    line_width = 0.35,
    alpha = 0.95
) {
  plot_style <- match.arg(plot_style)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
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
    subject_col = subject_col,
    media_col = media_col,
    trial_col = trial_col,
    trial_global_col = trial_global_col,
    condition_col = condition_col,
    raw_pupil_col = raw_pupil_col,
    clean_pupil_col = clean_pupil_col,
    interpolated_pupil_col = interpolated_pupil_col,
    baseline_pupil_col = baseline_pupil_col,
    smoothed_pupil_col = smoothed_pupil_col,
    artifact_col = artifact_col,
    artifact_reason_col = artifact_reason_col,
    status_col = status_col
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

  if (!is.character(time_col) ||
      length(time_col) != 1L ||
      is.na(time_col) ||
      !nzchar(time_col)) {
    stop("`time_col` must be a non-missing character scalar.", call. = FALSE)
  }

  numeric_args <- c(
    bin_width_ms = bin_width_ms,
    max_event_marks = max_event_marks,
    point_size = point_size,
    line_width = line_width,
    alpha = alpha
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

  if (bin_width_ms <= 0) {
    stop("`bin_width_ms` must be greater than 0.", call. = FALSE)
  }

  if (max_event_marks < 1) {
    stop("`max_event_marks` must be at least 1.", call. = FALSE)
  }

  if (alpha < 0 || alpha > 1) {
    stop("`alpha` must be between 0 and 1.", call. = FALSE)
  }

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
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

  required_cols <- time_col

  add_selector_col <- function(cols, selector_value, selector_col) {
    if (!is.null(selector_value)) {
      cols <- unique(c(cols, selector_col))
    }

    cols
  }

  required_cols <- add_selector_col(required_cols, subject, subject_col)
  required_cols <- add_selector_col(required_cols, media_id, media_col)
  required_cols <- add_selector_col(required_cols, trial, trial_col)
  required_cols <- add_selector_col(
    required_cols,
    trial_global,
    trial_global_col
  )
  required_cols <- add_selector_col(required_cols, condition, condition_col)

  candidate_pupil_cols <- c(
    raw_pupil_col,
    clean_pupil_col,
    interpolated_pupil_col,
    baseline_pupil_col,
    smoothed_pupil_col
  )

  candidate_pupil_cols <- candidate_pupil_cols[
    !is.na(candidate_pupil_cols) &
      nzchar(candidate_pupil_cols)
  ]

  available_pupil_cols <- candidate_pupil_cols[
    candidate_pupil_cols %in% names(data)
  ]

  if (length(available_pupil_cols) == 0L) {
    stop(
      "No requested pupil columns were found in `data`.",
      call. = FALSE
    )
  }

  required_cols <- unique(c(required_cols, available_pupil_cols))

  if (!is.null(artifact_col) && artifact_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_col))
  }

  if (!is.null(artifact_reason_col) && artifact_reason_col %in% names(data)) {
    required_cols <- unique(c(required_cols, artifact_reason_col))
  }

  if (!is.null(status_col) && status_col %in% names(data)) {
    required_cols <- unique(c(required_cols, status_col))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
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

  value_matches <- function(x, value) {
    as.character(x) == as.character(value)
  }

  apply_value_filter <- function(x, col, value) {
    if (is.null(value)) {
      return(x)
    }

    x[value_matches(x[[col]], value), , drop = FALSE]
  }

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  thin_event_data <- function(x, max_rows) {
    if (nrow(x) <= max_rows) {
      return(x)
    }

    keep <- unique(round(seq(1, nrow(x), length.out = max_rows)))

    x[keep, , drop = FALSE]
  }

  filtered <- tibble::as_tibble(data)

  filtered <- apply_value_filter(filtered, subject_col, subject)
  filtered <- apply_value_filter(filtered, media_col, media_id)
  filtered <- apply_value_filter(filtered, trial_col, trial)
  filtered <- apply_value_filter(filtered, trial_global_col, trial_global)
  filtered <- apply_value_filter(filtered, condition_col, condition)

  if (nrow(filtered) == 0L) {
    stop(
      "No rows remain after applying the requested filters.",
      call. = FALSE
    )
  }

  filtered <- filtered |>
    dplyr::mutate(
      .gp3_pre_time = as_numeric_safe(.data[[time_col]])
    ) |>
    dplyr::filter(!is.na(.data[[".gp3_pre_time"]]))

  if (nrow(filtered) == 0L) {
    stop(
      "No non-missing time values remain after filtering.",
      call. = FALSE
    )
  }

  series_specs <- c(
    raw = raw_pupil_col,
    clean = clean_pupil_col,
    interpolated = interpolated_pupil_col,
    baseline_corrected = baseline_pupil_col,
    smoothed = smoothed_pupil_col
  )

  series_specs <- series_specs[
    !is.na(series_specs) &
      nzchar(series_specs) &
      series_specs %in% names(filtered)
  ]

  series_data <- lapply(
    names(series_specs),
    function(series_name) {
      col_name <- series_specs[[series_name]]

      tibble::tibble(
        .gp3_pre_time = filtered[[".gp3_pre_time"]],
        .gp3_pre_pupil = as_numeric_safe(filtered[[col_name]]),
        pupil_series = series_name,
        pupil_column = col_name
      )
    }
  )

  plot_data <- dplyr::bind_rows(series_data) |>
    dplyr::filter(!is.na(.data[[".gp3_pre_pupil"]])) |>
    dplyr::mutate(
      .gp3_pre_time_bin =
        floor(.data[[".gp3_pre_time"]] / bin_width_ms) * bin_width_ms +
        bin_width_ms / 2
    ) |>
    dplyr::group_by(
      .data$pupil_series,
      .data$pupil_column,
      .data[[".gp3_pre_time_bin"]]
    ) |>
    dplyr::summarise(
      .gp3_pre_pupil = mean_or_na(.data[[".gp3_pre_pupil"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      .gp3_pre_time = .data[[".gp3_pre_time_bin"]]
    ) |>
    dplyr::select(
      -dplyr::all_of(".gp3_pre_time_bin")
    ) |>
    dplyr::filter(!is.na(.data[[".gp3_pre_pupil"]]))

  if (nrow(plot_data) == 0L) {
    stop(
      "No non-missing pupil values remain in the selected data.",
      call. = FALSE
    )
  }

  if (!is.null(artifact_col) && artifact_col %in% names(filtered)) {
    filtered <- filtered |>
      dplyr::mutate(
        .gp3_pre_artifact = as_logical_flag(.data[[artifact_col]])
      )
  } else if (!is.null(artifact_reason_col) &&
             artifact_reason_col %in% names(filtered)) {
    filtered <- filtered |>
      dplyr::mutate(
        .gp3_pre_artifact =
          !is.na(.data[[artifact_reason_col]]) &
          as.character(.data[[artifact_reason_col]]) != "valid" &
          as.character(.data[[artifact_reason_col]]) != ""
      )
  } else {
    filtered <- filtered |>
      dplyr::mutate(
        .gp3_pre_artifact = FALSE
      )
  }

  artifact_times <- filtered |>
    dplyr::filter(.data[[".gp3_pre_artifact"]]) |>
    dplyr::distinct(.data[[".gp3_pre_time"]]) |>
    dplyr::arrange(.data[[".gp3_pre_time"]])

  artifact_times <- thin_event_data(artifact_times, max_event_marks)

  title_parts <- c()

  if (!is.null(subject)) {
    title_parts <- c(title_parts, paste0(subject_col, "=", subject))
  }

  if (!is.null(media_id)) {
    title_parts <- c(title_parts, paste0(media_col, "=", media_id))
  }

  if (!is.null(trial)) {
    title_parts <- c(title_parts, paste0(trial_col, "=", trial))
  }

  if (!is.null(trial_global)) {
    title_parts <- c(
      title_parts,
      paste0(trial_global_col, "=", trial_global)
    )
  }

  if (!is.null(condition)) {
    title_parts <- c(title_parts, paste0(condition_col, "=", condition))
  }

  subtitle_text <- if (length(title_parts) == 0L) {
    paste0(nrow(filtered), " samples selected")
  } else {
    paste(title_parts, collapse = " | ")
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[".gp3_pre_time"]],
      y = .data[[".gp3_pre_pupil"]],
      colour = .data$pupil_series
    )
  ) +
    ggplot2::geom_line(
      linewidth = line_width,
      alpha = alpha,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Gazepoint pupil preprocessing audit",
      subtitle = subtitle_text,
      x = time_col,
      y = "Pupil value",
      colour = "Pupil series"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )

  if (nrow(artifact_times) > 0L) {
    p <- p +
      ggplot2::geom_rug(
        data = artifact_times,
        ggplot2::aes(
          x = .data[[".gp3_pre_time"]]
        ),
        inherit.aes = FALSE,
        sides = "b",
        alpha = alpha,
        linewidth = point_size / 3
      )
  }

  if (!is.null(status_col) && status_col %in% names(filtered)) {
    interpolation_times <- filtered |>
      dplyr::filter(as.character(.data[[status_col]]) == "interpolated") |>
      dplyr::distinct(.data[[".gp3_pre_time"]]) |>
      dplyr::arrange(.data[[".gp3_pre_time"]])

    interpolation_times <- thin_event_data(
      interpolation_times,
      max_event_marks
    )

    if (nrow(interpolation_times) > 0L) {
      p <- p +
        ggplot2::geom_rug(
          data = interpolation_times,
          ggplot2::aes(
            x = .data[[".gp3_pre_time"]]
          ),
          inherit.aes = FALSE,
          sides = "t",
          alpha = alpha / 1.5,
          linewidth = point_size / 4
        )
    }
  }

  if (plot_style == "faceted") {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula("~ pupil_series"),
        scales = "free_y",
        ncol = 1
      )
  }

  p
}
