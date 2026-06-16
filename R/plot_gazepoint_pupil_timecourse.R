#' Plot Gazepoint pupil time course
#'
#' Plot a binned pupil time course with a mean line and confidence band.
#' The function can plot one overall time course or condition-wise time
#' courses, with optional faceting by variables such as condition, media,
#' AOI, subject, or trial.
#'
#' @param data A Gazepoint pupil data frame.
#' @param pupil_col Name of the pupil column to plot. If `NULL`, the function
#'   tries `pupil_smoothed`, `pupil_baseline_corrected`,
#'   `pupil_baseline_percent_change`, `pupil_interpolated`, `pupil_clean`, and
#'   `pupil`.
#' @param time_col Name of the time column.
#' @param condition_col Optional condition column used for separate lines.
#'   If the column is missing or contains only missing values, the function
#'   plots a single `"all_data"` time course.
#' @param facet_cols Optional character vector of columns used for faceting.
#' @param bin_width_ms Width of time bins in milliseconds.
#' @param ci_level Confidence level for the band.
#' @param min_samples Minimum number of valid pupil samples required per
#'   time bin.
#' @param band_alpha Transparency of the confidence band.
#' @param line_width Width of the mean time-course line.
#'
#' @return A `ggplot2` plot object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_pupil_timecourse <- function(
    data,
    pupil_col = NULL,
    time_col = "time",
    condition_col = "condition",
    facet_cols = NULL,
    bin_width_ms = 100,
    ci_level = 0.95,
    min_samples = 1,
    band_alpha = 0.2,
    line_width = 0.8
) {
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
    pupil_col = pupil_col,
    condition_col = condition_col
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

  numeric_args <- c(
    bin_width_ms = bin_width_ms,
    ci_level = ci_level,
    min_samples = min_samples,
    band_alpha = band_alpha,
    line_width = line_width
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

  if (ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be greater than 0 and less than 1.", call. = FALSE)
  }

  if (min_samples < 1) {
    stop("`min_samples` must be at least 1.", call. = FALSE)
  }

  if (band_alpha < 0 || band_alpha > 1) {
    stop("`band_alpha` must be between 0 and 1.", call. = FALSE)
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
        "pupil_baseline_percent_change",
        "pupil_interpolated",
        "pupil_clean",
        "pupil_for_preprocessing",
        "pupil"
      )
    )
  }

  if (is.null(pupil_col)) {
    stop(
      "Could not automatically detect a pupil column. Please provide `pupil_col`.",
      call. = FALSE
    )
  }

  required_cols <- unique(c(time_col, pupil_col, facet_cols))

  if (!is.null(condition_col) && condition_col %in% names(data)) {
    required_cols <- unique(c(required_cols, condition_col))
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

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  sd_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  clean_group_values <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_timecourse_time = as_numeric_safe(.data[[time_col]]),
      .gp3_timecourse_pupil = as_numeric_safe(.data[[pupil_col]])
    )

  if (!is.null(condition_col) && condition_col %in% names(working)) {
    condition_values <- clean_group_values(working[[condition_col]])

    if (all(is.na(condition_values))) {
      condition_values <- rep("all_data", nrow(working))
    } else {
      condition_values[is.na(condition_values)] <- "missing_condition"
    }

    working$.gp3_timecourse_condition <- condition_values
  } else {
    working$.gp3_timecourse_condition <- rep("all_data", nrow(working))
  }

  working <- working |>
    dplyr::mutate(
      .gp3_timecourse_bin =
        floor(.data[[".gp3_timecourse_time"]] / bin_width_ms) * bin_width_ms +
        bin_width_ms / 2
    )

  summary_group_cols <- unique(
    c(
      facet_cols,
      ".gp3_timecourse_condition",
      ".gp3_timecourse_bin"
    )
  )

  summary_data <- working |>
    dplyr::filter(
      !is.na(.data[[".gp3_timecourse_time"]]),
      !is.na(.data[[".gp3_timecourse_bin"]])
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(summary_group_cols))
    ) |>
    dplyr::summarise(
      n_samples = sum(!is.na(.data[[".gp3_timecourse_pupil"]])),
      mean_pupil = mean_or_na(.data[[".gp3_timecourse_pupil"]]),
      sd_pupil = sd_or_na(.data[[".gp3_timecourse_pupil"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      se_pupil = dplyr::if_else(
        .data$n_samples > 1L & !is.na(.data$sd_pupil),
        .data$sd_pupil / sqrt(.data$n_samples),
        NA_real_
      ),
      ci_half_width = dplyr::if_else(
        .data$n_samples > 1L & !is.na(.data$se_pupil),
        stats::qnorm(1 - ((1 - ci_level) / 2)) * .data$se_pupil,
        NA_real_
      ),
      ci_lower = .data$mean_pupil - .data$ci_half_width,
      ci_upper = .data$mean_pupil + .data$ci_half_width
    ) |>
    dplyr::filter(
      .data$n_samples >= min_samples,
      !is.na(.data$mean_pupil)
    )

  if (nrow(summary_data) == 0L) {
    stop(
      "No valid pupil/time samples available to plot after filtering.",
      call. = FALSE
    )
  }

  p <- ggplot2::ggplot(
    summary_data,
    ggplot2::aes(
      x = .data[[".gp3_timecourse_bin"]]
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$ci_lower,
        ymax = .data$ci_upper,
        fill = .data[[".gp3_timecourse_condition"]],
        group = .data[[".gp3_timecourse_condition"]]
      ),
      alpha = band_alpha,
      colour = NA,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(
        y = .data$mean_pupil,
        colour = .data[[".gp3_timecourse_condition"]],
        group = .data[[".gp3_timecourse_condition"]]
      ),
      linewidth = line_width,
      na.rm = TRUE
    ) +
    ggplot2::labs(
      title = "Gazepoint pupil time course",
      x = paste0(time_col, " bin centre (ms)"),
      y = pupil_col,
      colour = if (is.null(condition_col)) "Group" else condition_col,
      fill = if (is.null(condition_col)) "Group" else condition_col
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

  p
}
