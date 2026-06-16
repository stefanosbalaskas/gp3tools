#' Plot Gazepoint tracking-quality diagnostics
#'
#' Creates a readable diagnostic plot of selected gaze and pupil validity
#' percentages by participant/file and media stimulus.
#'
#' @param data A tracking-quality or flagged-quality table, usually from
#' `summarise_tracking_quality()` or `flag_tracking_quality()`.
#' @param metric_cols Validity percentage columns to plot. If `NULL`, the default
#' is `FPOGV_valid_pct` and `RPV_valid_pct`, which provide a compact diagnostic
#' view of gaze and right-pupil validity.
#' @param user_col Column identifying the source/user file.
#' @param media_col Column identifying the media/stimulus.
#' @param review_col Optional column indicating whether a row requires review.
#' @param min_valid_pct Vertical threshold line for acceptable validity.
#'
#' @return A ggplot object.
#' @export
plot_tracking_quality <- function(
    data,
    metric_cols = NULL,
    user_col = "USER_FILE",
    media_col = "MEDIA_ID",
    review_col = "review_required",
    min_valid_pct = 70
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or tibble.")
  }

  required_cols <- c(user_col, media_col)
  missing_required <- setdiff(required_cols, names(data))

  if (length(missing_required) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_required, collapse = ", ")
      )
    )
  }

  if (is.null(metric_cols)) {
    metric_cols <- intersect(
      c(
        "FPOGV_valid_pct",
        "RPV_valid_pct"
      ),
      names(data)
    )
  }

  if (length(metric_cols) == 0) {
    rlang::abort("No validity-percentage columns were found to plot.")
  }

  missing_metric_cols <- setdiff(metric_cols, names(data))

  if (length(missing_metric_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing metric columns: ",
        paste(missing_metric_cols, collapse = ", ")
      )
    )
  }

  if (!review_col %in% names(data)) {
    data[[review_col]] <- FALSE
  }

  long_data <- tidyr::pivot_longer(
    tibble::as_tibble(data),
    cols = dplyr::all_of(metric_cols),
    names_to = "metric",
    values_to = "valid_pct"
  )

  short_user <- gsub(
    "_all_gaze\\.csv$",
    "",
    long_data[[user_col]]
  )

  short_user <- gsub(
    "_fixations\\.csv$",
    "",
    short_user
  )

  long_data$recording <- paste0(
    short_user,
    " | Media ",
    long_data[[media_col]]
  )

  long_data$recording <- factor(
    long_data$recording,
    levels = rev(unique(long_data$recording))
  )

  long_data$review_required_plot <- as.logical(long_data[[review_col]])

  ggplot2::ggplot(
    long_data,
    ggplot2::aes(
      x = .data$valid_pct,
      y = .data$recording,
      shape = .data$review_required_plot
    )
  ) +
    ggplot2::geom_vline(
      xintercept = min_valid_pct,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$metric),
      ncol = 1
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 25)
    ) +
    ggplot2::labs(
      title = "Gazepoint tracking-quality diagnostics",
      x = "Validity (%)",
      y = "Recording",
      shape = "Review required"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot Gazepoint sampling-rate diagnostics
#'
#' Creates a diagnostic plot of estimated sampling rate by participant/file and
#' media stimulus.
#'
#' @param sampling Sampling-rate table, usually from `check_sampling_rate()`.
#' @param user_col Column identifying the source/user file.
#' @param media_col Column identifying the media/stimulus.
#' @param expected_hz Expected sampling rate.
#' @param hz_tolerance Allowed deviation from the expected sampling rate.
#'
#' @return A ggplot object.
#' @export
plot_sampling_rate <- function(
    sampling,
    user_col = "USER_FILE",
    media_col = "MEDIA_ID",
    expected_hz = 60,
    hz_tolerance = 5
) {
  if (!is.data.frame(sampling)) {
    rlang::abort("`sampling` must be a data frame or tibble.")
  }

  required_cols <- c(user_col, media_col, "estimated_hz")
  missing_required <- setdiff(required_cols, names(sampling))

  if (length(missing_required) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns: ",
        paste(missing_required, collapse = ", ")
      )
    )
  }

  plot_data <- tibble::as_tibble(sampling)

  short_user <- gsub(
    "_all_gaze\\.csv$",
    "",
    plot_data[[user_col]]
  )

  short_user <- gsub(
    "_fixations\\.csv$",
    "",
    short_user
  )

  plot_data$recording <- paste0(
    short_user,
    " | Media ",
    plot_data[[media_col]]
  )

  plot_data$recording <- factor(
    plot_data$recording,
    levels = rev(unique(plot_data$recording))
  )

  plot_data$sampling_rate_flag <- abs(
    plot_data$estimated_hz - expected_hz
  ) > hz_tolerance

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$estimated_hz,
      y = .data$recording,
      shape = .data$sampling_rate_flag
    )
  ) +
    ggplot2::geom_vline(
      xintercept = expected_hz,
      linetype = "solid"
    ) +
    ggplot2::geom_vline(
      xintercept = expected_hz - hz_tolerance,
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      xintercept = expected_hz + hz_tolerance,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "Gazepoint sampling-rate diagnostics",
      x = "Estimated sampling rate (Hz)",
      y = "Recording",
      shape = "Outside tolerance"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}
