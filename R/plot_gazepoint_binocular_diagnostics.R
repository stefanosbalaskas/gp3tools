#' Plot binocular pupil diagnostics, reconstruction, and sensitivity
#'
#' Provides a compact plotting interface for binocular traces, agreement,
#' Bland-Altman-style differences, artificial-missingness validation, validation
#' residuals, provenance timelines, reconstruction burden, sensitivity summaries,
#' and reconstructed gap durations. Every non-dashboard call returns a ggplot
#' object that can be further customised.
#'
#' @param x A data frame/reconstruction result, or an object produced by
#'   [diagnose_gazepoint_binocular_pupil()],
#'   [validate_gazepoint_binocular_reconstruction()],
#'   [audit_gazepoint_binocular_reconstruction()], or
#'   [analyse_gazepoint_binocular_sensitivity()].
#' @param type Plot type: `"trace"`, `"agreement"`, `"bland_altman"`,
#'   `"validation"`, `"residuals"`, `"timeline"`, `"burden"`,
#'   `"sensitivity"`, `"gaps"`, or `"dashboard"`.
#' @param left_col,right_col,time_col Column names for data-frame plots. For
#'   reconstruction outputs these are inferred from metadata when omitted.
#' @param prefix Reconstruction prefix.
#' @param point_alpha Point transparency for dense scatterplots.
#' @param bins Histogram bins for `type = "gaps"`.
#'
#' @return A `ggplot2` object. `type = "dashboard"` returns a named list of
#'   ggplot objects rather than introducing a layout dependency.
#'
#' @seealso [reconstruct_gazepoint_binocular_pupil()],
#'   [validate_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 3, n_trials = 2, seed = 31)
#' dat$pupil_left[20:24] <- NA_real_
#' rec <- reconstruct_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
#'   group_cols = "subject", min_pairs = 20
#' )
#' plot_gazepoint_binocular_diagnostics(rec, type = "agreement")
#'
#' @export
plot_gazepoint_binocular_diagnostics <- function(
    x,
    type = c(
      "trace", "agreement", "bland_altman", "validation", "residuals",
      "timeline", "burden", "sensitivity", "gaps", "dashboard"
    ),
    left_col = NULL,
    right_col = NULL,
    time_col = NULL,
    prefix = "gp3_binocular",
    point_alpha = 0.35,
    bins = 30L) {
  type <- match.arg(type)
  .gp3_binoc_assert_scalar_number(point_alpha, "point_alpha", lower = 0, upper = 1)
  .gp3_binoc_assert_scalar_number(bins, "bins", lower = 1)

  if (type == "dashboard") {
    if (inherits(x, "gp3_binocular_validation")) {
      return(list(
        validation = plot_gazepoint_binocular_diagnostics(x, "validation", point_alpha = point_alpha),
        residuals = plot_gazepoint_binocular_diagnostics(x, "residuals", point_alpha = point_alpha)
      ))
    }
    if (inherits(x, "gp3_binocular_audit")) {
      return(list(
        burden = plot_gazepoint_binocular_diagnostics(x, "burden", point_alpha = point_alpha)
      ))
    }
    if (inherits(x, "gp3_binocular_sensitivity")) {
      return(list(
        sensitivity = plot_gazepoint_binocular_diagnostics(x, "sensitivity", point_alpha = point_alpha)
      ))
    }
    if (is.data.frame(x)) {
      return(list(
        trace = plot_gazepoint_binocular_diagnostics(
          x, "trace", left_col, right_col, time_col, prefix, point_alpha
        ),
        agreement = plot_gazepoint_binocular_diagnostics(
          x, "agreement", left_col, right_col, time_col, prefix, point_alpha
        ),
        timeline = plot_gazepoint_binocular_diagnostics(
          x, "timeline", left_col, right_col, time_col, prefix, point_alpha
        ),
        gaps = plot_gazepoint_binocular_diagnostics(
          x, "gaps", left_col, right_col, time_col, prefix, point_alpha, bins
        )
      ))
    }
    stop("No dashboard is defined for this object type.", call. = FALSE)
  }

  if (type %in% c("validation", "residuals")) {
    if (!inherits(x, "gp3_binocular_validation")) {
      stop("Validation plots require a `gp3_binocular_validation` object.", call. = FALSE)
    }
    dat <- as.data.frame(x$predictions)
    if (!nrow(dat)) stop("The validation object contains no predictions to plot.", call. = FALSE)
    if (type == "validation") {
      return(
        ggplot2::ggplot(dat, ggplot2::aes(x = .data$observed, y = .data$predicted)) +
          ggplot2::geom_point(alpha = point_alpha) +
          ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
          ggplot2::facet_wrap(~direction, scales = "free") +
          ggplot2::labs(
            x = "Held-out observed pupil diameter",
            y = "Reconstructed pupil diameter",
            title = "Artificial monocular-loss validation",
            subtitle = "Identity line indicates exact prediction"
          ) +
          ggplot2::theme_minimal()
      )
    }
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$predicted, y = .data$error)) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::geom_point(alpha = point_alpha) +
        ggplot2::facet_wrap(~direction, scales = "free_x") +
        ggplot2::labs(
          x = "Reconstructed pupil diameter",
          y = "Prediction error (reconstructed - observed)",
          title = "Binocular reconstruction residual diagnostics"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "burden") {
    if (!inherits(x, "gp3_binocular_audit")) {
      stop("`type = \"burden\"` requires a `gp3_binocular_audit` object.", call. = FALSE)
    }
    dat <- as.data.frame(x$by_group)
    if (!nrow(dat)) dat <- as.data.frame(x$overall)
    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = stats::reorder(.data$group_key, .data$reconstruction_fraction),
                     y = .data$reconstruction_fraction)
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(labels = function(z) paste0(round(100 * z), "%")) +
        ggplot2::labs(
          x = NULL,
          y = "Rows containing model-based reconstruction",
          title = "Binocular reconstruction burden"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "sensitivity") {
    if (!inherits(x, "gp3_binocular_sensitivity")) {
      stop("`type = \"sensitivity\"` requires a `gp3_binocular_sensitivity` object.", call. = FALSE)
    }
    dat <- as.data.frame(x$summary)
    if (!nrow(dat)) stop("The sensitivity object contains no summaries to plot.", call. = FALSE)
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$policy, y = .data$mean)) +
        ggplot2::geom_point(size = 2.2, alpha = max(point_alpha, 0.6)) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
          width = 0.15,
          na.rm = TRUE
        ) +
        ggplot2::labs(
          x = "Pupil-construction policy",
          y = "Mean pupil diameter (error bar = +/- 1 SD)",
          title = "Sensitivity to binocular pupil-construction policy"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
    )
  }

  if (!is.data.frame(x)) {
    stop(sprintf("`type = \"%s\"` requires a data frame or reconstruction output.", type), call. = FALSE)
  }
  metadata <- attr(x, "gp3_binocular_reconstruction", exact = TRUE)
  if (!is.null(metadata)) {
    if (is.null(left_col)) left_col <- metadata$left_col
    if (is.null(right_col)) right_col <- metadata$right_col
    if (is.null(time_col)) time_col <- metadata$time_col
    if (identical(prefix, "gp3_binocular") && !is.null(metadata$prefix)) prefix <- metadata$prefix
  }
  if (is.null(left_col) || is.null(right_col)) {
    stop("Supply `left_col` and `right_col`, or pass a reconstruction output with metadata.", call. = FALSE)
  }
  .gp3_binoc_assert_cols(x, c(left_col, right_col, time_col))
  .gp3_binoc_assert_numeric_col(x, left_col)
  .gp3_binoc_assert_numeric_col(x, right_col)

  left <- as.numeric(x[[left_col]])
  right <- as.numeric(x[[right_col]])

  if (type == "agreement") {
    dat <- data.frame(left = left, right = right)
    dat <- dat[is.finite(dat$left) & is.finite(dat$right), , drop = FALSE]
    if (!nrow(dat)) stop("No bilateral observations are available for an agreement plot.", call. = FALSE)
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$left, y = .data$right)) +
        ggplot2::geom_point(alpha = point_alpha) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        ggplot2::labs(
          x = paste("Left pupil:", left_col),
          y = paste("Right pupil:", right_col),
          title = "Observed left/right pupil agreement",
          subtitle = "Dashed line is identity; fitted line is descriptive"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "bland_altman") {
    ok <- is.finite(left) & is.finite(right)
    dat <- data.frame(
      pair_mean = (left[ok] + right[ok]) / 2,
      difference = left[ok] - right[ok]
    )
    if (nrow(dat) < 2L) stop("At least two bilateral observations are required.", call. = FALSE)
    bias <- mean(dat$difference)
    sd_diff <- stats::sd(dat$difference)
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$pair_mean, y = .data$difference)) +
        ggplot2::geom_point(alpha = point_alpha) +
        ggplot2::geom_hline(yintercept = bias) +
        ggplot2::geom_hline(yintercept = bias + c(-1.96, 1.96) * sd_diff, linetype = 2) +
        ggplot2::labs(
          x = "Mean of observed left and right pupil",
          y = "Left - right pupil",
          title = "Descriptive binocular agreement",
          subtitle = "Horizontal lines show mean difference and +/- 1.96 SD limits"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "trace") {
    if (is.null(time_col)) {
      time <- seq_len(nrow(x))
      x_label <- "Row order"
    } else {
      time <- as.numeric(x[[time_col]])
      x_label <- time_col
    }
    dat <- data.frame(
      time = rep(time, 2L),
      pupil = c(left, right),
      channel = rep(c("left observed", "right observed"), each = nrow(x)),
      stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$time, y = .data$pupil, linetype = .data$channel)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::labs(
        x = x_label,
        y = "Pupil diameter",
        linetype = NULL,
        title = "Binocular pupil trace"
      ) +
      ggplot2::theme_minimal()
    final_left <- paste0(prefix, "_left_final")
    final_right <- paste0(prefix, "_right_final")
    rec_flag <- paste0(prefix, "_reconstructed")
    if (all(c(final_left, final_right, rec_flag) %in% names(x))) {
      rec <- as.logical(x[[rec_flag]])
      rec[is.na(rec)] <- FALSE
      final_matrix <- cbind(x[[final_left]], x[[final_right]])
      combined <- rowMeans(final_matrix, na.rm = TRUE)
      combined[!is.finite(combined)] <- NA_real_
      combined_dat <- data.frame(time = time, pupil = combined)
      p <- p + ggplot2::geom_line(
        data = combined_dat,
        ggplot2::aes(x = .data$time, y = .data$pupil),
        inherit.aes = FALSE,
        linewidth = 0.7,
        alpha = 0.75
      )
      if (any(rec)) {
        overlay <- data.frame(
          time = rep(time[rec], 2L),
          pupil = c(x[[final_left]][rec], x[[final_right]][rec]),
          channel = rep(c("left reconstructed", "right reconstructed"), each = sum(rec)),
          stringsAsFactors = FALSE
        )
        p <- p + ggplot2::geom_point(
          data = overlay,
          ggplot2::aes(x = .data$time, y = .data$pupil, shape = .data$channel),
          inherit.aes = FALSE,
          size = 1.6,
          na.rm = TRUE
        )
      }
      p <- p + ggplot2::labs(subtitle = "Solid overlay is the available final-channel mean; reconstructed eye values are marked")
    }
    return(p)
  }

  if (type == "timeline") {
    status_col <- paste0(prefix, "_status")
    .gp3_binoc_assert_cols(x, status_col, "reconstruction status column")
    time <- if (is.null(time_col)) seq_len(nrow(x)) else as.numeric(x[[time_col]])
    dat <- data.frame(time = time, status = as.character(x[[status_col]]), stringsAsFactors = FALSE)
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$time, y = .data$status)) +
        ggplot2::geom_point(alpha = max(point_alpha, 0.55), size = 1.2) +
        ggplot2::labs(
          x = if (is.null(time_col)) "Row order" else time_col,
          y = NULL,
          title = "Binocular information provenance over time"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "gaps") {
    gap_col <- paste0(prefix, "_gap_ms")
    rec_col <- paste0(prefix, "_reconstructed")
    .gp3_binoc_assert_cols(x, c(gap_col, rec_col), "reconstruction gap columns")
    rec <- as.logical(x[[rec_col]])
    rec[is.na(rec)] <- FALSE
    dat <- data.frame(gap_ms = x[[gap_col]][rec])
    dat <- dat[is.finite(dat$gap_ms), , drop = FALSE]
    if (!nrow(dat)) stop("No finite reconstructed gap durations are available to plot.", call. = FALSE)
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = .data$gap_ms)) +
        ggplot2::geom_histogram(bins = as.integer(bins)) +
        ggplot2::labs(
          x = "Reconstructed missing-eye run duration (ms)",
          y = "Rows",
          title = "Duration of model-supported reconstruction intervals"
        ) +
        ggplot2::theme_minimal()
    )
  }

  stop("Unsupported plot type.", call. = FALSE)
}
