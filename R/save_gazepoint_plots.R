#' Save standard Gazepoint diagnostic plots
#'
#' Saves standard diagnostic plots produced from `gp3tools` workflow outputs.
#'
#' @param flagged_quality Flagged tracking-quality table, usually from
#' `flag_tracking_quality()` or `run_gazepoint_workflow()`.
#' @param sampling Sampling-rate table, usually from `check_sampling_rate()`
#' or `run_gazepoint_workflow()`.
#' @param output_dir Folder where plot files should be saved.
#' @param prefix Filename prefix used for saved plot files.
#' @param overwrite Logical. If `FALSE`, stop when output plot files already exist.
#' @param width Plot width in inches.
#' @param height_quality Tracking-quality plot height in inches.
#' @param height_sampling Sampling-rate plot height in inches.
#' @param dpi Plot resolution.
#'
#' @return A tibble with plot names and written file paths.
#' @export
save_gazepoint_plots <- function(
    flagged_quality = NULL,
    sampling = NULL,
    output_dir,
    prefix = "gazepoint",
    overwrite = TRUE,
    width = 9,
    height_quality = 6,
    height_sampling = 5,
    dpi = 300
) {
  plots <- list()
  paths <- character()

  if (!is.null(flagged_quality)) {
    plots$tracking_quality_plot <- plot_tracking_quality(flagged_quality)
    paths["tracking_quality_plot"] <- file.path(
      output_dir,
      paste0(prefix, "_tracking_quality_plot.png")
    )
  }

  if (!is.null(sampling)) {
    plots$sampling_rate_plot <- plot_sampling_rate(sampling)
    paths["sampling_rate_plot"] <- file.path(
      output_dir,
      paste0(prefix, "_sampling_rate_plot.png")
    )
  }

  if (length(plots) == 0) {
    rlang::abort("At least one plot input must be provided.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  existing <- paths[file.exists(paths)]

  if (length(existing) > 0 && !overwrite) {
    rlang::abort(
      paste0(
        "The following plot files already exist: ",
        paste(basename(existing), collapse = ", ")
      )
    )
  }

  for (plot_name in names(plots)) {
    plot_height <- if (plot_name == "tracking_quality_plot") {
      height_quality
    } else {
      height_sampling
    }

    ggplot2::ggsave(
      filename = paths[[plot_name]],
      plot = plots[[plot_name]],
      width = width,
      height = plot_height,
      dpi = dpi
    )
  }

  tibble::tibble(
    plot = names(plots),
    file = unname(paths)
  )
}
