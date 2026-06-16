#' Run a complete Gazepoint analysis workflow
#'
#' Reads Gazepoint all-gaze and fixation exports from a folder, computes
#' sampling-rate checks, tracking-quality summaries, quality flags, and AOI-level
#' metrics, and optionally exports output tables, diagnostic plots, and an HTML
#' diagnostic report.
#'
#' @param export_dir Folder containing Gazepoint CSV export files.
#' @param all_gaze_pattern Regular expression for selecting all-gaze files.
#' @param fixation_pattern Regular expression for selecting fixation files.
#' @param check_file_pairs Logical. If `TRUE`, check that each participant/source
#' has both an all-gaze file and a fixation file before importing.
#' @param group_cols Columns used for grouped sampling and tracking-quality summaries.
#' @param user_col Column name used to identify the source/user file.
#' @param sample_rate Sampling rate used for approximate sample-based AOI viewed time.
#' @param min_gaze_valid_pct Minimum acceptable FPOGV validity percentage.
#' @param min_pupil_valid_pct Minimum acceptable pupil validity percentage.
#' @param expected_hz Expected sampling rate.
#' @param hz_tolerance Allowed deviation from the expected sampling rate.
#' @param min_duration_sec Optional minimum acceptable recording duration in seconds.
#' @param output_dir Optional folder where output CSV files should be written.
#' @param prefix Filename prefix used when exporting output tables, plots, and report.
#' @param overwrite Logical. If `FALSE`, stop when output files already exist.
#' @param save_plots Logical. If `TRUE`, save standard diagnostic plots.
#' @param plot_output_dir Optional folder where diagnostic plots should be saved.
#' If `NULL`, `output_dir` is used.
#' @param create_report Logical. If `TRUE`, create an HTML diagnostic report.
#' @param report_file Optional path to the HTML report. If `NULL`, a report file
#' is created in `output_dir` using `prefix`.
#' @param report_title Title used in the HTML diagnostic report.
#' @param report_plot_dir Optional folder for plots used inside the HTML report.
#' @param report_max_rows Maximum number of rows shown in report preview tables.
#'
#' @return A named list containing file-pair checks, imported data, analysis
#' tables, quality flags, written table paths, written plot paths, and written
#' report path.
#' @export
run_gazepoint_workflow <- function(
    export_dir,
    all_gaze_pattern = "_all_gaze\\.csv$",
    fixation_pattern = "_fixations\\.csv$",
    check_file_pairs = TRUE,
    group_cols = c("USER_FILE", "MEDIA_ID"),
    user_col = "USER_FILE",
    sample_rate = 60,
    min_gaze_valid_pct = 70,
    min_pupil_valid_pct = 70,
    expected_hz = 60,
    hz_tolerance = 5,
    min_duration_sec = NULL,
    output_dir = NULL,
    prefix = "gazepoint",
    overwrite = TRUE,
    save_plots = FALSE,
    plot_output_dir = NULL,
    create_report = FALSE,
    report_file = NULL,
    report_title = "Gazepoint diagnostic report",
    report_plot_dir = NULL,
    report_max_rows = 30
) {
  if (!dir.exists(export_dir)) {
    rlang::abort(paste0("`export_dir` does not exist: ", export_dir))
  }

  file_pairs <- NULL

  if (isTRUE(check_file_pairs)) {
    file_pairs <- check_gazepoint_file_pairs(
      folder = export_dir,
      all_gaze_pattern = all_gaze_pattern,
      fixation_pattern = fixation_pattern
    )

    problematic_pairs <- file_pairs[file_pairs$status != "complete", , drop = FALSE]

    if (nrow(problematic_pairs) > 0) {
      problem_summary <- paste0(
        problematic_pairs$participant,
        " (",
        problematic_pairs$status,
        ")",
        collapse = ", "
      )

      rlang::abort(
        paste0(
          "Gazepoint file-pair check failed. Problematic participant/source files: ",
          problem_summary,
          "."
        )
      )
    }
  }

  all_gaze <- read_gazepoint_folder(
    folder = export_dir,
    pattern = all_gaze_pattern,
    source_col = user_col
  )

  all_fix <- read_gazepoint_folder(
    folder = export_dir,
    pattern = fixation_pattern,
    source_col = user_col
  )

  sampling <- check_sampling_rate(
    all_gaze,
    group_cols = group_cols
  )

  quality <- summarise_tracking_quality(
    all_gaze,
    group_cols = group_cols
  )

  flagged_quality <- flag_tracking_quality(
    quality = quality,
    sampling = sampling,
    by = group_cols,
    min_gaze_valid_pct = min_gaze_valid_pct,
    min_pupil_valid_pct = min_pupil_valid_pct,
    expected_hz = expected_hz,
    hz_tolerance = hz_tolerance,
    min_duration_sec = min_duration_sec
  )

  aoi_table <- summarise_gazepoint_aoi(
    gaze_data = all_gaze,
    fixation_data = all_fix,
    user_col = user_col,
    sample_rate = sample_rate
  )

  written_files <- NULL

  if (!is.null(output_dir)) {
    written_files <- write_gazepoint_outputs(
      sampling = sampling,
      quality = quality,
      flagged_quality = flagged_quality,
      aoi_table = aoi_table,
      output_dir = output_dir,
      prefix = prefix,
      overwrite = overwrite
    )
  }

  written_plots <- NULL

  if (isTRUE(save_plots)) {
    if (is.null(plot_output_dir)) {
      plot_output_dir <- output_dir
    }

    if (is.null(plot_output_dir)) {
      rlang::abort(
        "`output_dir` or `plot_output_dir` must be provided when `save_plots = TRUE`."
      )
    }

    written_plots <- save_gazepoint_plots(
      flagged_quality = flagged_quality,
      sampling = sampling,
      output_dir = plot_output_dir,
      prefix = prefix,
      overwrite = overwrite
    )
  }

  results <- list(
    file_pairs = file_pairs,
    all_gaze = all_gaze,
    all_fix = all_fix,
    sampling = sampling,
    quality = quality,
    flagged_quality = flagged_quality,
    aoi_table = aoi_table,
    written_files = written_files,
    written_plots = written_plots
  )

  written_report <- NULL

  if (isTRUE(create_report)) {
    if (is.null(report_file)) {
      if (is.null(output_dir)) {
        rlang::abort(
          "`output_dir` or `report_file` must be provided when `create_report = TRUE`."
        )
      }

      report_file <- file.path(
        output_dir,
        paste0(prefix, "_report.html")
      )
    }

    written_report <- create_gazepoint_report(
      results = results,
      output_file = report_file,
      title = report_title,
      overwrite = overwrite,
      max_rows = report_max_rows,
      save_plots = TRUE,
      plot_dir = report_plot_dir
    )
  }

  results["written_report"] <- list(written_report)

  results
}
