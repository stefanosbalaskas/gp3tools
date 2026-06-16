#' Summarise a Gazepoint workflow result
#'
#' Creates a compact one-row summary from a result object returned by
#' [run_gazepoint_workflow()]. This is useful for quickly checking how many rows,
#' file pairs, flagged recordings, exported tables, exported plots, and reports
#' were produced by the workflow.
#'
#' @param results A named list returned by [run_gazepoint_workflow()].
#'
#' @return A tibble with one row containing workflow-level summary counts.
#'
#' @examples
#' \dontrun{
#' results <- run_gazepoint_workflow(
#'   export_dir = "C:/Users/YourName/Desktop/gp3_test_exports",
#'   output_dir = "C:/Users/YourName/Desktop/gp3_outputs",
#'   prefix = "study1",
#'   save_plots = TRUE,
#'   create_report = TRUE
#' )
#'
#' summarise_gazepoint_workflow(results)
#' }
#'
#' @export
summarise_gazepoint_workflow <- function(results) {
  if (!is.list(results)) {
    rlang::abort("`results` must be a list returned by `run_gazepoint_workflow()`.")
  }

  required_results <- c(
    "all_gaze",
    "all_fix",
    "sampling",
    "quality",
    "flagged_quality",
    "aoi_table"
  )

  missing_results <- setdiff(required_results, names(results))

  if (length(missing_results) > 0) {
    rlang::abort(
      paste0(
        "`results` is missing required elements: ",
        paste(missing_results, collapse = ", ")
      )
    )
  }

  n_rows_safe <- function(x) {
    if (is.data.frame(x)) {
      return(nrow(x))
    }

    NA_integer_
  }

  n_entries_safe <- function(x) {
    if (is.null(x)) {
      return(0L)
    }

    if (is.data.frame(x)) {
      return(nrow(x))
    }

    length(x)
  }

  n_review_required <- NA_integer_

  if (
    is.data.frame(results$flagged_quality) &&
    "review_required" %in% names(results$flagged_quality)
  ) {
    n_review_required <- sum(
      results$flagged_quality$review_required %in% TRUE,
      na.rm = TRUE
    )
  }

  file_pair_rows <- NA_integer_
  complete_file_pairs <- NA_integer_
  problem_file_pairs <- NA_integer_

  if (
    "file_pairs" %in% names(results) &&
    is.data.frame(results$file_pairs)
  ) {
    file_pair_rows <- nrow(results$file_pairs)

    if ("status" %in% names(results$file_pairs)) {
      complete_file_pairs <- sum(
        results$file_pairs$status == "complete",
        na.rm = TRUE
      )

      problem_file_pairs <- sum(
        results$file_pairs$status != "complete",
        na.rm = TRUE
      )
    }
  }

  tibble::tibble(
    all_gaze_rows = n_rows_safe(results$all_gaze),
    fixation_rows = n_rows_safe(results$all_fix),
    sampling_rows = n_rows_safe(results$sampling),
    tracking_quality_rows = n_rows_safe(results$quality),
    flagged_quality_rows = n_rows_safe(results$flagged_quality),
    aoi_rows = n_rows_safe(results$aoi_table),
    review_required_rows = n_review_required,
    file_pair_rows = file_pair_rows,
    complete_file_pairs = complete_file_pairs,
    problem_file_pairs = problem_file_pairs,
    output_table_files = n_entries_safe(results$written_files),
    output_plot_files = n_entries_safe(results$written_plots),
    report_created = !is.null(results$written_report)
  )
}
