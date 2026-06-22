#' Export a Gazepoint master table, audit tables, and validation tables
#'
#' Exports a Gazepoint master sample-level table together with the output of
#' [audit_gazepoint_master()] and, optionally, [validate_gazepoint_master()].
#' This function is useful after creating a master table with
#' [as_gazepoint_master()] or [create_gazepoint_master()].
#'
#' @param master A Gazepoint master sample-level table.
#' @param audit Optional audit list returned by [audit_gazepoint_master()]. If
#'   `NULL`, the audit is created automatically.
#' @param validation Optional validation list returned by
#'   [validate_gazepoint_master()]. If `NULL` and `export_validation = TRUE`,
#'   validation is created automatically.
#' @param output_dir Directory where CSV files should be written. Must be supplied explicitly.
#' @param prefix File prefix used for all exported CSV files.
#' @param export_master Logical. If `TRUE`, exports the full master table.
#' @param export_audit Logical. If `TRUE`, exports audit tables.
#' @param export_validation Logical. If `TRUE`, exports validation tables.
#' @param overwrite Logical. If `FALSE`, the function aborts when any target
#'   file already exists.
#' @param na String used for missing values in CSV output.
#'
#' @return A tibble listing the exported files, table names, and dimensions.
#'
#' @examples
#' \donttest{
#' master <- tibble::tibble(
#'   subject = c("P1", "P1", "P2", "P2"),
#'   MEDIA_ID = c("M1", "M1", "M1", "M1"),
#'   time = c(0, 16, 0, 16),
#'   x = c(100, 120, 200, 220),
#'   y = c(100, 130, 200, 230),
#'   raw_x = c(0.10, 0.12, 0.20, 0.22),
#'   raw_y = c(0.20, 0.26, 0.30, 0.34),
#'   valid_sample = c(TRUE, TRUE, TRUE, TRUE),
#'   missing_gaze = c(FALSE, FALSE, FALSE, FALSE),
#'   missing_pupil = c(FALSE, FALSE, FALSE, FALSE),
#'   gaze_offscreen = c(FALSE, FALSE, FALSE, FALSE),
#'   mean_pupil = c(3.5, 3.6, 3.7, 3.8),
#'   aoi_current = c("AOI 1", "AOI 1", "AOI 2", "AOI 2"),
#'   aoi_count = c(1L, 1L, 1L, 1L),
#'   screen_width_px = rep(1000, 4),
#'   screen_height_px = rep(500, 4)
#' )
#'
#' exported <- export_gazepoint_master_audit(
#'   master = master,
#'   output_dir = file.path(tempdir(), "gp3_master_audit"),
#'   prefix = "example"
#' )
#'
#' exported
#' }
#' @export
export_gazepoint_master_audit <- function(
    master,
    audit = NULL,
    validation = NULL,
    output_dir = NULL,
    prefix = "gazepoint",
    export_master = TRUE,
    export_audit = TRUE,
    export_validation = TRUE,
    overwrite = TRUE,
    na = ""
) {
  if (!is.data.frame(master)) {
    rlang::abort("`master` must be a data frame.")
  }

  if (is.null(output_dir)) {
    rlang::abort(
      "`output_dir` must be supplied explicitly. In examples, tests, and vignettes, use `tempdir()` or `file.path(tempdir(), ...)`."
    )
  }

  if (
    !is.character(output_dir) ||
    length(output_dir) != 1 ||
    is.na(output_dir) ||
    output_dir == ""
  ) {
    rlang::abort("`output_dir` must be a single non-empty character string.")
  }

  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix) || prefix == "") {
    rlang::abort("`prefix` must be a single non-empty character string.")
  }

  if (!is.logical(export_master) || length(export_master) != 1) {
    rlang::abort("`export_master` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(export_audit) || length(export_audit) != 1) {
    rlang::abort("`export_audit` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(export_validation) || length(export_validation) != 1) {
    rlang::abort("`export_validation` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(overwrite) || length(overwrite) != 1) {
    rlang::abort("`overwrite` must be `TRUE` or `FALSE`.")
  }

  if (!is.character(na) || length(na) != 1 || is.na(na)) {
    rlang::abort("`na` must be a single character string.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(output_dir)) {
    rlang::abort("`output_dir` could not be created.")
  }

  if (isTRUE(export_audit) && is.null(audit)) {
    audit <- audit_gazepoint_master(master)
  }

  if (isTRUE(export_validation) && is.null(validation)) {
    validation <- validate_gazepoint_master(master)
  }

  if (isTRUE(export_audit) && !is.list(audit)) {
    rlang::abort("`audit` must be a list returned by `audit_gazepoint_master()`.")
  }

  if (isTRUE(export_validation) && !is.list(validation)) {
    rlang::abort("`validation` must be a list returned by `validate_gazepoint_master()`.")
  }

  tables <- list()

  if (isTRUE(export_master)) {
    tables$master <- master
  }

  if (isTRUE(export_audit)) {
    expected_audit_tables <- c(
      "overview",
      "by_subject",
      "by_media",
      "by_subject_media",
      "aoi_states",
      "pupil_summary",
      "coordinate_summary"
    )

    missing_audit_tables <- setdiff(expected_audit_tables, names(audit))

    if (length(missing_audit_tables) > 0) {
      rlang::abort(
        paste0(
          "`audit` is missing required element(s): ",
          paste(missing_audit_tables, collapse = ", ")
        )
      )
    }

    for (name in expected_audit_tables) {
      tables[[paste0("audit_", name)]] <- audit[[name]]
    }
  }

  if (isTRUE(export_validation)) {
    expected_validation_tables <- c(
      "summary",
      "checks",
      "failed_checks",
      "warning_checks",
      "column_map"
    )

    missing_validation_tables <- setdiff(
      expected_validation_tables,
      names(validation)
    )

    if (length(missing_validation_tables) > 0) {
      rlang::abort(
        paste0(
          "`validation` is missing required element(s): ",
          paste(missing_validation_tables, collapse = ", ")
        )
      )
    }

    for (name in expected_validation_tables) {
      tables[[paste0("validation_", name)]] <- validation[[name]]
    }
  }

  if (length(tables) == 0) {
    rlang::abort("Nothing to export. At least one of `export_master`, `export_audit`, or `export_validation` must be `TRUE`.")
  }

  non_data_frames <- names(tables)[
    !vapply(tables, is.data.frame, logical(1))
  ]

  if (length(non_data_frames) > 0) {
    rlang::abort(
      paste0(
        "All exported objects must be data frames. Problematic object(s): ",
        paste(non_data_frames, collapse = ", ")
      )
    )
  }

  make_file <- function(name) {
    file.path(output_dir, paste0(prefix, "_", name, ".csv"))
  }

  files <- vapply(names(tables), make_file, character(1))

  existing_files <- files[file.exists(files)]

  if (!isTRUE(overwrite) && length(existing_files) > 0) {
    rlang::abort(
      paste0(
        "The following output file(s) already exist and `overwrite = FALSE`: ",
        paste(existing_files, collapse = ", ")
      )
    )
  }

  for (name in names(tables)) {
    utils::write.csv(
      tables[[name]],
      file = files[[name]],
      row.names = FALSE,
      na = na
    )
  }

  tibble::tibble(
    table = names(tables),
    file = unname(normalizePath(files, winslash = "/", mustWork = FALSE)),
    n_rows = unname(vapply(tables, nrow, integer(1))),
    n_cols = unname(vapply(tables, ncol, integer(1)))
  )
}
