#' Export Gazepoint analysis tables to CSV files
#'
#' Writes a named list of analysis tables to CSV files in an output folder.
#'
#' @param tables A named list of data frames or tibbles.
#' @param output_dir Folder where CSV files should be written.
#' @param prefix Optional filename prefix.
#' @param overwrite Logical. If `FALSE`, the function stops when a target file already exists.
#' @param na Value used for missing values in the exported CSV files.
#'
#' @return A tibble with table names and written file paths.
#' @export
export_gazepoint_tables <- function(
    tables,
    output_dir,
    prefix = NULL,
    overwrite = TRUE,
    na = ""
) {
  if (!is.list(tables) || is.null(names(tables)) || any(names(tables) == "")) {
    rlang::abort("`tables` must be a named list of data frames.")
  }

  is_table <- vapply(tables, is.data.frame, logical(1))

  if (!all(is_table)) {
    rlang::abort("Every element of `tables` must be a data frame or tibble.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  safe_names <- names(tables)
  safe_names <- gsub("[^A-Za-z0-9_\\-]+", "_", safe_names)

  if (!is.null(prefix)) {
    safe_names <- paste(prefix, safe_names, sep = "_")
  }

  paths <- file.path(output_dir, paste0(safe_names, ".csv"))

  existing <- paths[file.exists(paths)]

  if (length(existing) > 0 && !overwrite) {
    rlang::abort(
      paste0(
        "The following output files already exist: ",
        paste(basename(existing), collapse = ", ")
      )
    )
  }

  for (i in seq_along(tables)) {
    readr::write_csv(
      tables[[i]],
      paths[[i]],
      na = na
    )
  }

  tibble::tibble(
    table = names(tables),
    file = paths
  )
}

#' Write standard Gazepoint analysis outputs
#'
#' Convenience wrapper for exporting standard `gp3tools` outputs such as sampling
#' checks, tracking quality summaries, flagged quality rows, and AOI tables.
#'
#' @param sampling Sampling-rate table, usually from `check_sampling_rate()`.
#' @param quality Tracking-quality table, usually from `summarise_tracking_quality()`.
#' @param flagged_quality Flagged quality table, usually from `flag_tracking_quality()`.
#' @param aoi_table AOI summary table, usually from `summarise_gazepoint_aoi()`.
#' @param output_dir Folder where CSV files should be written.
#' @param prefix Optional filename prefix.
#' @param overwrite Logical. If `FALSE`, stop when files already exist.
#'
#' @return A tibble with table names and written file paths.
#' @export
write_gazepoint_outputs <- function(
    sampling = NULL,
    quality = NULL,
    flagged_quality = NULL,
    aoi_table = NULL,
    output_dir,
    prefix = "gazepoint",
    overwrite = TRUE
) {
  tables <- list()

  if (!is.null(sampling)) {
    tables$sampling <- sampling
  }

  if (!is.null(quality)) {
    tables$quality <- quality
  }

  if (!is.null(flagged_quality)) {
    tables$flagged_quality <- flagged_quality
  }

  if (!is.null(aoi_table)) {
    tables$aoi_table <- aoi_table
  }

  if (length(tables) == 0) {
    rlang::abort("At least one output table must be provided.")
  }

  export_gazepoint_tables(
    tables = tables,
    output_dir = output_dir,
    prefix = prefix,
    overwrite = overwrite
  )
}
