#' Read multiple Gazepoint CSV exports from a folder
#'
#' Reads all Gazepoint all-gaze or fixation CSV exports in a folder that match
#' a filename pattern and combines them into one tibble.
#'
#' @param folder Path to the folder containing Gazepoint CSV exports.
#' @param pattern Regular expression used to select files. For example,
#' `"_all_gaze\\.csv$"` or `"_fixations\\.csv$"`.
#' @param source_col Name of the column storing the source filename.
#' @param recursive Logical. If `TRUE`, search subfolders recursively.
#' @param ... Additional arguments passed to `read_gazepoint()`.
#'
#' @return A tibble containing all matching files combined row-wise.
#' @export
read_gazepoint_folder <- function(
    folder,
    pattern = "\\.csv$",
    source_col = "USER_FILE",
    recursive = FALSE,
    ...
) {
  if (!dir.exists(folder)) {
    rlang::abort(paste0(folder, " does not exist."))
  }

  files <- list.files(
    folder,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(files) == 0) {
    rlang::abort(
      paste0(
        "No files matching pattern `",
        pattern,
        "` were found in ",
        folder,
        "."
      )
    )
  }

  out <- lapply(files, function(path) {
    dat <- read_gazepoint(path, ...)
    dat[[source_col]] <- basename(path)
    dat
  })

  dplyr::bind_rows(out)
}
