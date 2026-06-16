#' Check Gazepoint all-gaze and fixation file pairs
#'
#' Checks whether each Gazepoint participant/source has both an all-gaze CSV file
#' and a fixation CSV file before running the full workflow.
#'
#' @param folder Folder containing Gazepoint CSV export files.
#' @param all_gaze_pattern Regular expression for selecting all-gaze files.
#' @param fixation_pattern Regular expression for selecting fixation files.
#' @param recursive Logical. If `TRUE`, search subfolders recursively.
#'
#' @return A tibble with one row per detected participant/source and file-pair status.
#' @export
check_gazepoint_file_pairs <- function(
    folder,
    all_gaze_pattern = "_all_gaze\\.csv$",
    fixation_pattern = "_fixations\\.csv$",
    recursive = FALSE
) {
  if (!dir.exists(folder)) {
    rlang::abort(paste0("`folder` does not exist: ", folder))
  }

  all_gaze_files <- list.files(
    folder,
    pattern = all_gaze_pattern,
    full.names = TRUE,
    recursive = recursive
  )

  fixation_files <- list.files(
    folder,
    pattern = fixation_pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(all_gaze_files) == 0 && length(fixation_files) == 0) {
    rlang::abort(
      paste0(
        "No files matching `",
        all_gaze_pattern,
        "` or `",
        fixation_pattern,
        "` were found in ",
        folder,
        "."
      )
    )
  }

  if (length(all_gaze_files) > 0) {
    all_ids <- sub(
      all_gaze_pattern,
      "",
      basename(all_gaze_files),
      perl = TRUE
    )

    all_split <- split(basename(all_gaze_files), all_ids)

    all_tbl <- tibble::tibble(
      participant = names(all_split),
      all_gaze_file = vapply(
        all_split,
        function(x) paste(sort(unique(x)), collapse = "; "),
        character(1)
      ),
      n_all_gaze = as.integer(lengths(all_split))
    )
  } else {
    all_tbl <- tibble::tibble(
      participant = character(),
      all_gaze_file = character(),
      n_all_gaze = integer()
    )
  }

  if (length(fixation_files) > 0) {
    fixation_ids <- sub(
      fixation_pattern,
      "",
      basename(fixation_files),
      perl = TRUE
    )

    fixation_split <- split(basename(fixation_files), fixation_ids)

    fixation_tbl <- tibble::tibble(
      participant = names(fixation_split),
      fixation_file = vapply(
        fixation_split,
        function(x) paste(sort(unique(x)), collapse = "; "),
        character(1)
      ),
      n_fixation = as.integer(lengths(fixation_split))
    )
  } else {
    fixation_tbl <- tibble::tibble(
      participant = character(),
      fixation_file = character(),
      n_fixation = integer()
    )
  }

  out <- dplyr::full_join(
    all_tbl,
    fixation_tbl,
    by = "participant"
  )

  out$all_gaze_file[is.na(out$all_gaze_file)] <- ""
  out$fixation_file[is.na(out$fixation_file)] <- ""

  out$n_all_gaze[is.na(out$n_all_gaze)] <- 0L
  out$n_fixation[is.na(out$n_fixation)] <- 0L

  out$has_all_gaze <- out$n_all_gaze > 0
  out$has_fixation <- out$n_fixation > 0

  out$duplicate_all_gaze <- out$n_all_gaze > 1
  out$duplicate_fixation <- out$n_fixation > 1

  out$status <- "complete"

  out$status[out$duplicate_all_gaze | out$duplicate_fixation] <- "duplicate_files"
  out$status[!out$has_fixation] <- "missing_fixation"
  out$status[!out$has_all_gaze] <- "missing_all_gaze"

  out <- out[
    order(out$participant),
    c(
      "participant",
      "all_gaze_file",
      "fixation_file",
      "n_all_gaze",
      "n_fixation",
      "has_all_gaze",
      "has_fixation",
      "duplicate_all_gaze",
      "duplicate_fixation",
      "status"
    )
  ]

  tibble::as_tibble(out)
}
