#' Summarise Gazepoint preprocessing multiverse results
#'
#' Summarise pupil and/or AOI preprocessing multiverse result objects created by
#' `run_gazepoint_pupil_multiverse()` and `run_gazepoint_aoi_multiverse()`.
#'
#' @param ... One or more multiverse result objects.
#' @param results Optional named list of multiverse result objects.
#'
#' @return A list with class `gp3_multiverse_summary_results` containing
#'   overview, branch summary, failure summary, and settings tables.
#' @export
summarise_gazepoint_multiverse_results <- function(..., results = NULL) {
  dots <- list(...)

  if (!is.null(results)) {
    if (!is.list(results) || is.data.frame(results)) {
      stop("`results` must be a named list of multiverse result objects.", call. = FALSE)
    }

    dots <- c(dots, results)
  }

  if (length(dots) == 0L) {
    stop("At least one multiverse result object must be supplied.", call. = FALSE)
  }

  if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
    names(dots) <- .gp3_multiverse_default_result_names(dots)
  }

  valid <- vapply(
    dots,
    .gp3_is_multiverse_result_object,
    logical(1)
  )

  if (any(!valid)) {
    stop(
      "All supplied objects must be pupil or AOI multiverse result objects.",
      call. = FALSE
    )
  }

  overview <- .gp3_summarise_multiverse_overview(dots)
  branch_summary <- .gp3_summarise_multiverse_branches(dots)
  failure_summary <- .gp3_summarise_multiverse_failures(branch_summary)

  settings <- tibble::tibble(
    setting = c(
      "n_result_objects",
      "result_names",
      "result_classes"
    ),
    value = c(
      as.character(length(dots)),
      paste(names(dots), collapse = ", "),
      paste(
        vapply(
          dots,
          function(x) paste(class(x), collapse = "/"),
          character(1)
        ),
        collapse = " | "
      )
    )
  )

  out <- list(
    overview = overview,
    branch_summary = branch_summary,
    failure_summary = failure_summary,
    settings = settings
  )

  class(out) <- c("gp3_multiverse_summary_results", "list")

  out
}

.gp3_is_multiverse_result_object <- function(x) {
  inherits(x, "gp3_pupil_multiverse_results") ||
    inherits(x, "gp3_aoi_multiverse_results")
}

.gp3_multiverse_result_family <- function(x) {
  if (inherits(x, "gp3_pupil_multiverse_results")) {
    return("pupil")
  }

  if (inherits(x, "gp3_aoi_multiverse_results")) {
    return("aoi")
  }

  "unknown"
}

.gp3_multiverse_default_result_names <- function(x) {
  paste0(
    vapply(x, .gp3_multiverse_result_family, character(1)),
    "_multiverse_",
    seq_along(x)
  )
}

.gp3_summarise_multiverse_overview <- function(results) {
  rows <- vector("list", length(results))

  for (i in seq_along(results)) {
    result_name <- names(results)[[i]]
    result <- results[[i]]
    family <- .gp3_multiverse_result_family(result)

    ov <- result$overview

    rows[[i]] <- tibble::tibble(
      result_name = result_name,
      multiverse_family = family,
      n_defined_branches = .gp3_multiverse_first_or_na(ov$n_defined_branches),
      n_requested_branches = .gp3_multiverse_first_or_na(ov$n_requested_branches),
      n_completed_branches = .gp3_multiverse_first_or_na(ov$n_completed_branches),
      n_failed_branches = .gp3_multiverse_first_or_na(ov$n_failed_branches),
      n_skipped_branches = .gp3_multiverse_first_or_na(ov$n_skipped_branches),
      multiverse_status = .gp3_multiverse_first_or_na_character(ov$multiverse_status)
    )
  }

  overview <- dplyr::bind_rows(rows)

  tibble::add_row(
    overview,
    result_name = "overall",
    multiverse_family = "combined",
    n_defined_branches = sum(overview$n_defined_branches, na.rm = TRUE),
    n_requested_branches = sum(overview$n_requested_branches, na.rm = TRUE),
    n_completed_branches = sum(overview$n_completed_branches, na.rm = TRUE),
    n_failed_branches = sum(overview$n_failed_branches, na.rm = TRUE),
    n_skipped_branches = sum(overview$n_skipped_branches, na.rm = TRUE),
    multiverse_status = dplyr::case_when(
      sum(overview$n_requested_branches, na.rm = TRUE) == 0L ~ "not_run",
      sum(overview$n_failed_branches, na.rm = TRUE) > 0L ~ "completed_with_errors",
      all(overview$multiverse_status == "completed") ~ "completed",
      TRUE ~ "completed_with_cautions"
    )
  )
}

.gp3_summarise_multiverse_branches <- function(results) {
  rows <- list()
  row_i <- 0L

  for (i in seq_along(results)) {
    result_name <- names(results)[[i]]
    result <- results[[i]]
    family <- .gp3_multiverse_result_family(result)

    branches <- result$branch_results

    if (!is.data.frame(branches) || nrow(branches) == 0L) {
      next
    }

    branches$result_name <- result_name
    branches$multiverse_family <- family

    front_cols <- c(
      "result_name",
      "multiverse_family",
      "branch_id",
      "branch_label",
      "branch_status"
    )

    ordered_cols <- c(
      front_cols[front_cols %in% names(branches)],
      setdiff(names(branches), front_cols)
    )

    row_i <- row_i + 1L
    rows[[row_i]] <- branches[, ordered_cols, drop = FALSE]
  }

  if (length(rows) == 0L) {
    return(tibble::tibble(
      result_name = character(),
      multiverse_family = character(),
      branch_id = character(),
      branch_label = character(),
      branch_status = character()
    ))
  }

  dplyr::bind_rows(rows)
}

.gp3_summarise_multiverse_failures <- function(branch_summary) {
  if (!is.data.frame(branch_summary) || nrow(branch_summary) == 0L) {
    return(tibble::tibble(
      result_name = character(),
      multiverse_family = character(),
      branch_id = character(),
      branch_label = character(),
      branch_status = character(),
      message = character()
    ))
  }

  if (!"message" %in% names(branch_summary)) {
    branch_summary$message <- NA_character_
  }

  out <- branch_summary[
    branch_summary$branch_status %in% c("failed", "skipped") |
      (!is.na(branch_summary$message) & nzchar(branch_summary$message)),
    ,
    drop = FALSE
  ]

  keep_cols <- c(
    "result_name",
    "multiverse_family",
    "branch_id",
    "branch_label",
    "branch_status",
    "message"
  )

  keep_cols <- keep_cols[keep_cols %in% names(out)]

  out[, keep_cols, drop = FALSE]
}

.gp3_multiverse_first_or_na <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_integer_)
  }

  x[[1]]
}

.gp3_multiverse_first_or_na_character <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  as.character(x[[1]])
}
