#' Transform AOI proportions to empirical logits
#'
#' Convert bounded AOI proportions into empirical logits for linear mixed models,
#' growth-curve analysis, or other approximately Gaussian time-course models.
#'
#' Binomial GLMMs are usually preferable when numerator and denominator counts
#' are available. This helper is intended for sensitivity analyses, GCA-style
#' models, and linear time-course summaries where a transformed AOI proportion
#' is needed.
#'
#' @param data A data frame containing AOI proportions or AOI count data.
#' @param numerator_col Optional numerator column, for example number of samples
#'   or fixations inside the AOI.
#' @param denominator_col Optional denominator column, for example total valid
#'   samples or total fixations in the window.
#' @param proportion_col Optional bounded AOI proportion column. If
#'   `numerator_col` and `denominator_col` are supplied, the raw proportion is
#'   computed from those columns. If only `proportion_col` is supplied, a
#'   pseudo-denominator is used and recorded in the output.
#' @param correction Positive correction constant added to numerator and
#'   non-AOI count. The common empirical-logit correction is `0.5`.
#' @param pseudo_denominator Positive pseudo-denominator used only when
#'   `proportion_col` is supplied without `denominator_col`.
#' @param output_col Name of the empirical-logit output column.
#' @param adjusted_proportion_col Name of the adjusted proportion output column.
#' @param raw_proportion_col Name of the raw proportion output column.
#' @param numerator_output_col Name of the numerator output column used in the
#'   transformation.
#' @param denominator_output_col Name of the denominator output column used in
#'   the transformation.
#' @param status_col Name of the row-level transformation status column.
#' @param overwrite Logical. If `FALSE`, the function errors when output columns
#'   already exist in `data`.
#' @param name Character label stored in object attributes.
#'
#' @return A tibble with empirical-logit transformation columns added. The object
#'   has class `gp3_aoi_empirical_logit_data`.
#' @export
transform_gazepoint_aoi_empirical_logit <- function(
    data,
    numerator_col = NULL,
    denominator_col = NULL,
    proportion_col = NULL,
    correction = 0.5,
    pseudo_denominator = 1,
    output_col = "aoi_empirical_logit",
    adjusted_proportion_col = "aoi_proportion_adjusted",
    raw_proportion_col = "aoi_proportion_raw",
    numerator_output_col = "aoi_numerator",
    denominator_output_col = "aoi_denominator",
    status_col = "aoi_empirical_logit_status",
    overwrite = FALSE,
    name = "gazepoint_aoi_empirical_logit"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (is.null(proportion_col) && (is.null(numerator_col) || is.null(denominator_col))) {
    stop(
      "Supply either `proportion_col` or both `numerator_col` and `denominator_col`.",
      call. = FALSE
    )
  }

  if (!is.null(numerator_col)) {
    .gp3_emp_logit_check_col(numerator_col, names(data), "numerator_col")
  }

  if (!is.null(denominator_col)) {
    .gp3_emp_logit_check_col(denominator_col, names(data), "denominator_col")
  }

  if (!is.null(proportion_col)) {
    .gp3_emp_logit_check_col(proportion_col, names(data), "proportion_col")
  }

  .gp3_emp_logit_check_positive_number(correction, "correction")
  .gp3_emp_logit_check_positive_number(pseudo_denominator, "pseudo_denominator")
  .gp3_emp_logit_check_logical(overwrite, "overwrite")
  .gp3_emp_logit_check_label(name, "name")

  output_cols <- c(
    output_col,
    adjusted_proportion_col,
    raw_proportion_col,
    numerator_output_col,
    denominator_output_col,
    status_col
  )

  vapply(
    output_cols,
    .gp3_emp_logit_check_output_name,
    logical(1),
    arg = "output column"
  )

  if (anyDuplicated(output_cols)) {
    stop("Output column names must be unique.", call. = FALSE)
  }

  if (!isTRUE(overwrite)) {
    existing <- intersect(output_cols, names(data))

    if (length(existing) > 0L) {
      stop(
        "Output column(s) already exist in `data`: ",
        paste(existing, collapse = ", "),
        ". Use `overwrite = TRUE` to replace them.",
        call. = FALSE
      )
    }
  }

  numerator <- NULL
  denominator <- NULL
  raw_proportion <- NULL
  denominator_source <- NULL

  if (!is.null(numerator_col) && !is.null(denominator_col)) {
    numerator <- suppressWarnings(as.numeric(data[[numerator_col]]))
    denominator <- suppressWarnings(as.numeric(data[[denominator_col]]))
    raw_proportion <- numerator / denominator
    denominator_source <- "observed_denominator"
  } else if (!is.null(proportion_col) && !is.null(denominator_col)) {
    raw_proportion <- suppressWarnings(as.numeric(data[[proportion_col]]))
    denominator <- suppressWarnings(as.numeric(data[[denominator_col]]))
    numerator <- raw_proportion * denominator
    denominator_source <- "observed_denominator_from_proportion"
  } else {
    raw_proportion <- suppressWarnings(as.numeric(data[[proportion_col]]))
    denominator <- rep(pseudo_denominator, length(raw_proportion))
    numerator <- raw_proportion * denominator
    denominator_source <- "pseudo_denominator_from_proportion"
  }

  non_aoi <- denominator - numerator

  status <- rep("complete", length(raw_proportion))

  status[!is.finite(raw_proportion)] <- "missing_or_nonfinite_proportion"
  status[!is.finite(numerator)] <- "missing_or_nonfinite_numerator"
  status[!is.finite(denominator)] <- "missing_or_nonfinite_denominator"
  status[is.finite(denominator) & denominator <= 0] <- "invalid_denominator"
  status[is.finite(numerator) & numerator < 0] <- "invalid_numerator"
  status[is.finite(non_aoi) & non_aoi < 0] <- "numerator_exceeds_denominator"
  status[is.finite(raw_proportion) & (raw_proportion < 0 | raw_proportion > 1)] <- "proportion_out_of_bounds"

  valid <- status == "complete"

  adjusted_proportion <- rep(NA_real_, length(raw_proportion))
  empirical_logit <- rep(NA_real_, length(raw_proportion))

  adjusted_proportion[valid] <- (numerator[valid] + correction) /
    (denominator[valid] + 2 * correction)

  empirical_logit[valid] <- log(
    (numerator[valid] + correction) /
      (non_aoi[valid] + correction)
  )

  out <- tibble::as_tibble(data)

  out[[raw_proportion_col]] <- raw_proportion
  out[[numerator_output_col]] <- numerator
  out[[denominator_output_col]] <- denominator
  out[[adjusted_proportion_col]] <- adjusted_proportion
  out[[output_col]] <- empirical_logit
  out[[status_col]] <- status

  overview <- tibble::tibble(
    object_name = name,
    transformation = "aoi_empirical_logit",
    numerator_col = .gp3_emp_logit_collapse_nullable(numerator_col),
    denominator_col = .gp3_emp_logit_collapse_nullable(denominator_col),
    proportion_col = .gp3_emp_logit_collapse_nullable(proportion_col),
    denominator_source = denominator_source,
    correction = correction,
    pseudo_denominator = pseudo_denominator,
    n_input_rows = nrow(data),
    n_complete = sum(status == "complete"),
    n_problem_rows = sum(status != "complete"),
    min_raw_proportion = suppressWarnings(min(raw_proportion, na.rm = TRUE)),
    max_raw_proportion = suppressWarnings(max(raw_proportion, na.rm = TRUE)),
    min_empirical_logit = suppressWarnings(min(empirical_logit, na.rm = TRUE)),
    max_empirical_logit = suppressWarnings(max(empirical_logit, na.rm = TRUE))
  )

  overview$min_raw_proportion[!is.finite(overview$min_raw_proportion)] <- NA_real_
  overview$max_raw_proportion[!is.finite(overview$max_raw_proportion)] <- NA_real_
  overview$min_empirical_logit[!is.finite(overview$min_empirical_logit)] <- NA_real_
  overview$max_empirical_logit[!is.finite(overview$max_empirical_logit)] <- NA_real_

  status_summary <- tibble::tibble(
    status = status
  ) |>
    dplyr::count(.data$status, name = "n") |>
    dplyr::arrange(.data$status)

  settings <- tibble::tibble(
    setting = c(
      "numerator_col",
      "denominator_col",
      "proportion_col",
      "correction",
      "pseudo_denominator",
      "output_col",
      "adjusted_proportion_col",
      "raw_proportion_col",
      "numerator_output_col",
      "denominator_output_col",
      "status_col",
      "overwrite",
      "name"
    ),
    value = c(
      .gp3_emp_logit_collapse_nullable(numerator_col),
      .gp3_emp_logit_collapse_nullable(denominator_col),
      .gp3_emp_logit_collapse_nullable(proportion_col),
      as.character(correction),
      as.character(pseudo_denominator),
      output_col,
      adjusted_proportion_col,
      raw_proportion_col,
      numerator_output_col,
      denominator_output_col,
      status_col,
      as.character(overwrite),
      name
    )
  )

  attr(out, "gp3_empirical_logit_overview") <- overview
  attr(out, "gp3_empirical_logit_status_summary") <- status_summary
  attr(out, "gp3_empirical_logit_settings") <- settings

  class(out) <- c("gp3_aoi_empirical_logit_data", class(out))

  out
}

.gp3_emp_logit_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_emp_logit_check_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_emp_logit_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_emp_logit_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_emp_logit_check_output_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("Each ", arg, " must be a non-missing character scalar.", call. = FALSE)
  }

  TRUE
}

.gp3_emp_logit_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
