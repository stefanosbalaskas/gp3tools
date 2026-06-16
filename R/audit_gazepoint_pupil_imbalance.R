#' Audit Gazepoint pupil preprocessing imbalance
#'
#' Check whether pupil preprocessing loss differs across experimental
#' conditions or other grouping variables.
#'
#' The function summarises valid pupil samples, interpolated samples,
#' artifact-flagged samples, and remaining missing samples. It also adds
#' simple imbalance flags based on differences between groups.
#'
#' @param data A data frame from a pupil preprocessing pipeline.
#' @param group_cols Character vector of grouping columns. By default,
#'   summaries are produced by `condition`.
#' @param pupil_col Name of the post-preprocessing pupil column used to define
#'   remaining valid and missing samples.
#' @param interpolated_col Name of the logical interpolation flag column.
#' @param interpolation_status_col Name of the interpolation-status column.
#' @param artifact_col Optional artifact flag column. If `NULL`, the function
#'   tries to detect `pupil_artifact_flag`, `pupil_flag_invalid`, or
#'   `artifact_flag`.
#' @param artifact_reason_col Optional artifact-reason column. If `NULL`, the
#'   function tries to detect `pupil_artifact_reason`, `pupil_flag_reason`, or
#'   `artifact_reason`.
#' @param min_group_n Minimum group size below which a group is flagged.
#' @param max_valid_pct_diff Maximum acceptable range in valid-sample
#'   percentage across groups.
#' @param max_artifact_pct_diff Maximum acceptable range in artifact percentage
#'   across groups.
#' @param max_missing_pct_diff Maximum acceptable range in remaining-missing
#'   percentage across groups.
#' @param max_interpolated_pct_diff Maximum acceptable range in interpolated
#'   percentage across groups.
#'
#' @return A tibble with one row per group and imbalance-warning columns.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_pupil_imbalance <- function(
    data,
    group_cols = "condition",
    pupil_col = "pupil_interpolated",
    interpolated_col = "pupil_was_interpolated",
    interpolation_status_col = "pupil_interpolation_status",
    artifact_col = NULL,
    artifact_reason_col = NULL,
    min_group_n = 1,
    max_valid_pct_diff = 10,
    max_artifact_pct_diff = 10,
    max_missing_pct_diff = 10,
    max_interpolated_pct_diff = 10
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(group_cols) ||
      any(is.na(group_cols)) ||
      any(!nzchar(group_cols)) ||
      anyDuplicated(group_cols)) {
    stop(
      "`group_cols` must be a character vector of unique column names.",
      call. = FALSE
    )
  }

  scalar_column_args <- c(
    pupil_col = pupil_col,
    interpolated_col = interpolated_col,
    interpolation_status_col = interpolation_status_col
  )

  valid_scalar_column_arg <- vapply(
    scalar_column_args,
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )

  if (any(!valid_scalar_column_arg)) {
    stop(
      "Column-name arguments must be non-missing character scalars: ",
      paste(names(scalar_column_args)[!valid_scalar_column_arg], collapse = ", "),
      call. = FALSE
    )
  }

  optional_column_args <- c(
    artifact_col = artifact_col,
    artifact_reason_col = artifact_reason_col
  )

  supplied_optional_args <- !vapply(
    optional_column_args,
    is.null,
    logical(1)
  )

  valid_optional_arg <- vapply(
    optional_column_args[supplied_optional_args],
    function(x) {
      is.character(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )

  if (length(valid_optional_arg) > 0L && any(!valid_optional_arg)) {
    stop(
      "Optional column-name arguments must be NULL or non-missing character scalars: ",
      paste(names(valid_optional_arg)[!valid_optional_arg], collapse = ", "),
      call. = FALSE
    )
  }

  numeric_args <- c(
    min_group_n = min_group_n,
    max_valid_pct_diff = max_valid_pct_diff,
    max_artifact_pct_diff = max_artifact_pct_diff,
    max_missing_pct_diff = max_missing_pct_diff,
    max_interpolated_pct_diff = max_interpolated_pct_diff
  )

  valid_numeric_arg <- vapply(
    numeric_args,
    function(x) {
      is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x)
    },
    logical(1)
  )

  if (any(!valid_numeric_arg)) {
    stop(
      "Threshold arguments must be finite numeric scalars: ",
      paste(names(numeric_args)[!valid_numeric_arg], collapse = ", "),
      call. = FALSE
    )
  }

  auto_detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0L) {
      return(NULL)
    }

    found[[1]]
  }

  if (is.null(artifact_col)) {
    artifact_col <- auto_detect_col(
      c("pupil_artifact_flag", "pupil_flag_invalid", "artifact_flag")
    )
  }

  if (is.null(artifact_reason_col)) {
    artifact_reason_col <- auto_detect_col(
      c("pupil_artifact_reason", "pupil_flag_reason", "artifact_reason")
    )
  }

  required_cols <- unique(c(
    group_cols,
    unname(scalar_column_args)
  ))

  if (!is.null(artifact_col)) {
    required_cols <- unique(c(required_cols, artifact_col))
  }

  if (!is.null(artifact_reason_col)) {
    required_cols <- unique(c(required_cols, artifact_reason_col))
  }

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  as_logical_flag <- function(x) {
    if (is.logical(x)) {
      return(dplyr::coalesce(x, FALSE))
    }

    if (is.numeric(x) || is.integer(x)) {
      return(!is.na(x) & x != 0)
    }

    x_chr <- tolower(trimws(as.character(x)))

    x_chr %in% c("true", "t", "1", "yes", "y")
  }

  range_or_na <- function(x) {
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x) - min(x)
  }

  group_data <- function(x) {
    if (length(group_cols) > 0L) {
      dplyr::group_by(x, dplyr::across(dplyr::all_of(group_cols)))
    } else {
      x
    }
  }

  working <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_imbalance_pupil = .data[[pupil_col]],
      .gp3_imbalance_interpolated =
        as_logical_flag(.data[[interpolated_col]]),
      .gp3_imbalance_status =
        as.character(.data[[interpolation_status_col]])
    )

  if (!is.null(artifact_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_imbalance_artifact =
          as_logical_flag(.data[[artifact_col]])
      )
  } else if (!is.null(artifact_reason_col)) {
    working <- working |>
      dplyr::mutate(
        .gp3_imbalance_artifact =
          !is.na(.data[[artifact_reason_col]]) &
          as.character(.data[[artifact_reason_col]]) != "valid" &
          as.character(.data[[artifact_reason_col]]) != ""
      )
  } else {
    working <- working |>
      dplyr::mutate(
        .gp3_imbalance_artifact = FALSE
      )
  }

  out <- working |>
    group_data() |>
    dplyr::summarise(
      n_rows = dplyr::n(),

      n_valid_samples = sum(
        !is.na(.data[[".gp3_imbalance_pupil"]]),
        na.rm = TRUE
      ),

      n_interpolated_samples = sum(
        .data[[".gp3_imbalance_interpolated"]],
        na.rm = TRUE
      ),

      n_artifact_samples = sum(
        .data[[".gp3_imbalance_artifact"]],
        na.rm = TRUE
      ),

      n_remaining_missing_samples = sum(
        is.na(.data[[".gp3_imbalance_pupil"]]),
        na.rm = TRUE
      ),

      n_observed_samples = sum(
        .data[[".gp3_imbalance_status"]] == "observed",
        na.rm = TRUE
      ),

      n_missing_edge_gap_samples = sum(
        .data[[".gp3_imbalance_status"]] == "missing_edge_gap",
        na.rm = TRUE
      ),

      n_missing_long_gap_samples = sum(
        .data[[".gp3_imbalance_status"]] == "missing_long_gap",
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      valid_sample_pct = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_valid_samples / .data$n_rows,
        NA_real_
      ),

      interpolated_sample_pct = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_interpolated_samples / .data$n_rows,
        NA_real_
      ),

      artifact_sample_pct = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_artifact_samples / .data$n_rows,
        NA_real_
      ),

      remaining_missing_sample_pct = dplyr::if_else(
        .data$n_rows > 0L,
        100 * .data$n_remaining_missing_samples / .data$n_rows,
        NA_real_
      )
    )

  global_valid_pct_range <- range_or_na(out$valid_sample_pct)
  global_artifact_pct_range <- range_or_na(out$artifact_sample_pct)
  global_missing_pct_range <- range_or_na(out$remaining_missing_sample_pct)
  global_interpolated_pct_range <- range_or_na(out$interpolated_sample_pct)

  imbalance_warning <- dplyr::coalesce(
    global_valid_pct_range > max_valid_pct_diff,
    FALSE
  ) |
    dplyr::coalesce(
      global_artifact_pct_range > max_artifact_pct_diff,
      FALSE
    ) |
    dplyr::coalesce(
      global_missing_pct_range > max_missing_pct_diff,
      FALSE
    ) |
    dplyr::coalesce(
      global_interpolated_pct_range > max_interpolated_pct_diff,
      FALSE
    ) |
    any(out$n_rows < min_group_n, na.rm = TRUE)

  imbalance_reason <- c()

  if (isTRUE(global_valid_pct_range > max_valid_pct_diff)) {
    imbalance_reason <- c(imbalance_reason, "valid_pct_diff")
  }

  if (isTRUE(global_artifact_pct_range > max_artifact_pct_diff)) {
    imbalance_reason <- c(imbalance_reason, "artifact_pct_diff")
  }

  if (isTRUE(global_missing_pct_range > max_missing_pct_diff)) {
    imbalance_reason <- c(imbalance_reason, "missing_pct_diff")
  }

  if (isTRUE(global_interpolated_pct_range > max_interpolated_pct_diff)) {
    imbalance_reason <- c(imbalance_reason, "interpolated_pct_diff")
  }

  if (any(out$n_rows < min_group_n, na.rm = TRUE)) {
    imbalance_reason <- c(imbalance_reason, "small_group_n")
  }

  if (length(imbalance_reason) == 0L) {
    imbalance_reason <- "ok"
  } else {
    imbalance_reason <- paste(imbalance_reason, collapse = ";")
  }

  output_cols <- c(
    "n_rows",
    "n_valid_samples",
    "n_interpolated_samples",
    "n_artifact_samples",
    "n_remaining_missing_samples",
    "n_observed_samples",
    "n_missing_edge_gap_samples",
    "n_missing_long_gap_samples",
    "valid_sample_pct",
    "interpolated_sample_pct",
    "artifact_sample_pct",
    "remaining_missing_sample_pct"
  )

  out |>
    dplyr::mutate(
      valid_sample_pct_range = global_valid_pct_range,
      artifact_sample_pct_range = global_artifact_pct_range,
      remaining_missing_sample_pct_range = global_missing_pct_range,
      interpolated_sample_pct_range = global_interpolated_pct_range,
      preprocessing_imbalance_warning = imbalance_warning,
      preprocessing_imbalance_reason = imbalance_reason
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          group_cols,
          output_cols,
          "valid_sample_pct_range",
          "artifact_sample_pct_range",
          "remaining_missing_sample_pct_range",
          "interpolated_sample_pct_range",
          "preprocessing_imbalance_warning",
          "preprocessing_imbalance_reason"
        )
      )
    )
}
