#' Prepare time-course data for cluster-based permutation tests
#'
#' Prepare sample-level or already binned Gazepoint time-course data for
#' cluster-based permutation testing. The function standardises subject,
#' condition, time-bin, outcome, sample-count, trial-count, and status columns.
#' It can be used for AOI proportions, pupil time-course outcomes, or other
#' continuous time-varying measures.
#'
#' Cluster-based permutation tests are intended for time-course inference.
#' They should not be used to discover a time window and then test that same
#' window again as a confirmatory analysis.
#'
#' @param data A data frame containing sample-level or binned time-course data.
#.
#'
#' @param data A data frame containing sample-level or binned time-course data.
#' @param outcome_col Column containing the outcome to test. For AOI analyses
#'   this is often a 0/1 or logical AOI column. For pupil analyses this is
#'   often a processed pupil column.
#' @param subject_col Subject/participant column.
#' @param condition_col Optional condition column.
#' @param time_col Time column in milliseconds.
#' @param trial_col Optional trial identifier column.
#' @param time_bin_col Optional existing time-bin column. If `NULL`, time bins
#'   are created from `time_col` and `bin_size_ms`.
#' @param conditions Optional character vector of condition levels to keep.
#'   Cluster tests are usually pairwise, so this is typically length 2.
#' @param time_window Optional numeric vector of length 2 giving the time range
#'   to retain, in milliseconds.
#' @param bin_size_ms Bin size in milliseconds when `time_bin_col = NULL`.
#' @param aggregation How to aggregate samples within subject-condition-time
#'   bins. Supported values are `"mean"`, `"proportion"`, `"sum"`, and
#'   `"median"`. `"proportion"` is equivalent to the mean of a numeric/logical
#'   0/1 outcome.
#' @param min_samples_per_bin Minimum number of samples required per
#'   subject-condition-time bin.
#' @param paired Logical. If `TRUE`, retain only subjects with all retained
#'   condition levels.
#' @param drop_invalid Logical. If `TRUE`, rows and bins that are not suitable
#'   for cluster testing are removed.
#' @param missing_condition_label Label used when condition is missing or
#'   `condition_col` is unavailable.
#' @param outcome_label Label stored in the output to identify the outcome.
#'
#' @return A tibble with standardised cluster-test preparation columns.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_cluster_data <- function(
    data,
    outcome_col,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = NULL,
    time_bin_col = NULL,
    conditions = NULL,
    time_window = NULL,
    bin_size_ms = 50,
    aggregation = c("mean", "proportion", "sum", "median"),
    min_samples_per_bin = 1,
    paired = TRUE,
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "outcome"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_logical <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  valid_column(outcome_col, "outcome_col")
  valid_column(subject_col, "subject_col")
  valid_optional_column(condition_col, "condition_col")
  valid_column(time_col, "time_col")
  valid_optional_column(trial_col, "trial_col")
  valid_optional_column(time_bin_col, "time_bin_col")

  valid_logical(paired, "paired")
  valid_logical(drop_invalid, "drop_invalid")

  aggregation <- match.arg(aggregation)

  if (!is.numeric(bin_size_ms) ||
      length(bin_size_ms) != 1L ||
      is.na(bin_size_ms) ||
      !is.finite(bin_size_ms) ||
      bin_size_ms <= 0) {
    stop("`bin_size_ms` must be a positive finite numeric scalar.", call. = FALSE)
  }

  if (!is.numeric(min_samples_per_bin) ||
      length(min_samples_per_bin) != 1L ||
      is.na(min_samples_per_bin) ||
      !is.finite(min_samples_per_bin) ||
      min_samples_per_bin < 1) {
    stop(
      "`min_samples_per_bin` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.null(time_window)) {
    if (!is.numeric(time_window) ||
        length(time_window) != 2L ||
        any(is.na(time_window)) ||
        any(!is.finite(time_window)) ||
        time_window[[1L]] >= time_window[[2L]]) {
      stop(
        "`time_window` must be NULL or a finite numeric vector of length 2 ",
        "with start < end.",
        call. = FALSE
      )
    }
  }

  if (!is.null(conditions)) {
    if (!is.character(conditions) ||
        length(conditions) < 1L ||
        any(is.na(conditions)) ||
        any(!nzchar(conditions))) {
      stop(
        "`conditions` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    conditions <- unique(conditions)
  }

  if (!is.character(missing_condition_label) ||
      length(missing_condition_label) != 1L ||
      is.na(missing_condition_label) ||
      !nzchar(missing_condition_label)) {
    stop(
      "`missing_condition_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(outcome_label) ||
      length(outcome_label) != 1L ||
      is.na(outcome_label) ||
      !nzchar(outcome_label)) {
    stop(
      "`outcome_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  required_cols <- c(outcome_col, subject_col, time_col)

  missing_required_cols <- setdiff(required_cols, names(dat))

  if (length(missing_required_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(time_bin_col) && !time_bin_col %in% names(dat)) {
    stop(
      "Missing required time-bin column: ",
      time_bin_col,
      call. = FALSE
    )
  }

  dat$.gp3_cluster_outcome_raw <- suppressWarnings(
    as.numeric(dat[[outcome_col]])
  )

  if (is.logical(dat[[outcome_col]])) {
    dat$.gp3_cluster_outcome_raw <- as.numeric(dat[[outcome_col]])
  }

  dat$.gp3_cluster_subject <- as.character(dat[[subject_col]])
  dat$.gp3_cluster_subject <- trimws(dat$.gp3_cluster_subject)
  dat$.gp3_cluster_subject[
    is.na(dat$.gp3_cluster_subject) |
      !nzchar(dat$.gp3_cluster_subject)
  ] <- NA_character_

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$.gp3_cluster_condition <- as.character(dat[[condition_col]])
    dat$.gp3_cluster_condition <- trimws(dat$.gp3_cluster_condition)
    dat$.gp3_cluster_condition[
      is.na(dat$.gp3_cluster_condition) |
        !nzchar(dat$.gp3_cluster_condition)
    ] <- missing_condition_label
  } else {
    dat$.gp3_cluster_condition <- missing_condition_label
  }

  dat$.gp3_cluster_time <- suppressWarnings(as.numeric(dat[[time_col]]))

  if (!is.null(time_bin_col)) {
    dat$.gp3_cluster_time_bin <- suppressWarnings(
      as.numeric(dat[[time_bin_col]])
    )
  } else {
    dat$.gp3_cluster_time_bin <-
      floor(dat$.gp3_cluster_time / bin_size_ms) * bin_size_ms
  }

  if (!is.null(trial_col) && trial_col %in% names(dat)) {
    dat$.gp3_cluster_trial <- as.character(dat[[trial_col]])
    dat$.gp3_cluster_trial <- trimws(dat$.gp3_cluster_trial)
    dat$.gp3_cluster_trial[
      is.na(dat$.gp3_cluster_trial) |
        !nzchar(dat$.gp3_cluster_trial)
    ] <- NA_character_
    trial_available <- TRUE
  } else {
    dat$.gp3_cluster_trial <- NA_character_
    trial_available <- FALSE
  }

  if (!is.null(time_window)) {
    dat <- dat[
      is.finite(dat$.gp3_cluster_time) &
        dat$.gp3_cluster_time >= time_window[[1L]] &
        dat$.gp3_cluster_time <= time_window[[2L]],
      ,
      drop = FALSE
    ]
  }

  if (!is.null(conditions)) {
    dat <- dat[
      dat$.gp3_cluster_condition %in% conditions,
      ,
      drop = FALSE
    ]
  }

  dat$.gp3_cluster_row_status <- dplyr::case_when(
    is.na(dat$.gp3_cluster_subject) ~ "missing_subject",
    is.na(dat$.gp3_cluster_condition) ~ "missing_condition",
    is.na(dat$.gp3_cluster_outcome_raw) ~ "missing_outcome",
    !is.finite(dat$.gp3_cluster_outcome_raw) ~ "non_finite_outcome",
    is.na(dat$.gp3_cluster_time) ~ "missing_time",
    !is.finite(dat$.gp3_cluster_time) ~ "non_finite_time",
    is.na(dat$.gp3_cluster_time_bin) ~ "missing_time_bin",
    !is.finite(dat$.gp3_cluster_time_bin) ~ "non_finite_time_bin",
    TRUE ~ "ok"
  )

  if (drop_invalid) {
    dat <- dat[
      dat$.gp3_cluster_row_status == "ok",
      ,
      drop = FALSE
    ]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows are available after preparing cluster-test input rows.",
      call. = FALSE
    )
  }

  condition_levels <- if (!is.null(conditions)) {
    conditions
  } else {
    unique(dat$.gp3_cluster_condition)
  }

  dat$.gp3_cluster_condition <- factor(
    dat$.gp3_cluster_condition,
    levels = condition_levels
  )

  dat$.gp3_cluster_subject <- factor(dat$.gp3_cluster_subject)

  observed_condition_levels <- levels(droplevels(dat$.gp3_cluster_condition))
  n_condition_levels <- length(observed_condition_levels)

  if (paired && n_condition_levels > 1L) {
    paired_subjects <- dat |>
      dplyr::distinct(
        .data[[".gp3_cluster_subject"]],
        .data[[".gp3_cluster_condition"]]
      ) |>
      dplyr::count(
        .data[[".gp3_cluster_subject"]],
        name = ".gp3_n_conditions"
      ) |>
      dplyr::filter(.data[[".gp3_n_conditions"]] == n_condition_levels) |>
      dplyr::pull(.data[[".gp3_cluster_subject"]])

    dat <- dat[
      dat$.gp3_cluster_subject %in% paired_subjects,
      ,
      drop = FALSE
    ]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows remain after applying paired-subject filtering.",
      call. = FALSE
    )
  }

  summarise_outcome <- function(x) {
    if (aggregation %in% c("mean", "proportion")) {
      mean(x, na.rm = TRUE)
    } else if (aggregation == "sum") {
      sum(x, na.rm = TRUE)
    } else if (aggregation == "median") {
      stats::median(x, na.rm = TRUE)
    } else {
      NA_real_
    }
  }

  cluster_data <- dat |>
    dplyr::group_by(
      .data[[".gp3_cluster_subject"]],
      .data[[".gp3_cluster_condition"]],
      .data[[".gp3_cluster_time_bin"]]
    ) |>
    dplyr::summarise(
      .gp3_cluster_outcome = summarise_outcome(
        .data[[".gp3_cluster_outcome_raw"]]
      ),
      .gp3_cluster_n_samples = dplyr::n(),
      .gp3_cluster_n_trials = if (trial_available) {
        dplyr::n_distinct(
          .data[[".gp3_cluster_trial"]][
            !is.na(.data[[".gp3_cluster_trial"]])
          ]
        )
      } else {
        NA_integer_
      },
      .groups = "drop"
    )

  cluster_data$.gp3_cluster_status <- dplyr::case_when(
    is.na(cluster_data$.gp3_cluster_outcome) ~ "missing_aggregated_outcome",
    !is.finite(cluster_data$.gp3_cluster_outcome) ~
      "non_finite_aggregated_outcome",
    cluster_data$.gp3_cluster_n_samples < min_samples_per_bin ~ "low_samples",
    TRUE ~ "ok"
  )

  if (drop_invalid) {
    cluster_data <- cluster_data[
      cluster_data$.gp3_cluster_status == "ok",
      ,
      drop = FALSE
    ]
  }

  if (nrow(cluster_data) == 0L) {
    stop(
      "No bins remain after preparing cluster-test data.",
      call. = FALSE
    )
  }

  cluster_data$.gp3_cluster_subject <- droplevels(
    factor(cluster_data$.gp3_cluster_subject)
  )

  cluster_data$.gp3_cluster_condition <- factor(
    cluster_data$.gp3_cluster_condition,
    levels = condition_levels
  )

  cluster_data <- cluster_data |>
    dplyr::arrange(
      .data[[".gp3_cluster_subject"]],
      .data[[".gp3_cluster_condition"]],
      .data[[".gp3_cluster_time_bin"]]
    )

  n_subjects <- dplyr::n_distinct(cluster_data$.gp3_cluster_subject)
  n_conditions <- dplyr::n_distinct(cluster_data$.gp3_cluster_condition)
  n_time_bins <- dplyr::n_distinct(cluster_data$.gp3_cluster_time_bin)

  cluster_data$.gp3_cluster_outcome_col <- outcome_col
  cluster_data$.gp3_cluster_outcome_label <- outcome_label
  cluster_data$.gp3_cluster_aggregation <- aggregation
  cluster_data$.gp3_cluster_bin_size_ms <- bin_size_ms
  cluster_data$.gp3_cluster_paired <- paired
  cluster_data$.gp3_cluster_condition_status <- dplyr::case_when(
    n_conditions < 2L ~ "less_than_two_conditions",
    n_conditions == 2L ~ "two_conditions",
    TRUE ~ "more_than_two_conditions"
  )

  class(cluster_data) <- c("gp3_cluster_data", class(cluster_data))

  attr(cluster_data, "settings") <- list(
    outcome_col = outcome_col,
    subject_col = subject_col,
    condition_col = condition_col,
    time_col = time_col,
    trial_col = trial_col,
    time_bin_col = time_bin_col,
    conditions = conditions,
    time_window = time_window,
    bin_size_ms = bin_size_ms,
    aggregation = aggregation,
    min_samples_per_bin = min_samples_per_bin,
    paired = paired,
    drop_invalid = drop_invalid,
    missing_condition_label = missing_condition_label,
    outcome_label = outcome_label
  )

  attr(cluster_data, "summary") <- list(
    n_rows = nrow(cluster_data),
    n_subjects = n_subjects,
    n_conditions = n_conditions,
    n_time_bins = n_time_bins,
    condition_levels = levels(droplevels(cluster_data$.gp3_cluster_condition)),
    condition_status = unique(cluster_data$.gp3_cluster_condition_status)
  )

  cluster_data
}
