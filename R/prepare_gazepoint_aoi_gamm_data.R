#' Prepare AOI time-course data for GAMM analysis
#'
#' Prepare sample-level or binned Gazepoint AOI data for AOI time-course GAMM
#' analysis. The function creates binned subject-by-condition-by-time summaries
#' with binomial success/failure counts for target-AOI looking over time.
#'
#' This helper is intended for modelling AOI time-course trajectories, such as
#' target-AOI looking probability over time. It is separate from confirmatory
#' AOI-window GLMMs and from cluster-based permutation tests.
#'
#' @param data A data frame containing sample-level or binned AOI data.
#' @param aoi_col Name of the AOI-state column. Used when `outcome_col = NULL`.
#' @param target_aoi_values Character vector identifying target AOI values.
#'   Required when `outcome_col = NULL`.
#' @param outcome_col Optional logical or 0/1 numeric column indicating target
#'   AOI looking at the sample level. If supplied, this takes priority over
#'   `aoi_col` and `target_aoi_values`.
#' @param subject_col Name of the subject/participant column.
#' @param condition_col Name of the condition column. If unavailable, a single
#'   fallback condition is created.
#' @param time_col Name of the time column, in milliseconds.
#' @param trial_col Optional trial identifier column.
#' @param time_bin_col Optional existing time-bin column. If `NULL`, time bins
#'   are created from `time_col` using `bin_size_ms`.
#' @param conditions Optional character vector of condition levels to retain
#'   and order.
#' @param time_window Optional numeric vector of length 2 defining the retained
#'   time window in milliseconds.
#' @param bin_size_ms Time-bin width in milliseconds when `time_bin_col = NULL`.
#' @param denominator Denominator definition. `"valid"` uses non-missing AOI
#'   states, `"all"` uses all retained rows, and `"aoi_only"` uses only explicit
#'   AOI states.
#' @param valid_aoi_values Optional character vector defining explicit AOI
#'   values for `"aoi_only"` denominators. If `NULL`, values beginning with
#'   `"AOI"` are treated as explicit AOIs, excluding `non_aoi_values`.
#' @param non_aoi_values Character vector identifying non-AOI/background states.
#' @param missing_aoi_values Character vector identifying missing AOI-state
#'   labels.
#' @param min_denominator_samples Minimum number of denominator samples required
#'   per subject-condition-time bin.
#' @param drop_invalid Logical. If `TRUE`, bins with zero or low denominators
#'   are removed from the returned data.
#' @param missing_condition_label Fallback condition label when no usable
#'   condition column is available.
#' @param outcome_label Descriptive label for the AOI-GAMM outcome.
#'
#' @return A tibble with standardised AOI-GAMM columns.
#'
#' @export
#' @importFrom rlang .data
prepare_gazepoint_aoi_gamm_data <- function(
    data,
    aoi_col = "aoi_current",
    target_aoi_values = NULL,
    outcome_col = NULL,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time",
    trial_col = NULL,
    time_bin_col = NULL,
    conditions = NULL,
    time_window = NULL,
    bin_size_ms = 50,
    denominator = c("valid", "all", "aoi_only"),
    valid_aoi_values = NULL,
    non_aoi_values = c("non_aoi"),
    missing_aoi_values = c("missing", ""),
    min_denominator_samples = 1,
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "target_aoi"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  check_col <- function(x, arg, allow_null = FALSE) {
    if (is.null(x) && allow_null) {
      return(invisible(TRUE))
    }


    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop("`", arg, "` must be a non-missing character scalar.",
           call. = FALSE)
    }

    invisible(TRUE)


  }

  check_col(aoi_col, "aoi_col")
  check_col(outcome_col, "outcome_col", allow_null = TRUE)
  check_col(subject_col, "subject_col")
  check_col(condition_col, "condition_col")
  check_col(time_col, "time_col")
  check_col(trial_col, "trial_col", allow_null = TRUE)
  check_col(time_bin_col, "time_bin_col", allow_null = TRUE)
  check_col(missing_condition_label, "missing_condition_label")
  check_col(outcome_label, "outcome_label")

  denominator <- match.arg(denominator)

  if (is.null(outcome_col)) {
    if (is.null(target_aoi_values) ||
        !is.character(target_aoi_values) ||
        length(target_aoi_values) == 0L ||
        any(is.na(target_aoi_values)) ||
        any(!nzchar(target_aoi_values))) {
      stop(
        "`target_aoi_values` must be a non-empty character vector when ",
        "`outcome_col = NULL`.",
        call. = FALSE
      )
    }
  }

  if (!is.null(conditions)) {
    if (!is.character(conditions) ||
        length(conditions) == 0L ||
        any(is.na(conditions)) ||
        any(!nzchar(conditions))) {
      stop(
        "`conditions` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    conditions <- unique(conditions)


  }

  if (!is.null(valid_aoi_values)) {
    if (!is.character(valid_aoi_values) ||
        length(valid_aoi_values) == 0L ||
        any(is.na(valid_aoi_values)) ||
        any(!nzchar(valid_aoi_values))) {
      stop(
        "`valid_aoi_values` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }


    valid_aoi_values <- unique(valid_aoi_values)


  }

  if (!is.character(non_aoi_values)) {
    stop("`non_aoi_values` must be a character vector.", call. = FALSE)
  }

  if (!is.character(missing_aoi_values)) {
    stop("`missing_aoi_values` must be a character vector.", call. = FALSE)
  }

  if (!is.numeric(bin_size_ms) ||
      length(bin_size_ms) != 1L ||
      is.na(bin_size_ms) ||
      !is.finite(bin_size_ms) ||
      bin_size_ms <= 0) {
    stop("`bin_size_ms` must be a positive finite numeric scalar.",
         call. = FALSE)
  }

  if (!is.numeric(min_denominator_samples) ||
      length(min_denominator_samples) != 1L ||
      is.na(min_denominator_samples) ||
      !is.finite(min_denominator_samples) ||
      min_denominator_samples < 1) {
    stop(
      "`min_denominator_samples` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  min_denominator_samples <- as.integer(min_denominator_samples)

  if (!is.logical(drop_invalid) ||
      length(drop_invalid) != 1L ||
      is.na(drop_invalid)) {
    stop("`drop_invalid` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(time_window)) {
    if (!is.numeric(time_window) ||
        length(time_window) != 2L ||
        any(is.na(time_window)) ||
        any(!is.finite(time_window)) ||
        time_window[[1L]] > time_window[[2L]]) {
      stop(
        "`time_window` must be NULL or a finite numeric vector of length 2.",
        call. = FALSE
      )
    }
  }

  required_cols <- c(subject_col, time_col)

  if (is.null(outcome_col)) {
    required_cols <- c(required_cols, aoi_col)
  } else {
    required_cols <- c(required_cols, outcome_col)
  }

  if (!is.null(trial_col)) {
    required_cols <- c(required_cols, trial_col)
  }

  if (!is.null(time_bin_col)) {
    required_cols <- c(required_cols, time_bin_col)
  }

  required_cols <- unique(required_cols)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  dat$.gp3_aoi_gamm_subject <- as.character(dat[[subject_col]])
  dat$.gp3_aoi_gamm_time <- suppressWarnings(as.numeric(dat[[time_col]]))

  if (!is.null(time_bin_col)) {
    dat$.gp3_aoi_gamm_time_bin <-
      suppressWarnings(as.numeric(dat[[time_bin_col]]))
  } else {
    dat$.gp3_aoi_gamm_time_bin <-
      floor(dat$.gp3_aoi_gamm_time / bin_size_ms) * bin_size_ms
  }

  if (!is.null(trial_col)) {
    dat$.gp3_aoi_gamm_trial <- as.character(dat[[trial_col]])
  } else {
    dat$.gp3_aoi_gamm_trial <- NA_character_
  }

  if (condition_col %in% names(dat)) {
    condition_values <- as.character(dat[[condition_col]])
    condition_values[is.na(condition_values)] <- missing_condition_label
    condition_values[!nzchar(trimws(condition_values))] <-
      missing_condition_label
    dat$.gp3_aoi_gamm_condition <- condition_values
  } else {
    dat$.gp3_aoi_gamm_condition <- missing_condition_label
  }

  if (!is.null(conditions)) {
    dat <- dat[
      dat$.gp3_aoi_gamm_condition %in% conditions,
      ,
      drop = FALSE
    ]
  }

  if (!is.null(time_window)) {
    dat <- dat[
      is.finite(dat$.gp3_aoi_gamm_time) &
        dat$.gp3_aoi_gamm_time >= time_window[[1L]] &
        dat$.gp3_aoi_gamm_time <= time_window[[2L]],
      ,
      drop = FALSE
    ]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No rows are available after applying condition/time filters.",
      call. = FALSE
    )
  }

  missing_subject <- is.na(dat$.gp3_aoi_gamm_subject) |
    !nzchar(trimws(dat$.gp3_aoi_gamm_subject))

  missing_time <- is.na(dat$.gp3_aoi_gamm_time)
  non_finite_time <- !is.na(dat$.gp3_aoi_gamm_time) &
    !is.finite(dat$.gp3_aoi_gamm_time)

  missing_time_bin <- is.na(dat$.gp3_aoi_gamm_time_bin)
  non_finite_time_bin <- !is.na(dat$.gp3_aoi_gamm_time_bin) &
    !is.finite(dat$.gp3_aoi_gamm_time_bin)

  if (!is.null(outcome_col)) {
    outcome_raw <- dat[[outcome_col]]


    if (is.logical(outcome_raw)) {
      outcome_num <- as.numeric(outcome_raw)
    } else {
      outcome_num <- suppressWarnings(as.numeric(outcome_raw))
    }

    missing_outcome <- is.na(outcome_num)
    non_finite_outcome <- !is.na(outcome_num) & !is.finite(outcome_num)

    non_binary_outcome <- is.finite(outcome_num) &
      !(outcome_num %in% c(0, 1))

    dat$.gp3_aoi_gamm_target_sample <- outcome_num == 1
    dat$.gp3_aoi_gamm_denominator_sample <-
      !missing_outcome & !non_finite_outcome & !non_binary_outcome

    dat$.gp3_aoi_gamm_aoi_value <- NA_character_
    dat$.gp3_aoi_gamm_source <- outcome_col

    row_status <- dplyr::case_when(
      missing_subject ~ "missing_subject",
      missing_time ~ "missing_time",
      non_finite_time ~ "non_finite_time",
      missing_time_bin ~ "missing_time_bin",
      non_finite_time_bin ~ "non_finite_time_bin",
      missing_outcome ~ "missing_outcome",
      non_finite_outcome ~ "non_finite_outcome",
      non_binary_outcome ~ "non_binary_outcome",
      TRUE ~ "ok"
    )


  } else {
    aoi_value <- as.character(dat[[aoi_col]])


    missing_aoi <- is.na(aoi_value) |
      !nzchar(trimws(aoi_value)) |
      aoi_value %in% missing_aoi_values

    target_sample <- !missing_aoi & aoi_value %in% target_aoi_values

    if (denominator == "all") {
      denominator_sample <- rep(TRUE, nrow(dat))
    } else if (denominator == "valid") {
      denominator_sample <- !missing_aoi
    } else {
      if (!is.null(valid_aoi_values)) {
        denominator_sample <- !missing_aoi & aoi_value %in% valid_aoi_values
      } else {
        denominator_sample <- !missing_aoi &
          grepl("^AOI", aoi_value) &
          !(aoi_value %in% non_aoi_values)
      }
    }

    dat$.gp3_aoi_gamm_target_sample <- target_sample
    dat$.gp3_aoi_gamm_denominator_sample <- denominator_sample
    dat$.gp3_aoi_gamm_aoi_value <- aoi_value
    dat$.gp3_aoi_gamm_source <- aoi_col

    row_status <- dplyr::case_when(
      missing_subject ~ "missing_subject",
      missing_time ~ "missing_time",
      non_finite_time ~ "non_finite_time",
      missing_time_bin ~ "missing_time_bin",
      non_finite_time_bin ~ "non_finite_time_bin",
      missing_aoi ~ "missing_aoi",
      TRUE ~ "ok"
    )


  }

  dat$.gp3_aoi_gamm_row_status <- row_status

  core_valid <- dat$.gp3_aoi_gamm_row_status %in% c("ok", "missing_aoi")

  dat <- dat[
    core_valid,
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop(
      "No rows are available after preparing AOI-GAMM input rows.",
      call. = FALSE
    )
  }

  if (!is.null(conditions)) {
    condition_levels <- conditions
  } else {
    condition_levels <- sort(unique(dat$.gp3_aoi_gamm_condition))
  }

  dat$.gp3_aoi_gamm_subject <- factor(dat$.gp3_aoi_gamm_subject)
  dat$.gp3_aoi_gamm_condition <- factor(
    dat$.gp3_aoi_gamm_condition,
    levels = condition_levels
  )

  gamm_data <- dat |>
    dplyr::group_by(
      .data[[".gp3_aoi_gamm_subject"]],
      .data[[".gp3_aoi_gamm_condition"]],
      .data[[".gp3_aoi_gamm_time_bin"]]
    ) |>
    dplyr::summarise(
      .gp3_aoi_gamm_success = sum(
        .data[[".gp3_aoi_gamm_target_sample"]] &
          .data[[".gp3_aoi_gamm_denominator_sample"]],
        na.rm = TRUE
      ),
      .gp3_aoi_gamm_denominator = sum(
        .data[[".gp3_aoi_gamm_denominator_sample"]],
        na.rm = TRUE
      ),
      .gp3_aoi_gamm_n_samples = dplyr::n(),
      .gp3_aoi_gamm_n_trials = dplyr::n_distinct(
        .data[[".gp3_aoi_gamm_trial"]][
          !is.na(.data[[".gp3_aoi_gamm_trial"]])
        ]
      ),
      .groups = "drop"
    )

  gamm_data$.gp3_aoi_gamm_failure <-
    gamm_data$.gp3_aoi_gamm_denominator -
    gamm_data$.gp3_aoi_gamm_success

  gamm_data$.gp3_aoi_gamm_proportion <- dplyr::if_else(
    gamm_data$.gp3_aoi_gamm_denominator > 0,
    gamm_data$.gp3_aoi_gamm_success /
      gamm_data$.gp3_aoi_gamm_denominator,
    NA_real_
  )

  gamm_data$.gp3_aoi_gamm_weight <- gamm_data$.gp3_aoi_gamm_denominator

  gamm_data$.gp3_aoi_gamm_status <- dplyr::case_when(
    gamm_data$.gp3_aoi_gamm_denominator <= 0 ~ "zero_denominator",
    gamm_data$.gp3_aoi_gamm_denominator < min_denominator_samples ~
      "low_denominator",
    TRUE ~ "ok"
  )

  if (drop_invalid) {
    gamm_data <- gamm_data[
      gamm_data$.gp3_aoi_gamm_status == "ok",
      ,
      drop = FALSE
    ]
  }

  if (nrow(gamm_data) == 0L) {
    stop(
      "No AOI-GAMM bins are available after denominator filtering.",
      call. = FALSE
    )
  }

  condition_n <- dplyr::n_distinct(gamm_data$.gp3_aoi_gamm_condition)

  condition_status <- dplyr::case_when(
    condition_n < 2L ~ "less_than_two_conditions",
    condition_n == 2L ~ "two_conditions",
    TRUE ~ "more_than_two_conditions"
  )

  gamm_data$.gp3_aoi_gamm_condition_status <- condition_status
  gamm_data$.gp3_aoi_gamm_outcome_label <- outcome_label
  gamm_data$.gp3_aoi_gamm_denominator_type <- denominator
  gamm_data$.gp3_aoi_gamm_bin_size_ms <- bin_size_ms
  target_label <- if (!is.null(target_aoi_values)) {
    paste(target_aoi_values, collapse = ", ")
  } else {
    outcome_label
  }

  gamm_data$.gp3_aoi_gamm_target_label <- target_label

  gamm_data <- gamm_data[
    ,
    c(
      ".gp3_aoi_gamm_subject",
      ".gp3_aoi_gamm_condition",
      ".gp3_aoi_gamm_time_bin",
      ".gp3_aoi_gamm_success",
      ".gp3_aoi_gamm_failure",
      ".gp3_aoi_gamm_denominator",
      ".gp3_aoi_gamm_proportion",
      ".gp3_aoi_gamm_weight",
      ".gp3_aoi_gamm_n_samples",
      ".gp3_aoi_gamm_n_trials",
      ".gp3_aoi_gamm_status",
      ".gp3_aoi_gamm_condition_status",
      ".gp3_aoi_gamm_outcome_label",
      ".gp3_aoi_gamm_denominator_type",
      ".gp3_aoi_gamm_bin_size_ms",
      ".gp3_aoi_gamm_target_label"
    ),
    drop = FALSE
  ]

  gamm_data <- tibble::as_tibble(gamm_data)

  settings <- list(
    aoi_col = aoi_col,
    target_aoi_values = target_aoi_values,
    outcome_col = outcome_col,
    subject_col = subject_col,
    condition_col = condition_col,
    time_col = time_col,
    trial_col = trial_col,
    time_bin_col = time_bin_col,
    conditions = conditions,
    time_window = time_window,
    bin_size_ms = bin_size_ms,
    denominator = denominator,
    valid_aoi_values = valid_aoi_values,
    non_aoi_values = non_aoi_values,
    missing_aoi_values = missing_aoi_values,
    min_denominator_samples = min_denominator_samples,
    drop_invalid = drop_invalid,
    missing_condition_label = missing_condition_label,
    outcome_label = outcome_label
  )

  summary <- list(
    n_rows = nrow(gamm_data),
    n_subjects = dplyr::n_distinct(gamm_data$.gp3_aoi_gamm_subject),
    n_conditions = dplyr::n_distinct(gamm_data$.gp3_aoi_gamm_condition),
    n_time_bins = dplyr::n_distinct(gamm_data$.gp3_aoi_gamm_time_bin),
    condition_levels = levels(gamm_data$.gp3_aoi_gamm_condition),
    condition_status = condition_status,
    denominator_type = denominator,
    outcome_label = outcome_label
  )

  attr(gamm_data, "settings") <- settings
  attr(gamm_data, "summary") <- summary

  class(gamm_data) <- c("gp3_aoi_gamm_data", class(gamm_data))

  gamm_data
}
