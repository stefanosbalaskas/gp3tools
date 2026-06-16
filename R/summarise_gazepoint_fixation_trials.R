#' Summarise Gazepoint fixation trial features
#'
#' Create trial-level fixation features from Gazepoint fixation-level data. The
#' function supports common Gazepoint fixation export columns such as `FPOGS`,
#' `FPOGD`, `FPOGX`, `FPOGY`, `FPOGID`, `FPOGV`, and `AOI`, as well as
#' already-standardised columns.
#'
#' @param data A Gazepoint fixation-level data frame.
#' @param group_cols Character vector of columns defining independent trials. If
#'   `NULL`, the function tries to infer sensible grouping columns from
#'   participant, media, and trial columns.
#' @param fixation_id_col Optional fixation ID column. If `NULL`, the function
#'   tries `FPOGID`, `fixation_id`, and related names.
#' @param start_col Optional fixation start-time column. If `NULL`, the function
#'   tries `FPOGS`, `fixation_start_time`, `time`, `TIME`, and `TIMETICK`.
#' @param duration_col Optional fixation-duration column. If `NULL`, the
#'   function tries `FPOGD`, `fixation_duration_ms`, `fixation_duration`, and
#'   related names.
#' @param x_col Optional fixation x-coordinate column.
#' @param y_col Optional fixation y-coordinate column.
#' @param valid_col Optional fixation-validity column. If detected and
#'   `valid_only = TRUE`, invalid fixations are removed.
#' @param aoi_col Optional AOI column. If `NULL`, the function tries `AOI`,
#'   `aoi_current`, and `aoi_state`.
#' @param start_time_unit Unit for the start-time column: `"auto"`, `"ms"`, or
#'   `"s"`.
#' @param duration_unit Unit for the duration column: `"auto"`, `"ms"`, or
#'   `"s"`.
#' @param valid_only Logical. If `TRUE`, invalid fixations are removed when a
#'   validity column is available.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background fixations are
#'   included. If `FALSE`, they are removed before summaries are computed.
#' @param target_aoi_values Optional character vector defining target AOI labels.
#' @param distractor_aoi_values Optional character vector defining distractor AOI
#'   labels.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#'
#' @return A tibble with one row per trial/group and fixation-level features.
#'
#' @export
#' @importFrom rlang .data
summarise_gazepoint_fixation_trials <- function(
    data,
    group_cols = NULL,
    fixation_id_col = NULL,
    start_col = NULL,
    duration_col = NULL,
    x_col = NULL,
    y_col = NULL,
    valid_col = NULL,
    aoi_col = NULL,
    start_time_unit = c("auto", "ms", "s"),
    duration_unit = c("auto", "ms", "s"),
    valid_only = TRUE,
    include_non_aoi = TRUE,
    target_aoi_values = NULL,
    distractor_aoi_values = NULL,
    non_aoi_values = c(
      "non_aoi",
      "none",
      "background",
      "outside",
      "outside_aoi",
      "missing",
      "missing_aoi"
    ),
    missing_aoi_label = "missing_aoi"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  start_time_unit <- match.arg(start_time_unit)
  duration_unit <- match.arg(duration_unit)

  valid_optional_column <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      )
  }

  valid_optional_values <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) >= 1L &&
          all(!is.na(x)) &&
          all(nzchar(x))
      )
  }

  if (!valid_optional_column(fixation_id_col)) {
    stop(
      "`fixation_id_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(start_col)) {
    stop(
      "`start_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(duration_col)) {
    stop(
      "`duration_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(x_col)) {
    stop(
      "`x_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(y_col)) {
    stop(
      "`y_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(valid_col)) {
    stop(
      "`valid_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!valid_optional_column(aoi_col)) {
    stop(
      "`aoi_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.null(group_cols) &&
      (!is.character(group_cols) ||
       length(group_cols) < 1L ||
       any(is.na(group_cols)) ||
       any(!nzchar(group_cols)) ||
       anyDuplicated(group_cols))) {
    stop(
      "`group_cols` must be NULL or a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.logical(valid_only) ||
      length(valid_only) != 1L ||
      is.na(valid_only)) {
    stop("`valid_only` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(include_non_aoi) ||
      length(include_non_aoi) != 1L ||
      is.na(include_non_aoi)) {
    stop("`include_non_aoi` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!valid_optional_values(target_aoi_values)) {
    stop(
      "`target_aoi_values` must be NULL or a character vector of non-missing labels.",
      call. = FALSE
    )
  }

  if (!valid_optional_values(distractor_aoi_values)) {
    stop(
      "`distractor_aoi_values` must be NULL or a character vector of non-missing labels.",
      call. = FALSE
    )
  }

  if (!is.character(non_aoi_values) ||
      any(is.na(non_aoi_values)) ||
      any(!nzchar(non_aoi_values))) {
    stop(
      "`non_aoi_values` must be a character vector of non-missing labels.",
      call. = FALSE
    )
  }

  if (!is.character(missing_aoi_label) ||
      length(missing_aoi_label) != 1L ||
      is.na(missing_aoi_label) ||
      !nzchar(missing_aoi_label)) {
    stop(
      "`missing_aoi_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  first_existing <- function(candidates, names_available) {
    hit <- candidates[candidates %in% names_available]

    if (length(hit) == 0L) {
      return(NULL)
    }

    hit[[1]]
  }

  first_informative_existing <- function(candidates, dat) {
    hit <- candidates[candidates %in% names(dat)]

    if (length(hit) == 0L) {
      return(NULL)
    }

    for (col in hit) {
      values <- dat[[col]]

      values <- as.character(values)
      values <- trimws(values)
      values <- values[!is.na(values) & nzchar(values)]

      if (length(unique(values)) > 0L) {
        return(col)
      }
    }

    hit[[1]]
  }

  detect_group_cols <- function(dat) {
    subject_col <- first_informative_existing(
      c(
        "subject",
        "USER_ID",
        "USER_FILE",
        "USER",
        "user",
        "participant",
        "participant_id"
      ),
      dat
    )

    media_col <- first_informative_existing(
      c("MEDIA_ID", "media_id", "MEDIA_NAME", "media_name", "stimulus"),
      dat
    )

    trial_col <- first_informative_existing(
      c("trial_global", "trial", "trial_id", "TRIAL"),
      dat
    )

    detected <- c(subject_col, media_col, trial_col)
    detected <- detected[!vapply(detected, is.null, logical(1))]

    unique(unlist(detected, use.names = FALSE))
  }
  if (is.null(group_cols)) {
    group_cols <- detect_group_cols(dat)

    if (length(group_cols) == 0L) {
      stop(
        "Could not automatically detect grouping columns. Please provide `group_cols`.",
        call. = FALSE
      )
    }
  }
  missing_group_cols <- setdiff(group_cols, names(dat))

  if (length(missing_group_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_group_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(fixation_id_col)) {
    fixation_id_col <- first_existing(
      c("FPOGID", "fixation_id", "fixationID", "fix_id", "id"),
      names(dat)
    )
  }

  if (is.null(start_col)) {
    start_col <- first_existing(
      c(
        "FPOGS",
        "fixation_start_time",
        "fixation_start_ms",
        "start_time",
        "start_time_ms",
        "time",
        "TIME",
        "TIMETICK"
      ),
      names(dat)
    )
  }

  if (is.null(duration_col)) {
    duration_col <- first_existing(
      c(
        "FPOGD",
        "fixation_duration_ms",
        "fixation_duration",
        "duration_ms",
        "duration",
        "FPOGD_MS"
      ),
      names(dat)
    )
  }

  if (is.null(x_col)) {
    x_col <- first_existing(
      c("FPOGX", "fixation_x", "x", "X", "gaze_x"),
      names(dat)
    )
  }

  if (is.null(y_col)) {
    y_col <- first_existing(
      c("FPOGY", "fixation_y", "y", "Y", "gaze_y"),
      names(dat)
    )
  }

  if (is.null(valid_col)) {
    valid_col <- first_existing(
      c("FPOGV", "fixation_valid", "valid", "VALID"),
      names(dat)
    )
  }

  if (is.null(aoi_col)) {
    aoi_col <- first_existing(
      c("AOI", "aoi_current", "aoi_state", "aoi"),
      names(dat)
    )
  }

  required_detected <- c(start_col, duration_col)
  missing_detected <- c()

  if (is.null(start_col)) {
    missing_detected <- c(missing_detected, "start_col")
  }

  if (is.null(duration_col)) {
    missing_detected <- c(missing_detected, "duration_col")
  }

  if (length(missing_detected) > 0L) {
    stop(
      "Could not automatically detect required fixation columns: ",
      paste(missing_detected, collapse = ", "),
      call. = FALSE
    )
  }

  optional_existing <- c(
    fixation_id_col,
    start_col,
    duration_col,
    x_col,
    y_col,
    valid_col,
    aoi_col
  )

  missing_optional_existing <- setdiff(
    optional_existing[!is.na(optional_existing)],
    names(dat)
  )

  if (length(missing_optional_existing) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_optional_existing, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_safe <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  should_convert_seconds <- function(x, col, unit, role) {
    if (unit == "s") {
      return(TRUE)
    }

    if (unit == "ms") {
      return(FALSE)
    }

    col_lower <- tolower(col)

    if (col_lower %in% c("fpogs", "fpogd")) {
      return(TRUE)
    }

    if (col_lower %in% c("time")) {
      x_num <- as_numeric_safe(x)
      x_num <- x_num[is.finite(x_num)]

      if (length(x_num) == 0L) {
        return(FALSE)
      }

      return(max(x_num, na.rm = TRUE) <= 60)
    }

    if (role == "duration") {
      x_num <- as_numeric_safe(x)
      x_num <- x_num[is.finite(x_num)]

      if (length(x_num) == 0L) {
        return(FALSE)
      }

      return(max(x_num, na.rm = TRUE) <= 60)
    }

    FALSE
  }

  convert_to_ms <- function(x, col, unit, role) {
    x_num <- as_numeric_safe(x)

    if (should_convert_seconds(x_num, col, unit, role)) {
      x_num * 1000
    } else {
      x_num
    }
  }

  clean_aoi_state <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[is.na(x) | x == ""] <- missing_aoi_label
    x
  }

  normalise_values <- function(x) {
    if (is.null(x)) {
      return(character(0))
    }

    tolower(trimws(as.character(x)))
  }

  target_values <- normalise_values(target_aoi_values)
  distractor_values <- normalise_values(distractor_aoi_values)
  background_values <- normalise_values(non_aoi_values)

  target_aoi_defined <- length(target_values) > 0L
  distractor_aoi_defined <- length(distractor_values) > 0L
  aoi_available <- !is.null(aoi_col)

  is_non_aoi_state <- function(x) {
    tolower(trimws(as.character(x))) %in% background_values
  }

  classify_state <- function(x, is_non_aoi) {
    x_norm <- tolower(trimws(as.character(x)))

    dplyr::case_when(
      !aoi_available ~ "no_aoi_column",
      is_non_aoi ~ "background",
      target_aoi_defined & x_norm %in% target_values ~ "target",
      distractor_aoi_defined & x_norm %in% distractor_values ~ "distractor",
      TRUE ~ "other_aoi"
    )
  }

  parse_valid <- function(x) {
    if (is.logical(x)) {
      return(dplyr::coalesce(x, FALSE))
    }

    x_chr <- tolower(trimws(as.character(x)))
    x_num <- suppressWarnings(as.numeric(x_chr))

    dplyr::case_when(
      !is.na(x_num) ~ x_num != 0,
      x_chr %in% c("true", "valid", "yes", "y") ~ TRUE,
      x_chr %in% c("false", "invalid", "no", "n") ~ FALSE,
      TRUE ~ FALSE
    )
  }

  dat$.gp3_start_time_ms <- convert_to_ms(
    dat[[start_col]],
    start_col,
    start_time_unit,
    "start"
  )

  dat$.gp3_duration_ms <- convert_to_ms(
    dat[[duration_col]],
    duration_col,
    duration_unit,
    "duration"
  )

  dat$.gp3_x <- if (!is.null(x_col)) {
    as_numeric_safe(dat[[x_col]])
  } else {
    NA_real_
  }

  dat$.gp3_y <- if (!is.null(y_col)) {
    as_numeric_safe(dat[[y_col]])
  } else {
    NA_real_
  }

  dat$.gp3_valid <- if (!is.null(valid_col)) {
    parse_valid(dat[[valid_col]])
  } else {
    TRUE
  }

  dat$.gp3_aoi_state <- if (aoi_available) {
    clean_aoi_state(dat[[aoi_col]])
  } else {
    rep(NA_character_, nrow(dat))
  }

  dat$.gp3_is_non_aoi <- if (aoi_available) {
    is_non_aoi_state(dat$.gp3_aoi_state)
  } else {
    rep(FALSE, nrow(dat))
  }

  dat$.gp3_state_class <- classify_state(
    dat$.gp3_aoi_state,
    dat$.gp3_is_non_aoi
  )

  dat$.gp3_fixation_id <- if (!is.null(fixation_id_col)) {
    as.character(dat[[fixation_id_col]])
  } else {
    as.character(seq_len(nrow(dat)))
  }

  dat <- dat |>
    dplyr::filter(!is.na(.data[[".gp3_start_time_ms"]]))

  if (valid_only) {
    dat <- dat |>
      dplyr::filter(.data[[".gp3_valid"]])
  }

  if (!include_non_aoi && aoi_available) {
    dat <- dat |>
      dplyr::filter(!.data[[".gp3_is_non_aoi"]])
  }

  if (nrow(dat) == 0L) {
    stop(
      "No fixation rows remain after filtering.",
      call. = FALSE
    )
  }

  first_or_na <- function(x) {
    if (length(x) == 0L) {
      return(NA)
    }

    x[[1]]
  }

  fixation_rows <- dat |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(group_cols)),
      .data[[".gp3_start_time_ms"]]
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(group_cols, ".gp3_fixation_id"))
      )
    ) |>
    dplyr::summarise(
      fixation_start_time_ms = first_or_na(.data[[".gp3_start_time_ms"]]),
      fixation_duration_ms = first_or_na(.data[[".gp3_duration_ms"]]),
      fixation_end_time_ms =
        first_or_na(.data[[".gp3_start_time_ms"]]) +
        first_or_na(.data[[".gp3_duration_ms"]]),
      fixation_x = first_or_na(.data[[".gp3_x"]]),
      fixation_y = first_or_na(.data[[".gp3_y"]]),
      fixation_valid = first_or_na(.data[[".gp3_valid"]]),
      fixation_aoi = first_or_na(.data[[".gp3_aoi_state"]]),
      is_non_aoi = first_or_na(.data[[".gp3_is_non_aoi"]]),
      state_class = first_or_na(.data[[".gp3_state_class"]]),
      n_rows_per_fixation = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(fixation_rows) == 0L) {
    stop(
      "No fixation rows remain after fixation-level reduction.",
      call. = FALSE
    )
  }

  mean_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  median_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    stats::median(x)
  }

  min_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    min(x)
  }

  max_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    max(x)
  }

  sd_or_na <- function(x) {
    x <- as_numeric_safe(x)
    x <- x[!is.na(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    stats::sd(x)
  }

  sum_or_zero <- function(x) {
    x <- as_numeric_safe(x)
    sum(x, na.rm = TRUE)
  }

  count_where <- function(condition) {
    sum(condition %in% TRUE, na.rm = TRUE)
  }

  sum_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE

    sum(x[condition], na.rm = TRUE)
  }

  mean_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    mean(x)
  }

  min_where <- function(x, condition) {
    x <- as_numeric_safe(x)
    condition <- condition %in% TRUE
    x <- x[condition]
    x <- x[!is.na(x)]

    if (length(x) == 0L) {
      return(NA_real_)
    }

    min(x)
  }

  n_unique_where <- function(x, condition) {
    condition <- condition %in% TRUE
    x <- as.character(x[condition])
    x <- x[!is.na(x) & nzchar(x)]

    length(unique(x))
  }

  first_state_where <- function(state, time, condition) {
    condition <- condition %in% TRUE
    state <- as.character(state)
    time <- as_numeric_safe(time)

    keep <- condition & !is.na(time) & !is.na(state) & nzchar(state)

    if (!any(keep)) {
      return(NA_character_)
    }

    state[keep][order(time[keep])][1]
  }

  last_state_where <- function(state, time, condition) {
    condition <- condition %in% TRUE
    state <- as.character(state)
    time <- as_numeric_safe(time)

    keep <- condition & !is.na(time) & !is.na(state) & nzchar(state)

    if (!any(keep)) {
      return(NA_character_)
    }

    state[keep][order(time[keep], decreasing = TRUE)][1]
  }

  safe_ratio <- function(numerator, denominator) {
    numerator <- as_numeric_safe(numerator)
    denominator <- as_numeric_safe(denominator)

    dplyr::if_else(
      is.na(denominator) | denominator <= 0,
      NA_real_,
      numerator / denominator
    )
  }

  fixation_rows |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols))
    ) |>
    dplyr::summarise(
      trial_start_time_ms = min_or_na(.data[["fixation_start_time_ms"]]),
      trial_end_time_ms = max_or_na(.data[["fixation_end_time_ms"]]),
      n_fixations = dplyr::n(),
      n_valid_fixations = count_where(.data[["fixation_valid"]]),
      n_rows_represented = sum_or_zero(.data[["n_rows_per_fixation"]]),

      total_fixation_duration_ms = sum_or_zero(.data[["fixation_duration_ms"]]),
      mean_fixation_duration_ms = mean_or_na(.data[["fixation_duration_ms"]]),
      median_fixation_duration_ms = median_or_na(.data[["fixation_duration_ms"]]),
      min_fixation_duration_ms = min_or_na(.data[["fixation_duration_ms"]]),
      max_fixation_duration_ms = max_or_na(.data[["fixation_duration_ms"]]),

      mean_fixation_x = mean_or_na(.data[["fixation_x"]]),
      mean_fixation_y = mean_or_na(.data[["fixation_y"]]),
      sd_fixation_x = sd_or_na(.data[["fixation_x"]]),
      sd_fixation_y = sd_or_na(.data[["fixation_y"]]),
      min_fixation_x = min_or_na(.data[["fixation_x"]]),
      max_fixation_x = max_or_na(.data[["fixation_x"]]),
      min_fixation_y = min_or_na(.data[["fixation_y"]]),
      max_fixation_y = max_or_na(.data[["fixation_y"]]),

      n_aoi_fixations = count_where(aoi_available & !.data[["is_non_aoi"]]),
      n_non_aoi_fixations = count_where(aoi_available & .data[["is_non_aoi"]]),
      n_unique_aoi_fixated = n_unique_where(
        .data[["fixation_aoi"]],
        aoi_available & !.data[["is_non_aoi"]]
      ),
      first_aoi_fixated = first_state_where(
        .data[["fixation_aoi"]],
        .data[["fixation_start_time_ms"]],
        aoi_available & !.data[["is_non_aoi"]]
      ),
      last_aoi_fixated = last_state_where(
        .data[["fixation_aoi"]],
        .data[["fixation_start_time_ms"]],
        aoi_available & !.data[["is_non_aoi"]]
      ),
      first_aoi_fixation_time_ms = min_where(
        .data[["fixation_start_time_ms"]],
        aoi_available & !.data[["is_non_aoi"]]
      ),

      target_fixation_count = count_where(.data[["state_class"]] == "target"),
      target_revisits = pmax(
        count_where(.data[["state_class"]] == "target") - 1L,
        0L
      ),
      target_fixation_duration_ms = sum_where(
        .data[["fixation_duration_ms"]],
        .data[["state_class"]] == "target"
      ),
      target_ttff_ms = min_where(
        .data[["fixation_start_time_ms"]],
        .data[["state_class"]] == "target"
      ),
      mean_target_fixation_duration_ms = mean_where(
        .data[["fixation_duration_ms"]],
        .data[["state_class"]] == "target"
      ),

      distractor_fixation_count = count_where(
        .data[["state_class"]] == "distractor"
      ),
      distractor_revisits = pmax(
        count_where(.data[["state_class"]] == "distractor") - 1L,
        0L
      ),
      distractor_fixation_duration_ms = sum_where(
        .data[["fixation_duration_ms"]],
        .data[["state_class"]] == "distractor"
      ),
      distractor_ttff_ms = min_where(
        .data[["fixation_start_time_ms"]],
        .data[["state_class"]] == "distractor"
      ),
      mean_distractor_fixation_duration_ms = mean_where(
        .data[["fixation_duration_ms"]],
        .data[["state_class"]] == "distractor"
      ),

      other_aoi_fixation_count = count_where(
        .data[["state_class"]] == "other_aoi"
      ),
      other_aoi_fixation_duration_ms = sum_where(
        .data[["fixation_duration_ms"]],
        .data[["state_class"]] == "other_aoi"
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      trial_duration_ms =
        .data[["trial_end_time_ms"]] - .data[["trial_start_time_ms"]],
      fixation_rate_per_sec = safe_ratio(
        .data[["n_fixations"]],
        .data[["trial_duration_ms"]] / 1000
      ),
      fixation_duration_prop = safe_ratio(
        .data[["total_fixation_duration_ms"]],
        .data[["trial_duration_ms"]]
      ),
      aoi_fixation_prop = safe_ratio(
        .data[["n_aoi_fixations"]],
        .data[["n_fixations"]]
      ),
      non_aoi_fixation_prop = safe_ratio(
        .data[["n_non_aoi_fixations"]],
        .data[["n_fixations"]]
      ),
      target_fixation_prop_of_aoi = safe_ratio(
        .data[["target_fixation_count"]],
        .data[["n_aoi_fixations"]]
      ),
      distractor_fixation_prop_of_aoi = safe_ratio(
        .data[["distractor_fixation_count"]],
        .data[["n_aoi_fixations"]]
      ),
      target_duration_prop_of_aoi = safe_ratio(
        .data[["target_fixation_duration_ms"]],
        .data[["target_fixation_duration_ms"]] +
          .data[["distractor_fixation_duration_ms"]] +
          .data[["other_aoi_fixation_duration_ms"]]
      ),
      distractor_duration_prop_of_aoi = safe_ratio(
        .data[["distractor_fixation_duration_ms"]],
        .data[["target_fixation_duration_ms"]] +
          .data[["distractor_fixation_duration_ms"]] +
          .data[["other_aoi_fixation_duration_ms"]]
      ),
      aoi_available = aoi_available,
      target_aoi_defined = target_aoi_defined,
      distractor_aoi_defined = distractor_aoi_defined,
      fixation_trial_feature_status = dplyr::case_when(
        !aoi_available ~ "no_aoi_column",
        .data[["n_aoi_fixations"]] == 0L ~ "no_aoi_fixations",
        !target_aoi_defined & !distractor_aoi_defined ~
          "no_target_or_distractor_defined",
        target_aoi_defined & .data[["target_fixation_count"]] == 0L ~
          "target_not_observed",
        distractor_aoi_defined & .data[["distractor_fixation_count"]] == 0L ~
          "distractor_not_observed",
        TRUE ~ "ok"
      )
    )
}
