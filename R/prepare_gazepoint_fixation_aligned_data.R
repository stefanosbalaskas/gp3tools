#' Prepare fixation- or saccade-contingent aligned Gazepoint data
#'
#' Align Gazepoint observations to a within-trial event such as first fixation,
#' first target-AOI entry, first fixation to a target AOI, first saccade to a
#' target AOI, or a custom event marker. The helper returns the original data
#' with event-aligned time, event metadata, pre-event/post-event flags, and
#' trial-level summaries that help separate event-driven looking from looks that
#' were already present before the event.
#'
#' @param data A data frame containing Gazepoint samples, fixation rows, or
#'   trial-level time-course rows.
#' @param time_col Time column.
#' @param participant_col Optional participant column.
#' @param trial_col Optional trial column.
#' @param aoi_col Optional AOI column.
#' @param target_aoi Optional character vector identifying the target AOI(s).
#' @param fixation_col Optional fixation indicator column.
#' @param saccade_col Optional saccade indicator column.
#' @param event_col Optional custom event indicator column.
#' @param event_value Optional value(s) in `event_col` defining the custom event.
#'   If `NULL` and `alignment_event = "custom"`, `event_col` is interpreted as
#'   a logical-like indicator.
#' @param alignment_event Alignment event. Options are `"first_target_entry"`,
#'   `"first_fixation_to_target"`, `"first_saccade_to_aoi"`,
#'   `"first_fixation"`, and `"custom"`.
#' @param baseline_window Optional numeric vector of length two giving the
#'   aligned-time baseline window, for example `c(-200, 0)`.
#' @param analysis_window Optional numeric vector of length two giving the
#'   aligned-time analysis window, for example `c(0, 1000)`.
#' @param keep_unaligned Logical. If `FALSE`, groups without an alignment event
#'   are removed from `aligned_data`. Their status remains in `event_table`.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_fixation_aligned_data`.
#' @export
prepare_gazepoint_fixation_aligned_data <- function(
    data,
    time_col,
    participant_col = NULL,
    trial_col = NULL,
    aoi_col = NULL,
    target_aoi = NULL,
    fixation_col = NULL,
    saccade_col = NULL,
    event_col = NULL,
    event_value = NULL,
    alignment_event = c(
      "first_target_entry",
      "first_fixation_to_target",
      "first_saccade_to_aoi",
      "first_fixation",
      "custom"
    ),
    baseline_window = NULL,
    analysis_window = NULL,
    keep_unaligned = FALSE,
    name = "gazepoint_fixation_aligned_data"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  alignment_event <- match.arg(alignment_event)

  .gp3_fixalign_check_col(time_col, names(data), "time_col")

  if (!is.null(participant_col)) {
    .gp3_fixalign_check_col(participant_col, names(data), "participant_col")
  }

  if (!is.null(trial_col)) {
    .gp3_fixalign_check_col(trial_col, names(data), "trial_col")
  }

  if (!is.null(aoi_col)) {
    .gp3_fixalign_check_col(aoi_col, names(data), "aoi_col")
  }

  if (!is.null(fixation_col)) {
    .gp3_fixalign_check_col(fixation_col, names(data), "fixation_col")
  }

  if (!is.null(saccade_col)) {
    .gp3_fixalign_check_col(saccade_col, names(data), "saccade_col")
  }

  if (!is.null(event_col)) {
    .gp3_fixalign_check_col(event_col, names(data), "event_col")
  }

  .gp3_fixalign_check_window(baseline_window, "baseline_window")
  .gp3_fixalign_check_window(analysis_window, "analysis_window")
  .gp3_fixalign_check_logical(keep_unaligned, "keep_unaligned")
  .gp3_fixalign_check_label(name, "name")

  if (alignment_event %in% c("first_target_entry", "first_fixation_to_target", "first_saccade_to_aoi")) {
    if (is.null(aoi_col)) {
      stop("`aoi_col` is required for target-AOI alignment events.", call. = FALSE)
    }

    if (is.null(target_aoi) || !is.character(target_aoi) || length(target_aoi) == 0L || anyNA(target_aoi)) {
      stop("`target_aoi` must be a non-empty character vector for target-AOI alignment events.", call. = FALSE)
    }

    target_aoi <- unique(target_aoi[nzchar(target_aoi)])

    if (length(target_aoi) == 0L) {
      stop("`target_aoi` must contain at least one non-empty value.", call. = FALSE)
    }
  }

  if (identical(alignment_event, "first_fixation") && is.null(fixation_col)) {
    stop("`fixation_col` is required when `alignment_event = 'first_fixation'`.", call. = FALSE)
  }

  if (identical(alignment_event, "first_fixation_to_target") && is.null(fixation_col)) {
    stop("`fixation_col` is required when `alignment_event = 'first_fixation_to_target'`.", call. = FALSE)
  }

  if (identical(alignment_event, "first_saccade_to_aoi") && is.null(saccade_col)) {
    stop("`saccade_col` is required when `alignment_event = 'first_saccade_to_aoi'`.", call. = FALSE)
  }

  if (identical(alignment_event, "custom") && is.null(event_col)) {
    stop("`event_col` is required when `alignment_event = 'custom'`.", call. = FALSE)
  }

  prepared <- tibble::tibble(
    .gp3_fixalign_row_id = seq_len(nrow(data)),
    .gp3_participant = if (!is.null(participant_col)) as.character(data[[participant_col]]) else "all_participants",
    .gp3_trial = if (!is.null(trial_col)) as.character(data[[trial_col]]) else "all_trials",
    .gp3_time = suppressWarnings(as.numeric(data[[time_col]])),
    .gp3_aoi = if (!is.null(aoi_col)) as.character(data[[aoi_col]]) else NA_character_,
    .gp3_is_fixation = if (!is.null(fixation_col)) .gp3_fixalign_as_logical(data[[fixation_col]]) else FALSE,
    .gp3_is_saccade = if (!is.null(saccade_col)) .gp3_fixalign_as_logical(data[[saccade_col]]) else FALSE,
    .gp3_custom_event = if (!is.null(event_col)) {
      .gp3_fixalign_custom_event(data[[event_col]], event_value)
    } else {
      FALSE
    }
  ) |>
    dplyr::mutate(
      .gp3_group_id = paste(.data$.gp3_participant, .data$.gp3_trial, sep = "||"),
      .gp3_is_target_aoi = if (!is.null(target_aoi) && !is.null(aoi_col)) {
        .data$.gp3_aoi %in% target_aoi
      } else {
        FALSE
      }
    )

  if (any(!is.finite(prepared$.gp3_time))) {
    stop("`time_col` must be numeric or coercible to finite numeric values.", call. = FALSE)
  }

  alignment_candidate <- switch(
    alignment_event,
    first_target_entry = prepared$.gp3_is_target_aoi,
    first_fixation_to_target = prepared$.gp3_is_target_aoi & prepared$.gp3_is_fixation,
    first_saccade_to_aoi = prepared$.gp3_is_target_aoi & prepared$.gp3_is_saccade,
    first_fixation = prepared$.gp3_is_fixation,
    custom = prepared$.gp3_custom_event
  )

  alignment_candidate[is.na(alignment_candidate)] <- FALSE

  prepared <- prepared |>
    dplyr::mutate(
      .gp3_alignment_candidate = alignment_candidate
    ) |>
    dplyr::arrange(.data$.gp3_group_id, .data$.gp3_time, .data$.gp3_fixalign_row_id)

  event_table <- .gp3_fixalign_event_table(
    prepared = prepared,
    alignment_event = alignment_event
  )

  aligned_internal <- prepared |>
    dplyr::left_join(
      event_table |>
        dplyr::select(
          "gp3_group_id",
          "gp3_has_alignment_event",
          "gp3_alignment_time",
          "gp3_alignment_row_id",
          "gp3_alignment_event",
          "gp3_target_present_before_event",
          "gp3_fixation_to_target_before_event",
          "gp3_already_on_target_at_trial_start"
        ),
      by = c(".gp3_group_id" = "gp3_group_id")
    ) |>
    dplyr::mutate(
      gp3_aligned_time = dplyr::if_else(
        .data$gp3_has_alignment_event,
        .data$.gp3_time - .data$gp3_alignment_time,
        NA_real_
      ),
      gp3_is_alignment_event_row = .data$.gp3_fixalign_row_id == .data$gp3_alignment_row_id,
      gp3_alignment_phase = dplyr::case_when(
        !.data$gp3_has_alignment_event ~ "unaligned",
        .data$gp3_aligned_time < 0 ~ "pre_event",
        .data$gp3_is_alignment_event_row ~ "alignment_event",
        .data$gp3_aligned_time >= 0 ~ "post_event",
        TRUE ~ "unaligned"
      ),
      gp3_is_target_aoi = .data$.gp3_is_target_aoi,
      gp3_is_fixation_sample = .data$.gp3_is_fixation,
      gp3_is_saccade_sample = .data$.gp3_is_saccade,
      gp3_in_baseline_window = .gp3_fixalign_in_window(.data$gp3_aligned_time, baseline_window),
      gp3_in_analysis_window = .gp3_fixalign_in_window(.data$gp3_aligned_time, analysis_window)
    ) |>
    dplyr::select(
      ".gp3_fixalign_row_id",
      "gp3_has_alignment_event",
      "gp3_alignment_event",
      "gp3_alignment_time",
      "gp3_alignment_row_id",
      "gp3_aligned_time",
      "gp3_alignment_phase",
      "gp3_is_alignment_event_row",
      "gp3_is_target_aoi",
      "gp3_is_fixation_sample",
      "gp3_is_saccade_sample",
      "gp3_in_baseline_window",
      "gp3_in_analysis_window",
      "gp3_target_present_before_event",
      "gp3_fixation_to_target_before_event",
      "gp3_already_on_target_at_trial_start"
    )

  aligned_data <- tibble::as_tibble(data) |>
    dplyr::mutate(.gp3_fixalign_row_id = seq_len(dplyr::n())) |>
    dplyr::left_join(aligned_internal, by = ".gp3_fixalign_row_id") |>
    dplyr::select(-".gp3_fixalign_row_id")

  if (!isTRUE(keep_unaligned)) {
    aligned_data <- aligned_data |>
      dplyr::filter(.data$gp3_has_alignment_event)
  }

  trial_summary <- event_table |>
    dplyr::group_by(.data$gp3_has_alignment_event) |>
    dplyr::summarise(
      n_groups = dplyr::n(),
      n_with_pre_event_target = sum(.data$gp3_target_present_before_event, na.rm = TRUE),
      n_already_on_target_at_start = sum(.data$gp3_already_on_target_at_trial_start, na.rm = TRUE),
      median_alignment_time = stats::median(.data$gp3_alignment_time, na.rm = TRUE),
      .groups = "drop"
    )

  n_groups <- nrow(event_table)
  n_aligned_groups <- sum(event_table$gp3_has_alignment_event)
  n_unaligned_groups <- n_groups - n_aligned_groups

  alignment_status <- if (n_aligned_groups == n_groups) {
    "complete"
  } else if (n_aligned_groups > 0L) {
    "partial_complete"
  } else {
    "no_alignment_events"
  }

  overview <- tibble::tibble(
    object_name = name,
    alignment_status = alignment_status,
    alignment_event = alignment_event,
    n_input_rows = nrow(data),
    n_output_rows = nrow(aligned_data),
    n_groups = n_groups,
    n_aligned_groups = n_aligned_groups,
    n_unaligned_groups = n_unaligned_groups,
    target_aoi = .gp3_fixalign_collapse_nullable(target_aoi),
    baseline_window = .gp3_fixalign_collapse_nullable(baseline_window),
    analysis_window = .gp3_fixalign_collapse_nullable(analysis_window),
    keep_unaligned = keep_unaligned
  )

  settings <- tibble::tibble(
    setting = c(
      "time_col",
      "participant_col",
      "trial_col",
      "aoi_col",
      "target_aoi",
      "fixation_col",
      "saccade_col",
      "event_col",
      "event_value",
      "alignment_event",
      "baseline_window",
      "analysis_window",
      "keep_unaligned",
      "name"
    ),
    value = c(
      time_col,
      .gp3_fixalign_collapse_nullable(participant_col),
      .gp3_fixalign_collapse_nullable(trial_col),
      .gp3_fixalign_collapse_nullable(aoi_col),
      .gp3_fixalign_collapse_nullable(target_aoi),
      .gp3_fixalign_collapse_nullable(fixation_col),
      .gp3_fixalign_collapse_nullable(saccade_col),
      .gp3_fixalign_collapse_nullable(event_col),
      .gp3_fixalign_collapse_nullable(event_value),
      alignment_event,
      .gp3_fixalign_collapse_nullable(baseline_window),
      .gp3_fixalign_collapse_nullable(analysis_window),
      as.character(keep_unaligned),
      name
    )
  )

  out <- list(
    overview = overview,
    aligned_data = aligned_data,
    event_table = event_table,
    trial_summary = trial_summary,
    settings = settings
  )

  class(out) <- c("gp3_fixation_aligned_data", "list")

  out
}

.gp3_fixalign_event_table <- function(prepared, alignment_event) {
  split_data <- split(prepared, prepared$.gp3_group_id)

  rows <- lapply(split_data, function(df) {
    df <- df[order(df$.gp3_time, df$.gp3_fixalign_row_id), , drop = FALSE]
    candidate_index <- which(df$.gp3_alignment_candidate)
    has_event <- length(candidate_index) > 0L

    if (has_event) {
      idx <- candidate_index[[1]]
      alignment_time <- df$.gp3_time[[idx]]
      alignment_row_id <- df$.gp3_fixalign_row_id[[idx]]
      event_aoi <- df$.gp3_aoi[[idx]]
      event_is_target <- isTRUE(df$.gp3_is_target_aoi[[idx]])
      event_is_fixation <- isTRUE(df$.gp3_is_fixation[[idx]])
      event_is_saccade <- isTRUE(df$.gp3_is_saccade[[idx]])
    } else {
      alignment_time <- NA_real_
      alignment_row_id <- NA_integer_
      event_aoi <- NA_character_
      event_is_target <- NA
      event_is_fixation <- NA
      event_is_saccade <- NA
    }

    first_row <- df[1, , drop = FALSE]
    pre_event <- if (has_event) {
      df[df$.gp3_time < alignment_time, , drop = FALSE]
    } else {
      df[0, , drop = FALSE]
    }

    post_event <- if (has_event) {
      df[df$.gp3_time > alignment_time, , drop = FALSE]
    } else {
      df[0, , drop = FALSE]
    }

    pre_event_n <- nrow(pre_event)
    pre_event_target_n <- sum(pre_event$.gp3_is_target_aoi, na.rm = TRUE)

    tibble::tibble(
      gp3_group_id = df$.gp3_group_id[[1]],
      gp3_participant = df$.gp3_participant[[1]],
      gp3_trial = df$.gp3_trial[[1]],
      gp3_alignment_event = alignment_event,
      gp3_has_alignment_event = has_event,
      gp3_alignment_time = alignment_time,
      gp3_alignment_row_id = alignment_row_id,
      gp3_event_aoi = event_aoi,
      gp3_event_is_target_aoi = event_is_target,
      gp3_event_is_fixation = event_is_fixation,
      gp3_event_is_saccade = event_is_saccade,
      gp3_n_rows = nrow(df),
      gp3_start_time = min(df$.gp3_time, na.rm = TRUE),
      gp3_end_time = max(df$.gp3_time, na.rm = TRUE),
      gp3_pre_event_n_samples = pre_event_n,
      gp3_pre_event_target_n_samples = pre_event_target_n,
      gp3_pre_event_target_prop = if (pre_event_n > 0L) pre_event_target_n / pre_event_n else NA_real_,
      gp3_post_event_n_samples = nrow(post_event),
      gp3_target_present_before_event = any(pre_event$.gp3_is_target_aoi, na.rm = TRUE),
      gp3_fixation_to_target_before_event = any(pre_event$.gp3_is_target_aoi & pre_event$.gp3_is_fixation, na.rm = TRUE),
      gp3_already_on_target_at_trial_start = isTRUE(first_row$.gp3_is_target_aoi[[1]])
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_fixalign_as_logical <- function(x) {
  if (is.logical(x)) {
    out <- x
  } else if (is.numeric(x)) {
    out <- !is.na(x) & x != 0
  } else {
    x_chr <- tolower(trimws(as.character(x)))
    out <- x_chr %in% c(
      "true",
      "t",
      "1",
      "yes",
      "y",
      "fixation",
      "fixated",
      "saccade",
      "saccadic",
      "event",
      "target"
    )
  }

  out[is.na(out)] <- FALSE
  out
}

.gp3_fixalign_custom_event <- function(x, event_value = NULL) {
  if (is.null(event_value)) {
    return(.gp3_fixalign_as_logical(x))
  }

  as.character(x) %in% as.character(event_value)
}

.gp3_fixalign_in_window <- function(x, window) {
  if (is.null(window)) {
    return(!is.na(x))
  }

  !is.na(x) & x >= window[[1]] & x <= window[[2]]
}

.gp3_fixalign_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_fixalign_check_window <- function(window, arg) {
  if (is.null(window)) {
    return(invisible(TRUE))
  }

  if (!is.numeric(window) || length(window) != 2L || anyNA(window) || any(!is.finite(window))) {
    stop("`", arg, "` must be NULL or a finite numeric vector of length two.", call. = FALSE)
  }

  if (window[[1]] > window[[2]]) {
    stop("`", arg, "` lower bound must be less than or equal to its upper bound.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_fixalign_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_fixalign_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_fixalign_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
