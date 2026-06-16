#' Recommend trial and participant exclusions
#'
#' Create explicit trial-level and participant-level exclusion recommendations
#' from Gazepoint sample-level quality information. The helper can use validity
#' flags, gaze-coordinate missingness, pupil missingness, and optional artifact
#' flags to produce transparent exclusion tables.
#'
#' This function recommends exclusions only. It does not remove rows.
#'
#' @param data A data frame containing sample-level or trial-level data.
#' @param participant_col Participant identifier column.
#' @param trial_col Optional trial identifier column.
#' @param condition_col Optional condition column retained in summaries.
#' @param validity_col Optional logical/numeric/character validity column.
#' @param x_col Optional horizontal gaze coordinate column.
#' @param y_col Optional vertical gaze coordinate column.
#' @param pupil_col Optional pupil column.
#' @param artifact_col Optional logical/numeric/character artifact flag column.
#' @param min_trial_samples Minimum samples required per trial.
#' @param max_trial_missing_prop Maximum missing/unusable sample proportion per trial.
#' @param max_trial_artifact_prop Maximum artifact proportion per trial.
#' @param min_participant_trials Minimum total trials required per participant.
#' @param min_participant_valid_trials Minimum retained trials required per participant.
#' @param max_participant_missing_prop Maximum missing/unusable sample proportion per participant.
#' @param max_participant_artifact_prop Maximum artifact proportion per participant.
#' @param require_both_gaze_coordinates Logical. If both gaze columns are supplied,
#'   should a sample be usable only when both coordinates are finite?
#' @param name Character label stored in object attributes.
#'
#' @return A list with overview, trial recommendations, participant
#'   recommendations, an explicit exclusion table, and settings.
#' @export
recommend_gazepoint_exclusions <- function(
    data,
    participant_col,
    trial_col = NULL,
    condition_col = NULL,
    validity_col = NULL,
    x_col = NULL,
    y_col = NULL,
    pupil_col = NULL,
    artifact_col = NULL,
    min_trial_samples = 10L,
    max_trial_missing_prop = 0.50,
    max_trial_artifact_prop = 0.50,
    min_participant_trials = 2L,
    min_participant_valid_trials = 1L,
    max_participant_missing_prop = 0.50,
    max_participant_artifact_prop = 0.50,
    require_both_gaze_coordinates = TRUE,
    name = "gazepoint_exclusion_recommendations"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_excl_check_col(participant_col, names(data), "participant_col")

  if (!is.null(trial_col)) {
    .gp3_excl_check_col(trial_col, names(data), "trial_col")
  }

  if (!is.null(condition_col)) {
    .gp3_excl_check_col(condition_col, names(data), "condition_col")
  }

  if (!is.null(validity_col)) {
    .gp3_excl_check_col(validity_col, names(data), "validity_col")
  }

  if (!is.null(x_col)) {
    .gp3_excl_check_col(x_col, names(data), "x_col")
  }

  if (!is.null(y_col)) {
    .gp3_excl_check_col(y_col, names(data), "y_col")
  }

  if (!is.null(pupil_col)) {
    .gp3_excl_check_col(pupil_col, names(data), "pupil_col")
  }

  if (!is.null(artifact_col)) {
    .gp3_excl_check_col(artifact_col, names(data), "artifact_col")
  }

  if (
    is.null(validity_col) &&
    is.null(x_col) &&
    is.null(y_col) &&
    is.null(pupil_col) &&
    is.null(artifact_col)
  ) {
    stop(
      "Supply at least one quality indicator: `validity_col`, gaze columns, `pupil_col`, or `artifact_col`.",
      call. = FALSE
    )
  }

  if (isTRUE(require_both_gaze_coordinates) && xor(is.null(x_col), is.null(y_col))) {
    stop(
      "When `require_both_gaze_coordinates = TRUE`, supply both `x_col` and `y_col` or neither.",
      call. = FALSE
    )
  }

  .gp3_excl_check_positive_integer(min_trial_samples, "min_trial_samples")
  .gp3_excl_check_proportion(max_trial_missing_prop, "max_trial_missing_prop")
  .gp3_excl_check_proportion(max_trial_artifact_prop, "max_trial_artifact_prop")
  .gp3_excl_check_positive_integer(min_participant_trials, "min_participant_trials")
  .gp3_excl_check_positive_integer(min_participant_valid_trials, "min_participant_valid_trials")
  .gp3_excl_check_proportion(max_participant_missing_prop, "max_participant_missing_prop")
  .gp3_excl_check_proportion(max_participant_artifact_prop, "max_participant_artifact_prop")
  .gp3_excl_check_logical(require_both_gaze_coordinates, "require_both_gaze_coordinates")
  .gp3_excl_check_label(name, "name")

  prepared <- tibble::as_tibble(data) |>
    dplyr::mutate(
      .gp3_excl_row_id = seq_len(dplyr::n()),
      .gp3_participant = as.character(.data[[participant_col]])
    )

  if (!is.null(trial_col)) {
    prepared$.gp3_trial <- as.character(prepared[[trial_col]])
  } else {
    prepared$.gp3_trial <- "all_trials"
  }

  if (!is.null(condition_col)) {
    prepared$.gp3_condition <- as.character(prepared[[condition_col]])
  } else {
    prepared$.gp3_condition <- NA_character_
  }

  validity_missing <- rep(FALSE, nrow(prepared))

  if (!is.null(validity_col)) {
    validity_values <- .gp3_excl_as_logical(prepared[[validity_col]])
    validity_missing <- is.na(validity_values) | !validity_values
  }

  gaze_missing <- rep(FALSE, nrow(prepared))

  if (!is.null(x_col) && !is.null(y_col)) {
    gaze_x <- suppressWarnings(as.numeric(prepared[[x_col]]))
    gaze_y <- suppressWarnings(as.numeric(prepared[[y_col]]))

    if (isTRUE(require_both_gaze_coordinates)) {
      gaze_missing <- !is.finite(gaze_x) | !is.finite(gaze_y)
    } else {
      gaze_missing <- !is.finite(gaze_x) & !is.finite(gaze_y)
    }
  } else if (!is.null(x_col)) {
    gaze_x <- suppressWarnings(as.numeric(prepared[[x_col]]))
    gaze_missing <- !is.finite(gaze_x)
  } else if (!is.null(y_col)) {
    gaze_y <- suppressWarnings(as.numeric(prepared[[y_col]]))
    gaze_missing <- !is.finite(gaze_y)
  }

  pupil_missing <- rep(FALSE, nrow(prepared))

  if (!is.null(pupil_col)) {
    pupil <- suppressWarnings(as.numeric(prepared[[pupil_col]]))
    pupil_missing <- !is.finite(pupil)
  }

  artifact_flag <- rep(FALSE, nrow(prepared))

  if (!is.null(artifact_col)) {
    artifact_values <- .gp3_excl_as_logical(prepared[[artifact_col]])
    artifact_flag <- artifact_values %in% TRUE
  }

  prepared <- prepared |>
    dplyr::mutate(
      .gp3_validity_missing = validity_missing,
      .gp3_gaze_missing = gaze_missing,
      .gp3_pupil_missing = pupil_missing,
      .gp3_artifact = artifact_flag,
      .gp3_missing_or_unusable = .data$.gp3_validity_missing |
        .data$.gp3_gaze_missing |
        .data$.gp3_pupil_missing,
      .gp3_usable_sample = !.data$.gp3_missing_or_unusable & !.data$.gp3_artifact
    )

  trial_recommendations <- prepared |>
    dplyr::group_by(.data$.gp3_participant, .data$.gp3_trial) |>
    dplyr::summarise(
      condition = .gp3_excl_collapse_unique(.data$.gp3_condition),
      n_samples = dplyr::n(),
      n_missing_or_unusable = sum(.data$.gp3_missing_or_unusable, na.rm = TRUE),
      missing_or_unusable_prop = .data$n_missing_or_unusable / .data$n_samples,
      n_artifact = sum(.data$.gp3_artifact, na.rm = TRUE),
      artifact_prop = .data$n_artifact / .data$n_samples,
      n_usable = sum(.data$.gp3_usable_sample, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(
      participant = ".gp3_participant",
      trial = ".gp3_trial"
    )

  trial_recommendations$exclusion_reason <- mapply(
    .gp3_excl_trial_reason,
    n_samples = trial_recommendations$n_samples,
    missing_prop = trial_recommendations$missing_or_unusable_prop,
    artifact_prop = trial_recommendations$artifact_prop,
    MoreArgs = list(
      min_trial_samples = min_trial_samples,
      max_trial_missing_prop = max_trial_missing_prop,
      max_trial_artifact_prop = max_trial_artifact_prop
    ),
    USE.NAMES = FALSE
  )

  trial_recommendations <- trial_recommendations |>
    dplyr::mutate(
      recommend_exclude = nzchar(.data$exclusion_reason),
      recommendation_status = dplyr::if_else(.data$recommend_exclude, "exclude", "retain")
    ) |>
    dplyr::arrange(.data$participant, .data$trial)

  participant_sample_summary <- prepared |>
    dplyr::group_by(.data$.gp3_participant) |>
    dplyr::summarise(
      conditions = .gp3_excl_collapse_unique(.data$.gp3_condition),
      n_samples = dplyr::n(),
      n_missing_or_unusable = sum(.data$.gp3_missing_or_unusable, na.rm = TRUE),
      missing_or_unusable_prop = .data$n_missing_or_unusable / .data$n_samples,
      n_artifact = sum(.data$.gp3_artifact, na.rm = TRUE),
      artifact_prop = .data$n_artifact / .data$n_samples,
      n_usable = sum(.data$.gp3_usable_sample, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(participant = ".gp3_participant")

  participant_trial_summary <- trial_recommendations |>
    dplyr::group_by(.data$participant) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_trial_exclusions = sum(.data$recommend_exclude, na.rm = TRUE),
      n_retained_trials = sum(!.data$recommend_exclude, na.rm = TRUE),
      .groups = "drop"
    )

  participant_recommendations <- participant_sample_summary |>
    dplyr::left_join(
      participant_trial_summary,
      by = "participant"
    )

  participant_recommendations$exclusion_reason <- mapply(
    .gp3_excl_participant_reason,
    n_trials = participant_recommendations$n_trials,
    n_retained_trials = participant_recommendations$n_retained_trials,
    missing_prop = participant_recommendations$missing_or_unusable_prop,
    artifact_prop = participant_recommendations$artifact_prop,
    MoreArgs = list(
      min_participant_trials = min_participant_trials,
      min_participant_valid_trials = min_participant_valid_trials,
      max_participant_missing_prop = max_participant_missing_prop,
      max_participant_artifact_prop = max_participant_artifact_prop
    ),
    USE.NAMES = FALSE
  )

  participant_recommendations <- participant_recommendations |>
    dplyr::mutate(
      recommend_exclude = nzchar(.data$exclusion_reason),
      recommendation_status = dplyr::if_else(.data$recommend_exclude, "exclude", "retain")
    ) |>
    dplyr::arrange(.data$participant)

  trial_level_table <- trial_recommendations |>
    dplyr::transmute(
      exclusion_level = "trial",
      participant = .data$participant,
      trial = if (is.null(trial_col)) NA_character_ else .data$trial,
      condition = .data$condition,
      n_samples = .data$n_samples,
      n_trials = NA_integer_,
      n_retained_trials = NA_integer_,
      missing_or_unusable_prop = .data$missing_or_unusable_prop,
      artifact_prop = .data$artifact_prop,
      recommend_exclude = .data$recommend_exclude,
      recommendation_status = .data$recommendation_status,
      exclusion_reason = .data$exclusion_reason
    )

  participant_level_table <- participant_recommendations |>
    dplyr::transmute(
      exclusion_level = "participant",
      participant = .data$participant,
      trial = NA_character_,
      condition = .data$conditions,
      n_samples = .data$n_samples,
      n_trials = .data$n_trials,
      n_retained_trials = .data$n_retained_trials,
      missing_or_unusable_prop = .data$missing_or_unusable_prop,
      artifact_prop = .data$artifact_prop,
      recommend_exclude = .data$recommend_exclude,
      recommendation_status = .data$recommendation_status,
      exclusion_reason = .data$exclusion_reason
    )

  exclusion_table <- dplyr::bind_rows(
    participant_level_table,
    trial_level_table
  ) |>
    dplyr::arrange(
      dplyr::desc(.data$recommend_exclude),
      .data$exclusion_level,
      .data$participant,
      .data$trial
    )

  overview <- tibble::tibble(
    object_name = name,
    recommendation_status = "complete",
    participant_col = participant_col,
    trial_col = .gp3_excl_collapse_nullable(trial_col),
    condition_col = .gp3_excl_collapse_nullable(condition_col),
    validity_col = .gp3_excl_collapse_nullable(validity_col),
    x_col = .gp3_excl_collapse_nullable(x_col),
    y_col = .gp3_excl_collapse_nullable(y_col),
    pupil_col = .gp3_excl_collapse_nullable(pupil_col),
    artifact_col = .gp3_excl_collapse_nullable(artifact_col),
    n_input_rows = nrow(data),
    n_participants = dplyr::n_distinct(prepared$.gp3_participant),
    n_trials = nrow(trial_recommendations),
    n_recommended_participant_exclusions = sum(participant_recommendations$recommend_exclude, na.rm = TRUE),
    n_recommended_trial_exclusions = sum(trial_recommendations$recommend_exclude, na.rm = TRUE),
    min_trial_samples = min_trial_samples,
    max_trial_missing_prop = max_trial_missing_prop,
    max_trial_artifact_prop = max_trial_artifact_prop,
    min_participant_trials = min_participant_trials,
    min_participant_valid_trials = min_participant_valid_trials,
    max_participant_missing_prop = max_participant_missing_prop,
    max_participant_artifact_prop = max_participant_artifact_prop
  )

  settings <- tibble::tibble(
    setting = c(
      "participant_col",
      "trial_col",
      "condition_col",
      "validity_col",
      "x_col",
      "y_col",
      "pupil_col",
      "artifact_col",
      "min_trial_samples",
      "max_trial_missing_prop",
      "max_trial_artifact_prop",
      "min_participant_trials",
      "min_participant_valid_trials",
      "max_participant_missing_prop",
      "max_participant_artifact_prop",
      "require_both_gaze_coordinates",
      "name"
    ),
    value = c(
      participant_col,
      .gp3_excl_collapse_nullable(trial_col),
      .gp3_excl_collapse_nullable(condition_col),
      .gp3_excl_collapse_nullable(validity_col),
      .gp3_excl_collapse_nullable(x_col),
      .gp3_excl_collapse_nullable(y_col),
      .gp3_excl_collapse_nullable(pupil_col),
      .gp3_excl_collapse_nullable(artifact_col),
      as.character(min_trial_samples),
      as.character(max_trial_missing_prop),
      as.character(max_trial_artifact_prop),
      as.character(min_participant_trials),
      as.character(min_participant_valid_trials),
      as.character(max_participant_missing_prop),
      as.character(max_participant_artifact_prop),
      as.character(require_both_gaze_coordinates),
      name
    )
  )

  out <- list(
    overview = overview,
    participant_recommendations = participant_recommendations,
    trial_recommendations = trial_recommendations,
    exclusion_table = exclusion_table,
    settings = settings
  )

  class(out) <- c("gp3_exclusion_recommendations", "list")

  out
}

.gp3_excl_trial_reason <- function(
    n_samples,
    missing_prop,
    artifact_prop,
    min_trial_samples,
    max_trial_missing_prop,
    max_trial_artifact_prop
) {
  reasons <- character(0)

  if (n_samples < min_trial_samples) {
    reasons <- c(reasons, "too_few_trial_samples")
  }

  if (is.finite(missing_prop) && missing_prop > max_trial_missing_prop) {
    reasons <- c(reasons, "high_trial_missingness")
  }

  if (is.finite(artifact_prop) && artifact_prop > max_trial_artifact_prop) {
    reasons <- c(reasons, "high_trial_artifact_rate")
  }

  paste(reasons, collapse = "; ")
}

.gp3_excl_participant_reason <- function(
    n_trials,
    n_retained_trials,
    missing_prop,
    artifact_prop,
    min_participant_trials,
    min_participant_valid_trials,
    max_participant_missing_prop,
    max_participant_artifact_prop
) {
  reasons <- character(0)

  if (n_trials < min_participant_trials) {
    reasons <- c(reasons, "too_few_participant_trials")
  }

  if (n_retained_trials < min_participant_valid_trials) {
    reasons <- c(reasons, "too_few_retained_trials")
  }

  if (is.finite(missing_prop) && missing_prop > max_participant_missing_prop) {
    reasons <- c(reasons, "high_participant_missingness")
  }

  if (is.finite(artifact_prop) && artifact_prop > max_participant_artifact_prop) {
    reasons <- c(reasons, "high_participant_artifact_rate")
  }

  paste(reasons, collapse = "; ")
}

.gp3_excl_as_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[x == 1] <- TRUE
    out[x == 0] <- FALSE
    return(out)
  }

  x_chr <- tolower(trimws(as.character(x)))

  out <- rep(NA, length(x_chr))
  out[x_chr %in% c("true", "t", "yes", "y", "1", "valid", "good")] <- TRUE
  out[x_chr %in% c("false", "f", "no", "n", "0", "invalid", "bad")] <- FALSE

  out
}

.gp3_excl_collapse_unique <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = ", ")
}

.gp3_excl_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_excl_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_excl_check_proportion <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x > 1) {
    stop("`", arg, "` must be a finite number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_excl_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_excl_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_excl_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
