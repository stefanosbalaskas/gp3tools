
#' Compute AOI entropy metrics
#'
#' Computes spatial AOI entropy, directed transition entropy, and conditional
#' transition entropy for Gazepoint-style AOI sequences. The function is useful
#' for quantifying how concentrated, dispersed, or predictable gaze allocation is
#' across Areas of Interest.
#'
#' @param data A data frame containing AOI observations.
#' @param aoi_col Character scalar. Column containing AOI labels.
#' @param group_cols Optional character vector of grouping columns, such as
#'   participant, trial, stimulus, or condition columns.
#' @param time_col Optional character scalar. If supplied, observations are
#'   ordered by this column within each group before transitions are computed.
#' @param include_missing Logical. If `TRUE`, missing or empty AOI labels are
#'   retained as `missing_label`; otherwise they are removed.
#' @param missing_label Character scalar used when `include_missing = TRUE`.
#' @param collapse_repeats Logical. If `TRUE`, consecutive identical AOI labels
#'   are collapsed before transition entropy is computed.
#' @param log_base Numeric scalar. Base of the logarithm used for entropy.
#'
#' @return A data frame with one row per group and entropy/count columns.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   subject = "S01",
#'   trial = "T01",
#'   time = 1:6,
#'   AOI = c("A", "A", "B", "C", "B", "A")
#' )
#'
#' compute_gazepoint_aoi_entropy(
#'   dat,
#'   aoi_col = "AOI",
#'   group_cols = c("subject", "trial"),
#'   time_col = "time"
#' )
compute_gazepoint_aoi_entropy <- function(data,
                                          aoi_col,
                                          group_cols = NULL,
                                          time_col = NULL,
                                          include_missing = FALSE,
                                          missing_label = "missing",
                                          collapse_repeats = FALSE,
                                          log_base = 2) {
  .gp3_sequence_check_data(data)
  .gp3_sequence_check_scalar_string(aoi_col, "aoi_col")
  .gp3_sequence_check_character_vector(group_cols, "group_cols", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(time_col, "time_col", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(missing_label, "missing_label")

  if (!is.logical(include_missing) || length(include_missing) != 1L ||
      is.na(include_missing)) {
    stop("`include_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(collapse_repeats) || length(collapse_repeats) != 1L ||
      is.na(collapse_repeats)) {
    stop("`collapse_repeats` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(log_base) || length(log_base) != 1L ||
      is.na(log_base) || !is.finite(log_base) || log_base <= 0 ||
      log_base == 1) {
    stop("`log_base` must be a positive finite numeric scalar other than 1.",
         call. = FALSE)
  }

  .gp3_sequence_check_columns(data, c(aoi_col, group_cols, time_col))

  groups <- .gp3_sequence_split_groups(data, group_cols)

  rows <- lapply(groups, function(dat) {
    dat <- .gp3_sequence_order_data(dat, time_col)
    group_values <- .gp3_sequence_group_values(dat, group_cols)

    aoi <- .gp3_sequence_prepare_aoi(
      dat[[aoi_col]],
      include_missing = include_missing,
      missing_label = missing_label
    )

    if (isTRUE(collapse_repeats)) {
      aoi <- .gp3_sequence_collapse_repeats(aoi)
    }

    n_observations <- length(aoi)
    n_aoi <- length(unique(aoi))

    if (n_observations == 0L) {
      metrics <- data.frame(
        n_observations = 0L,
        n_aoi = 0L,
        spatial_entropy = NA_real_,
        spatial_entropy_norm = NA_real_,
        n_transitions = 0L,
        n_transition_types = 0L,
        transition_entropy = NA_real_,
        transition_entropy_norm = NA_real_,
        conditional_transition_entropy = NA_real_,
        conditional_transition_entropy_norm = NA_real_,
        entropy_status = "no_valid_aoi",
        stringsAsFactors = FALSE
      )

      return(cbind(group_values, metrics))
    }

    spatial_counts <- table(aoi)
    spatial_entropy <- .gp3_sequence_entropy_value(
      spatial_counts,
      log_base = log_base
    )

    spatial_entropy_norm <- .gp3_sequence_normalized_entropy(
      spatial_entropy,
      n_aoi,
      log_base = log_base
    )

    if (n_observations < 2L) {
      metrics <- data.frame(
        n_observations = n_observations,
        n_aoi = n_aoi,
        spatial_entropy = spatial_entropy,
        spatial_entropy_norm = spatial_entropy_norm,
        n_transitions = 0L,
        n_transition_types = 0L,
        transition_entropy = NA_real_,
        transition_entropy_norm = NA_real_,
        conditional_transition_entropy = NA_real_,
        conditional_transition_entropy_norm = NA_real_,
        entropy_status = "no_transitions",
        stringsAsFactors = FALSE
      )

      return(cbind(group_values, metrics))
    }

    from <- utils::head(aoi, -1L)
    to <- utils::tail(aoi, -1L)
    transitions <- paste(from, to, sep = " -> ")

    transition_counts <- table(transitions)
    transition_entropy <- .gp3_sequence_entropy_value(
      transition_counts,
      log_base = log_base
    )

    n_transition_types <- length(transition_counts)
    n_transitions <- length(transitions)

    transition_entropy_norm <- .gp3_sequence_normalized_entropy(
      transition_entropy,
      n_transition_types,
      log_base = log_base
    )

    from_levels <- unique(from)
    conditional_parts <- vapply(from_levels, function(level) {
      idx <- from == level
      weight <- sum(idx) / length(from)
      weight * .gp3_sequence_entropy_value(table(to[idx]), log_base = log_base)
    }, numeric(1))

    conditional_entropy <- sum(conditional_parts)

    conditional_entropy_norm <- .gp3_sequence_normalized_entropy(
      conditional_entropy,
      n_aoi,
      log_base = log_base
    )

    metrics <- data.frame(
      n_observations = n_observations,
      n_aoi = n_aoi,
      spatial_entropy = spatial_entropy,
      spatial_entropy_norm = spatial_entropy_norm,
      n_transitions = n_transitions,
      n_transition_types = n_transition_types,
      transition_entropy = transition_entropy,
      transition_entropy_norm = transition_entropy_norm,
      conditional_transition_entropy = conditional_entropy,
      conditional_transition_entropy_norm = conditional_entropy_norm,
      entropy_status = "ok",
      stringsAsFactors = FALSE
    )

    cbind(group_values, metrics)
  })

  .gp3_sequence_bind_rows(rows)
}
