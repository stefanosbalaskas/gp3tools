
#' Compute AOI sequence metrics
#'
#' Computes compact scanpath-style descriptors from Gazepoint AOI sequences,
#' including sequence length, AOI visits, transitions, revisits, first and last
#' AOI, dominant AOI, and run-length summaries.
#'
#' @param data A data frame containing AOI observations.
#' @param aoi_col Character scalar. Column containing AOI labels.
#' @param group_cols Optional character vector of grouping columns.
#' @param time_col Optional character scalar. If supplied, observations are
#'   ordered by this column within each group.
#' @param include_missing Logical. If `TRUE`, missing or empty AOI labels are
#'   retained as `missing_label`; otherwise they are removed.
#' @param missing_label Character scalar used when `include_missing = TRUE`.
#' @param collapse_repeats Logical. If `TRUE`, consecutive identical AOI labels
#'   are collapsed before visit, transition, and revisit metrics are computed.
#'
#' @return A data frame with one row per group and sequence-metric columns.
#' @export
#'
#' @examples
#' dat <- data.frame(
#'   subject = "S01",
#'   trial = "T01",
#'   time = 1:6,
#'   AOI = c("A", "A", "B", "A", "C", "C")
#' )
#'
#' compute_gazepoint_aoi_sequence_metrics(
#'   dat,
#'   aoi_col = "AOI",
#'   group_cols = c("subject", "trial"),
#'   time_col = "time"
#' )
compute_gazepoint_aoi_sequence_metrics <- function(data,
                                                   aoi_col,
                                                   group_cols = NULL,
                                                   time_col = NULL,
                                                   include_missing = FALSE,
                                                   missing_label = "missing",
                                                   collapse_repeats = TRUE) {
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

  .gp3_sequence_check_columns(data, c(aoi_col, group_cols, time_col))

  groups <- .gp3_sequence_split_groups(data, group_cols)

  rows <- lapply(groups, function(dat) {
    dat <- .gp3_sequence_order_data(dat, time_col)
    group_values <- .gp3_sequence_group_values(dat, group_cols)

    raw_sequence <- .gp3_sequence_prepare_aoi(
      dat[[aoi_col]],
      include_missing = include_missing,
      missing_label = missing_label
    )

    sequence_length <- length(raw_sequence)

    if (sequence_length == 0L) {
      metrics <- data.frame(
        sequence_length = 0L,
        n_aoi_visits = 0L,
        n_unique_aoi = 0L,
        transition_count = 0L,
        revisit_count = NA_integer_,
        revisit_prop = NA_real_,
        dominant_aoi = NA_character_,
        first_aoi = NA_character_,
        last_aoi = NA_character_,
        mean_run_length = NA_real_,
        max_run_length = NA_real_,
        sequence_status = "no_valid_aoi",
        stringsAsFactors = FALSE
      )

      return(cbind(group_values, metrics))
    }

    runs <- rle(raw_sequence)
    analysis_sequence <- if (isTRUE(collapse_repeats)) {
      runs$values
    } else {
      raw_sequence
    }

    visit_count <- length(analysis_sequence)
    transition_count <- max(visit_count - 1L, 0L)
    revisit_count <- sum(duplicated(analysis_sequence))
    revisit_prop <- if (visit_count > 0L) revisit_count / visit_count else NA_real_

    raw_counts <- table(raw_sequence)
    dominant_aoi <- names(raw_counts)[which.max(raw_counts)]

    metrics <- data.frame(
      sequence_length = sequence_length,
      n_aoi_visits = visit_count,
      n_unique_aoi = length(unique(analysis_sequence)),
      transition_count = transition_count,
      revisit_count = revisit_count,
      revisit_prop = revisit_prop,
      dominant_aoi = dominant_aoi,
      first_aoi = analysis_sequence[[1L]],
      last_aoi = analysis_sequence[[length(analysis_sequence)]],
      mean_run_length = mean(runs$lengths),
      max_run_length = max(runs$lengths),
      sequence_status = "ok",
      stringsAsFactors = FALSE
    )

    cbind(group_values, metrics)
  })

  .gp3_sequence_bind_rows(rows)
}
