
#' Compute AOI sequence distance
#'
#' Computes a lightweight edit distance between two AOI sequences using a
#' vector-based Levenshtein distance. This provides a simple scanpath
#' dissimilarity measure without requiring heavy sequence-analysis dependencies.
#'
#' @param sequence_a Character, factor, or atomic vector representing the first
#'   AOI sequence.
#' @param sequence_b Character, factor, or atomic vector representing the second
#'   AOI sequence.
#' @param ignore_missing Logical. If `TRUE`, missing and empty labels are removed.
#' @param missing_label Character scalar used when `ignore_missing = FALSE`.
#' @param collapse_repeats Logical. If `TRUE`, consecutive identical labels are
#'   collapsed before distance is computed.
#' @param substitution_cost Numeric scalar substitution cost.
#' @param insertion_cost Numeric scalar insertion cost.
#' @param deletion_cost Numeric scalar deletion cost.
#'
#' @return A one-row data frame with edit distance, normalized distance, and
#' sequence lengths.
#' @export
#'
#' @examples
#' compute_gazepoint_sequence_distance(
#'   sequence_a = c("Claim", "Evidence", "CTA"),
#'   sequence_b = c("Claim", "CTA", "Evidence")
#' )
compute_gazepoint_sequence_distance <- function(sequence_a,
                                                sequence_b,
                                                ignore_missing = TRUE,
                                                missing_label = "missing",
                                                collapse_repeats = FALSE,
                                                substitution_cost = 1,
                                                insertion_cost = 1,
                                                deletion_cost = 1) {
  if (!is.logical(ignore_missing) || length(ignore_missing) != 1L ||
      is.na(ignore_missing)) {
    stop("`ignore_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(collapse_repeats) || length(collapse_repeats) != 1L ||
      is.na(collapse_repeats)) {
    stop("`collapse_repeats` must be TRUE or FALSE.", call. = FALSE)
  }

  .gp3_sequence_check_scalar_string(missing_label, "missing_label")

  costs <- c(
    substitution_cost = substitution_cost,
    insertion_cost = insertion_cost,
    deletion_cost = deletion_cost
  )

  if (any(!is.finite(costs)) || any(costs < 0)) {
    stop("Edit costs must be non-negative finite numeric values.", call. = FALSE)
  }

  sequence_a <- .gp3_sequence_prepare_aoi(
    sequence_a,
    include_missing = !ignore_missing,
    missing_label = missing_label
  )

  sequence_b <- .gp3_sequence_prepare_aoi(
    sequence_b,
    include_missing = !ignore_missing,
    missing_label = missing_label
  )

  if (isTRUE(collapse_repeats)) {
    sequence_a <- .gp3_sequence_collapse_repeats(sequence_a)
    sequence_b <- .gp3_sequence_collapse_repeats(sequence_b)
  }

  distance <- .gp3_sequence_levenshtein(
    sequence_a,
    sequence_b,
    substitution_cost = substitution_cost,
    insertion_cost = insertion_cost,
    deletion_cost = deletion_cost
  )

  max_length <- max(length(sequence_a), length(sequence_b))
  normalized_distance <- if (max_length == 0L) 0 else distance / max_length

  data.frame(
    edit_distance = as.numeric(distance),
    normalized_distance = as.numeric(normalized_distance),
    sequence_a_length = length(sequence_a),
    sequence_b_length = length(sequence_b),
    distance_status = "ok",
    stringsAsFactors = FALSE
  )
}
