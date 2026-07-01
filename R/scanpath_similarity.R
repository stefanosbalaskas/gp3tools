#' Compute AOI scanpath similarity
#'
#' Compute pairwise AOI-sequence similarity between grouped scanpaths using
#' a lightweight Levenshtein edit-distance implementation. Similarity is
#' reported as \code{1 - normalized_distance}, where normalized distance is
#' divided by the longer sequence length.
#'
#' @param data A data frame containing AOI observations.
#' @param aoi_col Name of the AOI column.
#' @param group_cols Columns defining each scanpath, for example subject and
#'   trial.
#' @param time_col Optional time/order column.
#' @param include_missing Should missing AOI labels be retained as a state?
#' @param missing_label Label used when retaining missing AOIs.
#' @param collapse_repeats Should consecutive repeated AOI labels be collapsed?
#' @param max_sequences Maximum number of grouped sequences to compare.
#'
#' @return A long-format data frame containing pairwise edit distances,
#'   normalized distances, and similarities.
#' @export
compute_gazepoint_scanpath_similarity <- function(data,
                                                  aoi_col,
                                                  group_cols,
                                                  time_col = NULL,
                                                  include_missing = FALSE,
                                                  missing_label = "missing",
                                                  collapse_repeats = FALSE,
                                                  max_sequences = 200) {
  .gp3_ext_check_data(data)
  aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
  group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  if (!is.null(time_col)) {
    time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  }
  .gp3_ext_check_columns(data, c(aoi_col, group_cols, time_col))
  if (!is.numeric(max_sequences) || length(max_sequences) != 1L ||
      is.na(max_sequences) || max_sequences < 2L) {
    stop("max_sequences must be a number of at least 2.", call. = FALSE)
  }

  groups <- .gp3_ext_split_groups(data, group_cols)
  if (length(groups) > max_sequences) {
    stop("Too many grouped sequences. Increase max_sequences if this is intentional.", call. = FALSE)
  }

  seqs <- lapply(groups, function(group_data) {
    group_data <- .gp3_ext_order_data(group_data, time_col)
    aoi <- .gp3_ext_prepare_aoi(group_data[[aoi_col]], include_missing, missing_label)
    if (isTRUE(collapse_repeats)) .gp3_ext_collapse_repeats(aoi) else aoi
  })
  ids <- vapply(groups, .gp3_ext_group_key, character(1), group_cols = group_cols)

  pairs <- utils::combn(seq_along(seqs), 2L, simplify = FALSE)
  self_pairs <- lapply(seq_along(seqs), function(i) c(i, i))
  all_pairs <- c(self_pairs, pairs)

  rows <- lapply(all_pairs, function(pair) {
    i <- pair[1L]
    j <- pair[2L]
    d <- .gp3_sequence_levenshtein(seqs[[i]], seqs[[j]])
    denom <- max(length(seqs[[i]]), length(seqs[[j]]))
    norm <- if (denom == 0L) 0 else d / denom
    data.frame(
      sequence_a = ids[i],
      sequence_b = ids[j],
      edit_distance = d,
      normalized_distance = norm,
      similarity = 1 - norm,
      sequence_a_length = length(seqs[[i]]),
      sequence_b_length = length(seqs[[j]]),
      n_sequences = length(seqs),
      similarity_status = "ok",
      stringsAsFactors = FALSE
    )
  })
  out <- .gp3_ext_bind_rows(rows)
  rownames(out) <- NULL
  out
}
