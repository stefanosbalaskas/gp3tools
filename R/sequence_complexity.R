#' Compute AOI sequence complexity metrics
#'
#' Compute compact sequence-complexity summaries from AOI labels. This
#' helper complements transition and entropy summaries by returning simple,
#' interpretable indices such as type-token ratio, transition density,
#' normalized entropy, and a combined complexity index.
#'
#' @param data Optional data frame containing AOI observations.
#' @param sequence Optional AOI sequence vector. Used when \code{data} is not
#'   supplied.
#' @param aoi_col Name of the AOI column when \code{data} is supplied.
#' @param group_cols Optional grouping columns.
#' @param time_col Optional time/order column.
#' @param include_missing Should missing AOI labels be retained as a state?
#' @param missing_label Label used when retaining missing AOIs.
#' @param collapse_repeats Should consecutive repeated AOI labels be collapsed?
#'
#' @return A data frame with sequence length, unique-state count, entropy,
#'   transition density, type-token ratio, and complexity index.
#' @export
compute_gazepoint_sequence_complexity <- function(data = NULL,
                                                 sequence = NULL,
                                                 aoi_col = NULL,
                                                 group_cols = NULL,
                                                 time_col = NULL,
                                                 include_missing = FALSE,
                                                 missing_label = "missing",
                                                 collapse_repeats = FALSE) {
  one_sequence <- function(aoi) {
    aoi <- .gp3_ext_prepare_aoi(aoi, include_missing, missing_label)
    aoi <- if (isTRUE(collapse_repeats)) .gp3_ext_collapse_repeats(aoi) else aoi
    n <- length(aoi)
    n_unique <- length(unique(aoi))
    transitions <- if (n > 1L) sum(aoi[-1L] != aoi[-n]) else 0L
    type_token_ratio <- if (n > 0L) n_unique / n else NA_real_
    transition_density <- if (n > 1L) transitions / (n - 1L) else NA_real_
    entropy_norm <- .gp3_ext_normalized_entropy(aoi, log_base = 2)
    revisit_prop <- if (n > 0L) mean(duplicated(aoi)) else NA_real_
    parts <- c(type_token_ratio, transition_density, entropy_norm)
    complexity_index <- if (all(is.na(parts))) NA_real_ else mean(parts, na.rm = TRUE)
    data.frame(
      sequence_length = n,
      n_unique_aoi = n_unique,
      transition_count = transitions,
      type_token_ratio = type_token_ratio,
      transition_density = transition_density,
      normalized_entropy = entropy_norm,
      revisit_prop = revisit_prop,
      complexity_index = complexity_index,
      complexity_status = if (n == 0L) "empty_sequence" else "ok",
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(data)) {
    .gp3_ext_check_data(data)
    aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
    if (!is.null(group_cols)) {
      group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
    }
    if (!is.null(time_col)) {
      time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
    }
    .gp3_ext_check_columns(data, c(aoi_col, group_cols, time_col))
    groups <- .gp3_ext_split_groups(data, group_cols)
    rows <- lapply(groups, function(group_data) {
      group_data <- .gp3_ext_order_data(group_data, time_col)
      cbind(.gp3_ext_group_values(group_data, group_cols), one_sequence(group_data[[aoi_col]]))
    })
    out <- .gp3_ext_bind_rows(rows)
    rownames(out) <- NULL
    return(out)
  }

  if (is.null(sequence)) {
    stop("Supply either data with aoi_col or a sequence vector.", call. = FALSE)
  }
  one_sequence(sequence)
}
