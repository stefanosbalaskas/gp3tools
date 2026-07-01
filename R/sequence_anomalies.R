#' Flag unusual AOI sequences
#'
#' Identify grouped AOI sequences that are unusually short, unusually long,
#' have high missingness, or contain very few unique AOI states. The function
#' is intended as a lightweight quality-control helper rather than a
#' definitive exclusion rule.
#'
#' @param data A data frame containing AOI observations.
#' @param aoi_col Name of the AOI column.
#' @param group_cols Columns defining each sequence.
#' @param time_col Optional time/order column.
#' @param min_length Minimum acceptable non-missing sequence length.
#' @param max_length Optional maximum acceptable non-missing sequence length.
#' @param max_missing_prop Maximum acceptable missing AOI proportion.
#' @param z_threshold Absolute z-score threshold for unusual sequence length.
#' @param min_unique_aoi Minimum number of unique AOI labels expected.
#'
#' @return A data frame with sequence diagnostics and anomaly flags.
#' @export
flag_gazepoint_sequence_anomalies <- function(data,
                                             aoi_col,
                                             group_cols,
                                             time_col = NULL,
                                             min_length = 2,
                                             max_length = NULL,
                                             max_missing_prop = 0.5,
                                             z_threshold = 3,
                                             min_unique_aoi = 1) {
  .gp3_ext_check_data(data)
  aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
  group_cols <- .gp3_ext_check_character_vector(group_cols, "group_cols")
  if (!is.null(time_col)) {
    time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  }
  .gp3_ext_check_columns(data, c(aoi_col, group_cols, time_col))
  if (!is.numeric(min_length) || length(min_length) != 1L || is.na(min_length) || min_length < 0) {
    stop("min_length must be a non-negative number.", call. = FALSE)
  }
  if (!is.null(max_length) && (!is.numeric(max_length) || length(max_length) != 1L || is.na(max_length))) {
    stop("max_length must be NULL or a single number.", call. = FALSE)
  }
  if (!is.numeric(max_missing_prop) || length(max_missing_prop) != 1L ||
      is.na(max_missing_prop) || max_missing_prop < 0 || max_missing_prop > 1) {
    stop("max_missing_prop must be between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(z_threshold) || length(z_threshold) != 1L || is.na(z_threshold) || z_threshold <= 0) {
    stop("z_threshold must be positive.", call. = FALSE)
  }

  groups <- .gp3_ext_split_groups(data, group_cols)
  rows <- lapply(groups, function(group_data) {
    group_data <- .gp3_ext_order_data(group_data, time_col)
    raw_aoi <- group_data[[aoi_col]]
    missing <- is.na(raw_aoi) | trimws(as.character(raw_aoi)) == ""
    observed <- as.character(raw_aoi[!missing])
    data.frame(
      .gp3_key = .gp3_ext_group_key(group_data, group_cols),
      total_observations = length(raw_aoi),
      sequence_length = length(observed),
      missing_prop = if (length(raw_aoi) == 0L) NA_real_ else mean(missing),
      n_unique_aoi = length(unique(observed)),
      stringsAsFactors = FALSE
    ) |>
      cbind(.gp3_ext_group_values(group_data, group_cols))
  })
  out <- .gp3_ext_bind_rows(rows)
  len_sd <- stats::sd(out$sequence_length, na.rm = TRUE)
  len_mean <- mean(out$sequence_length, na.rm = TRUE)
  out$length_z <- if (is.na(len_sd) || len_sd == 0) 0 else (out$sequence_length - len_mean) / len_sd
  out$flag_short <- out$sequence_length < min_length
  out$flag_long <- if (is.null(max_length)) FALSE else out$sequence_length > max_length
  out$flag_high_missing <- out$missing_prop > max_missing_prop
  out$flag_length_outlier <- abs(out$length_z) > z_threshold
  out$flag_low_unique <- out$n_unique_aoi < min_unique_aoi
  out$anomaly_flag <- out$flag_short | out$flag_long | out$flag_high_missing |
    out$flag_length_outlier | out$flag_low_unique
  reasons <- lapply(seq_len(nrow(out)), function(i) {
    r <- character(0)
    if (isTRUE(out$flag_short[i])) r <- c(r, "short_sequence")
    if (isTRUE(out$flag_long[i])) r <- c(r, "long_sequence")
    if (isTRUE(out$flag_high_missing[i])) r <- c(r, "high_missing")
    if (isTRUE(out$flag_length_outlier[i])) r <- c(r, "length_outlier")
    if (isTRUE(out$flag_low_unique[i])) r <- c(r, "low_unique_aoi")
    if (length(r) == 0L) "none" else paste(r, collapse = ";")
  })
  out$anomaly_reason <- unlist(reasons, use.names = FALSE)
  out$anomaly_status <- "ok"
  rownames(out) <- NULL
  out
}
