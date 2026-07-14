#' Interpolate pupil values across detected blink intervals
#'
#' Masks samples inside blink intervals and interpolates only bounded internal
#' gaps. Long or edge gaps remain missing.
#'
#' @param master_df A sample-level data frame.
#' @param blink_df Blink intervals returned by [detect_gazepoint_blinks()].
#' @param pupil_cols Pupil columns to interpolate. When `NULL`, common pupil
#'   columns are detected automatically.
#' @param id_col Participant identifier shared by both inputs.
#' @param group_cols Optional additional grouping columns shared by both inputs.
#' @param ts_col Timestamp column in `master_df`.
#' @param start_col,end_col Blink interval boundaries.
#' @param method Interpolation method.
#' @param max_gap_ms Maximum blink duration eligible for interpolation.
#' @param suffix Suffix used for interpolated columns.
#' @param keep_mask Add `blink_interpolated` and `blink_masked` columns.
#' @param time_unit Timestamp unit.
#'
#' @return The sample table with interpolated pupil columns.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = "P01",
#'   TIME = seq(0, 0.09, by = 0.01),
#'   mean_pupil = c(3, 3.1, 3.2, NA, NA, 3.3, 3.4, 3.5, 3.6, 3.7)
#' )
#' blinks <- detect_gazepoint_blinks(
#'   pupil,
#'   min_duration = 10
#' )
#' interpolate_gazepoint_blinks(pupil, blinks)
interpolate_gazepoint_blinks <- function(
  master_df,
  blink_df,
  pupil_cols = NULL,
  id_col = "USER_ID",
  group_cols = NULL,
  ts_col = "TIME",
  start_col = "start_time",
  end_col = "end_time",
  method = c("linear", "spline"),
  max_gap_ms = 500,
  suffix = "_blink_interp",
  keep_mask = TRUE,
  time_unit = c("auto", "seconds", "milliseconds")
) {
  .gp3_hp_assert_data_frame(master_df, "master_df")
  .gp3_hp_assert_data_frame(blink_df, "blink_df")
  method <- match.arg(method)
  time_unit <- match.arg(time_unit)

  pupil_cols <- .gp3_hp_detect_pupil_columns(master_df, pupil_cols)
  group_cols <- unique(c(id_col, group_cols))

  .gp3_hp_assert_columns(
    master_df,
    unique(c(group_cols, pupil_cols, ts_col)),
    "master_df"
  )
  .gp3_hp_assert_columns(
    blink_df,
    unique(c(group_cols, start_col, end_col)),
    "blink_df"
  )

  if (!is.numeric(max_gap_ms) || length(max_gap_ms) != 1L ||
      !is.finite(max_gap_ms) || max_gap_ms < 0) {
    stop("`max_gap_ms` must be one finite non-negative number.",
         call. = FALSE)
  }

  output <- master_df
  output$blink_masked <- FALSE
  output$blink_interpolated <- FALSE

  for (column in pupil_cols) {
    output[[paste0(column, suffix)]] <- suppressWarnings(
      as.numeric(master_df[[column]])
    )
  }

  master_groups <- .gp3_hp_split_indices(master_df, group_cols)
  master_keys <- names(master_groups)
  blink_keys <- .gp3_hp_group_keys(blink_df, group_cols)

  for (key_i in seq_along(master_groups)) {
    idx <- master_groups[[key_i]]
    key <- master_keys[key_i]
    blink_idx <- which(blink_keys == key)

    if (!length(blink_idx)) {
      next
    }

    ord <- order(master_df[[ts_col]][idx], na.last = TRUE)
    gi <- idx[ord]
    time_raw <- suppressWarnings(as.numeric(master_df[[ts_col]][gi]))
    time_info <- .gp3_hp_time_info(time_raw, time_unit)
    time_sec <- time_raw * time_info$to_seconds

    blink_start_raw <- suppressWarnings(
      as.numeric(blink_df[[start_col]][blink_idx])
    )
    blink_end_raw <- suppressWarnings(
      as.numeric(blink_df[[end_col]][blink_idx])
    )
    blink_start_sec <- blink_start_raw * time_info$to_seconds
    blink_end_sec <- blink_end_raw * time_info$to_seconds

    mask <- rep(FALSE, length(gi))
    eligible <- rep(FALSE, length(gi))

    for (blink_i in seq_along(blink_idx)) {
      if (!is.finite(blink_start_sec[blink_i]) ||
          !is.finite(blink_end_sec[blink_i])) {
        next
      }

      interval <- time_sec >= blink_start_sec[blink_i] &
        time_sec <= blink_end_sec[blink_i]
      interval[is.na(interval)] <- FALSE
      mask <- mask | interval

      duration_ms <- max(
        0,
        blink_end_sec[blink_i] - blink_start_sec[blink_i]
      ) * 1000

      if (duration_ms <= max_gap_ms) {
        eligible <- eligible | interval
      }
    }

    output$blink_masked[gi] <- mask

    for (column in pupil_cols) {
      original <- suppressWarnings(as.numeric(master_df[[column]][gi]))
      masked <- original
      masked[mask] <- NA_real_

      filled <- .gp3_hp_interpolate_series(
        time_sec,
        masked,
        method = method
      )

      replace <- eligible & is.finite(filled)
      target_col <- paste0(column, suffix)
      output[[target_col]][gi] <- masked
      output[[target_col]][gi[replace]] <- filled[replace]
      output$blink_interpolated[gi[replace]] <- TRUE
    }
  }

  if (!isTRUE(keep_mask)) {
    output$blink_masked <- NULL
    output$blink_interpolated <- NULL
  }

  attr(output, "gazepoint_blink_interpolation") <- list(
    pupil_cols = pupil_cols,
    method = method,
    max_gap_ms = max_gap_ms,
    suffix = suffix
  )

  .gp3_hp_restore_class(output, master_df)
}


#' Fuse binocular pupil traces using cross-eye regression
#'
#' Fits cross-eye regressions within independent sequences and creates a
#' regression-smoothed binocular pupil trace. The function is diagnostic and
#' preprocessing-oriented; it does not imply that one eye causally predicts the
#' other.
#'
#' @param master_df A sample-level pupil data frame.
#' @param lp_col,rp_col Left- and right-pupil columns.
#' @param id_col Participant identifier.
#' @param group_cols Optional additional independent-sequence columns.
#' @param direction Regression direction. `"bidirectional"` fits both
#'   right-on-left and left-on-right models.
#' @param output_col Name of the fused pupil column.
#' @param residual_col Name of the right-on-left residual column.
#' @param min_complete Minimum complete binocular samples required for
#'   regression. Groups below this threshold use the binocular mean.
#'
#' @return The input data with regression-smoothed pupil columns.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = rep("P01", 20),
#'   LPupil = seq(3, 4, length.out = 20),
#'   RPupil = seq(3.1, 4.1, length.out = 20)
#' )
#' regress_gazepoint_pupils(pupil)
regress_gazepoint_pupils <- function(
  master_df,
  lp_col = "LPupil",
  rp_col = "RPupil",
  id_col = "USER_ID",
  group_cols = NULL,
  direction = c("bidirectional", "right_on_left", "left_on_right"),
  output_col = "pupil_regressed",
  residual_col = "pupil_regression_residual",
  min_complete = 10
) {
  .gp3_hp_assert_data_frame(master_df, "master_df")
  direction <- match.arg(direction)
  group_cols <- unique(c(id_col, group_cols))
  .gp3_hp_assert_columns(
    master_df,
    unique(c(group_cols, lp_col, rp_col)),
    "master_df"
  )

  if (!is.numeric(min_complete) || length(min_complete) != 1L ||
      !is.finite(min_complete) || min_complete < 2 ||
      min_complete != as.integer(min_complete)) {
    stop("`min_complete` must be an integer of at least 2.",
         call. = FALSE)
  }
  min_complete <- as.integer(min_complete)

  output <- master_df
  output[[output_col]] <- NA_real_
  output[[residual_col]] <- NA_real_
  output$pupil_regression_n <- NA_integer_
  output$pupil_regression_method <- NA_character_

  groups <- .gp3_hp_split_indices(master_df, group_cols)

  for (idx in groups) {
    left <- suppressWarnings(as.numeric(master_df[[lp_col]][idx]))
    right <- suppressWarnings(as.numeric(master_df[[rp_col]][idx]))
    complete <- is.finite(left) & is.finite(right)
    n_complete <- sum(complete)

    fallback <- .gp3_hp_row_mean_two(left, right)
    fused <- fallback
    residual <- rep(NA_real_, length(idx))
    method_used <- "binocular_mean_fallback"

    can_fit <- n_complete >= min_complete &&
      stats::sd(left[complete]) > 0 &&
      stats::sd(right[complete]) > 0

    if (can_fit) {
      right_fit <- .gp3_hp_fit_line(left[complete], right[complete])
      left_fit <- .gp3_hp_fit_line(right[complete], left[complete])

      predicted_right <- rep(NA_real_, length(idx))
      predicted_left <- rep(NA_real_, length(idx))

      finite_left <- is.finite(left)
      finite_right <- is.finite(right)

      predicted_right[finite_left] <-
        right_fit[1L] + right_fit[2L] * left[finite_left]
      predicted_left[finite_right] <-
        left_fit[1L] + left_fit[2L] * right[finite_right]

      residual[finite_right & finite_left] <-
        right[finite_right & finite_left] -
        predicted_right[finite_right & finite_left]

      if (direction == "bidirectional") {
        fused <- .gp3_hp_row_mean_two(predicted_left, predicted_right)
      } else if (direction == "right_on_left") {
        fused <- predicted_right
      } else {
        fused <- predicted_left
      }

      missing_fused <- !is.finite(fused)
      fused[missing_fused] <- fallback[missing_fused]
      method_used <- direction
    }

    output[[output_col]][idx] <- fused
    output[[residual_col]][idx] <- residual
    output$pupil_regression_n[idx] <- n_complete
    output$pupil_regression_method[idx] <- method_used
  }

  attr(output, "gazepoint_pupil_regression") <- list(
    lp_col = lp_col,
    rp_col = rp_col,
    direction = direction,
    output_col = output_col,
    group_cols = group_cols
  )

  .gp3_hp_restore_class(output, master_df)
}


#' Calculate mean binocular pupil size
#'
#' @param master_df A sample-level pupil data frame.
#' @param lp_col,rp_col Left- and right-pupil columns.
#' @param output_col Name of the generated column.
#' @param min_eyes Minimum number of finite eye measurements required.
#'
#' @return The input data with a binocular mean-pupil column.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   LPupil = c(3, NA, 4),
#'   RPupil = c(3.2, 3.5, NA)
#' )
#' mean_gazepoint_pupil(pupil)
mean_gazepoint_pupil <- function(
  master_df,
  lp_col = "LPupil",
  rp_col = "RPupil",
  output_col = "mean_pupil",
  min_eyes = 1
) {
  .gp3_hp_assert_data_frame(master_df, "master_df")
  .gp3_hp_assert_columns(master_df, c(lp_col, rp_col), "master_df")

  if (!min_eyes %in% c(1, 2)) {
    stop("`min_eyes` must be 1 or 2.", call. = FALSE)
  }

  left <- suppressWarnings(as.numeric(master_df[[lp_col]]))
  right <- suppressWarnings(as.numeric(master_df[[rp_col]]))
  available <- is.finite(left) + is.finite(right)
  value <- .gp3_hp_row_mean_two(left, right)
  value[available < min_eyes] <- NA_real_

  output <- master_df
  output[[output_col]] <- value

  attr(output, "gazepoint_mean_pupil") <- list(
    lp_col = lp_col,
    rp_col = rp_col,
    output_col = output_col,
    min_eyes = min_eyes
  )

  .gp3_hp_restore_class(output, master_df)
}


#' Downsample pupil data by integer-factor aggregation
#'
#' Aggregates consecutive samples within independent sequences. The default
#' mean method is safer than simple decimation because it reduces aliasing and
#' preserves a representative timestamp for each bin.
#'
#' @param master_df A sample-level data frame.
#' @param factor Positive integer downsampling factor.
#' @param pupil_cols Pupil columns to aggregate. When `NULL`, common pupil
#'   columns are detected automatically.
#' @param id_col Participant identifier.
#' @param group_cols Optional additional independent-sequence columns.
#' @param ts_col Optional timestamp column. Its bin value is the finite mean.
#' @param method `"mean"` aggregation or `"first"`-sample decimation.
#' @param keep_bin Keep the generated downsample-bin identifier.
#'
#' @return A downsampled data frame.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = "P01",
#'   TIME = seq(0, 0.09, by = 0.01),
#'   mean_pupil = seq(3, 4, length.out = 10)
#' )
#' downsample_gazepoint_pupil(pupil, factor = 2)
downsample_gazepoint_pupil <- function(
  master_df,
  factor = 2,
  pupil_cols = NULL,
  id_col = "USER_ID",
  group_cols = NULL,
  ts_col = "TIME",
  method = c("mean", "first"),
  keep_bin = FALSE
) {
  .gp3_hp_assert_data_frame(master_df, "master_df")
  method <- match.arg(method)

  if (!is.numeric(factor) || length(factor) != 1L ||
      !is.finite(factor) || factor < 1 ||
      factor != as.integer(factor)) {
    stop("`factor` must be one positive integer.", call. = FALSE)
  }
  factor <- as.integer(factor)

  pupil_cols <- .gp3_hp_detect_pupil_columns(master_df, pupil_cols)
  group_cols <- unique(c(id_col, group_cols))
  required <- unique(c(group_cols, pupil_cols))
  if (!is.null(ts_col)) required <- unique(c(required, ts_col))
  .gp3_hp_assert_columns(master_df, required, "master_df")

  groups <- .gp3_hp_split_indices(master_df, group_cols)
  output_rows <- list()
  counter <- 0L

  for (idx in groups) {
    if (!is.null(ts_col)) {
      ord <- order(master_df[[ts_col]][idx], na.last = TRUE)
      idx <- idx[ord]
    }

    bin <- (seq_along(idx) - 1L) %/% factor + 1L
    bin_split <- split(idx, bin)

    for (bin_name in names(bin_split)) {
      bin_idx <- bin_split[[bin_name]]
      row <- master_df[bin_idx[1L], , drop = FALSE]

      if (method == "mean") {
        for (column in pupil_cols) {
          values <- suppressWarnings(
            as.numeric(master_df[[column]][bin_idx])
          )
          row[[column]] <- if (any(is.finite(values))) {
            mean(values[is.finite(values)])
          } else {
            NA_real_
          }
        }

        if (!is.null(ts_col)) {
          time_values <- suppressWarnings(
            as.numeric(master_df[[ts_col]][bin_idx])
          )
          row[[ts_col]] <- if (any(is.finite(time_values))) {
            mean(time_values[is.finite(time_values)])
          } else {
            NA_real_
          }
        }
      }

      row$n_samples_aggregated <- length(bin_idx)
      row$downsample_factor <- factor
      row$downsample_bin <- as.integer(bin_name)

      counter <- counter + 1L
      output_rows[[counter]] <- row
    }
  }

  output <- if (length(output_rows)) {
    do.call(rbind, output_rows)
  } else {
    master_df[0, , drop = FALSE]
  }
  rownames(output) <- NULL

  if (!isTRUE(keep_bin) && "downsample_bin" %in% names(output)) {
    output$downsample_bin <- NULL
  }

  attr(output, "gazepoint_downsampling") <- list(
    factor = factor,
    method = method,
    pupil_cols = pupil_cols,
    group_cols = group_cols
  )

  .gp3_hp_restore_class(output, master_df)
}
