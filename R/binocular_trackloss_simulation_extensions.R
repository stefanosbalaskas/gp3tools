#' Combine left and right Gazepoint eye channels
#'
#' Combines two numeric eye-specific columns into a single analysis column.
#' The helper is intentionally simple and transparent: it can average available
#' left/right values, prefer one eye with fallback to the other, or select the
#' globally less-missing eye as a pragmatic "best eye" rule.
#'
#' @param data A data frame.
#' @param left_col,right_col Character names of the left- and right-eye columns.
#' @param output_col Character name of the combined output column.
#' @param method Combination rule. One of `"mean"`, `"left"`, `"right"`,
#'   `"prefer_left"`, `"prefer_right"`, or `"best"`.
#' @param valid_min,valid_max Optional numeric bounds. Values outside these
#'   bounds are treated as missing before combination.
#'
#' @return A copy of `data` with `output_col` added.
#' @export
#'
#' @examples
#' x <- data.frame(left_pupil = c(3.1, NA, 3.4), right_pupil = c(3.3, 3.2, NA))
#' combine_gazepoint_eyes(x, "left_pupil", "right_pupil", "pupil")
combine_gazepoint_eyes <- function(data,
                                   left_col,
                                   right_col,
                                   output_col = "combined_eye",
                                   method = c(
                                     "mean",
                                     "left",
                                     "right",
                                     "prefer_left",
                                     "prefer_right",
                                     "best"
                                   ),
                                   valid_min = NULL,
                                   valid_max = NULL) {
  method <- match.arg(method)

  .gp3_require_data_frame(data, "data")
  .gp3_require_columns(data, c(left_col, right_col), "data")

  left <- suppressWarnings(as.numeric(data[[left_col]]))
  right <- suppressWarnings(as.numeric(data[[right_col]]))

  left <- .gp3_apply_numeric_bounds(left, valid_min, valid_max)
  right <- .gp3_apply_numeric_bounds(right, valid_min, valid_max)

  combined <- switch(
    method,
    mean = {
      out <- rowMeans(cbind(left, right), na.rm = TRUE)
      out[is.nan(out)] <- NA_real_
      out
    },
    left = left,
    right = right,
    prefer_left = ifelse(!is.na(left), left, right),
    prefer_right = ifelse(!is.na(right), right, left),
    best = {
      left_missing <- mean(is.na(left))
      right_missing <- mean(is.na(right))

      if (left_missing <= right_missing) {
        ifelse(!is.na(left), left, right)
      } else {
        ifelse(!is.na(right), right, left)
      }
    }
  )

  data[[output_col]] <- combined
  data
}


#' Flag or filter Gazepoint data by trackloss
#'
#' Computes trackloss rates globally or within user-specified grouping columns,
#' then flags or removes groups exceeding a transparent threshold. Trackloss can
#' be supplied directly through a validity/tracking column or inferred from
#' missing/out-of-range gaze coordinates.
#'
#' @param data A data frame.
#' @param group_cols Optional character vector of grouping columns, for example
#'   participant and trial identifiers.
#' @param tracking_col Optional tracking/validity column. Logical, numeric, and
#'   character encodings are supported.
#' @param x_col,y_col Optional gaze coordinate columns used when `tracking_col`
#'   is not supplied.
#' @param max_trackloss Maximum allowed trackloss proportion per group.
#' @param action Either `"flag"` to retain all rows and add diagnostic columns,
#'   or `"filter"` to remove groups above the threshold.
#' @param treat_zero_zero_as_loss If `TRUE`, `(0, 0)` gaze coordinates are
#'   treated as trackloss when using `x_col` and `y_col`.
#' @param rate_col,exclude_col Names of the added diagnostic columns.
#'
#' @return A data frame with diagnostic columns. If `action = "filter"`, rows
#'   from excluded groups are removed. A compact summary is stored in the
#'   `"gp3_trackloss_summary"` attribute.
#' @export
#'
#' @examples
#' x <- data.frame(
#'   participant = c("P1", "P1", "P2", "P2"),
#'   trial = c(1, 1, 1, 1),
#'   valid = c(1, 0, 1, 1)
#' )
#' clean_gazepoint_by_trackloss(
#'   x,
#'   group_cols = c("participant", "trial"),
#'   tracking_col = "valid",
#'   max_trackloss = 0.25
#' )
clean_gazepoint_by_trackloss <- function(data,
                                         group_cols = NULL,
                                         tracking_col = NULL,
                                         x_col = NULL,
                                         y_col = NULL,
                                         max_trackloss = 0.25,
                                         action = c("flag", "filter"),
                                         treat_zero_zero_as_loss = TRUE,
                                         rate_col = ".gp3_trackloss_rate",
                                         exclude_col = ".gp3_trackloss_exclude") {
  action <- match.arg(action)

  .gp3_require_data_frame(data, "data")

  if (!is.numeric(max_trackloss) || length(max_trackloss) != 1L ||
      is.na(max_trackloss) || max_trackloss < 0 || max_trackloss > 1) {
    stop("`max_trackloss` must be a single numeric value between 0 and 1.", call. = FALSE)
  }

  if (!is.null(group_cols)) {
    .gp3_require_columns(data, group_cols, "data")
  }

  if (!is.null(tracking_col)) {
    .gp3_require_columns(data, tracking_col, "data")
    trackloss <- .gp3_trackloss_from_tracking(data[[tracking_col]])
  } else {
    if (is.null(x_col) || is.null(y_col)) {
      stop("Supply either `tracking_col` or both `x_col` and `y_col`.", call. = FALSE)
    }

    .gp3_require_columns(data, c(x_col, y_col), "data")
    trackloss <- .gp3_trackloss_from_xy(
      data[[x_col]],
      data[[y_col]],
      treat_zero_zero_as_loss = treat_zero_zero_as_loss
    )
  }

  if (is.null(group_cols) || length(group_cols) == 0L) {
    group_id <- rep(".gp3_all_rows", nrow(data))
  } else {
    group_id <- interaction(data[group_cols], drop = TRUE, lex.order = TRUE)
    group_id <- as.character(group_id)
  }

  group_rate <- tapply(trackloss, group_id, mean, na.rm = TRUE)
  group_n <- tapply(trackloss, group_id, length)
  group_lost <- tapply(trackloss, group_id, sum, na.rm = TRUE)

  row_rate <- unname(group_rate[group_id])
  row_exclude <- row_rate > max_trackloss

  out <- data
  out[[rate_col]] <- row_rate
  out[[exclude_col]] <- row_exclude

  summary <- data.frame(
    group_id = names(group_rate),
    n_rows = as.integer(group_n[names(group_rate)]),
    n_trackloss_rows = as.integer(group_lost[names(group_rate)]),
    trackloss_rate = as.numeric(group_rate),
    exclude = as.logical(group_rate > max_trackloss),
    stringsAsFactors = FALSE
  )

  attr(out, "gp3_trackloss_summary") <- summary

  if (identical(action, "filter")) {
    out <- out[!out[[exclude_col]], , drop = FALSE]
    row.names(out) <- NULL
    attr(out, "gp3_trackloss_summary") <- summary
  }

  out
}


#' Simulate Gazepoint-like pupil data
#'
#' Generates a privacy-safe synthetic pupil data set with balanced conditions,
#' left/right pupil channels, a combined pupil column, blink/trackloss flags, and
#' simple gaze coordinates. The generator is intended for examples and unit
#' tests, not for claims about empirical pupil physiology.
#'
#' @param n_subjects Number of synthetic participants.
#' @param n_trials Number of trials per participant.
#' @param n_time_bins Number of time bins per trial.
#' @param conditions Character vector of condition labels.
#' @param baseline_mean Mean pupil size around which synthetic data are generated.
#' @param condition_effect Numeric effect added to non-reference conditions.
#'   If a single value is supplied, it is applied to all non-reference
#'   conditions. If multiple values are supplied, they are recycled across
#'   `conditions`.
#' @param noise_sd Standard deviation of sample-level noise.
#' @param subject_sd Standard deviation of participant-level random offsets.
#' @param blink_probability Probability that a sample is marked as blink/trackloss.
#' @param seed Optional random seed.
#'
#' @return A data frame with synthetic Gazepoint-like pupil and gaze columns.
#' @export
#'
#' @examples
#' simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
simulate_gazepoint_pupil_data <- function(n_subjects = 12,
                                          n_trials = 8,
                                          n_time_bins = 60,
                                          conditions = c("control", "treatment"),
                                          baseline_mean = 3.5,
                                          condition_effect = 0.15,
                                          noise_sd = 0.08,
                                          subject_sd = 0.25,
                                          blink_probability = 0.03,
                                          seed = NULL) {
  .gp3_require_positive_integer(n_subjects, "n_subjects")
  .gp3_require_positive_integer(n_trials, "n_trials")
  .gp3_require_positive_integer(n_time_bins, "n_time_bins")

  if (!is.character(conditions) || length(conditions) < 1L || anyNA(conditions)) {
    stop("`conditions` must be a non-empty character vector without missing values.", call. = FALSE)
  }

  if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      get(".Random.seed", envir = .GlobalEnv)
    } else {
      NULL
    }

    set.seed(seed)

    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  subjects <- sprintf("S%03d", seq_len(n_subjects))
  trials <- seq_len(n_trials)
  time_bins <- seq_len(n_time_bins)

  condition_by_trial <- conditions[((trials - 1L) %% length(conditions)) + 1L]
  condition_lookup <- data.frame(
    trial = trials,
    condition = condition_by_trial,
    stringsAsFactors = FALSE
  )

  grid <- expand.grid(
    subject = subjects,
    trial = trials,
    time_bin = time_bins,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  grid <- merge(grid, condition_lookup, by = "trial", all.x = TRUE, sort = FALSE)
  grid <- grid[order(grid$subject, grid$trial, grid$time_bin), , drop = FALSE]
  row.names(grid) <- NULL

  subject_offsets <- stats::rnorm(n_subjects, mean = 0, sd = subject_sd)
  names(subject_offsets) <- subjects

  condition_values <- rep(condition_effect, length.out = length(conditions))
  condition_values[1L] <- 0
  names(condition_values) <- conditions

  time_scaled <- (grid$time_bin - mean(time_bins)) / max(time_bins)
  smooth_response <- sin(grid$time_bin / max(time_bins) * pi)

  expected_pupil <- baseline_mean +
    subject_offsets[grid$subject] +
    condition_values[grid$condition] * smooth_response +
    0.03 * time_scaled

  pupil_left <- expected_pupil + stats::rnorm(nrow(grid), mean = 0, sd = noise_sd)
  pupil_right <- expected_pupil + stats::rnorm(nrow(grid), mean = 0, sd = noise_sd)

  blink <- stats::runif(nrow(grid)) < blink_probability
  pupil_left[blink] <- NA_real_
  pupil_right[blink] <- NA_real_

  out <- data.frame(
    subject = grid$subject,
    trial = grid$trial,
    condition = grid$condition,
    time_bin = grid$time_bin,
    timestamp_ms = (grid$time_bin - 1L) * 16.67,
    gaze_x = 960 + stats::rnorm(nrow(grid), mean = 0, sd = 120),
    gaze_y = 540 + stats::rnorm(nrow(grid), mean = 0, sd = 80),
    pupil_left = pupil_left,
    pupil_right = pupil_right,
    blink = blink,
    trackloss = blink,
    stringsAsFactors = FALSE
  )

  out <- combine_gazepoint_eyes(
    out,
    left_col = "pupil_left",
    right_col = "pupil_right",
    output_col = "pupil",
    method = "mean"
  )

  out
}


.gp3_require_data_frame <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    stop("`", arg, "` must be a data frame.", call. = FALSE)
  }

  invisible(TRUE)
}


.gp3_require_columns <- function(data, cols, arg = "data") {
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "`", arg, "` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.gp3_apply_numeric_bounds <- function(x, valid_min = NULL, valid_max = NULL) {
  if (!is.null(valid_min)) {
    x[x < valid_min] <- NA_real_
  }

  if (!is.null(valid_max)) {
    x[x > valid_max] <- NA_real_
  }

  x
}


.gp3_trackloss_from_tracking <- function(x) {
  if (is.logical(x)) {
    return(is.na(x) | !x)
  }

  if (is.numeric(x) || is.integer(x)) {
    return(is.na(x) | x <= 0)
  }

  x_chr <- tolower(trimws(as.character(x)))

  is.na(x) |
    x_chr %in% c("", "0", "false", "f", "invalid", "lost", "missing", "na", "nan")
}


.gp3_trackloss_from_xy <- function(x, y, treat_zero_zero_as_loss = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  y <- suppressWarnings(as.numeric(y))

  lost <- !is.finite(x) | !is.finite(y)

  if (isTRUE(treat_zero_zero_as_loss)) {
    lost <- lost | (x == 0 & y == 0)
  }

  lost
}


.gp3_require_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}
