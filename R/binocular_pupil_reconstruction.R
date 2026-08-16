# Auditable binocular pupil reconstruction ---------------------------------
#
# This module is intentionally additive. It does not change the behaviour of
# combine_gazepoint_eyes() or regress_gazepoint_pupils(). The functions below
# preserve source pupil channels and make every reconstructed value traceable.

.gp3_binoc_null <- function(x, y) if (is.null(x)) y else x

.gp3_binoc_assert_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_binoc_assert_cols <- function(data, cols, arg = "columns") {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    stop(
      sprintf(
        "Missing %s: %s.",
        arg,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gp3_binoc_assert_numeric_col <- function(data, col) {
  if (!is.numeric(data[[col]])) {
    stop(sprintf("`%s` must be numeric.", col), call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_binoc_assert_scalar_number <- function(x, arg, lower = -Inf, upper = Inf,
                                             inclusive = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    if (inclusive) {
      ok <- x >= lower && x <= upper
    } else {
      ok <- x > lower && x < upper
    }
  }
  if (!ok) {
    stop(
      sprintf("`%s` must be one finite numeric value in the permitted range.", arg),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gp3_binoc_validate_bounds <- function(valid_min, valid_max) {
  if (!is.null(valid_min)) {
    .gp3_binoc_assert_scalar_number(valid_min, "valid_min")
  }
  if (!is.null(valid_max)) {
    .gp3_binoc_assert_scalar_number(valid_max, "valid_max")
  }
  if (!is.null(valid_min) && !is.null(valid_max) && valid_min >= valid_max) {
    stop("`valid_min` must be smaller than `valid_max`.", call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_binoc_observed <- function(x, valid_min = NULL, valid_max = NULL) {
  out <- as.numeric(x)
  out[!is.finite(out)] <- NA_real_
  if (!is.null(valid_min)) {
    out[out < valid_min] <- NA_real_
  }
  if (!is.null(valid_max)) {
    out[out > valid_max] <- NA_real_
  }
  out
}

.gp3_binoc_make_group_key <- function(data, group_cols) {
  if (!length(group_cols)) {
    return(rep("__pooled__", nrow(data)))
  }
  parts <- lapply(group_cols, function(col) {
    x <- as.character(data[[col]])
    x[is.na(x)] <- "<NA>"
    paste0(col, "=", x)
  })
  do.call(paste, c(parts, sep = "||"))
}

.gp3_binoc_group_label <- function(group_cols) {
  if (!length(group_cols)) "pooled" else paste(group_cols, collapse = "+")
}

.gp3_binoc_split_indices <- function(data, group_cols) {
  key <- .gp3_binoc_make_group_key(data, group_cols)
  split(seq_len(nrow(data)), key, drop = TRUE)
}

.gp3_binoc_rbind_fill <- function(parts) {
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, parts)
  if (!length(parts)) {
    return(data.frame())
  }
  all_names <- unique(unlist(lapply(parts, names), use.names = FALSE))
  parts <- lapply(parts, function(x) {
    missing <- setdiff(all_names, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[all_names]
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

.gp3_binoc_time_scale_ms <- function(time, unit = c("auto", "milliseconds", "seconds")) {
  unit <- match.arg(unit)
  if (unit == "milliseconds") return(1)
  if (unit == "seconds") return(1000)
  t <- sort(unique(time[is.finite(time)]))
  if (length(t) < 2L) return(1)
  delta <- stats::median(diff(t), na.rm = TRUE)
  if (!is.finite(delta)) return(1)
  if (delta < 2) 1000 else 1
}

.gp3_binoc_gap_vectors <- function(data, missing, group_cols, time_col = NULL,
                                    time_unit = c("auto", "milliseconds", "seconds")) {
  time_unit <- match.arg(time_unit)
  n <- nrow(data)
  gap_id <- rep(NA_integer_, n)
  gap_ms <- rep(NA_real_, n)
  edge <- rep(FALSE, n)
  gap_parts <- list()
  next_id <- 0L

  groups <- .gp3_binoc_split_indices(data, group_cols)
  for (group_name in names(groups)) {
    idx <- groups[[group_name]]
    if (!is.null(time_col)) {
      t <- data[[time_col]][idx]
      ord <- order(t, seq_along(t), na.last = TRUE)
    } else {
      t <- seq_along(idx)
      ord <- seq_along(idx)
    }
    idx_ord <- idx[ord]
    m <- missing[idx_ord]
    if (!any(m, na.rm = TRUE)) next

    rr <- rle(m)
    ends <- cumsum(rr$lengths)
    starts <- ends - rr$lengths + 1L
    true_runs <- which(rr$values %in% TRUE)
    if (!length(true_runs)) next

    if (!is.null(time_col)) {
      raw_time <- as.numeric(data[[time_col]][idx_ord])
      scale_ms <- .gp3_binoc_time_scale_ms(raw_time, time_unit)
      finite_time <- raw_time[is.finite(raw_time)]
      if (length(finite_time) >= 2L) {
        dt <- stats::median(diff(sort(unique(finite_time))), na.rm = TRUE) * scale_ms
        if (!is.finite(dt) || dt <= 0) dt <- NA_real_
      } else {
        dt <- NA_real_
      }
    } else {
      raw_time <- rep(NA_real_, length(idx_ord))
      scale_ms <- NA_real_
      dt <- NA_real_
    }

    for (run in true_runs) {
      next_id <- next_id + 1L
      pos <- starts[[run]]:ends[[run]]
      rows <- idx_ord[pos]
      is_edge <- starts[[run]] == 1L || ends[[run]] == length(idx_ord)
      if (!is.null(time_col)) {
        tr <- raw_time[pos]
        if (all(is.finite(tr))) {
          duration <- if (length(tr) == 1L) {
            dt
          } else {
            (max(tr) - min(tr)) * scale_ms + ifelse(is.na(dt), 0, dt)
          }
        } else {
          duration <- NA_real_
        }
      } else {
        duration <- NA_real_
      }
      gap_id[rows] <- next_id
      gap_ms[rows] <- duration
      edge[rows] <- is_edge
      gap_parts[[length(gap_parts) + 1L]] <- data.frame(
        gap_id = next_id,
        group_key = group_name,
        n_samples = length(rows),
        gap_ms = duration,
        edge_gap = is_edge,
        start_row = min(rows),
        end_row = max(rows),
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    gap_id = gap_id,
    gap_ms = gap_ms,
    edge_gap = edge,
    gaps = .gp3_binoc_rbind_fill(gap_parts)
  )
}

.gp3_binoc_fit_one <- function(x, y, min_pairs, min_unique, min_r2,
                                allow_negative_slope, max_abs_slope) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  result <- list(
    n_pairs = n,
    intercept = NA_real_,
    slope = NA_real_,
    r_squared = NA_real_,
    adjusted_r_squared = NA_real_,
    rmse = NA_real_,
    mae = NA_real_,
    residual_sd = NA_real_,
    residual_median = NA_real_,
    residual_mad = NA_real_,
    predictor_min = if (n) min(x) else NA_real_,
    predictor_max = if (n) max(x) else NA_real_,
    outcome_min = if (n) min(y) else NA_real_,
    outcome_max = if (n) max(y) else NA_real_,
    eligible = FALSE,
    status = "ineligible",
    reason = "insufficient_paired_samples"
  )
  if (n < min_pairs) return(result)
  if (length(unique(x)) < min_unique || length(unique(y)) < min_unique) {
    result$reason <- "insufficient_unique_values"
    return(result)
  }
  if (!is.finite(stats::var(x)) || stats::var(x) <= .Machine$double.eps) {
    result$reason <- "zero_predictor_variance"
    return(result)
  }
  if (!is.finite(stats::var(y)) || stats::var(y) <= .Machine$double.eps) {
    result$reason <- "zero_outcome_variance"
    return(result)
  }

  X <- cbind(1, x)
  fit <- tryCatch(stats::lm.fit(x = X, y = y), error = function(e) NULL)
  if (is.null(fit) || length(fit$coefficients) != 2L ||
      any(!is.finite(fit$coefficients))) {
    result$reason <- "unstable_linear_fit"
    return(result)
  }

  pred <- as.numeric(X %*% fit$coefficients)
  resid <- y - pred
  sse <- sum(resid^2)
  sst <- sum((y - mean(y))^2)
  r2 <- if (sst > 0) 1 - sse / sst else NA_real_
  adj <- if (is.finite(r2) && n > 2L) 1 - (1 - r2) * (n - 1) / (n - 2) else NA_real_

  result$intercept <- unname(fit$coefficients[[1L]])
  result$slope <- unname(fit$coefficients[[2L]])
  result$r_squared <- r2
  result$adjusted_r_squared <- adj
  result$rmse <- sqrt(mean(resid^2))
  result$mae <- mean(abs(resid))
  result$residual_sd <- if (n > 1L) stats::sd(resid) else NA_real_
  result$residual_median <- stats::median(resid)
  result$residual_mad <- stats::mad(resid, constant = 1)

  if (!allow_negative_slope && result$slope <= 0) {
    result$reason <- "non_positive_slope"
    return(result)
  }
  if (!is.null(max_abs_slope) && abs(result$slope) > max_abs_slope) {
    result$reason <- "slope_outside_limit"
    return(result)
  }
  if (!is.null(min_r2) && (!is.finite(r2) || r2 < min_r2)) {
    result$reason <- "r_squared_below_threshold"
    return(result)
  }

  result$eligible <- TRUE
  result$status <- "eligible"
  result$reason <- NA_character_
  result
}

.gp3_binoc_level_specs <- function(group_cols, fallback_group_cols) {
  if (is.null(fallback_group_cols)) {
    fallback_group_cols <- if (length(group_cols)) list(character(0)) else list()
  }
  if (!is.list(fallback_group_cols)) {
    stop("`fallback_group_cols` must be NULL or a list of character vectors.", call. = FALSE)
  }
  specs <- c(list(group_cols), fallback_group_cols)
  specs <- lapply(specs, unique)
  labels <- vapply(specs, function(x) paste(x, collapse = "\r"), character(1))
  specs[!duplicated(labels)]
}

.gp3_binoc_flatten_models <- function(levels) {
  parts <- lapply(levels, `[[`, "models")
  out <- .gp3_binoc_rbind_fill(parts)
  if (nrow(out)) out$model_index <- seq_len(nrow(out))
  out
}

.gp3_binoc_assign_models <- function(data, calibration, direction) {
  n <- nrow(data)
  selected <- rep(NA_integer_, n)
  models <- calibration$models
  if (!nrow(models)) return(selected)

  for (level in calibration$levels) {
    unresolved <- which(is.na(selected))
    if (!length(unresolved)) break
    level_models <- level$models
    use <- level_models$direction == direction & level_models$eligible
    level_models <- level_models[use, , drop = FALSE]
    if (!nrow(level_models)) next
    key <- .gp3_binoc_make_group_key(data[unresolved, , drop = FALSE], level$group_cols)
    hit <- match(key, level_models$group_key)
    has <- !is.na(hit)
    if (any(has)) {
      model_ids <- level_models$model_id[hit[has]]
      selected[unresolved[has]] <- match(model_ids, models$model_id)
    }
  }
  selected
}

.gp3_binoc_excluded <- function(data, exclude_flag_cols) {
  if (!length(exclude_flag_cols)) return(rep(FALSE, nrow(data)))
  .gp3_binoc_assert_cols(data, exclude_flag_cols, "exclude flag columns")
  flags <- lapply(exclude_flag_cols, function(col) {
    x <- data[[col]]
    if (!(is.logical(x) || is.numeric(x) || is.integer(x))) {
      stop(sprintf("Exclusion flag `%s` must be logical or numeric.", col), call. = FALSE)
    }
    out <- as.logical(x)
    out[is.na(out)] <- FALSE
    out
  })
  Reduce(`|`, flags)
}

.gp3_binoc_check_output_names <- function(data, names_out, overwrite) {
  conflicts <- intersect(names_out, names(data))
  if (length(conflicts) && !isTRUE(overwrite)) {
    stop(
      sprintf(
        "Output columns already exist: %s. Set `overwrite = TRUE` to replace them.",
        paste(conflicts, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Diagnose binocular pupil quality and agreement
#'
#' Quantifies binocular availability, monocular loss, left/right agreement,
#' systematic offset, regression diagnostics, and temporal missingness runs before
#' any model-based reconstruction is attempted. Source pupil columns are never
#' modified.
#'
#' @param data A data frame containing left and right pupil measurements.
#' @param left_col,right_col Names of numeric pupil columns.
#' @param time_col Optional numeric time column. When supplied, gap durations and
#'   time-order diagnostics are reported.
#' @param group_cols Optional grouping columns, typically participant and/or session.
#' @param time_unit Unit for `time_col`: `"auto"`, `"milliseconds"`, or `"seconds"`.
#' @param valid_min,valid_max Optional physiological or instrument-specific bounds.
#'   Values outside the declared bounds are treated as unavailable for diagnostics;
#'   the source data remain unchanged.
#' @param min_pairs Minimum bilateral observations used to label a group as
#'   calibration-eligible.
#' @param min_unique Minimum unique values required in each eye for eligibility.
#' @param disagreement_mad_k Robust multiplier used to describe unusually large
#'   absolute between-eye differences. This is a diagnostic flag, not an exclusion
#'   threshold.
#'
#' @return An object of class `gp3_binocular_diagnostics` containing `summary`,
#'   `gaps`, and `settings` components.
#'
#' @details The Bland-Altman-style limits returned here are descriptive
#'   mean-difference +/- 1.96 SD limits. They do not establish interchangeability
#'   of the two measurements. `correlation` is Pearson correlation and
#'   `rank_correlation` is Spearman correlation on bilateral observations.
#'
#' @seealso [fit_gazepoint_binocular_calibration()],
#'   [reconstruct_gazepoint_binocular_pupil()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 11)
#' dat$pupil_left[25:28] <- NA_real_
#' diagnose_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right",
#'   time_col = "timestamp_ms", group_cols = "subject", min_pairs = 20
#' )
#'
#' @export
diagnose_gazepoint_binocular_pupil <- function(
    data,
    left_col,
    right_col,
    time_col = NULL,
    group_cols = NULL,
    time_unit = c("auto", "milliseconds", "seconds"),
    valid_min = NULL,
    valid_max = NULL,
    min_pairs = 30L,
    min_unique = 5L,
    disagreement_mad_k = 6) {
  .gp3_binoc_assert_data(data)
  time_unit <- match.arg(time_unit)
  group_cols <- unique(.gp3_binoc_null(group_cols, character(0)))
  needed <- c(left_col, right_col, time_col, group_cols)
  .gp3_binoc_assert_cols(data, needed)
  .gp3_binoc_assert_numeric_col(data, left_col)
  .gp3_binoc_assert_numeric_col(data, right_col)
  if (!is.null(time_col)) .gp3_binoc_assert_numeric_col(data, time_col)
  .gp3_binoc_validate_bounds(valid_min, valid_max)
  .gp3_binoc_assert_scalar_number(min_pairs, "min_pairs", lower = 2)
  .gp3_binoc_assert_scalar_number(min_unique, "min_unique", lower = 2)
  .gp3_binoc_assert_scalar_number(disagreement_mad_k, "disagreement_mad_k", lower = 0)

  left <- .gp3_binoc_observed(data[[left_col]], valid_min, valid_max)
  right <- .gp3_binoc_observed(data[[right_col]], valid_min, valid_max)
  left_ok <- is.finite(left)
  right_ok <- is.finite(right)

  left_gap <- .gp3_binoc_gap_vectors(
    data, !left_ok, group_cols, time_col, time_unit
  )
  right_gap <- .gp3_binoc_gap_vectors(
    data, !right_ok, group_cols, time_col, time_unit
  )
  if (nrow(left_gap$gaps)) left_gap$gaps$eye <- "left"
  if (nrow(right_gap$gaps)) right_gap$gaps$eye <- "right"
  gaps <- .gp3_binoc_rbind_fill(list(left_gap$gaps, right_gap$gaps))

  split_idx <- .gp3_binoc_split_indices(data, group_cols)
  rows <- lapply(names(split_idx), function(key) {
    idx <- split_idx[[key]]
    l <- left[idx]
    r <- right[idx]
    bilateral <- is.finite(l) & is.finite(r)
    lb <- l[bilateral]
    rb <- r[bilateral]
    diff_lr <- lb - rb
    abs_diff <- abs(diff_lr)
    n_b <- length(lb)
    disagreement_threshold <- disagreement_fraction <- NA_real_
    if (n_b) {
      center_abs <- stats::median(abs_diff)
      spread_abs <- stats::mad(abs_diff, constant = 1.4826)
      if (is.finite(spread_abs)) {
        disagreement_threshold <- center_abs + disagreement_mad_k * spread_abs
        disagreement_fraction <- mean(abs_diff > disagreement_threshold)
      }
    }

    intercept <- slope <- NA_real_
    if (n_b >= 2L && stats::var(rb) > .Machine$double.eps) {
      fit <- tryCatch(stats::lm.fit(cbind(1, rb), lb), error = function(e) NULL)
      if (!is.null(fit) && length(fit$coefficients) == 2L) {
        intercept <- unname(fit$coefficients[[1L]])
        slope <- unname(fit$coefficients[[2L]])
      }
    }

    if (!is.null(time_col)) {
      tt <- as.numeric(data[[time_col]][idx])
      finite_tt <- tt[is.finite(tt)]
      duplicate_time <- if (length(finite_tt)) sum(duplicated(finite_tt)) else 0L
      unsorted_time <- if (length(finite_tt) > 1L) any(diff(finite_tt) < 0) else FALSE
    } else {
      duplicate_time <- NA_integer_
      unsorted_time <- NA
    }

    out <- data.frame(
      group_key = key,
      n = length(idx),
      n_left = sum(is.finite(l)),
      n_right = sum(is.finite(r)),
      n_bilateral = n_b,
      n_left_only = sum(is.finite(l) & !is.finite(r)),
      n_right_only = sum(!is.finite(l) & is.finite(r)),
      n_both_missing = sum(!is.finite(l) & !is.finite(r)),
      prop_bilateral = mean(is.finite(l) & is.finite(r)),
      prop_left_only = mean(is.finite(l) & !is.finite(r)),
      prop_right_only = mean(!is.finite(l) & is.finite(r)),
      prop_both_missing = mean(!is.finite(l) & !is.finite(r)),
      left_mean = if (any(is.finite(l))) mean(l, na.rm = TRUE) else NA_real_,
      right_mean = if (any(is.finite(r))) mean(r, na.rm = TRUE) else NA_real_,
      left_sd = if (sum(is.finite(l)) > 1L) stats::sd(l, na.rm = TRUE) else NA_real_,
      right_sd = if (sum(is.finite(r)) > 1L) stats::sd(r, na.rm = TRUE) else NA_real_,
      left_median = if (any(is.finite(l))) stats::median(l, na.rm = TRUE) else NA_real_,
      right_median = if (any(is.finite(r))) stats::median(r, na.rm = TRUE) else NA_real_,
      left_mad = if (any(is.finite(l))) stats::mad(l, na.rm = TRUE, constant = 1) else NA_real_,
      right_mad = if (any(is.finite(r))) stats::mad(r, na.rm = TRUE, constant = 1) else NA_real_,
      mean_difference = if (n_b) mean(diff_lr) else NA_real_,
      median_difference = if (n_b) stats::median(diff_lr) else NA_real_,
      correlation = if (n_b > 2L && stats::sd(lb) > 0 && stats::sd(rb) > 0) stats::cor(lb, rb) else NA_real_,
      rank_correlation = if (n_b > 2L) suppressWarnings(stats::cor(lb, rb, method = "spearman")) else NA_real_,
      rmse_between_eyes = if (n_b) sqrt(mean(diff_lr^2)) else NA_real_,
      mae_between_eyes = if (n_b) mean(abs(diff_lr)) else NA_real_,
      disagreement_threshold = disagreement_threshold,
      disagreement_fraction = disagreement_fraction,
      agreement_lower = if (n_b > 1L) mean(diff_lr) - 1.96 * stats::sd(diff_lr) else NA_real_,
      agreement_upper = if (n_b > 1L) mean(diff_lr) + 1.96 * stats::sd(diff_lr) else NA_real_,
      left_from_right_intercept = intercept,
      left_from_right_slope = slope,
      longest_left_gap_ms = if (any(left_gap$gap_id[idx] %in% left_gap$gap_id, na.rm = TRUE)) {
        z <- left_gap$gap_ms[idx]
        if (any(is.finite(z))) max(z, na.rm = TRUE) else NA_real_
      } else NA_real_,
      longest_right_gap_ms = if (any(right_gap$gap_id[idx] %in% right_gap$gap_id, na.rm = TRUE)) {
        z <- right_gap$gap_ms[idx]
        if (any(is.finite(z))) max(z, na.rm = TRUE) else NA_real_
      } else NA_real_,
      duplicate_time_count = duplicate_time,
      time_unsorted = unsorted_time,
      calibration_eligible = n_b >= min_pairs &&
        length(unique(lb)) >= min_unique && length(unique(rb)) >= min_unique &&
        n_b > 1L && is.finite(stats::var(lb)) && stats::var(lb) > .Machine$double.eps &&
        is.finite(stats::var(rb)) && stats::var(rb) > .Machine$double.eps,
      stringsAsFactors = FALSE
    )
    out$status <- if (out$calibration_eligible) "eligible" else "review"
    if (length(group_cols)) {
      for (col in group_cols) out[[col]] <- data[[col]][idx[[1L]]]
      out <- out[c(group_cols, setdiff(names(out), group_cols))]
    }
    out
  })

  structure(
    list(
      summary = tibble::as_tibble(.gp3_binoc_rbind_fill(rows)),
      gaps = tibble::as_tibble(gaps),
      settings = list(
        left_col = left_col,
        right_col = right_col,
        time_col = time_col,
        group_cols = group_cols,
        time_unit = time_unit,
        valid_min = valid_min,
        valid_max = valid_max,
        min_pairs = as.integer(min_pairs),
        min_unique = as.integer(min_unique),
        disagreement_mad_k = disagreement_mad_k
      )
    ),
    class = "gp3_binocular_diagnostics"
  )
}

#' Fit audited cross-eye pupil calibration models
#'
#' Fits separate left-from-right and right-from-left linear calibration models
#' using only bilateral observations. Optional fallback levels permit transparent
#' participant/session-to-pooled fallback without silently mixing calibration
#' scopes.
#'
#' @param data A data frame containing pupil channels.
#' @param left_col,right_col Numeric pupil columns.
#' @param group_cols Primary calibration grouping columns.
#' @param fallback_group_cols Optional list of fallback grouping specifications.
#'   By default a pooled fallback is added when `group_cols` is non-empty. Use
#'   `list()` to disable fallback.
#' @param valid_min,valid_max Optional measurement bounds.
#' @param min_pairs Minimum bilateral training observations per model.
#' @param min_unique Minimum unique predictor and outcome values.
#' @param min_r2 Optional minimum in-sample R-squared. `NULL` reports R-squared
#'   without using it as a gate.
#' @param allow_negative_slope Whether a non-positive cross-eye slope can be
#'   eligible. The conservative default is `FALSE`.
#' @param max_abs_slope Optional absolute slope ceiling; use `NULL` for none.
#'
#' @return An object of class `gp3_binocular_calibration` with a flat `models`
#'   table, per-level model tables, and settings.
#'
#' @details R-squared is a diagnostic, not evidence that reconstructed values are
#'   measured observations. Calibration eligibility is explicit and can be
#'   reviewed before reconstruction.
#'
#' @references Ong J, He W, Maglanque P, Jiang X, Gillman LM, Vergis A,
#'   Hardy K (2025). A Preprocessing Pipeline for Pupillometry Signal from
#'   Multimodal iMotion Data. *Sensors*, 25(15), 4737.
#'   \doi{10.3390/s25154737}
#'
#' @seealso [validate_gazepoint_binocular_reconstruction()],
#'   [reconstruct_gazepoint_binocular_pupil()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 12)
#' fit_gazepoint_binocular_calibration(
#'   dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
#' )
#'
#' @export
fit_gazepoint_binocular_calibration <- function(
    data,
    left_col,
    right_col,
    group_cols = NULL,
    fallback_group_cols = NULL,
    valid_min = NULL,
    valid_max = NULL,
    min_pairs = 30L,
    min_unique = 5L,
    min_r2 = NULL,
    allow_negative_slope = FALSE,
    max_abs_slope = NULL) {
  .gp3_binoc_assert_data(data)
  group_cols <- unique(.gp3_binoc_null(group_cols, character(0)))
  .gp3_binoc_assert_cols(data, c(left_col, right_col, group_cols))
  .gp3_binoc_assert_numeric_col(data, left_col)
  .gp3_binoc_assert_numeric_col(data, right_col)
  .gp3_binoc_validate_bounds(valid_min, valid_max)
  .gp3_binoc_assert_scalar_number(min_pairs, "min_pairs", lower = 2)
  .gp3_binoc_assert_scalar_number(min_unique, "min_unique", lower = 2)
  if (!is.null(min_r2)) .gp3_binoc_assert_scalar_number(min_r2, "min_r2", lower = 0, upper = 1)
  if (!is.null(max_abs_slope)) .gp3_binoc_assert_scalar_number(max_abs_slope, "max_abs_slope", lower = 0)
  if (!is.logical(allow_negative_slope) || length(allow_negative_slope) != 1L || is.na(allow_negative_slope)) {
    stop("`allow_negative_slope` must be TRUE or FALSE.", call. = FALSE)
  }

  specs <- .gp3_binoc_level_specs(group_cols, fallback_group_cols)
  for (spec in specs) .gp3_binoc_assert_cols(data, spec, "calibration grouping columns")
  left <- .gp3_binoc_observed(data[[left_col]], valid_min, valid_max)
  right <- .gp3_binoc_observed(data[[right_col]], valid_min, valid_max)

  levels <- vector("list", length(specs))
  model_counter <- 0L
  for (level_i in seq_along(specs)) {
    spec <- specs[[level_i]]
    split_idx <- .gp3_binoc_split_indices(data, spec)
    level_rows <- list()
    for (key in names(split_idx)) {
      idx <- split_idx[[key]]
      directions <- list(
        left_from_right = list(x = right[idx], y = left[idx]),
        right_from_left = list(x = left[idx], y = right[idx])
      )
      for (direction in names(directions)) {
        model_counter <- model_counter + 1L
        fit <- .gp3_binoc_fit_one(
          directions[[direction]]$x,
          directions[[direction]]$y,
          min_pairs = as.integer(min_pairs),
          min_unique = as.integer(min_unique),
          min_r2 = min_r2,
          allow_negative_slope = allow_negative_slope,
          max_abs_slope = max_abs_slope
        )
        row <- as.data.frame(fit, stringsAsFactors = FALSE)
        row$model_id <- sprintf("binoc_%03d_%s", model_counter, direction)
        row$direction <- direction
        row$calibration_level <- .gp3_binoc_group_label(spec)
        row$group_key <- key
        if (length(spec)) {
          for (col in spec) row[[col]] <- data[[col]][idx[[1L]]]
        }
        front <- c("model_id", "direction", "calibration_level", "group_key", spec)
        row <- row[c(front, setdiff(names(row), front))]
        level_rows[[length(level_rows) + 1L]] <- row
      }
    }
    levels[[level_i]] <- list(
      group_cols = spec,
      calibration_level = .gp3_binoc_group_label(spec),
      models = .gp3_binoc_rbind_fill(level_rows)
    )
  }
  models <- .gp3_binoc_flatten_models(levels)

  structure(
    list(
      models = tibble::as_tibble(models),
      levels = levels,
      settings = list(
        left_col = left_col,
        right_col = right_col,
        group_cols = group_cols,
        fallback_group_cols = specs[-1L],
        valid_min = valid_min,
        valid_max = valid_max,
        min_pairs = as.integer(min_pairs),
        min_unique = as.integer(min_unique),
        min_r2 = min_r2,
        allow_negative_slope = allow_negative_slope,
        max_abs_slope = max_abs_slope
      )
    ),
    class = "gp3_binocular_calibration"
  )
}

#' Reconstruct temporarily unavailable binocular pupil channels
#'
#' Applies a declared reconstruction policy without overwriting the original eye
#' channels. Linear-regression reconstruction uses separately fitted
#' left-from-right and right-from-left models, explicit eligibility gates,
#' gap-duration restrictions, extrapolation controls, physiological bounds, and
#' row-level provenance.
#'
#' @param data A data frame containing pupil channels.
#' @param left_col,right_col Numeric pupil columns.
#' @param time_col Optional numeric time column. Required when `max_gap_ms` is
#'   finite.
#' @param group_cols Primary calibration grouping columns.
#' @param gap_group_cols Optional grouping columns used only to define temporal
#'   missing-eye runs. When `NULL`, `group_cols` are used. This permits, for
#'   example, participant-level calibration with participant-by-trial gap gates.
#' @param method Reconstruction policy: `"linear_regression"`,
#'   `"available_eye"`, or `"none"`. The latter two do not synthesize a missing
#'   eye; they retain explicit monocular provenance for downstream construction.
#' @param calibration Optional result from [fit_gazepoint_binocular_calibration()].
#'   When omitted and `method = "linear_regression"`, calibration is fitted from
#'   `data`.
#' @param fallback_group_cols,min_pairs,min_unique,min_r2 Calibration settings
#'   used only when `calibration` is not supplied.
#' @param time_unit Unit for `time_col`.
#' @param max_gap_ms Maximum contiguous missing-eye run eligible for model-based
#'   reconstruction. `Inf` disables the duration gate. No study-specific cutoff
#'   is imposed by default.
#' @param allow_edge_gaps Whether missing runs touching a group boundary may be
#'   reconstructed from the simultaneously observed contralateral eye.
#' @param allow_extrapolation Whether predictions outside the calibration
#'   predictor range are allowed.
#' @param valid_min,valid_max Optional bounds applied both to observed values used
#'   by this workflow and to predictions.
#' @param exclude_flag_cols Optional logical/numeric flag columns. Rows flagged in
#'   any supplied column are not reconstructed.
#' @param prefix Prefix for added provenance and reconstructed-channel columns.
#' @param overwrite Whether existing output columns with this prefix may be
#'   replaced.
#'
#' @return The input data with additional observed, final-channel, reconstruction,
#'   model, gap, and status columns. Original pupil columns are untouched. A
#'   `gp3_binocular_reconstruction` metadata attribute records the declared policy
#'   and calibration object.
#'
#' @details Reconstructed values are predictions from the contralateral eye; they
#'   are never labelled as measurements. Temporal interpolation and cross-eye
#'   reconstruction solve different missing-data problems and are deliberately
#'   kept separate.
#'
#' @references Ong J, He W, Maglanque P, Jiang X, Gillman LM, Vergis A,
#'   Hardy K (2025). A Preprocessing Pipeline for Pupillometry Signal from
#'   Multimodal iMotion Data. *Sensors*, 25(15), 4737.
#'   \doi{10.3390/s25154737}
#'
#' @seealso [construct_gazepoint_combined_pupil()],
#'   [validate_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 13)
#' dat$pupil_left[40:43] <- NA_real_
#' rec <- reconstruct_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
#'   group_cols = "subject", min_pairs = 20
#' )
#' table(rec$gp3_binocular_status)
#'
#' @export
reconstruct_gazepoint_binocular_pupil <- function(
    data,
    left_col,
    right_col,
    time_col = NULL,
    group_cols = NULL,
    gap_group_cols = NULL,
    method = c("linear_regression", "available_eye", "none"),
    calibration = NULL,
    fallback_group_cols = NULL,
    min_pairs = 30L,
    min_unique = 5L,
    min_r2 = NULL,
    time_unit = c("auto", "milliseconds", "seconds"),
    max_gap_ms = Inf,
    allow_edge_gaps = TRUE,
    allow_extrapolation = FALSE,
    valid_min = NULL,
    valid_max = NULL,
    exclude_flag_cols = NULL,
    prefix = "gp3_binocular",
    overwrite = FALSE) {
  .gp3_binoc_assert_data(data)
  method <- match.arg(method)
  time_unit <- match.arg(time_unit)
  if (is.null(group_cols) && inherits(calibration, "gp3_binocular_calibration")) {
    group_cols <- calibration$settings$group_cols
  }
  group_cols <- unique(.gp3_binoc_null(group_cols, character(0)))
  gap_group_cols <- unique(.gp3_binoc_null(gap_group_cols, group_cols))
  exclude_flag_cols <- unique(.gp3_binoc_null(exclude_flag_cols, character(0)))
  .gp3_binoc_assert_cols(data, c(left_col, right_col, time_col, group_cols, gap_group_cols, exclude_flag_cols))
  .gp3_binoc_assert_numeric_col(data, left_col)
  .gp3_binoc_assert_numeric_col(data, right_col)
  if (!is.null(time_col)) .gp3_binoc_assert_numeric_col(data, time_col)
  .gp3_binoc_validate_bounds(valid_min, valid_max)
  if (!(is.numeric(max_gap_ms) && length(max_gap_ms) == 1L && !is.na(max_gap_ms) && max_gap_ms >= 0)) {
    stop("`max_gap_ms` must be one non-negative numeric value or Inf.", call. = FALSE)
  }
  if (is.finite(max_gap_ms) && is.null(time_col)) {
    stop("A numeric `time_col` is required when `max_gap_ms` is finite.", call. = FALSE)
  }
  for (arg in c("allow_edge_gaps", "allow_extrapolation", "overwrite")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", arg), call. = FALSE)
    }
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) || !nzchar(prefix)) {
    stop("`prefix` must be one non-empty character string.", call. = FALSE)
  }

  nms <- paste0(prefix, c(
    "_left_observed", "_right_observed", "_left_final", "_right_final",
    "_left_reconstructed", "_right_reconstructed", "_reconstructed",
    "_direction", "_model_id", "_calibration_level", "_r_squared",
    "_extrapolated", "_gap_ms", "_status"
  ))
  .gp3_binoc_check_output_names(data, nms, overwrite)

  left <- .gp3_binoc_observed(data[[left_col]], valid_min, valid_max)
  right <- .gp3_binoc_observed(data[[right_col]], valid_min, valid_max)
  left_ok <- is.finite(left)
  right_ok <- is.finite(right)
  excluded <- .gp3_binoc_excluded(data, exclude_flag_cols)

  left_gap <- .gp3_binoc_gap_vectors(data, !left_ok, gap_group_cols, time_col, time_unit)
  right_gap <- .gp3_binoc_gap_vectors(data, !right_ok, gap_group_cols, time_col, time_unit)

  out <- data
  left_final <- left
  right_final <- right
  left_rec <- rep(FALSE, nrow(data))
  right_rec <- rep(FALSE, nrow(data))
  direction <- rep(NA_character_, nrow(data))
  model_id <- rep(NA_character_, nrow(data))
  calibration_level <- rep(NA_character_, nrow(data))
  model_r2 <- rep(NA_real_, nrow(data))
  extrapolated <- rep(FALSE, nrow(data))
  used_gap <- rep(NA_real_, nrow(data))
  status <- rep("both_unavailable", nrow(data))
  status[left_ok & right_ok] <- "bilateral_observed"
  status[left_ok & !right_ok] <- "left_only_observed"
  status[!left_ok & right_ok] <- "right_only_observed"

  if (method == "linear_regression") {
    if (is.null(calibration)) {
      calibration <- fit_gazepoint_binocular_calibration(
        data = data,
        left_col = left_col,
        right_col = right_col,
        group_cols = group_cols,
        fallback_group_cols = fallback_group_cols,
        valid_min = valid_min,
        valid_max = valid_max,
        min_pairs = min_pairs,
        min_unique = min_unique,
        min_r2 = min_r2
      )
    }
    if (!inherits(calibration, "gp3_binocular_calibration")) {
      stop("`calibration` must be created by `fit_gazepoint_binocular_calibration()`.", call. = FALSE)
    }
    if (!identical(calibration$settings$left_col, left_col) ||
        !identical(calibration$settings$right_col, right_col)) {
      stop(
        "`calibration` was fitted to different left/right pupil columns.",
        call. = FALSE
      )
    }
    calibration_group_cols <- unique(unlist(
      lapply(calibration$levels, `[[`, "group_cols"),
      use.names = FALSE
    ))
    .gp3_binoc_assert_cols(data, calibration_group_cols, "calibration grouping columns")
    models <- calibration$models
    left_model_idx <- .gp3_binoc_assign_models(data, calibration, "left_from_right")
    right_model_idx <- .gp3_binoc_assign_models(data, calibration, "right_from_left")

    for (target in c("left", "right")) {
      if (target == "left") {
        candidate <- !left_ok & right_ok
        predictor <- right
        selected <- left_model_idx
        gap <- left_gap
      } else {
        candidate <- left_ok & !right_ok
        predictor <- left
        selected <- right_model_idx
        gap <- right_gap
      }
      idx <- which(candidate)
      if (!length(idx)) next

      for (i in idx) {
        used_gap[[i]] <- gap$gap_ms[[i]]
        direction[[i]] <- paste0(target, "_from_", if (target == "left") "right" else "left")
        if (excluded[[i]]) {
          status[[i]] <- "reconstruction_blocked_exclusion"
          next
        }
        if (is.finite(max_gap_ms) && (is.na(gap$gap_ms[[i]]) || gap$gap_ms[[i]] > max_gap_ms)) {
          status[[i]] <- "reconstruction_blocked_gap"
          next
        }
        if (!allow_edge_gaps && isTRUE(gap$edge_gap[[i]])) {
          status[[i]] <- "reconstruction_blocked_edge"
          next
        }
        mi <- selected[[i]]
        if (is.na(mi) || mi < 1L || mi > nrow(models)) {
          status[[i]] <- "reconstruction_ineligible"
          next
        }
        m <- models[mi, , drop = FALSE]
        x <- predictor[[i]]
        is_extra <- is.finite(x) && (x < m$predictor_min[[1L]] || x > m$predictor_max[[1L]])
        extrapolated[[i]] <- is_extra
        model_id[[i]] <- m$model_id[[1L]]
        calibration_level[[i]] <- m$calibration_level[[1L]]
        model_r2[[i]] <- m$r_squared[[1L]]
        if (is_extra && !allow_extrapolation) {
          status[[i]] <- "reconstruction_blocked_extrapolation"
          next
        }
        pred <- m$intercept[[1L]] + m$slope[[1L]] * x
        if (!is.finite(pred) || (!is.null(valid_min) && pred < valid_min) ||
            (!is.null(valid_max) && pred > valid_max)) {
          status[[i]] <- "reconstruction_blocked_bounds"
          next
        }
        if (target == "left") {
          left_final[[i]] <- pred
          left_rec[[i]] <- TRUE
          status[[i]] <- "left_reconstructed"
        } else {
          right_final[[i]] <- pred
          right_rec[[i]] <- TRUE
          status[[i]] <- "right_reconstructed"
        }
      }
    }
  } else {
    calibration <- NULL
  }

  out[[paste0(prefix, "_left_observed")]] <- left
  out[[paste0(prefix, "_right_observed")]] <- right
  out[[paste0(prefix, "_left_final")]] <- left_final
  out[[paste0(prefix, "_right_final")]] <- right_final
  out[[paste0(prefix, "_left_reconstructed")]] <- left_rec
  out[[paste0(prefix, "_right_reconstructed")]] <- right_rec
  out[[paste0(prefix, "_reconstructed")]] <- left_rec | right_rec
  out[[paste0(prefix, "_direction")]] <- direction
  out[[paste0(prefix, "_model_id")]] <- model_id
  out[[paste0(prefix, "_calibration_level")]] <- calibration_level
  out[[paste0(prefix, "_r_squared")]] <- model_r2
  out[[paste0(prefix, "_extrapolated")]] <- extrapolated
  out[[paste0(prefix, "_gap_ms")]] <- used_gap
  out[[paste0(prefix, "_status")]] <- status

  attr(out, "gp3_binocular_reconstruction") <- list(
    method = method,
    left_col = left_col,
    right_col = right_col,
    time_col = time_col,
    group_cols = group_cols,
    gap_group_cols = gap_group_cols,
    time_unit = time_unit,
    max_gap_ms = max_gap_ms,
    allow_edge_gaps = allow_edge_gaps,
    allow_extrapolation = allow_extrapolation,
    valid_min = valid_min,
    valid_max = valid_max,
    exclude_flag_cols = exclude_flag_cols,
    prefix = prefix,
    calibration = calibration,
    package_version = tryCatch(as.character(utils::packageVersion("gp3tools")), error = function(e) NA_character_)
  )
  out
}

#' Construct an explicitly governed combined pupil signal
#'
#' Builds a combined pupil series under a declared policy while retaining a
#' per-row source label. This function replaces opaque `rowMeans(..., na.rm =
#' TRUE)` decisions with explicit provenance.
#'
#' @param data Raw data or output from [reconstruct_gazepoint_binocular_pupil()].
#' @param left_col,right_col Original pupil column names.
#' @param policy One of `"complete_case"`, `"available_eye"`,
#'   `"reconstructed_mean"`, `"left_only"`, or `"right_only"`.
#' @param prefix Reconstruction prefix used by `reconstructed_mean`.
#' @param output_col Name of the constructed pupil column.
#' @param status_col Name of the provenance column.
#' @param valid_min,valid_max Optional bounds when working directly from raw
#'   channels.
#' @param overwrite Whether existing output columns may be replaced.
#'
#' @return `data` with `output_col` and `status_col` appended.
#'
#' @seealso [reconstruct_gazepoint_binocular_pupil()],
#'   [analyse_gazepoint_binocular_sensitivity()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 3, n_trials = 2, seed = 14)
#' dat$pupil_right[30:33] <- NA_real_
#' construct_gazepoint_combined_pupil(
#'   dat, "pupil_left", "pupil_right", policy = "available_eye"
#' )
#'
#' @export
construct_gazepoint_combined_pupil <- function(
    data,
    left_col,
    right_col,
    policy = c("complete_case", "available_eye", "reconstructed_mean", "left_only", "right_only"),
    prefix = "gp3_binocular",
    output_col = "pupil_binocular",
    status_col = "pupil_binocular_status",
    valid_min = NULL,
    valid_max = NULL,
    overwrite = FALSE) {
  .gp3_binoc_assert_data(data)
  policy <- match.arg(policy)
  .gp3_binoc_assert_cols(data, c(left_col, right_col))
  .gp3_binoc_assert_numeric_col(data, left_col)
  .gp3_binoc_assert_numeric_col(data, right_col)
  .gp3_binoc_validate_bounds(valid_min, valid_max)
  .gp3_binoc_check_output_names(data, c(output_col, status_col), overwrite)

  if (policy == "reconstructed_mean") {
    lf <- paste0(prefix, "_left_final")
    rf <- paste0(prefix, "_right_final")
    lr <- paste0(prefix, "_left_reconstructed")
    rr <- paste0(prefix, "_right_reconstructed")
    .gp3_binoc_assert_cols(data, c(lf, rf, lr, rr), "reconstruction columns")
    left <- data[[lf]]
    right <- data[[rf]]
    left_rec <- as.logical(data[[lr]])
    right_rec <- as.logical(data[[rr]])
    left_rec[is.na(left_rec)] <- FALSE
    right_rec[is.na(right_rec)] <- FALSE
  } else {
    left <- .gp3_binoc_observed(data[[left_col]], valid_min, valid_max)
    right <- .gp3_binoc_observed(data[[right_col]], valid_min, valid_max)
    left_rec <- right_rec <- rep(FALSE, nrow(data))
  }

  left_ok <- is.finite(left)
  right_ok <- is.finite(right)
  value <- rep(NA_real_, nrow(data))
  source <- rep("unavailable", nrow(data))

  if (policy == "complete_case") {
    both <- left_ok & right_ok
    value[both] <- (left[both] + right[both]) / 2
    source[both] <- "bilateral_observed"
  } else if (policy == "available_eye" || policy == "reconstructed_mean") {
    both <- left_ok & right_ok
    only_left <- left_ok & !right_ok
    only_right <- !left_ok & right_ok
    value[both] <- (left[both] + right[both]) / 2
    value[only_left] <- left[only_left]
    value[only_right] <- right[only_right]
    source[both] <- "bilateral_observed"
    source[only_left] <- "left_only_observed"
    source[only_right] <- "right_only_observed"
    if (policy == "reconstructed_mean") {
      source[both & (left_rec | right_rec)] <- "bilateral_with_reconstruction"
      source[only_left & left_rec] <- "left_reconstructed_only"
      source[only_right & right_rec] <- "right_reconstructed_only"
    }
  } else if (policy == "left_only") {
    value[left_ok] <- left[left_ok]
    source[left_ok] <- "left_observed"
  } else if (policy == "right_only") {
    value[right_ok] <- right[right_ok]
    source[right_ok] <- "right_observed"
  }

  out <- data
  out[[output_col]] <- value
  out[[status_col]] <- source
  attr(out, "gp3_binocular_combination") <- list(
    policy = policy,
    left_col = left_col,
    right_col = right_col,
    prefix = prefix,
    output_col = output_col,
    status_col = status_col
  )
  out
}
