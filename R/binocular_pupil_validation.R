# Validation, stress testing, auditing, and sensitivity --------------------

.gp3_binoc_restore_rng <- function(had_seed, old_seed) {
  if (had_seed) {
    assign(".Random.seed", old_seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

.gp3_binoc_mask_indices <- function(data, eligible, proportion, mode,
                                     block_size, group_cols, time_col) {
  groups <- .gp3_binoc_split_indices(data, group_cols)
  selected <- integer(0)
  for (idx in groups) {
    idx <- idx[eligible[idx]]
    if (!length(idx)) next
    if (!is.null(time_col)) {
      idx <- idx[order(data[[time_col]][idx], idx, na.last = TRUE)]
    }
    target <- max(1L, min(length(idx), as.integer(round(length(idx) * proportion))))
    if (mode == "random") {
      selected <- c(selected, sample(idx, target, replace = FALSE))
      next
    }

    chosen <- integer(0)
    attempts <- 0L
    max_attempts <- max(50L, 10L * target)
    while (length(chosen) < target && attempts < max_attempts) {
      attempts <- attempts + 1L
      remaining <- setdiff(seq_along(idx), match(chosen, idx, nomatch = 0L))
      remaining <- remaining[remaining > 0L]
      if (!length(remaining)) break
      start <- sample(remaining, 1L)
      pos <- seq.int(start, min(length(idx), start + block_size - 1L))
      add <- setdiff(idx[pos], chosen)
      chosen <- unique(c(chosen, add))
    }
    if (length(chosen) > target) chosen <- chosen[seq_len(target)]
    if (length(chosen) < target) {
      rest <- setdiff(idx, chosen)
      if (length(rest)) {
        chosen <- c(chosen, sample(rest, min(length(rest), target - length(chosen))))
      }
    }
    selected <- c(selected, chosen)
  }
  sort(unique(selected))
}

.gp3_binoc_metric_row <- function(observed, predicted, repeat_id, direction,
                                   requested) {
  ok <- is.finite(observed) & is.finite(predicted)
  obs <- observed[ok]
  pred <- predicted[ok]
  err <- pred - obs
  data.frame(
    repeat_id = repeat_id,
    direction = direction,
    n_requested = requested,
    n_predicted = length(obs),
    prediction_rate = if (requested > 0L) length(obs) / requested else NA_real_,
    rmse = if (length(obs)) sqrt(mean(err^2)) else NA_real_,
    mae = if (length(obs)) mean(abs(err)) else NA_real_,
    bias = if (length(obs)) mean(err) else NA_real_,
    median_error = if (length(obs)) stats::median(err) else NA_real_,
    error_mad = if (length(obs)) stats::mad(err, constant = 1) else NA_real_,
    correlation = if (length(obs) > 2L && stats::sd(obs) > 0 && stats::sd(pred) > 0) {
      stats::cor(obs, pred)
    } else NA_real_,
    stringsAsFactors = FALSE
  )
}

.gp3_binoc_validation_summary <- function(metrics, predictions) {
  if (!nrow(metrics)) return(tibble::tibble())
  dirs <- unique(metrics$direction)
  rows <- lapply(dirs, function(direction) {
    x <- metrics[metrics$direction == direction, , drop = FALSE]
    p <- predictions[predictions$direction == direction, , drop = FALSE]
    ok <- is.finite(p$observed) & is.finite(p$predicted)
    observed <- p$observed[ok]
    predicted <- p$predicted[ok]
    error <- predicted - observed
    requested <- sum(x$n_requested)
    n_predicted <- length(error)
    data.frame(
      direction = direction,
      repeats = nrow(x),
      total_requested = requested,
      total_predicted = n_predicted,
      prediction_rate = if (requested > 0L) n_predicted / requested else NA_real_,
      rmse = if (n_predicted) sqrt(mean(error^2)) else NA_real_,
      mae = if (n_predicted) mean(abs(error)) else NA_real_,
      bias = if (n_predicted) mean(error) else NA_real_,
      median_error = if (n_predicted) stats::median(error) else NA_real_,
      error_mad = if (n_predicted) stats::mad(error, constant = 1) else NA_real_,
      correlation = if (n_predicted > 2L && stats::sd(observed) > 0 &&
                        stats::sd(predicted) > 0) {
        stats::cor(observed, predicted)
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(.gp3_binoc_rbind_fill(rows))
}

#' Validate binocular reconstruction using artificial monocular loss
#'
#' Uses bilateral observations as known reference values, temporarily masks one
#' eye, refits cross-eye calibration without the masked target values, reconstructs
#' the hidden observations, and compares predictions with the held-out values.
#' This provides dataset-specific empirical reconstruction diagnostics rather than
#' assuming that a cross-eye regression is adequate.
#'
#' @param data A data frame containing binocular pupil measurements.
#' @param left_col,right_col Numeric pupil columns.
#' @param time_col Optional time column; required for meaningful contiguous-gap
#'   validation and finite `max_gap_ms`.
#' @param group_cols Primary calibration and masking groups.
#' @param gap_group_cols Optional groups used to define temporal missing-eye runs
#'   during reconstruction; defaults to `group_cols`.
#' @param fallback_group_cols Optional calibration fallback groups.
#' @param direction `"both"`, `"left_from_right"`, or `"right_from_left"`.
#' @param mask_prop Proportion of bilateral observations masked in each repeat.
#' @param mask_mode `"random"` masks individual observations; `"contiguous"`
#'   masks short ordered runs.
#' @param block_size Number of samples per attempted contiguous block.
#' @param repeats Number of repeated artificial-missingness evaluations.
#' @param seed Random seed. The caller's RNG state is restored on exit.
#' @param min_pairs,min_unique,min_r2 Calibration gates.
#' @param time_unit Time unit used by reconstruction.
#' @param max_gap_ms,allow_edge_gaps,allow_extrapolation Reconstruction gates.
#' @param valid_min,valid_max Optional bounds.
#'
#' @return A `gp3_binocular_validation` object containing repeat-level `metrics`,
#'   row-level `predictions`, aggregated `summary`, and settings.
#'
#' @details Artificial masking evaluates prediction error where the hidden target
#'   is actually known. It does not prove that naturally missing observations are
#'   missing at random or that their unobserved values would follow the same error
#'   distribution.
#'
#' @seealso [fit_gazepoint_binocular_calibration()],
#'   [stress_test_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 5, n_trials = 2, seed = 21)
#' val <- validate_gazepoint_binocular_reconstruction(
#'   dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
#'   group_cols = "subject", mask_prop = 0.1, repeats = 2,
#'   min_pairs = 20, seed = 9
#' )
#' val$summary
#'
#' @export
validate_gazepoint_binocular_reconstruction <- function(
    data,
    left_col,
    right_col,
    time_col = NULL,
    group_cols = NULL,
    gap_group_cols = NULL,
    fallback_group_cols = NULL,
    direction = c("both", "left_from_right", "right_from_left"),
    mask_prop = 0.20,
    mask_mode = c("random", "contiguous"),
    block_size = 6L,
    repeats = 5L,
    seed = 1L,
    min_pairs = 30L,
    min_unique = 5L,
    min_r2 = NULL,
    time_unit = c("auto", "milliseconds", "seconds"),
    max_gap_ms = Inf,
    allow_edge_gaps = TRUE,
    allow_extrapolation = FALSE,
    valid_min = NULL,
    valid_max = NULL) {
  .gp3_binoc_assert_data(data)
  direction <- match.arg(direction)
  mask_mode <- match.arg(mask_mode)
  time_unit <- match.arg(time_unit)
  group_cols <- unique(.gp3_binoc_null(group_cols, character(0)))
  gap_group_cols <- unique(.gp3_binoc_null(gap_group_cols, group_cols))
  .gp3_binoc_assert_cols(data, c(left_col, right_col, time_col, group_cols, gap_group_cols))
  .gp3_binoc_assert_numeric_col(data, left_col)
  .gp3_binoc_assert_numeric_col(data, right_col)
  if (!is.null(time_col)) .gp3_binoc_assert_numeric_col(data, time_col)
  .gp3_binoc_validate_bounds(valid_min, valid_max)
  .gp3_binoc_assert_scalar_number(mask_prop, "mask_prop", lower = 0, upper = 1, inclusive = FALSE)
  .gp3_binoc_assert_scalar_number(block_size, "block_size", lower = 1)
  .gp3_binoc_assert_scalar_number(repeats, "repeats", lower = 1)
  .gp3_binoc_assert_scalar_number(seed, "seed")
  if (mask_mode == "contiguous" && is.null(time_col)) {
    warning(
      "`mask_mode = \"contiguous\"` without `time_col` uses within-group row order.",
      call. = FALSE
    )
  }

  left_ref <- .gp3_binoc_observed(data[[left_col]], valid_min, valid_max)
  right_ref <- .gp3_binoc_observed(data[[right_col]], valid_min, valid_max)
  bilateral <- is.finite(left_ref) & is.finite(right_ref)
  if (sum(bilateral) < 2L) {
    stop("At least two bilateral observations are required for artificial-missingness validation.", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  on.exit(.gp3_binoc_restore_rng(had_seed, old_seed), add = TRUE)
  set.seed(as.integer(seed))

  metric_parts <- list()
  pred_parts <- list()

  for (rep_id in seq_len(as.integer(repeats))) {
    mask_idx <- .gp3_binoc_mask_indices(
      data, bilateral, mask_prop, mask_mode, as.integer(block_size),
      gap_group_cols, time_col
    )
    if (!length(mask_idx)) next

    if (direction == "both") {
      if (mask_mode == "contiguous") {
        target_eye <- rep(NA_character_, length(mask_idx))
        mask_data <- data[mask_idx, , drop = FALSE]
        mask_keys <- .gp3_binoc_make_group_key(mask_data, gap_group_cols)
        for (key in unique(mask_keys)) {
          pos <- which(mask_keys == key)
          if (!is.null(time_col)) {
            pos <- pos[order(mask_data[[time_col]][pos], pos, na.last = TRUE)]
          }
          block_id <- ceiling(seq_along(pos) / as.integer(block_size))
          block_eyes <- sample(c("left", "right"), max(block_id), replace = TRUE)
          target_eye[pos] <- block_eyes[block_id]
        }
      } else {
        target_eye <- sample(c("left", "right"), length(mask_idx), replace = TRUE)
      }
      if (length(mask_idx) >= 2L && length(unique(target_eye)) == 1L) {
        target_eye[[1L]] <- if (target_eye[[1L]] == "left") "right" else "left"
      }
    } else {
      target_eye <- rep(if (direction == "left_from_right") "left" else "right", length(mask_idx))
    }

    masked <- data
    left_mask <- mask_idx[target_eye == "left"]
    right_mask <- mask_idx[target_eye == "right"]
    if (length(left_mask)) masked[[left_col]][left_mask] <- NA_real_
    if (length(right_mask)) masked[[right_col]][right_mask] <- NA_real_

    calibration <- fit_gazepoint_binocular_calibration(
      masked,
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
    rec <- reconstruct_gazepoint_binocular_pupil(
      masked,
      left_col = left_col,
      right_col = right_col,
      time_col = time_col,
      group_cols = group_cols,
      gap_group_cols = gap_group_cols,
      method = "linear_regression",
      calibration = calibration,
      time_unit = time_unit,
      max_gap_ms = max_gap_ms,
      allow_edge_gaps = allow_edge_gaps,
      allow_extrapolation = allow_extrapolation,
      valid_min = valid_min,
      valid_max = valid_max
    )

    for (eye in c("left", "right")) {
      idx <- mask_idx[target_eye == eye]
      if (!length(idx)) next
      dir_name <- if (eye == "left") "left_from_right" else "right_from_left"
      observed <- if (eye == "left") left_ref[idx] else right_ref[idx]
      final_col <- paste0("gp3_binocular_", eye, "_final")
      predicted <- rec[[final_col]][idx]
      metric_parts[[length(metric_parts) + 1L]] <- .gp3_binoc_metric_row(
        observed, predicted, rep_id, dir_name, length(idx)
      )
      p <- data.frame(
        repeat_id = rep_id,
        row_id = idx,
        direction = dir_name,
        observed = observed,
        predicted = predicted,
        error = predicted - observed,
        status = rec$gp3_binocular_status[idx],
        model_id = rec$gp3_binocular_model_id[idx],
        calibration_level = rec$gp3_binocular_calibration_level[idx],
        r_squared = rec$gp3_binocular_r_squared[idx],
        extrapolated = rec$gp3_binocular_extrapolated[idx],
        gap_ms = rec$gp3_binocular_gap_ms[idx],
        stringsAsFactors = FALSE
      )
      if (length(group_cols)) {
        for (col in group_cols) p[[col]] <- data[[col]][idx]
      }
      if (!is.null(time_col)) p[[time_col]] <- data[[time_col]][idx]
      pred_parts[[length(pred_parts) + 1L]] <- p
    }
  }

  metrics <- tibble::as_tibble(.gp3_binoc_rbind_fill(metric_parts))
  predictions <- tibble::as_tibble(.gp3_binoc_rbind_fill(pred_parts))
  summary <- .gp3_binoc_validation_summary(metrics, predictions)

  structure(
    list(
      summary = summary,
      metrics = metrics,
      predictions = predictions,
      settings = list(
        left_col = left_col,
        right_col = right_col,
        time_col = time_col,
        group_cols = group_cols,
        gap_group_cols = gap_group_cols,
        direction = direction,
        mask_prop = mask_prop,
        mask_mode = mask_mode,
        block_size = as.integer(block_size),
        repeats = as.integer(repeats),
        seed = as.integer(seed),
        min_pairs = as.integer(min_pairs),
        min_unique = as.integer(min_unique),
        min_r2 = min_r2,
        max_gap_ms = max_gap_ms,
        allow_edge_gaps = allow_edge_gaps,
        allow_extrapolation = allow_extrapolation,
        valid_min = valid_min,
        valid_max = valid_max
      )
    ),
    class = "gp3_binocular_validation"
  )
}

#' Stress-test binocular reconstruction across missingness levels
#'
#' Repeats artificial monocular-loss validation across declared missingness levels
#' and random/contiguous masking modes to show where cross-eye reconstruction
#' begins to degrade.
#'
#' @param data,left_col,right_col,time_col,group_cols,gap_group_cols,fallback_group_cols See
#'   [validate_gazepoint_binocular_reconstruction()].
#' @param missingness Numeric proportions strictly between 0 and 1.
#' @param mask_modes One or both of `"random"` and `"contiguous"`.
#' @param block_size Contiguous mask size in samples.
#' @param repeats Repeats per stress-test cell.
#' @param seed Base seed; deterministic offsets are used across cells.
#' @param min_pairs,min_unique,min_r2,max_gap_ms,valid_min,valid_max Passed to
#'   validation.
#'
#' @return A `gp3_binocular_stress_test` object containing cell-level `results`
#'   and individual validation objects.
#'
#' @seealso [validate_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 22)
#' stress_test_gazepoint_binocular_reconstruction(
#'   dat, "pupil_left", "pupil_right", time_col = "timestamp_ms",
#'   group_cols = "subject", missingness = c(0.05, 0.10),
#'   mask_modes = "random", repeats = 1, min_pairs = 20
#' )
#'
#' @export
stress_test_gazepoint_binocular_reconstruction <- function(
    data,
    left_col,
    right_col,
    time_col = NULL,
    group_cols = NULL,
    gap_group_cols = NULL,
    fallback_group_cols = NULL,
    missingness = c(0.05, 0.10, 0.20, 0.30),
    mask_modes = c("random", "contiguous"),
    block_size = 6L,
    repeats = 3L,
    seed = 1L,
    min_pairs = 30L,
    min_unique = 5L,
    min_r2 = NULL,
    max_gap_ms = Inf,
    valid_min = NULL,
    valid_max = NULL) {
  if (!is.numeric(missingness) || !length(missingness) ||
      any(!is.finite(missingness)) || any(missingness <= 0 | missingness >= 1)) {
    stop("`missingness` must contain proportions strictly between 0 and 1.", call. = FALSE)
  }
  allowed_modes <- c("random", "contiguous")
  if (!is.character(mask_modes) || !length(mask_modes) || any(!mask_modes %in% allowed_modes)) {
    stop("`mask_modes` must contain `random` and/or `contiguous`.", call. = FALSE)
  }

  validations <- list()
  rows <- list()
  cell <- 0L
  for (mode in unique(mask_modes)) {
    for (prop in unique(missingness)) {
      cell <- cell + 1L
      val <- validate_gazepoint_binocular_reconstruction(
        data = data,
        left_col = left_col,
        right_col = right_col,
        time_col = time_col,
        group_cols = group_cols,
        gap_group_cols = gap_group_cols,
        fallback_group_cols = fallback_group_cols,
        direction = "both",
        mask_prop = prop,
        mask_mode = mode,
        block_size = block_size,
        repeats = repeats,
        seed = as.integer(seed) + cell - 1L,
        min_pairs = min_pairs,
        min_unique = min_unique,
        min_r2 = min_r2,
        max_gap_ms = max_gap_ms,
        valid_min = valid_min,
        valid_max = valid_max
      )
      key <- paste(mode, format(prop, trim = TRUE), sep = "_")
      validations[[key]] <- val
      s <- as.data.frame(val$summary)
      if (nrow(s)) {
        s$mask_mode <- mode
        s$missingness <- prop
        rows[[length(rows) + 1L]] <- s
      }
    }
  }
  results <- tibble::as_tibble(.gp3_binoc_rbind_fill(rows))
  if (nrow(results)) {
    results <- results[c("mask_mode", "missingness", setdiff(names(results), c("mask_mode", "missingness")))]
  }
  structure(
    list(
      results = results,
      validations = validations,
      settings = list(
        missingness = missingness,
        mask_modes = unique(mask_modes),
        block_size = as.integer(block_size),
        repeats = as.integer(repeats),
        seed = as.integer(seed)
      )
    ),
    class = "gp3_binocular_stress_test"
  )
}

.gp3_binoc_burden_table <- function(data, prefix, by) {
  status_col <- paste0(prefix, "_status")
  rec_col <- paste0(prefix, "_reconstructed")
  .gp3_binoc_assert_cols(data, c(status_col, rec_col, by), "reconstruction audit columns")
  groups <- .gp3_binoc_split_indices(data, by)
  rows <- lapply(names(groups), function(key) {
    idx <- groups[[key]]
    st <- as.character(data[[status_col]][idx])
    rec <- as.logical(data[[rec_col]][idx])
    rec[is.na(rec)] <- FALSE
    bilateral <- st == "bilateral_observed"
    mono_obs <- st %in% c("left_only_observed", "right_only_observed")
    blocked <- grepl("^reconstruction_blocked|^reconstruction_ineligible", st)
    out <- data.frame(
      group_key = key,
      n = length(idx),
      n_bilateral_observed = sum(bilateral, na.rm = TRUE),
      n_reconstructed = sum(rec),
      n_monocular_unreconstructed = sum(mono_obs & !rec, na.rm = TRUE),
      n_unavailable = sum(st == "both_unavailable", na.rm = TRUE),
      n_blocked = sum(blocked, na.rm = TRUE),
      bilateral_observed_fraction = mean(bilateral, na.rm = TRUE),
      reconstruction_fraction = mean(rec),
      monocular_unreconstructed_fraction = mean(mono_obs & !rec, na.rm = TRUE),
      unavailable_fraction = mean(st == "both_unavailable", na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    if (length(by)) {
      for (col in by) out[[col]] <- data[[col]][idx[[1L]]]
      out <- out[c(by, setdiff(names(out), by))]
    }
    out
  })
  tibble::as_tibble(.gp3_binoc_rbind_fill(rows))
}

#' Audit binocular reconstruction burden and imbalance
#'
#' Summarises how much of the retained pupil signal depends on model-based
#' reconstruction, where reconstruction was blocked, whether reconstruction is
#' uneven across declared groups, and how much reconstruction shifts a simple
#' available-eye combined signal.
#'
#' @param data Output from [reconstruct_gazepoint_binocular_pupil()].
#' @param by Optional condition, participant, stimulus, trial, or other columns
#'   used to assess reconstruction-rate imbalance.
#' @param prefix Reconstruction prefix.
#' @param max_reconstruction_prop Optional descriptive threshold above which
#'   overall reconstruction burden is flagged for review. `NULL` reports burden
#'   without imposing a universal cutoff.
#' @param max_group_rate_difference Optional descriptive threshold for the
#'   maximum minus minimum reconstruction fraction across `by` groups. `NULL`
#'   reports imbalance without imposing a universal cutoff.
#'
#' @return A `gp3_binocular_audit` object with overall burden, grouped burden,
#'   status counts, model diagnostics, and audit status.
#'
#' @details Thresholds are governance flags, not statistical tests and not
#'   universal validity cutoffs.
#'
#' @seealso [analyse_gazepoint_binocular_sensitivity()],
#'   [summarise_gazepoint_binocular_reporting()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 23)
#' dat$pupil_left[35:38] <- NA_real_
#' rec <- reconstruct_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
#' )
#' audit_gazepoint_binocular_reconstruction(rec, by = "condition")
#'
#' @export
audit_gazepoint_binocular_reconstruction <- function(
    data,
    by = NULL,
    prefix = "gp3_binocular",
    max_reconstruction_prop = NULL,
    max_group_rate_difference = NULL) {
  .gp3_binoc_assert_data(data)
  by <- unique(.gp3_binoc_null(by, character(0)))
  .gp3_binoc_assert_cols(data, by)
  if (!is.null(max_reconstruction_prop)) {
    .gp3_binoc_assert_scalar_number(max_reconstruction_prop, "max_reconstruction_prop", lower = 0, upper = 1)
  }
  if (!is.null(max_group_rate_difference)) {
    .gp3_binoc_assert_scalar_number(max_group_rate_difference, "max_group_rate_difference", lower = 0, upper = 1)
  }

  metadata <- attr(data, "gp3_binocular_reconstruction", exact = TRUE)
  if (is.null(metadata)) {
    stop("`data` does not contain gp3tools binocular reconstruction metadata.", call. = FALSE)
  }
  status_col <- paste0(prefix, "_status")
  rec_col <- paste0(prefix, "_reconstructed")
  left_obs <- paste0(prefix, "_left_observed")
  right_obs <- paste0(prefix, "_right_observed")
  left_final <- paste0(prefix, "_left_final")
  right_final <- paste0(prefix, "_right_final")
  .gp3_binoc_assert_cols(data, c(status_col, rec_col, left_obs, right_obs, left_final, right_final))

  overall <- .gp3_binoc_burden_table(data, prefix, character(0))
  grouped <- if (length(by)) .gp3_binoc_burden_table(data, prefix, by) else tibble::tibble()
  status_tab <- as.data.frame(table(data[[status_col]], useNA = "ifany"), stringsAsFactors = FALSE)
  names(status_tab) <- c("status", "n")
  status_tab$proportion <- status_tab$n / nrow(data)

  observed_available <- rowMeans(cbind(data[[left_obs]], data[[right_obs]]), na.rm = TRUE)
  observed_available[!is.finite(observed_available)] <- NA_real_
  reconstructed_combined <- rowMeans(cbind(data[[left_final]], data[[right_final]]), na.rm = TRUE)
  reconstructed_combined[!is.finite(reconstructed_combined)] <- NA_real_
  rec_flag <- as.logical(data[[rec_col]])
  rec_flag[is.na(rec_flag)] <- FALSE
  shift <- reconstructed_combined[rec_flag] - observed_available[rec_flag]
  shift <- shift[is.finite(shift)]
  shift_summary <- tibble::tibble(
    n_reconstructed_rows_with_shift = length(shift),
    mean_reconstruction_shift = if (length(shift)) mean(shift) else NA_real_,
    median_reconstruction_shift = if (length(shift)) stats::median(shift) else NA_real_,
    mean_absolute_reconstruction_shift = if (length(shift)) mean(abs(shift)) else NA_real_,
    max_absolute_reconstruction_shift = if (length(shift)) max(abs(shift)) else NA_real_
  )

  overall_rate <- overall$reconstruction_fraction[[1L]]
  group_difference <- if (nrow(grouped) > 1L) {
    max(grouped$reconstruction_fraction, na.rm = TRUE) -
      min(grouped$reconstruction_fraction, na.rm = TRUE)
  } else NA_real_
  burden_flag <- !is.null(max_reconstruction_prop) &&
    is.finite(overall_rate) && overall_rate > max_reconstruction_prop
  imbalance_flag <- !is.null(max_group_rate_difference) &&
    is.finite(group_difference) && group_difference > max_group_rate_difference
  thresholds_declared <- !is.null(max_reconstruction_prop) || !is.null(max_group_rate_difference)
  audit_status <- if (!thresholds_declared) {
    "descriptive"
  } else if (burden_flag || imbalance_flag) {
    "review"
  } else {
    "ok"
  }

  calibration <- metadata$calibration
  models <- if (!is.null(calibration)) calibration$models else tibble::tibble()

  structure(
    list(
      overall = overall,
      by_group = grouped,
      status_counts = tibble::as_tibble(status_tab),
      reconstruction_shift = shift_summary,
      models = models,
      imbalance = tibble::tibble(
        max_group_rate_difference = group_difference,
        threshold = if (is.null(max_group_rate_difference)) NA_real_ else max_group_rate_difference,
        flagged = imbalance_flag
      ),
      audit = tibble::tibble(
        status = audit_status,
        reconstruction_fraction = overall_rate,
        reconstruction_threshold = if (is.null(max_reconstruction_prop)) NA_real_ else max_reconstruction_prop,
        burden_flag = burden_flag,
        imbalance_flag = imbalance_flag
      ),
      settings = list(by = by, prefix = prefix)
    ),
    class = "gp3_binocular_audit"
  )
}

.gp3_binoc_policy_values <- function(data, left_col, right_col, prefix, policy,
                                      valid_min, valid_max) {
  tmp_name <- ".gp3_binoc_value"
  tmp_status <- ".gp3_binoc_source"
  out <- construct_gazepoint_combined_pupil(
    data,
    left_col = left_col,
    right_col = right_col,
    policy = policy,
    prefix = prefix,
    output_col = tmp_name,
    status_col = tmp_status,
    valid_min = valid_min,
    valid_max = valid_max,
    overwrite = TRUE
  )
  list(value = out[[tmp_name]], source = out[[tmp_status]])
}

.gp3_binoc_summary_values <- function(data, value, policy, group_cols) {
  tmp <- data
  tmp$.gp3_value <- value
  split_idx <- .gp3_binoc_split_indices(tmp, group_cols)
  rows <- lapply(names(split_idx), function(key) {
    idx <- split_idx[[key]]
    x <- value[idx]
    good <- is.finite(x)
    out <- data.frame(
      policy = policy,
      group_key = key,
      n_total = length(idx),
      n_usable = sum(good),
      missing_fraction = mean(!good),
      mean = if (any(good)) mean(x[good]) else NA_real_,
      sd = if (sum(good) > 1L) stats::sd(x[good]) else NA_real_,
      median = if (any(good)) stats::median(x[good]) else NA_real_,
      mad = if (any(good)) stats::mad(x[good], constant = 1) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (length(group_cols)) {
      for (col in group_cols) out[[col]] <- data[[col]][idx[[1L]]]
      out <- out[c("policy", group_cols, setdiff(names(out), c("policy", group_cols)))]
    }
    out
  })
  .gp3_binoc_rbind_fill(rows)
}

#' Compare pupil-construction policies as a sensitivity analysis
#'
#' Constructs the same pupil stream under multiple declared policies and compares
#' data retention, descriptive distribution summaries, condition means/contrasts,
#' and pairwise series correlations. It deliberately avoids converting policy
#' disagreement into an automatic inferential verdict.
#'
#' @param data Raw data or reconstruction output. `reconstructed_mean` requires
#'   reconstruction columns.
#' @param left_col,right_col Original pupil channels.
#' @param policies Any of `"complete_case"`, `"available_eye"`,
#'   `"reconstructed_mean"`, `"left_only"`, and `"right_only"`.
#' @param prefix Reconstruction prefix.
#' @param group_cols Optional grouping columns for descriptive summaries.
#' @param condition_col Optional condition column. When supplied, simple
#'   descriptive mean contrasts relative to the first observed condition are
#'   returned; no p-values are calculated.
#' @param valid_min,valid_max Optional raw-channel bounds.
#'
#' @return A `gp3_binocular_sensitivity` object containing policy summaries,
#'   pairwise correlations, optional condition summaries/contrasts, and the
#'   constructed long series.
#'
#' @seealso [audit_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 24)
#' dat$pupil_right[20:24] <- NA_real_
#' rec <- reconstruct_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
#' )
#' analyse_gazepoint_binocular_sensitivity(
#'   rec, "pupil_left", "pupil_right", condition_col = "condition"
#' )
#'
#' @export
analyse_gazepoint_binocular_sensitivity <- function(
    data,
    left_col,
    right_col,
    policies = c("complete_case", "available_eye", "reconstructed_mean", "left_only", "right_only"),
    prefix = "gp3_binocular",
    group_cols = NULL,
    condition_col = NULL,
    valid_min = NULL,
    valid_max = NULL) {
  .gp3_binoc_assert_data(data)
  allowed <- c("complete_case", "available_eye", "reconstructed_mean", "left_only", "right_only")
  policies <- unique(policies)
  if (!is.character(policies) || !length(policies) || any(!policies %in% allowed)) {
    stop("`policies` contains an unsupported pupil-construction policy.", call. = FALSE)
  }
  group_cols <- unique(.gp3_binoc_null(group_cols, character(0)))
  .gp3_binoc_assert_cols(data, c(left_col, right_col, group_cols, condition_col))
  .gp3_binoc_validate_bounds(valid_min, valid_max)

  values <- list()
  sources <- list()
  summary_parts <- list()
  long_parts <- list()
  for (policy in policies) {
    p <- .gp3_binoc_policy_values(data, left_col, right_col, prefix, policy, valid_min, valid_max)
    values[[policy]] <- p$value
    sources[[policy]] <- p$source
    summary_parts[[length(summary_parts) + 1L]] <- .gp3_binoc_summary_values(
      data, p$value, policy, group_cols
    )
    long <- data.frame(
      row_id = seq_len(nrow(data)),
      policy = policy,
      pupil = p$value,
      source = p$source,
      stringsAsFactors = FALSE
    )
    if (length(group_cols)) for (col in group_cols) long[[col]] <- data[[col]]
    if (!is.null(condition_col)) long[[condition_col]] <- data[[condition_col]]
    long_parts[[length(long_parts) + 1L]] <- long
  }

  summary <- tibble::as_tibble(.gp3_binoc_rbind_fill(summary_parts))
  series <- tibble::as_tibble(.gp3_binoc_rbind_fill(long_parts))

  cor_rows <- list()
  if (length(policies) >= 2L) {
    pairs <- utils::combn(policies, 2L, simplify = FALSE)
    for (pair in pairs) {
      x <- values[[pair[[1L]]]]
      y <- values[[pair[[2L]]]]
      ok <- is.finite(x) & is.finite(y)
      cor_rows[[length(cor_rows) + 1L]] <- data.frame(
        policy_1 = pair[[1L]],
        policy_2 = pair[[2L]],
        n_complete = sum(ok),
        correlation = if (sum(ok) > 2L && stats::sd(x[ok]) > 0 && stats::sd(y[ok]) > 0) stats::cor(x[ok], y[ok]) else NA_real_,
        mean_difference = if (any(ok)) mean(x[ok] - y[ok]) else NA_real_,
        mean_absolute_difference = if (any(ok)) mean(abs(x[ok] - y[ok])) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  correlations <- tibble::as_tibble(.gp3_binoc_rbind_fill(cor_rows))

  condition_summary <- condition_contrasts <- tibble::tibble()
  if (!is.null(condition_col)) {
    condition_summary <- tibble::as_tibble(.gp3_binoc_rbind_fill(lapply(policies, function(policy) {
      .gp3_binoc_summary_values(data, values[[policy]], policy, condition_col)
    })))
    cond_values <- unique(as.character(data[[condition_col]]))
    cond_values <- cond_values[!is.na(cond_values)]
    if (length(cond_values) >= 2L) {
      ref <- cond_values[[1L]]
      contrast_rows <- list()
      for (policy in policies) {
        tab <- condition_summary[condition_summary$policy == policy, , drop = FALSE]
        cond_chr <- as.character(tab[[condition_col]])
        ref_mean <- tab$mean[match(ref, cond_chr)]
        for (cond in cond_values[-1L]) {
          current <- tab$mean[match(cond, cond_chr)]
          contrast_rows[[length(contrast_rows) + 1L]] <- data.frame(
            policy = policy,
            reference = ref,
            comparison = cond,
            mean_difference = current - ref_mean,
            stringsAsFactors = FALSE
          )
        }
      }
      condition_contrasts <- tibble::as_tibble(.gp3_binoc_rbind_fill(contrast_rows))
    }
  }

  structure(
    list(
      summary = summary,
      correlations = correlations,
      condition_summary = condition_summary,
      condition_contrasts = condition_contrasts,
      series = series,
      settings = list(
        policies = policies,
        prefix = prefix,
        group_cols = group_cols,
        condition_col = condition_col,
        valid_min = valid_min,
        valid_max = valid_max
      )
    ),
    class = "gp3_binocular_sensitivity"
  )
}

.gp3_binoc_fmt_pct <- function(x, digits = 1L) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("not available")
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

.gp3_binoc_fmt_num <- function(x, digits = 3L) {
  if (!length(x) || is.na(x) || !is.finite(x)) return("not available")
  formatC(x, format = "f", digits = digits)
}

#' Create manuscript-ready binocular reconstruction reporting
#'
#' Produces structured reporting fields plus conservative prose describing the
#' declared calibration, reconstruction burden, and optional pseudo-missing
#' validation. The text never describes predicted values as measured or as the
#' biological truth.
#'
#' @param data Output from [reconstruct_gazepoint_binocular_pupil()].
#' @param audit Optional result from [audit_gazepoint_binocular_reconstruction()].
#' @param validation Optional result from
#'   [validate_gazepoint_binocular_reconstruction()].
#' @param by Optional audit grouping used when `audit` is not supplied.
#' @param prefix Reconstruction prefix.
#'
#' @return A `gp3_binocular_reporting` object with `summary`, `models`,
#'   `validation`, `text`, and `limitations`.
#'
#' @seealso [audit_gazepoint_binocular_reconstruction()]
#'
#' @examples
#' dat <- simulate_gazepoint_pupil_data(n_subjects = 4, n_trials = 2, seed = 25)
#' dat$pupil_left[30:33] <- NA_real_
#' rec <- reconstruct_gazepoint_binocular_pupil(
#'   dat, "pupil_left", "pupil_right", group_cols = "subject", min_pairs = 20
#' )
#' summarise_gazepoint_binocular_reporting(rec)$text
#'
#' @export
summarise_gazepoint_binocular_reporting <- function(
    data,
    audit = NULL,
    validation = NULL,
    by = NULL,
    prefix = "gp3_binocular") {
  metadata <- attr(data, "gp3_binocular_reconstruction", exact = TRUE)
  if (is.null(metadata)) {
    stop("`data` must be output from `reconstruct_gazepoint_binocular_pupil()`.", call. = FALSE)
  }
  if (is.null(audit)) {
    audit <- audit_gazepoint_binocular_reconstruction(data, by = by, prefix = prefix)
  }
  if (!inherits(audit, "gp3_binocular_audit")) {
    stop("`audit` must be created by `audit_gazepoint_binocular_reconstruction()`.", call. = FALSE)
  }
  if (!is.null(validation) && !inherits(validation, "gp3_binocular_validation")) {
    stop("`validation` must be created by `validate_gazepoint_binocular_reconstruction()`.", call. = FALSE)
  }

  overall <- audit$overall[1, , drop = FALSE]
  n <- overall$n[[1L]]
  n_rec <- overall$n_reconstructed[[1L]]
  rec_fraction <- overall$reconstruction_fraction[[1L]]
  bilateral_fraction <- overall$bilateral_observed_fraction[[1L]]
  mono_fraction <- overall$monocular_unreconstructed_fraction[[1L]]
  method <- metadata$method
  calibration <- metadata$calibration
  models <- if (!is.null(calibration)) calibration$models else tibble::tibble()
  eligible_models <- if (nrow(models)) models[models$eligible %in% TRUE, , drop = FALSE] else models

  model_sentence <- if (nrow(eligible_models)) {
    paste0(
      "Eligible cross-eye calibration models used a median of ",
      as.integer(stats::median(eligible_models$n_pairs, na.rm = TRUE)),
      " paired observations; the median in-sample R-squared was ",
      .gp3_binoc_fmt_num(stats::median(eligible_models$r_squared, na.rm = TRUE)),
      "."
    )
  } else {
    "No eligible cross-eye calibration model was available under the declared gates."
  }

  validation_sentence <- NULL
  validation_table <- tibble::tibble()
  if (!is.null(validation)) {
    validation_table <- validation$summary
    if (nrow(validation_table)) {
      pred <- as.data.frame(validation$predictions)
      ok <- is.finite(pred$observed) & is.finite(pred$predicted)
      error <- pred$predicted[ok] - pred$observed[ok]
      rmse <- if (length(error)) sqrt(mean(error^2)) else NA_real_
      mae <- if (length(error)) mean(abs(error)) else NA_real_
      requested <- sum(validation_table$total_requested)
      pred_rate <- if (requested > 0L) sum(validation_table$total_predicted) / requested else NA_real_
      validation_sentence <- paste0(
        "Artificial monocular-loss validation reconstructed ",
        .gp3_binoc_fmt_pct(pred_rate),
        " of requested held-out values, with RMSE ", .gp3_binoc_fmt_num(rmse),
        " and MAE ", .gp3_binoc_fmt_num(mae), "."
      )
    }
  }

  text <- paste(
    paste0(
      "Binocular pupil handling used the declared `", method,
      "` policy. Of ", n, " rows, ", n_rec, " (", .gp3_binoc_fmt_pct(rec_fraction),
      ") contained model-based cross-eye reconstruction; ",
      .gp3_binoc_fmt_pct(bilateral_fraction),
      " were retained as directly observed bilateral samples and ",
      .gp3_binoc_fmt_pct(mono_fraction),
      " remained monocular without reconstruction."
    ),
    model_sentence,
    validation_sentence,
    paste0(
      "Reconstructed values were retained as predicted values with explicit row-level provenance; ",
      "they were not treated as independently measured pupil observations."
    )
  )

  summary <- tibble::tibble(
    n_rows = n,
    n_reconstructed = n_rec,
    reconstruction_fraction = rec_fraction,
    bilateral_observed_fraction = bilateral_fraction,
    monocular_unreconstructed_fraction = mono_fraction,
    audit_status = audit$audit$status[[1L]],
    method = method,
    max_gap_ms = metadata$max_gap_ms,
    allow_extrapolation = metadata$allow_extrapolation
  )

  limitations <- c(
    "Cross-eye prediction is a preprocessing reconstruction and does not recover an independently observed biological truth.",
    "Artificial masking quantifies prediction performance on observed bilateral samples but cannot establish the missingness mechanism of naturally unavailable samples.",
    "Calibration diagnostics and reconstruction burden should be reported alongside downstream sensitivity analyses.",
    "Temporal interpolation and cross-eye reconstruction should remain separately declared analytical decisions."
  )

  structure(
    list(
      summary = summary,
      models = models,
      validation = validation_table,
      text = text,
      limitations = limitations
    ),
    class = "gp3_binocular_reporting"
  )
}
