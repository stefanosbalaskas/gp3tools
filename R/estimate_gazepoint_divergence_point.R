#' Estimate a bootstrapped divergence point between two Gazepoint time courses
#'
#' Estimate the earliest time point at which two condition-level time courses
#' reliably diverge. The helper computes observed condition curves, bootstraps
#' the condition difference, identifies the first time point where the bootstrap
#' confidence interval excludes the null value for a requested number of
#' consecutive time points, and returns a bootstrap uncertainty interval for the
#' divergence onset.
#'
#' This helper complements cluster-permutation analysis. Cluster permutation asks
#' where a reliable time window exists; divergence-point analysis asks when the
#' condition difference first emerges.
#'
#' @param data A data frame containing time-course observations.
#' @param outcome_col Outcome column, for example pupil size, fixation
#'   probability, gaze proportion, or AOI time-course value.
#' @param time_col Time column.
#' @param condition_col Condition column. Exactly two conditions are compared
#'   unless `comparison` is supplied.
#' @param participant_col Optional participant column used for participant-level
#'   bootstrap resampling.
#' @param trial_col Optional trial column used for trial-level bootstrap
#'   resampling.
#' @param comparison Optional character vector of two condition values. The
#'   estimated difference is `comparison[2] - comparison[1]`.
#' @param bootstrap_unit Resampling unit. Options are `"participant"`,
#'   `"trial"`, and `"row"`.
#' @param summary_function Function used to summarise observations within
#'   condition-by-time cells. Options are `"mean"` and `"median"`.
#' @param n_boot Number of bootstrap resamples.
#' @param ci Confidence level for bootstrap intervals.
#' @param consecutive_points Number of consecutive time points required before
#'   declaring divergence.
#' @param null_value Null difference value. Default is `0`.
#' @param min_abs_difference Optional minimum absolute observed difference
#'   required at a time point.
#' @param direction Direction of divergence. `"two_sided"` checks whether the
#'   bootstrap interval excludes `null_value` in either direction. `"positive"`
#'   checks whether `comparison[2] > comparison[1]`. `"negative"` checks whether
#'   `comparison[2] < comparison[1]`.
#' @param seed Optional random seed for reproducible bootstrap resampling.
#' @param keep_bootstrap Logical. If `TRUE`, return bootstrap differences for
#'   each time point.
#' @param name Character label stored in the returned object.
#'
#' @return A list with class `gp3_divergence_point_analysis`.
#' @export
estimate_gazepoint_divergence_point <- function(
    data,
    outcome_col,
    time_col,
    condition_col,
    participant_col = NULL,
    trial_col = NULL,
    comparison = NULL,
    bootstrap_unit = c("participant", "trial", "row"),
    summary_function = c("mean", "median"),
    n_boot = 1000L,
    ci = 0.95,
    consecutive_points = 1L,
    null_value = 0,
    min_abs_difference = 0,
    direction = c("two_sided", "positive", "negative"),
    seed = NULL,
    keep_bootstrap = TRUE,
    name = "gazepoint_divergence_point"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  bootstrap_unit <- match.arg(bootstrap_unit)
  summary_function <- match.arg(summary_function)
  direction <- match.arg(direction)

  .gp3_divergence_check_col(outcome_col, names(data), "outcome_col")
  .gp3_divergence_check_col(time_col, names(data), "time_col")
  .gp3_divergence_check_col(condition_col, names(data), "condition_col")

  if (!is.null(participant_col)) {
    .gp3_divergence_check_col(participant_col, names(data), "participant_col")
  }

  if (!is.null(trial_col)) {
    .gp3_divergence_check_col(trial_col, names(data), "trial_col")
  }

  .gp3_divergence_check_positive_integer(n_boot, "n_boot")
  .gp3_divergence_check_ci(ci)
  .gp3_divergence_check_positive_integer(consecutive_points, "consecutive_points")
  .gp3_divergence_check_finite_number(null_value, "null_value")
  .gp3_divergence_check_nonnegative_number(min_abs_difference, "min_abs_difference")
  .gp3_divergence_check_logical(keep_bootstrap, "keep_bootstrap")
  .gp3_divergence_check_label(name, "name")

  if (!is.null(seed)) {
    .gp3_divergence_check_seed(seed)
  }

  if (identical(bootstrap_unit, "participant") && is.null(participant_col)) {
    stop("`participant_col` is required when `bootstrap_unit = 'participant'`.", call. = FALSE)
  }

  if (identical(bootstrap_unit, "trial") && is.null(trial_col)) {
    stop("`trial_col` is required when `bootstrap_unit = 'trial'`.", call. = FALSE)
  }

  prepared_data <- tibble::tibble(
    .gp3_row_id = seq_len(nrow(data)),
    participant = if (!is.null(participant_col)) as.character(data[[participant_col]]) else NA_character_,
    trial = if (!is.null(trial_col)) as.character(data[[trial_col]]) else NA_character_,
    time = suppressWarnings(as.numeric(data[[time_col]])),
    condition = as.character(data[[condition_col]]),
    outcome = suppressWarnings(as.numeric(data[[outcome_col]]))
  )

  prepared_data <- prepared_data |>
    dplyr::filter(
      is.finite(.data$time),
      is.finite(.data$outcome),
      !is.na(.data$condition),
      nzchar(.data$condition)
    )

  if (nrow(prepared_data) == 0L) {
    stop("No valid rows remain after removing missing/non-finite time, condition, or outcome values.", call. = FALSE)
  }

  comparison <- .gp3_divergence_resolve_comparison(prepared_data$condition, comparison)

  prepared_data <- prepared_data |>
    dplyr::filter(.data$condition %in% comparison)

  if (nrow(prepared_data) == 0L) {
    stop("No rows remain for the requested condition comparison.", call. = FALSE)
  }

  time_grid <- sort(unique(prepared_data$time))

  observed_curve <- .gp3_divergence_condition_curve(
    prepared_data,
    comparison = comparison,
    time_grid = time_grid,
    summary_function = summary_function
  )

  observed_difference <- .gp3_divergence_difference_curve(
    observed_curve,
    comparison = comparison,
    time_grid = time_grid
  )

  run_bootstrap <- function() {
    .gp3_divergence_bootstrap_differences(
      prepared_data = prepared_data,
      comparison = comparison,
      time_grid = time_grid,
      bootstrap_unit = bootstrap_unit,
      summary_function = summary_function,
      n_boot = n_boot
    )
  }

  boot_differences <- if (!is.null(seed)) {
    withr::with_seed(seed, run_bootstrap())
  } else {
    run_bootstrap()
  }

  alpha <- 1 - ci
  lower_prob <- alpha / 2
  upper_prob <- 1 - alpha / 2

  difference_summary <- boot_differences |>
    dplyr::group_by(.data$time) |>
    dplyr::summarise(
      boot_mean_difference = mean(.data$difference, na.rm = TRUE),
      boot_sd_difference = stats::sd(.data$difference, na.rm = TRUE),
      lower_ci = stats::quantile(.data$difference, probs = lower_prob, na.rm = TRUE, names = FALSE),
      upper_ci = stats::quantile(.data$difference, probs = upper_prob, na.rm = TRUE, names = FALSE),
      prop_positive = mean(.data$difference > null_value, na.rm = TRUE),
      prop_negative = mean(.data$difference < null_value, na.rm = TRUE),
      n_boot_available = sum(!is.na(.data$difference)),
      .groups = "drop"
    ) |>
    dplyr::left_join(observed_difference, by = "time") |>
    dplyr::mutate(
      reliable = .gp3_divergence_reliable_vector(
        lower_ci = .data$lower_ci,
        upper_ci = .data$upper_ci,
        observed_difference = .data$observed_difference,
        null_value = null_value,
        min_abs_difference = min_abs_difference,
        direction = direction
      )
    ) |>
    dplyr::arrange(.data$time)

  observed_onset <- .gp3_divergence_first_run(
    times = difference_summary$time,
    reliable = difference_summary$reliable,
    consecutive_points = consecutive_points
  )

  if (is.na(observed_onset)) {
    detector_status <- "no_reliable_divergence"
    observed_direction <- NA_character_
    observed_difference_at_onset <- NA_real_
    bootstrap_threshold <- min_abs_difference
  } else {
    detector_status <- "complete"
    onset_row <- difference_summary[difference_summary$time == observed_onset, , drop = FALSE]
    observed_difference_at_onset <- onset_row$observed_difference[[1]]
    observed_direction <- if (observed_difference_at_onset > null_value) {
      "positive"
    } else if (observed_difference_at_onset < null_value) {
      "negative"
    } else {
      "zero"
    }

    bootstrap_threshold <- if (min_abs_difference > 0) {
      min_abs_difference
    } else {
      abs(observed_difference_at_onset - null_value) / 2
    }
  }

  bootstrap_onsets <- .gp3_divergence_bootstrap_onsets(
    boot_differences = boot_differences,
    time_grid = time_grid,
    consecutive_points = consecutive_points,
    null_value = null_value,
    direction = direction,
    observed_direction = observed_direction,
    bootstrap_threshold = bootstrap_threshold
  )

  if (is.na(observed_onset)) {
    bootstrap_onsets$divergence_time <- NA_real_
  }

  onset_values <- bootstrap_onsets$divergence_time
  onset_values <- onset_values[!is.na(onset_values)]

  onset_ci <- if (length(onset_values) > 0L) {
    stats::quantile(
      onset_values,
      probs = c(lower_prob, 0.5, upper_prob),
      na.rm = TRUE,
      names = FALSE
    )
  } else {
    c(NA_real_, NA_real_, NA_real_)
  }

  divergence_point <- tibble::tibble(
    object_name = name,
    comparison_reference = comparison[[1]],
    comparison_test = comparison[[2]],
    difference_label = paste0(comparison[[2]], " - ", comparison[[1]]),
    divergence_time = observed_onset,
    divergence_time_lower_ci = onset_ci[[1]],
    divergence_time_median_bootstrap = onset_ci[[2]],
    divergence_time_upper_ci = onset_ci[[3]],
    observed_difference_at_onset = observed_difference_at_onset,
    observed_direction = observed_direction,
    bootstrap_onset_detection_rate = mean(!is.na(bootstrap_onsets$divergence_time)),
    detector_status = detector_status
  )

  overview <- tibble::tibble(
    object_name = name,
    detector_status = detector_status,
    analysis_type = "divergence_point",
    comparison_reference = comparison[[1]],
    comparison_test = comparison[[2]],
    difference_label = paste0(comparison[[2]], " - ", comparison[[1]]),
    outcome_col = outcome_col,
    time_col = time_col,
    condition_col = condition_col,
    bootstrap_unit = bootstrap_unit,
    n_input_rows = nrow(data),
    n_rows_used = nrow(prepared_data),
    n_time_points = length(time_grid),
    n_boot = n_boot,
    ci = ci,
    consecutive_points = consecutive_points,
    divergence_time = observed_onset,
    divergence_time_lower_ci = onset_ci[[1]],
    divergence_time_upper_ci = onset_ci[[3]]
  )

  settings <- tibble::tibble(
    setting = c(
      "outcome_col",
      "time_col",
      "condition_col",
      "participant_col",
      "trial_col",
      "comparison",
      "bootstrap_unit",
      "summary_function",
      "n_boot",
      "ci",
      "consecutive_points",
      "null_value",
      "min_abs_difference",
      "direction",
      "seed",
      "keep_bootstrap",
      "name"
    ),
    value = c(
      outcome_col,
      time_col,
      condition_col,
      .gp3_divergence_collapse_nullable(participant_col),
      .gp3_divergence_collapse_nullable(trial_col),
      paste(comparison, collapse = ", "),
      bootstrap_unit,
      summary_function,
      as.character(n_boot),
      as.character(ci),
      as.character(consecutive_points),
      as.character(null_value),
      as.character(min_abs_difference),
      direction,
      .gp3_divergence_collapse_nullable(seed),
      as.character(keep_bootstrap),
      name
    )
  )

  out <- list(
    overview = overview,
    divergence_point = divergence_point,
    observed_curve = observed_curve,
    difference_summary = difference_summary,
    bootstrap_onsets = bootstrap_onsets,
    bootstrap_differences = if (isTRUE(keep_bootstrap)) boot_differences else NULL,
    settings = settings
  )

  class(out) <- c("gp3_divergence_point_analysis", "list")

  out
}

.gp3_divergence_condition_curve <- function(data, comparison, time_grid, summary_function) {
  summary_fun <- switch(
    summary_function,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE)
  )

  curve <- data |>
    dplyr::group_by(.data$condition, .data$time) |>
    dplyr::summarise(
      estimate = summary_fun(.data$outcome),
      n = dplyr::n(),
      .groups = "drop"
    )

  full_grid <- expand.grid(
    condition = comparison,
    time = time_grid,
    stringsAsFactors = FALSE
  )

  full_grid |>
    tibble::as_tibble() |>
    dplyr::left_join(curve, by = c("condition", "time")) |>
    dplyr::arrange(.data$condition, .data$time)
}

.gp3_divergence_difference_curve <- function(curve, comparison, time_grid) {
  ref <- curve |>
    dplyr::filter(.data$condition == comparison[[1]]) |>
    dplyr::select("time", reference_estimate = "estimate", reference_n = "n")

  test <- curve |>
    dplyr::filter(.data$condition == comparison[[2]]) |>
    dplyr::select("time", test_estimate = "estimate", test_n = "n")

  tibble::tibble(time = time_grid) |>
    dplyr::left_join(ref, by = "time") |>
    dplyr::left_join(test, by = "time") |>
    dplyr::mutate(
      observed_difference = .data$test_estimate - .data$reference_estimate
    ) |>
    dplyr::arrange(.data$time)
}

.gp3_divergence_bootstrap_differences <- function(
    prepared_data,
    comparison,
    time_grid,
    bootstrap_unit,
    summary_function,
    n_boot
) {
  rows <- vector("list", n_boot)

  for (boot_id in seq_len(n_boot)) {
    boot_data <- .gp3_divergence_resample_data(
      prepared_data = prepared_data,
      bootstrap_unit = bootstrap_unit
    )

    boot_curve <- .gp3_divergence_condition_curve(
      data = boot_data,
      comparison = comparison,
      time_grid = time_grid,
      summary_function = summary_function
    )

    rows[[boot_id]] <- .gp3_divergence_difference_curve(
      curve = boot_curve,
      comparison = comparison,
      time_grid = time_grid
    ) |>
      dplyr::transmute(
        boot_id = boot_id,
        time = .data$time,
        difference = .data$observed_difference
      )
  }

  dplyr::bind_rows(rows)
}

.gp3_divergence_resample_data <- function(prepared_data, bootstrap_unit) {
  if (identical(bootstrap_unit, "row")) {
    sampled_rows <- sample(seq_len(nrow(prepared_data)), size = nrow(prepared_data), replace = TRUE)
    return(prepared_data[sampled_rows, , drop = FALSE])
  }

  if (identical(bootstrap_unit, "participant")) {
    units <- unique(prepared_data$participant)
    sampled_units <- sample(units, size = length(units), replace = TRUE)

    return(.gp3_divergence_bind_sampled_units(
      data = prepared_data,
      unit_values = sampled_units,
      unit_col = "participant"
    ))
  }

  prepared_data <- prepared_data |>
    dplyr::mutate(
      .gp3_trial_unit = ifelse(
        is.na(.data$participant),
        .data$trial,
        paste(.data$participant, .data$trial, sep = "||")
      )
    )

  units <- unique(prepared_data$.gp3_trial_unit)
  sampled_units <- sample(units, size = length(units), replace = TRUE)

  out <- .gp3_divergence_bind_sampled_units(
    data = prepared_data,
    unit_values = sampled_units,
    unit_col = ".gp3_trial_unit"
  )

  out$.gp3_trial_unit <- NULL

  out
}

.gp3_divergence_bind_sampled_units <- function(data, unit_values, unit_col) {
  rows <- vector("list", length(unit_values))

  for (i in seq_along(unit_values)) {
    unit_rows <- data[data[[unit_col]] == unit_values[[i]], , drop = FALSE]
    unit_rows$.gp3_bootstrap_unit_index <- i
    rows[[i]] <- unit_rows
  }

  dplyr::bind_rows(rows)
}

.gp3_divergence_reliable_vector <- function(
    lower_ci,
    upper_ci,
    observed_difference,
    null_value,
    min_abs_difference,
    direction
) {
  magnitude_ok <- abs(observed_difference - null_value) >= min_abs_difference

  reliable <- switch(
    direction,
    two_sided = (lower_ci > null_value | upper_ci < null_value) & magnitude_ok,
    positive = lower_ci > null_value & magnitude_ok,
    negative = upper_ci < null_value & magnitude_ok
  )

  reliable[is.na(reliable)] <- FALSE

  reliable
}

.gp3_divergence_bootstrap_onsets <- function(
    boot_differences,
    time_grid,
    consecutive_points,
    null_value,
    direction,
    observed_direction,
    bootstrap_threshold
) {
  split_boot <- split(boot_differences, boot_differences$boot_id)

  rows <- lapply(names(split_boot), function(id) {
    x <- split_boot[[id]]
    x <- x[match(time_grid, x$time), , drop = FALSE]

    reliable <- .gp3_divergence_boot_reliable(
      difference = x$difference,
      null_value = null_value,
      direction = direction,
      observed_direction = observed_direction,
      bootstrap_threshold = bootstrap_threshold
    )

    onset <- .gp3_divergence_first_run(
      times = time_grid,
      reliable = reliable,
      consecutive_points = consecutive_points
    )

    tibble::tibble(
      boot_id = as.integer(id),
      divergence_time = onset
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_divergence_boot_reliable <- function(
    difference,
    null_value,
    direction,
    observed_direction,
    bootstrap_threshold
) {
  if (identical(direction, "positive")) {
    out <- difference > null_value + bootstrap_threshold
  } else if (identical(direction, "negative")) {
    out <- difference < null_value - bootstrap_threshold
  } else if (identical(observed_direction, "positive")) {
    out <- difference > null_value + bootstrap_threshold
  } else if (identical(observed_direction, "negative")) {
    out <- difference < null_value - bootstrap_threshold
  } else {
    out <- abs(difference - null_value) > bootstrap_threshold
  }

  out[is.na(out)] <- FALSE

  out
}

.gp3_divergence_first_run <- function(times, reliable, consecutive_points) {
  if (length(times) == 0L || length(reliable) == 0L) {
    return(NA_real_)
  }

  if (!any(reliable)) {
    return(NA_real_)
  }

  n <- length(reliable)

  for (i in seq_len(n)) {
    end_i <- i + consecutive_points - 1L

    if (end_i > n) {
      break
    }

    if (all(reliable[i:end_i])) {
      return(times[[i]])
    }
  }

  NA_real_
}

.gp3_divergence_resolve_comparison <- function(condition, comparison) {
  condition <- sort(unique(condition[!is.na(condition) & nzchar(condition)]))

  if (!is.null(comparison)) {
    if (!is.character(comparison) || length(comparison) != 2L || anyNA(comparison) || any(!nzchar(comparison))) {
      stop("`comparison` must be NULL or a character vector of two condition values.", call. = FALSE)
    }

    missing_conditions <- setdiff(comparison, condition)

    if (length(missing_conditions) > 0L) {
      stop("All values in `comparison` must be present in `condition_col`.", call. = FALSE)
    }

    return(comparison)
  }

  if (length(condition) != 2L) {
    stop("`condition_col` must contain exactly two conditions unless `comparison` is supplied.", call. = FALSE)
  }

  condition
}

.gp3_divergence_check_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1 || x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_ci <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x >= 1) {
    stop("`ci` must be a finite number between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_finite_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", arg, "` must be a finite number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_nonnegative_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) {
    stop("`", arg, "` must be a finite non-negative number.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_logical <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_check_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
    stop("`seed` must be NULL or a finite numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_divergence_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
