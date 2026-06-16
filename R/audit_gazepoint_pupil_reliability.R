#' Audit split-half reliability for Gazepoint pupil outcomes
#'
#' Create a split-half reliability audit for trial-level or window-level pupil
#' outcomes. The helper is intended for publication-readiness checks when pupil
#' features are interpreted as stable participant-level outcomes or individual
#' difference measures.
#'
#' @param data A data frame containing trial-level or window-level pupil outcomes.
#' @param outcome_cols Character vector of pupil outcome columns. If `NULL`,
#'   common pupil outcome columns are detected automatically.
#' @param participant_col Participant/subject column. If `NULL`, common
#'   participant columns are detected automatically.
#' @param trial_col Trial/order column. If `NULL`, common trial columns are
#'   detected automatically when available. If no trial column is available, row
#'   order within participant is used.
#' @param split_col Optional pre-existing split column. If supplied, it must have
#'   exactly two non-missing levels.
#' @param by_cols Optional grouping columns for separate reliability audits, such
#'   as `"condition"` or `"window"`.
#' @param split_method Split method used when `split_col = NULL`. Options are
#'   `"odd_even"` and `"first_second"`.
#' @param aggregate_function Function used to aggregate trial-level values within
#'   participant and split. Options are `"mean"` and `"median"`.
#' @param correlation_method Correlation method for split-half association.
#'   Options are `"pearson"` and `"spearman"`.
#' @param min_trials_per_split Minimum number of non-missing outcome values
#'   required in each split for a participant to contribute to the reliability
#'   estimate.
#' @param name Character label stored in the audit object.
#'
#' @return A list with class `gp3_pupil_reliability_audit`.
#' @export
audit_gazepoint_pupil_reliability <- function(
    data,
    outcome_cols = NULL,
    participant_col = NULL,
    trial_col = NULL,
    split_col = NULL,
    by_cols = NULL,
    split_method = c("odd_even", "first_second"),
    aggregate_function = c("mean", "median"),
    correlation_method = c("pearson", "spearman"),
    min_trials_per_split = 2,
    name = "gazepoint_pupil_reliability"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  split_method <- match.arg(split_method)
  aggregate_function <- match.arg(aggregate_function)
  correlation_method <- match.arg(correlation_method)

  .gp3_pupil_rel_check_positive_integer(
    min_trials_per_split,
    "min_trials_per_split"
  )
  .gp3_pupil_rel_check_label(name, "name")

  names_data <- names(data)

  participant_col <- .gp3_pupil_rel_resolve_or_detect_col(
    col = participant_col,
    names_data = names_data,
    arg = "participant_col",
    candidates = c(
      "subject",
      "participant",
      "participant_id",
      "pID",
      "USER_FILE",
      "user",
      "user_id",
      "recording_id"
    ),
    required = TRUE
  )

  trial_col <- .gp3_pupil_rel_resolve_or_detect_col(
    col = trial_col,
    names_data = names_data,
    arg = "trial_col",
    candidates = c(
      "trial_global",
      "trial",
      "trial_id",
      "TRIAL_INDEX",
      "trial_number",
      "item_trial",
      "sample_index"
    ),
    required = FALSE
  )

  if (!is.null(split_col)) {
    split_col <- .gp3_pupil_rel_resolve_col(
      split_col,
      names_data,
      "split_col"
    )
  }

  if (!is.null(by_cols)) {
    by_cols <- .gp3_pupil_rel_resolve_cols_allow_empty(
      by_cols,
      names_data,
      "by_cols"
    )
  } else {
    by_cols <- character(0)
  }

  if (!is.null(outcome_cols)) {
    outcome_cols <- .gp3_pupil_rel_resolve_cols(
      outcome_cols,
      names_data,
      "outcome_cols"
    )
  } else {
    outcome_cols <- .gp3_pupil_rel_detect_outcomes(
      data = data,
      exclude_cols = c(participant_col, trial_col, split_col, by_cols)
    )
  }

  outcome_cols <- outcome_cols[
    vapply(data[outcome_cols], is.numeric, logical(1))
  ]

  if (length(outcome_cols) == 0L) {
    stop(
      "`outcome_cols` could not be detected and must include at least one numeric column.",
      call. = FALSE
    )
  }

  by_cols <- setdiff(
    by_cols,
    c(participant_col, trial_col, split_col, outcome_cols)
  )

  trial_values <- if (!is.null(trial_col)) {
    data[[trial_col]]
  } else {
    seq_len(nrow(data))
  }

  trial_order <- .gp3_pupil_rel_extract_trial_order(trial_values)

  split_data <- tibble::tibble(
    .row_id = seq_len(nrow(data)),
    participant = as.character(data[[participant_col]]),
    trial = as.character(trial_values),
    trial_order = trial_order
  )

  if (length(by_cols) > 0L) {
    split_data <- dplyr::bind_cols(
      split_data,
      tibble::as_tibble(data[by_cols])
    )
  }

  split_data <- dplyr::bind_cols(
    split_data,
    tibble::as_tibble(data[outcome_cols])
  )

  if (!is.null(split_col)) {
    raw_split <- as.character(data[[split_col]])
    raw_split[is.na(raw_split) | !nzchar(raw_split)] <- NA_character_

    split_levels <- sort(unique(raw_split[!is.na(raw_split)]))

    if (length(split_levels) != 2L) {
      stop("`split_col` must contain exactly two non-missing split levels.", call. = FALSE)
    }

    split_data$split <- raw_split
    split_data <- split_data[!is.na(split_data$split), , drop = FALSE]
  } else {
    split_group_cols <- c("participant", by_cols)

    split_data <- split_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(split_group_cols))) |>
      dplyr::arrange(
        .data$trial_order,
        .data$.row_id,
        .by_group = TRUE
      ) |>
      dplyr::mutate(
        .within_group_order = dplyr::row_number(),
        .n_in_group = dplyr::n(),
        .split_number = dplyr::if_else(
          is.na(.data$trial_order),
          as.numeric(.data$.within_group_order),
          as.numeric(.data$trial_order)
        )
      ) |>
      dplyr::ungroup()

    if (identical(split_method, "odd_even")) {
      split_data$split <- ifelse(
        split_data$.split_number %% 2 == 0,
        "even",
        "odd"
      )
    } else {
      split_data$split <- ifelse(
        split_data$.within_group_order <= ceiling(split_data$.n_in_group / 2),
        "first",
        "second"
      )
    }

    split_levels <- if (identical(split_method, "odd_even")) {
      c("odd", "even")
    } else {
      c("first", "second")
    }
  }

  split_data$split <- factor(split_data$split, levels = split_levels)

  long_data <- tidyr::pivot_longer(
    split_data,
    cols = dplyr::all_of(outcome_cols),
    names_to = "outcome",
    values_to = "value"
  )

  split_summary <- long_data |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("participant", by_cols, "outcome", "split")))
    ) |>
    dplyr::summarise(
      n_trials = dplyr::n(),
      n_valid = sum(!is.na(.data$value) & is.finite(.data$value)),
      split_value = .gp3_pupil_rel_aggregate(
        .data$value,
        aggregate_function = aggregate_function
      ),
      .groups = "drop"
    )

  reliability_pairs <- .gp3_pupil_rel_create_pairs(
    split_summary = split_summary,
    by_cols = by_cols,
    split_levels = split_levels
  )

  if (nrow(reliability_pairs) > 0L) {
    reliability_pairs <- reliability_pairs |>
      dplyr::mutate(
        complete_pair = !is.na(.data$split1_value) &
          !is.na(.data$split2_value) &
          is.finite(.data$split1_value) &
          is.finite(.data$split2_value) &
          .data$split1_n_valid >= min_trials_per_split &
          .data$split2_n_valid >= min_trials_per_split
      )
  } else {
    reliability_pairs$complete_pair <- logical(0)
  }

  reliability_summary <- .gp3_pupil_rel_summarise_reliability(
    reliability_pairs = reliability_pairs,
    by_cols = by_cols,
    correlation_method = correlation_method
  )

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_rows_used = nrow(split_data),
    n_participants = dplyr::n_distinct(split_data$participant),
    n_outcomes = length(outcome_cols),
    n_by_groups = .gp3_pupil_rel_n_by_groups(split_data, by_cols),
    n_reliability_rows = nrow(reliability_summary),
    n_ready_reliability_rows = sum(
      reliability_summary$reliability_status == "ready",
      na.rm = TRUE
    ),
    split_method = if (is.null(split_col)) split_method else "predefined_split_col",
    aggregate_function = aggregate_function,
    correlation_method = correlation_method,
    min_trials_per_split = min_trials_per_split
  )

  settings <- tibble::tibble(
    setting = c(
      "outcome_cols",
      "participant_col",
      "trial_col",
      "split_col",
      "by_cols",
      "split_method",
      "aggregate_function",
      "correlation_method",
      "min_trials_per_split",
      "name"
    ),
    value = c(
      .gp3_pupil_rel_collapse_nullable(outcome_cols),
      participant_col,
      .gp3_pupil_rel_collapse_nullable(trial_col),
      .gp3_pupil_rel_collapse_nullable(split_col),
      .gp3_pupil_rel_collapse_nullable(by_cols),
      split_method,
      aggregate_function,
      correlation_method,
      as.character(min_trials_per_split),
      name
    )
  )

  out <- list(
    overview = overview,
    split_data = split_data,
    split_summary = split_summary,
    reliability_pairs = reliability_pairs,
    reliability_summary = reliability_summary,
    settings = settings
  )

  class(out) <- c("gp3_pupil_reliability_audit", "list")

  out
}

.gp3_pupil_rel_detect_outcomes <- function(data, exclude_cols) {
  candidates <- c(
    "auc_pupil_0_2000",
    "mean_pupil_0_2000",
    "peak_pupil_0_2000",
    "latency_to_peak_ms",
    "mean_pupil_window",
    "pupil_window_mean",
    "pupil_mean",
    "pupil_peak",
    "pupil_auc",
    "mean_pupil",
    "peak_pupil",
    "auc_pupil",
    "pupil"
  )

  detected <- intersect(candidates, names(data))
  detected <- detected[
    vapply(data[detected], is.numeric, logical(1))
  ]

  if (length(detected) > 0L) {
    return(setdiff(unique(detected), exclude_cols))
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, exclude_cols)

  exclude_pattern <- paste(
    c(
      "^n_",
      "_n$",
      "count",
      "prop",
      "percent",
      "missing",
      "valid",
      "sample",
      "trial",
      "time_bin",
      "order"
    ),
    collapse = "|"
  )

  numeric_cols <- numeric_cols[
    !grepl(exclude_pattern, numeric_cols, ignore.case = TRUE)
  ]

  unique(numeric_cols)
}

.gp3_pupil_rel_extract_trial_order <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(suppressWarnings(as.numeric(x)))
  }

  x_chr <- as.character(x)

  matches <- gregexpr("[0-9]+", x_chr)
  extracted <- regmatches(x_chr, matches)

  vapply(extracted, function(z) {
    if (length(z) == 0L) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(utils::tail(z, 1)))
  }, numeric(1))
}

.gp3_pupil_rel_aggregate <- function(x, aggregate_function) {
  x <- x[!is.na(x) & is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  if (identical(aggregate_function, "median")) {
    return(stats::median(x, na.rm = TRUE))
  }

  mean(x, na.rm = TRUE)
}

.gp3_pupil_rel_create_pairs <- function(split_summary, by_cols, split_levels) {
  split1 <- split_summary[split_summary$split == split_levels[[1]], , drop = FALSE]
  split2 <- split_summary[split_summary$split == split_levels[[2]], , drop = FALSE]

  key_cols <- c("participant", by_cols, "outcome")

  split1 <- split1 |>
    dplyr::select(
      dplyr::all_of(c(key_cols, "n_trials", "n_valid", "split_value"))
    ) |>
    dplyr::rename(
      split1_n_trials = "n_trials",
      split1_n_valid = "n_valid",
      split1_value = "split_value"
    )

  split2 <- split2 |>
    dplyr::select(
      dplyr::all_of(c(key_cols, "n_trials", "n_valid", "split_value"))
    ) |>
    dplyr::rename(
      split2_n_trials = "n_trials",
      split2_n_valid = "n_valid",
      split2_value = "split_value"
    )

  out <- merge(
    split1,
    split2,
    by = key_cols,
    all = TRUE,
    sort = FALSE
  )

  out <- tibble::as_tibble(out)

  out$split1_label <- split_levels[[1]]
  out$split2_label <- split_levels[[2]]

  out
}

.gp3_pupil_rel_summarise_reliability <- function(
    reliability_pairs,
    by_cols,
    correlation_method
) {
  if (nrow(reliability_pairs) == 0L) {
    return(
      tibble::tibble(
        outcome = character(),
        n_participants = integer(),
        n_complete_pairs = integer(),
        split1_label = character(),
        split2_label = character(),
        split1_mean = numeric(),
        split2_mean = numeric(),
        split1_sd = numeric(),
        split2_sd = numeric(),
        split_half_correlation = numeric(),
        spearman_brown_reliability = numeric(),
        reliability_status = character()
      )
    )
  }

  group_cols <- c(by_cols, "outcome")

  reliability_pairs |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_participants = dplyr::n_distinct(.data$participant),
      n_complete_pairs = sum(.data$complete_pair, na.rm = TRUE),
      split1_label = dplyr::first(.data$split1_label),
      split2_label = dplyr::first(.data$split2_label),
      split1_mean = mean(.data$split1_value[.data$complete_pair], na.rm = TRUE),
      split2_mean = mean(.data$split2_value[.data$complete_pair], na.rm = TRUE),
      split1_sd = stats::sd(.data$split1_value[.data$complete_pair], na.rm = TRUE),
      split2_sd = stats::sd(.data$split2_value[.data$complete_pair], na.rm = TRUE),
      split_half_correlation = .gp3_pupil_rel_safe_cor(
        .data$split1_value[.data$complete_pair],
        .data$split2_value[.data$complete_pair],
        method = correlation_method
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      spearman_brown_reliability = .gp3_pupil_rel_spearman_brown(
        .data$split_half_correlation
      ),
      reliability_status = .gp3_pupil_rel_status(
        n_complete_pairs = .data$n_complete_pairs,
        split1_sd = .data$split1_sd,
        split2_sd = .data$split2_sd,
        correlation = .data$split_half_correlation
      )
    )
}

.gp3_pupil_rel_safe_cor <- function(x, y, method) {
  valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]

  if (length(x) < 3L) {
    return(NA_real_)
  }

  if (stats::sd(x, na.rm = TRUE) == 0 || stats::sd(y, na.rm = TRUE) == 0) {
    return(NA_real_)
  }

  suppressWarnings(stats::cor(x, y, method = method))
}

.gp3_pupil_rel_spearman_brown <- function(r) {
  out <- rep(NA_real_, length(r))
  valid <- !is.na(r) & is.finite(r) & (1 + r) != 0
  out[valid] <- (2 * r[valid]) / (1 + r[valid])
  out
}

.gp3_pupil_rel_status <- function(
    n_complete_pairs,
    split1_sd,
    split2_sd,
    correlation
) {
  dplyr::case_when(
    n_complete_pairs < 3 ~ "too_few_complete_pairs",
    is.na(split1_sd) | is.na(split2_sd) ~ "too_few_complete_pairs",
    split1_sd == 0 | split2_sd == 0 ~ "constant_split_values",
    is.na(correlation) | !is.finite(correlation) ~ "correlation_unavailable",
    TRUE ~ "ready"
  )
}

.gp3_pupil_rel_n_by_groups <- function(split_data, by_cols) {
  if (length(by_cols) == 0L) {
    return(1L)
  }

  split_data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(by_cols))) |>
    nrow()
}

.gp3_pupil_rel_resolve_cols <- function(cols, names_data, arg) {
  if (!is.character(cols) || length(cols) == 0L || anyNA(cols)) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(cols, names_data)

  if (length(missing_cols) > 0L) {
    stop("All `", arg, "` must be present in `data`.", call. = FALSE)
  }

  cols
}

.gp3_pupil_rel_resolve_cols_allow_empty <- function(cols, names_data, arg) {
  if (!is.character(cols) || anyNA(cols)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }

  if (length(cols) == 0L) {
    return(character(0))
  }

  .gp3_pupil_rel_resolve_cols(cols, names_data, arg)
}

.gp3_pupil_rel_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_pupil_rel_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_pupil_rel_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_pupil_rel_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pupil_rel_check_positive_integer <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 1) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  if (x != as.integer(x)) {
    stop("`", arg, "` must be a positive integer.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_pupil_rel_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
