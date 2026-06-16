#' Audit condition-level quality imbalance
#'
#' Create a publication-level audit of whether gaze, pupil, retention, or other
#' quality metrics differ across experimental conditions.
#'
#' @param data A data frame containing condition-level, unit-level, or
#'   subject-condition-level quality metrics.
#' @param condition_col Condition column.
#' @param quality_cols Numeric quality-metric columns. If `NULL`, common
#'   quality columns are detected automatically.
#' @param subject_col Optional subject column.
#' @param min_units_per_condition Minimum number of rows/units expected per
#'   condition.
#' @param max_mean_difference Maximum acceptable absolute difference between
#'   condition means for each quality metric.
#' @param max_condition_ratio Maximum acceptable ratio between the largest and
#'   smallest non-zero condition mean for each quality metric.
#' @param lower_is_better Optional character vector naming metrics where lower
#'   values indicate better quality, such as missing-gaze or exclusion metrics.
#'
#' @return A list with class `gp3_condition_quality_imbalance_audit`
#'   containing overview, condition_summary, metric_summary, flagged_metrics,
#'   and settings tables.
#' @export
audit_gazepoint_condition_quality_imbalance <- function(
    data,
    condition_col = "condition",
    quality_cols = NULL,
    subject_col = NULL,
    min_units_per_condition = 1L,
    max_mean_difference = 0.10,
    max_condition_ratio = 2,
    lower_is_better = c(
      "missing_gaze_prop",
      "offscreen_prop",
      "excluded_prop",
      "failure_prop",
      "artifact_prop"
    )
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_condition_quality_standardise_aliases(data)

  condition_col <- .gp3_condition_quality_resolve_col(
    condition_col,
    names(data),
    "condition_col"
  )

  subject_col <- .gp3_condition_quality_resolve_optional_col(
    subject_col,
    names(data),
    "subject_col"
  )

  if (is.null(quality_cols)) {
    quality_cols <- .gp3_condition_quality_detect_quality_cols(names(data))
  } else {
    .gp3_condition_quality_check_character_vector(
      quality_cols,
      "quality_cols"
    )

    missing_quality_cols <- setdiff(quality_cols, names(data))

    if (length(missing_quality_cols) > 0L) {
      stop("All `quality_cols` must be present in `data`.", call. = FALSE)
    }
  }

  if (length(quality_cols) == 0L) {
    stop(
      "No quality columns were detected. Supply `quality_cols` explicitly.",
      call. = FALSE
    )
  }

  non_numeric <- quality_cols[
    !vapply(data[quality_cols], is.numeric, logical(1))
  ]

  if (length(non_numeric) > 0L) {
    stop("All `quality_cols` must be numeric.", call. = FALSE)
  }

  .gp3_condition_quality_check_positive_numeric(
    min_units_per_condition,
    "min_units_per_condition"
  )

  .gp3_condition_quality_check_nonnegative_numeric(
    max_mean_difference,
    "max_mean_difference"
  )

  .gp3_condition_quality_check_positive_numeric(
    max_condition_ratio,
    "max_condition_ratio"
  )

  .gp3_condition_quality_check_character_vector(
    lower_is_better,
    "lower_is_better"
  )

  data[[condition_col]] <- as.character(data[[condition_col]])

  data <- data[
    !is.na(data[[condition_col]]) & nzchar(data[[condition_col]]),
    ,
    drop = FALSE
  ]

  if (nrow(data) == 0L) {
    stop("`condition_col` must contain at least one usable condition.", call. = FALSE)
  }

  condition_summary <- .gp3_condition_quality_create_condition_summary(
    data = data,
    condition_col = condition_col,
    quality_cols = quality_cols,
    subject_col = subject_col,
    min_units_per_condition = min_units_per_condition
  )

  metric_summary <- .gp3_condition_quality_create_metric_summary(
    condition_summary = condition_summary,
    condition_col = condition_col,
    quality_cols = quality_cols,
    max_mean_difference = max_mean_difference,
    max_condition_ratio = max_condition_ratio,
    lower_is_better = lower_is_better
  )

  flagged_metrics <- metric_summary[
    metric_summary$condition_quality_imbalance_status != "ok",
    ,
    drop = FALSE
  ]

  n_low_n_conditions <- sum(
    condition_summary$condition_n_status != "ok",
    na.rm = TRUE
  )

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_conditions = length(unique(data[[condition_col]])),
    n_quality_metrics = length(quality_cols),
    n_flagged_metrics = nrow(flagged_metrics),
    n_low_n_conditions = n_low_n_conditions,
    condition_quality_imbalance_status = dplyr::case_when(
      n_low_n_conditions > 0L ~ "review",
      nrow(flagged_metrics) > 0L ~ "review",
      TRUE ~ "ok"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "condition_col",
      "quality_cols",
      "subject_col",
      "min_units_per_condition",
      "max_mean_difference",
      "max_condition_ratio",
      "lower_is_better"
    ),
    value = c(
      condition_col,
      paste(quality_cols, collapse = ", "),
      .gp3_condition_quality_collapse_nullable(subject_col),
      as.character(min_units_per_condition),
      as.character(max_mean_difference),
      as.character(max_condition_ratio),
      paste(lower_is_better, collapse = ", ")
    )
  )

  out <- list(
    overview = overview,
    condition_summary = condition_summary,
    metric_summary = metric_summary,
    flagged_metrics = flagged_metrics,
    settings = settings
  )

  class(out) <- c("gp3_condition_quality_imbalance_audit", "list")

  out
}

.gp3_condition_quality_create_condition_summary <- function(
    data,
    condition_col,
    quality_cols,
    subject_col,
    min_units_per_condition
) {
  split_idx <- split(
    seq_len(nrow(data)),
    data[[condition_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- data[idx, , drop = FALSE]

    base <- tibble::tibble(
      condition = as.character(d[[condition_col]][[1]]),
      n_units = nrow(d),
      n_subjects = if (!is.null(subject_col)) {
        length(unique(d[[subject_col]]))
      } else {
        NA_integer_
      },
      condition_n_status = ifelse(
        nrow(d) < min_units_per_condition,
        "too_few_units",
        "ok"
      )
    )

    metric_parts <- lapply(quality_cols, function(metric) {
      values <- d[[metric]]
      finite_values <- values[is.finite(values)]
      n_nonmissing <- sum(!is.na(values))

      metric_mean <- if (length(finite_values) > 0L) {
        mean(finite_values)
      } else {
        NA_real_
      }

      metric_sd <- if (length(finite_values) > 1L) {
        stats::sd(finite_values)
      } else {
        NA_real_
      }

      metric_min <- if (length(finite_values) > 0L) {
        min(finite_values)
      } else {
        NA_real_
      }

      metric_max <- if (length(finite_values) > 0L) {
        max(finite_values)
      } else {
        NA_real_
      }

      out <- tibble::tibble(
        metric_mean,
        metric_sd,
        metric_min,
        metric_max,
        n_nonmissing
      )

      names(out) <- c(
        paste0(metric, "_mean"),
        paste0(metric, "_sd"),
        paste0(metric, "_min"),
        paste0(metric, "_max"),
        paste0(metric, "_n_nonmissing")
      )

      out
    })

    cbind(base, dplyr::bind_cols(metric_parts))
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "condition"] <- condition_col

  out
}
.gp3_condition_quality_create_metric_summary <- function(
    condition_summary,
    condition_col,
    quality_cols,
    max_mean_difference,
    max_condition_ratio,
    lower_is_better
) {
  rows <- lapply(quality_cols, function(metric) {
    mean_col <- paste0(metric, "_mean")
    means <- condition_summary[[mean_col]]
    condition_values <- condition_summary[[condition_col]]

    finite_means <- means[is.finite(means)]

    min_mean <- ifelse(length(finite_means) > 0L, min(finite_means), NA_real_)
    max_mean <- ifelse(length(finite_means) > 0L, max(finite_means), NA_real_)
    mean_difference <- max_mean - min_mean

    ratio <- .gp3_condition_quality_ratio(finite_means)

    worst_condition <- .gp3_condition_quality_worst_condition(
      metric = metric,
      means = means,
      condition_values = condition_values,
      lower_is_better = lower_is_better
    )

    status <- .gp3_condition_quality_metric_status(
      mean_difference = mean_difference,
      ratio = ratio,
      max_mean_difference = max_mean_difference,
      max_condition_ratio = max_condition_ratio
    )

    tibble::tibble(
      quality_metric = metric,
      n_conditions = length(condition_values),
      min_condition_mean = min_mean,
      max_condition_mean = max_mean,
      mean_difference = mean_difference,
      condition_ratio = ratio,
      worst_condition = worst_condition,
      metric_direction = ifelse(
        metric %in% lower_is_better,
        "lower_is_better",
        "higher_is_better"
      ),
      condition_quality_imbalance_status = status
    )
  })

  dplyr::bind_rows(rows)
}

.gp3_condition_quality_metric_status <- function(
    mean_difference,
    ratio,
    max_mean_difference,
    max_condition_ratio
) {
  if (is.na(mean_difference) || is.na(ratio)) {
    return("insufficient_data")
  }

  if (mean_difference > max_mean_difference) {
    return("mean_difference_imbalance")
  }

  if (is.infinite(ratio) || ratio > max_condition_ratio) {
    return("condition_ratio_imbalance")
  }

  "ok"
}

.gp3_condition_quality_ratio <- function(values) {
  values <- values[is.finite(values)]

  if (length(values) <= 1L) {
    return(NA_real_)
  }

  if (all(values == 0)) {
    return(1)
  }

  if (any(values == 0) && any(values > 0)) {
    return(Inf)
  }

  nonzero <- values[values > 0]

  if (length(nonzero) <= 1L) {
    return(NA_real_)
  }

  max(nonzero) / min(nonzero)
}

.gp3_condition_quality_worst_condition <- function(
    metric,
    means,
    condition_values,
    lower_is_better
) {
  if (all(is.na(means))) {
    return(NA_character_)
  }

  if (metric %in% lower_is_better) {
    return(as.character(condition_values[which.max(means)][[1]]))
  }

  as.character(condition_values[which.min(means)][[1]])
}

.gp3_condition_quality_detect_quality_cols <- function(names_data) {
  candidates <- c(
    "gaze_valid_prop",
    "missing_gaze_prop",
    "offscreen_prop",
    "pupil_valid_prop",
    "retained_prop",
    "excluded_prop",
    "valid_sample_prop",
    "valid_pupil_prop",
    "valid_gaze_prop",
    "tracking_quality_prop",
    "artifact_prop",
    "failure_prop"
  )

  candidates[candidates %in% names_data]
}

.gp3_condition_quality_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_condition_quality_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (col == "USER_FILE" && "subject" %in% names_data) {
    return("subject")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_condition_quality_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_condition_quality_resolve_col(col, names_data, arg)
}

.gp3_condition_quality_check_positive_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_condition_quality_check_nonnegative_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0) {
    stop("`", arg, "` must be a non-negative numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_condition_quality_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_condition_quality_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
