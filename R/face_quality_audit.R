#' Audit external facial-behaviour data quality
#'
#' Audits the quality of standardised external facial-behaviour data imported
#' with `read_gazepoint_face_export()` and standardised with
#' `standardize_gazepoint_face_columns()`. The helper checks face-detection
#' validity, confidence, success, duplicate frame indices, and basic timing
#' continuity. It does not infer facial expressions or emotional states.
#'
#' @param data A data frame returned by `standardize_gazepoint_face_columns()`,
#'   a data frame that can be standardised by that function, or a path to a CSV
#'   file readable by `read_gazepoint_face_export()`.
#' @param group_cols Character vector of grouping columns for quality summaries.
#'   Columns not present in `data` are ignored. Use `NULL` for an overall-only
#'   audit.
#' @param confidence_threshold Minimum face-detection confidence used when
#'   standardising unstandardised data.
#' @param min_valid_percent Minimum valid-row percentage below which a group is
#'   marked as `"fail"`.
#' @param warning_valid_percent Valid-row percentage below which a group is
#'   marked as `"warn"` when it is still above `min_valid_percent`.
#' @param max_time_gap_sec Optional maximum allowed time gap in seconds. If
#'   supplied, groups with larger observed positive gaps are marked as `"warn"`.
#' @param max_duplicate_frame_percent Maximum tolerated percentage of duplicate
#'   non-missing frame indices before a group is marked as `"warn"`.
#' @param standardize Should unstandardised data be passed through
#'   `standardize_gazepoint_face_columns()` before auditing?
#'
#' @return A list with `overview`, `group_summary`, `issue_summary`, `data`, and
#'   `settings`. The returned object has class `gp3_face_quality_audit`.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   frame = 1:3,
#'   timestamp = c(0, 0.033, 0.066),
#'   confidence = c(0.95, 0.90, 0.40),
#'   success = c(1, 1, 1),
#'   AU12_r = c(0.1, 0.2, 0.3)
#' )
#'
#' audit_gazepoint_face_quality(face)
audit_gazepoint_face_quality <- function(data,
                                         group_cols = c(
                                           "participant_id",
                                           "face_file"
                                         ),
                                         confidence_threshold = 0.80,
                                         min_valid_percent = 70,
                                         warning_valid_percent = 85,
                                         max_time_gap_sec = NULL,
                                         max_duplicate_frame_percent = 1,
                                         standardize = TRUE) {
  data <- .gp3_face_quality_prepare_data(
    data = data,
    confidence_threshold = confidence_threshold,
    standardize = standardize
  )

  if (nrow(data) < 1L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (!is.null(group_cols)) {
    group_cols <- intersect(group_cols, names(data))
  }

  if (length(group_cols) < 1L) {
    group_cols <- NULL
  }

  groups <- .gp3_face_quality_group_indices(data, group_cols)

  group_summary <- lapply(groups, function(idx) {
    .gp3_face_quality_summarise_subset(
      data = data,
      idx = idx,
      group_cols = group_cols,
      min_valid_percent = min_valid_percent,
      warning_valid_percent = warning_valid_percent,
      max_time_gap_sec = max_time_gap_sec,
      max_duplicate_frame_percent = max_duplicate_frame_percent
    )
  })

  group_summary <- .gp3_face_quality_bind_rows(group_summary)
  group_summary <- tibble::as_tibble(group_summary)

  overview <- .gp3_face_quality_summarise_subset(
    data = data,
    idx = seq_len(nrow(data)),
    group_cols = NULL,
    min_valid_percent = min_valid_percent,
    warning_valid_percent = warning_valid_percent,
    max_time_gap_sec = max_time_gap_sec,
    max_duplicate_frame_percent = max_duplicate_frame_percent
  )

  overview$face_quality_group <- NULL
  overview <- cbind(
    data.frame(
      n_groups = nrow(group_summary),
      stringsAsFactors = FALSE
    ),
    overview,
    stringsAsFactors = FALSE
  )

  overview <- tibble::as_tibble(overview)

  issue_summary <- .gp3_face_quality_issue_summary(
    group_summary = group_summary,
    min_valid_percent = min_valid_percent,
    warning_valid_percent = warning_valid_percent,
    max_time_gap_sec = max_time_gap_sec,
    max_duplicate_frame_percent = max_duplicate_frame_percent
  )

  out <- list(
    overview = overview,
    group_summary = group_summary,
    issue_summary = issue_summary,
    data = tibble::as_tibble(data),
    settings = list(
      group_cols = group_cols,
      confidence_threshold = confidence_threshold,
      min_valid_percent = min_valid_percent,
      warning_valid_percent = warning_valid_percent,
      max_time_gap_sec = max_time_gap_sec,
      max_duplicate_frame_percent = max_duplicate_frame_percent,
      standardize = standardize
    )
  )

  class(out) <- c("gp3_face_quality_audit", class(out))

  out
}


#' Summarise external facial-behaviour data quality
#'
#' Returns the overview table from `audit_gazepoint_face_quality()`. The helper
#' accepts either an existing face-quality audit object or data that can be
#' audited directly.
#'
#' @param data A `gp3_face_quality_audit` object, a standardised face data frame,
#'   an unstandardised face data frame, or a CSV path.
#' @param ... Additional arguments passed to `audit_gazepoint_face_quality()`
#'   when `data` is not already an audit object.
#'
#' @return A tibble with class `gp3_face_quality_summary`.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   frame = 1:2,
#'   timestamp = c(0, 0.033),
#'   confidence = c(0.95, 0.90),
#'   success = c(1, 1)
#' )
#'
#' summarize_gazepoint_face_quality(face)
summarize_gazepoint_face_quality <- function(data, ...) {
  if (inherits(data, "gp3_face_quality_audit")) {
    out <- data$overview
  } else {
    out <- audit_gazepoint_face_quality(data, ...)$overview
  }

  class(out) <- c("gp3_face_quality_summary", class(out))

  out
}


#' @rdname summarize_gazepoint_face_quality
#' @export
summarise_gazepoint_face_quality <- summarize_gazepoint_face_quality


.gp3_face_quality_prepare_data <- function(data,
                                           confidence_threshold = 0.80,
                                           standardize = TRUE) {
  if (is.character(data) && length(data) == 1L && file.exists(data)) {
    return(
      standardize_gazepoint_face_columns(
        data,
        confidence_threshold = confidence_threshold
      )
    )
  }

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame, face-quality audit object, or readable CSV path.",
      call. = FALSE
    )
  }

  required <- c(
    "face_frame",
    "face_time_sec",
    "face_confidence",
    "face_success",
    "face_valid"
  )

  missing_required <- setdiff(required, names(data))

  if (standardize || length(missing_required) > 0L) {
    data <- standardize_gazepoint_face_columns(
      data,
      confidence_threshold = confidence_threshold
    )
  }

  as.data.frame(data, stringsAsFactors = FALSE)
}


.gp3_face_quality_group_indices <- function(data, group_cols = NULL) {
  if (is.null(group_cols) || length(group_cols) < 1L) {
    out <- list(overall = seq_len(nrow(data)))
    return(out)
  }

  group_data <- data[, group_cols, drop = FALSE]
  group_data[] <- lapply(group_data, function(x) {
    x <- as.character(x)
    x[is.na(x) | x == ""] <- "missing"
    x
  })

  group_id <- do.call(paste, c(group_data, sep = " | "))
  split(seq_len(nrow(data)), group_id)
}


.gp3_face_quality_summarise_subset <- function(data,
                                               idx,
                                               group_cols = NULL,
                                               min_valid_percent = 70,
                                               warning_valid_percent = 85,
                                               max_time_gap_sec = NULL,
                                               max_duplicate_frame_percent = 1) {
  n_rows <- length(idx)

  group_values <- .gp3_face_quality_group_values(data, idx, group_cols)

  face_valid <- data$face_valid[idx]
  face_confidence <- data$face_confidence[idx]
  face_success <- data$face_success[idx]
  face_frame <- data$face_frame[idx]
  face_time_sec <- data$face_time_sec[idx]

  n_valid <- sum(face_valid %in% TRUE, na.rm = TRUE)
  n_invalid <- sum(face_valid %in% FALSE, na.rm = TRUE)
  n_unknown_validity <- sum(is.na(face_valid))

  valid_percent <- .gp3_face_quality_percent(n_valid, n_rows)
  invalid_percent <- .gp3_face_quality_percent(n_invalid, n_rows)
  unknown_validity_percent <- .gp3_face_quality_percent(
    n_unknown_validity,
    n_rows
  )

  n_missing_confidence <- sum(is.na(face_confidence))
  confidence_missing_percent <- .gp3_face_quality_percent(
    n_missing_confidence,
    n_rows
  )

  known_success <- !is.na(face_success)
  n_success <- sum(face_success %in% TRUE, na.rm = TRUE)
  success_percent <- if (any(known_success)) {
    .gp3_face_quality_percent(n_success, sum(known_success))
  } else {
    NA_real_
  }

  frame_non_missing <- face_frame[!is.na(face_frame)]
  n_duplicate_frames <- if (length(frame_non_missing) > 0L) {
    sum(duplicated(frame_non_missing))
  } else {
    NA_integer_
  }

  duplicate_frame_percent <- if (length(frame_non_missing) > 0L) {
    .gp3_face_quality_percent(n_duplicate_frames, length(frame_non_missing))
  } else {
    NA_real_
  }

  time_finite <- face_time_sec[is.finite(face_time_sec)]
  n_missing_time <- n_rows - length(time_finite)

  time_steps <- if (length(time_finite) > 1L) {
    diff(sort(time_finite))
  } else {
    numeric(0)
  }

  n_nonpositive_time_steps <- sum(time_steps <= 0)
  positive_steps <- time_steps[time_steps > 0]

  max_time_gap_sec_observed <- if (length(positive_steps) > 0L) {
    max(positive_steps)
  } else {
    NA_real_
  }

  median_time_step_sec <- if (length(positive_steps) > 0L) {
    stats::median(positive_steps)
  } else {
    NA_real_
  }

  estimated_sampling_rate_hz <- if (
    is.finite(median_time_step_sec) &&
    !is.na(median_time_step_sec) &&
    median_time_step_sec > 0
  ) {
    1 / median_time_step_sec
  } else {
    NA_real_
  }

  face_quality_status <- .gp3_face_quality_status(
    n_rows = n_rows,
    face_valid = face_valid,
    valid_percent = valid_percent,
    duplicate_frame_percent = duplicate_frame_percent,
    max_time_gap_sec_observed = max_time_gap_sec_observed,
    min_valid_percent = min_valid_percent,
    warning_valid_percent = warning_valid_percent,
    max_time_gap_sec = max_time_gap_sec,
    max_duplicate_frame_percent = max_duplicate_frame_percent
  )

  message <- .gp3_face_quality_message(
    face_quality_status = face_quality_status,
    face_valid = face_valid,
    valid_percent = valid_percent,
    min_valid_percent = min_valid_percent,
    warning_valid_percent = warning_valid_percent
  )

  metrics <- data.frame(
    n_rows = n_rows,
    n_valid = n_valid,
    valid_percent = valid_percent,
    n_invalid = n_invalid,
    invalid_percent = invalid_percent,
    n_unknown_validity = n_unknown_validity,
    unknown_validity_percent = unknown_validity_percent,
    n_missing_confidence = n_missing_confidence,
    confidence_missing_percent = confidence_missing_percent,
    mean_confidence = .gp3_face_quality_mean(face_confidence),
    median_confidence = .gp3_face_quality_median(face_confidence),
    min_confidence = .gp3_face_quality_min(face_confidence),
    max_confidence = .gp3_face_quality_max(face_confidence),
    n_success = n_success,
    success_percent = success_percent,
    n_duplicate_frames = n_duplicate_frames,
    duplicate_frame_percent = duplicate_frame_percent,
    n_missing_time = n_missing_time,
    n_nonpositive_time_steps = n_nonpositive_time_steps,
    max_time_gap_sec = max_time_gap_sec_observed,
    median_time_step_sec = median_time_step_sec,
    estimated_sampling_rate_hz = estimated_sampling_rate_hz,
    face_quality_status = face_quality_status,
    message = message,
    stringsAsFactors = FALSE
  )

  cbind(group_values, metrics, stringsAsFactors = FALSE)
}


.gp3_face_quality_group_values <- function(data, idx, group_cols = NULL) {
  if (is.null(group_cols) || length(group_cols) < 1L) {
    return(
      data.frame(
        face_quality_group = "overall",
        stringsAsFactors = FALSE
      )
    )
  }

  vals <- data[idx[[1L]], group_cols, drop = FALSE]
  vals[] <- lapply(vals, as.character)
  vals[is.na(vals)] <- "missing"

  group_label <- paste(
    paste0(names(vals), "=", unlist(vals, use.names = FALSE)),
    collapse = " | "
  )

  cbind(
    data.frame(
      face_quality_group = group_label,
      stringsAsFactors = FALSE
    ),
    vals,
    stringsAsFactors = FALSE
  )
}


.gp3_face_quality_status <- function(n_rows,
                                     face_valid,
                                     valid_percent,
                                     duplicate_frame_percent,
                                     max_time_gap_sec_observed,
                                     min_valid_percent = 70,
                                     warning_valid_percent = 85,
                                     max_time_gap_sec = NULL,
                                     max_duplicate_frame_percent = 1) {
  if (n_rows < 1L) {
    return("fail")
  }

  if (all(is.na(face_valid))) {
    return("unknown")
  }

  if (!is.na(valid_percent) && valid_percent < min_valid_percent) {
    return("fail")
  }

  if (!is.na(valid_percent) && valid_percent < warning_valid_percent) {
    return("warn")
  }

  if (
    !is.na(duplicate_frame_percent) &&
    duplicate_frame_percent > max_duplicate_frame_percent
  ) {
    return("warn")
  }

  if (
    !is.null(max_time_gap_sec) &&
    !is.na(max_time_gap_sec_observed) &&
    max_time_gap_sec_observed > max_time_gap_sec
  ) {
    return("warn")
  }

  "pass"
}


.gp3_face_quality_message <- function(face_quality_status,
                                      face_valid,
                                      valid_percent,
                                      min_valid_percent = 70,
                                      warning_valid_percent = 85) {
  if (identical(face_quality_status, "unknown")) {
    return(
      "Face-data validity could not be evaluated because no confidence or success information was available."
    )
  }

  if (identical(face_quality_status, "fail")) {
    return(
      paste0(
        "Face-data validity is below the minimum threshold (",
        round(valid_percent, 1),
        "% valid; minimum ",
        min_valid_percent,
        "%)."
      )
    )
  }

  if (identical(face_quality_status, "warn")) {
    return(
      paste0(
        "Face-data quality should be reviewed before analysis (",
        round(valid_percent, 1),
        "% valid; warning threshold ",
        warning_valid_percent,
        "%)."
      )
    )
  }

  "Face-data quality passed the configured validity checks."
}


.gp3_face_quality_issue_summary <- function(group_summary,
                                            min_valid_percent = 70,
                                            warning_valid_percent = 85,
                                            max_time_gap_sec = NULL,
                                            max_duplicate_frame_percent = 1) {
  n_groups <- nrow(group_summary)

  out <- data.frame(
    issue = c(
      "valid_percent_below_minimum",
      "valid_percent_below_warning",
      "unknown_validity",
      "duplicate_frames",
      "large_time_gaps",
      "missing_confidence"
    ),
    n_groups_affected = c(
      sum(group_summary$valid_percent < min_valid_percent, na.rm = TRUE),
      sum(group_summary$valid_percent < warning_valid_percent, na.rm = TRUE),
      sum(group_summary$face_quality_status == "unknown", na.rm = TRUE),
      sum(
        group_summary$duplicate_frame_percent > max_duplicate_frame_percent,
        na.rm = TRUE
      ),
      if (!is.null(max_time_gap_sec)) {
        sum(group_summary$max_time_gap_sec > max_time_gap_sec, na.rm = TRUE)
      } else {
        NA_integer_
      },
      sum(group_summary$n_missing_confidence > 0, na.rm = TRUE)
    ),
    n_groups = n_groups,
    threshold = c(
      min_valid_percent,
      warning_valid_percent,
      NA_real_,
      max_duplicate_frame_percent,
      if (is.null(max_time_gap_sec)) NA_real_ else max_time_gap_sec,
      NA_real_
    ),
    stringsAsFactors = FALSE
  )

  out$status <- ifelse(
    is.na(out$n_groups_affected),
    "not_checked",
    ifelse(out$n_groups_affected > 0L, "review", "ok")
  )

  tibble::as_tibble(out)
}


.gp3_face_quality_percent <- function(x, n) {
  if (is.na(n) || n <= 0) {
    return(NA_real_)
  }

  100 * x / n
}


.gp3_face_quality_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}


.gp3_face_quality_median <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  stats::median(x, na.rm = TRUE)
}


.gp3_face_quality_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  min(x, na.rm = TRUE)
}


.gp3_face_quality_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}


.gp3_face_quality_bind_rows <- function(x) {
  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))

  x <- lapply(x, function(dat) {
    missing <- setdiff(all_names, names(dat))
    for (m in missing) {
      dat[[m]] <- NA
    }
    dat[, all_names, drop = FALSE]
  })

  do.call(rbind, x)
}

#' Plot external facial-behaviour data quality
#'
#' Creates descriptive quality-control plots from
#' `audit_gazepoint_face_quality()` output. The plots summarise face-data
#' validity, confidence, status, or timing gaps. They do not infer facial
#' expressions or emotional states.
#'
#' @param data A `gp3_face_quality_audit` object, a standardised face data frame,
#'   an unstandardised face data frame, or a CSV path.
#' @param plot_type One of `"status"`, `"validity"`, `"confidence"`, or
#'   `"time_gaps"`.
#' @param group_col Optional grouping column to use on the y-axis for
#'   group-level plots. Defaults to `face_quality_group`.
#' @param title Optional plot title.
#' @param ... Additional arguments passed to `audit_gazepoint_face_quality()`
#'   when `data` is not already an audit object.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' face <- data.frame(
#'   frame = 1:3,
#'   timestamp = c(0, 0.033, 0.066),
#'   confidence = c(0.95, 0.90, 0.40),
#'   success = c(1, 1, 1)
#' )
#'
#' plot_gazepoint_face_quality(face)
plot_gazepoint_face_quality <- function(data,
                                        plot_type = c(
                                          "status",
                                          "validity",
                                          "confidence",
                                          "time_gaps"
                                        ),
                                        group_col = NULL,
                                        title = NULL,
                                        ...) {
  plot_type <- match.arg(plot_type)

  audit <- if (inherits(data, "gp3_face_quality_audit")) {
    data
  } else {
    audit_gazepoint_face_quality(data, ...)
  }

  group_summary <- as.data.frame(audit$group_summary, stringsAsFactors = FALSE)

  if (nrow(group_summary) < 1L) {
    stop("The face-quality audit contains no group-summary rows.", call. = FALSE)
  }

  if (is.null(group_col)) {
    group_col <- "face_quality_group"
  }

  if (!group_col %in% names(group_summary)) {
    stop("`group_col` was not found in the face-quality summary.", call. = FALSE)
  }

  if (identical(plot_type, "status")) {
    plot_data <- as.data.frame(
      table(group_summary$face_quality_status),
      stringsAsFactors = FALSE
    )
    names(plot_data) <- c("face_quality_status", "n_groups")

    plot_data$.gp3_face_quality_status <- factor(
      plot_data$face_quality_status,
      levels = c("fail", "warn", "unknown", "pass")
    )
    plot_data$.gp3_face_quality_n_groups <- as.numeric(plot_data$n_groups)

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .gp3_face_quality_status,
          y = .gp3_face_quality_n_groups,
          fill = .gp3_face_quality_status
        )
      ) +
        ggplot2::geom_col(na.rm = TRUE) +
        ggplot2::labs(
          title = title,
          x = "Face-data quality status",
          y = "Number of groups",
          fill = "Status"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::guides(fill = "none")
    )
  }

  plot_data <- group_summary
  plot_data$.gp3_face_quality_group <- factor(
    plot_data[[group_col]],
    levels = rev(unique(plot_data[[group_col]]))
  )
  plot_data$.gp3_face_quality_status <- factor(
    plot_data$face_quality_status,
    levels = c("fail", "warn", "unknown", "pass")
  )

  if (identical(plot_type, "validity")) {
    plot_data$.gp3_face_quality_value <- as.numeric(plot_data$valid_percent)

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .gp3_face_quality_value,
          y = .gp3_face_quality_group,
          fill = .gp3_face_quality_status
        )
      ) +
        ggplot2::geom_col(na.rm = TRUE) +
        ggplot2::labs(
          title = title,
          x = "Valid rows (%)",
          y = "Group",
          fill = "Status"
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (identical(plot_type, "confidence")) {
    plot_data$.gp3_face_quality_value <- as.numeric(
      plot_data$median_confidence
    )

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .gp3_face_quality_value,
          y = .gp3_face_quality_group,
          fill = .gp3_face_quality_status
        )
      ) +
        ggplot2::geom_col(na.rm = TRUE) +
        ggplot2::labs(
          title = title,
          x = "Median face-detection confidence",
          y = "Group",
          fill = "Status"
        ) +
        ggplot2::theme_minimal()
    )
  }

  plot_data$.gp3_face_quality_value <- as.numeric(plot_data$max_time_gap_sec)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .gp3_face_quality_value,
      y = .gp3_face_quality_group,
      fill = .gp3_face_quality_status
    )
  ) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::labs(
      title = title,
      x = "Maximum time gap (seconds)",
      y = "Group",
      fill = "Status"
    ) +
    ggplot2::theme_minimal()
}
