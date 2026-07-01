
#' Audit split-half reliability of fixation or AOI metrics
#'
#' Computes odd-even or random split-half reliability for common fixation and
#' AOI-derived metrics. The audit returns the split-half correlation and the
#' Spearman-Brown corrected reliability estimate.
#'
#' @param data A fixation-level or AOI-event-level data frame.
#' @param subject_col Character scalar. Subject identifier column.
#' @param trial_col Character scalar. Trial identifier column.
#' @param metric Metric to audit. Supported values are `"fixation_count"`,
#'   `"mean_fixation_duration"`, `"total_fixation_duration"`,
#'   `"aoi_dwell_prop"`, `"transition_count"`, and `"entropy_score"`.
#' @param duration_col Optional duration column. Required for duration metrics.
#'   Optional for `"aoi_dwell_prop"`; if omitted, row proportions are used.
#' @param aoi_col Optional AOI column. Required for AOI, transition, and entropy
#'   metrics.
#' @param target_aoi Optional AOI label required when `metric = "aoi_dwell_prop"`.
#' @param time_col Optional time column used to order AOI sequences.
#' @param group_cols Optional grouping columns. Reliability is computed
#'   separately within each group.
#' @param min_trials Minimum number of trials per subject required for inclusion.
#' @param split_method `"odd_even"` or `"random"`.
#' @param seed Optional random seed used when `split_method = "random"`.
#' @param correlation_method Correlation method passed to [stats::cor()].
#'
#' @return A data frame containing split-half reliability diagnostics.
#' @export
#'
#' @examples
#' dat <- expand.grid(
#'   subject = paste0("S", 1:6),
#'   trial = paste0("T", 1:4),
#'   KEEP.OUT.ATTRS = FALSE
#' )
#' dat$duration <- rep(seq_len(6), each = 4) + rep(c(0, 0.1, 0, 0.1), 6)
#'
#' audit_gazepoint_fixation_reliability(
#'   dat,
#'   subject_col = "subject",
#'   trial_col = "trial",
#'   metric = "total_fixation_duration",
#'   duration_col = "duration"
#' )
audit_gazepoint_fixation_reliability <- function(
    data,
    subject_col,
    trial_col,
    metric = c(
      "fixation_count",
      "mean_fixation_duration",
      "total_fixation_duration",
      "aoi_dwell_prop",
      "transition_count",
      "entropy_score"
    ),
    duration_col = NULL,
    aoi_col = NULL,
    target_aoi = NULL,
    time_col = NULL,
    group_cols = NULL,
    min_trials = 4,
    split_method = c("odd_even", "random"),
    seed = NULL,
    correlation_method = c("pearson", "spearman")
) {
  .gp3_sequence_check_data(data)
  .gp3_sequence_check_scalar_string(subject_col, "subject_col")
  .gp3_sequence_check_scalar_string(trial_col, "trial_col")
  .gp3_sequence_check_scalar_string(duration_col, "duration_col", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(aoi_col, "aoi_col", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(target_aoi, "target_aoi", allow_null = TRUE)
  .gp3_sequence_check_scalar_string(time_col, "time_col", allow_null = TRUE)
  .gp3_sequence_check_character_vector(group_cols, "group_cols", allow_null = TRUE)

  metric <- match.arg(metric)
  split_method <- match.arg(split_method)
  correlation_method <- match.arg(correlation_method)

  if (!is.numeric(min_trials) || length(min_trials) != 1L ||
      is.na(min_trials) || !is.finite(min_trials) || min_trials < 2) {
    stop("`min_trials` must be a finite numeric scalar of at least 2.",
         call. = FALSE)
  }

  min_trials <- as.integer(min_trials)

  required_cols <- c(subject_col, trial_col, group_cols)

  if (metric %in% c("mean_fixation_duration", "total_fixation_duration")) {
    if (is.null(duration_col)) {
      stop("`duration_col` is required for duration reliability metrics.",
           call. = FALSE)
    }
    required_cols <- c(required_cols, duration_col)
  }

  if (metric %in% c("aoi_dwell_prop", "transition_count", "entropy_score")) {
    if (is.null(aoi_col)) {
      stop("`aoi_col` is required for AOI reliability metrics.",
           call. = FALSE)
    }
    required_cols <- c(required_cols, aoi_col, time_col)
  }

  if (metric == "aoi_dwell_prop" && is.null(target_aoi)) {
    stop("`target_aoi` is required when `metric = \"aoi_dwell_prop\"`.",
         call. = FALSE)
  }

  if (metric == "aoi_dwell_prop" && !is.null(duration_col)) {
    required_cols <- c(required_cols, duration_col)
  }

  .gp3_sequence_check_columns(data, required_cols)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  trial_cols <- c(subject_col, trial_col, group_cols)
  trial_groups <- .gp3_sequence_split_groups(data, trial_cols)

  trial_rows <- lapply(trial_groups, function(dat) {
    group_values <- .gp3_sequence_group_values(dat, group_cols)

    metric_value <- switch(
      metric,
      fixation_count = nrow(dat),
      mean_fixation_duration = {
        x <- suppressWarnings(as.numeric(dat[[duration_col]]))
        x <- x[is.finite(x)]
        if (length(x)) mean(x) else NA_real_
      },
      total_fixation_duration = {
        x <- suppressWarnings(as.numeric(dat[[duration_col]]))
        x <- x[is.finite(x)]
        if (length(x)) sum(x) else NA_real_
      },
      aoi_dwell_prop = {
        aoi <- as.character(dat[[aoi_col]])
        valid_aoi <- !(is.na(aoi) | !nzchar(trimws(aoi)))

        if (!any(valid_aoi)) {
          NA_real_
        } else if (!is.null(duration_col)) {
          duration <- suppressWarnings(as.numeric(dat[[duration_col]]))
          valid <- valid_aoi & is.finite(duration) & duration >= 0

          if (!any(valid) || sum(duration[valid]) == 0) {
            NA_real_
          } else {
            sum(duration[valid & aoi == target_aoi]) / sum(duration[valid])
          }
        } else {
          mean(aoi[valid_aoi] == target_aoi)
        }
      },
      transition_count = {
        dat <- .gp3_sequence_order_data(dat, time_col)
        aoi <- .gp3_sequence_prepare_aoi(dat[[aoi_col]])
        aoi <- .gp3_sequence_collapse_repeats(aoi)
        max(length(aoi) - 1L, 0L)
      },
      entropy_score = {
        dat <- .gp3_sequence_order_data(dat, time_col)
        aoi <- .gp3_sequence_prepare_aoi(dat[[aoi_col]])

        if (!length(aoi)) {
          NA_real_
        } else {
          entropy <- .gp3_sequence_entropy_value(table(aoi), log_base = 2)
          .gp3_sequence_normalized_entropy(entropy, length(unique(aoi)), log_base = 2)
        }
      }
    )

    data.frame(
      group_values,
      .gp3_subject = as.character(dat[[subject_col]][[1L]]),
      .gp3_trial = as.character(dat[[trial_col]][[1L]]),
      .gp3_metric_value = as.numeric(metric_value),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  trial_summary <- .gp3_sequence_bind_rows(trial_rows)

  if (!nrow(trial_summary)) {
    return(data.frame(
      metric = metric,
      split_method = split_method,
      correlation_method = correlation_method,
      split_half_r = NA_real_,
      spearman_brown = NA_real_,
      n_subjects_total = 0L,
      n_subjects_used = 0L,
      n_trials = 0L,
      min_trials = min_trials,
      reliability_status = "no_trials",
      reliability_warning = "No trial-level metrics could be computed.",
      stringsAsFactors = FALSE
    ))
  }

  split_groups <- .gp3_sequence_split_groups(trial_summary, c(group_cols, ".gp3_subject"))

  split_rows <- lapply(split_groups, function(dat) {
    if (split_method == "random") {
      ord <- sample(seq_len(nrow(dat)))
    } else {
      ord <- order(dat$.gp3_trial)
    }

    dat <- dat[ord, , drop = FALSE]
    dat$.gp3_half <- rep(c("odd", "even"), length.out = nrow(dat))
    dat
  })

  trial_summary <- .gp3_sequence_bind_rows(split_rows)

  reliability_groups <- .gp3_sequence_split_groups(trial_summary, group_cols)

  reliability_rows <- lapply(reliability_groups, function(dat) {
    group_values <- .gp3_sequence_group_values(dat, group_cols)
    subject_groups <- split(dat, dat$.gp3_subject, drop = TRUE)

    subject_rows <- lapply(subject_groups, function(subdat) {
      odd <- mean(subdat$.gp3_metric_value[subdat$.gp3_half == "odd"], na.rm = TRUE)
      even <- mean(subdat$.gp3_metric_value[subdat$.gp3_half == "even"], na.rm = TRUE)

      if (!is.finite(odd)) odd <- NA_real_
      if (!is.finite(even)) even <- NA_real_

      data.frame(
        .gp3_subject = subdat$.gp3_subject[[1L]],
        odd = odd,
        even = even,
        n_trials = nrow(subdat),
        stringsAsFactors = FALSE
      )
    })

    subject_summary <- .gp3_sequence_bind_rows(subject_rows)
    eligible <- subject_summary[
      subject_summary$n_trials >= min_trials &
        is.finite(subject_summary$odd) &
        is.finite(subject_summary$even),
      ,
      drop = FALSE
    ]

    n_subjects_total <- nrow(subject_summary)
    n_subjects_used <- nrow(eligible)
    n_trials <- sum(eligible$n_trials)

    if (n_subjects_used < 3L) {
      split_half_r <- NA_real_
      spearman_brown <- NA_real_
      status <- "too_few_subjects"
      warning <- "Fewer than three subjects had enough complete split-half data."
    } else if (stats::sd(eligible$odd) == 0 || stats::sd(eligible$even) == 0) {
      split_half_r <- NA_real_
      spearman_brown <- NA_real_
      status <- "no_variance"
      warning <- "At least one split had zero between-subject variance."
    } else {
      split_half_r <- stats::cor(
        eligible$odd,
        eligible$even,
        method = correlation_method,
        use = "complete.obs"
      )

      spearman_brown <- if (is.finite(split_half_r) && split_half_r != -1) {
        (2 * split_half_r) / (1 + split_half_r)
      } else {
        NA_real_
      }

      status <- "ok"
      warning <- ""
    }

    metrics <- data.frame(
      metric = metric,
      split_method = split_method,
      correlation_method = correlation_method,
      split_half_r = split_half_r,
      spearman_brown = spearman_brown,
      n_subjects_total = n_subjects_total,
      n_subjects_used = n_subjects_used,
      n_trials = n_trials,
      min_trials = min_trials,
      reliability_status = status,
      reliability_warning = warning,
      stringsAsFactors = FALSE
    )

    cbind(group_values, metrics)
  })

  .gp3_sequence_bind_rows(reliability_rows)
}
