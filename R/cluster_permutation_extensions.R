#' Prepare time-course data for Gazepoint cluster-permutation testing
#'
#' `prepare_gazepoint_timecourse_test_data()` converts a long-format
#' subject-by-condition-by-time data frame into the internal column contract used
#' by `run_gazepoint_cluster_permutation()`. It is intended for conservative
#' two-condition, within-subject, one-dimensional time-course workflows.
#'
#' @param data A long-format data frame.
#' @param subject_col Column identifying participants or paired units.
#' @param condition_col Column identifying the two within-subject conditions.
#' @param time_col Numeric time or time-bin column.
#' @param outcome_col Numeric outcome column.
#' @param condition_order Optional character vector of length two giving the
#'   condition order used for contrasts.
#' @param aggregate_fun Function used when duplicate subject-condition-time rows
#'   are present. Defaults to `mean`.
#' @param complete_only If `TRUE`, keep only subject-by-time cells with both
#'   conditions present.
#'
#' @return A data frame with internal cluster columns:
#'   `.gp3_cluster_subject`, `.gp3_cluster_condition`,
#'   `.gp3_cluster_time_bin`, and `.gp3_cluster_outcome`.
#' @export
#'
#' @examples
#' d <- data.frame(
#'   subject = rep(1:4, each = 6),
#'   condition = rep(rep(c("A", "B"), each = 3), 4),
#'   time = rep(1:3, 8),
#'   value = rnorm(24)
#' )
#'
#' prepare_gazepoint_timecourse_test_data(
#'   d,
#'   subject_col = "subject",
#'   condition_col = "condition",
#'   time_col = "time",
#'   outcome_col = "value"
#' )
prepare_gazepoint_timecourse_test_data <- function(data,
                                                   subject_col,
                                                   condition_col,
                                                   time_col,
                                                   outcome_col,
                                                   condition_order = NULL,
                                                   aggregate_fun = mean,
                                                   complete_only = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  subject_col <- .gp3_cluster_ext_scalar_string(subject_col, "subject_col")
  condition_col <- .gp3_cluster_ext_scalar_string(condition_col, "condition_col")
  time_col <- .gp3_cluster_ext_scalar_string(time_col, "time_col")
  outcome_col <- .gp3_cluster_ext_scalar_string(outcome_col, "outcome_col")

  .gp3_cluster_ext_check_columns(
    data,
    c(subject_col, condition_col, time_col, outcome_col)
  )

  if (!is.function(aggregate_fun)) {
    stop("`aggregate_fun` must be a function.", call. = FALSE)
  }

  .gp3_cluster_ext_logical_scalar(complete_only, "complete_only")

  dat <- data.frame(
    .gp3_cluster_subject = as.character(data[[subject_col]]),
    .gp3_cluster_condition = as.character(data[[condition_col]]),
    .gp3_cluster_time_bin = suppressWarnings(as.numeric(data[[time_col]])),
    .gp3_cluster_outcome = suppressWarnings(as.numeric(data[[outcome_col]])),
    stringsAsFactors = FALSE
  )

  dat <- dat[
    !is.na(dat$.gp3_cluster_subject) &
      !is.na(dat$.gp3_cluster_condition) &
      is.finite(dat$.gp3_cluster_time_bin) &
      is.finite(dat$.gp3_cluster_outcome),
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop("No valid time-course rows remained after preparation.", call. = FALSE)
  }

  available_conditions <- unique(dat$.gp3_cluster_condition)

  if (is.null(condition_order)) {
    condition_order <- available_conditions
  } else {
    if (!is.character(condition_order) ||
        length(condition_order) != 2L ||
        any(is.na(condition_order)) ||
        any(!nzchar(condition_order))) {
      stop("`condition_order` must be NULL or a character vector of length two.", call. = FALSE)
    }
  }

  condition_order <- unique(condition_order)

  if (length(condition_order) != 2L) {
    stop(
      "Cluster-permutation preparation requires exactly two conditions. Found: ",
      paste(available_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  missing_conditions <- setdiff(condition_order, available_conditions)

  if (length(missing_conditions) > 0L) {
    stop(
      "Requested condition(s) not found in `data`: ",
      paste(missing_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- dat[
    dat$.gp3_cluster_condition %in% condition_order,
    ,
    drop = FALSE
  ]

  dat$.gp3_cluster_condition <- factor(
    dat$.gp3_cluster_condition,
    levels = condition_order
  )

  dat <- stats::aggregate(
    .gp3_cluster_outcome ~
      .gp3_cluster_subject +
      .gp3_cluster_condition +
      .gp3_cluster_time_bin,
    data = dat,
    FUN = function(x) aggregate_fun(x, na.rm = TRUE)
  )

  if (isTRUE(complete_only)) {
    cell_key <- paste(
      dat$.gp3_cluster_subject,
      dat$.gp3_cluster_time_bin,
      sep = "\r"
    )

    cell_n <- stats::ave(
      as.character(dat$.gp3_cluster_condition),
      cell_key,
      FUN = function(x) length(unique(x))
    )

    dat <- dat[cell_n == 2L, , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No complete paired subject-by-time cells remained after preparation.",
      call. = FALSE
    )
  }

  dat$.gp3_cluster_status <- "ok"

  dat <- dat[order(
    dat$.gp3_cluster_subject,
    dat$.gp3_cluster_time_bin,
    dat$.gp3_cluster_condition
  ), , drop = FALSE]

  rownames(dat) <- NULL
  class(dat) <- unique(c("gp3_timecourse_test_data", class(dat)))

  dat
}


#' Summarize Gazepoint time clusters
#'
#' `summarize_gazepoint_time_clusters()` provides a compact US-spelling summary
#' of cluster-permutation output from `run_gazepoint_cluster_permutation()`.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param alpha Significance threshold used for the descriptive
#'   `cluster_significant` flag.
#'
#' @return A data frame with one row per cluster. If no clusters are present, an
#'   empty data frame with the expected columns is returned.
#' @export
#'
#' @examples
#' # See run_gazepoint_cluster_permutation() for the inferential workflow.
summarize_gazepoint_time_clusters <- function(result, alpha = 0.05) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  if (!"clusters" %in% names(result)) {
    stop("`result` must contain a `clusters` element.", call. = FALSE)
  }

  if (!is.numeric(alpha) ||
      length(alpha) != 1L ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1) {
    stop("`alpha` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  clusters <- as.data.frame(result$clusters, stringsAsFactors = FALSE)

  out_cols <- c(
    "cluster_id",
    "cluster_direction",
    "start_time_bin",
    "end_time_bin",
    "n_time_bins",
    "cluster_statistic",
    "p_value",
    "cluster_significant",
    "cluster_summary_status"
  )

  if (nrow(clusters) == 0L) {
    out <- data.frame(
      cluster_id = integer(0),
      cluster_direction = character(0),
      start_time_bin = numeric(0),
      end_time_bin = numeric(0),
      n_time_bins = integer(0),
      cluster_statistic = numeric(0),
      p_value = numeric(0),
      cluster_significant = logical(0),
      cluster_summary_status = character(0),
      stringsAsFactors = FALSE
    )
    return(out[, out_cols])
  }

  required <- c("cluster_id", "start_time_bin", "end_time_bin", "p_value")
  .gp3_cluster_ext_check_columns(clusters, required)

  get_col <- function(name, default) {
    if (name %in% names(clusters)) clusters[[name]] else default
  }

  start_time <- as.numeric(clusters$start_time_bin)
  end_time <- as.numeric(clusters$end_time_bin)

  out <- data.frame(
    cluster_id = clusters$cluster_id,
    cluster_direction = as.character(
      get_col("cluster_direction", rep(NA_character_, nrow(clusters)))
    ),
    start_time_bin = start_time,
    end_time_bin = end_time,
    n_time_bins = as.integer(end_time - start_time + 1),
    cluster_statistic = as.numeric(
      get_col("cluster_statistic", rep(NA_real_, nrow(clusters)))
    ),
    p_value = as.numeric(clusters$p_value),
    cluster_significant = as.numeric(clusters$p_value) < alpha,
    cluster_summary_status = "ok",
    stringsAsFactors = FALSE
  )

  out[, out_cols]
}


#' Plot a Gazepoint cluster-permutation result
#'
#' `plot_gazepoint_cluster_permutation()` is a compatibility wrapper around
#' `plot_gazepoint_cluster_results()`. It uses the existing validated gp3tools
#' plotting engine while providing a name aligned with the cluster-permutation
#' workflow.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param ... Additional arguments passed to
#'   `plot_gazepoint_cluster_results()`.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_cluster_permutation <- function(result, ...) {
  plot_gazepoint_cluster_results(result, ...)
}


.gp3_cluster_ext_scalar_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a non-empty character scalar.", call. = FALSE)
  }

  x
}


.gp3_cluster_ext_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(x)
}


.gp3_cluster_ext_check_columns <- function(data, cols) {
  cols <- cols[!is.na(cols) & nzchar(cols)]
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Audit a Gazepoint time-course grid for cluster-permutation readiness
#'
#' Check whether a prepared or raw long-format time-course data set has the
#' subject-by-condition-by-time structure expected by the conservative
#' two-condition cluster-permutation workflow.
#'
#' @param data A data frame.
#' @param subject_col Subject column. Defaults to the internal prepared column.
#' @param condition_col Condition column. Defaults to the internal prepared
#'   column.
#' @param time_col Time-bin column. Defaults to the internal prepared column.
#' @param outcome_col Outcome column. Defaults to the internal prepared column.
#'
#' @return A list containing grid counts, missing cells, duplicate cells, and
#'   readiness flags.
#' @export
audit_gazepoint_timecourse_grid <- function(data,
                                            subject_col = ".gp3_cluster_subject",
                                            condition_col = ".gp3_cluster_condition",
                                            time_col = ".gp3_cluster_time_bin",
                                            outcome_col = ".gp3_cluster_outcome") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  subject_col <- .gp3_cluster_ext_scalar_string(subject_col, "subject_col")
  condition_col <- .gp3_cluster_ext_scalar_string(condition_col, "condition_col")
  time_col <- .gp3_cluster_ext_scalar_string(time_col, "time_col")
  outcome_col <- .gp3_cluster_ext_scalar_string(outcome_col, "outcome_col")

  .gp3_cluster_ext_check_columns(data, c(subject_col, condition_col, time_col, outcome_col))

  d <- data.frame(
    subject = as.character(data[[subject_col]]),
    condition = as.character(data[[condition_col]]),
    time_bin = suppressWarnings(as.numeric(data[[time_col]])),
    outcome = suppressWarnings(as.numeric(data[[outcome_col]])),
    stringsAsFactors = FALSE
  )

  n_input <- nrow(d)

  d <- d[
    !is.na(d$subject) &
      !is.na(d$condition) &
      is.finite(d$time_bin) &
      is.finite(d$outcome),
    ,
    drop = FALSE
  ]

  subjects <- sort(unique(d$subject))
  conditions <- sort(unique(d$condition))
  time_bins <- sort(unique(d$time_bin))

  observed_key <- paste(d$subject, d$condition, d$time_bin, sep = "\r")
  duplicate_table <- sort(table(observed_key), decreasing = TRUE)
  duplicate_cells <- sum(duplicate_table > 1L)

  expected <- expand.grid(
    subject = subjects,
    condition = conditions,
    time_bin = time_bins,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  expected_key <- paste(expected$subject, expected$condition, expected$time_bin, sep = "\r")
  missing_key <- setdiff(expected_key, unique(observed_key))

  missing_cells <- expected[expected_key %in% missing_key, , drop = FALSE]
  rownames(missing_cells) <- NULL

  paired_key <- paste(d$subject, d$time_bin, sep = "\r")
  paired_n <- stats::aggregate(
    condition ~ paired_key,
    data = d,
    FUN = function(x) length(unique(x))
  )

  unpaired_cells <- sum(paired_n$condition < 2L)

  grid_summary <- data.frame(
    n_input_rows = n_input,
    n_valid_rows = nrow(d),
    n_subjects = length(subjects),
    n_conditions = length(conditions),
    n_time_bins = length(time_bins),
    n_expected_cells = nrow(expected),
    n_observed_cells = length(unique(observed_key)),
    n_missing_cells = nrow(missing_cells),
    n_duplicate_cells = duplicate_cells,
    n_unpaired_subject_time_cells = unpaired_cells,
    stringsAsFactors = FALSE
  )

  readiness <- data.frame(
    check = c(
      "exactly_two_conditions",
      "no_missing_grid_cells",
      "no_duplicate_cells",
      "no_unpaired_subject_time_cells",
      "numeric_outcome"
    ),
    passed = c(
      length(conditions) == 2L,
      nrow(missing_cells) == 0L,
      duplicate_cells == 0L,
      unpaired_cells == 0L,
      is.numeric(d$outcome)
    ),
    stringsAsFactors = FALSE
  )

  out <- list(
    grid_summary = grid_summary,
    readiness = readiness,
    missing_cells = missing_cells,
    duplicate_cell_count = duplicate_table[duplicate_table > 1L],
    audit_status = if (all(readiness$passed)) "ready" else "review"
  )

  class(out) <- c("gp3_timecourse_grid_audit", "list")
  out
}


#' Diagnose the design assumptions of a Gazepoint cluster-permutation workflow
#'
#' Provide a compact diagnostic summary for the conservative two-condition,
#' within-subject, one-dimensional cluster-permutation workflow.
#'
#' @param data A prepared or raw long-format time-course data frame.
#' @param subject_col Subject column.
#' @param condition_col Condition column.
#' @param time_col Time-bin column.
#' @param outcome_col Outcome column.
#'
#' @return A data frame of design checks and cautious interpretations.
#' @export
diagnose_gazepoint_cluster_design <- function(data,
                                              subject_col = ".gp3_cluster_subject",
                                              condition_col = ".gp3_cluster_condition",
                                              time_col = ".gp3_cluster_time_bin",
                                              outcome_col = ".gp3_cluster_outcome") {
  audit <- audit_gazepoint_timecourse_grid(
    data,
    subject_col = subject_col,
    condition_col = condition_col,
    time_col = time_col,
    outcome_col = outcome_col
  )

  gs <- audit$grid_summary

  out <- data.frame(
    diagnostic = c(
      "conditions",
      "paired_time_grid",
      "duplicates",
      "sample_size",
      "time_bins",
      "validated_scope"
    ),
    value = c(
      gs$n_conditions,
      gs$n_unpaired_subject_time_cells,
      gs$n_duplicate_cells,
      gs$n_subjects,
      gs$n_time_bins,
      "two-condition within-subject one-dimensional time course"
    ),
    passed = c(
      gs$n_conditions == 2L,
      gs$n_unpaired_subject_time_cells == 0L && gs$n_missing_cells == 0L,
      gs$n_duplicate_cells == 0L,
      gs$n_subjects >= 2L,
      gs$n_time_bins >= 2L,
      TRUE
    ),
    interpretation = c(
      "The implemented inferential engine is for exactly two conditions.",
      "Each subject-time cell should have both conditions represented.",
      "Duplicate cells should be aggregated before permutation testing.",
      "Permutation stability depends on the number of paired units.",
      "Cluster formation requires an ordered one-dimensional time grid.",
      "ANOVA, mixed-model, TFCE, multidimensional, covariate-adjusted, and parallel engines are intentionally outside this validated scope."
    ),
    stringsAsFactors = FALSE
  )

  class(out) <- unique(c("gp3_cluster_design_diagnostic", class(out)))
  out
}


#' Plot the cluster-permutation null distribution
#'
#' Plot the null distribution of maximum cluster statistics from a
#' cluster-permutation result when available.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param observed_line Should observed cluster statistics be added when
#'   available?
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_cluster_null_distribution <- function(result,
                                                     observed_line = TRUE,
                                                     title = NULL) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  null_values <- .gp3_cluster_ext_find_numeric_vector(
    result,
    c("null_distribution", "null_statistics", "max_cluster_statistics", "permutation_distribution")
  )

  if (is.null(null_values)) {
    stop(
      "Could not identify a numeric null distribution in `result`.",
      call. = FALSE
    )
  }

  d <- data.frame(
    null_statistic = as.numeric(null_values),
    stringsAsFactors = FALSE
  )

  d <- d[is.finite(d$null_statistic), , drop = FALSE]

  if (nrow(d) == 0L) {
    stop("The null distribution contains no finite values.", call. = FALSE)
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(x = null_statistic)) +
    ggplot2::geom_histogram(bins = 30) +
    ggplot2::labs(
      title = title,
      x = "Permutation maximum cluster statistic",
      y = "Count"
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(observed_line) && "clusters" %in% names(result)) {
    clusters <- as.data.frame(result$clusters, stringsAsFactors = FALSE)

    if (nrow(clusters) > 0L && "cluster_statistic" %in% names(clusters)) {
      obs <- data.frame(
        observed_statistic = abs(as.numeric(clusters$cluster_statistic))
      )
      obs <- obs[is.finite(obs$observed_statistic), , drop = FALSE]

      if (nrow(obs) > 0L) {
        p <- p + ggplot2::geom_vline(
          data = obs,
          ggplot2::aes(xintercept = observed_statistic),
          linetype = "dashed"
        )
      }
    }
  }

  p
}


#' Report a Gazepoint cluster-permutation result
#'
#' Create a compact, cautious, text-ready report from a cluster-permutation
#' result. The report avoids exact onset/offset claims and describes detected
#' clusters as time ranges surviving the specified cluster procedure.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param alpha Significance threshold.
#'
#' @return A list with cluster table, settings, and report text.
#' @export
report_gazepoint_cluster_permutation <- function(result, alpha = 0.05) {
  clusters <- summarize_gazepoint_time_clusters(result, alpha = alpha)

  settings <- if ("settings" %in% names(result)) {
    result$settings
  } else {
    list()
  }

  significant <- clusters[clusters$cluster_significant, , drop = FALSE]

  if (nrow(clusters) == 0L) {
    text <- paste(
      "The cluster-permutation workflow did not identify any supra-threshold",
      "time clusters under the specified settings. This should be interpreted as",
      "absence of detected cluster-level evidence in this analysis, not as evidence",
      "for absence of any effect."
    )
  } else if (nrow(significant) == 0L) {
    text <- paste(
      "Supra-threshold time clusters were observed, but none reached the specified",
      "cluster-level alpha threshold. Time ranges should be treated as descriptive",
      "unless supported by the permutation-adjusted cluster result."
    )
  } else {
    ranges <- paste0(
      significant$start_time_bin,
      "-",
      significant$end_time_bin,
      " (p = ",
      format(significant$p_value, digits = 3),
      ")"
    )

    text <- paste(
      "The cluster-permutation workflow identified",
      nrow(significant),
      "cluster(s) below alpha =",
      alpha,
      "over the following time-bin range(s):",
      paste(ranges, collapse = "; "),
      ". These ranges should be reported as cluster-level time intervals, not as",
      "precise effect-onset or effect-offset estimates."
    )
  }

  out <- list(
    cluster_table = clusters,
    settings = settings,
    report_text = text,
    report_status = "ok"
  )

  class(out) <- c("gp3_cluster_permutation_report", "list")
  out
}


#' Run threshold-sensitivity checks for Gazepoint cluster permutation
#'
#' Re-run `run_gazepoint_cluster_permutation()` across a small set of
#' cluster-forming thresholds and summarize how many clusters are detected.
#'
#' @param data Prepared cluster-permutation data.
#' @param thresholds Numeric vector of cluster-forming thresholds.
#' @param ... Additional arguments passed to `run_gazepoint_cluster_permutation()`.
#'
#' @return A list containing threshold-level summaries and full result objects.
#' @export
run_gazepoint_cluster_threshold_sensitivity <- function(data,
                                                        thresholds = c(1.5, 2, 2.5),
                                                        ...) {
  if (!is.numeric(thresholds) ||
      length(thresholds) == 0L ||
      any(!is.finite(thresholds)) ||
      any(thresholds <= 0)) {
    stop("`thresholds` must be a positive numeric vector.", call. = FALSE)
  }

  results <- lapply(thresholds, function(threshold) {
    run_gazepoint_cluster_permutation(
      data,
      cluster_threshold = threshold,
      ...
    )
  })

  summaries <- lapply(seq_along(results), function(i) {
    clusters <- summarize_gazepoint_time_clusters(results[[i]])
    data.frame(
      cluster_threshold = thresholds[i],
      n_clusters = nrow(clusters),
      n_significant_clusters = sum(clusters$cluster_significant),
      min_p_value = if (nrow(clusters) == 0L) NA_real_ else min(clusters$p_value, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })

  out <- list(
    summary = do.call(rbind, summaries),
    results = results,
    sensitivity_status = "ok"
  )

  class(out) <- c("gp3_cluster_threshold_sensitivity", "list")
  out
}


#' Simulate simple Gazepoint cluster time-course data
#'
#' Generate synthetic two-condition within-subject time-course data for examples
#' and tests. The simulation is intentionally simple and should not be treated as
#' a realistic model of gaze, pupil, or biometric time-series data.
#'
#' @param n_subjects Number of subjects.
#' @param n_time_bins Number of time bins.
#' @param conditions Two condition labels.
#' @param effect_start First time bin with an injected treatment effect.
#' @param effect_end Last time bin with an injected treatment effect.
#' @param effect_size Added treatment effect inside the effect window.
#' @param subject_sd Standard deviation of subject random shifts.
#' @param noise_sd Standard deviation of observation noise.
#' @param seed Optional random seed.
#'
#' @return A long-format data frame.
#' @export
simulate_gazepoint_cluster_timecourse_data <- function(n_subjects = 20,
                                                       n_time_bins = 60,
                                                       conditions = c("control", "treatment"),
                                                       effect_start = 25,
                                                       effect_end = 40,
                                                       effect_size = 0.5,
                                                       subject_sd = 0.3,
                                                       noise_sd = 0.4,
                                                       seed = NULL) {
  if (!is.numeric(n_subjects) || length(n_subjects) != 1L || is.na(n_subjects) || n_subjects < 2L) {
    stop("`n_subjects` must be at least 2.", call. = FALSE)
  }

  if (!is.numeric(n_time_bins) || length(n_time_bins) != 1L || is.na(n_time_bins) || n_time_bins < 2L) {
    stop("`n_time_bins` must be at least 2.", call. = FALSE)
  }

  if (!is.character(conditions) ||
      length(conditions) != 2L ||
      any(is.na(conditions)) ||
      any(!nzchar(conditions))) {
    stop("`conditions` must be a character vector of length two.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n_subjects <- as.integer(n_subjects)
  n_time_bins <- as.integer(n_time_bins)

  subject_id <- sprintf("S%03d", seq_len(n_subjects))
  time_bins <- seq_len(n_time_bins)

  grid <- expand.grid(
    subject = subject_id,
    condition = conditions,
    time_bin = time_bins,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  subject_shift <- stats::rnorm(n_subjects, 0, subject_sd)
  names(subject_shift) <- subject_id

  in_window <- grid$time_bin >= effect_start & grid$time_bin <= effect_end
  treatment <- grid$condition == conditions[2L]

  baseline_curve <- 0.15 * sin(grid$time_bin / max(grid$time_bin) * 2 * pi)

  grid$outcome <- baseline_curve +
    subject_shift[grid$subject] +
    ifelse(treatment & in_window, effect_size, 0) +
    stats::rnorm(nrow(grid), 0, noise_sd)

  rownames(grid) <- NULL
  class(grid) <- unique(c("gp3_cluster_simulated_timecourse", class(grid)))
  grid
}


#' Export Gazepoint cluster-permutation results
#'
#' Export cluster tables, optional null distributions, settings, and cautious
#' report text to a folder of CSV/TXT files.
#'
#' @param result A result object returned by
#'   `run_gazepoint_cluster_permutation()`.
#' @param outdir Output directory.
#' @param overwrite Should an existing directory be reused?
#'
#' @return A data frame listing written files.
#' @export
export_gazepoint_cluster_results <- function(result,
                                             outdir,
                                             overwrite = FALSE) {
  if (!is.list(result)) {
    stop("`result` must be a cluster-permutation result object.", call. = FALSE)
  }

  outdir <- .gp3_cluster_ext_scalar_string(outdir, "outdir")
  .gp3_cluster_ext_logical_scalar(overwrite, "overwrite")

  if (dir.exists(outdir) && !isTRUE(overwrite)) {
    stop("`outdir` already exists. Use `overwrite = TRUE` to reuse it.", call. = FALSE)
  }

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  written <- list()
  k <- 1L

  clusters <- summarize_gazepoint_time_clusters(result)
  cluster_path <- file.path(outdir, "cluster_summary.csv")
  utils::write.csv(clusters, cluster_path, row.names = FALSE)
  written[[k]] <- data.frame(file = cluster_path, file_type = "cluster_summary", stringsAsFactors = FALSE)
  k <- k + 1L

  null_values <- .gp3_cluster_ext_find_numeric_vector(
    result,
    c("null_distribution", "null_statistics", "max_cluster_statistics", "permutation_distribution")
  )

  if (!is.null(null_values)) {
    null_path <- file.path(outdir, "null_distribution.csv")
    utils::write.csv(
      data.frame(null_statistic = as.numeric(null_values)),
      null_path,
      row.names = FALSE
    )
    written[[k]] <- data.frame(file = null_path, file_type = "null_distribution", stringsAsFactors = FALSE)
    k <- k + 1L
  }

  report <- report_gazepoint_cluster_permutation(result)
  report_path <- file.path(outdir, "cluster_report.txt")
  writeLines(report$report_text, report_path, useBytes = TRUE)
  written[[k]] <- data.frame(file = report_path, file_type = "report_text", stringsAsFactors = FALSE)
  k <- k + 1L

  if ("settings" %in% names(result)) {
    settings_path <- file.path(outdir, "cluster_settings.csv")
    settings <- as.data.frame(
      as.list(result$settings),
      stringsAsFactors = FALSE,
      optional = TRUE
    )
    utils::write.csv(settings, settings_path, row.names = FALSE)
    written[[k]] <- data.frame(file = settings_path, file_type = "settings", stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, written)
  rownames(out) <- NULL
  out
}


.gp3_cluster_ext_find_numeric_vector <- function(x, names_to_try) {
  if (!is.list(x)) {
    return(NULL)
  }

  for (nm in names_to_try) {
    if (nm %in% names(x) && is.numeric(x[[nm]])) {
      return(x[[nm]])
    }
  }

  for (item in x) {
    if (is.numeric(item) && is.vector(item)) {
      return(item)
    }
  }

  NULL
}
