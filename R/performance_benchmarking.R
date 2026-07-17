#' Default performance limits for gp3tools export workflows
#'
#' Returns conservative, explicit limits for elapsed time, approximate R-heap
#' growth, and scaling behaviour. The limits are intended as regression gates,
#' not as hardware-independent claims about absolute package speed.
#'
#' @return A data frame with one row per benchmarked operation.
#' @export
gp3tools_performance_limits <- function() {
  data.frame(
    operation = c("generate", "import", "master", "sampling", "quality"),
    max_seconds_per_million_rows = c(90, 240, 240, 180, 180),
    max_heap_delta_mb_per_million_rows = c(1200, 1800, 1800, 1200, 1200),
    max_scaling_exponent = c(1.60, 1.60, 1.60, 1.60, 1.60),
    stringsAsFactors = FALSE
  )
}

#' Benchmark gp3tools on increasingly large Gazepoint exports
#'
#' Generates deterministic Gazepoint-like all-gaze exports and benchmarks
#' selected package operations across increasing row and file counts. Ordinary
#' unit tests should use small scales. The script installed under
#' `inst/benchmarks/` runs the large-export profile.
#'
#' @param scales Data frame with integer `total_rows` and `n_files` columns.
#' @param operations Any of `"generate"`, `"import"`, `"master"`,
#'   `"sampling"`, and `"quality"`.
#' @param trials Number of repetitions per scale.
#' @param seed Integer random seed.
#' @param limits Performance limits returned by
#'   [gp3tools_performance_limits()] or a compatible data frame.
#' @param stop_on_regression Stop when a completed benchmark exceeds a limit.
#' @param output_dir Optional directory used for generated exports.
#' @param keep_exports Retain generated CSV exports.
#' @param on_error Whether operation errors are recorded or stop the benchmark.
#'
#' @return A `"gazepoint_performance_benchmark"` object containing trial-level
#'   measurements, aggregated summaries, regression checks, settings, and
#'   session metadata.
#' @export
benchmark_gazepoint_export_performance <- function(
    scales = data.frame(
      total_rows = c(10000L, 50000L, 200000L),
      n_files = c(1L, 4L, 8L)
    ),
    operations = c("generate", "import", "master", "sampling", "quality"),
    trials = 3L,
    seed = 20260717L,
    limits = gp3tools_performance_limits(),
    stop_on_regression = FALSE,
    output_dir = NULL,
    keep_exports = FALSE,
    on_error = c("record", "stop")) {
  on_error <- match.arg(on_error)
  .gp3_perf_validate_scales(scales)
  operations <- unique(as.character(operations))
  allowed <- c("generate", "import", "master", "sampling", "quality")
  unsupported <- setdiff(operations, allowed)
  if (length(unsupported) > 0L) {
    stop(
      "Unsupported operations: ",
      paste(unsupported, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  trials <- .gp3_perf_positive_integer(trials, "trials")
  seed <- .gp3_perf_integer(seed, "seed")
  .gp3_perf_logical(stop_on_regression, "stop_on_regression")
  .gp3_perf_logical(keep_exports, "keep_exports")
  .gp3_perf_validate_limits(limits)

  if (is.null(output_dir)) {
    output_dir <- tempfile("gp3tools-performance-")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  if (!isTRUE(keep_exports)) {
    on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)
  }

  measurements <- list()
  measurement_index <- 0L

  for (scale_index in seq_len(nrow(scales))) {
    total_rows <- as.integer(scales$total_rows[scale_index])
    n_files <- as.integer(scales$n_files[scale_index])
    rows_per_file <- ceiling(total_rows / n_files)

    for (trial in seq_len(trials)) {
      trial_seed <- seed + 10000L * scale_index + trial
      scale_dir <- file.path(
        output_dir,
        sprintf("scale_%02d_trial_%02d", scale_index, trial)
      )
      dir.create(scale_dir, recursive = TRUE, showWarnings = FALSE)

      generated <- .gp3_perf_measure(
        function() {
          .gp3_perf_simulate_export(
            total_rows = total_rows,
            n_files = n_files,
            seed = trial_seed
          )
        }
      )

      if ("generate" %in% operations) {
        measurement_index <- measurement_index + 1L
        measurements[[measurement_index]] <- .gp3_perf_measurement_row(
          scale_index,
          total_rows,
          n_files,
          rows_per_file,
          trial,
          "generate",
          generated
        )
      }

      if (!identical(generated$status, "ok")) {
        if (identical(on_error, "stop")) {
          stop(generated$error_message, call. = FALSE)
        }
        next
      }

      export_data <- generated$value
      .gp3_perf_write_exports(export_data, scale_dir)

      imported_data <- export_data

      if ("import" %in% operations) {
        imported <- .gp3_perf_measure(
          function() {
            read_gazepoint_folder(scale_dir)
          }
        )
        measurement_index <- measurement_index + 1L
        measurements[[measurement_index]] <- .gp3_perf_measurement_row(
          scale_index,
          total_rows,
          n_files,
          rows_per_file,
          trial,
          "import",
          imported
        )
        if (identical(imported$status, "ok")) {
          imported_data <- imported$value
        } else if (identical(on_error, "stop")) {
          stop(imported$error_message, call. = FALSE)
        }
      }

      if ("master" %in% operations) {
        master_measure <- .gp3_perf_measure(
          function() create_gazepoint_master(imported_data)
        )
        measurement_index <- measurement_index + 1L
        measurements[[measurement_index]] <- .gp3_perf_measurement_row(
          scale_index,
          total_rows,
          n_files,
          rows_per_file,
          trial,
          "master",
          master_measure
        )
        if (!identical(master_measure$status, "ok") &&
            identical(on_error, "stop")) {
          stop(master_measure$error_message, call. = FALSE)
        }
      }

      if ("sampling" %in% operations) {
        sampling_measure <- .gp3_perf_measure(
          function() {
            check_sampling_rate(imported_data)
          }
        )
        measurement_index <- measurement_index + 1L
        measurements[[measurement_index]] <- .gp3_perf_measurement_row(
          scale_index,
          total_rows,
          n_files,
          rows_per_file,
          trial,
          "sampling",
          sampling_measure
        )
        if (!identical(sampling_measure$status, "ok") &&
            identical(on_error, "stop")) {
          stop(sampling_measure$error_message, call. = FALSE)
        }
      }

      if ("quality" %in% operations) {
        quality_measure <- .gp3_perf_measure(
          function() {
            summarise_tracking_quality(imported_data)
          }
        )
        measurement_index <- measurement_index + 1L
        measurements[[measurement_index]] <- .gp3_perf_measurement_row(
          scale_index,
          total_rows,
          n_files,
          rows_per_file,
          trial,
          "quality",
          quality_measure
        )
        if (!identical(quality_measure$status, "ok") &&
            identical(on_error, "stop")) {
          stop(quality_measure$error_message, call. = FALSE)
        }
      }
    }
  }

  trial_results <- if (length(measurements) == 0L) {
    data.frame()
  } else {
    do.call(rbind, measurements)
  }
  rownames(trial_results) <- NULL

  summary <- .gp3_perf_summarise_trials(trial_results)
  regression <- check_gazepoint_performance_regression(
    summary,
    limits = limits
  )

  if (isTRUE(stop_on_regression) && !isTRUE(regression$overall$pass)) {
    failed <- regression$checks$check[
      regression$checks$status == "fail"
    ]
    stop(
      "Performance regression limits were exceeded: ",
      paste(unique(failed), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  structure(
    list(
      trials = trial_results,
      summary = summary,
      regression = regression,
      limits = limits,
      settings = list(
        scales = scales,
        operations = operations,
        trials = trials,
        seed = seed,
        output_dir = if (keep_exports) output_dir else NA_character_,
        keep_exports = keep_exports,
        on_error = on_error
      ),
      session = list(
        timestamp_utc = format(
          Sys.time(),
          tz = "UTC",
          usetz = TRUE
        ),
        r_version = R.version.string,
        platform = R.version$platform,
        gp3tools_version = as.character(
          utils::packageVersion("gp3tools")
        )
      )
    ),
    class = c("gazepoint_performance_benchmark", "list")
  )
}

#' Check gp3tools performance results against regression limits
#'
#' Applies explicit absolute limits and, optionally, compares matched rows with
#' a saved baseline. Scaling exponents are estimated from median elapsed times
#' across row-count levels.
#'
#' @param x A benchmark object or summary data frame.
#' @param limits Explicit operation-level limits.
#' @param baseline Optional prior benchmark object or summary data frame.
#' @param elapsed_ratio_limit Maximum allowed elapsed-time ratio to baseline.
#' @param memory_ratio_limit Maximum allowed heap-growth ratio to baseline.
#'
#' @return A `"gazepoint_performance_regression"` object.
#' @export
check_gazepoint_performance_regression <- function(
    x,
    limits = gp3tools_performance_limits(),
    baseline = NULL,
    elapsed_ratio_limit = 1.50,
    memory_ratio_limit = 1.50) {
  .gp3_perf_validate_limits(limits)
  .gp3_perf_positive_scalar(elapsed_ratio_limit, "elapsed_ratio_limit")
  .gp3_perf_positive_scalar(memory_ratio_limit, "memory_ratio_limit")

  current <- .gp3_perf_as_summary(x)
  required <- c(
    "operation",
    "total_rows",
    "n_files",
    "median_elapsed_s",
    "median_heap_delta_mb",
    "n_success",
    "n_trials"
  )
  missing <- setdiff(required, names(current))
  if (length(missing) > 0L) {
    stop(
      "`x` is missing required columns: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  current$seconds_per_million_rows <- current$median_elapsed_s /
    pmax(current$total_rows / 1e6, .Machine$double.eps)
  current$heap_mb_per_million_rows <- current$median_heap_delta_mb /
    pmax(current$total_rows / 1e6, .Machine$double.eps)

  current <- merge(
    current,
    limits,
    by = "operation",
    all.x = TRUE,
    sort = FALSE
  )

  scaling <- .gp3_perf_scaling_exponents(current)
  current <- merge(
    current,
    scaling,
    by = "operation",
    all.x = TRUE,
    sort = FALSE
  )

  current$elapsed_limit_pass <- is.na(
    current$max_seconds_per_million_rows
  ) | current$seconds_per_million_rows <=
    current$max_seconds_per_million_rows

  current$memory_limit_pass <- is.na(
    current$max_heap_delta_mb_per_million_rows
  ) | current$heap_mb_per_million_rows <=
    current$max_heap_delta_mb_per_million_rows

  current$scaling_limit_pass <- is.na(
    current$max_scaling_exponent
  ) | is.na(current$scaling_exponent) |
    current$scaling_exponent <= current$max_scaling_exponent

  current$operation_success <- current$n_success == current$n_trials

  if (!is.null(baseline)) {
    baseline_summary <- .gp3_perf_as_summary(baseline)
    baseline_keys <- c("operation", "total_rows", "n_files")
    baseline_keep <- c(
      baseline_keys,
      "median_elapsed_s",
      "median_heap_delta_mb"
    )
    baseline_summary <- baseline_summary[baseline_keep]
    names(baseline_summary)[
      names(baseline_summary) == "median_elapsed_s"
    ] <- "baseline_elapsed_s"
    names(baseline_summary)[
      names(baseline_summary) == "median_heap_delta_mb"
    ] <- "baseline_heap_delta_mb"

    current <- merge(
      current,
      baseline_summary,
      by = baseline_keys,
      all.x = TRUE,
      sort = FALSE
    )
    current$elapsed_ratio <- current$median_elapsed_s /
      current$baseline_elapsed_s
    current$memory_ratio <- current$median_heap_delta_mb /
      current$baseline_heap_delta_mb

    current$baseline_elapsed_pass <- is.na(current$elapsed_ratio) |
      current$elapsed_ratio <= elapsed_ratio_limit
    current$baseline_memory_pass <- is.na(current$memory_ratio) |
      current$memory_ratio <= memory_ratio_limit
  } else {
    current$baseline_elapsed_s <- NA_real_
    current$baseline_heap_delta_mb <- NA_real_
    current$elapsed_ratio <- NA_real_
    current$memory_ratio <- NA_real_
    current$baseline_elapsed_pass <- TRUE
    current$baseline_memory_pass <- TRUE
  }

  checks <- rbind(
    .gp3_perf_check_rows(
      current,
      "operation_completed",
      current$operation_success
    ),
    .gp3_perf_check_rows(
      current,
      "elapsed_absolute_limit",
      current$elapsed_limit_pass
    ),
    .gp3_perf_check_rows(
      current,
      "memory_absolute_limit",
      current$memory_limit_pass
    ),
    .gp3_perf_check_rows(
      current,
      "scaling_exponent_limit",
      current$scaling_limit_pass
    ),
    .gp3_perf_check_rows(
      current,
      "elapsed_baseline_ratio",
      current$baseline_elapsed_pass
    ),
    .gp3_perf_check_rows(
      current,
      "memory_baseline_ratio",
      current$baseline_memory_pass
    )
  )
  rownames(checks) <- NULL

  overall <- data.frame(
    pass = all(checks$status != "fail"),
    n_checks = nrow(checks),
    n_pass = sum(checks$status == "pass"),
    n_fail = sum(checks$status == "fail"),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      overall = overall,
      checks = checks,
      evaluated = current,
      limits = limits,
      baseline_used = !is.null(baseline),
      elapsed_ratio_limit = elapsed_ratio_limit,
      memory_ratio_limit = memory_ratio_limit
    ),
    class = c("gazepoint_performance_regression", "list")
  )
}

#' Write gp3tools performance benchmark tables
#'
#' @param x A `"gazepoint_performance_benchmark"` object.
#' @param output_dir Output directory.
#' @param prefix Filename prefix.
#'
#' @return Named character vector of written files.
#' @export
write_gazepoint_performance_benchmark <- function(
    x,
    output_dir,
    prefix = "gp3tools-performance") {
  if (!inherits(x, "gazepoint_performance_benchmark")) {
    stop(
      "`x` must be a gazepoint_performance_benchmark object.",
      call. = FALSE
    )
  }
  output_dir <- .gp3_perf_nonempty_string(output_dir, "output_dir")
  prefix <- .gp3_perf_nonempty_string(prefix, "prefix")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  files <- c(
    trials = file.path(output_dir, paste0(prefix, "-trials.csv")),
    summary = file.path(output_dir, paste0(prefix, "-summary.csv")),
    checks = file.path(output_dir, paste0(prefix, "-checks.csv")),
    evaluated = file.path(output_dir, paste0(prefix, "-evaluated.csv"))
  )

  utils::write.csv(x$trials, files[["trials"]], row.names = FALSE)
  utils::write.csv(x$summary, files[["summary"]], row.names = FALSE)
  utils::write.csv(
    x$regression$checks,
    files[["checks"]],
    row.names = FALSE
  )
  utils::write.csv(
    x$regression$evaluated,
    files[["evaluated"]],
    row.names = FALSE
  )

  normalized <- normalizePath(
    unname(files),
    winslash = "/",
    mustWork = TRUE
  )
  names(normalized) <- names(files)
  normalized
}

#' @export
print.gazepoint_performance_benchmark <- function(x, ...) {
  cat("gp3tools large-export performance benchmark\n")
  cat("  Measurements: ", nrow(x$trials), "\n", sep = "")
  cat("  Summary rows: ", nrow(x$summary), "\n", sep = "")
  cat(
    "  Regression gate: ",
    if (isTRUE(x$regression$overall$pass)) "pass" else "fail",
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.gazepoint_performance_regression <- function(x, ...) {
  cat("gp3tools performance regression audit\n")
  cat(
    "  Status: ",
    if (isTRUE(x$overall$pass)) "pass" else "fail",
    "\n",
    sep = ""
  )
  cat("  Checks: ", x$overall$n_checks, "\n", sep = "")
  cat("  Failed: ", x$overall$n_fail, "\n", sep = "")
  invisible(x)
}

.gp3_perf_simulate_export <- function(total_rows, n_files, seed) {
  set.seed(seed)
  file_ids <- rep(seq_len(n_files), length.out = total_rows)
  within_file <- stats::ave(
    seq_len(total_rows),
    file_ids,
    FUN = seq_along
  )
  participant <- sprintf("P%03d", file_ids)
  media <- sprintf("M%02d", ((file_ids - 1L) %% 4L) + 1L)
  time_s <- (within_file - 1L) / 60
  phase <- 2 * pi * time_s / 3

  gaze_x <- pmin(
    0.98,
    pmax(0.02, 0.50 + 0.22 * sin(phase) + stats::rnorm(total_rows, 0, 0.01))
  )
  gaze_y <- pmin(
    0.98,
    pmax(0.02, 0.50 + 0.18 * cos(phase) + stats::rnorm(total_rows, 0, 0.01))
  )
  valid <- stats::runif(total_rows) > 0.025
  pupil_left <- 3.2 + 0.12 * sin(phase / 2) +
    stats::rnorm(total_rows, 0, 0.025)
  pupil_right <- 3.18 + 0.12 * sin(phase / 2 + 0.03) +
    stats::rnorm(total_rows, 0, 0.025)
  pupil_left[!valid] <- NA_real_
  pupil_right[!valid] <- NA_real_

  data.frame(
    USER_ID = participant,
    USER_FILE = sprintf("User %03d_all_gaze.csv", file_ids),
    MEDIA_ID = media,
    MEDIA_NAME = paste0("Stimulus_", media),
    TRIAL_ID = paste(participant, media, sep = "_"),
    TIME = time_s,
    MSTIMER = round(time_s * 1000),
    FPOGX = gaze_x,
    FPOGY = gaze_y,
    FPOGS = pmax(0, time_s - 0.15),
    FPOGD = 0.15,
    FPOGV = as.integer(valid),
    BPOGX = gaze_x,
    BPOGY = gaze_y,
    BPOGV = as.integer(valid),
    LPOGX = pmin(1, pmax(0, gaze_x - 0.002)),
    LPOGY = pmin(1, pmax(0, gaze_y + 0.001)),
    LPOGV = as.integer(valid),
    RPOGX = pmin(1, pmax(0, gaze_x + 0.002)),
    RPOGY = pmin(1, pmax(0, gaze_y - 0.001)),
    RPOGV = as.integer(valid),
    LPD = pupil_left,
    RPD = pupil_right,
    LPV = as.integer(valid),
    RPV = as.integer(valid),
    AOI = ifelse(gaze_x < 0.5, "left", "right"),
    EVENT = ifelse(within_file == 1L, "trial_start", ""),
    stringsAsFactors = FALSE
  )
}

.gp3_perf_write_exports <- function(data, directory) {
  file_values <- unique(data$USER_FILE)
  for (file_value in file_values) {
    file_data <- data[data$USER_FILE == file_value, , drop = FALSE]
    file_data$USER_FILE <- NULL
    utils::write.csv(
      file_data,
      file.path(directory, file_value),
      row.names = FALSE,
      na = ""
    )
  }
  invisible(directory)
}

.gp3_perf_measure <- function(fun) {
  before <- gc(reset = TRUE)
  baseline_mb <- .gp3_perf_gc_mb(before[, "used"])
  start <- proc.time()[["elapsed"]]

  result <- tryCatch(
    {
      value <- fun()
      list(status = "ok", value = value, error_message = NA_character_)
    },
    error = function(error) {
      list(
        status = "error",
        value = NULL,
        error_message = conditionMessage(error)
      )
    }
  )

  elapsed <- proc.time()[["elapsed"]] - start
  after <- gc()
  peak_mb <- .gp3_perf_gc_mb(after[, "max used"])
  heap_delta_mb <- max(0, peak_mb - baseline_mb)
  output_size_mb <- if (identical(result$status, "ok")) {
    as.numeric(utils::object.size(result$value)) / 1024^2
  } else {
    NA_real_
  }

  c(
    result,
    list(
      elapsed_s = unname(elapsed),
      heap_delta_mb = heap_delta_mb,
      output_size_mb = output_size_mb
    )
  )
}

.gp3_perf_gc_mb <- function(cells) {
  ncell <- unname(cells[["Ncells"]])
  vcell <- unname(cells[["Vcells"]])
  (ncell * 56 + vcell * 8) / 1024^2
}

.gp3_perf_measurement_row <- function(
    scale_id,
    total_rows,
    n_files,
    rows_per_file,
    trial,
    operation,
    measurement) {
  data.frame(
    scale_id = scale_id,
    total_rows = total_rows,
    n_files = n_files,
    rows_per_file = rows_per_file,
    trial = trial,
    operation = operation,
    status = measurement$status,
    elapsed_s = measurement$elapsed_s,
    heap_delta_mb = measurement$heap_delta_mb,
    output_size_mb = measurement$output_size_mb,
    error_message = measurement$error_message,
    stringsAsFactors = FALSE
  )
}

.gp3_perf_summarise_trials <- function(results) {
  if (nrow(results) == 0L) {
    return(data.frame())
  }
  key <- interaction(
    results$scale_id,
    results$total_rows,
    results$n_files,
    results$rows_per_file,
    results$operation,
    drop = TRUE,
    lex.order = TRUE
  )
  rows <- lapply(split(seq_len(nrow(results)), key), function(index) {
    part <- results[index, , drop = FALSE]
    ok <- part$status == "ok"
    data.frame(
      scale_id = part$scale_id[1L],
      total_rows = part$total_rows[1L],
      n_files = part$n_files[1L],
      rows_per_file = part$rows_per_file[1L],
      operation = part$operation[1L],
      n_trials = nrow(part),
      n_success = sum(ok),
      median_elapsed_s = if (any(ok)) {
        stats::median(part$elapsed_s[ok])
      } else {
        NA_real_
      },
      minimum_elapsed_s = if (any(ok)) min(part$elapsed_s[ok]) else NA_real_,
      maximum_elapsed_s = if (any(ok)) max(part$elapsed_s[ok]) else NA_real_,
      median_heap_delta_mb = if (any(ok)) {
        stats::median(part$heap_delta_mb[ok])
      } else {
        NA_real_
      },
      maximum_heap_delta_mb = if (any(ok)) {
        max(part$heap_delta_mb[ok])
      } else {
        NA_real_
      },
      median_output_size_mb = if (any(ok)) {
        stats::median(part$output_size_mb[ok], na.rm = TRUE)
      } else {
        NA_real_
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$operation, out$total_rows, out$n_files), , drop = FALSE]
}

.gp3_perf_as_summary <- function(x) {
  if (inherits(x, "gazepoint_performance_benchmark")) {
    return(x$summary)
  }
  if (!is.data.frame(x)) {
    stop(
      "`x` must be a performance benchmark or data frame.",
      call. = FALSE
    )
  }
  if (all(c("trial", "elapsed_s", "status") %in% names(x))) {
    return(.gp3_perf_summarise_trials(x))
  }
  x
}

.gp3_perf_scaling_exponents <- function(summary) {
  operations <- unique(summary$operation)
  rows <- lapply(operations, function(operation) {
    part <- summary[
      summary$operation == operation &
        is.finite(summary$median_elapsed_s) &
        summary$median_elapsed_s > 0 &
        summary$total_rows > 0,
      ,
      drop = FALSE
    ]
    exponent <- NA_real_
    if (length(unique(part$total_rows)) >= 2L) {
      model <- stats::lm(
        log(median_elapsed_s) ~ log(total_rows),
        data = part
      )
      exponent <- unname(stats::coef(model)[[2L]])
    }
    data.frame(
      operation = operation,
      scaling_exponent = exponent,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.gp3_perf_check_rows <- function(current, check, pass) {
  data.frame(
    operation = current$operation,
    total_rows = current$total_rows,
    n_files = current$n_files,
    check = check,
    status = ifelse(pass, "pass", "fail"),
    stringsAsFactors = FALSE
  )
}

.gp3_perf_validate_scales <- function(scales) {
  if (!is.data.frame(scales)) {
    stop("`scales` must be a data frame.", call. = FALSE)
  }
  required <- c("total_rows", "n_files")
  missing <- setdiff(required, names(scales))
  if (length(missing) > 0L) {
    stop(
      "`scales` is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  for (column in required) {
    values <- scales[[column]]
    if (!is.numeric(values) || any(!is.finite(values)) ||
        any(values < 1) || any(values != round(values))) {
      stop(
        "`scales$", column, "` must contain positive integers.",
        call. = FALSE
      )
    }
  }
  if (any(scales$n_files > scales$total_rows)) {
    stop(
      "`n_files` cannot exceed `total_rows`.",
      call. = FALSE
    )
  }
  invisible(scales)
}

.gp3_perf_validate_limits <- function(limits) {
  if (!is.data.frame(limits)) {
    stop("`limits` must be a data frame.", call. = FALSE)
  }
  required <- c(
    "operation",
    "max_seconds_per_million_rows",
    "max_heap_delta_mb_per_million_rows",
    "max_scaling_exponent"
  )
  missing <- setdiff(required, names(limits))
  if (length(missing) > 0L) {
    stop(
      "`limits` is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(limits)
}

.gp3_perf_positive_integer <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < 1 || x != round(x)) {
    stop("`", argument, "` must be a positive integer.", call. = FALSE)
  }
  as.integer(x)
}

.gp3_perf_integer <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x != round(x)) {
    stop("`", argument, "` must be one integer.", call. = FALSE)
  }
  as.integer(x)
}

.gp3_perf_positive_scalar <- function(x, argument) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop(
      "`", argument, "` must be one positive finite number.",
      call. = FALSE
    )
  }
  invisible(x)
}

.gp3_perf_logical <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", argument, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

.gp3_perf_nonempty_string <- function(x, argument) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(
      "`", argument, "` must be one non-empty character value.",
      call. = FALSE
    )
  }
  x
}
