.gp3_ext_json_escape <- function(x) {
  x <- as.character(x)
  x <- gsub('\\\\', '\\\\\\\\', x)
  x <- gsub('"', '\\\"', x)
  x
}

.gp3_ext_write_json <- function(x, path) {
  lines <- c("{")
  nms <- names(x)
  body <- vapply(seq_along(x), function(i) {
    value <- x[[i]]
    key <- paste0('  "', .gp3_ext_json_escape(nms[i]), '": ')
    if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
      paste0(key, as.character(value))
    } else if (is.logical(value) && length(value) == 1L && !is.na(value)) {
      paste0(key, tolower(as.character(value)))
    } else if (length(value) > 1L) {
      vals <- paste0('"', .gp3_ext_json_escape(value), '"', collapse = ", ")
      paste0(key, "[", vals, "]")
    } else if (length(value) == 0L || is.na(value)) {
      paste0(key, "null")
    } else {
      paste0(key, '"', .gp3_ext_json_escape(value), '"')
    }
  }, character(1))
  if (length(body) > 1L) {
    body[-length(body)] <- paste0(body[-length(body)], ",")
  }
  lines <- c(lines, body, "}")
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' Simulate simple Gazepoint-style gaze data
#'
#' Generate a compact synthetic Gazepoint-style data set for examples, tests,
#' teaching, and workflow demonstrations. The simulation is intentionally
#' simple and should not be treated as a realistic generative model of visual
#' attention.
#'
#' @param n_subjects Number of synthetic participants.
#' @param n_trials Number of trials per participant.
#' @param trial_duration_ms Trial duration in milliseconds.
#' @param sampling_rate_hz Sampling rate in Hz.
#' @param conditions Character vector of condition labels.
#' @param aoi_labels Character vector of AOI labels.
#' @param effect_size Logit-scale increase in target-AOI probability for the
#'   second and later conditions.
#' @param target_aoi AOI receiving the simulated condition effect.
#' @param seed Optional random seed.
#' @param include_fixations Should a simple fixation-level table be returned?
#'
#' @return A list containing \code{all_gaze}, \code{aoi_windows}, optional
#'   \code{fixations}, and simulation metadata.
#' @export
simulate_gazepoint_data <- function(n_subjects = 12,
                                    n_trials = 8,
                                    trial_duration_ms = 2000,
                                    sampling_rate_hz = 60,
                                    conditions = c("control", "treatment"),
                                    aoi_labels = c("target", "other"),
                                    effect_size = 0.5,
                                    target_aoi = aoi_labels[1L],
                                    seed = NULL,
                                    include_fixations = TRUE) {
  if (!is.numeric(n_subjects) || length(n_subjects) != 1L || is.na(n_subjects) || n_subjects < 1L) {
    stop("n_subjects must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(n_trials) || length(n_trials) != 1L || is.na(n_trials) || n_trials < 1L) {
    stop("n_trials must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(trial_duration_ms) || trial_duration_ms <= 0) {
    stop("trial_duration_ms must be positive.", call. = FALSE)
  }
  if (!is.numeric(sampling_rate_hz) || sampling_rate_hz <= 0) {
    stop("sampling_rate_hz must be positive.", call. = FALSE)
  }
  conditions <- as.character(conditions)
  aoi_labels <- as.character(aoi_labels)
  if (length(conditions) == 0L || any(!nzchar(conditions))) {
    stop("conditions must contain non-empty labels.", call. = FALSE)
  }
  if (length(aoi_labels) < 2L || any(!nzchar(aoi_labels))) {
    stop("aoi_labels must contain at least two non-empty labels.", call. = FALSE)
  }
  if (!target_aoi %in% aoi_labels) {
    stop("target_aoi must be one of aoi_labels.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n_subjects <- as.integer(n_subjects)
  n_trials <- as.integer(n_trials)
  sample_interval <- 1000 / sampling_rate_hz
  time_values <- seq(0, trial_duration_ms, by = sample_interval)
  n_samples <- length(time_values)
  base_prob <- rep((1 - 0.45) / (length(aoi_labels) - 1L), length(aoi_labels))
  names(base_prob) <- aoi_labels
  base_prob[target_aoi] <- 0.45

  rows <- vector("list", n_subjects * n_trials)
  k <- 1L
  for (s in seq_len(n_subjects)) {
    subject_id <- sprintf("S%03d", s)
    subject_shift <- stats::rnorm(1L, 0, 0.35)
    for (tr in seq_len(n_trials)) {
      condition <- conditions[((tr - 1L) %% length(conditions)) + 1L]
      cond_effect <- if (condition == conditions[1L]) 0 else effect_size
      logits <- stats::qlogis(pmin(pmax(base_prob, 0.001), 0.999))
      logits[target_aoi] <- logits[target_aoi] + cond_effect + subject_shift
      prob <- exp(logits) / sum(exp(logits))
      aoi <- sample(aoi_labels, n_samples, replace = TRUE, prob = prob)
      x_center <- seq(0.25, 0.75, length.out = length(aoi_labels))
      names(x_center) <- aoi_labels
      y_center <- rev(x_center)
      names(y_center) <- aoi_labels
      x <- stats::rnorm(n_samples, x_center[aoi], 0.04)
      y <- stats::rnorm(n_samples, y_center[aoi], 0.04)
      pupil <- 3 + 0.10 * (aoi == target_aoi) + 0.05 * (condition != conditions[1L]) +
        stats::rnorm(n_samples, 0, 0.05)
      rows[[k]] <- data.frame(
        subject_id = subject_id,
        trial_id = tr,
        condition = condition,
        time_ms = time_values,
        aoi = aoi,
        x = pmin(pmax(x, 0), 1),
        y = pmin(pmax(y, 0), 1),
        pupil = pupil,
        valid = TRUE,
        stringsAsFactors = FALSE
      )
      k <- k + 1L
    }
  }
  all_gaze <- .gp3_ext_bind_rows(rows)
  rownames(all_gaze) <- NULL

  split_trial <- split(all_gaze, interaction(all_gaze$subject_id, all_gaze$trial_id, drop = TRUE))
  aoi_windows <- .gp3_ext_bind_rows(lapply(split_trial, function(block) {
    target_prop <- mean(block$aoi == target_aoi)
    data.frame(
      subject_id = block$subject_id[1L],
      trial_id = block$trial_id[1L],
      condition = block$condition[1L],
      target_aoi = target_aoi,
      n_samples = nrow(block),
      target_samples = sum(block$aoi == target_aoi),
      target_prop = target_prop,
      mean_pupil = mean(block$pupil),
      stringsAsFactors = FALSE
    )
  }))
  rownames(aoi_windows) <- NULL

  fixations <- NULL
  if (isTRUE(include_fixations)) {
    fix_rows <- lapply(split_trial, function(block) {
      run_id <- cumsum(c(TRUE, block$aoi[-1L] != block$aoi[-nrow(block)]))
      runs <- split(block, run_id)
      .gp3_ext_bind_rows(lapply(seq_along(runs), function(i) {
        r <- runs[[i]]
        data.frame(
          subject_id = r$subject_id[1L],
          trial_id = r$trial_id[1L],
          condition = r$condition[1L],
          fixation_index = i,
          aoi = r$aoi[1L],
          start_time_ms = min(r$time_ms),
          end_time_ms = max(r$time_ms),
          duration_ms = max(r$time_ms) - min(r$time_ms) + sample_interval,
          x = mean(r$x),
          y = mean(r$y),
          stringsAsFactors = FALSE
        )
      }))
    })
    fixations <- .gp3_ext_bind_rows(fix_rows)
    rownames(fixations) <- NULL
  }

  out <- list(
    all_gaze = all_gaze,
    aoi_windows = aoi_windows,
    fixations = fixations,
    metadata = list(
      n_subjects = n_subjects,
      n_trials = n_trials,
      trial_duration_ms = trial_duration_ms,
      sampling_rate_hz = sampling_rate_hz,
      conditions = conditions,
      aoi_labels = aoi_labels,
      target_aoi = target_aoi,
      effect_size = effect_size
    )
  )
  class(out) <- c("gp3_simulated_data", "list")
  out
}

#' Export Gazepoint data to a lightweight BIDS-style folder
#'
#' Write a conservative BIDS-style eye-tracking folder from a Gazepoint-style
#' data frame. This helper creates standard-looking TSV and JSON sidecar files
#' for sharing and inspection, but it does not claim full validation against a
#' specific evolving BIDS eye-tracking validator.
#'
#' @param data A gaze-sample data frame.
#' @param outdir Output directory.
#' @param subject_col Subject column.
#' @param task Task label used in file names.
#' @param session Optional session label.
#' @param time_col Time column.
#' @param x_col Horizontal gaze coordinate column.
#' @param y_col Vertical gaze coordinate column.
#' @param pupil_col Optional pupil column.
#' @param trial_col Optional trial column.
#' @param aoi_col Optional AOI column.
#' @param overwrite Should an existing output directory be reused?
#'
#' @return A data frame listing written files.
#' @export
export_gazepoint_to_bids <- function(data,
                                    outdir,
                                    subject_col,
                                    task = "gazepoint",
                                    session = NULL,
                                    time_col,
                                    x_col,
                                    y_col,
                                    pupil_col = NULL,
                                    trial_col = NULL,
                                    aoi_col = NULL,
                                    overwrite = FALSE) {
  .gp3_ext_check_data(data)
  outdir <- .gp3_ext_check_scalar_string(outdir, "outdir")
  subject_col <- .gp3_ext_check_scalar_string(subject_col, "subject_col")
  time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  x_col <- .gp3_ext_check_scalar_string(x_col, "x_col")
  y_col <- .gp3_ext_check_scalar_string(y_col, "y_col")
  if (!is.null(pupil_col)) pupil_col <- .gp3_ext_check_scalar_string(pupil_col, "pupil_col")
  if (!is.null(trial_col)) trial_col <- .gp3_ext_check_scalar_string(trial_col, "trial_col")
  if (!is.null(aoi_col)) aoi_col <- .gp3_ext_check_scalar_string(aoi_col, "aoi_col")
  .gp3_ext_check_columns(data, c(subject_col, time_col, x_col, y_col, pupil_col, trial_col, aoi_col))
  if (dir.exists(outdir) && !isTRUE(overwrite)) {
    stop("outdir already exists. Use overwrite = TRUE to write into it.", call. = FALSE)
  }
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  .gp3_ext_write_json(
    list(
      Name = "Gazepoint eye-tracking export",
      BIDSVersion = "BIDS-style",
      DatasetType = "raw",
      GeneratedBy = "gp3tools"
    ),
    file.path(outdir, "dataset_description.json")
  )

  subjects <- sort(unique(as.character(data[[subject_col]])))
  participants <- data.frame(
    participant_id = paste0("sub-", gsub("[^A-Za-z0-9]", "", subjects)),
    source_subject_id = subjects,
    stringsAsFactors = FALSE
  )
  utils::write.table(
    participants, file.path(outdir, "participants.tsv"),
    sep = "\t", row.names = FALSE, quote = FALSE
  )

  written <- list(
    data.frame(file = file.path(outdir, "dataset_description.json"), file_type = "dataset_description", stringsAsFactors = FALSE),
    data.frame(file = file.path(outdir, "participants.tsv"), file_type = "participants", stringsAsFactors = FALSE)
  )
  k <- length(written) + 1L
  for (sid in subjects) {
    sub_label <- paste0("sub-", gsub("[^A-Za-z0-9]", "", sid))
    subject_data <- data[as.character(data[[subject_col]]) == sid, , drop = FALSE]
    subdir <- file.path(outdir, sub_label)
    if (!is.null(session)) {
      ses_label <- paste0("ses-", gsub("[^A-Za-z0-9]", "", as.character(session)))
      subdir <- file.path(subdir, ses_label)
    } else {
      ses_label <- NULL
    }
    eyedir <- file.path(subdir, "eyetrack")
    dir.create(eyedir, recursive = TRUE, showWarnings = FALSE)
    prefix <- paste0(sub_label, "_")
    if (!is.null(ses_label)) prefix <- paste0(prefix, ses_label, "_")
    prefix <- paste0(prefix, "task-", gsub("[^A-Za-z0-9]", "", task), "_eyetrack")
    tsv_path <- file.path(eyedir, paste0(prefix, ".tsv"))
    json_path <- file.path(eyedir, paste0(prefix, ".json"))

    export_cols <- data.frame(
      time = subject_data[[time_col]],
      x_coordinate = subject_data[[x_col]],
      y_coordinate = subject_data[[y_col]],
      stringsAsFactors = FALSE
    )
    if (!is.null(pupil_col)) export_cols$pupil_size <- subject_data[[pupil_col]]
    if (!is.null(trial_col)) export_cols$trial_id <- subject_data[[trial_col]]
    if (!is.null(aoi_col)) export_cols$aoi <- subject_data[[aoi_col]]
    utils::write.table(export_cols, tsv_path, sep = "\t", row.names = FALSE, quote = FALSE)
    .gp3_ext_write_json(
      list(
        TaskName = task,
        SourceSubject = sid,
        SamplingCoordinateSystem = "screen_normalized_or_source_units",
        Columns = names(export_cols),
        Note = "BIDS-style export helper; inspect against the current BIDS eye-tracking specification before formal deposition."
      ),
      json_path
    )
    written[[k]] <- data.frame(file = tsv_path, file_type = "eyetrack_tsv", stringsAsFactors = FALSE)
    written[[k + 1L]] <- data.frame(file = json_path, file_type = "eyetrack_json", stringsAsFactors = FALSE)
    k <- k + 2L
  }
  out <- .gp3_ext_bind_rows(written)
  rownames(out) <- NULL
  out
}

#' Fit an optional Bayesian AOI model with brms
#'
#' Prepare and optionally fit a Bayesian AOI model using \pkg{brms}. The
#' dependency is optional: the function checks for \pkg{brms} only when
#' \code{dry_run = FALSE}. Tests and lightweight workflows can use
#' \code{dry_run = TRUE} to inspect the formula and data without running Stan.
#'
#' @param data A data frame.
#' @param response Response column.
#' @param predictors Character vector of fixed-effect predictors.
#' @param subject_col Optional grouping column for a random intercept.
#' @param family brms family specification as a character string.
#' @param prior Optional brms prior object.
#' @param dry_run If TRUE, return the prepared call components without fitting.
#' @param ... Additional arguments passed to \code{brms::brm()} when fitting.
#'
#' @return A dry-run specification list or a \code{brmsfit} object.
#' @export
fit_gazepoint_aoi_brms <- function(data,
                                  response,
                                  predictors,
                                  subject_col = NULL,
                                  family = "bernoulli",
                                  prior = NULL,
                                  dry_run = TRUE,
                                  ...) {
  .gp3_ext_check_data(data)
  response <- .gp3_ext_check_scalar_string(response, "response")
  predictors <- .gp3_ext_check_character_vector(predictors, "predictors")
  if (!is.null(subject_col)) {
    subject_col <- .gp3_ext_check_scalar_string(subject_col, "subject_col")
  }
  .gp3_ext_check_columns(data, c(response, predictors, subject_col))
  fixed <- paste(predictors, collapse = " + ")
  rhs <- fixed
  if (!is.null(subject_col)) {
    rhs <- paste(rhs, paste0("(1 | ", subject_col, ")"), sep = " + ")
  }
  formula <- stats::as.formula(paste(response, "~", rhs))
  spec <- list(
    formula = formula,
    data = data,
    family = family,
    prior = prior,
    dry_run = isTRUE(dry_run),
    model_status = if (isTRUE(dry_run)) "dry_run" else "pending_fit"
  )
  class(spec) <- c("gp3_brms_spec", "list")
  if (isTRUE(dry_run)) {
    return(spec)
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Package 'brms' is required for fitting. Install it or use dry_run = TRUE.", call. = FALSE)
  }
  brms_family <- get(family, envir = asNamespace("brms"))()
  brms::brm(
    formula = formula,
    data = data,
    family = brms_family,
    prior = prior,
    ...
  )
}
