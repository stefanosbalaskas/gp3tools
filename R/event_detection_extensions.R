#' Detect fixations with a velocity-threshold algorithm
#'
#' Converts sample-level gaze coordinates into fixation events using an
#' I-VT-style velocity threshold. The function is intended as a high-level
#' event-table companion to [detect_gazepoint_fixations_ivt()].
#'
#' @param all_gaze A data frame containing sample-level gaze data.
#' @param id_col Participant identifier column.
#' @param x_col Horizontal gaze-coordinate column.
#' @param y_col Vertical gaze-coordinate column.
#' @param ts_col Timestamp column.
#' @param vmax Maximum velocity classified as fixation. The threshold is in
#'   scaled coordinate units per second.
#' @param min_duration Minimum fixation duration in milliseconds.
#' @param group_cols Optional additional grouping columns, such as stimulus or
#'   trial identifiers.
#' @param time_unit Timestamp unit. `"auto"` infers seconds versus milliseconds
#'   from positive timestamp differences.
#' @param x_scale,y_scale Multipliers applied to coordinate differences before
#'   velocity is calculated. Use these to convert native coordinates to visual
#'   degrees when an appropriate conversion is available.
#' @param return Return fixation `"events"`, sample labels, or `"both"`.
#' @param keep_single_sample Retain single-sample events when they satisfy
#'   `min_duration`.
#'
#' @return A tibble of fixation events, a labelled sample table, or a list
#'   containing both.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   USER_ID = "P01",
#'   TIME = seq(0, 0.19, by = 0.01),
#'   FPOGX = c(rep(0.25, 10), rep(0.75, 10)),
#'   FPOGY = 0.50
#' )
#' detect_gazepoint_fixations_velocity(
#'   gaze,
#'   vmax = 5,
#'   min_duration = 40
#' )
detect_gazepoint_fixations_velocity <- function(
  all_gaze,
  id_col = "USER_ID",
  x_col = "FPOGX",
  y_col = "FPOGY",
  ts_col = "TIME",
  vmax = 10,
  min_duration = 50,
  group_cols = NULL,
  time_unit = c("auto", "seconds", "milliseconds"),
  x_scale = 1,
  y_scale = 1,
  return = c("events", "samples", "both"),
  keep_single_sample = FALSE
) {
  .gp3_hp_assert_data_frame(all_gaze, "all_gaze")
  time_unit <- match.arg(time_unit)
  return <- match.arg(return)

  group_cols <- unique(c(id_col, group_cols))
  required <- unique(c(group_cols, x_col, y_col, ts_col))
  .gp3_hp_assert_columns(all_gaze, required, "all_gaze")

  if (!is.numeric(vmax) || length(vmax) != 1L ||
      !is.finite(vmax) || vmax <= 0) {
    stop("`vmax` must be one finite positive number.", call. = FALSE)
  }
  if (!is.numeric(min_duration) || length(min_duration) != 1L ||
      !is.finite(min_duration) || min_duration < 0) {
    stop("`min_duration` must be one finite non-negative number.",
         call. = FALSE)
  }
  if (!is.numeric(x_scale) || length(x_scale) != 1L ||
      !is.finite(x_scale) || x_scale <= 0 ||
      !is.numeric(y_scale) || length(y_scale) != 1L ||
      !is.finite(y_scale) || y_scale <= 0) {
    stop("`x_scale` and `y_scale` must be finite positive numbers.",
         call. = FALSE)
  }

  labelled <- all_gaze
  labelled$gaze_velocity <- NA_real_
  labelled$velocity_fixation <- FALSE
  labelled$velocity_fixation_id <- NA_integer_

  event_rows <- list()
  event_counter <- 0L

  groups <- .gp3_hp_split_indices(all_gaze, group_cols)

  for (idx in groups) {
    ord <- order(all_gaze[[ts_col]][idx], na.last = TRUE)
    gi <- idx[ord]

    time_raw <- suppressWarnings(as.numeric(all_gaze[[ts_col]][gi]))
    x <- suppressWarnings(as.numeric(all_gaze[[x_col]][gi]))
    y <- suppressWarnings(as.numeric(all_gaze[[y_col]][gi]))

    time_info <- .gp3_hp_time_info(time_raw, time_unit)
    time_sec <- time_raw * time_info$to_seconds

    dt <- c(NA_real_, diff(time_sec))
    dx <- c(NA_real_, diff(x) * x_scale)
    dy <- c(NA_real_, diff(y) * y_scale)

    velocity <- sqrt(dx^2 + dy^2) / dt
    velocity[!is.finite(velocity) | !is.finite(dt) | dt <= 0] <- NA_real_

    if (length(velocity) >= 2L && is.na(velocity[1L])) {
      velocity[1L] <- velocity[2L]
    }

    valid_sample <- is.finite(time_sec) & is.finite(x) & is.finite(y)
    fixation_flag <- valid_sample & is.finite(velocity) & velocity <= vmax
    runs <- .gp3_hp_true_runs(fixation_flag)

    labelled$gaze_velocity[gi] <- velocity

    local_fixation_id <- 0L

    if (nrow(runs) > 0L) {
      positive_dt <- dt[is.finite(dt) & dt > 0]
      sample_interval <- if (length(positive_dt)) {
        stats::median(positive_dt)
      } else {
        0
      }

      for (run_i in seq_len(nrow(runs))) {
        run_pos <- seq.int(runs$start[run_i], runs$end[run_i])
        run_idx <- gi[run_pos]

        start_sec <- time_sec[runs$start[run_i]]
        end_sec <- time_sec[runs$end[run_i]]
        duration_sec <- max(0, end_sec - start_sec)

        if (length(run_pos) > 1L || keep_single_sample) {
          duration_coverage_sec <- duration_sec + sample_interval
        } else {
          duration_coverage_sec <- duration_sec
        }

        duration_ms <- duration_coverage_sec * 1000

        if (duration_ms + sqrt(.Machine$double.eps) < min_duration) {
          next
        }

        local_fixation_id <- local_fixation_id + 1L
        labelled$velocity_fixation[run_idx] <- TRUE
        labelled$velocity_fixation_id[run_idx] <- local_fixation_id

        group_values <- all_gaze[run_idx[1L], group_cols, drop = FALSE]
        event_counter <- event_counter + 1L

        event_rows[[event_counter]] <- cbind(
          group_values,
          data.frame(
            fixation_id = local_fixation_id,
            start_time = all_gaze[[ts_col]][run_idx[1L]],
            end_time = all_gaze[[ts_col]][run_idx[length(run_idx)]],
            duration = duration_ms,
            duration_ms = duration_ms,
            n_samples = length(run_idx),
            mean_x = mean(x[run_pos], na.rm = TRUE),
            mean_y = mean(y[run_pos], na.rm = TRUE),
            median_velocity = stats::median(
              velocity[run_pos],
              na.rm = TRUE
            ),
            max_velocity = max(velocity[run_pos], na.rm = TRUE),
            velocity_threshold = vmax,
            algorithm = "I-VT",
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  events <- if (length(event_rows)) {
    do.call(rbind, event_rows)
  } else {
    template <- all_gaze[0, group_cols, drop = FALSE]
    cbind(
      template,
      data.frame(
        fixation_id = integer(),
        start_time = all_gaze[[ts_col]][0],
        end_time = all_gaze[[ts_col]][0],
        duration = numeric(),
        duration_ms = numeric(),
        n_samples = integer(),
        mean_x = numeric(),
        mean_y = numeric(),
        median_velocity = numeric(),
        max_velocity = numeric(),
        velocity_threshold = numeric(),
        algorithm = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  rownames(events) <- NULL
  events <- tibble::as_tibble(events)
  class(events) <- c("gp3_velocity_fixations", class(events))

  labelled <- .gp3_hp_restore_class(labelled, all_gaze)

  if (return == "events") {
    return(events)
  }
  if (return == "samples") {
    return(labelled)
  }

  structure(
    list(events = events, samples = labelled),
    class = c("gp3_velocity_fixation_result", "list")
  )
}


#' Detect blink intervals from pupil measurements
#'
#' Detects blink-like periods using missing or non-positive pupil samples and
#' optional robust drop/recovery rules. Multiple pupil columns can be supplied;
#' their row-wise mean is used for detection.
#'
#' @param all_gaze A data frame containing sample-level pupil data.
#' @param pupil_col Pupil column or columns. When `NULL`, common gp3tools and
#'   Gazepoint pupil names are detected automatically.
#' @param ts_col Timestamp column.
#' @param id_col Participant identifier column.
#' @param group_cols Optional additional grouping columns.
#' @param min_duration Minimum retained blink duration in milliseconds.
#' @param z_thresh Robust threshold for low values and rapid changes.
#' @param zero_threshold Values at or below this threshold are invalid.
#' @param merge_gap_ms Merge blink candidates separated by no more than this
#'   duration.
#' @param time_unit Timestamp unit.
#' @param include_rapid_changes Include robust rapid drop and recovery flags.
#' @param return Return event intervals, sample labels, or both.
#'
#' @return A blink-event tibble, a labelled sample table, or both.
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = "P01",
#'   TIME = seq(0, 0.19, by = 0.01),
#'   mean_pupil = c(rep(3.2, 7), NA, NA, NA, rep(3.2, 10))
#' )
#' detect_gazepoint_blinks(pupil, min_duration = 20)
detect_gazepoint_blinks <- function(
  all_gaze,
  pupil_col = NULL,
  ts_col = "TIME",
  id_col = "USER_ID",
  group_cols = NULL,
  min_duration = 50,
  z_thresh = 4,
  zero_threshold = 0,
  merge_gap_ms = 20,
  time_unit = c("auto", "seconds", "milliseconds"),
  include_rapid_changes = TRUE,
  return = c("events", "samples", "both")
) {
  .gp3_hp_assert_data_frame(all_gaze, "all_gaze")
  time_unit <- match.arg(time_unit)
  return <- match.arg(return)

  pupil_col <- .gp3_hp_detect_pupil_columns(all_gaze, pupil_col)
  group_cols <- unique(c(id_col, group_cols))
  required <- unique(c(group_cols, pupil_col, ts_col))
  .gp3_hp_assert_columns(all_gaze, required, "all_gaze")

  for (arg in c("min_duration", "z_thresh", "merge_gap_ms")) {
    value <- get(arg, inherits = FALSE)
    if (!is.numeric(value) || length(value) != 1L ||
        !is.finite(value) || value < 0) {
      stop(sprintf("`%s` must be one finite non-negative number.", arg),
           call. = FALSE)
    }
  }

  labelled <- all_gaze
  labelled$blink_detected <- FALSE
  labelled$blink_id <- NA_integer_
  labelled$blink_reason <- NA_character_

  event_rows <- list()
  event_counter <- 0L
  groups <- .gp3_hp_split_indices(all_gaze, group_cols)

  for (idx in groups) {
    ord <- order(all_gaze[[ts_col]][idx], na.last = TRUE)
    gi <- idx[ord]
    time_raw <- suppressWarnings(as.numeric(all_gaze[[ts_col]][gi]))
    time_info <- .gp3_hp_time_info(time_raw, time_unit)
    time_sec <- time_raw * time_info$to_seconds

    pupil_matrix <- as.matrix(all_gaze[gi, pupil_col, drop = FALSE])
    storage.mode(pupil_matrix) <- "double"
    available <- rowSums(is.finite(pupil_matrix))
    pupil <- rowMeans(pupil_matrix, na.rm = TRUE)
    pupil[available == 0L] <- NA_real_

    missing_flag <- !is.finite(pupil)
    zero_flag <- is.finite(pupil) & pupil <= zero_threshold

    finite_pupil <- pupil[is.finite(pupil) & pupil > zero_threshold]
    pupil_med <- if (length(finite_pupil)) {
      stats::median(finite_pupil)
    } else {
      NA_real_
    }
    pupil_mad <- if (length(finite_pupil) >= 3L) {
      stats::mad(finite_pupil, constant = 1.4826)
    } else {
      NA_real_
    }

    low_flag <- rep(FALSE, length(pupil))
    if (is.finite(pupil_mad) && pupil_mad > 0) {
      low_flag <- is.finite(pupil) &
        pupil < pupil_med - z_thresh * pupil_mad
    }

    drop_flag <- rep(FALSE, length(pupil))
    recovery_flag <- rep(FALSE, length(pupil))

    if (isTRUE(include_rapid_changes) && length(pupil) >= 3L) {
      delta <- c(NA_real_, diff(pupil))
      finite_delta <- delta[is.finite(delta)]
      delta_mad <- if (length(finite_delta) >= 3L) {
        stats::mad(finite_delta, constant = 1.4826)
      } else {
        NA_real_
      }

      if (is.finite(delta_mad) && delta_mad > 0) {
        raw_drop <- is.finite(delta) & delta < -z_thresh * delta_mad
        raw_recovery <- is.finite(delta) & delta > z_thresh * delta_mad

        drop_flag <- raw_drop |
          c(raw_drop[-1L], FALSE)
        recovery_flag <- raw_recovery |
          c(FALSE, raw_recovery[-length(raw_recovery)])
      }
    }

    candidate <- missing_flag | zero_flag | low_flag |
      drop_flag | recovery_flag
    runs <- .gp3_hp_merge_true_runs(
      candidate,
      time_sec,
      max_gap_sec = merge_gap_ms / 1000
    )

    local_blink_id <- 0L

    if (nrow(runs) > 0L) {
      positive_dt <- diff(time_sec)
      positive_dt <- positive_dt[is.finite(positive_dt) & positive_dt > 0]
      sample_interval <- if (length(positive_dt)) {
        stats::median(positive_dt)
      } else {
        0
      }

      for (run_i in seq_len(nrow(runs))) {
        run_pos <- seq.int(runs$start[run_i], runs$end[run_i])
        run_idx <- gi[run_pos]

        start_sec <- time_sec[runs$start[run_i]]
        end_sec <- time_sec[runs$end[run_i]]
        duration_ms <- max(
          0,
          end_sec - start_sec + sample_interval
        ) * 1000

        if (duration_ms + sqrt(.Machine$double.eps) < min_duration) {
          next
        }

        reasons <- character()
        if (any(missing_flag[run_pos])) reasons <- c(reasons, "missing")
        if (any(zero_flag[run_pos])) reasons <- c(reasons, "zero")
        if (any(low_flag[run_pos])) reasons <- c(reasons, "low_outlier")
        if (any(drop_flag[run_pos])) reasons <- c(reasons, "rapid_drop")
        if (any(recovery_flag[run_pos])) {
          reasons <- c(reasons, "rapid_recovery")
        }
        reason <- paste(unique(reasons), collapse = ";")

        local_blink_id <- local_blink_id + 1L
        labelled$blink_detected[run_idx] <- TRUE
        labelled$blink_id[run_idx] <- local_blink_id
        labelled$blink_reason[run_idx] <- reason

        group_values <- all_gaze[run_idx[1L], group_cols, drop = FALSE]
        event_counter <- event_counter + 1L

        event_rows[[event_counter]] <- cbind(
          group_values,
          data.frame(
            blink_id = local_blink_id,
            start_time = all_gaze[[ts_col]][run_idx[1L]],
            end_time = all_gaze[[ts_col]][run_idx[length(run_idx)]],
            duration = duration_ms,
            duration_ms = duration_ms,
            n_samples = length(run_idx),
            reason = reason,
            pupil_columns = paste(pupil_col, collapse = ";"),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  events <- if (length(event_rows)) {
    do.call(rbind, event_rows)
  } else {
    template <- all_gaze[0, group_cols, drop = FALSE]
    cbind(
      template,
      data.frame(
        blink_id = integer(),
        start_time = all_gaze[[ts_col]][0],
        end_time = all_gaze[[ts_col]][0],
        duration = numeric(),
        duration_ms = numeric(),
        n_samples = integer(),
        reason = character(),
        pupil_columns = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  rownames(events) <- NULL
  events <- tibble::as_tibble(events)
  class(events) <- c("gp3_blink_events", class(events))
  labelled <- .gp3_hp_restore_class(labelled, all_gaze)

  if (return == "events") {
    return(events)
  }
  if (return == "samples") {
    return(labelled)
  }

  structure(
    list(events = events, samples = labelled),
    class = c("gp3_blink_detection_result", "list")
  )
}


#' Smooth gaze coordinates within independent sequences
#'
#' Applies a centred rolling median or moving average to gaze coordinates.
#'
#' @param all_gaze A sample-level gaze data frame.
#' @param method Smoothing method.
#' @param window Positive integer rolling-window width.
#' @param x_col,y_col Coordinate columns.
#' @param id_col Participant identifier.
#' @param group_cols Additional independent-sequence columns.
#' @param suffix Suffix for generated columns.
#' @param min_valid Minimum finite samples required in a rolling window.
#' @param preserve_missing Keep smoothed values missing where the original
#'   coordinate is missing.
#'
#' @return The input data with smoothed coordinate columns.
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   USER_ID = "P01",
#'   FPOGX = c(0.1, 0.11, 0.5, 0.12, 0.13),
#'   FPOGY = c(0.2, 0.21, 0.6, 0.22, 0.23)
#' )
#' smooth_gazepoint_coordinate(gaze, window = 3)
smooth_gazepoint_coordinate <- function(
  all_gaze,
  method = c("median", "mean"),
  window = 5,
  x_col = "FPOGX",
  y_col = "FPOGY",
  id_col = "USER_ID",
  group_cols = NULL,
  suffix = "_smooth",
  min_valid = 1,
  preserve_missing = TRUE
) {
  .gp3_hp_assert_data_frame(all_gaze, "all_gaze")
  method <- match.arg(method)
  group_cols <- unique(c(id_col, group_cols))
  .gp3_hp_assert_columns(
    all_gaze,
    unique(c(group_cols, x_col, y_col)),
    "all_gaze"
  )

  if (!is.numeric(window) || length(window) != 1L ||
      !is.finite(window) || window < 1 ||
      window != as.integer(window)) {
    stop("`window` must be one positive integer.", call. = FALSE)
  }
  window <- as.integer(window)

  if (!is.numeric(min_valid) || length(min_valid) != 1L ||
      !is.finite(min_valid) || min_valid < 1 ||
      min_valid != as.integer(min_valid)) {
    stop("`min_valid` must be one positive integer.", call. = FALSE)
  }
  min_valid <- as.integer(min_valid)

  output <- all_gaze
  x_out <- paste0(x_col, suffix)
  y_out <- paste0(y_col, suffix)
  output[[x_out]] <- NA_real_
  output[[y_out]] <- NA_real_

  groups <- .gp3_hp_split_indices(all_gaze, group_cols)

  for (idx in groups) {
    x <- suppressWarnings(as.numeric(all_gaze[[x_col]][idx]))
    y <- suppressWarnings(as.numeric(all_gaze[[y_col]][idx]))

    sx <- .gp3_hp_roll(
      x,
      window = window,
      method = method,
      min_valid = min_valid
    )
    sy <- .gp3_hp_roll(
      y,
      window = window,
      method = method,
      min_valid = min_valid
    )

    if (isTRUE(preserve_missing)) {
      sx[!is.finite(x)] <- NA_real_
      sy[!is.finite(y)] <- NA_real_
    }

    output[[x_out]][idx] <- sx
    output[[y_out]][idx] <- sy
  }

  attr(output, "gazepoint_coordinate_smoothing") <- list(
    method = method,
    window = window,
    x_col = x_col,
    y_col = y_col,
    group_cols = group_cols
  )

  .gp3_hp_restore_class(output, all_gaze)
}


#' Simulate Gazepoint-like fixation events
#'
#' Generates event-level fixation data with bounded random spatial drift.
#'
#' @param n_subjects Number of simulated participants.
#' @param n_fix Number of fixations per participant.
#' @param sd Standard deviation of spatial random-walk increments.
#' @param coordinate_system Coordinate representation.
#' @param screen_width,screen_height Pixel dimensions when
#'   `coordinate_system = "pixels"`.
#' @param duration_mean,duration_sd Mean and standard deviation of fixation
#'   duration in milliseconds.
#' @param saccade_gap_mean Mean interval between fixations in milliseconds.
#' @param seed Optional random seed.
#'
#' @return A tibble resembling a Gazepoint fixation export.
#' @export
#'
#' @examples
#' simulate_gazepoint_fixations(
#'   n_subjects = 2,
#'   n_fix = 10,
#'   seed = 1
#' )
simulate_gazepoint_fixations <- function(
  n_subjects = 10,
  n_fix = 50,
  sd = 10,
  coordinate_system = c("pixels", "normalized"),
  screen_width = 1920,
  screen_height = 1080,
  duration_mean = 250,
  duration_sd = 80,
  saccade_gap_mean = 40,
  seed = NULL
) {
  coordinate_system <- match.arg(coordinate_system)

  integer_args <- c(n_subjects = n_subjects, n_fix = n_fix)
  if (any(!is.finite(integer_args)) ||
      any(integer_args < 1) ||
      any(integer_args != as.integer(integer_args))) {
    stop("`n_subjects` and `n_fix` must be positive integers.",
         call. = FALSE)
  }

  numeric_args <- c(
    sd = sd,
    screen_width = screen_width,
    screen_height = screen_height,
    duration_mean = duration_mean,
    duration_sd = duration_sd,
    saccade_gap_mean = saccade_gap_mean
  )
  if (any(!is.finite(numeric_args)) || any(numeric_args < 0)) {
    stop("Simulation scale and duration arguments must be non-negative.",
         call. = FALSE)
  }
  if (screen_width <= 0 || screen_height <= 0 || duration_mean <= 0) {
    stop("Screen dimensions and `duration_mean` must be positive.",
         call. = FALSE)
  }

  if (!is.null(seed)) {
    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv,
                              inherits = FALSE)
    if (old_seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv,
                      inherits = FALSE)
    }
    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv,
                        inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
  }

  rows <- vector("list", n_subjects)

  for (subject_i in seq_len(n_subjects)) {
    duration_ms <- pmax(
      40,
      stats::rnorm(n_fix, mean = duration_mean, sd = duration_sd)
    )
    gap_ms <- pmax(
      0,
      stats::rexp(n_fix, rate = 1 / max(saccade_gap_mean, 1))
    )

    if (coordinate_system == "pixels") {
      x0 <- stats::runif(1, 0.2 * screen_width, 0.8 * screen_width)
      y0 <- stats::runif(1, 0.2 * screen_height, 0.8 * screen_height)
      x <- x0 + cumsum(stats::rnorm(n_fix, 0, sd))
      y <- y0 + cumsum(stats::rnorm(n_fix, 0, sd))
      x <- pmin(screen_width, pmax(0, x))
      y <- pmin(screen_height, pmax(0, y))
      f_x <- x / screen_width
      f_y <- y / screen_height
    } else {
      x0 <- stats::runif(1, 0.2, 0.8)
      y0 <- stats::runif(1, 0.2, 0.8)
      step_sd <- if (sd > 1) sd / max(screen_width, screen_height) else sd
      x <- x0 + cumsum(stats::rnorm(n_fix, 0, step_sd))
      y <- y0 + cumsum(stats::rnorm(n_fix, 0, step_sd))
      x <- pmin(1, pmax(0, x))
      y <- pmin(1, pmax(0, y))
      f_x <- x
      f_y <- y
    }

    onset_ms <- cumsum(c(0, utils::head(duration_ms + gap_ms, -1L)))
    end_ms <- onset_ms + duration_ms

    rows[[subject_i]] <- data.frame(
      USER_ID = sprintf("P%03d", subject_i),
      MEDIA_ID = "simulated_stimulus",
      FPOGID = seq_len(n_fix),
      FPOGS = onset_ms / 1000,
      FPOGD = duration_ms / 1000,
      FPOGX = f_x,
      FPOGY = f_y,
      FPOGV = 1L,
      subject = sprintf("P%03d", subject_i),
      fixation_id = seq_len(n_fix),
      start_time = onset_ms / 1000,
      end_time = end_ms / 1000,
      duration = duration_ms,
      duration_ms = duration_ms,
      x = x,
      y = y,
      coordinate_system = coordinate_system,
      stringsAsFactors = FALSE
    )
  }

  output <- tibble::as_tibble(do.call(rbind, rows))
  class(output) <- c("gp3_simulated_fixations", class(output))
  output
}
