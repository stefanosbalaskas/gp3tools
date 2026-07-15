#' Run an integrated Gazepoint signal-preprocessing workflow
#'
#' Orchestrate transparent blink, pupil, coordinate, downsampling, and
#' velocity-based fixation-processing steps while preserving the original
#' input columns. Every requested operation is recorded in a decision log.
#'
#' @param data A sample-level Gazepoint or gp3tools data frame.
#' @param id_col Participant identifier column.
#' @param group_cols Optional additional columns defining independent time
#'   series, such as trial or stimulus.
#' @param time_col Timestamp column.
#' @param x_col,y_col Gaze-coordinate columns.
#' @param left_pupil_col,right_pupil_col Optional binocular pupil columns.
#'   When `NULL`, common Gazepoint and gp3tools names are detected.
#' @param pupil_col Optional existing monocular or fused pupil column used when
#'   `pupil_mode = "none"`.
#' @param pupil_mode Binocular fusion mode: `"mean"`, `"regression"`, or
#'   `"none"`.
#' @param detect_blinks Should [detect_gazepoint_blinks()] be run?
#' @param interpolate_blinks Should detected blink intervals be interpolated?
#' @param smooth_pupil Should [smooth_gazepoint_pupil()] be run?
#' @param smooth_coordinates Should [smooth_gazepoint_coordinate()] be run?
#' @param downsample_factor Positive integer downsampling factor. Use `1` to
#'   retain the original sample count.
#' @param detect_fixations Should [detect_gazepoint_fixations_velocity()] be
#'   run on the final full-resolution coordinates?
#' @param blink_args Named list overriding blink-detection defaults.
#' @param interpolation_args Named list overriding blink-interpolation
#'   defaults.
#' @param pupil_args Named list overriding binocular fusion defaults.
#' @param pupil_smoothing_args Named list overriding pupil-smoothing defaults.
#' @param coordinate_smoothing_args Named list overriding coordinate-smoothing
#'   defaults.
#' @param downsampling_args Named list overriding downsampling defaults.
#' @param fixation_args Named list overriding fixation-detection defaults.
#'
#' @return An object of class `"gp3_signal_preprocessing_result"` containing
#'   processed `data`, detected `blinks`, detected `fixations`, diagnostic
#'   tables, a `decision_log`, and resolved `settings`.
#'
#' @export
#'
#' @examples
#' pupil <- data.frame(
#'   USER_ID = rep("P01", 30),
#'   trial = rep("T01", 30),
#'   TIME = seq(0, 0.29, by = 0.01),
#'   FPOGX = c(rep(0.25, 15), rep(0.75, 15)),
#'   FPOGY = 0.50,
#'   LPupil = c(rep(3.2, 10), NA, NA, rep(3.2, 18)),
#'   RPupil = c(rep(3.1, 10), NA, NA, rep(3.1, 18))
#' )
#'
#' result <- preprocess_gazepoint_signals(
#'   pupil,
#'   group_cols = "trial",
#'   downsample_factor = 2
#' )
#'
#' result$decision_log
preprocess_gazepoint_signals <- function(
    data,
    id_col = "USER_ID",
    group_cols = NULL,
    time_col = "TIME",
    x_col = "FPOGX",
    y_col = "FPOGY",
    left_pupil_col = NULL,
    right_pupil_col = NULL,
    pupil_col = NULL,
    pupil_mode = c("mean", "regression", "none"),
    detect_blinks = TRUE,
    interpolate_blinks = TRUE,
    smooth_pupil = TRUE,
    smooth_coordinates = TRUE,
    downsample_factor = 1L,
    detect_fixations = TRUE,
    blink_args = list(),
    interpolation_args = list(),
    pupil_args = list(),
    pupil_smoothing_args = list(),
    coordinate_smoothing_args = list(),
    downsampling_args = list(),
    fixation_args = list()) {

  .gp3_hp_assert_data_frame(data, "data")
  pupil_mode <- match.arg(pupil_mode)

  logical_args <- c(
    detect_blinks = detect_blinks,
    interpolate_blinks = interpolate_blinks,
    smooth_pupil = smooth_pupil,
    smooth_coordinates = smooth_coordinates,
    detect_fixations = detect_fixations
  )

  if (anyNA(logical_args) || any(!logical_args %in% c(TRUE, FALSE))) {
    stop(
      "Workflow switches must be non-missing logical values.",
      call. = FALSE
    )
  }

  list_args <- list(
    blink_args = blink_args,
    interpolation_args = interpolation_args,
    pupil_args = pupil_args,
    pupil_smoothing_args = pupil_smoothing_args,
    coordinate_smoothing_args = coordinate_smoothing_args,
    downsampling_args = downsampling_args,
    fixation_args = fixation_args
  )

  invalid_lists <- names(list_args)[
    !vapply(list_args, is.list, logical(1))
  ]

  if (length(invalid_lists)) {
    stop(
      paste0(
        "Workflow override arguments must be lists. Invalid: ",
        paste(invalid_lists, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(downsample_factor) ||
      length(downsample_factor) != 1L ||
      is.na(downsample_factor) ||
      !is.finite(downsample_factor) ||
      downsample_factor < 1 ||
      downsample_factor != as.integer(downsample_factor)) {
    stop(
      "`downsample_factor` must be one positive integer.",
      call. = FALSE
    )
  }

  downsample_factor <- as.integer(downsample_factor)
  group_cols <- unique(as.character(group_cols))
  series_cols <- unique(c(id_col, group_cols))

  .gp3_hp_assert_columns(
    data,
    unique(c(series_cols, time_col)),
    "data"
  )

  needs_coordinates <- isTRUE(smooth_coordinates) ||
    isTRUE(detect_fixations)

  if (needs_coordinates) {
    .gp3_hp_assert_columns(
      data,
      c(x_col, y_col),
      "data"
    )
  }

  pupil_columns <- .gp3_preprocess_resolve_pupil_columns(
    data = data,
    left_pupil_col = left_pupil_col,
    right_pupil_col = right_pupil_col,
    pupil_col = pupil_col,
    pupil_mode = pupil_mode
  )

  working <- data
  blinks <- .gp3_empty_blink_events(
    data,
    series_cols = series_cols,
    time_col = time_col
  )
  fixations <- .gp3_empty_fixation_events(
    data,
    series_cols = series_cols,
    time_col = time_col
  )

  log_rows <- list()
  log_counter <- 0L

  add_log <- function(
      operation,
      requested,
      status,
      input_rows,
      output_rows,
      details = NA_character_) {

    log_counter <<- log_counter + 1L
    log_rows[[log_counter]] <<- data.frame(
      step = log_counter,
      operation = operation,
      requested = isTRUE(requested),
      status = status,
      input_rows = as.integer(input_rows),
      output_rows = as.integer(output_rows),
      details = as.character(details),
      stringsAsFactors = FALSE
    )
  }

  current_pupil_col <- pupil_columns$pupil_col
  full_resolution_rows <- nrow(working)

  if (identical(pupil_mode, "mean")) {
    input_rows <- nrow(working)

    call_args <- .gp3_preprocess_merge_args(
      list(
        master_df = working,
        lp_col = pupil_columns$left,
        rp_col = pupil_columns$right,
        output_col = "gp3_pupil_fused",
        min_eyes = 1
      ),
      pupil_args,
      protected = c("master_df", "lp_col", "rp_col", "output_col")
    )

    working <- do.call(
      mean_gazepoint_pupil,
      call_args
    )

    current_pupil_col <- "gp3_pupil_fused"

    add_log(
      operation = "binocular_pupil_mean",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste(
        pupil_columns$left,
        pupil_columns$right,
        sep = " + "
      )
    )
  } else if (identical(pupil_mode, "regression")) {
    input_rows <- nrow(working)

    call_args <- .gp3_preprocess_merge_args(
      list(
        master_df = working,
        lp_col = pupil_columns$left,
        rp_col = pupil_columns$right,
        id_col = id_col,
        group_cols = group_cols,
        direction = "bidirectional",
        output_col = "gp3_pupil_fused",
        residual_col = "gp3_pupil_regression_residual",
        min_complete = 10
      ),
      pupil_args,
      protected = c(
        "master_df",
        "lp_col",
        "rp_col",
        "id_col",
        "group_cols",
        "output_col",
        "residual_col"
      )
    )

    working <- do.call(
      regress_gazepoint_pupils,
      call_args
    )

    current_pupil_col <- "gp3_pupil_fused"

    add_log(
      operation = "binocular_pupil_regression",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste(
        pupil_columns$left,
        pupil_columns$right,
        sep = " ~ "
      )
    )
  } else {
    add_log(
      operation = "binocular_pupil_fusion",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = paste0(
        "Using existing pupil column: ",
        current_pupil_col
      )
    )
  }

  if (isTRUE(detect_blinks)) {
    input_rows <- nrow(working)

    call_args <- .gp3_preprocess_merge_args(
      list(
        all_gaze = working,
        pupil_col = current_pupil_col,
        ts_col = time_col,
        id_col = id_col,
        group_cols = group_cols,
        min_duration = 50,
        z_thresh = 4,
        zero_threshold = 0,
        merge_gap_ms = 20,
        time_unit = "auto",
        include_rapid_changes = TRUE,
        return = "both"
      ),
      blink_args,
      protected = c(
        "all_gaze",
        "pupil_col",
        "ts_col",
        "id_col",
        "group_cols",
        "return"
      )
    )

    blink_result <- do.call(
      detect_gazepoint_blinks,
      call_args
    )

    working <- blink_result$samples
    blinks <- blink_result$events

    add_log(
      operation = "blink_detection",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste0(
        nrow(blinks),
        " blink interval(s)"
      )
    )
  } else {
    add_log(
      operation = "blink_detection",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Blink detection disabled."
    )
  }

  if (isTRUE(interpolate_blinks)) {
    if (!isTRUE(detect_blinks)) {
      stop(
        "`interpolate_blinks = TRUE` requires `detect_blinks = TRUE`.",
        call. = FALSE
      )
    }

    input_rows <- nrow(working)
    interpolation_suffix <- if (
      !is.null(interpolation_args$suffix)
    ) {
      as.character(interpolation_args$suffix)[1L]
    } else {
      "_blink_interp"
    }

    call_args <- .gp3_preprocess_merge_args(
      list(
        master_df = working,
        blink_df = blinks,
        pupil_cols = current_pupil_col,
        id_col = id_col,
        group_cols = group_cols,
        ts_col = time_col,
        start_col = "start_time",
        end_col = "end_time",
        method = "linear",
        max_gap_ms = 500,
        suffix = interpolation_suffix,
        keep_mask = TRUE,
        time_unit = "auto"
      ),
      interpolation_args,
      protected = c(
        "master_df",
        "blink_df",
        "pupil_cols",
        "id_col",
        "group_cols",
        "ts_col",
        "start_col",
        "end_col"
      )
    )

    working <- do.call(
      interpolate_gazepoint_blinks,
      call_args
    )

    current_pupil_col <- paste0(
      current_pupil_col,
      interpolation_suffix
    )

    add_log(
      operation = "blink_interpolation",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste0(
        "Output pupil column: ",
        current_pupil_col
      )
    )
  } else {
    add_log(
      operation = "blink_interpolation",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Blink interpolation disabled."
    )
  }

  if (isTRUE(smooth_pupil)) {
    input_rows <- nrow(working)

    call_args <- .gp3_preprocess_merge_args(
      list(
        data = working,
        pupil_col = current_pupil_col,
        time_col = time_col,
        group_cols = series_cols,
        window_samples = 5,
        method = "mean",
        align = "center",
        min_points = 1,
        preserve_missing = TRUE
      ),
      pupil_smoothing_args,
      protected = c(
        "data",
        "pupil_col",
        "time_col",
        "group_cols"
      )
    )

    working <- do.call(
      smooth_gazepoint_pupil,
      call_args
    )

    current_pupil_col <- "pupil_smoothed"

    add_log(
      operation = "pupil_smoothing",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = "Output pupil column: pupil_smoothed"
    )
  } else {
    add_log(
      operation = "pupil_smoothing",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Pupil smoothing disabled."
    )
  }

  fixation_x_col <- x_col
  fixation_y_col <- y_col

  if (isTRUE(smooth_coordinates)) {
    input_rows <- nrow(working)
    coordinate_suffix <- if (
      !is.null(coordinate_smoothing_args$suffix)
    ) {
      as.character(coordinate_smoothing_args$suffix)[1L]
    } else {
      "_smooth"
    }

    call_args <- .gp3_preprocess_merge_args(
      list(
        all_gaze = working,
        method = "median",
        window = 5,
        x_col = x_col,
        y_col = y_col,
        id_col = id_col,
        group_cols = group_cols,
        suffix = coordinate_suffix,
        min_valid = 1,
        preserve_missing = TRUE
      ),
      coordinate_smoothing_args,
      protected = c(
        "all_gaze",
        "x_col",
        "y_col",
        "id_col",
        "group_cols"
      )
    )

    working <- do.call(
      smooth_gazepoint_coordinate,
      call_args
    )

    fixation_x_col <- paste0(x_col, coordinate_suffix)
    fixation_y_col <- paste0(y_col, coordinate_suffix)

    add_log(
      operation = "coordinate_smoothing",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste(
        fixation_x_col,
        fixation_y_col,
        sep = ", "
      )
    )
  } else {
    add_log(
      operation = "coordinate_smoothing",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Coordinate smoothing disabled."
    )
  }

  if (isTRUE(detect_fixations)) {
    input_rows <- nrow(working)

    call_args <- .gp3_preprocess_merge_args(
      list(
        all_gaze = working,
        id_col = id_col,
        x_col = fixation_x_col,
        y_col = fixation_y_col,
        ts_col = time_col,
        vmax = 10,
        min_duration = 50,
        group_cols = group_cols,
        time_unit = "auto",
        x_scale = 1,
        y_scale = 1,
        return = "events",
        keep_single_sample = FALSE
      ),
      fixation_args,
      protected = c(
        "all_gaze",
        "id_col",
        "x_col",
        "y_col",
        "ts_col",
        "group_cols",
        "return"
      )
    )

    fixations <- do.call(
      detect_gazepoint_fixations_velocity,
      call_args
    )

    add_log(
      operation = "velocity_fixation_detection",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste0(
        nrow(fixations),
        " fixation event(s)"
      )
    )
  } else {
    add_log(
      operation = "velocity_fixation_detection",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Velocity-based fixation detection disabled."
    )
  }

  full_resolution_data <- working

  if (downsample_factor > 1L) {
    input_rows <- nrow(working)

    pupil_candidates <- unique(c(
      current_pupil_col,
      "gp3_pupil_fused",
      "pupil_smoothed"
    ))

    pupil_candidates <- pupil_candidates[
      pupil_candidates %in% names(working)
    ]

    call_args <- .gp3_preprocess_merge_args(
      list(
        master_df = working,
        factor = downsample_factor,
        pupil_cols = pupil_candidates,
        id_col = id_col,
        group_cols = group_cols,
        ts_col = time_col,
        method = "mean",
        keep_bin = FALSE
      ),
      downsampling_args,
      protected = c(
        "master_df",
        "factor",
        "id_col",
        "group_cols",
        "ts_col"
      )
    )

    working <- do.call(
      downsample_gazepoint_pupil,
      call_args
    )

    add_log(
      operation = "downsampling",
      requested = TRUE,
      status = "applied",
      input_rows = input_rows,
      output_rows = nrow(working),
      details = paste0(
        "Aggregation factor: ",
        downsample_factor
      )
    )
  } else {
    add_log(
      operation = "downsampling",
      requested = FALSE,
      status = "skipped",
      input_rows = nrow(working),
      output_rows = nrow(working),
      details = "Downsampling factor equals 1."
    )
  }

  decision_log <- do.call(
    rbind,
    log_rows
  )

  rownames(decision_log) <- NULL

  overview <- data.frame(
    original_rows = nrow(data),
    full_resolution_processed_rows = full_resolution_rows,
    returned_rows = nrow(working),
    original_columns = ncol(data),
    returned_columns = ncol(working),
    n_blinks = nrow(blinks),
    n_fixations = nrow(fixations),
    pupil_mode = pupil_mode,
    final_pupil_col = current_pupil_col,
    fixation_x_col = fixation_x_col,
    fixation_y_col = fixation_y_col,
    downsample_factor = downsample_factor,
    workflow_status = "ok",
    stringsAsFactors = FALSE
  )

  signal_summary <- .gp3_preprocess_signal_summary(
    original = data,
    processed = full_resolution_data,
    returned = working,
    pupil_col = current_pupil_col,
    x_col = fixation_x_col,
    y_col = fixation_y_col
  )

  blink_summary <- .gp3_preprocess_blink_summary(
    blinks
  )

  fixation_summary <- .gp3_preprocess_fixation_summary(
    fixations
  )

  settings <- list(
    id_col = id_col,
    group_cols = group_cols,
    time_col = time_col,
    x_col = x_col,
    y_col = y_col,
    left_pupil_col = pupil_columns$left,
    right_pupil_col = pupil_columns$right,
    input_pupil_col = pupil_columns$pupil_col,
    final_pupil_col = current_pupil_col,
    pupil_mode = pupil_mode,
    detect_blinks = detect_blinks,
    interpolate_blinks = interpolate_blinks,
    smooth_pupil = smooth_pupil,
    smooth_coordinates = smooth_coordinates,
    downsample_factor = downsample_factor,
    detect_fixations = detect_fixations,
    blink_args = blink_args,
    interpolation_args = interpolation_args,
    pupil_args = pupil_args,
    pupil_smoothing_args = pupil_smoothing_args,
    coordinate_smoothing_args = coordinate_smoothing_args,
    downsampling_args = downsampling_args,
    fixation_args = fixation_args
  )

  out <- list(
    data = working,
    blinks = blinks,
    fixations = fixations,
    diagnostics = list(
      overview = overview,
      signal_summary = signal_summary,
      blink_summary = blink_summary,
      fixation_summary = fixation_summary
    ),
    decision_log = decision_log,
    settings = settings
  )

  class(out) <- c(
    "gp3_signal_preprocessing_result",
    "list"
  )

  out
}

.gp3_preprocess_merge_args <- function(
    defaults,
    overrides,
    protected = character()) {

  if (!is.list(overrides)) {
    stop(
      "`overrides` must be a list.",
      call. = FALSE
    )
  }

  if (is.null(names(overrides)) && length(overrides)) {
    stop(
      "Workflow override lists must be named.",
      call. = FALSE
    )
  }

  protected_overrides <- intersect(
    names(overrides),
    protected
  )

  if (length(protected_overrides)) {
    stop(
      paste0(
        "These workflow-managed arguments cannot be overridden: ",
        paste(protected_overrides, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  utils::modifyList(
    defaults,
    overrides,
    keep.null = TRUE
  )
}

.gp3_preprocess_resolve_pupil_columns <- function(
    data,
    left_pupil_col,
    right_pupil_col,
    pupil_col,
    pupil_mode) {

  detect_one <- function(
      supplied,
      candidates,
      argument_name,
      required = TRUE) {

    if (!is.null(supplied)) {
      supplied <- as.character(supplied)[1L]
      .gp3_hp_assert_columns(
        data,
        supplied,
        "data"
      )
      return(supplied)
    }

    hit <- candidates[
      candidates %in% names(data)
    ]

    if (length(hit)) {
      return(hit[[1L]])
    }

    if (isTRUE(required)) {
      stop(
        paste0(
          "Could not detect `",
          argument_name,
          "`. Supply it explicitly."
        ),
        call. = FALSE
      )
    }

    NULL
  }

  left <- detect_one(
    supplied = left_pupil_col,
    candidates = c(
      "LPupil",
      "LPD",
      "LPMM",
      "left_pupil",
      "pupil_left"
    ),
    argument_name = "left_pupil_col",
    required = pupil_mode %in% c("mean", "regression")
  )

  right <- detect_one(
    supplied = right_pupil_col,
    candidates = c(
      "RPupil",
      "RPD",
      "RPMM",
      "right_pupil",
      "pupil_right"
    ),
    argument_name = "right_pupil_col",
    required = pupil_mode %in% c("mean", "regression")
  )

  existing <- if (!is.null(pupil_col)) {
    detect_one(
      supplied = pupil_col,
      candidates = character(),
      argument_name = "pupil_col",
      required = TRUE
    )
  } else if (pupil_mode %in% c("mean", "regression")) {
    NULL
  } else {
    detect_one(
      supplied = NULL,
      candidates = c(
        "pupil_smoothed",
        "pupil_interpolated",
        "pupil_clean",
        "pupil_for_preprocessing",
        "mean_pupil",
        "pupil_regressed",
        "pupil",
        "pupil_raw",
        "LPupil",
        "RPupil",
        "LPD",
        "RPD",
        "LPMM",
        "RPMM"
      ),
      argument_name = "pupil_col",
      required = TRUE
    )
  }

  list(
    left = left,
    right = right,
    pupil_col = existing
  )
}

.gp3_empty_blink_events <- function(
    data,
    series_cols,
    time_col) {

  template <- data[
    0,
    series_cols,
    drop = FALSE
  ]

  out <- cbind(
    template,
    data.frame(
      blink_id = integer(),
      start_time = data[[time_col]][0],
      end_time = data[[time_col]][0],
      duration = numeric(),
      duration_ms = numeric(),
      n_samples = integer(),
      reason = character(),
      pupil_columns = character(),
      stringsAsFactors = FALSE
    )
  )

  tibble::as_tibble(out)
}

.gp3_empty_fixation_events <- function(
    data,
    series_cols,
    time_col) {

  template <- data[
    0,
    series_cols,
    drop = FALSE
  ]

  out <- cbind(
    template,
    data.frame(
      fixation_id = integer(),
      start_time = data[[time_col]][0],
      end_time = data[[time_col]][0],
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

  tibble::as_tibble(out)
}

.gp3_preprocess_signal_summary <- function(
    original,
    processed,
    returned,
    pupil_col,
    x_col,
    y_col) {

  count_finite <- function(data, column) {
    if (is.null(column) || !column %in% names(data)) {
      return(NA_integer_)
    }

    values <- suppressWarnings(
      as.numeric(data[[column]])
    )

    as.integer(sum(is.finite(values)))
  }

  data.frame(
    stage = c(
      "original",
      "full_resolution_processed",
      "returned"
    ),
    n_rows = c(
      nrow(original),
      nrow(processed),
      nrow(returned)
    ),
    finite_pupil = c(
      count_finite(original, pupil_col),
      count_finite(processed, pupil_col),
      count_finite(returned, pupil_col)
    ),
    finite_x = c(
      count_finite(original, x_col),
      count_finite(processed, x_col),
      count_finite(returned, x_col)
    ),
    finite_y = c(
      count_finite(original, y_col),
      count_finite(processed, y_col),
      count_finite(returned, y_col)
    ),
    stringsAsFactors = FALSE
  )
}

.gp3_preprocess_blink_summary <- function(blinks) {
  if (!is.data.frame(blinks) || nrow(blinks) == 0L) {
    return(
      data.frame(
        reason = character(),
        n_blinks = integer(),
        mean_duration_ms = numeric(),
        max_duration_ms = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  reason <- if ("reason" %in% names(blinks)) {
    as.character(blinks$reason)
  } else {
    rep("unspecified", nrow(blinks))
  }

  duration <- if ("duration_ms" %in% names(blinks)) {
    suppressWarnings(
      as.numeric(blinks$duration_ms)
    )
  } else {
    rep(NA_real_, nrow(blinks))
  }

  groups <- split(
    seq_len(nrow(blinks)),
    reason,
    drop = TRUE
  )

  rows <- lapply(
    names(groups),
    function(label) {
      idx <- groups[[label]]
      finite_duration <- duration[idx][
        is.finite(duration[idx])
      ]

      data.frame(
        reason = label,
        n_blinks = length(idx),
        mean_duration_ms = if (length(finite_duration)) {
          mean(finite_duration)
        } else {
          NA_real_
        },
        max_duration_ms = if (length(finite_duration)) {
          max(finite_duration)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

.gp3_preprocess_fixation_summary <- function(fixations) {
  if (!is.data.frame(fixations) || nrow(fixations) == 0L) {
    return(
      data.frame(
        algorithm = character(),
        n_fixations = integer(),
        mean_duration_ms = numeric(),
        median_duration_ms = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  algorithm <- if ("algorithm" %in% names(fixations)) {
    as.character(fixations$algorithm)
  } else {
    rep("unspecified", nrow(fixations))
  }

  duration <- suppressWarnings(
    as.numeric(fixations$duration_ms)
  )

  groups <- split(
    seq_len(nrow(fixations)),
    algorithm,
    drop = TRUE
  )

  rows <- lapply(
    names(groups),
    function(label) {
      idx <- groups[[label]]
      finite_duration <- duration[idx][
        is.finite(duration[idx])
      ]

      data.frame(
        algorithm = label,
        n_fixations = length(idx),
        mean_duration_ms = if (length(finite_duration)) {
          mean(finite_duration)
        } else {
          NA_real_
        },
        median_duration_ms = if (length(finite_duration)) {
          stats::median(finite_duration)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}
