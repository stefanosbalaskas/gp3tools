#' Compare Gazepoint event-detection workflows
#'
#' Run native velocity-threshold detection, the lightweight gp3tools HMM
#' classifier, and an optional eyetools branch on the same sample-level gaze
#' data. Detector failures are recorded without invalidating successful
#' branches.
#'
#' @param data A sample-level gaze data frame.
#' @param id_col Participant identifier column.
#' @param trial_col Optional trial identifier column.
#' @param group_cols Optional additional sequence columns.
#' @param x_col,y_col Gaze-coordinate columns.
#' @param time_col Timestamp column.
#' @param methods Detector branches to run: `"velocity"`, `"hmm"`, and
#'   `"eyetools"`.
#' @param velocity_thresholds One or more positive velocity thresholds.
#' @param min_duration Minimum fixation duration in milliseconds for the native
#'   velocity branch.
#' @param hmm_states Number of HMM states.
#' @param eyetools_method eyetools detector method.
#' @param run_optional_eyetools Should the optional eyetools branch be run?
#' @param min_overlap Minimum intersection-over-union used to classify
#'   overlapping events as matched.
#' @param velocity_args Named list overriding native velocity-detector
#'   defaults.
#' @param hmm_args Named list overriding HMM-classifier defaults.
#' @param eyetools_args Named list overriding eyetools-wrapper defaults.
#'
#' @return An object of class `"gp3_event_detector_comparison"` containing
#'   standardized event tables, detector-run status, detector summaries,
#'   pairwise agreement, unmatched events, raw detector outputs, and settings.
#'
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   USER_ID = rep("P01", 80),
#'   trial = rep("T01", 80),
#'   TIME = seq(0, by = 0.01, length.out = 80),
#'   FPOGX = c(rep(0.2, 30), seq(0.2, 0.8, length.out = 10), rep(0.8, 40)),
#'   FPOGY = 0.5
#' )
#'
#' comparison <- compare_gazepoint_event_detectors(
#'   gaze,
#'   trial_col = "trial",
#'   methods = "velocity",
#'   velocity_thresholds = c(5, 10)
#' )
#'
#' comparison$detector_summary
compare_gazepoint_event_detectors <- function(
    data,
    id_col = "USER_ID",
    trial_col = NULL,
    group_cols = NULL,
    x_col = "FPOGX",
    y_col = "FPOGY",
    time_col = "TIME",
    methods = c("velocity", "hmm", "eyetools"),
    velocity_thresholds = c(5, 10, 20),
    min_duration = 50,
    hmm_states = 3L,
    eyetools_method = c("vti", "dispersion"),
    run_optional_eyetools = FALSE,
    min_overlap = 0.5,
    velocity_args = list(),
    hmm_args = list(),
    eyetools_args = list()) {

  .gp3_hp_assert_data_frame(data, "data")
  eyetools_method <- match.arg(eyetools_method)

  supported_methods <- c(
    "velocity",
    "hmm",
    "eyetools"
  )

  methods <- unique(as.character(methods))
  unsupported <- setdiff(methods, supported_methods)

  if (length(unsupported)) {
    stop(
      paste0(
        "Unsupported detector method(s): ",
        paste(unsupported, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!length(methods)) {
    stop(
      "`methods` must contain at least one detector.",
      call. = FALSE
    )
  }

  sequence_cols <- unique(c(
    id_col,
    trial_col,
    group_cols
  ))

  sequence_cols <- sequence_cols[
    !is.na(sequence_cols) &
      nzchar(sequence_cols)
  ]

  .gp3_hp_assert_columns(
    data,
    unique(c(
      sequence_cols,
      x_col,
      y_col,
      time_col
    )),
    "data"
  )

  if (!is.numeric(velocity_thresholds) ||
      !length(velocity_thresholds) ||
      anyNA(velocity_thresholds) ||
      any(!is.finite(velocity_thresholds)) ||
      any(velocity_thresholds <= 0)) {
    stop(
      "`velocity_thresholds` must contain finite positive numbers.",
      call. = FALSE
    )
  }

  velocity_thresholds <- sort(
    unique(as.numeric(velocity_thresholds))
  )

  if (!is.numeric(min_duration) ||
      length(min_duration) != 1L ||
      is.na(min_duration) ||
      !is.finite(min_duration) ||
      min_duration < 0) {
    stop(
      "`min_duration` must be one finite non-negative number.",
      call. = FALSE
    )
  }

  if (!is.numeric(hmm_states) ||
      length(hmm_states) != 1L ||
      is.na(hmm_states) ||
      !is.finite(hmm_states) ||
      hmm_states < 2 ||
      hmm_states != as.integer(hmm_states)) {
    stop(
      "`hmm_states` must be an integer of at least 2.",
      call. = FALSE
    )
  }

  hmm_states <- as.integer(hmm_states)

  if (!is.logical(run_optional_eyetools) ||
      length(run_optional_eyetools) != 1L ||
      is.na(run_optional_eyetools)) {
    stop(
      "`run_optional_eyetools` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_overlap) ||
      length(min_overlap) != 1L ||
      is.na(min_overlap) ||
      !is.finite(min_overlap) ||
      min_overlap < 0 ||
      min_overlap > 1) {
    stop(
      "`min_overlap` must be between 0 and 1.",
      call. = FALSE
    )
  }

  override_lists <- list(
    velocity_args = velocity_args,
    hmm_args = hmm_args,
    eyetools_args = eyetools_args
  )

  if (any(!vapply(override_lists, is.list, logical(1)))) {
    stop(
      "Detector override arguments must be lists.",
      call. = FALSE
    )
  }

  event_tables <- list()
  raw_outputs <- list()
  run_rows <- list()
  event_counter <- 0L
  run_counter <- 0L

  add_run <- function(
      detector,
      family,
      status,
      n_events = NA_integer_,
      message = NA_character_) {

    run_counter <<- run_counter + 1L

    run_rows[[run_counter]] <<- data.frame(
      detector = detector,
      family = family,
      status = status,
      n_events = as.integer(n_events),
      message = as.character(message),
      stringsAsFactors = FALSE
    )
  }

  add_events <- function(events) {
    if (!is.data.frame(events) || !nrow(events)) {
      return(invisible(NULL))
    }

    event_counter <<- event_counter + 1L
    event_tables[[event_counter]] <<- events
    invisible(NULL)
  }

  if ("velocity" %in% methods) {
    for (threshold in velocity_thresholds) {
      detector_name <- paste0(
        "velocity_",
        format(
          threshold,
          scientific = FALSE,
          trim = TRUE
        )
      )

      call_args <- .gp3_detector_merge_args(
        list(
          all_gaze = data,
          id_col = id_col,
          x_col = x_col,
          y_col = y_col,
          ts_col = time_col,
          vmax = threshold,
          min_duration = min_duration,
          group_cols = setdiff(
            sequence_cols,
            id_col
          ),
          time_unit = "auto",
          x_scale = 1,
          y_scale = 1,
          return = "events",
          keep_single_sample = FALSE
        ),
        velocity_args,
        protected = c(
          "all_gaze",
          "id_col",
          "x_col",
          "y_col",
          "ts_col",
          "group_cols",
          "return",
          "vmax"
        )
      )

      result <- tryCatch(
        do.call(
          detect_gazepoint_fixations_velocity,
          call_args
        ),
        error = identity
      )

      raw_outputs[[detector_name]] <- result

      if (inherits(result, "error")) {
        add_run(
          detector = detector_name,
          family = "velocity",
          status = "error",
          message = conditionMessage(result)
        )
        next
      }

      standardized <- .gp3_standardize_velocity_events(
        result,
        detector = detector_name,
        sequence_cols = sequence_cols,
        threshold = threshold
      )

      add_events(standardized)

      add_run(
        detector = detector_name,
        family = "velocity",
        status = "ok",
        n_events = nrow(standardized)
      )
    }
  }

  if ("hmm" %in% methods) {
    detector_name <- paste0(
      "hmm_",
      hmm_states,
      "_states"
    )

    hmm_data <- data
    hmm_data$.gp3_detector_sequence <- .gp3_detector_sequence_key(
      hmm_data,
      sequence_cols
    )

    call_args <- .gp3_detector_merge_args(
      list(
        data = hmm_data,
        x = x_col,
        y = y_col,
        time = time_col,
        subject = ".gp3_detector_sequence",
        n_states = hmm_states,
        state_labels = NULL
      ),
      hmm_args,
      protected = c(
        "data",
        "x",
        "y",
        "time",
        "subject",
        "n_states"
      )
    )

    result <- tryCatch(
      do.call(
        classify_gazepoint_events_hmm,
        call_args
      ),
      error = identity
    )

    raw_outputs[[detector_name]] <- result

    if (inherits(result, "error")) {
      add_run(
        detector = detector_name,
        family = "hmm",
        status = "error",
        message = conditionMessage(result)
      )
    } else {
      standardized <- tryCatch(
        .gp3_hmm_samples_to_events(
          result,
          detector = detector_name,
          sequence_cols = sequence_cols,
          x_col = x_col,
          y_col = y_col,
          time_col = time_col
        ),
        error = identity
      )

      if (inherits(standardized, "error")) {
        add_run(
          detector = detector_name,
          family = "hmm",
          status = "error",
          message = conditionMessage(standardized)
        )
      } else {
        add_events(standardized)

        add_run(
          detector = detector_name,
          family = "hmm",
          status = "ok",
          n_events = nrow(standardized)
        )
      }
    }
  }

  if ("eyetools" %in% methods) {
    detector_name <- paste0(
      "eyetools_",
      eyetools_method
    )

    if (!isTRUE(run_optional_eyetools)) {
      raw_outputs[[detector_name]] <- NULL

      add_run(
        detector = detector_name,
        family = "eyetools",
        status = "skipped_disabled",
        message = paste0(
          "Set `run_optional_eyetools = TRUE` to run ",
          "the optional external detector."
        )
      )
    } else if (!requireNamespace(
      "eyetools",
      quietly = TRUE
    )) {
      raw_outputs[[detector_name]] <- NULL

      add_run(
        detector = detector_name,
        family = "eyetools",
        status = "skipped_missing_package",
        message = "Optional package 'eyetools' is not installed."
      )
    } else {
      eyetools_data <- data
      eyetools_trial_col <- trial_col

      if (is.null(eyetools_trial_col)) {
        eyetools_trial_col <- ".gp3_detector_trial"
        eyetools_data[[eyetools_trial_col]] <-
          .gp3_detector_sequence_key(
            eyetools_data,
            setdiff(sequence_cols, id_col)
          )
      }

      call_args <- .gp3_detector_merge_args(
        list(
          data = eyetools_data,
          participant_col = id_col,
          trial_col = eyetools_trial_col,
          time_col = time_col,
          x_col = x_col,
          y_col = y_col,
          condition_col = NULL,
          stimulus_col = NULL,
          method = eyetools_method,
          sample_rate = NULL,
          threshold = 100,
          min_dur = min_duration,
          min_dur_sac = 20,
          disp_tol = 100,
          NA_tol = 0.25,
          smooth = FALSE,
          drop_missing = TRUE,
          progress = FALSE,
          name = detector_name
        ),
        eyetools_args,
        protected = c(
          "data",
          "participant_col",
          "trial_col",
          "time_col",
          "x_col",
          "y_col",
          "method",
          "name"
        )
      )

      result <- tryCatch(
        do.call(
          run_gazepoint_eyetools_fixation_detection,
          call_args
        ),
        error = identity
      )

      raw_outputs[[detector_name]] <- result

      if (inherits(result, "error")) {
        add_run(
          detector = detector_name,
          family = "eyetools",
          status = "error",
          message = conditionMessage(result)
        )
      } else {
        standardized <- tryCatch(
          .gp3_extract_eyetools_events(
            result,
            detector = detector_name,
            original_data = eyetools_data,
            sequence_cols = sequence_cols,
            id_col = id_col,
            trial_col = eyetools_trial_col,
            x_col = x_col,
            y_col = y_col,
            time_col = time_col
          ),
          error = identity
        )

        if (inherits(standardized, "error")) {
          add_run(
            detector = detector_name,
            family = "eyetools",
            status = "error",
            message = conditionMessage(standardized)
          )
        } else {
          add_events(standardized)

          add_run(
            detector = detector_name,
            family = "eyetools",
            status = "ok",
            n_events = nrow(standardized)
          )
        }
      }
    }
  }

  events <- .gp3_bind_detector_events(
    event_tables,
    sequence_cols = sequence_cols
  )

  runs <- do.call(
    rbind,
    run_rows
  )

  rownames(runs) <- NULL

  successful <- runs$status == "ok"

  if (!any(successful)) {
    stop(
      paste0(
        "No event detector completed successfully. ",
        paste(
          unique(runs$message[!is.na(runs$message)]),
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }

  out <- list(
    events = events,
    runs = runs,
    raw_outputs = raw_outputs,
    settings = list(
      id_col = id_col,
      trial_col = trial_col,
      group_cols = group_cols,
      sequence_cols = sequence_cols,
      x_col = x_col,
      y_col = y_col,
      time_col = time_col,
      methods = methods,
      velocity_thresholds = velocity_thresholds,
      min_duration = min_duration,
      hmm_states = hmm_states,
      eyetools_method = eyetools_method,
      run_optional_eyetools = run_optional_eyetools,
      min_overlap = min_overlap,
      velocity_args = velocity_args,
      hmm_args = hmm_args,
      eyetools_args = eyetools_args
    )
  )

  class(out) <- c(
    "gp3_event_detector_comparison",
    "list"
  )

  agreement <- summarise_gazepoint_event_detector_agreement(
    out,
    min_overlap = min_overlap
  )

  out$detector_summary <- agreement$detector_summary
  out$pairwise_agreement <- agreement$pairwise_agreement
  out$unmatched_events <- agreement$unmatched_events
  out
}

#' Summarise agreement between event detectors
#'
#' Compare standardized fixation intervals using event-level
#' intersection-over-union. Agreement is methodological and does not identify a
#' uniquely correct detector.
#'
#' @param x An object returned by [compare_gazepoint_event_detectors()] or a
#'   standardized event data frame containing detector, sequence, start, end,
#'   and duration columns.
#' @param min_overlap Minimum interval intersection-over-union required for a
#'   match.
#'
#' @return A list containing detector summaries, pairwise agreement, unmatched
#'   events, and settings.
#'
#' @export
summarise_gazepoint_event_detector_agreement <- function(
    x,
    min_overlap = 0.5) {

  if (inherits(x, "gp3_event_detector_comparison")) {
    events <- x$events
    sequence_cols <- x$settings$sequence_cols
  } else if (is.data.frame(x)) {
    events <- x
    sequence_cols <- setdiff(
      names(events),
      c(
        "detector",
        "family",
        "threshold",
        "event_id",
        "start_time",
        "end_time",
        "duration_ms",
        "mean_x",
        "mean_y",
        "n_samples",
        "source_status"
      )
    )
  } else {
    stop(
      "`x` must be a detector-comparison object or event data frame.",
      call. = FALSE
    )
  }

  required <- c(
    "detector",
    "start_time",
    "end_time",
    "duration_ms"
  )

  .gp3_hp_assert_columns(
    events,
    required,
    "events"
  )

  if (!is.numeric(min_overlap) ||
      length(min_overlap) != 1L ||
      is.na(min_overlap) ||
      !is.finite(min_overlap) ||
      min_overlap < 0 ||
      min_overlap > 1) {
    stop(
      "`min_overlap` must be between 0 and 1.",
      call. = FALSE
    )
  }

  detector_summary <- .gp3_detector_summary(
    events
  )

  detectors <- unique(
    as.character(events$detector)
  )

  detector_pairs <- if (length(detectors) >= 2L) {
    utils::combn(
      detectors,
      2L,
      simplify = FALSE
    )
  } else {
    list()
  }

  pair_rows <- list()
  unmatched_rows <- list()
  pair_counter <- 0L
  unmatched_counter <- 0L

  for (pair in detector_pairs) {
    detector_a <- pair[[1L]]
    detector_b <- pair[[2L]]

    events_a <- events[
      events$detector == detector_a,
      ,
      drop = FALSE
    ]

    events_b <- events[
      events$detector == detector_b,
      ,
      drop = FALSE
    ]

    sequence_keys <- unique(c(
      .gp3_detector_sequence_key(
        events_a,
        sequence_cols
      ),
      .gp3_detector_sequence_key(
        events_b,
        sequence_cols
      )
    ))

    for (sequence_key in sequence_keys) {
      key_a <- .gp3_detector_sequence_key(
        events_a,
        sequence_cols
      )

      key_b <- .gp3_detector_sequence_key(
        events_b,
        sequence_cols
      )

      block_a <- events_a[
        key_a == sequence_key,
        ,
        drop = FALSE
      ]

      block_b <- events_b[
        key_b == sequence_key,
        ,
        drop = FALSE
      ]

      overlap_a <- .gp3_event_best_overlap(
        block_a,
        block_b
      )

      overlap_b <- .gp3_event_best_overlap(
        block_b,
        block_a
      )

      pair_counter <- pair_counter + 1L

      sequence_values <- .gp3_detector_key_values(
        block_a,
        block_b,
        sequence_cols
      )

      pair_rows[[pair_counter]] <- cbind(
        sequence_values,
        data.frame(
          detector_a = detector_a,
          detector_b = detector_b,
          n_a = nrow(block_a),
          n_b = nrow(block_b),
          matched_a = sum(
            overlap_a >= min_overlap,
            na.rm = TRUE
          ),
          matched_b = sum(
            overlap_b >= min_overlap,
            na.rm = TRUE
          ),
          agreement_a = if (nrow(block_a)) {
            mean(overlap_a >= min_overlap)
          } else {
            NA_real_
          },
          agreement_b = if (nrow(block_b)) {
            mean(overlap_b >= min_overlap)
          } else {
            NA_real_
          },
          mean_best_overlap_a = if (length(overlap_a)) {
            mean(overlap_a)
          } else {
            NA_real_
          },
          mean_best_overlap_b = if (length(overlap_b)) {
            mean(overlap_b)
          } else {
            NA_real_
          },
          min_overlap = min_overlap,
          stringsAsFactors = FALSE
        )
      )

      if (nrow(block_a)) {
        unmatched_a <- which(
          overlap_a < min_overlap
        )

        if (length(unmatched_a)) {
          unmatched_counter <- unmatched_counter + 1L

          temp <- block_a[
            unmatched_a,
            ,
            drop = FALSE
          ]

          temp$compared_with <- detector_b
          temp$best_overlap <- overlap_a[unmatched_a]
          unmatched_rows[[unmatched_counter]] <- temp
        }
      }

      if (nrow(block_b)) {
        unmatched_b <- which(
          overlap_b < min_overlap
        )

        if (length(unmatched_b)) {
          unmatched_counter <- unmatched_counter + 1L

          temp <- block_b[
            unmatched_b,
            ,
            drop = FALSE
          ]

          temp$compared_with <- detector_a
          temp$best_overlap <- overlap_b[unmatched_b]
          unmatched_rows[[unmatched_counter]] <- temp
        }
      }
    }
  }

  pairwise_agreement <- if (length(pair_rows)) {
    do.call(
      rbind,
      pair_rows
    )
  } else {
    template <- events[
      0,
      sequence_cols,
      drop = FALSE
    ]

    cbind(
      template,
      data.frame(
        detector_a = character(),
        detector_b = character(),
        n_a = integer(),
        n_b = integer(),
        matched_a = integer(),
        matched_b = integer(),
        agreement_a = numeric(),
        agreement_b = numeric(),
        mean_best_overlap_a = numeric(),
        mean_best_overlap_b = numeric(),
        min_overlap = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  unmatched_events <- if (length(unmatched_rows)) {
    do.call(
      rbind,
      unmatched_rows
    )
  } else {
    temp <- events[0, , drop = FALSE]
    temp$compared_with <- character()
    temp$best_overlap <- numeric()
    temp
  }

  rownames(pairwise_agreement) <- NULL
  rownames(unmatched_events) <- NULL

  list(
    detector_summary = detector_summary,
    pairwise_agreement = pairwise_agreement,
    unmatched_events = unmatched_events,
    settings = list(
      sequence_cols = sequence_cols,
      min_overlap = min_overlap
    )
  )
}

#' Plot event-detector comparison diagnostics
#'
#' @param x An object returned by [compare_gazepoint_event_detectors()].
#' @param plot Plot type: `"counts"`, `"durations"`, or `"agreement"`.
#' @param main Optional plot title.
#' @param ylab Optional vertical-axis label.
#' @param las Axis-label orientation passed to base graphics.
#'
#' @return Invisibly returns the plotted data.
#'
#' @export
plot_gazepoint_event_detector_agreement <- function(
    x,
    plot = c("counts", "durations", "agreement"),
    main = NULL,
    ylab = NULL,
    las = 2) {

  if (!inherits(
    x,
    "gp3_event_detector_comparison"
  )) {
    stop(
      "`x` must be returned by `compare_gazepoint_event_detectors()`.",
      call. = FALSE
    )
  }

  plot <- match.arg(plot)

  if (identical(plot, "counts")) {
    plot_data <- x$detector_summary

    if (is.null(main)) {
      main <- "Fixation counts by detector"
    }

    if (is.null(ylab)) {
      ylab <- "Number of fixation events"
    }

    graphics::barplot(
      height = plot_data$n_fixations,
      names.arg = plot_data$detector,
      las = las,
      main = main,
      ylab = ylab
    )
  }

  if (identical(plot, "durations")) {
    plot_data <- x$detector_summary

    if (is.null(main)) {
      main <- "Mean fixation duration by detector"
    }

    if (is.null(ylab)) {
      ylab <- "Mean duration (ms)"
    }

    graphics::barplot(
      height = plot_data$mean_duration_ms,
      names.arg = plot_data$detector,
      las = las,
      main = main,
      ylab = ylab
    )
  }

  if (identical(plot, "agreement")) {
    plot_data <- x$pairwise_agreement

    if (!nrow(plot_data)) {
      stop(
        "At least two successful detectors are required for agreement plots.",
        call. = FALSE
      )
    }

    pair_label <- paste(
      plot_data$detector_a,
      plot_data$detector_b,
      sep = " vs "
    )

    value <- rowMeans(
      cbind(
        plot_data$agreement_a,
        plot_data$agreement_b
      ),
      na.rm = TRUE
    )

    value[!is.finite(value)] <- NA_real_

    if (is.null(main)) {
      main <- "Pairwise event agreement"
    }

    if (is.null(ylab)) {
      ylab <- "Matched-event proportion"
    }

    graphics::barplot(
      height = value,
      names.arg = pair_label,
      las = las,
      ylim = c(0, 1),
      main = main,
      ylab = ylab
    )

    graphics::abline(
      h = x$settings$min_overlap,
      lty = 2
    )
  }

  invisible(plot_data)
}

.gp3_detector_merge_args <- function(
    defaults,
    overrides,
    protected = character()) {

  if (!is.list(overrides)) {
    stop(
      "`overrides` must be a list.",
      call. = FALSE
    )
  }

  if (length(overrides) && is.null(names(overrides))) {
    stop(
      "Detector override lists must be named.",
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
        "These detector-managed arguments cannot be overridden: ",
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

.gp3_detector_sequence_key <- function(
    data,
    sequence_cols) {

  if (!nrow(data)) {
    return(character())
  }

  if (!length(sequence_cols)) {
    return(rep(".all", nrow(data)))
  }

  values <- lapply(
    data[sequence_cols],
    function(x) {
      out <- as.character(x)
      out[is.na(out)] <- "<NA>"
      out
    }
  )

  do.call(
    paste,
    c(values, sep = "\r")
  )
}

.gp3_standardize_velocity_events <- function(
    events,
    detector,
    sequence_cols,
    threshold) {

  if (!nrow(events)) {
    return(
      .gp3_empty_detector_events(
        events,
        sequence_cols
      )
    )
  }

  out <- events[
    ,
    unique(c(
      sequence_cols,
      intersect(
        c(
          "fixation_id",
          "start_time",
          "end_time",
          "duration_ms",
          "mean_x",
          "mean_y",
          "n_samples"
        ),
        names(events)
      )
    )),
    drop = FALSE
  ]

  out$detector <- detector
  out$family <- "velocity"
  out$threshold <- threshold

  if ("fixation_id" %in% names(out)) {
    out$event_id <- as.integer(out$fixation_id)
    out$fixation_id <- NULL
  } else {
    out$event_id <- seq_len(nrow(out))
  }

  out$source_status <- "ok"
  .gp3_reorder_detector_events(
    out,
    sequence_cols
  )
}

.gp3_hmm_samples_to_events <- function(
    data,
    detector,
    sequence_cols,
    x_col,
    y_col,
    time_col) {

  .gp3_hp_assert_columns(
    data,
    c(
      sequence_cols,
      x_col,
      y_col,
      time_col,
      "velocity",
      "hmm_state"
    ),
    "HMM output"
  )

  output_rows <- list()
  counter <- 0L
  keys <- .gp3_detector_sequence_key(
    data,
    sequence_cols
  )

  groups <- split(
    seq_len(nrow(data)),
    keys,
    drop = TRUE
  )

  for (idx in groups) {
    ord <- order(
      data[[time_col]][idx],
      na.last = TRUE
    )

    gi <- idx[ord]

    state <- data$hmm_state[gi]
    velocity <- suppressWarnings(
      as.numeric(data$velocity[gi])
    )

    state_levels <- unique(
      state[!is.na(state)]
    )

    if (!length(state_levels)) {
      next
    }

    state_velocity <- vapply(
      state_levels,
      function(level) {
        values <- velocity[
          state == level &
            is.finite(velocity)
        ]

        if (length(values)) {
          stats::median(values)
        } else {
          Inf
        }
      },
      numeric(1)
    )

    fixation_state <- state_levels[
      which.min(state_velocity)
    ]

    fixation_flag <- !is.na(state) &
      state == fixation_state

    runs <- .gp3_detector_true_runs(
      fixation_flag
    )

    if (!nrow(runs)) {
      next
    }

    time_raw <- suppressWarnings(
      as.numeric(data[[time_col]][gi])
    )

    time_info <- .gp3_hp_time_info(
      time_raw,
      "auto"
    )

    time_sec <- time_raw * time_info$to_seconds
    positive_dt <- diff(time_sec)
    positive_dt <- positive_dt[
      is.finite(positive_dt) &
        positive_dt > 0
    ]

    sample_interval <- if (length(positive_dt)) {
      stats::median(positive_dt)
    } else {
      0
    }

    for (run_i in seq_len(nrow(runs))) {
      pos <- seq.int(
        runs$start[[run_i]],
        runs$end[[run_i]]
      )

      row_idx <- gi[pos]
      finite_x <- suppressWarnings(
        as.numeric(data[[x_col]][row_idx])
      )
      finite_y <- suppressWarnings(
        as.numeric(data[[y_col]][row_idx])
      )

      start_time <- time_raw[pos[[1L]]]
      end_time <- time_raw[pos[[length(pos)]]]

      duration_ms <- (
        max(
          0,
          time_sec[pos[[length(pos)]]] -
            time_sec[pos[[1L]]] +
            sample_interval
        )
      ) * 1000

      group_values <- data[
        row_idx[[1L]],
        sequence_cols,
        drop = FALSE
      ]

      counter <- counter + 1L

      output_rows[[counter]] <- cbind(
        group_values,
        data.frame(
          detector = detector,
          family = "hmm",
          threshold = NA_real_,
          event_id = run_i,
          start_time = start_time,
          end_time = end_time,
          duration_ms = duration_ms,
          mean_x = if (any(is.finite(finite_x))) {
            mean(finite_x[is.finite(finite_x)])
          } else {
            NA_real_
          },
          mean_y = if (any(is.finite(finite_y))) {
            mean(finite_y[is.finite(finite_y)])
          } else {
            NA_real_
          },
          n_samples = length(pos),
          source_status = "ok",
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (!length(output_rows)) {
    return(
      .gp3_empty_detector_events(
        data,
        sequence_cols
      )
    )
  }

  out <- do.call(
    rbind,
    output_rows
  )

  rownames(out) <- NULL
  .gp3_reorder_detector_events(
    out,
    sequence_cols
  )
}

.gp3_extract_eyetools_events <- function(
    result,
    detector,
    original_data,
    sequence_cols,
    id_col,
    trial_col,
    x_col,
    y_col,
    time_col) {

  candidates <- .gp3_collect_data_frames(
    result
  )

  if (!length(candidates)) {
    stop(
      "No data-frame output was found in the eyetools result.",
      call. = FALSE
    )
  }

  candidate_names <- names(candidates)

  fixation_priority <- grepl(
    "fix",
    candidate_names,
    ignore.case = TRUE
  )

  candidates <- c(
    candidates[fixation_priority],
    candidates[!fixation_priority]
  )

  for (candidate in candidates) {
    standardized <- tryCatch(
      .gp3_standardize_external_event_frame(
        candidate,
        detector = detector,
        sequence_cols = sequence_cols,
        id_col = id_col,
        trial_col = trial_col,
        x_col = x_col,
        y_col = y_col,
        time_col = time_col
      ),
      error = identity
    )

    if (!inherits(standardized, "error")) {
      return(standardized)
    }
  }

  stop(
    paste0(
      "Could not identify a fixation-level event table in the ",
      "eyetools result."
    ),
    call. = FALSE
  )
}

.gp3_collect_data_frames <- function(
    x,
    path = "result") {

  out <- list()

  if (is.data.frame(x)) {
    out[[path]] <- x
    return(out)
  }

  if (is.list(x)) {
    object_names <- names(x)

    if (is.null(object_names)) {
      object_names <- as.character(seq_along(x))
    }

    for (i in seq_along(x)) {
      child <- .gp3_collect_data_frames(
        x[[i]],
        path = paste0(
          path,
          "$",
          object_names[[i]]
        )
      )

      out <- c(out, child)
    }
  }

  out
}

.gp3_standardize_external_event_frame <- function(
    data,
    detector,
    sequence_cols,
    id_col,
    trial_col,
    x_col,
    y_col,
    time_col) {

  resolve <- function(candidates) {
    hit <- candidates[
      candidates %in% names(data)
    ]

    if (length(hit)) {
      hit[[1L]]
    } else {
      NA_character_
    }
  }

  start_col <- resolve(c(
    "start_time",
    "start",
    "onset",
    "fixation_start",
    "start_time_ms",
    "start_ms"
  ))

  end_col <- resolve(c(
    "end_time",
    "end",
    "offset",
    "fixation_end",
    "end_time_ms",
    "end_ms"
  ))

  duration_col <- resolve(c(
    "duration_ms",
    "duration",
    "dur",
    "fixation_duration",
    "fix_dur"
  ))

  event_id_col <- resolve(c(
    "event_id",
    "fixation_id",
    "fixation_index",
    "fix_id"
  ))

  x_event_col <- resolve(c(
    "mean_x",
    "x_mean",
    "fix_x",
    "x",
    x_col
  ))

  y_event_col <- resolve(c(
    "mean_y",
    "y_mean",
    "fix_y",
    "y",
    y_col
  ))

  sample_col <- resolve(c(
    "n_samples",
    "samples",
    "sample_count"
  ))

  if (is.na(start_col) || is.na(end_col)) {
    stop(
      "External event table lacks recognizable start and end columns.",
      call. = FALSE
    )
  }

  sequence_map <- list()

  for (column in sequence_cols) {
    if (column %in% names(data)) {
      sequence_map[[column]] <- column
    } else if (
      identical(column, id_col) &&
        "participant" %in% names(data)
    ) {
      sequence_map[[column]] <- "participant"
    } else if (
      identical(column, trial_col) &&
        "trial" %in% names(data)
    ) {
      sequence_map[[column]] <- "trial"
    } else {
      stop(
        paste0(
          "External event table lacks sequence column `",
          column,
          "`."
        ),
        call. = FALSE
      )
    }
  }

  out <- data.frame(
    detector = rep(detector, nrow(data)),
    family = rep("eyetools", nrow(data)),
    threshold = rep(NA_real_, nrow(data)),
    event_id = if (!is.na(event_id_col)) {
      suppressWarnings(
        as.integer(data[[event_id_col]])
      )
    } else {
      seq_len(nrow(data))
    },
    start_time = suppressWarnings(
      as.numeric(data[[start_col]])
    ),
    end_time = suppressWarnings(
      as.numeric(data[[end_col]])
    ),
    duration_ms = if (!is.na(duration_col)) {
      suppressWarnings(
        as.numeric(data[[duration_col]])
      )
    } else {
      NA_real_
    },
    mean_x = if (!is.na(x_event_col)) {
      suppressWarnings(
        as.numeric(data[[x_event_col]])
      )
    } else {
      NA_real_
    },
    mean_y = if (!is.na(y_event_col)) {
      suppressWarnings(
        as.numeric(data[[y_event_col]])
      )
    } else {
      NA_real_
    },
    n_samples = if (!is.na(sample_col)) {
      suppressWarnings(
        as.integer(data[[sample_col]])
      )
    } else {
      NA_integer_
    },
    source_status = rep("ok", nrow(data)),
    stringsAsFactors = FALSE
  )

  for (column in sequence_cols) {
    out[[column]] <- data[[sequence_map[[column]]]]
  }

  missing_duration <- !is.finite(
    out$duration_ms
  )

  out$duration_ms[missing_duration] <- (
    out$end_time[missing_duration] -
      out$start_time[missing_duration]
  )

  .gp3_reorder_detector_events(
    out,
    sequence_cols
  )
}

.gp3_detector_true_runs <- function(flag) {
  flag[is.na(flag)] <- FALSE

  if (!length(flag) || !any(flag)) {
    return(
      data.frame(
        start = integer(),
        end = integer()
      )
    )
  }

  encoded <- rle(flag)
  ends <- cumsum(encoded$lengths)
  starts <- ends - encoded$lengths + 1L
  keep <- encoded$values

  data.frame(
    start = starts[keep],
    end = ends[keep],
    row.names = NULL
  )
}

.gp3_empty_detector_events <- function(
    data,
    sequence_cols) {

  template <- data[
    0,
    intersect(sequence_cols, names(data)),
    drop = FALSE
  ]

  missing_sequence_cols <- setdiff(
    sequence_cols,
    names(template)
  )

  for (column in missing_sequence_cols) {
    template[[column]] <- character()
  }

  cbind(
    template[
      ,
      sequence_cols,
      drop = FALSE
    ],
    data.frame(
      detector = character(),
      family = character(),
      threshold = numeric(),
      event_id = integer(),
      start_time = numeric(),
      end_time = numeric(),
      duration_ms = numeric(),
      mean_x = numeric(),
      mean_y = numeric(),
      n_samples = integer(),
      source_status = character(),
      stringsAsFactors = FALSE
    )
  )
}

.gp3_reorder_detector_events <- function(
    data,
    sequence_cols) {

  standard_cols <- c(
    sequence_cols,
    "detector",
    "family",
    "threshold",
    "event_id",
    "start_time",
    "end_time",
    "duration_ms",
    "mean_x",
    "mean_y",
    "n_samples",
    "source_status"
  )

  missing_cols <- setdiff(
    standard_cols,
    names(data)
  )

  for (column in missing_cols) {
    data[[column]] <- NA
  }

  data <- data[
    ,
    standard_cols,
    drop = FALSE
  ]

  rownames(data) <- NULL
  data
}

.gp3_bind_detector_events <- function(
    event_tables,
    sequence_cols) {

  if (!length(event_tables)) {
    dummy <- as.data.frame(
      stats::setNames(
        replicate(
          length(sequence_cols),
          character(),
          simplify = FALSE
        ),
        sequence_cols
      ),
      stringsAsFactors = FALSE
    )

    return(
      .gp3_empty_detector_events(
        dummy,
        sequence_cols
      )
    )
  }

  all_names <- unique(
    unlist(
      lapply(event_tables, names),
      use.names = FALSE
    )
  )

  normalized <- lapply(
    event_tables,
    function(data) {
      missing <- setdiff(
        all_names,
        names(data)
      )

      for (column in missing) {
        data[[column]] <- NA
      }

      data[
        ,
        all_names,
        drop = FALSE
      ]
    }
  )

  out <- do.call(
    rbind,
    normalized
  )

  rownames(out) <- NULL
  .gp3_reorder_detector_events(
    out,
    sequence_cols
  )
}

.gp3_event_best_overlap <- function(
    events_a,
    events_b) {

  if (!nrow(events_a)) {
    return(numeric())
  }

  if (!nrow(events_b)) {
    return(rep(0, nrow(events_a)))
  }

  start_a <- suppressWarnings(
    as.numeric(events_a$start_time)
  )
  end_a <- suppressWarnings(
    as.numeric(events_a$end_time)
  )
  start_b <- suppressWarnings(
    as.numeric(events_b$start_time)
  )
  end_b <- suppressWarnings(
    as.numeric(events_b$end_time)
  )

  vapply(
    seq_len(nrow(events_a)),
    function(i) {
      intersection <- pmax(
        0,
        pmin(end_a[[i]], end_b) -
          pmax(start_a[[i]], start_b)
      )

      union <- pmax(
        end_a[[i]],
        end_b
      ) - pmin(
        start_a[[i]],
        start_b
      )

      iou <- ifelse(
        is.finite(union) &
          union > 0,
        intersection / union,
        0
      )

      max(iou, na.rm = TRUE)
    },
    numeric(1)
  )
}

.gp3_detector_key_values <- function(
    block_a,
    block_b,
    sequence_cols) {

  source <- if (nrow(block_a)) {
    block_a
  } else {
    block_b
  }

  if (!length(sequence_cols)) {
    return(
      data.frame(
        stringsAsFactors = FALSE
      )
    )
  }

  source[
    1L,
    sequence_cols,
    drop = FALSE
  ]
}

.gp3_detector_summary <- function(events) {
  detectors <- unique(
    as.character(events$detector)
  )

  rows <- lapply(
    detectors,
    function(detector) {
      block <- events[
        events$detector == detector,
        ,
        drop = FALSE
      ]

      duration <- suppressWarnings(
        as.numeric(block$duration_ms)
      )

      finite_duration <- duration[
        is.finite(duration)
      ]

      data.frame(
        detector = detector,
        family = as.character(block$family[[1L]]),
        threshold = if (
          any(is.finite(block$threshold))
        ) {
          unique(
            block$threshold[
              is.finite(block$threshold)
            ]
          )[[1L]]
        } else {
          NA_real_
        },
        n_fixations = nrow(block),
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
        total_duration_ms = if (length(finite_duration)) {
          sum(finite_duration)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    }
  )

  if (!length(rows)) {
    return(
      data.frame(
        detector = character(),
        family = character(),
        threshold = numeric(),
        n_fixations = integer(),
        mean_duration_ms = numeric(),
        median_duration_ms = numeric(),
        total_duration_ms = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
