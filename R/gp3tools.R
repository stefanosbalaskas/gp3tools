#' Standardise Gazepoint column names
#'
#' Converts timestamped Gazepoint headers such as `TIME(2026/02/20 00:53:57.275)`
#' to `TIME`, converts `TIMETICK(f=10000000)` to `TIMETICK`, trims whitespace,
#' and removes empty columns created by trailing commas in Gazepoint exports.
#'
#' @param x A data frame or character vector of column names.
#'
#' @return If `x` is a data frame, the same data frame with standardised names and
#' empty Gazepoint columns removed. If `x` is a character vector, a character vector.
#' @export
standardise_gazepoint_names <- function(x) {
  standardise_one <- function(nm) {
    nm <- trimws(nm)
    nm <- ifelse(grepl("^TIME\\(", nm), "TIME", nm)
    nm <- ifelse(grepl("^TIMETICK\\(", nm), "TIMETICK", nm)
    nm <- ifelse(nm == "" | is.na(nm), "EMPTY_TRAILING", nm)
    nm
  }

  if (is.data.frame(x)) {
    names(x) <- standardise_one(names(x))
    x <- .drop_empty_gazepoint_columns(x)
    return(x)
  }

  standardise_one(x)
}

.drop_empty_gazepoint_columns <- function(data) {
  column_names <- names(data)

  empty_or_auto_names <- is.na(column_names) |
    column_names == "" |
    column_names == "EMPTY_TRAILING" |
    grepl("^\\.\\.\\d+$", column_names) |
    grepl("^Unnamed", column_names)

  empty_values <- vapply(
    data,
    function(x) {
      if (is.character(x)) {
        all(is.na(x) | trimws(x) == "")
      } else {
        all(is.na(x))
      }
    },
    logical(1)
  )

  keep <- !(empty_or_auto_names & empty_values)

  data[, keep, drop = FALSE]
}

#' Classify a Gazepoint export
#'
#' Uses the filename and header structure to classify a file as `all_gaze`,
#' `fixations`, `summary`, or `unknown`.
#'
#' @param path File path.
#'
#' @return A single character string.
#' @export
classify_gazepoint_export <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste0(path, " does not exist."))
  }

  name <- basename(path)

  if (grepl("Data_Summary_export", name, ignore.case = TRUE)) return("summary")
  if (grepl("fix", name, ignore.case = TRUE)) return("fixations")
  if (grepl("all_gaze|user\\.csv", name, ignore.case = TRUE)) return("all_gaze")

  first <- readLines(path, n = 1, warn = FALSE)

  if (length(first) == 1 && grepl("Gazepoint Analysis", first, fixed = TRUE)) {
    return("summary")
  }

  if (length(first) == 1 && grepl("FPOGX", first, fixed = TRUE)) {
    return("gaze_table")
  }

  "unknown"
}

#' Read a Gazepoint all-gaze or fixation CSV export
#'
#' Reads Gazepoint all-gaze and fixation CSV exports, standardises timestamped
#' column names, and removes empty trailing columns produced by Gazepoint exports.
#'
#' @param path Path to a Gazepoint CSV export.
#' @param standardise_names Logical. If `TRUE`, standardise `TIME(...)` and
#' `TIMETICK(...)` headers.
#' @param drop_empty_cols Logical. If `TRUE`, remove empty trailing or unnamed
#' columns created by Gazepoint export formatting.
#'
#' @return A tibble with attributes `gp3_file_type` and `gp3_source_file`.
#' @export
read_gazepoint <- function(
    path,
    standardise_names = TRUE,
    drop_empty_cols = TRUE
) {
  if (!file.exists(path)) {
    rlang::abort(paste0(path, " does not exist."))
  }

  file_type <- classify_gazepoint_export(path)

  if (identical(file_type, "summary")) {
    rlang::abort(
      "This appears to be a Gazepoint Analysis summary export. ",
      "Use `read_gazepoint_summary()` instead."
    )
  }

  dat <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )

  if (standardise_names) {
    dat <- standardise_gazepoint_names(dat)
  }

  if (drop_empty_cols) {
    dat <- .drop_empty_gazepoint_columns(dat)
  }

  attr(dat, "gp3_file_type") <- file_type
  attr(dat, "gp3_source_file") <- basename(path)

  dat
}

#' Inspect Gazepoint columns
#'
#' @param x A data frame or path to a Gazepoint CSV export.
#'
#' @return A tibble describing column names, semantic groups, and missingness.
#' @export
inspect_gazepoint_columns <- function(x) {
  if (is.character(x) && length(x) == 1) {
    x <- read_gazepoint(x)
  } else {
    x <- standardise_gazepoint_names(x)
  }

  groups <- list(
    identification = c("MEDIA_ID", "MEDIA_NAME", "CNT"),
    time = c("TIME", "TIMETICK"),
    fixation_gaze = c("FPOGX", "FPOGY", "FPOGS", "FPOGD", "FPOGID", "FPOGV"),
    best_gaze = c("BPOGX", "BPOGY", "BPOGV"),
    cursor_keyboard_user = c("CX", "CY", "CS", "KB", "KBS", "USER"),
    left_eye_pupil = c("LPCX", "LPCY", "LPD", "LPS", "LPV", "LPMM", "LPMMV"),
    right_eye_pupil = c("RPCX", "RPCY", "RPD", "RPS", "RPV", "RPMM", "RPMMV"),
    blink = c("BKID", "BKDUR", "BKPMIN"),
    biometrics = c(
      "DIAL", "DIALV", "GSR", "GSR_US", "GSR_US_TONIC", "GSR_US_PHASIC",
      "GSRV", "HR", "HRV", "HRP", "IBI"
    ),
    ttl = c("TTL0", "TTL1", "TTL2", "TTL3", "TTL4", "TTL5", "TTL6", "TTLV"),
    derived = c("PIXS", "PIXV", "AOI", "SACCADE_MAG", "SACCADE_DIR", "VID_FRAME")
  )

  group_of <- function(col) {
    hit <- names(groups)[vapply(groups, function(g) col %in% g, logical(1))]
    if (length(hit) == 0) "other" else hit[1]
  }

  tibble::tibble(
    column = names(x),
    semantic_group = vapply(names(x), group_of, character(1)),
    dtype = vapply(x, function(z) paste(class(z), collapse = "/"), character(1)),
    n_missing = vapply(x, function(z) sum(is.na(z)), integer(1)),
    pct_missing = vapply(x, function(z) mean(is.na(z)) * 100, numeric(1))
  )
}

#' Check sampling rate by group
#'
#' @param data A Gazepoint all-gaze data frame.
#' @param group_cols Character vector of grouping columns, usually `MEDIA_ID`
#' or `c("participant_id", "MEDIA_ID")`.
#' @param time_col Name of elapsed-time column.
#'
#' @return A tibble with sample interval and estimated Hz.
#' @export
check_sampling_rate <- function(data, group_cols = "MEDIA_ID", time_col = "TIME") {
  data <- standardise_gazepoint_names(data)

  if (!time_col %in% names(data)) {
    rlang::abort(paste0("Column `", time_col, "` was not found."))
  }

  missing_groups <- setdiff(group_cols, names(data))

  if (length(missing_groups) > 0) {
    rlang::abort(
      paste0("Missing grouping columns: ", paste(missing_groups, collapse = ", "))
    )
  }

  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), .data[[time_col]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_samples = dplyr::n(),
      duration_sec = max(.data[[time_col]], na.rm = TRUE) -
        min(.data[[time_col]], na.rm = TRUE),
      mean_interval_ms = mean(diff(.data[[time_col]]), na.rm = TRUE) * 1000,
      median_interval_ms = stats::median(diff(.data[[time_col]]), na.rm = TRUE) * 1000,
      sd_interval_ms = stats::sd(diff(.data[[time_col]]), na.rm = TRUE) * 1000,
      estimated_hz = 1000 / mean_interval_ms,
      .groups = "drop"
    )
}

#' Summarise Gazepoint tracking quality
#'
#' @param data A Gazepoint data frame.
#' @param group_cols Grouping columns.
#'
#' @return A tibble with validity percentages for available validity columns.
#' @export
summarise_tracking_quality <- function(data, group_cols = "MEDIA_ID") {
  data <- standardise_gazepoint_names(data)

  missing_groups <- setdiff(group_cols, names(data))

  if (length(missing_groups) > 0) {
    rlang::abort(
      paste0("Missing grouping columns: ", paste(missing_groups, collapse = ", "))
    )
  }

  validity_cols <- intersect(
    c(
      "FPOGV", "BPOGV", "LPV", "RPV", "LPMMV", "RPMMV",
      "DIALV", "GSRV", "HRV", "TTLV"
    ),
    names(data)
  )

  if (length(validity_cols) == 0) {
    rlang::abort("No known Gazepoint validity columns were found.")
  }

  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(validity_cols),
        ~ mean(.x == 1, na.rm = TRUE) * 100,
        .names = "{.col}_valid_pct"
      ),
      .groups = "drop"
    )
}

#' Read a Gazepoint Analysis Data Summary export
#'
#' Parses the multi-section `Data_Summary_export_*.csv` file into metadata,
#' `aoi_summary`, and `aoi_by_user` tables.
#'
#' @param path Path to `Data_Summary_export_*.csv`.
#'
#' @return A list with `metadata`, `aoi_summary`, and `aoi_by_user`.
#' @export
read_gazepoint_summary <- function(path) {
  if (!file.exists(path)) {
    rlang::abort(paste0(path, " does not exist."))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

  parse_block <- function(marker) {
    idx <- which(lines == marker)

    if (length(idx) == 0) {
      return(tibble::tibble())
    }

    header <- lines[idx + 1]
    body <- character()
    i <- idx + 2

    while (i <= length(lines) && nzchar(lines[i])) {
      body <- c(body, lines[i])
      i <- i + 1
    }

    txt <- paste(c(header, body), collapse = "\n")

    out <- readr::read_csv(
      I(txt),
      show_col_types = FALSE,
      trim_ws = TRUE,
      name_repair = "minimal"
    )

    .drop_empty_gazepoint_columns(out)
  }

  version <- if (length(lines) >= 1) {
    strsplit(lines[1], ",", fixed = TRUE)[[1]][2]
  } else {
    NA_character_
  }

  processed_on <- if (length(lines) >= 2) {
    sub("^Processed on,", "", lines[2])
  } else {
    NA_character_
  }

  list(
    metadata = tibble::tibble(
      source_file = basename(path),
      gazepoint_analysis_version = version,
      processed_on = processed_on
    ),
    aoi_summary = parse_block("AOI Summary"),
    aoi_by_user = parse_block("AOI Statistics (for each user)")
  )
}

#' Summarise sample-level AOI viewing
#'
#' Computes transparent AOI metrics from sample-level rows. These may not exactly
#' reproduce Gazepoint Analysis summary metrics; use `read_gazepoint_summary()`
#' when official Gazepoint summary values are available.
#'
#' @param data A Gazepoint all-gaze data frame.
#' @param group_cols Grouping columns.
#' @param aoi_col AOI column name.
#' @param time_col Time column name.
#'
#' @return A tibble with AOI sample count, TTFF, and approximate dwell time.
#' @export
summarise_aoi_samples <- function(
    data,
    group_cols = "MEDIA_ID",
    aoi_col = "AOI",
    time_col = "TIME"
) {
  data <- standardise_gazepoint_names(data)

  needed <- c(group_cols, aoi_col, time_col)
  missing <- setdiff(needed, names(data))

  if (length(missing) > 0) {
    rlang::abort(paste0("Missing columns: ", paste(missing, collapse = ", ")))
  }

  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), .data[[time_col]]) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      .dt_next = dplyr::lead(.data[[time_col]]) - .data[[time_col]],
      .dt_next = dplyr::if_else(
        is.na(.dt_next),
        stats::median(.dt_next, na.rm = TRUE),
        .dt_next
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data[[aoi_col]]), .data[[aoi_col]] != "") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, aoi_col)))) |>
    dplyr::summarise(
      time_to_first_view_sec = min(.data[[time_col]], na.rm = TRUE),
      aoi_sample_count = dplyr::n(),
      approx_time_viewed_sec = sum(.dt_next, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Summarise fixation-level AOI metrics
#'
#' @param data A Gazepoint fixation data frame.
#' @param group_cols Grouping columns.
#' @param aoi_col AOI column name.
#'
#' @return A tibble with fixation counts and summed fixation duration by AOI.
#' @export
summarise_fixations <- function(
    data,
    group_cols = "MEDIA_ID",
    aoi_col = "AOI"
) {
  data <- standardise_gazepoint_names(data)

  needed <- c(group_cols, aoi_col, "FPOGD", "FPOGS")
  missing <- setdiff(needed, names(data))

  if (length(missing) > 0) {
    rlang::abort(paste0("Missing columns: ", paste(missing, collapse = ", ")))
  }

  data |>
    dplyr::filter(!is.na(.data[[aoi_col]]), .data[[aoi_col]] != "") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, aoi_col)))) |>
    dplyr::summarise(
      fixation_count = dplyr::n(),
      fixation_duration_sum_sec = sum(.data[["FPOGD"]], na.rm = TRUE),
      fixation_duration_mean_ms = mean(.data[["FPOGD"]], na.rm = TRUE) * 1000,
      fixation_ttff_sec = min(.data[["FPOGS"]], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Compute an AOI transition matrix
#'
#' @param data A Gazepoint data frame with AOI labels.
#' @param group_cols Columns defining independent sequences.
#' @param aoi_col AOI column name.
#' @param time_col Time column name.
#' @param collapse_repeats If `TRUE`, consecutive identical AOI labels are reduced
#' to one visit before transitions are counted.
#'
#' @return A tibble with `from`, `to`, `n`, and `prob`.
#' @export
compute_transition_matrix <- function(
    data,
    group_cols = "MEDIA_ID",
    aoi_col = "AOI",
    time_col = "TIME",
    collapse_repeats = TRUE
) {
  data <- standardise_gazepoint_names(data)

  needed <- c(group_cols, aoi_col, time_col)
  missing <- setdiff(needed, names(data))

  if (length(missing) > 0) {
    rlang::abort(paste0("Missing columns: ", paste(missing, collapse = ", ")))
  }

  visits <- data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), .data[[time_col]]) |>
    dplyr::filter(!is.na(.data[[aoi_col]]), .data[[aoi_col]] != "")

  if (collapse_repeats) {
    visits <- visits |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::mutate(.prev_aoi = dplyr::lag(.data[[aoi_col]])) |>
      dplyr::filter(is.na(.prev_aoi) | .data[[aoi_col]] != .prev_aoi) |>
      dplyr::ungroup()
  }

  visits |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      to = dplyr::lead(.data[[aoi_col]]),
      from = .data[[aoi_col]]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(to)) |>
    dplyr::count(dplyr::across(dplyr::all_of(group_cols)), from, to, name = "n") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "from")))) |>
    dplyr::mutate(prob = n / sum(n)) |>
    dplyr::ungroup()
}

#' Plot an AOI transition heatmap
#'
#' @param transitions Output of `compute_transition_matrix()`.
#'
#' @return A ggplot object.
#' @export
plot_transition_heatmap <- function(transitions) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Package `ggplot2` is required for plotting.")
  }

  if (nrow(transitions) == 0) {
    rlang::abort("No AOI transitions detected. The transition table is empty.")
  }

  ggplot2::ggplot(transitions, ggplot2::aes(x = to, y = from, fill = prob)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", prob))) +
    ggplot2::labs(x = "Next AOI", y = "Current AOI", fill = "Probability") +
    ggplot2::theme_minimal()
}
