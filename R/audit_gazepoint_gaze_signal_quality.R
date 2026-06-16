#' Audit Gazepoint gaze-signal quality
#'
#' Create a publication-level audit of gaze coordinate availability, validity,
#' off-screen samples, and optional pupil availability.
#'
#' @param data A sample-level Gazepoint data frame.
#' @param subject_col Subject/participant identifier column.
#' @param condition_col Optional condition column.
#' @param group_cols Columns defining a recording, stimulus, trial, or analysis
#'   unit.
#' @param x_col Optional gaze-x coordinate column. If `NULL`, common Gazepoint
#'   aliases are detected.
#' @param y_col Optional gaze-y coordinate column. If `NULL`, common Gazepoint
#'   aliases are detected.
#' @param validity_cols Optional gaze-validity columns. If `NULL`, common
#'   Gazepoint validity columns are detected.
#' @param pupil_col Optional pupil column. If `NULL`, common pupil aliases are
#'   detected.
#' @param screen_x_range Numeric length-2 vector defining plausible on-screen x
#'   range.
#' @param screen_y_range Numeric length-2 vector defining plausible on-screen y
#'   range.
#' @param min_gaze_valid_prop Minimum acceptable gaze-validity proportion.
#' @param max_missing_gaze_prop Maximum acceptable missing-gaze proportion.
#' @param max_offscreen_prop Maximum acceptable off-screen proportion.
#' @param min_pupil_valid_prop Minimum acceptable valid-pupil proportion when a
#'   pupil column is available.
#'
#' @return A list with class `gp3_gaze_signal_quality_audit` containing
#'   overview, unit_summary, subject_summary, condition_summary,
#'   signal_issue_summary, flagged_units, and settings tables.
#' @export
audit_gazepoint_gaze_signal_quality <- function(
    data,
    subject_col = "subject",
    condition_col = NULL,
    group_cols = c("subject", "media_id", "trial_global"),
    x_col = NULL,
    y_col = NULL,
    validity_cols = NULL,
    pupil_col = NULL,
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    min_gaze_valid_prop = 0.70,
    max_missing_gaze_prop = 0.30,
    max_offscreen_prop = 0.30,
    min_pupil_valid_prop = 0.70
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_gaze_signal_standardise_aliases(data)

  subject_col <- .gp3_gaze_signal_resolve_col(
    subject_col,
    names(data),
    "subject_col"
  )

  condition_col <- .gp3_gaze_signal_resolve_optional_col(
    condition_col,
    names(data),
    "condition_col"
  )

  group_cols <- .gp3_gaze_signal_standardise_cols(group_cols)
  group_cols <- group_cols[group_cols %in% names(data)]

  if (length(group_cols) == 0L) {
    group_cols <- subject_col
  }

  x_col <- .gp3_gaze_signal_resolve_or_detect_col(
    x_col,
    names(data),
    "x_col",
    candidates = c(
      "x",
      "gaze_x",
      "mean_x",
      "FPOGX",
      "BPOGX",
      "LPOGX",
      "RPOGX",
      "CX"
    )
  )

  y_col <- .gp3_gaze_signal_resolve_or_detect_col(
    y_col,
    names(data),
    "y_col",
    candidates = c(
      "y",
      "gaze_y",
      "mean_y",
      "FPOGY",
      "BPOGY",
      "LPOGY",
      "RPOGY",
      "CY"
    )
  )

  pupil_col <- .gp3_gaze_signal_resolve_or_detect_col(
    pupil_col,
    names(data),
    "pupil_col",
    candidates = c(
      "pupil",
      "pupil_left",
      "pupil_right",
      "pupil_clean",
      "pupil_interpolated",
      "pupil_smoothed",
      "LPD",
      "RPD",
      "BPD"
    )
  )

  validity_cols <- .gp3_gaze_signal_resolve_validity_cols(
    validity_cols,
    names(data)
  )

  .gp3_gaze_signal_check_range(screen_x_range, "screen_x_range")
  .gp3_gaze_signal_check_range(screen_y_range, "screen_y_range")

  .gp3_gaze_signal_check_prop(
    min_gaze_valid_prop,
    "min_gaze_valid_prop"
  )

  .gp3_gaze_signal_check_prop(
    max_missing_gaze_prop,
    "max_missing_gaze_prop"
  )

  .gp3_gaze_signal_check_prop(
    max_offscreen_prop,
    "max_offscreen_prop"
  )

  .gp3_gaze_signal_check_prop(
    min_pupil_valid_prop,
    "min_pupil_valid_prop"
  )

  unit_summary <- .gp3_gaze_signal_create_unit_summary(
    data = data,
    subject_col = subject_col,
    condition_col = condition_col,
    group_cols = group_cols,
    x_col = x_col,
    y_col = y_col,
    validity_cols = validity_cols,
    pupil_col = pupil_col,
    screen_x_range = screen_x_range,
    screen_y_range = screen_y_range,
    min_gaze_valid_prop = min_gaze_valid_prop,
    max_missing_gaze_prop = max_missing_gaze_prop,
    max_offscreen_prop = max_offscreen_prop,
    min_pupil_valid_prop = min_pupil_valid_prop
  )

  subject_summary <- .gp3_gaze_signal_create_subject_summary(
    unit_summary = unit_summary,
    subject_col = subject_col
  )

  condition_summary <- .gp3_gaze_signal_create_condition_summary(
    unit_summary = unit_summary,
    condition_col = condition_col
  )

  signal_issue_summary <- .gp3_gaze_signal_create_issue_summary(
    unit_summary
  )

  flagged_units <- unit_summary[
    unit_summary$gaze_signal_status != "ok",
    ,
    drop = FALSE
  ]

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_units = nrow(unit_summary),
    n_subjects = length(unique(unit_summary[[subject_col]])),
    n_flagged_units = nrow(flagged_units),
    x_col = .gp3_gaze_signal_collapse_nullable(x_col),
    y_col = .gp3_gaze_signal_collapse_nullable(y_col),
    validity_cols = .gp3_gaze_signal_collapse_nullable(validity_cols),
    pupil_col = .gp3_gaze_signal_collapse_nullable(pupil_col),
    has_gaze_coordinates = .gp3_gaze_signal_has_col(x_col) && .gp3_gaze_signal_has_col(y_col),
    has_validity_cols = length(validity_cols) > 0L,
    has_pupil_col = .gp3_gaze_signal_has_col(pupil_col),
    gaze_signal_quality_status = dplyr::case_when(
      is.null(x_col) && is.null(y_col) && length(validity_cols) == 0L ~
        "gaze_columns_not_available",
      nrow(flagged_units) == 0L ~ "ok",
      TRUE ~ "review"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "subject_col",
      "condition_col",
      "group_cols",
      "x_col",
      "y_col",
      "validity_cols",
      "pupil_col",
      "screen_x_range",
      "screen_y_range",
      "min_gaze_valid_prop",
      "max_missing_gaze_prop",
      "max_offscreen_prop",
      "min_pupil_valid_prop"
    ),
    value = c(
      subject_col,
      .gp3_gaze_signal_collapse_nullable(condition_col),
      paste(group_cols, collapse = ", "),
      .gp3_gaze_signal_collapse_nullable(x_col),
      .gp3_gaze_signal_collapse_nullable(y_col),
      .gp3_gaze_signal_collapse_nullable(validity_cols),
      .gp3_gaze_signal_collapse_nullable(pupil_col),
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      as.character(min_gaze_valid_prop),
      as.character(max_missing_gaze_prop),
      as.character(max_offscreen_prop),
      as.character(min_pupil_valid_prop)
    )
  )

  out <- list(
    overview = overview,
    unit_summary = unit_summary,
    subject_summary = subject_summary,
    condition_summary = condition_summary,
    signal_issue_summary = signal_issue_summary,
    flagged_units = flagged_units,
    settings = settings
  )

  class(out) <- c("gp3_gaze_signal_quality_audit", "list")

  out
}

.gp3_gaze_signal_create_unit_summary <- function(
    data,
    subject_col,
    condition_col,
    group_cols,
    x_col,
    y_col,
    validity_cols,
    pupil_col,
    screen_x_range,
    screen_y_range,
    min_gaze_valid_prop,
    max_missing_gaze_prop,
    max_offscreen_prop,
    min_pupil_valid_prop
) {
  id_cols <- unique(c(group_cols, condition_col))
  id_cols <- id_cols[!is.na(id_cols) & nzchar(id_cols)]

  split_key <- interaction(data[id_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(data)), split_key)

  rows <- vector("list", length(split_idx))

  for (i in seq_along(split_idx)) {
    idx <- split_idx[[i]]
    d <- data[idx, , drop = FALSE]

    x <- .gp3_gaze_signal_numeric_col(d, x_col)
    y <- .gp3_gaze_signal_numeric_col(d, y_col)
    pupil <- .gp3_gaze_signal_numeric_col(d, pupil_col)

    has_xy <- .gp3_gaze_signal_has_col(x_col) && .gp3_gaze_signal_has_col(y_col) && !is.null(x) && !is.null(y)
    has_validity <- length(validity_cols) > 0L
    has_pupil <- .gp3_gaze_signal_has_col(pupil_col) && !is.null(pupil)

    missing_gaze <- rep(NA, nrow(d))
    offscreen_gaze <- rep(NA, nrow(d))
    valid_gaze <- rep(NA, nrow(d))

    if (has_xy) {
      missing_gaze <- !is.finite(x) | !is.finite(y)
      offscreen_gaze <- is.finite(x) &
        is.finite(y) &
        (
          x < screen_x_range[[1]] |
            x > screen_x_range[[2]] |
            y < screen_y_range[[1]] |
            y > screen_y_range[[2]]
        )
      valid_gaze <- !missing_gaze & !offscreen_gaze
    }

    if (has_validity) {
      validity_matrix <- vapply(
        validity_cols,
        function(col) .gp3_gaze_signal_validity_vector(d[[col]]),
        logical(nrow(d))
      )

      if (is.null(dim(validity_matrix))) {
        validity_matrix <- matrix(validity_matrix, ncol = 1L)
      }

      valid_from_cols <- rowSums(validity_matrix, na.rm = TRUE) > 0L
      validity_available <- rowSums(!is.na(validity_matrix)) > 0L
      valid_from_cols[!validity_available] <- NA

      valid_gaze <- valid_from_cols

      if (has_xy) {
        valid_gaze <- valid_gaze & !missing_gaze & !offscreen_gaze
      }
    }

    pupil_valid <- rep(NA, nrow(d))

    if (has_pupil) {
      pupil_valid <- is.finite(pupil) & pupil > 0
    }

    n_samples <- nrow(d)

    n_valid_gaze <- sum(valid_gaze %in% TRUE, na.rm = TRUE)
    n_missing_gaze <- sum(missing_gaze %in% TRUE, na.rm = TRUE)
    n_offscreen_gaze <- sum(offscreen_gaze %in% TRUE, na.rm = TRUE)
    n_valid_pupil <- sum(pupil_valid %in% TRUE, na.rm = TRUE)

    gaze_valid_prop <- ifelse(any(!is.na(valid_gaze)), n_valid_gaze / n_samples, NA_real_)
    missing_gaze_prop <- ifelse(any(!is.na(missing_gaze)), n_missing_gaze / n_samples, NA_real_)
    offscreen_prop <- ifelse(any(!is.na(offscreen_gaze)), n_offscreen_gaze / n_samples, NA_real_)
    pupil_valid_prop <- ifelse(any(!is.na(pupil_valid)), n_valid_pupil / n_samples, NA_real_)

    status <- .gp3_gaze_signal_unit_status(
      has_xy = has_xy,
      has_validity = has_validity,
      has_pupil = has_pupil,
      gaze_valid_prop = gaze_valid_prop,
      missing_gaze_prop = missing_gaze_prop,
      offscreen_prop = offscreen_prop,
      pupil_valid_prop = pupil_valid_prop,
      min_gaze_valid_prop = min_gaze_valid_prop,
      max_missing_gaze_prop = max_missing_gaze_prop,
      max_offscreen_prop = max_offscreen_prop,
      min_pupil_valid_prop = min_pupil_valid_prop
    )

    id_row <- d[1, id_cols, drop = FALSE]

    rows[[i]] <- cbind(
      tibble::as_tibble(id_row),
      tibble::tibble(
        n_samples = n_samples,
        n_valid_gaze = n_valid_gaze,
        gaze_valid_prop = gaze_valid_prop,
        n_missing_gaze = n_missing_gaze,
        missing_gaze_prop = missing_gaze_prop,
        n_offscreen_gaze = n_offscreen_gaze,
        offscreen_prop = offscreen_prop,
        n_valid_pupil = ifelse(has_pupil, n_valid_pupil, NA_integer_),
        pupil_valid_prop = pupil_valid_prop,
        gaze_signal_status = status
      )
    )
  }

  dplyr::bind_rows(rows)
}

.gp3_gaze_signal_unit_status <- function(
    has_xy,
    has_validity,
    has_pupil,
    gaze_valid_prop,
    missing_gaze_prop,
    offscreen_prop,
    pupil_valid_prop,
    min_gaze_valid_prop,
    max_missing_gaze_prop,
    max_offscreen_prop,
    min_pupil_valid_prop
) {
  if (!has_xy && !has_validity) {
    return("gaze_columns_not_available")
  }

  if (!is.na(gaze_valid_prop) && gaze_valid_prop < min_gaze_valid_prop) {
    return("low_gaze_validity")
  }

  if (!is.na(missing_gaze_prop) && missing_gaze_prop > max_missing_gaze_prop) {
    return("high_missing_gaze")
  }

  if (!is.na(offscreen_prop) && offscreen_prop > max_offscreen_prop) {
    return("high_offscreen_gaze")
  }

  if (has_pupil &&
      !is.na(pupil_valid_prop) &&
      pupil_valid_prop < min_pupil_valid_prop) {
    return("low_pupil_validity")
  }

  "ok"
}

.gp3_gaze_signal_create_subject_summary <- function(
    unit_summary,
    subject_col
) {
  split_idx <- split(
    seq_len(nrow(unit_summary)),
    unit_summary[[subject_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- unit_summary[idx, , drop = FALSE]

    tibble::tibble(
      subject = as.character(d[[subject_col]][[1]]),
      n_units = nrow(d),
      n_flagged_units = sum(d$gaze_signal_status != "ok", na.rm = TRUE),
      mean_gaze_valid_prop = mean(d$gaze_valid_prop, na.rm = TRUE),
      mean_missing_gaze_prop = mean(d$missing_gaze_prop, na.rm = TRUE),
      mean_offscreen_prop = mean(d$offscreen_prop, na.rm = TRUE),
      mean_pupil_valid_prop = mean(d$pupil_valid_prop, na.rm = TRUE),
      subject_signal_status = ifelse(
        any(d$gaze_signal_status != "ok", na.rm = TRUE),
        "review",
        "ok"
      )
    )
  })

  out <- dplyr::bind_rows(rows)
  names(out)[names(out) == "subject"] <- subject_col
  out
}

.gp3_gaze_signal_create_condition_summary <- function(
    unit_summary,
    condition_col
) {
  if (is.null(condition_col) || !condition_col %in% names(unit_summary)) {
    return(tibble::tibble(
      condition = character(),
      n_units = integer(),
      n_flagged_units = integer(),
      mean_gaze_valid_prop = numeric(),
      mean_missing_gaze_prop = numeric(),
      mean_offscreen_prop = numeric(),
      mean_pupil_valid_prop = numeric(),
      condition_signal_status = character()
    ))
  }

  split_idx <- split(
    seq_len(nrow(unit_summary)),
    unit_summary[[condition_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- unit_summary[idx, , drop = FALSE]

    tibble::tibble(
      condition = as.character(d[[condition_col]][[1]]),
      n_units = nrow(d),
      n_flagged_units = sum(d$gaze_signal_status != "ok", na.rm = TRUE),
      mean_gaze_valid_prop = mean(d$gaze_valid_prop, na.rm = TRUE),
      mean_missing_gaze_prop = mean(d$missing_gaze_prop, na.rm = TRUE),
      mean_offscreen_prop = mean(d$offscreen_prop, na.rm = TRUE),
      mean_pupil_valid_prop = mean(d$pupil_valid_prop, na.rm = TRUE),
      condition_signal_status = ifelse(
        any(d$gaze_signal_status != "ok", na.rm = TRUE),
        "review",
        "ok"
      )
    )
  })

  out <- dplyr::bind_rows(rows)
  names(out)[names(out) == "condition"] <- condition_col
  out
}

.gp3_gaze_signal_create_issue_summary <- function(unit_summary) {
  tab <- as.data.frame(
    table(gaze_signal_status = unit_summary$gaze_signal_status),
    stringsAsFactors = FALSE
  )

  tibble::tibble(
    gaze_signal_status = as.character(tab$gaze_signal_status),
    n_units = as.integer(tab$Freq),
    unit_prop = as.integer(tab$Freq) / nrow(unit_summary)
  )
}

.gp3_gaze_signal_numeric_col <- function(data, col) {
  if (!.gp3_gaze_signal_has_col(col)) {
    return(NULL)
  }

  suppressWarnings(as.numeric(data[[col]]))
}

.gp3_gaze_signal_has_col <- function(col) {
  !is.null(col) &&
    length(col) == 1L &&
    !is.na(col) &&
    nzchar(col)
}

.gp3_gaze_signal_validity_vector <- function(x) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[x > 0] <- TRUE
    out[x <= 0] <- FALSE
    return(out)
  }

  values <- tolower(as.character(x))
  out <- rep(NA, length(values))

  true_values <- c("true", "t", "yes", "y", "1", "valid", "ok")
  false_values <- c("false", "f", "no", "n", "0", "invalid", "missing", "bad")

  out[values %in% true_values] <- TRUE
  out[values %in% false_values] <- FALSE

  out
}

.gp3_gaze_signal_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_gaze_signal_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols[cols == "USER_FILE"] <- "subject"
  cols
}

.gp3_gaze_signal_resolve_col <- function(col, names_data, arg) {
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

.gp3_gaze_signal_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_gaze_signal_resolve_col(col, names_data, arg)
}

.gp3_gaze_signal_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates
) {
  if (!is.null(col)) {
    return(.gp3_gaze_signal_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) == 0L) {
    return(NULL)
  }

  found[[1]]
}

.gp3_gaze_signal_resolve_validity_cols <- function(validity_cols, names_data) {
  if (!is.null(validity_cols)) {
    if (!is.character(validity_cols) ||
        length(validity_cols) == 0L ||
        any(is.na(validity_cols)) ||
        any(!nzchar(validity_cols))) {
      stop("`validity_cols` must be a non-empty character vector.", call. = FALSE)
    }

    missing_cols <- setdiff(validity_cols, names_data)

    if (length(missing_cols) > 0L) {
      stop("All `validity_cols` must be present in `data`.", call. = FALSE)
    }

    return(validity_cols)
  }

  candidates <- c(
    "valid_gaze",
    "gaze_valid",
    "gaze_validity",
    "FPOGV",
    "BPOGV",
    "LPOGV",
    "RPOGV",
    "LPV",
    "RPV"
  )

  candidates[candidates %in% names_data]
}

.gp3_gaze_signal_check_range <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 2L ||
      any(is.na(x)) ||
      any(!is.finite(x)) ||
      x[[1]] >= x[[2]]) {
    stop(
      "`",
      arg,
      "` must be a numeric length-2 vector with lower < upper.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_gaze_signal_check_prop <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0 ||
      x > 1) {
    stop("`", arg, "` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_gaze_signal_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
