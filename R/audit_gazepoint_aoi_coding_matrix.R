#' Audit AOI coding against geometry
#'
#' Validate observed sample-level AOI labels against AOI labels derived from gaze
#' coordinates and AOI geometry.
#'
#' @param gaze_data A data frame containing gaze samples and observed AOI labels.
#' @param aoi_geometry A data frame containing AOI geometry definitions.
#' @param observed_aoi_col Observed AOI label column in `gaze_data`.
#' @param gaze_x_col Gaze x-coordinate column. If `NULL`, common aliases are
#'   detected automatically.
#' @param gaze_y_col Gaze y-coordinate column. If `NULL`, common aliases are
#'   detected automatically.
#' @param gaze_stimulus_col Optional gaze stimulus/media column.
#' @param sample_id_cols Optional columns to carry into the sample-level coding
#'   audit table.
#' @param geometry_aoi_col AOI label/name column in `aoi_geometry`.
#' @param geometry_stimulus_col Optional stimulus/media column in
#'   `aoi_geometry`.
#' @param x_min_col Optional AOI left/x-min column.
#' @param y_min_col Optional AOI top/y-min column.
#' @param x_max_col Optional AOI right/x-max column.
#' @param y_max_col Optional AOI bottom/y-max column.
#' @param x_col Optional AOI left/x column used with `width_col`.
#' @param y_col Optional AOI top/y column used with `height_col`.
#' @param width_col Optional AOI width column.
#' @param height_col Optional AOI height column.
#' @param screen_x_range Numeric length-2 vector defining the screen x range.
#' @param screen_y_range Numeric length-2 vector defining the screen y range.
#' @param tie_method How to handle samples falling in multiple AOIs. Use
#'   `"ambiguous"` to label them as ambiguous or `"first"` to use the first
#'   matching AOI.
#' @param outside_label Label used for samples outside all AOIs.
#' @param ambiguous_label Label used for samples inside multiple AOIs when
#'   `tie_method = "ambiguous"`.
#' @param missing_label Label used for samples with missing/non-finite gaze
#'   coordinates.
#' @param observed_outside_values Character values in `observed_aoi_col` treated
#'   as outside/non-AOI labels.
#' @param max_mismatch_prop Maximum acceptable proportion of comparable samples
#'   where observed and geometry-derived AOI labels differ.
#' @param max_ambiguous_prop Maximum acceptable proportion of samples with
#'   ambiguous geometry-derived AOI assignment.
#' @param max_missing_coordinate_prop Maximum acceptable proportion of samples
#'   with missing/non-finite gaze coordinates.
#' @param ignore_invalid_geometry Logical. If `TRUE`, AOIs with invalid
#'   coordinates or dimensions are excluded before coding validation.
#'
#' @return A list with class `gp3_aoi_coding_matrix_audit` containing overview,
#'   geometry_summary, sample_coding, coding_matrix, observed_summary,
#'   derived_summary, flagged_samples, and settings tables.
#' @export
audit_gazepoint_aoi_coding_matrix <- function(
    gaze_data,
    aoi_geometry,
    observed_aoi_col = NULL,
    gaze_x_col = NULL,
    gaze_y_col = NULL,
    gaze_stimulus_col = NULL,
    sample_id_cols = NULL,
    geometry_aoi_col = NULL,
    geometry_stimulus_col = NULL,
    x_min_col = NULL,
    y_min_col = NULL,
    x_max_col = NULL,
    y_max_col = NULL,
    x_col = NULL,
    y_col = NULL,
    width_col = NULL,
    height_col = NULL,
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    tie_method = c("ambiguous", "first"),
    outside_label = "outside",
    ambiguous_label = "ambiguous",
    missing_label = "missing_coordinate",
    observed_outside_values = c(
      "outside", "none", "no_aoi", "non_aoi", "background", "off_aoi"
    ),
    max_mismatch_prop = 0.05,
    max_ambiguous_prop = 0.05,
    max_missing_coordinate_prop = 0.20,
    ignore_invalid_geometry = TRUE
) {
  if (!is.data.frame(gaze_data)) {
    stop("`gaze_data` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(aoi_geometry)) {
    stop("`aoi_geometry` must be a data frame.", call. = FALSE)
  }

  if (nrow(gaze_data) == 0L) {
    stop("`gaze_data` must contain at least one row.", call. = FALSE)
  }

  if (nrow(aoi_geometry) == 0L) {
    stop("`aoi_geometry` must contain at least one row.", call. = FALSE)
  }

  tie_method <- match.arg(tie_method)

  gaze_data <- .gp3_aoi_coding_standardise_gaze_aliases(gaze_data)

  observed_aoi_col <- .gp3_aoi_coding_resolve_or_detect_col(
    col = observed_aoi_col,
    names_data = names(gaze_data),
    arg = "observed_aoi_col",
    candidates = c("observed_aoi", "observed_aoi_label", "coded_aoi", "aoi", "AOI", "aoi_current", "aoi_label", "AOI_LABEL"),
    required = TRUE
  )

  gaze_x_col <- .gp3_aoi_coding_resolve_or_detect_col(
    col = gaze_x_col,
    names_data = names(gaze_data),
    arg = "gaze_x_col",
    candidates = c("x", "X", "gaze_x", "gaze_x_norm", "FPOGX", "BPOGX"),
    required = TRUE
  )

  gaze_y_col <- .gp3_aoi_coding_resolve_or_detect_col(
    col = gaze_y_col,
    names_data = names(gaze_data),
    arg = "gaze_y_col",
    candidates = c("y", "Y", "gaze_y", "gaze_y_norm", "FPOGY", "BPOGY"),
    required = TRUE
  )

  gaze_stimulus_col <- .gp3_aoi_coding_resolve_or_detect_col(
    col = gaze_stimulus_col,
    names_data = names(gaze_data),
    arg = "gaze_stimulus_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id"),
    required = FALSE
  )

  sample_id_cols <- .gp3_aoi_coding_standardise_cols(sample_id_cols)
  sample_id_cols <- sample_id_cols[sample_id_cols %in% names(gaze_data)]

  .gp3_aoi_coding_check_label(outside_label, "outside_label")
  .gp3_aoi_coding_check_label(ambiguous_label, "ambiguous_label")
  .gp3_aoi_coding_check_label(missing_label, "missing_label")
  .gp3_aoi_coding_check_character_vector(
    observed_outside_values,
    "observed_outside_values"
  )
  .gp3_aoi_coding_check_prop(max_mismatch_prop, "max_mismatch_prop")
  .gp3_aoi_coding_check_prop(max_ambiguous_prop, "max_ambiguous_prop")
  .gp3_aoi_coding_check_prop(
    max_missing_coordinate_prop,
    "max_missing_coordinate_prop"
  )
  .gp3_aoi_coding_check_logical_scalar(
    ignore_invalid_geometry,
    "ignore_invalid_geometry"
  )

  geometry_audit <- audit_gazepoint_aoi_geometry(
    data = aoi_geometry,
    aoi_col = geometry_aoi_col,
    stimulus_col = geometry_stimulus_col,
    x_min_col = x_min_col,
    y_min_col = y_min_col,
    x_max_col = x_max_col,
    y_max_col = y_max_col,
    x_col = x_col,
    y_col = y_col,
    width_col = width_col,
    height_col = height_col,
    screen_x_range = screen_x_range,
    screen_y_range = screen_y_range,
    require_within_screen = FALSE
  )

  geometry_summary <- geometry_audit$geometry_summary

  resolved_geometry_aoi_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "aoi_col"
  ]

  resolved_geometry_stimulus_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "stimulus_col"
  ]

  if (is.na(resolved_geometry_stimulus_col) ||
      !nzchar(resolved_geometry_stimulus_col)) {
    resolved_geometry_stimulus_col <- NULL
  }

  if (!is.null(resolved_geometry_stimulus_col) &&
      is.null(gaze_stimulus_col) &&
      length(unique(geometry_summary[[resolved_geometry_stimulus_col]])) > 1L) {
    stop(
      "`gaze_stimulus_col` is required when `aoi_geometry` contains multiple stimuli.",
      call. = FALSE
    )
  }

  if (isTRUE(ignore_invalid_geometry)) {
    geometry_for_coding <- geometry_summary[
      !geometry_summary$aoi_geometry_status %in%
        c("invalid_coordinate", "invalid_dimension"),
      ,
      drop = FALSE
    ]
  } else {
    geometry_for_coding <- geometry_summary
  }

  sample_coding <- .gp3_aoi_coding_create_sample_coding(
    gaze_data = gaze_data,
    geometry_summary = geometry_for_coding,
    observed_aoi_col = observed_aoi_col,
    gaze_x_col = gaze_x_col,
    gaze_y_col = gaze_y_col,
    gaze_stimulus_col = gaze_stimulus_col,
    geometry_aoi_col = resolved_geometry_aoi_col,
    geometry_stimulus_col = resolved_geometry_stimulus_col,
    sample_id_cols = sample_id_cols,
    tie_method = tie_method,
    outside_label = outside_label,
    ambiguous_label = ambiguous_label,
    missing_label = missing_label,
    observed_outside_values = observed_outside_values
  )

  coding_matrix <- .gp3_aoi_coding_create_matrix(sample_coding)
  observed_summary <- .gp3_aoi_coding_create_observed_summary(sample_coding)
  derived_summary <- .gp3_aoi_coding_create_derived_summary(sample_coding)

  flagged_samples <- sample_coding[
    sample_coding$aoi_coding_status != "ok",
    ,
    drop = FALSE
  ]

  n_comparable <- sum(sample_coding$comparable_sample, na.rm = TRUE)
  n_mismatched <- sum(
    sample_coding$aoi_coding_status == "mismatch",
    na.rm = TRUE
  )
  n_ambiguous <- sum(
    sample_coding$derived_assignment_status == "ambiguous_aoi",
    na.rm = TRUE
  )
  n_missing_coordinate <- sum(
    sample_coding$derived_assignment_status == "missing_coordinate",
    na.rm = TRUE
  )

  mismatch_prop <- ifelse(
    n_comparable > 0L,
    n_mismatched / n_comparable,
    NA_real_
  )

  ambiguous_prop <- n_ambiguous / nrow(sample_coding)
  missing_coordinate_prop <- n_missing_coordinate / nrow(sample_coding)

  overview_status <- .gp3_aoi_coding_overview_status(
    mismatch_prop = mismatch_prop,
    ambiguous_prop = ambiguous_prop,
    missing_coordinate_prop = missing_coordinate_prop,
    max_mismatch_prop = max_mismatch_prop,
    max_ambiguous_prop = max_ambiguous_prop,
    max_missing_coordinate_prop = max_missing_coordinate_prop
  )

  overview <- tibble::tibble(
    n_gaze_rows = nrow(gaze_data),
    n_geometry_rows = nrow(aoi_geometry),
    n_aois = nrow(geometry_summary),
    n_aois_used = nrow(geometry_for_coding),
    n_coded_samples = nrow(sample_coding),
    n_comparable_samples = n_comparable,
    n_mismatched_samples = n_mismatched,
    mismatch_prop = mismatch_prop,
    n_ambiguous_samples = n_ambiguous,
    ambiguous_prop = ambiguous_prop,
    n_missing_coordinate_samples = n_missing_coordinate,
    missing_coordinate_prop = missing_coordinate_prop,
    n_flagged_samples = nrow(flagged_samples),
    aoi_coding_matrix_status = overview_status
  )

  settings <- tibble::tibble(
    setting = c(
      "observed_aoi_col",
      "gaze_x_col",
      "gaze_y_col",
      "gaze_stimulus_col",
      "sample_id_cols",
      "geometry_aoi_col",
      "geometry_stimulus_col",
      "screen_x_range",
      "screen_y_range",
      "tie_method",
      "outside_label",
      "ambiguous_label",
      "missing_label",
      "observed_outside_values",
      "max_mismatch_prop",
      "max_ambiguous_prop",
      "max_missing_coordinate_prop",
      "ignore_invalid_geometry"
    ),
    value = c(
      observed_aoi_col,
      gaze_x_col,
      gaze_y_col,
      .gp3_aoi_coding_collapse_nullable(gaze_stimulus_col),
      .gp3_aoi_coding_collapse_nullable(sample_id_cols),
      resolved_geometry_aoi_col,
      .gp3_aoi_coding_collapse_nullable(resolved_geometry_stimulus_col),
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      tie_method,
      outside_label,
      ambiguous_label,
      missing_label,
      paste(observed_outside_values, collapse = ", "),
      as.character(max_mismatch_prop),
      as.character(max_ambiguous_prop),
      as.character(max_missing_coordinate_prop),
      as.character(ignore_invalid_geometry)
    )
  )

  out <- list(
    overview = overview,
    geometry_summary = geometry_summary,
    sample_coding = sample_coding,
    coding_matrix = coding_matrix,
    observed_summary = observed_summary,
    derived_summary = derived_summary,
    flagged_samples = flagged_samples,
    settings = settings
  )

  class(out) <- c("gp3_aoi_coding_matrix_audit", "list")

  out
}

.gp3_aoi_coding_create_sample_coding <- function(
    gaze_data,
    geometry_summary,
    observed_aoi_col,
    gaze_x_col,
    gaze_y_col,
    gaze_stimulus_col,
    geometry_aoi_col,
    geometry_stimulus_col,
    sample_id_cols,
    tie_method,
    outside_label,
    ambiguous_label,
    missing_label,
    observed_outside_values
) {
  base_id <- tibble::tibble(
    .gp3_sample_index = seq_len(nrow(gaze_data))
  )

  if (length(sample_id_cols) > 0L) {
    base_id <- cbind(
      base_id,
      tibble::as_tibble(gaze_data[, sample_id_cols, drop = FALSE])
    )
  }

  if (!is.null(gaze_stimulus_col) && !gaze_stimulus_col %in% names(base_id)) {
    base_id[[gaze_stimulus_col]] <- gaze_data[[gaze_stimulus_col]]
  }

  observed_aoi_raw <- as.character(gaze_data[[observed_aoi_col]])

  observed_aoi <- .gp3_aoi_coding_standardise_observed_aoi(
    observed_aoi_raw,
    outside_label = outside_label,
    observed_outside_values = observed_outside_values
  )

  derived <- .gp3_aoi_coding_assign_samples(
    gaze_data = gaze_data,
    geometry_summary = geometry_summary,
    gaze_x_col = gaze_x_col,
    gaze_y_col = gaze_y_col,
    gaze_stimulus_col = gaze_stimulus_col,
    geometry_aoi_col = geometry_aoi_col,
    geometry_stimulus_col = geometry_stimulus_col,
    tie_method = tie_method,
    outside_label = outside_label,
    ambiguous_label = ambiguous_label,
    missing_label = missing_label
  )

  comparable_sample <- !is.na(observed_aoi) &
    !derived$derived_aoi %in% c(ambiguous_label, missing_label)

  coding_match <- observed_aoi == derived$derived_aoi
  coding_match[!comparable_sample] <- NA

  coding_status <- .gp3_aoi_coding_sample_status(
    observed_aoi = observed_aoi,
    derived_aoi = derived$derived_aoi,
    derived_assignment_status = derived$derived_assignment_status,
    comparable_sample = comparable_sample,
    coding_match = coding_match,
    ambiguous_label = ambiguous_label,
    missing_label = missing_label
  )

  out <- cbind(
    base_id,
    tibble::tibble(
      observed_aoi_raw = observed_aoi_raw,
      observed_aoi = observed_aoi,
      derived_aoi = derived$derived_aoi,
      n_matching_aois = derived$n_matching_aois,
      derived_assignment_status = derived$derived_assignment_status,
      comparable_sample = comparable_sample,
      coding_match = coding_match,
      aoi_coding_status = coding_status
    )
  )

  tibble::as_tibble(out)
}

.gp3_aoi_coding_assign_samples <- function(
    gaze_data,
    geometry_summary,
    gaze_x_col,
    gaze_y_col,
    gaze_stimulus_col,
    geometry_aoi_col,
    geometry_stimulus_col,
    tie_method,
    outside_label,
    ambiguous_label,
    missing_label
) {
  x <- suppressWarnings(as.numeric(gaze_data[[gaze_x_col]]))
  y <- suppressWarnings(as.numeric(gaze_data[[gaze_y_col]]))

  derived_aoi <- rep(outside_label, nrow(gaze_data))
  n_matching_aois <- rep(0L, nrow(gaze_data))
  derived_assignment_status <- rep("no_aoi", nrow(gaze_data))

  missing_coordinate <- !is.finite(x) | !is.finite(y)

  derived_aoi[missing_coordinate] <- missing_label
  derived_assignment_status[missing_coordinate] <- "missing_coordinate"

  for (i in seq_len(nrow(gaze_data))) {
    if (missing_coordinate[[i]]) {
      next
    }

    candidate_geometry <- geometry_summary

    if (!is.null(gaze_stimulus_col) &&
        !is.null(geometry_stimulus_col) &&
        geometry_stimulus_col %in% names(candidate_geometry)) {
      candidate_geometry <- candidate_geometry[
        as.character(candidate_geometry[[geometry_stimulus_col]]) ==
          as.character(gaze_data[[gaze_stimulus_col]][[i]]),
        ,
        drop = FALSE
      ]
    }

    if (nrow(candidate_geometry) == 0L) {
      next
    }

    inside <- x[[i]] >= candidate_geometry$x_min &
      x[[i]] <= candidate_geometry$x_max &
      y[[i]] >= candidate_geometry$y_min &
      y[[i]] <= candidate_geometry$y_max

    hits <- which(inside)
    n_hits <- length(hits)

    n_matching_aois[[i]] <- n_hits

    if (n_hits == 0L) {
      next
    }

    if (n_hits == 1L) {
      derived_aoi[[i]] <- as.character(
        candidate_geometry[[geometry_aoi_col]][[hits]]
      )
      derived_assignment_status[[i]] <- "single_aoi"
      next
    }

    if (identical(tie_method, "first")) {
      derived_aoi[[i]] <- as.character(
        candidate_geometry[[geometry_aoi_col]][[hits[[1]]]]
      )
      derived_assignment_status[[i]] <- "multiple_aoi_resolved"
    } else {
      derived_aoi[[i]] <- ambiguous_label
      derived_assignment_status[[i]] <- "ambiguous_aoi"
    }
  }

  tibble::tibble(
    derived_aoi = derived_aoi,
    n_matching_aois = n_matching_aois,
    derived_assignment_status = derived_assignment_status
  )
}

.gp3_aoi_coding_standardise_observed_aoi <- function(
    observed_aoi_raw,
    outside_label,
    observed_outside_values
) {
  out <- observed_aoi_raw

  out[is.na(out)] <- NA_character_

  lower_values <- tolower(trimws(out))
  outside_lookup <- tolower(observed_outside_values)

  out[!is.na(lower_values) & lower_values %in% outside_lookup] <- outside_label
  out[!is.na(lower_values) & !nzchar(lower_values)] <- NA_character_

  out
}

.gp3_aoi_coding_sample_status <- function(
    observed_aoi,
    derived_aoi,
    derived_assignment_status,
    comparable_sample,
    coding_match,
    ambiguous_label,
    missing_label
) {
  out <- rep("ok", length(observed_aoi))

  out[is.na(observed_aoi)] <- "observed_missing"
  out[derived_assignment_status == "missing_coordinate"] <- "missing_coordinate"
  out[derived_aoi == ambiguous_label] <- "ambiguous_derived"
  out[comparable_sample & coding_match %in% FALSE] <- "mismatch"

  out
}

.gp3_aoi_coding_create_matrix <- function(sample_coding) {
  matrix_table <- as.data.frame(
    table(
      observed_aoi = sample_coding$observed_aoi,
      derived_aoi = sample_coding$derived_aoi,
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )

  names(matrix_table) <- c("observed_aoi", "derived_aoi", "n_samples")

  matrix_table <- matrix_table[matrix_table$n_samples > 0L, , drop = FALSE]

  matrix_table <- tibble::as_tibble(matrix_table)

  total <- sum(matrix_table$n_samples, na.rm = TRUE)

  matrix_table$sample_prop <- ifelse(
    total > 0L,
    matrix_table$n_samples / total,
    NA_real_
  )

  matrix_table
}

.gp3_aoi_coding_create_observed_summary <- function(sample_coding) {
  split_idx <- split(
    seq_len(nrow(sample_coding)),
    sample_coding$observed_aoi,
    drop = TRUE
  )

  rows <- lapply(split_idx, function(idx) {
    d <- sample_coding[idx, , drop = FALSE]

    tibble::tibble(
      observed_aoi = as.character(d$observed_aoi[[1]]),
      n_samples = nrow(d),
      n_comparable_samples = sum(d$comparable_sample, na.rm = TRUE),
      n_matches = sum(d$coding_match %in% TRUE, na.rm = TRUE),
      n_mismatches = sum(d$aoi_coding_status == "mismatch", na.rm = TRUE)
    )
  })

  out <- dplyr::bind_rows(rows)
  total <- sum(out$n_samples, na.rm = TRUE)
  out$sample_prop <- out$n_samples / total
  out$mismatch_prop <- ifelse(
    out$n_comparable_samples > 0L,
    out$n_mismatches / out$n_comparable_samples,
    NA_real_
  )

  out[order(out$observed_aoi), , drop = FALSE]
}

.gp3_aoi_coding_create_derived_summary <- function(sample_coding) {
  split_idx <- split(
    seq_len(nrow(sample_coding)),
    sample_coding$derived_aoi,
    drop = TRUE
  )

  rows <- lapply(split_idx, function(idx) {
    d <- sample_coding[idx, , drop = FALSE]

    tibble::tibble(
      derived_aoi = as.character(d$derived_aoi[[1]]),
      n_samples = nrow(d),
      n_comparable_samples = sum(d$comparable_sample, na.rm = TRUE),
      n_matches = sum(d$coding_match %in% TRUE, na.rm = TRUE),
      n_mismatches = sum(d$aoi_coding_status == "mismatch", na.rm = TRUE)
    )
  })

  out <- dplyr::bind_rows(rows)
  total <- sum(out$n_samples, na.rm = TRUE)
  out$sample_prop <- out$n_samples / total
  out$mismatch_prop <- ifelse(
    out$n_comparable_samples > 0L,
    out$n_mismatches / out$n_comparable_samples,
    NA_real_
  )

  out[order(out$derived_aoi), , drop = FALSE]
}

.gp3_aoi_coding_overview_status <- function(
    mismatch_prop,
    ambiguous_prop,
    missing_coordinate_prop,
    max_mismatch_prop,
    max_ambiguous_prop,
    max_missing_coordinate_prop
) {
  if (!is.na(mismatch_prop) && mismatch_prop > max_mismatch_prop) {
    return("review")
  }

  if (!is.na(ambiguous_prop) && ambiguous_prop > max_ambiguous_prop) {
    return("review")
  }

  if (!is.na(missing_coordinate_prop) &&
      missing_coordinate_prop > max_missing_coordinate_prop) {
    return("review")
  }

  "ok"
}

.gp3_aoi_coding_standardise_gaze_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("AOI" %in% names(data) && !"aoi" %in% names(data)) {
    data$aoi <- data$AOI
  }

  data
}

.gp3_aoi_coding_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols[cols == "AOI"] <- "aoi"
  cols
}

.gp3_aoi_coding_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (col == "AOI" && "aoi" %in% names_data) {
    return("aoi")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_aoi_coding_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_aoi_coding_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    if (found[[1]] == "MEDIA_ID" && "media_id" %in% names_data) {
      return("media_id")
    }

    if (found[[1]] == "AOI" && "aoi" %in% names_data) {
      return("aoi")
    }

    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_aoi_coding_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_coding_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_coding_check_prop <- function(x, arg) {
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

.gp3_aoi_coding_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_coding_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
