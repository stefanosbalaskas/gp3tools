#' Audit AOI margin sensitivity
#'
#' Audit whether sample-level AOI assignments are sensitive to small expansions
#' or shrinkages of AOI boundaries.
#'
#' @param gaze_data A data frame containing gaze samples.
#' @param aoi_geometry A data frame containing AOI geometry definitions.
#' @param gaze_x_col Gaze x-coordinate column. If `NULL`, common aliases are
#'   detected automatically.
#' @param gaze_y_col Gaze y-coordinate column. If `NULL`, common aliases are
#'   detected automatically.
#' @param gaze_stimulus_col Optional gaze stimulus/media column.
#' @param sample_id_cols Optional columns to carry into the sample-level audit
#'   table.
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
#' @param margins Numeric vector of AOI boundary margins. Positive values expand
#'   AOIs; negative values shrink AOIs. A zero-margin baseline is always added.
#' @param screen_x_range Numeric length-2 vector defining the screen x range.
#' @param screen_y_range Numeric length-2 vector defining the screen y range.
#' @param tie_method How to handle samples falling in multiple AOIs. Use
#'   `"ambiguous"` to label them as ambiguous or `"first"` to use the first
#'   matching AOI.
#' @param outside_label Label for samples outside all AOIs.
#' @param ambiguous_label Label for samples inside multiple AOIs when
#'   `tie_method = "ambiguous"`.
#' @param missing_label Label for samples with missing/non-finite gaze
#'   coordinates.
#' @param max_margin_change_prop Maximum acceptable proportion of samples whose
#'   AOI assignment changes from the zero-margin baseline.
#' @param max_ambiguous_prop Maximum acceptable proportion of ambiguous samples.
#' @param ignore_invalid_geometry Logical. If `TRUE`, AOIs with invalid
#'   coordinates or dimensions are excluded before margin coding.
#'
#' @return A list with class `gp3_aoi_margin_sensitivity_audit` containing
#'   overview, geometry_summary, sample_sensitivity, margin_summary,
#'   aoi_margin_summary, flagged_samples, and settings tables.
#' @export
audit_gazepoint_aoi_margin_sensitivity <- function(
    gaze_data,
    aoi_geometry,
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
    margins = c(-0.02, 0, 0.02),
    screen_x_range = c(0, 1),
    screen_y_range = c(0, 1),
    tie_method = c("ambiguous", "first"),
    outside_label = "outside",
    ambiguous_label = "ambiguous",
    missing_label = "missing_coordinate",
    max_margin_change_prop = 0.10,
    max_ambiguous_prop = 0.05,
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

  gaze_data <- .gp3_aoi_margin_standardise_gaze_aliases(gaze_data)

  gaze_x_col <- .gp3_aoi_margin_resolve_or_detect_col(
    col = gaze_x_col,
    names_data = names(gaze_data),
    arg = "gaze_x_col",
    candidates = c("x", "X", "gaze_x", "gaze_x_norm", "FPOGX", "BPOGX"),
    required = TRUE
  )

  gaze_y_col <- .gp3_aoi_margin_resolve_or_detect_col(
    col = gaze_y_col,
    names_data = names(gaze_data),
    arg = "gaze_y_col",
    candidates = c("y", "Y", "gaze_y", "gaze_y_norm", "FPOGY", "BPOGY"),
    required = TRUE
  )

  gaze_stimulus_col <- .gp3_aoi_margin_resolve_or_detect_col(
    col = gaze_stimulus_col,
    names_data = names(gaze_data),
    arg = "gaze_stimulus_col",
    candidates = c("media_id", "MEDIA_ID", "stimulus", "stimulus_id"),
    required = FALSE
  )

  sample_id_cols <- .gp3_aoi_margin_standardise_cols(sample_id_cols)
  sample_id_cols <- sample_id_cols[sample_id_cols %in% names(gaze_data)]

  .gp3_aoi_margin_check_numeric_vector(margins, "margins")
  .gp3_aoi_margin_check_nonnegative_numeric(
    max_margin_change_prop,
    "max_margin_change_prop"
  )
  .gp3_aoi_margin_check_prop(max_ambiguous_prop, "max_ambiguous_prop")
  .gp3_aoi_margin_check_label(outside_label, "outside_label")
  .gp3_aoi_margin_check_label(ambiguous_label, "ambiguous_label")
  .gp3_aoi_margin_check_label(missing_label, "missing_label")
  .gp3_aoi_margin_check_logical_scalar(
    ignore_invalid_geometry,
    "ignore_invalid_geometry"
  )

  margins_used <- sort(unique(c(0, as.numeric(margins))))

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

  sample_sensitivity <- .gp3_aoi_margin_create_sample_sensitivity(
    gaze_data = gaze_data,
    geometry_summary = geometry_for_coding,
    gaze_x_col = gaze_x_col,
    gaze_y_col = gaze_y_col,
    gaze_stimulus_col = gaze_stimulus_col,
    geometry_aoi_col = resolved_geometry_aoi_col,
    geometry_stimulus_col = resolved_geometry_stimulus_col,
    sample_id_cols = sample_id_cols,
    margins_used = margins_used,
    tie_method = tie_method,
    outside_label = outside_label,
    ambiguous_label = ambiguous_label,
    missing_label = missing_label
  )

  margin_summary <- .gp3_aoi_margin_create_margin_summary(
    sample_sensitivity = sample_sensitivity,
    outside_label = outside_label,
    ambiguous_label = ambiguous_label,
    missing_label = missing_label,
    max_margin_change_prop = max_margin_change_prop,
    max_ambiguous_prop = max_ambiguous_prop
  )

  aoi_margin_summary <- .gp3_aoi_margin_create_aoi_summary(
    sample_sensitivity = sample_sensitivity
  )

  flagged_samples <- sample_sensitivity[
    sample_sensitivity$margin != 0 &
      (
        sample_sensitivity$changed_from_base %in% TRUE |
          sample_sensitivity$margin_assignment_status == "ambiguous_aoi"
      ),
    ,
    drop = FALSE
  ]

  n_flagged_margins <- sum(
    !margin_summary$margin_sensitivity_status %in% c("ok", "base"),
    na.rm = TRUE
  )

  overview <- tibble::tibble(
    n_gaze_rows = nrow(gaze_data),
    n_geometry_rows = nrow(aoi_geometry),
    n_aois = nrow(geometry_summary),
    n_aois_used = nrow(geometry_for_coding),
    n_margins = length(margins_used),
    n_sample_margin_rows = nrow(sample_sensitivity),
    n_flagged_margins = n_flagged_margins,
    max_margin_change_prop_observed = .gp3_aoi_margin_safe_max(
      margin_summary$margin_change_prop[margin_summary$margin != 0]
    ),
    max_ambiguous_prop_observed = .gp3_aoi_margin_safe_max(
      margin_summary$ambiguous_prop
    ),
    aoi_margin_sensitivity_status = ifelse(
      n_flagged_margins > 0L,
      "review",
      "ok"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "gaze_x_col",
      "gaze_y_col",
      "gaze_stimulus_col",
      "sample_id_cols",
      "geometry_aoi_col",
      "geometry_stimulus_col",
      "margins",
      "margins_used",
      "screen_x_range",
      "screen_y_range",
      "tie_method",
      "outside_label",
      "ambiguous_label",
      "missing_label",
      "max_margin_change_prop",
      "max_ambiguous_prop",
      "ignore_invalid_geometry"
    ),
    value = c(
      gaze_x_col,
      gaze_y_col,
      .gp3_aoi_margin_collapse_nullable(gaze_stimulus_col),
      .gp3_aoi_margin_collapse_nullable(sample_id_cols),
      resolved_geometry_aoi_col,
      .gp3_aoi_margin_collapse_nullable(resolved_geometry_stimulus_col),
      paste(margins, collapse = ", "),
      paste(margins_used, collapse = ", "),
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      tie_method,
      outside_label,
      ambiguous_label,
      missing_label,
      as.character(max_margin_change_prop),
      as.character(max_ambiguous_prop),
      as.character(ignore_invalid_geometry)
    )
  )

  out <- list(
    overview = overview,
    geometry_summary = geometry_summary,
    sample_sensitivity = sample_sensitivity,
    margin_summary = margin_summary,
    aoi_margin_summary = aoi_margin_summary,
    flagged_samples = flagged_samples,
    settings = settings
  )

  class(out) <- c("gp3_aoi_margin_sensitivity_audit", "list")

  out
}

.gp3_aoi_margin_create_sample_sensitivity <- function(
    gaze_data,
    geometry_summary,
    gaze_x_col,
    gaze_y_col,
    gaze_stimulus_col,
    geometry_aoi_col,
    geometry_stimulus_col,
    sample_id_cols,
    margins_used,
    tie_method,
    outside_label,
    ambiguous_label,
    missing_label
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

  margin_rows <- lapply(margins_used, function(margin_value) {
    assigned <- .gp3_aoi_margin_assign_samples(
      gaze_data = gaze_data,
      geometry_summary = geometry_summary,
      gaze_x_col = gaze_x_col,
      gaze_y_col = gaze_y_col,
      gaze_stimulus_col = gaze_stimulus_col,
      geometry_aoi_col = geometry_aoi_col,
      geometry_stimulus_col = geometry_stimulus_col,
      margin_value = margin_value,
      tie_method = tie_method,
      outside_label = outside_label,
      ambiguous_label = ambiguous_label,
      missing_label = missing_label
    )

    cbind(
      base_id,
      tibble::tibble(
        margin = margin_value,
        assigned_aoi = assigned$assigned_aoi,
        n_matching_aois = assigned$n_matching_aois,
        margin_assignment_status = assigned$margin_assignment_status
      )
    )
  })

  out <- dplyr::bind_rows(margin_rows)

  base <- out[out$margin == 0, c(".gp3_sample_index", "assigned_aoi")]
  names(base)[names(base) == "assigned_aoi"] <- "base_assigned_aoi"

  out <- merge(
    out,
    base,
    by = ".gp3_sample_index",
    all.x = TRUE,
    sort = FALSE
  )

  out$changed_from_base <- out$assigned_aoi != out$base_assigned_aoi
  out$changed_from_base[out$margin == 0] <- FALSE

  out <- out[order(out$.gp3_sample_index, out$margin), , drop = FALSE]

  tibble::as_tibble(out)
}

.gp3_aoi_margin_assign_samples <- function(
    gaze_data,
    geometry_summary,
    gaze_x_col,
    gaze_y_col,
    gaze_stimulus_col,
    geometry_aoi_col,
    geometry_stimulus_col,
    margin_value,
    tie_method,
    outside_label,
    ambiguous_label,
    missing_label
) {
  x <- suppressWarnings(as.numeric(gaze_data[[gaze_x_col]]))
  y <- suppressWarnings(as.numeric(gaze_data[[gaze_y_col]]))

  assigned_aoi <- rep(outside_label, nrow(gaze_data))
  n_matching_aois <- rep(0L, nrow(gaze_data))
  margin_assignment_status <- rep("no_aoi", nrow(gaze_data))

  missing_coordinate <- !is.finite(x) | !is.finite(y)

  assigned_aoi[missing_coordinate] <- missing_label
  margin_assignment_status[missing_coordinate] <- "missing_coordinate"

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

    expanded_x_min <- candidate_geometry$x_min - margin_value
    expanded_y_min <- candidate_geometry$y_min - margin_value
    expanded_x_max <- candidate_geometry$x_max + margin_value
    expanded_y_max <- candidate_geometry$y_max + margin_value

    valid_expanded <- expanded_x_min < expanded_x_max &
      expanded_y_min < expanded_y_max

    inside <- valid_expanded &
      x[[i]] >= expanded_x_min &
      x[[i]] <= expanded_x_max &
      y[[i]] >= expanded_y_min &
      y[[i]] <= expanded_y_max

    hits <- which(inside)
    n_hits <- length(hits)

    n_matching_aois[[i]] <- n_hits

    if (n_hits == 0L) {
      next
    }

    if (n_hits == 1L) {
      assigned_aoi[[i]] <- as.character(
        candidate_geometry[[geometry_aoi_col]][[hits]]
      )
      margin_assignment_status[[i]] <- "single_aoi"
      next
    }

    if (identical(tie_method, "first")) {
      assigned_aoi[[i]] <- as.character(
        candidate_geometry[[geometry_aoi_col]][[hits[[1]]]]
      )
      margin_assignment_status[[i]] <- "multiple_aoi_resolved"
    } else {
      assigned_aoi[[i]] <- ambiguous_label
      margin_assignment_status[[i]] <- "ambiguous_aoi"
    }
  }

  tibble::tibble(
    assigned_aoi = assigned_aoi,
    n_matching_aois = n_matching_aois,
    margin_assignment_status = margin_assignment_status
  )
}

.gp3_aoi_margin_create_margin_summary <- function(
    sample_sensitivity,
    outside_label,
    ambiguous_label,
    missing_label,
    max_margin_change_prop,
    max_ambiguous_prop
) {
  split_idx <- split(
    seq_len(nrow(sample_sensitivity)),
    sample_sensitivity$margin
  )

  rows <- lapply(split_idx, function(idx) {
    d <- sample_sensitivity[idx, , drop = FALSE]

    n_samples <- nrow(d)
    n_changed <- sum(d$changed_from_base, na.rm = TRUE)
    n_ambiguous <- sum(d$assigned_aoi == ambiguous_label, na.rm = TRUE)
    n_outside <- sum(d$assigned_aoi == outside_label, na.rm = TRUE)
    n_missing <- sum(d$assigned_aoi == missing_label, na.rm = TRUE)

    margin_value <- d$margin[[1]]

    change_prop <- n_changed / n_samples
    ambiguous_prop <- n_ambiguous / n_samples
    outside_prop <- n_outside / n_samples
    missing_prop <- n_missing / n_samples

    status <- .gp3_aoi_margin_summary_status(
      margin = margin_value,
      change_prop = change_prop,
      ambiguous_prop = ambiguous_prop,
      max_margin_change_prop = max_margin_change_prop,
      max_ambiguous_prop = max_ambiguous_prop
    )

    tibble::tibble(
      margin = margin_value,
      n_samples = n_samples,
      n_changed_from_base = n_changed,
      margin_change_prop = change_prop,
      n_ambiguous = n_ambiguous,
      ambiguous_prop = ambiguous_prop,
      n_outside = n_outside,
      outside_prop = outside_prop,
      n_missing_coordinate = n_missing,
      missing_coordinate_prop = missing_prop,
      margin_sensitivity_status = status
    )
  })

  out <- dplyr::bind_rows(rows)
  out[order(out$margin), , drop = FALSE]
}

.gp3_aoi_margin_summary_status <- function(
    margin,
    change_prop,
    ambiguous_prop,
    max_margin_change_prop,
    max_ambiguous_prop
) {
  if (margin == 0) {
    if (ambiguous_prop > max_ambiguous_prop) {
      return("base_ambiguous")
    }

    return("base")
  }

  if (change_prop > max_margin_change_prop) {
    return("margin_sensitive")
  }

  if (ambiguous_prop > max_ambiguous_prop) {
    return("ambiguous_margin")
  }

  "ok"
}

.gp3_aoi_margin_create_aoi_summary <- function(sample_sensitivity) {
  split_key <- interaction(
    sample_sensitivity$margin,
    sample_sensitivity$assigned_aoi,
    drop = TRUE,
    lex.order = TRUE
  )

  split_idx <- split(seq_len(nrow(sample_sensitivity)), split_key)

  rows <- lapply(split_idx, function(idx) {
    d <- sample_sensitivity[idx, , drop = FALSE]

    tibble::tibble(
      margin = d$margin[[1]],
      assigned_aoi = d$assigned_aoi[[1]],
      n_samples = nrow(d),
      n_changed_from_base = sum(d$changed_from_base, na.rm = TRUE)
    )
  })

  out <- dplyr::bind_rows(rows)

  margin_totals <- stats::aggregate(
    out$n_samples,
    by = list(margin = out$margin),
    FUN = sum
  )
  names(margin_totals)[names(margin_totals) == "x"] <- "margin_total_samples"

  out <- merge(
    out,
    margin_totals,
    by = "margin",
    all.x = TRUE,
    sort = FALSE
  )

  out$sample_prop <- out$n_samples / out$margin_total_samples

  out <- out[
    order(out$margin, out$assigned_aoi),
    ,
    drop = FALSE
  ]

  tibble::as_tibble(out)
}

.gp3_aoi_margin_standardise_gaze_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  data
}

.gp3_aoi_margin_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols
}

.gp3_aoi_margin_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_aoi_margin_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_aoi_margin_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    if (found[[1]] == "MEDIA_ID" && "media_id" %in% names_data) {
      return("media_id")
    }

    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_aoi_margin_check_numeric_vector <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!is.finite(x))) {
    stop("`", arg, "` must be a non-empty finite numeric vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_margin_check_nonnegative_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0) {
    stop("`", arg, "` must be a non-negative numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_margin_check_prop <- function(x, arg) {
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

.gp3_aoi_margin_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_margin_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_margin_safe_max <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

.gp3_aoi_margin_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
