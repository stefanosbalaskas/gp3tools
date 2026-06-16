#' Audit AOI overlap
#'
#' Create a publication-level audit of pairwise AOI overlap within each stimulus.
#'
#' @param data A data frame containing AOI geometry definitions.
#' @param aoi_col AOI label/name column. If `NULL`, common AOI-name aliases are
#'   detected automatically by `audit_gazepoint_aoi_geometry()`.
#' @param stimulus_col Optional stimulus/media column.
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
#' @param min_overlap_area Minimum overlap area above which an AOI pair is
#'   flagged.
#' @param min_overlap_prop Minimum overlap proportion above which an AOI pair is
#'   flagged. This is computed relative to the smaller AOI in the pair.
#' @param ignore_invalid_geometry Logical. If `TRUE`, AOIs with invalid geometry
#'   are excluded from pairwise overlap calculations.
#'
#' @return A list with class `gp3_aoi_overlap_audit` containing overview,
#'   geometry_summary, pairwise_overlap, overlap_summary, flagged_overlaps, and
#'   settings tables.
#' @export
audit_gazepoint_aoi_overlap <- function(
    data,
    aoi_col = NULL,
    stimulus_col = NULL,
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
    min_overlap_area = 0,
    min_overlap_prop = 0,
    ignore_invalid_geometry = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_aoi_overlap_check_nonnegative_numeric(
    min_overlap_area,
    "min_overlap_area"
  )

  .gp3_aoi_overlap_check_prop(
    min_overlap_prop,
    "min_overlap_prop"
  )

  .gp3_aoi_overlap_check_logical_scalar(
    ignore_invalid_geometry,
    "ignore_invalid_geometry"
  )

  geometry_audit <- audit_gazepoint_aoi_geometry(
    data = data,
    aoi_col = aoi_col,
    stimulus_col = stimulus_col,
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

  resolved_aoi_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "aoi_col"
  ]

  resolved_stimulus_col <- geometry_audit$settings$value[
    geometry_audit$settings$setting == "stimulus_col"
  ]

  if (is.na(resolved_stimulus_col) || !nzchar(resolved_stimulus_col)) {
    resolved_stimulus_col <- NULL
  }

  if (isTRUE(ignore_invalid_geometry)) {
    geometry_for_overlap <- geometry_summary[
      geometry_summary$aoi_geometry_status != "invalid_coordinate" &
        geometry_summary$aoi_geometry_status != "invalid_dimension",
      ,
      drop = FALSE
    ]
  } else {
    geometry_for_overlap <- geometry_summary
  }

  pairwise_overlap <- .gp3_aoi_overlap_create_pairwise(
    geometry_summary = geometry_for_overlap,
    aoi_col = resolved_aoi_col,
    stimulus_col = resolved_stimulus_col,
    min_overlap_area = min_overlap_area,
    min_overlap_prop = min_overlap_prop
  )

  overlap_summary <- .gp3_aoi_overlap_create_summary(
    pairwise_overlap = pairwise_overlap,
    stimulus_col = resolved_stimulus_col
  )

  flagged_overlaps <- pairwise_overlap[
    pairwise_overlap$aoi_overlap_status != "ok",
    ,
    drop = FALSE
  ]

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_aois = nrow(geometry_summary),
    n_aois_used = nrow(geometry_for_overlap),
    n_stimuli = if (!is.null(resolved_stimulus_col)) {
      length(unique(geometry_summary[[resolved_stimulus_col]]))
    } else {
      NA_integer_
    },
    n_aoi_pairs = nrow(pairwise_overlap),
    n_overlapping_pairs = sum(
      pairwise_overlap$overlap_area > 0,
      na.rm = TRUE
    ),
    n_flagged_overlaps = nrow(flagged_overlaps),
    max_overlap_area = .gp3_aoi_overlap_safe_max(
      pairwise_overlap$overlap_area
    ),
    max_overlap_prop_smaller = .gp3_aoi_overlap_safe_max(
      pairwise_overlap$overlap_prop_smaller
    ),
    aoi_overlap_status = dplyr::case_when(
      nrow(flagged_overlaps) > 0L ~ "review",
      TRUE ~ "ok"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "aoi_col",
      "stimulus_col",
      "x_min_col",
      "y_min_col",
      "x_max_col",
      "y_max_col",
      "x_col",
      "y_col",
      "width_col",
      "height_col",
      "screen_x_range",
      "screen_y_range",
      "min_overlap_area",
      "min_overlap_prop",
      "ignore_invalid_geometry"
    ),
    value = c(
      resolved_aoi_col,
      .gp3_aoi_overlap_collapse_nullable(resolved_stimulus_col),
      geometry_audit$settings$value[geometry_audit$settings$setting == "x_min_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "y_min_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "x_max_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "y_max_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "x_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "y_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "width_col"],
      geometry_audit$settings$value[geometry_audit$settings$setting == "height_col"],
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      as.character(min_overlap_area),
      as.character(min_overlap_prop),
      as.character(ignore_invalid_geometry)
    )
  )

  out <- list(
    overview = overview,
    geometry_summary = geometry_summary,
    pairwise_overlap = pairwise_overlap,
    overlap_summary = overlap_summary,
    flagged_overlaps = flagged_overlaps,
    settings = settings
  )

  class(out) <- c("gp3_aoi_overlap_audit", "list")

  out
}

.gp3_aoi_overlap_create_pairwise <- function(
    geometry_summary,
    aoi_col,
    stimulus_col,
    min_overlap_area,
    min_overlap_prop
) {
  if (nrow(geometry_summary) < 2L) {
    return(.gp3_aoi_overlap_empty_pairwise(stimulus_col))
  }

  if (!is.null(stimulus_col) && stimulus_col %in% names(geometry_summary)) {
    split_idx <- split(
      seq_len(nrow(geometry_summary)),
      geometry_summary[[stimulus_col]]
    )
  } else {
    split_idx <- list(all_stimuli = seq_len(nrow(geometry_summary)))
  }

  rows <- list()

  for (stimulus_name in names(split_idx)) {
    idx <- split_idx[[stimulus_name]]

    if (length(idx) < 2L) {
      next
    }

    d <- geometry_summary[idx, , drop = FALSE]
    comb <- utils::combn(seq_len(nrow(d)), 2)

    for (j in seq_len(ncol(comb))) {
      a <- d[comb[1, j], , drop = FALSE]
      b <- d[comb[2, j], , drop = FALSE]

      overlap_x_min <- max(a$x_min, b$x_min)
      overlap_y_min <- max(a$y_min, b$y_min)
      overlap_x_max <- min(a$x_max, b$x_max)
      overlap_y_max <- min(a$y_max, b$y_max)

      overlap_width <- max(0, overlap_x_max - overlap_x_min)
      overlap_height <- max(0, overlap_y_max - overlap_y_min)
      overlap_area <- overlap_width * overlap_height

      overlap_prop_a <- ifelse(
        is.finite(a$area) && a$area > 0,
        overlap_area / a$area,
        NA_real_
      )

      overlap_prop_b <- ifelse(
        is.finite(b$area) && b$area > 0,
        overlap_area / b$area,
        NA_real_
      )

      overlap_prop_smaller <- ifelse(
        is.finite(a$area) &&
          is.finite(b$area) &&
          min(a$area, b$area) > 0,
        overlap_area / min(a$area, b$area),
        NA_real_
      )

      status <- ifelse(
        overlap_area > min_overlap_area ||
          (!is.na(overlap_prop_smaller) &&
             overlap_prop_smaller > min_overlap_prop),
        "overlap",
        "ok"
      )

      row <- tibble::tibble(
        aoi_1 = as.character(a[[aoi_col]][[1]]),
        aoi_2 = as.character(b[[aoi_col]][[1]]),
        x_min_1 = a$x_min,
        y_min_1 = a$y_min,
        x_max_1 = a$x_max,
        y_max_1 = a$y_max,
        x_min_2 = b$x_min,
        y_min_2 = b$y_min,
        x_max_2 = b$x_max,
        y_max_2 = b$y_max,
        overlap_x_min = overlap_x_min,
        overlap_y_min = overlap_y_min,
        overlap_x_max = overlap_x_max,
        overlap_y_max = overlap_y_max,
        overlap_width = overlap_width,
        overlap_height = overlap_height,
        overlap_area = overlap_area,
        overlap_prop_aoi_1 = overlap_prop_a,
        overlap_prop_aoi_2 = overlap_prop_b,
        overlap_prop_smaller = overlap_prop_smaller,
        aoi_overlap_status = status
      )

      if (!is.null(stimulus_col) && stimulus_col %in% names(d)) {
        row <- cbind(
          tibble::tibble(
            stimulus_value = as.character(a[[stimulus_col]][[1]])
          ),
          row
        )

        names(row)[names(row) == "stimulus_value"] <- stimulus_col
      }

      rows[[length(rows) + 1L]] <- row
    }
  }

  if (length(rows) == 0L) {
    return(.gp3_aoi_overlap_empty_pairwise(stimulus_col))
  }

  dplyr::bind_rows(rows)
}

.gp3_aoi_overlap_empty_pairwise <- function(stimulus_col) {
  base <- tibble::tibble(
    aoi_1 = character(),
    aoi_2 = character(),
    x_min_1 = numeric(),
    y_min_1 = numeric(),
    x_max_1 = numeric(),
    y_max_1 = numeric(),
    x_min_2 = numeric(),
    y_min_2 = numeric(),
    x_max_2 = numeric(),
    y_max_2 = numeric(),
    overlap_x_min = numeric(),
    overlap_y_min = numeric(),
    overlap_x_max = numeric(),
    overlap_y_max = numeric(),
    overlap_width = numeric(),
    overlap_height = numeric(),
    overlap_area = numeric(),
    overlap_prop_aoi_1 = numeric(),
    overlap_prop_aoi_2 = numeric(),
    overlap_prop_smaller = numeric(),
    aoi_overlap_status = character()
  )

  if (!is.null(stimulus_col)) {
    base <- cbind(
      tibble::tibble(stimulus_value = character()),
      base
    )

    names(base)[names(base) == "stimulus_value"] <- stimulus_col
  }

  base
}

.gp3_aoi_overlap_create_summary <- function(
    pairwise_overlap,
    stimulus_col
) {
  if (nrow(pairwise_overlap) == 0L) {
    return(tibble::tibble(
      n_aoi_pairs = integer(),
      n_overlapping_pairs = integer(),
      n_flagged_overlaps = integer(),
      max_overlap_area = numeric(),
      max_overlap_prop_smaller = numeric(),
      aoi_overlap_summary_status = character()
    ))
  }

  if (!is.null(stimulus_col) && stimulus_col %in% names(pairwise_overlap)) {
    split_idx <- split(
      seq_len(nrow(pairwise_overlap)),
      pairwise_overlap[[stimulus_col]]
    )

    rows <- lapply(split_idx, function(idx) {
      d <- pairwise_overlap[idx, , drop = FALSE]

      out <- tibble::tibble(
        stimulus = as.character(d[[stimulus_col]][[1]]),
        n_aoi_pairs = nrow(d),
        n_overlapping_pairs = sum(d$overlap_area > 0, na.rm = TRUE),
        n_flagged_overlaps = sum(d$aoi_overlap_status != "ok", na.rm = TRUE),
        max_overlap_area = .gp3_aoi_overlap_safe_max(d$overlap_area),
        max_overlap_prop_smaller = .gp3_aoi_overlap_safe_max(
          d$overlap_prop_smaller
        ),
        aoi_overlap_summary_status = ifelse(
          any(d$aoi_overlap_status != "ok", na.rm = TRUE),
          "review",
          "ok"
        )
      )

      names(out)[names(out) == "stimulus"] <- stimulus_col

      out
    })

    return(dplyr::bind_rows(rows))
  }

  tibble::tibble(
    n_aoi_pairs = nrow(pairwise_overlap),
    n_overlapping_pairs = sum(pairwise_overlap$overlap_area > 0, na.rm = TRUE),
    n_flagged_overlaps = sum(
      pairwise_overlap$aoi_overlap_status != "ok",
      na.rm = TRUE
    ),
    max_overlap_area = .gp3_aoi_overlap_safe_max(
      pairwise_overlap$overlap_area
    ),
    max_overlap_prop_smaller = .gp3_aoi_overlap_safe_max(
      pairwise_overlap$overlap_prop_smaller
    ),
    aoi_overlap_summary_status = ifelse(
      any(pairwise_overlap$aoi_overlap_status != "ok", na.rm = TRUE),
      "review",
      "ok"
    )
  )
}

.gp3_aoi_overlap_check_nonnegative_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0) {
    stop("`", arg, "` must be a non-negative numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_overlap_check_prop <- function(x, arg) {
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

.gp3_aoi_overlap_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_overlap_safe_max <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

.gp3_aoi_overlap_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
