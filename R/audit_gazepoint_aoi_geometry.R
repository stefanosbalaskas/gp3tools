#' Audit AOI geometry definitions
#'
#' Create a publication-level audit of AOI geometry definitions, including AOI
#' size, area, coordinate validity, screen-bound checks, and duplicate geometry.
#'
#' @param data A data frame containing AOI geometry definitions.
#' @param aoi_col AOI label/name column. If `NULL`, common AOI-name aliases are
#'   detected automatically.
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
#' @param min_width Minimum acceptable AOI width.
#' @param min_height Minimum acceptable AOI height.
#' @param min_area Minimum acceptable AOI area.
#' @param max_area_prop Maximum acceptable AOI area as a proportion of screen
#'   area.
#' @param require_within_screen Logical. If `TRUE`, AOIs extending outside the
#'   screen range are flagged.
#'
#' @return A list with class `gp3_aoi_geometry_audit` containing overview,
#'   geometry_summary, size_summary, flagged_aois, duplicate_geometry, and
#'   settings tables.
#' @export
audit_gazepoint_aoi_geometry <- function(
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
    min_width = 0,
    min_height = 0,
    min_area = 0,
    max_area_prop = 1,
    require_within_screen = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_aoi_geometry_standardise_aliases(data)

  aoi_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = aoi_col,
    names_data = names(data),
    arg = "aoi_col",
    candidates = c(
      "aoi",
      "aoi_name",
      "aoi_id",
      "AOI",
      "AOI_NAME",
      "AOI_ID"
    ),
    required = TRUE
  )

  stimulus_col <- .gp3_aoi_geometry_resolve_optional_col(
    stimulus_col,
    names(data),
    "stimulus_col"
  )

  x_min_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = x_min_col,
    names_data = names(data),
    arg = "x_min_col",
    candidates = c("x_min", "xmin", "left", "Left", "AOI_X_MIN", "AOI_LEFT"),
    required = FALSE
  )

  y_min_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = y_min_col,
    names_data = names(data),
    arg = "y_min_col",
    candidates = c("y_min", "ymin", "top", "Top", "AOI_Y_MIN", "AOI_TOP"),
    required = FALSE
  )

  x_max_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = x_max_col,
    names_data = names(data),
    arg = "x_max_col",
    candidates = c("x_max", "xmax", "right", "Right", "AOI_X_MAX", "AOI_RIGHT"),
    required = FALSE
  )

  y_max_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = y_max_col,
    names_data = names(data),
    arg = "y_max_col",
    candidates = c("y_max", "ymax", "bottom", "Bottom", "AOI_Y_MAX", "AOI_BOTTOM"),
    required = FALSE
  )

  x_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = x_col,
    names_data = names(data),
    arg = "x_col",
    candidates = c("x", "X", "aoi_x", "AOI_X"),
    required = FALSE
  )

  y_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = y_col,
    names_data = names(data),
    arg = "y_col",
    candidates = c("y", "Y", "aoi_y", "AOI_Y"),
    required = FALSE
  )

  width_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = width_col,
    names_data = names(data),
    arg = "width_col",
    candidates = c("width", "Width", "aoi_width", "AOI_WIDTH"),
    required = FALSE
  )

  height_col <- .gp3_aoi_geometry_resolve_or_detect_col(
    col = height_col,
    names_data = names(data),
    arg = "height_col",
    candidates = c("height", "Height", "aoi_height", "AOI_HEIGHT"),
    required = FALSE
  )

  has_bounds <- !is.null(x_min_col) &&
    !is.null(y_min_col) &&
    !is.null(x_max_col) &&
    !is.null(y_max_col)

  has_origin_size <- !is.null(x_col) &&
    !is.null(y_col) &&
    !is.null(width_col) &&
    !is.null(height_col)

  if (!has_bounds && !has_origin_size) {
    stop(
      "AOI geometry requires either x/y min-max columns or x/y plus width/height columns.",
      call. = FALSE
    )
  }

  .gp3_aoi_geometry_check_range(screen_x_range, "screen_x_range")
  .gp3_aoi_geometry_check_range(screen_y_range, "screen_y_range")

  .gp3_aoi_geometry_check_nonnegative_numeric(min_width, "min_width")
  .gp3_aoi_geometry_check_nonnegative_numeric(min_height, "min_height")
  .gp3_aoi_geometry_check_nonnegative_numeric(min_area, "min_area")
  .gp3_aoi_geometry_check_prop(max_area_prop, "max_area_prop")
  .gp3_aoi_geometry_check_logical_scalar(
    require_within_screen,
    "require_within_screen"
  )

  geometry_summary <- .gp3_aoi_geometry_create_summary(
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
    min_width = min_width,
    min_height = min_height,
    min_area = min_area,
    max_area_prop = max_area_prop,
    require_within_screen = require_within_screen,
    coordinate_format = ifelse(has_bounds, "bounds", "origin_size")
  )

  size_summary <- .gp3_aoi_geometry_create_size_summary(
    geometry_summary
  )

  duplicate_geometry <- .gp3_aoi_geometry_create_duplicate_summary(
    geometry_summary = geometry_summary,
    aoi_col = aoi_col,
    stimulus_col = stimulus_col
  )

  flagged_aois <- geometry_summary[
    geometry_summary$aoi_geometry_status != "ok",
    ,
    drop = FALSE
  ]

  n_duplicate_geometry <- sum(
    duplicate_geometry$duplicate_geometry_status != "ok",
    na.rm = TRUE
  )

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_aois = nrow(geometry_summary),
    n_stimuli = if (!is.null(stimulus_col)) {
      length(unique(geometry_summary[[stimulus_col]]))
    } else {
      NA_integer_
    },
    n_flagged_aois = nrow(flagged_aois),
    n_duplicate_geometry_groups = n_duplicate_geometry,
    coordinate_format = ifelse(has_bounds, "bounds", "origin_size"),
    screen_width = screen_x_range[[2]] - screen_x_range[[1]],
    screen_height = screen_y_range[[2]] - screen_y_range[[1]],
    screen_area = (screen_x_range[[2]] - screen_x_range[[1]]) *
      (screen_y_range[[2]] - screen_y_range[[1]]),
    aoi_geometry_status = dplyr::case_when(
      nrow(flagged_aois) > 0L ~ "review",
      n_duplicate_geometry > 0L ~ "review",
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
      "min_width",
      "min_height",
      "min_area",
      "max_area_prop",
      "require_within_screen"
    ),
    value = c(
      aoi_col,
      .gp3_aoi_geometry_collapse_nullable(stimulus_col),
      .gp3_aoi_geometry_collapse_nullable(x_min_col),
      .gp3_aoi_geometry_collapse_nullable(y_min_col),
      .gp3_aoi_geometry_collapse_nullable(x_max_col),
      .gp3_aoi_geometry_collapse_nullable(y_max_col),
      .gp3_aoi_geometry_collapse_nullable(x_col),
      .gp3_aoi_geometry_collapse_nullable(y_col),
      .gp3_aoi_geometry_collapse_nullable(width_col),
      .gp3_aoi_geometry_collapse_nullable(height_col),
      paste(screen_x_range, collapse = ", "),
      paste(screen_y_range, collapse = ", "),
      as.character(min_width),
      as.character(min_height),
      as.character(min_area),
      as.character(max_area_prop),
      as.character(require_within_screen)
    )
  )

  out <- list(
    overview = overview,
    geometry_summary = geometry_summary,
    size_summary = size_summary,
    duplicate_geometry = duplicate_geometry,
    flagged_aois = flagged_aois,
    settings = settings
  )

  class(out) <- c("gp3_aoi_geometry_audit", "list")

  out
}

.gp3_aoi_geometry_create_summary <- function(
    data,
    aoi_col,
    stimulus_col,
    x_min_col,
    y_min_col,
    x_max_col,
    y_max_col,
    x_col,
    y_col,
    width_col,
    height_col,
    screen_x_range,
    screen_y_range,
    min_width,
    min_height,
    min_area,
    max_area_prop,
    require_within_screen,
    coordinate_format
) {
  if (identical(coordinate_format, "bounds")) {
    x_min <- .gp3_aoi_geometry_numeric_col(data, x_min_col)
    y_min <- .gp3_aoi_geometry_numeric_col(data, y_min_col)
    x_max <- .gp3_aoi_geometry_numeric_col(data, x_max_col)
    y_max <- .gp3_aoi_geometry_numeric_col(data, y_max_col)
  } else {
    x_min <- .gp3_aoi_geometry_numeric_col(data, x_col)
    y_min <- .gp3_aoi_geometry_numeric_col(data, y_col)
    width <- .gp3_aoi_geometry_numeric_col(data, width_col)
    height <- .gp3_aoi_geometry_numeric_col(data, height_col)
    x_max <- x_min + width
    y_max <- y_min + height
  }

  width <- x_max - x_min
  height <- y_max - y_min
  area <- width * height

  screen_width <- screen_x_range[[2]] - screen_x_range[[1]]
  screen_height <- screen_y_range[[2]] - screen_y_range[[1]]
  screen_area <- screen_width * screen_height

  center_x <- x_min + width / 2
  center_y <- y_min + height / 2

  area_prop <- area / screen_area

  invalid_coordinate <- !is.finite(x_min) |
    !is.finite(y_min) |
    !is.finite(x_max) |
    !is.finite(y_max)

  invalid_dimension <- !invalid_coordinate &
    (
      width <= 0 |
        height <= 0
    )

  too_small <- !invalid_coordinate &
    !invalid_dimension &
    (
      width < min_width |
        height < min_height |
        area < min_area
    )

  too_large <- !invalid_coordinate &
    !invalid_dimension &
    area_prop > max_area_prop

  outside_screen <- !invalid_coordinate &
    !invalid_dimension &
    (
      x_min < screen_x_range[[1]] |
        x_max > screen_x_range[[2]] |
        y_min < screen_y_range[[1]] |
        y_max > screen_y_range[[2]]
    )

  status <- .gp3_aoi_geometry_status(
    invalid_coordinate = invalid_coordinate,
    invalid_dimension = invalid_dimension,
    too_small = too_small,
    too_large = too_large,
    outside_screen = outside_screen,
    require_within_screen = require_within_screen
  )

  id_cols <- c(aoi_col, stimulus_col)
  id_cols <- id_cols[!is.na(id_cols) & nzchar(id_cols)]

  out <- tibble::as_tibble(data[, id_cols, drop = FALSE])

  out$x_min <- x_min
  out$y_min <- y_min
  out$x_max <- x_max
  out$y_max <- y_max
  out$width <- width
  out$height <- height
  out$area <- area
  out$area_prop <- area_prop
  out$center_x <- center_x
  out$center_y <- center_y
  out$outside_screen <- outside_screen
  out$aoi_geometry_status <- status

  out
}

.gp3_aoi_geometry_status <- function(
    invalid_coordinate,
    invalid_dimension,
    too_small,
    too_large,
    outside_screen,
    require_within_screen
) {
  out <- rep("ok", length(invalid_coordinate))

  out[too_large %in% TRUE] <- "too_large"
  out[too_small %in% TRUE] <- "too_small"
  out[outside_screen %in% TRUE & isTRUE(require_within_screen)] <-
    "outside_screen"
  out[invalid_dimension %in% TRUE] <- "invalid_dimension"
  out[invalid_coordinate %in% TRUE] <- "invalid_coordinate"

  out
}

.gp3_aoi_geometry_create_size_summary <- function(geometry_summary) {
  tibble::tibble(
    n_aois = nrow(geometry_summary),
    min_width = .gp3_aoi_geometry_safe_min(geometry_summary$width),
    median_width = .gp3_aoi_geometry_safe_median(geometry_summary$width),
    max_width = .gp3_aoi_geometry_safe_max(geometry_summary$width),
    min_height = .gp3_aoi_geometry_safe_min(geometry_summary$height),
    median_height = .gp3_aoi_geometry_safe_median(geometry_summary$height),
    max_height = .gp3_aoi_geometry_safe_max(geometry_summary$height),
    min_area = .gp3_aoi_geometry_safe_min(geometry_summary$area),
    median_area = .gp3_aoi_geometry_safe_median(geometry_summary$area),
    max_area = .gp3_aoi_geometry_safe_max(geometry_summary$area),
    min_area_prop = .gp3_aoi_geometry_safe_min(geometry_summary$area_prop),
    median_area_prop = .gp3_aoi_geometry_safe_median(geometry_summary$area_prop),
    max_area_prop = .gp3_aoi_geometry_safe_max(geometry_summary$area_prop)
  )
}

.gp3_aoi_geometry_safe_min <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

.gp3_aoi_geometry_safe_median <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  stats::median(x)
}

.gp3_aoi_geometry_safe_max <- function(x) {
  x <- x[is.finite(x)]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}
.gp3_aoi_geometry_create_duplicate_summary <- function(
    geometry_summary,
    aoi_col,
    stimulus_col
) {
  group_cols <- c(stimulus_col, "x_min", "y_min", "x_max", "y_max")
  group_cols <- group_cols[!is.na(group_cols) & nzchar(group_cols)]

  if (length(group_cols) == 4L && is.null(stimulus_col)) {
    split_key <- interaction(
      geometry_summary[, group_cols, drop = FALSE],
      drop = TRUE,
      lex.order = TRUE
    )
  } else {
    split_key <- interaction(
      geometry_summary[, group_cols, drop = FALSE],
      drop = TRUE,
      lex.order = TRUE
    )
  }

  split_idx <- split(seq_len(nrow(geometry_summary)), split_key)

  rows <- lapply(split_idx, function(idx) {
    d <- geometry_summary[idx, , drop = FALSE]

    if (nrow(d) <= 1L) {
      return(NULL)
    }

    base <- d[1, group_cols, drop = FALSE]

    out <- tibble::as_tibble(base)
    out$n_aois <- nrow(d)
    out$aoi_values <- paste(as.character(d[[aoi_col]]), collapse = ", ")
    out$duplicate_geometry_status <- "duplicate_geometry"

    out
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0L) {
    return(tibble::tibble(
      n_aois = integer(),
      aoi_values = character(),
      duplicate_geometry_status = character()
    ))
  }

  dplyr::bind_rows(rows)
}

.gp3_aoi_geometry_numeric_col <- function(data, col) {
  suppressWarnings(as.numeric(data[[col]]))
}

.gp3_aoi_geometry_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("AOI" %in% names(data) && !"aoi" %in% names(data)) {
    data$aoi <- data$AOI
  }

  data
}

.gp3_aoi_geometry_resolve_col <- function(col, names_data, arg) {
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

.gp3_aoi_geometry_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_aoi_geometry_resolve_col(col, names_data, arg)
}

.gp3_aoi_geometry_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_aoi_geometry_resolve_col(col, names_data, arg))
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

.gp3_aoi_geometry_check_range <- function(x, arg) {
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

.gp3_aoi_geometry_check_nonnegative_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 0) {
    stop("`", arg, "` must be a non-negative numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_geometry_check_prop <- function(x, arg) {
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

.gp3_aoi_geometry_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_aoi_geometry_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
