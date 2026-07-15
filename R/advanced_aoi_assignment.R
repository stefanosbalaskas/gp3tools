#' Add polygon AOI membership to Gazepoint data
#'
#' Classify sample-level gaze coordinates against one or more polygon AOIs
#' using a base-R ray-casting implementation. Polygon vertices are grouped by
#' AOI name and ordered either by row order or an explicit vertex-order column.
#'
#' @param master_df A sample-level gaze data frame.
#' @param vertices Polygon-vertex data frame.
#' @param x_col,y_col Gaze-coordinate columns.
#' @param aoi_col AOI-name column in `vertices`.
#' @param vertex_x_col,vertex_y_col Vertex-coordinate columns.
#' @param vertex_order_col Optional vertex-order column.
#' @param output Add logical AOI columns, one label column, or both.
#' @param prefix Prefix for logical AOI columns.
#' @param label_col Name of the AOI-label column.
#' @param outside_label Label used outside all polygons.
#' @param overlap Overlap handling for the label column.
#' @param boundary Should points on polygon boundaries be treated as inside?
#' @param include_overlap_count Add the number of containing AOIs.
#'
#' @return The input data with polygon AOI membership columns.
#'
#' @export
#'
#' @examples
#' gaze <- data.frame(
#'   FPOGX = c(0.2, 0.7, 0.9),
#'   FPOGY = c(0.2, 0.7, 0.1)
#' )
#'
#' triangle <- data.frame(
#'   aoi_name = "triangle",
#'   vertex_x = c(0, 1, 0),
#'   vertex_y = c(0, 0, 1)
#' )
#'
#' add_gazepoint_polygon_aoi(
#'   gaze,
#'   triangle,
#'   output = "both"
#' )
add_gazepoint_polygon_aoi <- function(
    master_df,
    vertices,
    x_col = "FPOGX",
    y_col = "FPOGY",
    aoi_col = "aoi_name",
    vertex_x_col = "vertex_x",
    vertex_y_col = "vertex_y",
    vertex_order_col = NULL,
    output = c("label", "logical", "both"),
    prefix = "aoi_",
    label_col = "aoi_current",
    outside_label = "outside",
    overlap = c("first", "last", "error"),
    boundary = c("inside", "outside"),
    include_overlap_count = TRUE) {

  .gp3_hp_assert_data_frame(master_df, "master_df")
  .gp3_hp_assert_data_frame(vertices, "vertices")

  output <- match.arg(output)
  overlap <- match.arg(overlap)
  boundary <- match.arg(boundary)

  .gp3_hp_assert_columns(
    master_df,
    c(x_col, y_col),
    "master_df"
  )

  .gp3_hp_assert_columns(
    vertices,
    c(
      aoi_col,
      vertex_x_col,
      vertex_y_col,
      vertex_order_col
    ),
    "vertices"
  )

  polygon_definitions <- .gp3_prepare_polygon_definitions(
    vertices = vertices,
    aoi_col = aoi_col,
    vertex_x_col = vertex_x_col,
    vertex_y_col = vertex_y_col,
    vertex_order_col = vertex_order_col
  )

  x <- suppressWarnings(
    as.numeric(master_df[[x_col]])
  )

  y <- suppressWarnings(
    as.numeric(master_df[[y_col]])
  )

  membership <- .gp3_polygon_membership_matrix(
    x = x,
    y = y,
    polygon_definitions = polygon_definitions,
    boundary = boundary
  )

  out <- .gp3_apply_aoi_membership(
    master_df = master_df,
    membership = membership,
    output = output,
    prefix = prefix,
    label_col = label_col,
    outside_label = outside_label,
    overlap = overlap,
    include_overlap_count = include_overlap_count,
    valid_xy = is.finite(x) & is.finite(y)
  )

  attr(out, "gazepoint_polygon_aoi_definitions") <-
    polygon_definitions

  .gp3_hp_restore_class(
    out,
    master_df
  )
}

#' Add time-varying AOI membership to Gazepoint data
#'
#' Match each gaze sample to a time-indexed AOI definition and classify the
#' sample against rectangular or polygon geometry. Definitions can also be
#' grouped by participant, stimulus, trial, or other shared columns.
#'
#' @param master_df A sample-level gaze data frame.
#' @param aoi_defs Time-indexed AOI definitions.
#' @param x_col,y_col Gaze-coordinate columns in `master_df`.
#' @param time_col Sample timestamp column.
#' @param aoi_time_col Definition timestamp column.
#' @param aoi_name_col AOI-name column.
#' @param shape Geometry type: `"auto"`, `"rectangle"`, or `"polygon"`.
#' @param group_cols Columns shared by `master_df` and `aoi_defs` that define
#'   independent dynamic-definition streams.
#' @param match Time-matching rule: nearest, previous, or next definition.
#' @param max_time_gap Maximum permitted absolute gap between a sample and its
#'   matched definition timestamp, in native time units.
#' @param left_col,right_col,top_col,bottom_col Rectangle boundary columns.
#' @param vertex_x_col,vertex_y_col Polygon vertex columns.
#' @param vertex_order_col Optional polygon vertex-order column.
#' @param output Add logical columns, a label column, or both.
#' @param prefix Prefix for logical AOI columns.
#' @param label_col AOI-label column.
#' @param outside_label Label used outside all AOIs.
#' @param overlap Overlap handling.
#' @param boundary Polygon-boundary handling.
#' @param definition_time_col,time_gap_col Names of dynamic-match diagnostics.
#' @param include_overlap_count Add the number of containing AOIs.
#'
#' @return The input data with dynamic AOI membership and match-diagnostic
#'   columns.
#'
#' @export
add_gazepoint_dynamic_aoi <- function(
    master_df,
    aoi_defs,
    x_col = "FPOGX",
    y_col = "FPOGY",
    time_col = "TIME",
    aoi_time_col = "aoi_time",
    aoi_name_col = "aoi_name",
    shape = c("auto", "rectangle", "polygon"),
    group_cols = NULL,
    match = c("nearest", "previous", "next"),
    max_time_gap = Inf,
    left_col = "left",
    right_col = "right",
    top_col = "top",
    bottom_col = "bottom",
    vertex_x_col = "vertex_x",
    vertex_y_col = "vertex_y",
    vertex_order_col = NULL,
    output = c("label", "logical", "both"),
    prefix = "aoi_",
    label_col = "aoi_current",
    outside_label = "outside",
    overlap = c("first", "last", "error"),
    boundary = c("inside", "outside"),
    definition_time_col = "aoi_definition_time",
    time_gap_col = "aoi_time_gap",
    include_overlap_count = TRUE) {

  .gp3_hp_assert_data_frame(master_df, "master_df")
  .gp3_hp_assert_data_frame(aoi_defs, "aoi_defs")

  shape <- match.arg(shape)
  match <- match.arg(match)
  output <- match.arg(output)
  overlap <- match.arg(overlap)
  boundary <- match.arg(boundary)
  group_cols <- unique(as.character(group_cols))

  .gp3_hp_assert_columns(
    master_df,
    unique(c(
      x_col,
      y_col,
      time_col,
      group_cols
    )),
    "master_df"
  )

  .gp3_hp_assert_columns(
    aoi_defs,
    unique(c(
      aoi_time_col,
      aoi_name_col,
      group_cols
    )),
    "aoi_defs"
  )

  if (!is.numeric(max_time_gap) ||
      length(max_time_gap) != 1L ||
      is.na(max_time_gap) ||
      max_time_gap < 0) {
    stop(
      "`max_time_gap` must be one non-negative number or Inf.",
      call. = FALSE
    )
  }

  resolved_shape <- .gp3_resolve_dynamic_aoi_shape(
    aoi_defs = aoi_defs,
    shape = shape,
    left_col = left_col,
    right_col = right_col,
    top_col = top_col,
    bottom_col = bottom_col,
    vertex_x_col = vertex_x_col,
    vertex_y_col = vertex_y_col
  )

  if (identical(resolved_shape, "rectangle")) {
    .gp3_hp_assert_columns(
      aoi_defs,
      c(
        left_col,
        right_col,
        top_col,
        bottom_col
      ),
      "aoi_defs"
    )
  } else {
    .gp3_hp_assert_columns(
      aoi_defs,
      c(
        vertex_x_col,
        vertex_y_col,
        vertex_order_col
      ),
      "aoi_defs"
    )
  }

  all_aoi_names <- unique(
    as.character(aoi_defs[[aoi_name_col]])
  )

  invalid_names <- is.na(all_aoi_names) |
    !nzchar(all_aoi_names)

  if (any(invalid_names)) {
    stop(
      "Dynamic AOI names must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  membership <- matrix(
    FALSE,
    nrow = nrow(master_df),
    ncol = length(all_aoi_names),
    dimnames = list(
      NULL,
      all_aoi_names
    )
  )

  matched_definition_time <- rep(
    NA_real_,
    nrow(master_df)
  )

  matched_time_gap <- rep(
    NA_real_,
    nrow(master_df)
  )

  sample_keys <- .gp3_dynamic_aoi_group_key(
    master_df,
    group_cols
  )

  definition_keys <- .gp3_dynamic_aoi_group_key(
    aoi_defs,
    group_cols
  )

  sample_groups <- split(
    seq_len(nrow(master_df)),
    sample_keys,
    drop = TRUE
  )

  x_all <- suppressWarnings(
    as.numeric(master_df[[x_col]])
  )

  y_all <- suppressWarnings(
    as.numeric(master_df[[y_col]])
  )

  sample_time_all <- suppressWarnings(
    as.numeric(master_df[[time_col]])
  )

  definition_time_all <- suppressWarnings(
    as.numeric(aoi_defs[[aoi_time_col]])
  )

  for (group_key in names(sample_groups)) {
    sample_idx <- sample_groups[[group_key]]
    definition_idx <- which(
      definition_keys == group_key
    )

    if (!length(definition_idx)) {
      next
    }

    definition_times_for_group <- definition_time_all[
      definition_idx
    ]

    available_times <- sort(
      unique(
        definition_times_for_group[
          is.finite(definition_times_for_group)
        ]
      )
    )

    if (!length(available_times)) {
      next
    }

    sample_times <- sample_time_all[sample_idx]

    selected_times <- vapply(
      sample_times,
      .gp3_match_dynamic_aoi_time,
      numeric(1),
      definition_times = available_times,
      match = match
    )

    time_gap <- abs(
      sample_times - selected_times
    )

    valid_match <- is.finite(sample_times) &
      is.finite(selected_times) &
      is.finite(time_gap) &
      time_gap <= max_time_gap

    selected_times[!valid_match] <- NA_real_
    time_gap[!valid_match] <- NA_real_

    matched_definition_time[sample_idx] <-
      selected_times

    matched_time_gap[sample_idx] <-
      time_gap

    unique_selected <- unique(
      selected_times[
        is.finite(selected_times)
      ]
    )

    for (definition_time in unique_selected) {
      local_sample_idx <- sample_idx[
        is.finite(selected_times) &
          selected_times == definition_time
      ]

      local_definition_idx <- definition_idx[
        is.finite(
          definition_time_all[definition_idx]
        ) &
          definition_time_all[definition_idx] ==
            definition_time
      ]

      if (!length(local_sample_idx) ||
          !length(local_definition_idx)) {
        next
      }

      local_defs <- aoi_defs[
        local_definition_idx,
        ,
        drop = FALSE
      ]

      local_membership <- if (
        identical(resolved_shape, "rectangle")
      ) {
        .gp3_rectangle_membership_matrix(
          x = x_all[local_sample_idx],
          y = y_all[local_sample_idx],
          definitions = local_defs,
          aoi_name_col = aoi_name_col,
          left_col = left_col,
          right_col = right_col,
          top_col = top_col,
          bottom_col = bottom_col
        )
      } else {
        polygon_definitions <-
          .gp3_prepare_polygon_definitions(
            vertices = local_defs,
            aoi_col = aoi_name_col,
            vertex_x_col = vertex_x_col,
            vertex_y_col = vertex_y_col,
            vertex_order_col = vertex_order_col
          )

        .gp3_polygon_membership_matrix(
          x = x_all[local_sample_idx],
          y = y_all[local_sample_idx],
          polygon_definitions =
            polygon_definitions,
          boundary = boundary
        )
      }

      membership[
        local_sample_idx,
        colnames(local_membership)
      ] <- local_membership
    }
  }

  valid_xy <- is.finite(x_all) &
    is.finite(y_all)

  out <- .gp3_apply_aoi_membership(
    master_df = master_df,
    membership = membership,
    output = output,
    prefix = prefix,
    label_col = label_col,
    outside_label = outside_label,
    overlap = overlap,
    include_overlap_count =
      include_overlap_count,
    valid_xy = valid_xy
  )

  out[[definition_time_col]] <-
    matched_definition_time

  out[[time_gap_col]] <-
    matched_time_gap

  if (output %in% c("label", "both")) {
    no_definition <- !is.finite(
      matched_definition_time
    )

    out[[label_col]][no_definition] <-
      NA_character_
  }

  attr(out, "gazepoint_dynamic_aoi_settings") <- list(
    shape = resolved_shape,
    group_cols = group_cols,
    match = match,
    max_time_gap = max_time_gap,
    aoi_time_col = aoi_time_col,
    aoi_name_col = aoi_name_col,
    definition_time_col = definition_time_col,
    time_gap_col = time_gap_col
  )

  .gp3_hp_restore_class(
    out,
    master_df
  )
}

#' Audit time-varying AOI coverage
#'
#' Summarise whether gaze samples could be matched to dynamic AOI definitions,
#' how large the definition-time gaps were, and how often samples fell inside
#' versus outside defined AOIs.
#'
#' @param data Output from [add_gazepoint_dynamic_aoi()].
#' @param label_col AOI-label column.
#' @param definition_time_col Matched-definition timestamp column.
#' @param time_gap_col Definition-time-gap column.
#' @param group_cols Optional summary grouping columns.
#' @param outside_label Outside-AOI label.
#' @param max_time_gap Optional audit threshold for definition-time gaps.
#' @param x_col,y_col Optional coordinate columns used to flag missing gaze.
#'
#' @return An object of class `"gp3_dynamic_aoi_coverage_audit"` containing
#'   overview, group, AOI, and flagged-row tables plus settings.
#'
#' @export
audit_gazepoint_dynamic_aoi_coverage <- function(
    data,
    label_col = "aoi_current",
    definition_time_col = "aoi_definition_time",
    time_gap_col = "aoi_time_gap",
    group_cols = NULL,
    outside_label = "outside",
    max_time_gap = Inf,
    x_col = NULL,
    y_col = NULL) {

  .gp3_hp_assert_data_frame(data, "data")
  group_cols <- unique(as.character(group_cols))

  .gp3_hp_assert_columns(
    data,
    unique(c(
      label_col,
      definition_time_col,
      time_gap_col,
      group_cols,
      x_col,
      y_col
    )),
    "data"
  )

  if (!is.numeric(max_time_gap) ||
      length(max_time_gap) != 1L ||
      is.na(max_time_gap) ||
      max_time_gap < 0) {
    stop(
      "`max_time_gap` must be one non-negative number or Inf.",
      call. = FALSE
    )
  }

  label <- as.character(
    data[[label_col]]
  )

  definition_time <- suppressWarnings(
    as.numeric(data[[definition_time_col]])
  )

  time_gap <- suppressWarnings(
    as.numeric(data[[time_gap_col]])
  )

  has_definition <- is.finite(
    definition_time
  )

  inside_aoi <- has_definition &
    !is.na(label) &
    label != outside_label

  outside_aoi <- has_definition &
    !is.na(label) &
    label == outside_label

  missing_gaze <- rep(
    FALSE,
    nrow(data)
  )

  if (!is.null(x_col) &&
      !is.null(y_col)) {
    x <- suppressWarnings(
      as.numeric(data[[x_col]])
    )

    y <- suppressWarnings(
      as.numeric(data[[y_col]])
    )

    missing_gaze <- !is.finite(x) |
      !is.finite(y)
  }

  excessive_gap <- has_definition &
    is.finite(time_gap) &
    time_gap > max_time_gap

  issue <- ifelse(
    missing_gaze,
    "missing_gaze",
    ifelse(
      !has_definition,
      "no_dynamic_definition",
      ifelse(
        excessive_gap,
        "definition_gap_exceeds_threshold",
        ifelse(
          outside_aoi,
          "outside_all_aoi",
          "ok"
        )
      )
    )
  )

  finite_gap <- time_gap[
    is.finite(time_gap)
  ]

  overview <- data.frame(
    n_rows = nrow(data),
    n_with_definition = sum(has_definition),
    pct_with_definition = .gp3_aoi_percent(
      sum(has_definition),
      nrow(data)
    ),
    n_inside_aoi = sum(inside_aoi),
    pct_inside_aoi = .gp3_aoi_percent(
      sum(inside_aoi),
      nrow(data)
    ),
    n_outside_aoi = sum(outside_aoi),
    pct_outside_aoi = .gp3_aoi_percent(
      sum(outside_aoi),
      nrow(data)
    ),
    n_missing_gaze = sum(missing_gaze),
    n_excessive_gap = sum(excessive_gap),
    mean_time_gap = if (length(finite_gap)) {
      mean(finite_gap)
    } else {
      NA_real_
    },
    max_time_gap_observed = if (length(finite_gap)) {
      max(finite_gap)
    } else {
      NA_real_
    },
    audit_status = if (
      all(issue == "ok")
    ) {
      "ok"
    } else {
      "review"
    },
    stringsAsFactors = FALSE
  )

  group_summary <- .gp3_dynamic_aoi_group_summary(
    data = data,
    group_cols = group_cols,
    has_definition = has_definition,
    inside_aoi = inside_aoi,
    outside_aoi = outside_aoi,
    missing_gaze = missing_gaze,
    excessive_gap = excessive_gap,
    time_gap = time_gap
  )

  aoi_levels <- sort(
    unique(
      label[
        !is.na(label) &
          label != outside_label
      ]
    )
  )

  aoi_summary <- if (length(aoi_levels)) {
    do.call(
      rbind,
      lapply(
        aoi_levels,
        function(aoi_name) {
          n <- sum(
            label == aoi_name,
            na.rm = TRUE
          )

          data.frame(
            aoi = aoi_name,
            n_samples = n,
            pct_all_samples = .gp3_aoi_percent(
              n,
              nrow(data)
            ),
            pct_defined_samples =
              .gp3_aoi_percent(
                n,
                sum(has_definition)
              ),
            stringsAsFactors = FALSE
          )
        }
      )
    )
  } else {
    data.frame(
      aoi = character(),
      n_samples = integer(),
      pct_all_samples = numeric(),
      pct_defined_samples = numeric(),
      stringsAsFactors = FALSE
    )
  }

  flagged_rows <- data[
    issue != "ok",
    ,
    drop = FALSE
  ]

  flagged_rows$dynamic_aoi_issue <-
    issue[issue != "ok"]

  out <- list(
    overview = overview,
    group_summary = group_summary,
    aoi_summary = aoi_summary,
    flagged_rows = flagged_rows,
    settings = list(
      label_col = label_col,
      definition_time_col =
        definition_time_col,
      time_gap_col = time_gap_col,
      group_cols = group_cols,
      outside_label = outside_label,
      max_time_gap = max_time_gap,
      x_col = x_col,
      y_col = y_col
    )
  )

  class(out) <- c(
    "gp3_dynamic_aoi_coverage_audit",
    "list"
  )

  out
}

.gp3_prepare_polygon_definitions <- function(
    vertices,
    aoi_col,
    vertex_x_col,
    vertex_y_col,
    vertex_order_col) {

  aoi_names <- as.character(
    vertices[[aoi_col]]
  )

  if (anyNA(aoi_names) ||
      any(!nzchar(aoi_names))) {
    stop(
      "Polygon AOI names must be non-missing and non-empty.",
      call. = FALSE
    )
  }

  groups <- split(
    seq_len(nrow(vertices)),
    aoi_names,
    drop = TRUE
  )

  polygons <- lapply(
    names(groups),
    function(aoi_name) {
      idx <- groups[[aoi_name]]

      if (!is.null(vertex_order_col)) {
        idx <- idx[
          order(
            vertices[[vertex_order_col]][idx],
            na.last = TRUE
          )
        ]
      }

      x <- suppressWarnings(
        as.numeric(
          vertices[[vertex_x_col]][idx]
        )
      )

      y <- suppressWarnings(
        as.numeric(
          vertices[[vertex_y_col]][idx]
        )
      )

      if (any(!is.finite(x)) ||
          any(!is.finite(y))) {
        stop(
          paste0(
            "Polygon `",
            aoi_name,
            "` contains non-finite vertices."
          ),
          call. = FALSE
        )
      }

      points <- unique(
        data.frame(
          x = x,
          y = y
        )
      )

      if (nrow(points) < 3L) {
        stop(
          paste0(
            "Polygon `",
            aoi_name,
            "` must contain at least three unique vertices."
          ),
          call. = FALSE
        )
      }

      list(
        name = aoi_name,
        x = points$x,
        y = points$y
      )
    }
  )

  names(polygons) <- names(groups)
  polygons
}

.gp3_polygon_membership_matrix <- function(
    x,
    y,
    polygon_definitions,
    boundary) {

  membership <- vapply(
    polygon_definitions,
    function(polygon) {
      .gp3_points_in_polygon(
        x = x,
        y = y,
        polygon_x = polygon$x,
        polygon_y = polygon$y,
        boundary = boundary
      )
    },
    logical(length(x))
  )

  membership <- matrix(
    membership,
    nrow = length(x),
    ncol = length(polygon_definitions),
    dimnames = list(
      NULL,
      names(polygon_definitions)
    )
  )

  membership
}

.gp3_points_in_polygon <- function(
    x,
    y,
    polygon_x,
    polygon_y,
    boundary) {

  n_points <- length(x)
  n_vertices <- length(polygon_x)
  output <- rep(FALSE, n_points)

  valid <- is.finite(x) &
    is.finite(y)

  if (!any(valid)) {
    return(output)
  }

  tolerance <- sqrt(
    .Machine$double.eps
  ) * max(
    1,
    abs(c(
      x[valid],
      y[valid],
      polygon_x,
      polygon_y
    ))
  )

  for (point_index in which(valid)) {
    point_x <- x[[point_index]]
    point_y <- y[[point_index]]

    on_boundary <- FALSE

    for (vertex_index in seq_len(n_vertices)) {
      next_index <- if (
        vertex_index == n_vertices
      ) {
        1L
      } else {
        vertex_index + 1L
      }

      if (.gp3_point_on_segment(
        point_x = point_x,
        point_y = point_y,
        x1 = polygon_x[[vertex_index]],
        y1 = polygon_y[[vertex_index]],
        x2 = polygon_x[[next_index]],
        y2 = polygon_y[[next_index]],
        tolerance = tolerance
      )) {
        on_boundary <- TRUE
        break
      }
    }

    if (on_boundary) {
      output[[point_index]] <-
        identical(boundary, "inside")
      next
    }

    inside <- FALSE
    previous <- n_vertices

    for (current in seq_len(n_vertices)) {
      yi <- polygon_y[[current]]
      yj <- polygon_y[[previous]]
      xi <- polygon_x[[current]]
      xj <- polygon_x[[previous]]

      crosses <- (yi > point_y) !=
        (yj > point_y)

      if (crosses) {
        intersection_x <- (
          (xj - xi) *
            (point_y - yi) /
            (yj - yi)
        ) + xi

        if (point_x < intersection_x) {
          inside <- !inside
        }
      }

      previous <- current
    }

    output[[point_index]] <- inside
  }

  output
}

.gp3_point_on_segment <- function(
    point_x,
    point_y,
    x1,
    y1,
    x2,
    y2,
    tolerance) {

  cross <- (
    point_y - y1
  ) * (
    x2 - x1
  ) - (
    point_x - x1
  ) * (
    y2 - y1
  )

  if (abs(cross) > tolerance) {
    return(FALSE)
  }

  within_x <- point_x >=
    min(x1, x2) - tolerance &&
    point_x <= max(x1, x2) +
      tolerance

  within_y <- point_y >=
    min(y1, y2) - tolerance &&
    point_y <= max(y1, y2) +
      tolerance

  within_x && within_y
}

.gp3_rectangle_membership_matrix <- function(
    x,
    y,
    definitions,
    aoi_name_col,
    left_col,
    right_col,
    top_col,
    bottom_col) {

  names_vector <- as.character(
    definitions[[aoi_name_col]]
  )

  if (anyDuplicated(names_vector)) {
    stop(
      paste0(
        "Rectangle AOI names must be unique within each ",
        "definition timestamp."
      ),
      call. = FALSE
    )
  }

  left <- suppressWarnings(
    as.numeric(definitions[[left_col]])
  )

  right <- suppressWarnings(
    as.numeric(definitions[[right_col]])
  )

  top <- suppressWarnings(
    as.numeric(definitions[[top_col]])
  )

  bottom <- suppressWarnings(
    as.numeric(definitions[[bottom_col]])
  )

  boundaries <- cbind(
    left,
    right,
    top,
    bottom
  )

  if (any(!is.finite(boundaries))) {
    stop(
      "Dynamic rectangle boundaries must be finite.",
      call. = FALSE
    )
  }

  xmin <- pmin(left, right)
  xmax <- pmax(left, right)
  ymin <- pmin(top, bottom)
  ymax <- pmax(top, bottom)

  valid <- is.finite(x) &
    is.finite(y)

  membership <- vapply(
    seq_along(names_vector),
    function(i) {
      valid &
        x >= xmin[[i]] &
        x <= xmax[[i]] &
        y >= ymin[[i]] &
        y <= ymax[[i]]
    },
    logical(length(x))
  )

  if (length(names_vector) == 1L) {
    membership <- matrix(
      membership,
      ncol = 1L,
      dimnames = list(
        NULL,
        names_vector
      )
    )
  } else {
    colnames(membership) <-
      names_vector
  }

  membership
}

.gp3_apply_aoi_membership <- function(
    master_df,
    membership,
    output,
    prefix,
    label_col,
    outside_label,
    overlap,
    include_overlap_count,
    valid_xy) {

  overlap_count <- rowSums(
    membership
  )

  if (identical(overlap, "error") &&
      any(overlap_count > 1L)) {
    stop(
      paste0(
        sum(overlap_count > 1L),
        " sample(s) fall inside overlapping AOIs."
      ),
      call. = FALSE
    )
  }

  out <- master_df

  if (output %in% c("logical", "both")) {
    logical_names <- paste0(
      prefix,
      make.names(
        colnames(membership),
        unique = TRUE
      )
    )

    for (i in seq_len(ncol(membership))) {
      out[[logical_names[[i]]]] <-
        membership[, i]
    }
  }

  if (output %in% c("label", "both")) {
    labels <- rep(
      outside_label,
      nrow(master_df)
    )

    labels[!valid_xy] <- NA_character_

    for (i in seq_len(ncol(membership))) {
      hit <- membership[, i]

      if (identical(overlap, "last")) {
        labels[hit] <- colnames(
          membership
        )[[i]]
      } else {
        labels[
          hit &
            labels == outside_label
        ] <- colnames(membership)[[i]]
      }
    }

    out[[label_col]] <- labels
  }

  if (isTRUE(include_overlap_count)) {
    out$aoi_overlap_count <-
      overlap_count
  }

  out
}

.gp3_resolve_dynamic_aoi_shape <- function(
    aoi_defs,
    shape,
    left_col,
    right_col,
    top_col,
    bottom_col,
    vertex_x_col,
    vertex_y_col) {

  if (!identical(shape, "auto")) {
    return(shape)
  }

  rectangle_columns <- c(
    left_col,
    right_col,
    top_col,
    bottom_col
  )

  polygon_columns <- c(
    vertex_x_col,
    vertex_y_col
  )

  has_rectangle <- all(
    rectangle_columns %in% names(aoi_defs)
  )

  has_polygon <- all(
    polygon_columns %in% names(aoi_defs)
  )

  if (has_rectangle && !has_polygon) {
    return("rectangle")
  }

  if (has_polygon && !has_rectangle) {
    return("polygon")
  }

  if (has_rectangle && has_polygon) {
    stop(
      paste0(
        "Both rectangle and polygon fields were found. ",
        "Set `shape` explicitly."
      ),
      call. = FALSE
    )
  }

  stop(
    paste0(
      "Could not infer dynamic AOI geometry. Supply rectangle ",
      "boundaries or polygon vertices."
    ),
    call. = FALSE
  )
}

.gp3_dynamic_aoi_group_key <- function(
    data,
    group_cols) {

  if (!nrow(data)) {
    return(character())
  }

  if (!length(group_cols)) {
    return(rep(".all", nrow(data)))
  }

  values <- lapply(
    data[group_cols],
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

.gp3_match_dynamic_aoi_time <- function(
    sample_time,
    definition_times,
    match) {

  if (!is.finite(sample_time)) {
    return(NA_real_)
  }

  if (identical(match, "nearest")) {
    distance <- abs(
      definition_times - sample_time
    )

    return(
      definition_times[
        which.min(distance)
      ]
    )
  }

  if (identical(match, "previous")) {
    candidates <- definition_times[
      definition_times <= sample_time
    ]

    if (!length(candidates)) {
      return(NA_real_)
    }

    return(max(candidates))
  }

  candidates <- definition_times[
    definition_times >= sample_time
  ]

  if (!length(candidates)) {
    return(NA_real_)
  }

  min(candidates)
}

.gp3_dynamic_aoi_group_summary <- function(
    data,
    group_cols,
    has_definition,
    inside_aoi,
    outside_aoi,
    missing_gaze,
    excessive_gap,
    time_gap) {

  keys <- .gp3_dynamic_aoi_group_key(
    data,
    group_cols
  )

  groups <- split(
    seq_len(nrow(data)),
    keys,
    drop = TRUE
  )

  rows <- lapply(
    groups,
    function(idx) {
      group_values <- if (length(group_cols)) {
        data[
          idx[[1L]],
          group_cols,
          drop = FALSE
        ]
      } else {
        data.frame(
          stringsAsFactors = FALSE
        )
      }

      finite_gap <- time_gap[idx][
        is.finite(time_gap[idx])
      ]

      cbind(
        group_values,
        data.frame(
          n_rows = length(idx),
          n_with_definition =
            sum(has_definition[idx]),
          pct_with_definition =
            .gp3_aoi_percent(
              sum(has_definition[idx]),
              length(idx)
            ),
          n_inside_aoi =
            sum(inside_aoi[idx]),
          pct_inside_aoi =
            .gp3_aoi_percent(
              sum(inside_aoi[idx]),
              length(idx)
            ),
          n_outside_aoi =
            sum(outside_aoi[idx]),
          n_missing_gaze =
            sum(missing_gaze[idx]),
          n_excessive_gap =
            sum(excessive_gap[idx]),
          mean_time_gap =
            if (length(finite_gap)) {
              mean(finite_gap)
            } else {
              NA_real_
            },
          stringsAsFactors = FALSE
        )
      )
    }
  )

  out <- do.call(
    rbind,
    rows
  )

  rownames(out) <- NULL
  out
}

.gp3_aoi_percent <- function(
    numerator,
    denominator) {

  if (!is.finite(denominator) ||
      denominator <= 0) {
    return(NA_real_)
  }

  100 * numerator / denominator
}
