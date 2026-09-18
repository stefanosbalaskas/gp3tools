# Thin Gazepoint adapters for eyeprocess AOI uncertainty ----------------------

.gp3_aoi_uncertainty_core_fun <- function(name) {
  override <- getOption("gp3tools.eyeprocess_functions", NULL)
  if (is.list(override) && is.function(override[[name]])) return(override[[name]])
  fun <- tryCatch(getExportedValue("eyeprocess", name), error = function(e) NULL)
  if (!is.function(fun)) {
    stop(
      "The AOI uncertainty adapter requires an eyeprocess build exporting ",
      name, ". Install or update eyeprocess before running this adapter.",
      call. = FALSE
    )
  }
  fun
}

.gp3_aoi_uncertainty_detect_col <- function(data, supplied, candidates, arg) {
  if (!is.null(supplied)) {
    if (!is.character(supplied) || length(supplied) != 1L || !supplied %in% names(data)) {
      stop(arg, " must name a column in data.", call. = FALSE)
    }
    return(supplied)
  }
  found <- candidates[candidates %in% names(data)]
  if (!length(found)) stop(arg, " could not be detected and must be supplied.", call. = FALSE)
  found[1L]
}

.gp3_aoi_uncertainty_prepare_data <- function(
    data, gaze_x_col, gaze_y_col, coordinate_unit,
    screen_width_px, screen_height_px) {
  if (!is.data.frame(data) || !nrow(data)) stop("data must be a non-empty data frame.", call. = FALSE)
  x_col <- .gp3_aoi_uncertainty_detect_col(
    data, gaze_x_col,
    c("x", "gaze_x", "gaze_x_px", "gaze_x_norm", "FPOGX", "BPOGX"),
    "gaze_x_col"
  )
  y_col <- .gp3_aoi_uncertainty_detect_col(
    data, gaze_y_col,
    c("y", "gaze_y", "gaze_y_px", "gaze_y_norm", "FPOGY", "BPOGY"),
    "gaze_y_col"
  )
  out <- data
  out$.gp3_aoi_x <- suppressWarnings(as.numeric(out[[x_col]]))
  out$.gp3_aoi_y <- suppressWarnings(as.numeric(out[[y_col]]))
  if (coordinate_unit == "normalized") {
    if (is.null(screen_width_px) || is.null(screen_height_px) ||
        !is.finite(screen_width_px) || !is.finite(screen_height_px) ||
        screen_width_px <= 0 || screen_height_px <= 0) {
      stop(
        "Normalized Gazepoint coordinates require positive screen_width_px and screen_height_px; units are never changed silently.",
        call. = FALSE
      )
    }
    out$.gp3_aoi_x <- out$.gp3_aoi_x * screen_width_px
    out$.gp3_aoi_y <- out$.gp3_aoi_y * screen_height_px
  }
  list(
    data = out, x_col = ".gp3_aoi_x", y_col = ".gp3_aoi_y",
    source_x_col = x_col, source_y_col = y_col
  )
}

.gp3_aoi_uncertainty_scale_geometry <- function(
    geometry, coordinate_unit, screen_width_px, screen_height_px) {
  out <- geometry
  if (coordinate_unit != "normalized") return(out)
  if (is.null(screen_width_px) || is.null(screen_height_px) ||
      !is.finite(screen_width_px) || !is.finite(screen_height_px) ||
      screen_width_px <= 0 || screen_height_px <= 0) {
    stop("Normalized AOI geometry requires positive screen dimensions.", call. = FALSE)
  }
  if (all(c("xmin", "xmax", "ymin", "ymax") %in% names(out))) {
    out$xmin <- as.numeric(out$xmin) * screen_width_px
    out$xmax <- as.numeric(out$xmax) * screen_width_px
    out$ymin <- as.numeric(out$ymin) * screen_height_px
    out$ymax <- as.numeric(out$ymax) * screen_height_px
  }
  if ("polygon" %in% names(out)) {
    out$polygon <- I(lapply(out$polygon, function(p) {
      if (is.null(p)) return(NULL)
      p <- as.matrix(p)
      p[, 1L] <- p[, 1L] * screen_width_px
      p[, 2L] <- p[, 2L] * screen_height_px
      p
    }))
  }
  out
}

.gp3_aoi_uncertainty_prepare_geometry <- function(
    aoi_geometry, coordinate_unit, screen_width_px, screen_height_px,
    geometry_aoi_col = NULL) {
  if (!is.data.frame(aoi_geometry) || !nrow(aoi_geometry)) {
    stop("aoi_geometry must be a non-empty data frame.", call. = FALSE)
  }
  direct <- "aoi_id" %in% names(aoi_geometry) &&
    (all(c("xmin", "xmax", "ymin", "ymax") %in% names(aoi_geometry)) ||
       "polygon" %in% names(aoi_geometry))
  if (direct) {
    geometry <- aoi_geometry
    audit <- NULL
  } else {
    if (coordinate_unit == "px" &&
        (is.null(screen_width_px) || is.null(screen_height_px))) {
      stop(
        "Pixel AOI geometry parsed through the Gazepoint audit requires screen_width_px and screen_height_px.",
        call. = FALSE
      )
    }
    audit <- audit_gazepoint_aoi_geometry(
      data = aoi_geometry,
      aoi_col = geometry_aoi_col,
      screen_x_range = if (coordinate_unit == "normalized") c(0, 1) else c(0, screen_width_px),
      screen_y_range = if (coordinate_unit == "normalized") c(0, 1) else c(0, screen_height_px),
      require_within_screen = FALSE
    )
    aoi_col <- audit$settings$value[audit$settings$setting == "aoi_col"]
    if (!length(aoi_col) || is.na(aoi_col) || !nzchar(aoi_col)) {
      stop("Could not resolve the AOI identifier column from the Gazepoint geometry audit.", call. = FALSE)
    }
    g <- audit$geometry_summary
    geometry <- data.frame(
      aoi_id = as.character(g[[aoi_col]]),
      xmin = g$x_min, xmax = g$x_max,
      ymin = g$y_min, ymax = g$y_max,
      stringsAsFactors = FALSE
    )
  }
  list(
    geometry = .gp3_aoi_uncertainty_scale_geometry(
      geometry, coordinate_unit, screen_width_px, screen_height_px
    ),
    geometry_audit = audit
  )
}

#' Audit Gazepoint AOI geometry for uncertainty analysis
#'
#' Thin Gazepoint adapter that standardizes vendor coordinates and delegates
#' geometry validation to eyeprocess.
#'
#' @param data Gazepoint sample- or fixation-level data.
#' @param aoi_geometry Gazepoint AOI geometry or an eyeprocess-compatible table.
#' @param gaze_x_col,gaze_y_col Optional gaze coordinate columns.
#' @param geometry_aoi_col Optional Gazepoint AOI identifier column.
#' @param coordinate_unit Explicit source coordinate unit: px or normalized.
#' @param screen_width_px,screen_height_px Screen dimensions in pixels.
#' @return A gp3_aoi_uncertainty_audit object.
#' @export
audit_gazepoint_aoi_uncertainty <- function(
    data, aoi_geometry, gaze_x_col = NULL, gaze_y_col = NULL,
    geometry_aoi_col = NULL, coordinate_unit = c("px", "normalized"),
    screen_width_px = NULL, screen_height_px = NULL) {
  coordinate_unit <- match.arg(coordinate_unit)
  prepared_data <- .gp3_aoi_uncertainty_prepare_data(
    data, gaze_x_col, gaze_y_col, coordinate_unit,
    screen_width_px, screen_height_px
  )
  prepared_geometry <- .gp3_aoi_uncertainty_prepare_geometry(
    aoi_geometry, coordinate_unit, screen_width_px, screen_height_px,
    geometry_aoi_col
  )
  validation <- .gp3_aoi_uncertainty_core_fun("validate_aoi_geometry")(
    prepared_geometry$geometry
  )
  out <- list(
    core_validation = validation,
    gazepoint_geometry_audit = prepared_geometry$geometry_audit,
    overview = tibble::tibble(
      n_rows = nrow(data),
      n_missing_coordinates = sum(
        !is.finite(prepared_data$data$.gp3_aoi_x) |
          !is.finite(prepared_data$data$.gp3_aoi_y)
      ),
      n_aois = nrow(prepared_geometry$geometry),
      coordinate_unit = coordinate_unit,
      core_status = validation$status
    ),
    adapter_settings = list(
      source_x_col = prepared_data$source_x_col,
      source_y_col = prepared_data$source_y_col,
      coordinate_unit = coordinate_unit,
      screen_width_px = screen_width_px,
      screen_height_px = screen_height_px
    )
  )
  class(out) <- c("gp3_aoi_uncertainty_audit", "list")
  out
}

#' Run Gazepoint AOI perturbation sensitivity analysis
#'
#' Thin adapter around the eyeprocess AOI sensitivity core. Scientific
#' perturbation, reassignment, feature, and model logic remains in eyeprocess.
#'
#' @param data Gazepoint sample- or fixation-level data.
#' @param aoi_geometry Gazepoint AOI geometry or an eyeprocess-compatible table.
#' @param grid Optional eyeprocess perturbation grid.
#' @param gaze_x_col,gaze_y_col Optional gaze coordinate columns.
#' @param geometry_aoi_col Optional Gazepoint AOI identifier column.
#' @param coordinate_unit Explicit source coordinate unit.
#' @param perturbation_unit Perturbation unit: px or deg.
#' @param dilations,erosions,translations_x,translations_y,jitters,anisotropic Perturbation values.
#' @param screen_width_px,screen_height_px Display dimensions in pixels.
#' @param viewing_distance Viewing distance for degree conversion.
#' @param physical_screen_size Physical screen width and height.
#' @param observation_id_col,participant_col,trial_col,duration_col,time_col Optional analysis columns.
#' @param overlap_policy Explicit overlap handling.
#' @param model_callback Optional fixed model callback.
#' @param preprocessing_specification,event_detector,quality_rules,model_specification Provenance fields.
#' @param seed Reproducible jitter seed.
#' @param boundary_policy Explicit screen-boundary policy.
#' @return A gp3_aoi_sensitivity object containing the untouched core result.
#' @export
run_gazepoint_aoi_sensitivity <- function(
    data, aoi_geometry, grid = NULL,
    gaze_x_col = NULL, gaze_y_col = NULL, geometry_aoi_col = NULL,
    coordinate_unit = c("px", "normalized"),
    perturbation_unit = c("px", "deg"),
    dilations = NULL, erosions = NULL,
    translations_x = NULL, translations_y = NULL,
    jitters = NULL, anisotropic = NULL,
    screen_width_px = NULL, screen_height_px = NULL,
    viewing_distance = NULL, physical_screen_size = NULL,
    observation_id_col = NULL, participant_col = NULL, trial_col = NULL,
    duration_col = NULL, time_col = NULL,
    overlap_policy = c("ambiguous", "all", "error"),
    model_callback = NULL,
    preprocessing_specification = NULL, event_detector = NULL,
    quality_rules = NULL, model_specification = NULL,
    seed = 20260918L,
    boundary_policy = c("warn", "clip", "error", "allow")) {
  coordinate_unit <- match.arg(coordinate_unit)
  perturbation_unit <- match.arg(perturbation_unit)
  overlap_policy <- match.arg(overlap_policy)
  boundary_policy <- match.arg(boundary_policy)

  prepared_data <- .gp3_aoi_uncertainty_prepare_data(
    data, gaze_x_col, gaze_y_col, coordinate_unit,
    screen_width_px, screen_height_px
  )
  prepared_geometry <- .gp3_aoi_uncertainty_prepare_geometry(
    aoi_geometry, coordinate_unit, screen_width_px, screen_height_px,
    geometry_aoi_col
  )

  if (is.null(grid)) {
    create_grid <- .gp3_aoi_uncertainty_core_fun("create_aoi_perturbation_grid")
    grid_args <- list(
      dilations = dilations, erosions = erosions,
      translations_x = translations_x, translations_y = translations_y,
      jitters = jitters, anisotropic = anisotropic,
      unit = perturbation_unit, include_baseline = TRUE,
      seed = seed, boundary_policy = boundary_policy
    )
    if (!is.null(screen_width_px)) grid_args$screen_width_px <- screen_width_px
    if (!is.null(screen_height_px)) grid_args$screen_height_px <- screen_height_px
    if (!is.null(viewing_distance)) grid_args$viewing_distance <- viewing_distance
    if (!is.null(physical_screen_size)) grid_args$physical_screen_size <- physical_screen_size
    grid <- do.call(create_grid, grid_args)
  }

  core <- .gp3_aoi_uncertainty_core_fun("run_aoi_sensitivity_analysis")(
    data = prepared_data$data,
    aois = prepared_geometry$geometry,
    grid = grid,
    x_col = prepared_data$x_col,
    y_col = prepared_data$y_col,
    observation_id_col = observation_id_col,
    participant_col = participant_col,
    trial_col = trial_col,
    duration_col = duration_col,
    time_col = time_col,
    overlap_policy = overlap_policy,
    model_callback = model_callback,
    preprocessing_specification = preprocessing_specification,
    event_detector = event_detector,
    quality_rules = quality_rules,
    model_specification = model_specification
  )

  out <- list(
    core_result = core,
    gazepoint_geometry_audit = prepared_geometry$geometry_audit,
    adapter_settings = list(
      source_x_col = prepared_data$source_x_col,
      source_y_col = prepared_data$source_y_col,
      coordinate_unit = coordinate_unit,
      perturbation_unit = perturbation_unit,
      screen_width_px = screen_width_px,
      screen_height_px = screen_height_px,
      viewing_distance = viewing_distance,
      physical_screen_size = physical_screen_size,
      overlap_policy = overlap_policy,
      boundary_policy = boundary_policy
    )
  )
  class(out) <- c("gp3_aoi_sensitivity", "list")
  out
}

#' Plot Gazepoint AOI sensitivity
#'
#' Delegates plotting to the corresponding eyeprocess visual diagnostic.
#'
#' @param x A gp3_aoi_sensitivity object.
#' @param type Plot type: perturbations, assignment, coefficient, or surface.
#' @param ... Arguments passed to the delegated eyeprocess plotting function.
#' @return Invisibly returns x.
#' @export
plot_gazepoint_aoi_sensitivity <- function(
    x, type = c("perturbations", "assignment", "coefficient", "surface"), ...) {
  if (!inherits(x, "gp3_aoi_sensitivity")) {
    stop("x must be returned by run_gazepoint_aoi_sensitivity().", call. = FALSE)
  }
  type <- match.arg(type)
  fn <- switch(
    type,
    perturbations = "plot_aoi_perturbations",
    assignment = "plot_aoi_assignment_stability",
    coefficient = "plot_aoi_coefficient_stability",
    surface = "plot_aoi_robustness_surface"
  )
  .gp3_aoi_uncertainty_core_fun(fn)(x$core_result, ...)
  invisible(x)
}
