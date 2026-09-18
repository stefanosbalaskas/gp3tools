test_that("Gazepoint uncertainty audit delegates geometry validation", {
  calls <- new.env(parent = emptyenv())
  core <- list(
    validate_aoi_geometry = function(aois) {
      calls$validated <- aois
      structure(list(status = "valid", geometry = aois), class = "eye_aoi_geometry_validation")
    }
  )
  old <- options(gp3tools.eyeprocess_functions = core)
  on.exit(options(old), add = TRUE)

  data <- data.frame(FPOGX = c(.25, NA), FPOGY = c(.5, .2))
  aois <- data.frame(
    aoi_id = "claim", xmin = .1, xmax = .4, ymin = .2, ymax = .6
  )

  audit <- audit_gazepoint_aoi_uncertainty(
    data, aois,
    coordinate_unit = "normalized",
    screen_width_px = 1000,
    screen_height_px = 800
  )

  expect_s3_class(audit, "gp3_aoi_uncertainty_audit")
  expect_equal(calls$validated$xmin, 100)
  expect_equal(calls$validated$ymax, 480)
  expect_equal(audit$overview$n_missing_coordinates, 1)
  expect_equal(audit$adapter_settings$source_x_col, "FPOGX")
})

test_that("normalized coordinates are never converted without screen dimensions", {
  data <- data.frame(x = .2, y = .3)
  aois <- data.frame(aoi_id = "a", xmin = .1, xmax = .4, ymin = .1, ymax = .4)
  expect_error(
    audit_gazepoint_aoi_uncertainty(
      data, aois, coordinate_unit = "normalized"
    ),
    "require positive"
  )
})

test_that("run adapter delegates grid creation and analysis without scientific duplication", {
  calls <- new.env(parent = emptyenv())
  core <- list(
    create_aoi_perturbation_grid = function(...) {
      calls$grid_args <- list(...)
      structure(list(specifications = list(), table = data.frame()), class = "eye_aoi_perturbation_grid")
    },
    run_aoi_sensitivity_analysis = function(
        data, aois, grid, x_col, y_col, observation_id_col = NULL,
        participant_col = NULL, trial_col = NULL, duration_col = NULL,
        time_col = NULL, overlap_policy = "ambiguous", model_callback = NULL,
        preprocessing_specification = NULL, event_detector = NULL,
        quality_rules = NULL, model_specification = NULL) {
      calls$data <- data
      calls$aois <- aois
      calls$x_col <- x_col
      calls$y_col <- y_col
      calls$overlap_policy <- overlap_policy
      calls$event_detector <- event_detector
      structure(
        list(
          models = data.frame(),
          stability = list(overall = data.frame()),
          grid = grid
        ),
        class = "eye_aoi_sensitivity"
      )
    }
  )
  old <- options(gp3tools.eyeprocess_functions = core)
  on.exit(options(old), add = TRUE)

  data <- data.frame(
    id = 1:2,
    participant = c("p1", "p1"),
    FPOGX = c(.25, .75),
    FPOGY = c(.50, .60),
    duration = c(.1, .2)
  )
  aois <- data.frame(
    aoi_id = c("claim", "cta"),
    xmin = c(.1, .6), xmax = c(.4, .9),
    ymin = c(.2, .5), ymax = c(.6, .8)
  )

  result <- run_gazepoint_aoi_sensitivity(
    data, aois,
    coordinate_unit = "normalized",
    perturbation_unit = "deg",
    dilations = c(.25, .5),
    translations_x = .25,
    screen_width_px = 1000,
    screen_height_px = 800,
    viewing_distance = 60,
    physical_screen_size = c(53.1, 29.9),
    observation_id_col = "id",
    participant_col = "participant",
    duration_col = "duration",
    event_detector = "native_fixations"
  )

  expect_s3_class(result, "gp3_aoi_sensitivity")
  expect_equal(calls$data$.gp3_aoi_x, c(250, 750))
  expect_equal(calls$data$.gp3_aoi_y, c(400, 480))
  expect_equal(calls$aois$xmin, c(100, 600))
  expect_equal(calls$grid_args$unit, "deg")
  expect_equal(calls$grid_args$dilations, c(.25, .5))
  expect_equal(calls$overlap_policy, "ambiguous")
  expect_equal(calls$event_detector, "native_fixations")
  expect_equal(result$adapter_settings$coordinate_unit, "normalized")
})

test_that("supplied core grid bypasses grid construction", {
  calls <- new.env(parent = emptyenv())
  grid <- structure(list(id = "supplied"), class = "eye_aoi_perturbation_grid")
  core <- list(
    create_aoi_perturbation_grid = function(...) stop("should not be called"),
    run_aoi_sensitivity_analysis = function(data, aois, grid, ...) {
      calls$grid <- grid
      structure(
        list(models = data.frame(), stability = list(overall = data.frame()), grid = grid),
        class = "eye_aoi_sensitivity"
      )
    }
  )
  old <- options(gp3tools.eyeprocess_functions = core)
  on.exit(options(old), add = TRUE)

  result <- run_gazepoint_aoi_sensitivity(
    data.frame(x = 10, y = 20),
    data.frame(aoi_id = "a", xmin = 0, xmax = 30, ymin = 0, ymax = 30),
    grid = grid,
    gaze_x_col = "x",
    gaze_y_col = "y",
    coordinate_unit = "px"
  )

  expect_identical(calls$grid, grid)
  expect_identical(result$core_result$grid, grid)
})

test_that("plot adapter delegates all four plot families", {
  called <- new.env(parent = emptyenv())
  plot_stub <- function(name) {
    force(name)
    function(x, ...) {
      called[[name]] <- TRUE
      invisible(x)
    }
  }
  core <- list(
    plot_aoi_perturbations = plot_stub("perturbations"),
    plot_aoi_assignment_stability = plot_stub("assignment"),
    plot_aoi_coefficient_stability = plot_stub("coefficient"),
    plot_aoi_robustness_surface = plot_stub("surface")
  )
  old <- options(gp3tools.eyeprocess_functions = core)
  on.exit(options(old), add = TRUE)

  x <- structure(list(core_result = list()), class = "gp3_aoi_sensitivity")
  expect_silent(plot_gazepoint_aoi_sensitivity(x, "perturbations"))
  expect_silent(plot_gazepoint_aoi_sensitivity(x, "assignment"))
  expect_silent(plot_gazepoint_aoi_sensitivity(x, "coefficient", term = "condition"))
  expect_silent(plot_gazepoint_aoi_sensitivity(x, "surface"))
  expect_true(all(vapply(
    c("perturbations", "assignment", "coefficient", "surface"),
    function(nm) isTRUE(called[[nm]]),
    logical(1)
  )))
})

test_that("adapter reports a clear missing-core error", {
  old <- options(gp3tools.eyeprocess_functions = list())
  on.exit(options(old), add = TRUE)

  # Only assert this branch when the development eyeprocess core is unavailable.
  core <- tryCatch(getExportedValue("eyeprocess", "validate_aoi_geometry"), error = function(e) NULL)
  if (is.function(core)) skip("eyeprocess AOI core is installed in this environment")

  expect_error(
    audit_gazepoint_aoi_uncertainty(
      data.frame(x = 1, y = 1),
      data.frame(aoi_id = "a", xmin = 0, xmax = 2, ymin = 0, ymax = 2),
      gaze_x_col = "x", gaze_y_col = "y", coordinate_unit = "px"
    ),
    "requires an eyeprocess build"
  )
})
