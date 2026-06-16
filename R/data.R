#' Example Gazepoint master table
#'
#' A lightweight synthetic Gazepoint-style sample-level master table for examples,
#' tests, README workflows, and vignettes. The data are artificial and are not
#' from a real participant study.
#'
#' @format A tibble with sample-level rows and columns including:
#' \describe{
#'   \item{subject}{Synthetic participant identifier.}
#'   \item{USER_FILE}{Gazepoint-style participant/file identifier.}
#'   \item{MEDIA_ID}{Synthetic stimulus identifier.}
#'   \item{trial_global}{Synthetic trial identifier.}
#'   \item{condition}{Synthetic experimental condition.}
#'   \item{time}{Sample time in milliseconds.}
#'   \item{x, y}{Normalised gaze coordinates.}
#'   \item{pupil}{Synthetic pupil value.}
#'   \item{valid}{Logical gaze/pupil validity flag.}
#'   \item{artifact}{Logical synthetic pupil-artifact flag.}
#'   \item{aoi_current}{Synthetic AOI state.}
#'   \item{is_fixation, is_saccade}{Synthetic fixation and saccade indicators.}
#'   \item{event_label}{Synthetic event marker.}
#' }
#'
#' @examples
#' data(gazepoint_example_master)
#' head(gazepoint_example_master)
#'
#' @keywords datasets
"gazepoint_example_master"

#' Example Gazepoint fixation table
#'
#' A lightweight synthetic Gazepoint-style fixation table for examples,
#' tests, README workflows, and vignettes. The data are artificial and are not
#' from a real participant study.
#'
#' @format A tibble with fixation-level rows and columns including:
#' \describe{
#'   \item{USER_FILE}{Synthetic participant/file identifier.}
#'   \item{subject}{Synthetic participant identifier.}
#'   \item{MEDIA_ID}{Synthetic stimulus identifier.}
#'   \item{trial_global}{Synthetic trial identifier.}
#'   \item{condition}{Synthetic experimental condition.}
#'   \item{FPOGID}{Synthetic fixation identifier.}
#'   \item{FPOGS}{Synthetic fixation start time.}
#'   \item{FPOGD}{Synthetic fixation duration.}
#'   \item{FPOGX, FPOGY}{Synthetic fixation coordinates.}
#'   \item{FPOGV}{Synthetic fixation validity flag.}
#'   \item{AOI}{Synthetic AOI label.}
#' }
#'
#' @examples
#' data(gazepoint_example_fixations)
#' head(gazepoint_example_fixations)
#'
#' @keywords datasets
"gazepoint_example_fixations"

#' Example AOI geometry table
#'
#' A lightweight synthetic AOI geometry table for AOI-verification examples.
#' Coordinates are normalised to a 0--1 screen coordinate system.
#'
#' @format A tibble with one row per stimulus and AOI, including:
#' \describe{
#'   \item{media_id}{Synthetic stimulus identifier.}
#'   \item{aoi}{Synthetic AOI label.}
#'   \item{x_min, y_min, x_max, y_max}{Normalised rectangular AOI boundaries.}
#' }
#'
#' @examples
#' data(gazepoint_example_aoi_geometry)
#' gazepoint_example_aoi_geometry
#'
#' @keywords datasets
"gazepoint_example_aoi_geometry"

#' Example AOI-window summary table
#'
#' A lightweight synthetic AOI-window summary table created from
#' \code{gazepoint_example_master}. It can be used in examples for AOI-window
#' denominator checks, GLMM preparation, and AOI-window modelling.
#'
#' @format A tibble with one row per participant, stimulus/trial, and AOI time
#' window.
#'
#' @examples
#' data(gazepoint_example_aoi_windows)
#' head(gazepoint_example_aoi_windows)
#'
#' @keywords datasets
"gazepoint_example_aoi_windows"

#' Example pupil-window summary table
#'
#' A lightweight synthetic pupil-window summary table created from
#' \code{gazepoint_example_master}. It can be used in examples for pupil-window
#' model-data preparation and confirmatory pupil-window modelling.
#'
#' @format A tibble with one row per participant, stimulus/trial, condition, and
#' pupil time window.
#'
#' @examples
#' data(gazepoint_example_pupil_windows)
#' head(gazepoint_example_pupil_windows)
#'
#' @keywords datasets
"gazepoint_example_pupil_windows"
