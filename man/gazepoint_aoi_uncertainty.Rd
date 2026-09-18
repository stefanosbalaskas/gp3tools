\name{gazepoint_aoi_uncertainty}
\alias{gazepoint_aoi_uncertainty}
\alias{audit_gazepoint_aoi_uncertainty}
\alias{run_gazepoint_aoi_sensitivity}
\alias{plot_gazepoint_aoi_sensitivity}
\title{Gazepoint Adapters for AOI Perturbation and Uncertainty Analysis}
\description{
Thin Gazepoint convenience adapters for the vendor-neutral AOI perturbation
and uncertainty framework in eyeprocess. The adapters standardize Gazepoint
coordinate columns and explicit coordinate units, preserve adapter provenance,
and delegate geometry validation, perturbation, reassignment, feature
recomputation, model propagation, summaries, and plotting to eyeprocess.
}
\usage{
audit_gazepoint_aoi_uncertainty(
  data, aoi_geometry, gaze_x_col = NULL, gaze_y_col = NULL,
  geometry_aoi_col = NULL, coordinate_unit = c("px", "normalized"),
  screen_width_px = NULL, screen_height_px = NULL
)

run_gazepoint_aoi_sensitivity(
  data, aoi_geometry, grid = NULL,
  gaze_x_col = NULL, gaze_y_col = NULL, geometry_aoi_col = NULL,
  coordinate_unit = c("px", "normalized"),
  perturbation_unit = c("px", "deg"),
  dilations = NULL, erosions = NULL,
  translations_x = NULL, translations_y = NULL,
  translations_xy = NULL, jitters = NULL, anisotropic = NULL,
  screen_width_px = NULL, screen_height_px = NULL,
  viewing_distance = NULL, physical_screen_size = NULL,
  observation_id_col = NULL, participant_col = NULL, trial_col = NULL,
  duration_col = NULL, time_col = NULL,
  observation_level = c("fixation", "sample"),
  overlap_policy = c("ambiguous", "all", "error"),
  model_callback = NULL,
  preprocessing_specification = NULL, event_detector = NULL,
  quality_rules = NULL, model_specification = NULL,
  seed = 20260918L,
  boundary_policy = c("warn", "clip", "error", "allow")
)

plot_gazepoint_aoi_sensitivity(
  x,
  type = c("perturbations", "assignment", "coefficient", "surface"),
  ...
)
}
\arguments{
\item{data}{Gazepoint sample- or fixation-level data.}
\item{aoi_geometry}{Gazepoint rectangular geometry or an eyeprocess-compatible AOI table.}
\item{grid}{Optional AOI perturbation grid created by eyeprocess.}
\item{gaze_x_col, gaze_y_col}{Optional source gaze coordinate columns.}
\item{geometry_aoi_col}{Optional AOI identifier column for Gazepoint geometry.}
\item{coordinate_unit}{Explicit source coordinate unit, either pixels or normalized coordinates.}
\item{perturbation_unit}{Unit used for perturbation values, either pixels or degrees of visual angle.}
\item{dilations, erosions, translations_x, translations_y, translations_xy, jitters, anisotropic}{Perturbation values forwarded unchanged to eyeprocess when a grid is not supplied. `translations_xy` contains explicit horizontal/vertical translation pairs.}
\item{screen_width_px, screen_height_px}{Display dimensions in pixels. Required for conversion of normalized coordinates.}
\item{viewing_distance}{Viewing distance required for degree-based perturbations unless the supplied core grid already contains conversion metadata.}
\item{physical_screen_size}{Physical screen width and height in the same unit as viewing distance.}
\item{observation_id_col, participant_col, trial_col, duration_col, time_col}{Optional columns forwarded to the core sensitivity workflow.}
\item{observation_level}{Explicit source level, fixation or sample, forwarded unchanged to eyeprocess.}
\item{overlap_policy}{Explicit overlap handling. The default preserves ambiguity.}
\item{model_callback}{Optional fixed statistical-model callback forwarded unchanged to eyeprocess.}
\item{preprocessing_specification, event_detector, quality_rules, model_specification}{Provenance fields forwarded to eyeprocess.}
\item{seed}{Reproducible seed for jitter branches.}
\item{boundary_policy}{Explicit screen-boundary policy.}
\item{x}{A gp3_aoi_sensitivity object.}
\item{type}{Delegated plot family.}
\item{...}{Arguments passed to the selected eyeprocess plotting function.}
}
\details{
The wrappers do not contain the scientific AOI perturbation or stability
algorithms. They resolve the corresponding exported eyeprocess functions at
runtime and preserve the returned core result unchanged inside
\code{core_result}.

Normalized Gazepoint coordinates are never treated as pixels implicitly.
Positive screen dimensions must be supplied before conversion. Degree-based
perturbations additionally require viewing geometry through the eyeprocess
core.

The adapter deliberately does not provide a first-match default for overlap;
the default core policy is explicit ambiguity.
}
\value{
\code{audit_gazepoint_aoi_uncertainty()} returns a
\code{gp3_aoi_uncertainty_audit}. \code{run_gazepoint_aoi_sensitivity()}
returns a \code{gp3_aoi_sensitivity} containing the untouched eyeprocess core
result plus Gazepoint adapter settings. The plotting helper returns its input
invisibly after delegating the plot.
}
\examples{
\dontrun{
# Requires a current eyeprocess development/release build with
# AOI perturbation functions.
result <- run_gazepoint_aoi_sensitivity(
  data = gaze,
  aoi_geometry = aois,
  gaze_x_col = "FPOGX",
  gaze_y_col = "FPOGY",
  coordinate_unit = "normalized",
  perturbation_unit = "deg",
  observation_level = "sample",
  dilations = c(0.25, 0.5, 1),
  erosions = 0.25,
  translations_xy = list(c(0.25, -0.25)),
  screen_width_px = 1920,
  screen_height_px = 1080,
  viewing_distance = 60,
  physical_screen_size = c(53.1, 29.9)
)
plot_gazepoint_aoi_sensitivity(result, "assignment")
}
}
