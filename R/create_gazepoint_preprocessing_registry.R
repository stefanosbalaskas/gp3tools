#' Create a Gazepoint pupil-preprocessing registry
#'
#' Creates a compact registry of commonly used preprocessing parameters for
#' Gazepoint pupil and gaze analyses. The registry is designed to make
#' preprocessing choices explicit, auditable, and easy to report.
#'
#' @param blink_padding_pre_ms Padding before bad pupil samples, blinks, or
#'   tracking artifacts, in milliseconds. Defaults to `100`.
#' @param blink_padding_post_ms Padding after bad pupil samples, blinks, or
#'   tracking artifacts, in milliseconds. Defaults to `100`.
#' @param max_interpolation_gap_ms Maximum missing-pupil gap duration to
#'   interpolate, in milliseconds. Defaults to `150`.
#' @param smoothing_window_ms Rolling smoothing window, in milliseconds.
#'   Defaults to `50`.
#' @param baseline_start_ms Baseline-window start, in milliseconds. Defaults to
#'   `-200`.
#' @param baseline_end_ms Baseline-window end, in milliseconds. Defaults to `0`.
#' @param pupil_physiological_min Minimum plausible pupil value when the pupil
#'   unit is known to be millimetres. Defaults to `1`.
#' @param pupil_physiological_max Maximum plausible pupil value when the pupil
#'   unit is known to be millimetres. Defaults to `9`.
#' @param pupil_speed_mad_k MAD multiplier for pupil-speed outlier detection.
#'   Defaults to `6`.
#' @param binocular_mad_k MAD multiplier for left-right pupil disagreement.
#'   Defaults to `6`.
#' @param baseline_missing_prop_threshold Baseline missingness threshold used
#'   for baseline-quality audits. Defaults to `0.30`.
#' @param baseline_interpolated_prop_threshold Baseline interpolation threshold
#'   used for baseline-quality audits. Defaults to `0.30`.
#' @param baseline_artifact_prop_threshold Baseline artifact threshold used for
#'   baseline-quality audits. Defaults to `0.30`.
#' @param overlap_trial_duration_ms Trial-duration threshold below which pupil
#'   overlap/deconvolution risk should be considered. Defaults to `3000`.
#' @param overlap_event_gap_ms Event-gap threshold below which pupil-response
#'   overlap should be considered. Defaults to `1000`.
#'
#' @return A tibble with one row per preprocessing parameter.
#'
#' @examples
#' registry <- create_gazepoint_preprocessing_registry()
#' registry
#'
#' @export
create_gazepoint_preprocessing_registry <- function(
    blink_padding_pre_ms = 100,
    blink_padding_post_ms = 100,
    max_interpolation_gap_ms = 150,
    smoothing_window_ms = 50,
    baseline_start_ms = -200,
    baseline_end_ms = 0,
    pupil_physiological_min = 1,
    pupil_physiological_max = 9,
    pupil_speed_mad_k = 6,
    binocular_mad_k = 6,
    baseline_missing_prop_threshold = 0.30,
    baseline_interpolated_prop_threshold = 0.30,
    baseline_artifact_prop_threshold = 0.30,
    overlap_trial_duration_ms = 3000,
    overlap_event_gap_ms = 1000
) {
  check_numeric_scalar <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
      rlang::abort(paste0("`", name, "` must be a single non-missing numeric value."))
    }

    invisible(TRUE)
  }

  args <- list(
    blink_padding_pre_ms = blink_padding_pre_ms,
    blink_padding_post_ms = blink_padding_post_ms,
    max_interpolation_gap_ms = max_interpolation_gap_ms,
    smoothing_window_ms = smoothing_window_ms,
    baseline_start_ms = baseline_start_ms,
    baseline_end_ms = baseline_end_ms,
    pupil_physiological_min = pupil_physiological_min,
    pupil_physiological_max = pupil_physiological_max,
    pupil_speed_mad_k = pupil_speed_mad_k,
    binocular_mad_k = binocular_mad_k,
    baseline_missing_prop_threshold = baseline_missing_prop_threshold,
    baseline_interpolated_prop_threshold = baseline_interpolated_prop_threshold,
    baseline_artifact_prop_threshold = baseline_artifact_prop_threshold,
    overlap_trial_duration_ms = overlap_trial_duration_ms,
    overlap_event_gap_ms = overlap_event_gap_ms
  )

  for (name in names(args)) {
    check_numeric_scalar(args[[name]], name)
  }

  non_negative_args <- c(
    "blink_padding_pre_ms",
    "blink_padding_post_ms",
    "max_interpolation_gap_ms",
    "smoothing_window_ms",
    "pupil_speed_mad_k",
    "binocular_mad_k",
    "baseline_missing_prop_threshold",
    "baseline_interpolated_prop_threshold",
    "baseline_artifact_prop_threshold",
    "overlap_trial_duration_ms",
    "overlap_event_gap_ms"
  )

  negative_args <- non_negative_args[
    vapply(args[non_negative_args], function(x) x < 0, logical(1))
  ]

  if (length(negative_args) > 0) {
    rlang::abort(
      paste0(
        "The following parameter(s) must be greater than or equal to 0: ",
        paste(negative_args, collapse = ", ")
      )
    )
  }

  prop_args <- c(
    "baseline_missing_prop_threshold",
    "baseline_interpolated_prop_threshold",
    "baseline_artifact_prop_threshold"
  )

  invalid_props <- prop_args[
    vapply(args[prop_args], function(x) x > 1, logical(1))
  ]

  if (length(invalid_props) > 0) {
    rlang::abort(
      paste0(
        "The following proportion threshold(s) must be between 0 and 1: ",
        paste(invalid_props, collapse = ", ")
      )
    )
  }

  if (baseline_end_ms < baseline_start_ms) {
    rlang::abort("`baseline_end_ms` must be greater than or equal to `baseline_start_ms`.")
  }

  if (pupil_physiological_max <= pupil_physiological_min) {
    rlang::abort("`pupil_physiological_max` must be greater than `pupil_physiological_min`.")
  }

  tibble::tibble(
    parameter = c(
      "blink_padding_pre_ms",
      "blink_padding_post_ms",
      "max_interpolation_gap_ms",
      "smoothing_window_ms",
      "baseline_start_ms",
      "baseline_end_ms",
      "pupil_physiological_min",
      "pupil_physiological_max",
      "pupil_speed_mad_k",
      "binocular_mad_k",
      "baseline_missing_prop_threshold",
      "baseline_interpolated_prop_threshold",
      "baseline_artifact_prop_threshold",
      "overlap_trial_duration_ms",
      "overlap_event_gap_ms"
    ),
    value = c(
      blink_padding_pre_ms,
      blink_padding_post_ms,
      max_interpolation_gap_ms,
      smoothing_window_ms,
      baseline_start_ms,
      baseline_end_ms,
      pupil_physiological_min,
      pupil_physiological_max,
      pupil_speed_mad_k,
      binocular_mad_k,
      baseline_missing_prop_threshold,
      baseline_interpolated_prop_threshold,
      baseline_artifact_prop_threshold,
      overlap_trial_duration_ms,
      overlap_event_gap_ms
    ),
    unit = c(
      "ms",
      "ms",
      "ms",
      "ms",
      "ms",
      "ms",
      "mm_if_unit_is_mm",
      "mm_if_unit_is_mm",
      "MAD_multiplier",
      "MAD_multiplier",
      "proportion",
      "proportion",
      "proportion",
      "ms",
      "ms"
    ),
    category = c(
      "artifact_padding",
      "artifact_padding",
      "interpolation",
      "smoothing",
      "baseline",
      "baseline",
      "pupil_plausibility",
      "pupil_plausibility",
      "artifact_detection",
      "binocular_consistency",
      "baseline_quality",
      "baseline_quality",
      "baseline_quality",
      "overlap_risk",
      "overlap_risk"
    ),
    description = c(
      "Milliseconds removed before blink/artifact/bad-pupil samples.",
      "Milliseconds removed after blink/artifact/bad-pupil samples.",
      "Maximum short missing-pupil gap duration allowed for interpolation.",
      "Rolling smoothing window expressed in milliseconds.",
      "Start of the default baseline window.",
      "End of the default baseline window.",
      "Minimum physiologically plausible pupil value when units are millimetres.",
      "Maximum physiologically plausible pupil value when units are millimetres.",
      "Robust MAD multiplier for pupil-speed outlier detection.",
      "Robust MAD multiplier for left-right pupil disagreement detection.",
      "Baseline-window missingness threshold used to flag problematic baselines.",
      "Baseline-window interpolation threshold used to flag problematic baselines.",
      "Baseline-window artifact threshold used to flag problematic baselines.",
      "Short trial-duration threshold for possible pupil-response overlap.",
      "Short event-gap threshold for possible pupil-response overlap."
    )
  )
}
