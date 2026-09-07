#' Audit GP3 pupil-latency estimator sensitivity
#'
#' Compares a sustained-threshold onset and maximum-slope tangent onset while
#' reporting estimator spread, sampling rate, robust baseline noise, and a
#' latency-resolvability label. This is a QC/reporting diagnostic rather than a
#' claim that any one estimator is universally correct.
#'
#' @param time Numeric sample times in seconds.
#' @param pupil Numeric pupil series.
#' @param event_time Nominal event time in seconds.
#' @param baseline_window,search_window Two-element windows relative to the event.
#' @param direction `"constriction"` or `"dilation"`.
#' @param threshold_sigma Multiples of robust baseline noise.
#' @param sustain_ms Required sustained threshold duration in milliseconds.
#' @return A structured pupil-latency QC record suitable for reports.
#' @export
audit_gp3_pupil_latency_resolution <- function(
    time, pupil, event_time = 0, baseline_window = c(-0.5, 0),
    search_window = c(0, 2), direction = c("constriction", "dilation"),
    threshold_sigma = 3, sustain_ms = 50) {
  direction <- match.arg(direction)
  if (length(time) != length(pupil) || length(time) < 8L) stop("Inputs must have equal length and at least 8 samples.", call. = FALSE)
  keep <- is.finite(time) & is.finite(pupil)
  time <- as.numeric(time[keep]); pupil <- as.numeric(pupil[keep])
  ord <- order(time); time <- time[ord]; pupil <- pupil[ord]
  dt <- stats::median(diff(unique(time)))
  if (!is.finite(dt) || dt <= 0) stop("Time must increase.", call. = FALSE)
  b <- time >= event_time + baseline_window[1] & time < event_time + baseline_window[2]
  s <- time >= event_time + search_window[1] & time <= event_time + search_window[2]
  if (sum(b) < 3L || sum(s) < 4L) stop("Baseline/search windows are too sparse.", call. = FALSE)
  baseline <- stats::median(pupil[b])
  noise <- 1.4826 * stats::median(abs(pupil[b] - baseline))
  if (!is.finite(noise) || noise == 0) noise <- stats::sd(pupil[b])
  if (!is.finite(noise) || noise == 0) noise <- .Machine$double.eps
  sign <- if (direction == "constriction") -1 else 1
  st <- time[s]; response <- sign * (pupil[s] - baseline)
  run_n <- max(1L, round((sustain_ms / 1000) / dt))
  hits <- response >= threshold_sigma * noise
  run <- stats::filter(as.integer(hits), rep(1, run_n), sides = 1)
  idx <- which(run >= run_n)[1]
  threshold_latency <- if (!is.na(idx)) st[max(1L, idx - run_n + 1L)] - event_time else NA_real_
  slopes <- diff(response) / diff(st)
  peak <- if (length(slopes)) which.max(slopes) + 1L else NA_integer_
  tangent_latency <- if (!is.na(peak) && slopes[peak - 1L] > 0) st[peak] - response[peak] / slopes[peak - 1L] - event_time else NA_real_
  estimates <- c(sustained_threshold = threshold_latency, max_slope_tangent = tangent_latency)
  good <- estimates[is.finite(estimates)]
  spread <- if (length(good) >= 2L) diff(range(good)) * 1000 else NA_real_
  snr <- max(response, na.rm = TRUE) / noise
  resolution_floor_ms <- 1000 * dt
  state <- if (length(good) >= 2L && spread <= max(50, 2 * resolution_floor_ms) && snr >= 3) "high" else if (length(good) && snr >= 1.5) "moderate" else "low"
  structure(list(
    estimates_s = estimates, estimator_spread_ms = spread,
    sampling_hz = 1 / dt, sampling_resolution_ms = resolution_floor_ms,
    baseline = baseline, noise_mad_sigma = noise, signal_to_noise = snr,
    latency_resolvability = state,
    reporting_note = "Report estimator choice/spread with latency; do not treat latency as hardware- or algorithm-independent."
  ), class = "gp3_pupil_latency_resolution")
}
