#' Recommend model families for Gazepoint-derived metrics
#'
#' Maps common eye-tracking and pupillometry metrics to suitable statistical
#' model families, transformations, and modelling notes. The helper is intended
#' for planning and reporting; it does not fit a model.
#'
#' @param metric Character vector of metric names. If `NULL`, all available
#'   recommendations are returned.
#'
#' @return A data frame with metric, data property, recommended family,
#'   common transformation, and notes.
#' @export
recommend_gazepoint_model_family <- function(metric = NULL) {
  rec <- data.frame(
    metric = c(
      "fixation_duration",
      "dwell_time",
      "fixation_count",
      "aoi_proportion",
      "scanpath_efficiency",
      "pupil_timecourse",
      "blink_duration",
      "blink_rate",
      "saccade_amplitude",
      "saccade_velocity",
      "scanpath_length",
      "convex_hull_area",
      "binary_choice"
    ),
    data_property = c(
      "positive, right-skewed",
      "positive, right-skewed",
      "non-negative count, often overdispersed",
      "bounded proportion between 0 and 1",
      "bounded ratio between 0 and 1",
      "continuous time-series, often autocorrelated",
      "positive, right-skewed",
      "non-negative count per time unit",
      "positive, right-skewed",
      "positive, right-skewed",
      "positive, right-skewed",
      "positive, right-skewed",
      "binary 0/1 outcome"
    ),
    recommended_family = c(
      "lognormal or gamma",
      "lognormal or gamma",
      "negative binomial or Poisson",
      "beta or binomial after denominator construction",
      "beta",
      "Gaussian with smooth time terms; consider AR(1) sensitivity",
      "lognormal or gamma",
      "negative binomial or Poisson",
      "gamma or lognormal",
      "gamma or lognormal",
      "gamma or lognormal",
      "gamma or lognormal",
      "Bernoulli/binomial"
    ),
    common_transform = c(
      "log(duration_ms)",
      "log(dwell_ms)",
      "none; model as count",
      "logit after boundary adjustment if needed",
      "logit after boundary adjustment if needed",
      "baseline correction, smoothing, or time-course model",
      "log(duration_ms)",
      "none; model as count",
      "log(amplitude)",
      "log(velocity)",
      "log(scanpath_length)",
      "log(area)",
      "none"
    ),
    notes = c(
      "Avoid Gaussian models on raw fixation durations.",
      "Avoid Gaussian models on raw dwell times when strongly skewed.",
      "Check overdispersion before using Poisson models.",
      "Values exactly 0 or 1 require adjustment or a binomial formulation.",
      "Values exactly 0 or 1 require adjustment before beta regression.",
      "Do not use repeated pointwise tests without accounting for time dependence.",
      "Separate physiological blinks from tracking loss where possible.",
      "Use an exposure or time denominator if bin widths differ.",
      "Convert pixels to degrees of visual angle when possible.",
      "Check main-sequence plausibility for extreme saccades.",
      "Interpret as search extent or inefficiency depending on task.",
      "Requires at least three valid spatial points per trial.",
      "Check response coding and class balance."
    ),
    stringsAsFactors = FALSE
  )

  if (is.null(metric)) {
    return(rec)
  }

  metric <- tolower(metric)
  out <- rec[rec$metric %in% metric, , drop = FALSE]

  missing <- setdiff(metric, rec$metric)
  if (length(missing) > 0) {
    warning(
      "No recommendation found for: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  out
}


#' Check readiness of a Gazepoint-derived dataset for Bayesian or advanced models
#'
#' Performs lightweight structural checks before Bayesian, GAMM, HDDM, or other
#' advanced modelling workflows. The function does not fit any model.
#'
#' @param data A data frame.
#' @param outcome Name of the outcome column.
#' @param subject Name of the subject/participant column.
#' @param trial Optional trial column.
#' @param time Optional time column.
#' @param condition Optional condition column.
#' @param metric_type Character scalar describing the planned metric/model type.
#' @param baseline_window Optional numeric vector of length two.
#' @param min_observations_per_subject Minimum number of observations expected
#'   per subject.
#' @param max_missing_trial_prop Maximum acceptable missingness proportion per
#'   subject-trial cell before a warning is raised.
#'
#' @return A data frame of checks, status values, and messages.
#' @export
check_gazepoint_bayesian_readiness <- function(
    data,
    outcome,
    subject,
    trial = NULL,
    time = NULL,
    condition = NULL,
    metric_type = "continuous",
    baseline_window = NULL,
    min_observations_per_subject = 10,
    max_missing_trial_prop = 0.20) {
  stopifnot(is.data.frame(data))

  checks <- list()

  add_check <- function(check, status, message) {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check,
      status = status,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  required <- c(outcome, subject, trial, time, condition)
  required <- required[!is.null(required) & !is.na(required)]

  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    add_check(
      "required_columns",
      "fail",
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
    return(do.call(rbind, checks))
  }

  add_check("required_columns", "pass", "All specified columns are present.")

  y <- data[[outcome]]
  mt <- tolower(metric_type)

  if (all(is.na(y))) {
    add_check("outcome_values", "fail", "Outcome contains only missing values.")
  } else {
    add_check("outcome_values", "pass", "Outcome contains observed values.")
  }

  positive_types <- c(
    "fixation_duration", "dwell_time", "blink_duration",
    "saccade_amplitude", "saccade_velocity", "scanpath_length",
    "convex_hull_area", "lognormal", "gamma"
  )

  bounded_types <- c("aoi_proportion", "scanpath_efficiency", "beta", "proportion")
  binary_types <- c("binary_choice", "binomial", "bernoulli", "response")

  if (mt %in% positive_types) {
    bad <- !is.na(y) & y <= 0
    if (any(bad)) {
      add_check(
        "positive_outcome",
        "fail",
        paste(sum(bad), "non-positive outcome values found.")
      )
    } else {
      add_check("positive_outcome", "pass", "All observed outcome values are positive.")
    }
  }

  if (mt %in% bounded_types) {
    bad <- !is.na(y) & (y < 0 | y > 1)
    boundary <- !is.na(y) & (y == 0 | y == 1)
    if (any(bad)) {
      add_check(
        "bounded_outcome",
        "fail",
        paste(sum(bad), "values fall outside [0, 1].")
      )
    } else if (any(boundary)) {
      add_check(
        "bounded_outcome",
        "warn",
        paste(sum(boundary), "values are exactly 0 or 1; beta models may need adjustment.")
      )
    } else {
      add_check("bounded_outcome", "pass", "Observed values are inside (0, 1).")
    }
  }

  if (mt %in% binary_types) {
    vals <- sort(unique(y[!is.na(y)]))
    if (all(vals %in% c(0, 1)) && length(vals) == 2L) {
      add_check("binary_outcome", "pass", "Outcome is coded as 0/1 with both classes present.")
    } else {
      add_check(
        "binary_outcome",
        "fail",
        paste("Outcome is not a complete 0/1 variable. Observed values:", paste(vals, collapse = ", "))
      )
    }
  }

  subj_n <- table(data[[subject]])
  low_subj <- names(subj_n)[subj_n < min_observations_per_subject]
  if (length(low_subj) > 0) {
    add_check(
      "observations_per_subject",
      "warn",
      paste(length(low_subj), "subjects have fewer than", min_observations_per_subject, "observations.")
    )
  } else {
    add_check("observations_per_subject", "pass", "Subjects meet the minimum observation threshold.")
  }

  if (!is.null(condition)) {
    n_cond <- length(unique(data[[condition]][!is.na(data[[condition]])]))
    if (n_cond < 2L) {
      add_check("condition_variation", "fail", "Condition has fewer than two observed levels.")
    } else {
      add_check("condition_variation", "pass", paste("Condition has", n_cond, "observed levels."))
    }
  }

  if (!is.null(trial)) {
    miss <- is.na(y)
    key <- interaction(data[[subject]], data[[trial]], drop = TRUE)
    prop <- stats::aggregate(miss, list(subject_trial = key), mean)
    high <- prop$x > max_missing_trial_prop
    if (any(high)) {
      add_check(
        "trial_missingness",
        "warn",
        paste(sum(high), "subject-trial cells exceed the missingness threshold.")
      )
    } else {
      add_check("trial_missingness", "pass", "Subject-trial missingness is within threshold.")
    }
  }

  if (mt %in% c("pupil_timecourse", "pupil", "bayesian_gamm")) {
    if (is.null(time)) {
      add_check("time_column", "warn", "Pupil time-course models usually require a time column.")
    }
    if (is.null(baseline_window)) {
      add_check("baseline_window", "warn", "No baseline window was specified.")
    } else if (length(baseline_window) != 2L || !is.numeric(baseline_window)) {
      add_check("baseline_window", "fail", "baseline_window must be a numeric vector of length two.")
    } else if (!is.null(time)) {
      t <- data[[time]]
      has_baseline_samples <- any(!is.na(t) & t >= min(baseline_window) & t <= max(baseline_window))
      if (has_baseline_samples) {
        add_check("baseline_window", "pass", "Baseline window overlaps observed time values.")
      } else {
        add_check("baseline_window", "warn", "Baseline window does not overlap observed time values.")
      }
    }
  }

  out <- do.call(rbind, checks)
  rownames(out) <- NULL
  out
}


#' Create a Bayesian ocular Statistical Analysis Plan checklist
#'
#' Generates a structured checklist for Bayesian or advanced eye-tracking and
#' pupillometry analysis planning.
#'
#' @param outcome Outcome name or outcome family.
#' @param design Study design description.
#' @param primary_model Planned primary model.
#' @param baseline_window Optional baseline window.
#' @param analysis_window Optional analysis window.
#' @param missingness_threshold Trial-level missingness threshold.
#' @param blink_padding_ms Blink padding in milliseconds.
#' @param output Either `"data.frame"` or `"markdown"`.
#'
#' @return A data frame or markdown character vector.
#' @export
create_gazepoint_bayesian_sap <- function(
    outcome,
    design,
    primary_model,
    baseline_window = NULL,
    analysis_window = NULL,
    missingness_threshold = 0.20,
    blink_padding_ms = 50,
    output = c("data.frame", "markdown")) {
  output <- match.arg(output)

  fmt_window <- function(x) {
    if (is.null(x)) {
      return("Not specified")
    }
    paste0("[", paste(x, collapse = ", "), "]")
  }

  sap <- data.frame(
    section = c(
      "Study design",
      "Outcome",
      "Preprocessing",
      "Blink handling",
      "Missingness",
      "Baseline correction",
      "Analysis window",
      "Model family",
      "Prior specification",
      "Hierarchical structure",
      "MCMC diagnostics",
      "Inference rule",
      "Reporting"
    ),
    item = c(
      "Design",
      "Primary outcome",
      "Locked preprocessing decisions",
      "Blink padding",
      "Trial exclusion threshold",
      "Baseline window",
      "Primary analysis window",
      "Primary model",
      "Weakly informative priors",
      "Participant/item structure",
      "Convergence checks",
      "Decision criterion",
      "Reproducibility checklist"
    ),
    planned_specification = c(
      design,
      outcome,
      "Define filtering, interpolation, AOI assignment, and exclusion rules before analysis.",
      paste0(blink_padding_ms, " ms before and after detected blink edges, unless otherwise justified."),
      paste0("Flag or exclude trials above ", missingness_threshold * 100, "% missingness."),
      fmt_window(baseline_window),
      fmt_window(analysis_window),
      primary_model,
      "Specify priors for fixed effects, residual scale, and hierarchical variance terms.",
      "Include participant-level structure; include item/stimulus structure when the design supports it.",
      "Require acceptable R-hat, effective sample size, and no problematic divergent transitions.",
      "Define HDI, ROPE, Bayes factor, or posterior probability threshold before analysis.",
      "Report preprocessing decisions, model formula, priors, diagnostics, exclusions, and sensitivity checks."
    ),
    stringsAsFactors = FALSE
  )

  if (output == "data.frame") {
    return(sap)
  }

  c(
    "# Bayesian ocular Statistical Analysis Plan",
    "",
    paste0("**Outcome:** ", outcome),
    paste0("**Design:** ", design),
    paste0("**Primary model:** ", primary_model),
    "",
    paste0(
      apply(
        sap,
        1,
        function(z) paste0("- **", z[["section"]], " / ", z[["item"]], ":** ", z[["planned_specification"]])
      ),
      collapse = "\n"
    )
  )
}


#' Prepare a trial-level export for Python HDDM
#'
#' Prepares a clean trial-level CSV-compatible data frame for Python HDDM or
#' related drift-diffusion modelling workflows. This function does not fit HDDM.
#'
#' @param data Trial-level data frame.
#' @param subject Subject identifier column.
#' @param rt Response-time column.
#' @param response Binary response column coded as 0/1.
#' @param predictors Optional continuous predictors to include.
#' @param zscore_within_subject Logical; z-score predictors within subject.
#' @param drop_missing Logical; drop rows with missing required values.
#' @param file Optional CSV path. If supplied, the export is written to disk.
#'
#' @return A data frame ready for HDDM-style workflows.
#' @export
prepare_gazepoint_hddm_export <- function(
    data,
    subject,
    rt,
    response,
    predictors = NULL,
    zscore_within_subject = TRUE,
    drop_missing = TRUE,
    file = NULL) {
  stopifnot(is.data.frame(data))

  required <- c(subject, rt, response, predictors)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- data.frame(
    subj_idx = as.integer(factor(data[[subject]])),
    rt = data[[rt]],
    response = data[[response]],
    stringsAsFactors = FALSE
  )

  if (any(out$rt <= 0, na.rm = TRUE)) {
    stop("Response times must be positive.", call. = FALSE)
  }

  vals <- sort(unique(out$response[!is.na(out$response)]))
  if (!all(vals %in% c(0, 1))) {
    stop("Response must be coded as 0/1 for this export helper.", call. = FALSE)
  }

  if (!is.null(predictors)) {
    for (p in predictors) {
      x <- data[[p]]

      if (!is.numeric(x)) {
        warning("Predictor '", p, "' is not numeric; copying without z-scoring.", call. = FALSE)
        out[[p]] <- x
        next
      }

      if (zscore_within_subject) {
        mu <- stats::ave(x, out$subj_idx, FUN = function(v) mean(v, na.rm = TRUE))
        sig <- stats::ave(x, out$subj_idx, FUN = function(v) stats::sd(v, na.rm = TRUE))
        z <- (x - mu) / sig
        z[!is.finite(z)] <- NA_real_
        out[[paste0(p, "_z")]] <- z
      } else {
        mu <- mean(x, na.rm = TRUE)
        sig <- stats::sd(x, na.rm = TRUE)
        z <- (x - mu) / sig
        z[!is.finite(z)] <- NA_real_
        out[[paste0(p, "_z")]] <- z
      }
    }
  }

  if (drop_missing) {
    out <- out[stats::complete.cases(out), , drop = FALSE]
  }

  rownames(out) <- NULL

  if (!is.null(file)) {
    utils::write.csv(out, file, row.names = FALSE)
  }

  out
}


#' Create brms formula and prior templates for Gazepoint-derived metrics
#'
#' Returns formula, family, prior, and reporting-note templates. The function
#' does not require or call brms.
#'
#' @param metric_type Metric type, such as `"pupil_timecourse"`,
#'   `"fixation_duration"`, `"fixation_count"`, `"aoi_proportion"`, or
#'   `"binary_choice"`.
#' @param outcome Outcome column name.
#' @param time Optional time column.
#' @param condition Optional condition column.
#' @param subject Optional subject column.
#' @param item Optional item/stimulus column.
#'
#' @return A list containing formula, family, priors, and notes.
#' @export
create_gazepoint_brms_template <- function(
    metric_type,
    outcome,
    time = NULL,
    condition = NULL,
    subject = NULL,
    item = NULL) {
  mt <- tolower(metric_type)

  random_terms <- character(0)
  if (!is.null(subject)) {
    random_terms <- c(random_terms, paste0("(1 | ", subject, ")"))
  }
  if (!is.null(item)) {
    random_terms <- c(random_terms, paste0("(1 | ", item, ")"))
  }

  rhs_base <- if (!is.null(condition)) condition else "1"

  if (mt %in% c("pupil_timecourse", "pupil", "bayesian_gamm")) {
    if (is.null(time) || is.null(condition) || is.null(subject)) {
      stop("pupil_timecourse templates require time, condition, and subject.", call. = FALSE)
    }

    formula <- paste0(
      outcome, " ~ ", condition,
      " + s(", time, ", by = ", condition, ", k = 5)",
      " + s(", time, ", ", subject, ", bs = \"fs\", k = 5)"
    )

    if (!is.null(item)) {
      formula <- paste(formula, "+", paste0("(1 | ", item, ")"))
    }

    return(list(
      metric_type = metric_type,
      formula = formula,
      family = "gaussian()",
      priors = c(
        "prior(normal(0, 1), class = \"b\")",
        "prior(exponential(1), class = \"sigma\")",
        "prior(exponential(1), class = \"sd\")"
      ),
      notes = c(
        "Template only; inspect autocorrelation and consider AR(1) sensitivity.",
        "Use baseline-corrected pupil values when the research question concerns phasic change.",
        "Report time window, baseline window, smoothing basis, and convergence diagnostics."
      )
    ))
  }

  family <- switch(
    mt,
    fixation_duration = "lognormal()",
    dwell_time = "lognormal()",
    blink_duration = "lognormal()",
    fixation_count = "negbinomial()",
    blink_rate = "negbinomial()",
    aoi_proportion = "Beta() or binomial() depending on denominator structure",
    scanpath_efficiency = "Beta()",
    binary_choice = "bernoulli()",
    "gaussian()"
  )

  formula <- paste(outcome, "~", rhs_base)
  if (length(random_terms) > 0) {
    formula <- paste(formula, "+", paste(random_terms, collapse = " + "))
  }

  list(
    metric_type = metric_type,
    formula = formula,
    family = family,
    priors = c(
      "prior(normal(0, 1), class = \"b\")",
      "prior(exponential(1), class = \"sd\")"
    ),
    notes = c(
      "Template only; adapt priors to the scale of the outcome.",
      "Check missingness, distributional shape, and convergence diagnostics.",
      "Prefer optional brms/Stan use outside the core package workflow."
    )
  )
}


#' Summarize pupil response features by subject and trial
#'
#' Extracts common pupil-response features from sample-level pupil data.
#'
#' @param data A sample-level data frame.
#' @param pupil Pupil column.
#' @param time Time column.
#' @param subject Subject column.
#' @param trial Trial column.
#' @param baseline_window Numeric vector of length two.
#' @param response_window Numeric vector of length two.
#' @param condition Optional condition column to carry forward.
#' @param interpolated Optional logical/numeric interpolation flag column.
#'
#' @return A trial-level data frame of pupil features.
#' @export
summarize_gazepoint_pupil_response_features <- function(
    data,
    pupil,
    time,
    subject,
    trial,
    baseline_window,
    response_window,
    condition = NULL,
    interpolated = NULL) {
  stopifnot(is.data.frame(data))

  required <- c(pupil, time, subject, trial, condition, interpolated)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (length(baseline_window) != 2L || length(response_window) != 2L) {
    stop("baseline_window and response_window must each have length two.", call. = FALSE)
  }

  trapz <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    if (length(x) < 2L) {
      return(NA_real_)
    }
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    sum(diff(x) * (utils::head(y, -1L) + utils::tail(y, -1L)) / 2)
  }

  key <- interaction(data[[subject]], data[[trial]], drop = TRUE)
  groups <- split(data, key)

  out <- lapply(groups, function(d) {
    t <- d[[time]]
    p <- d[[pupil]]

    base_idx <- !is.na(t) & t >= min(baseline_window) & t <= max(baseline_window)
    resp_idx <- !is.na(t) & t >= min(response_window) & t <= max(response_window)

    baseline_mean <- mean(p[base_idx], na.rm = TRUE)
    if (!is.finite(baseline_mean)) {
      baseline_mean <- NA_real_
    }

    corrected <- p - baseline_mean
    resp_corrected <- corrected[resp_idx]
    resp_time <- t[resp_idx]

    if (all(is.na(resp_corrected))) {
      peak <- NA_real_
      latency <- NA_real_
    } else {
      peak_i <- which.max(resp_corrected)
      peak <- resp_corrected[peak_i]
      latency <- resp_time[peak_i]
    }

    missing_percent <- mean(is.na(p[resp_idx])) * 100
    if (!is.finite(missing_percent)) {
      missing_percent <- NA_real_
    }

    interp_percent <- NA_real_
    if (!is.null(interpolated)) {
      interp_percent <- mean(as.logical(d[[interpolated]][resp_idx]), na.rm = TRUE) * 100
      if (!is.finite(interp_percent)) {
        interp_percent <- NA_real_
      }
    }

    ans <- data.frame(
      subject = d[[subject]][1],
      trial = d[[trial]][1],
      baseline_mean = baseline_mean,
      peak_dilation = peak,
      latency_to_peak = latency,
      auc = trapz(resp_time, resp_corrected),
      missing_percent = missing_percent,
      interpolated_percent = interp_percent,
      stringsAsFactors = FALSE
    )

    if (!is.null(condition)) {
      ans$condition <- d[[condition]][1]
    }

    ans
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}


#' Compute scanpath geometry features by subject and trial
#'
#' Computes scanpath length, straight-line distance, efficiency, convex-hull
#' area, and spatial dispersion from sequential gaze/fixation coordinates.
#'
#' @param data A data frame.
#' @param x X-coordinate column.
#' @param y Y-coordinate column.
#' @param subject Subject column.
#' @param trial Trial column.
#' @param time Optional time column used for ordering.
#' @param condition Optional condition column to carry forward.
#'
#' @return A trial-level data frame of scanpath geometry features.
#' @export
compute_gazepoint_scanpath_geometry <- function(
    data,
    x,
    y,
    subject,
    trial,
    time = NULL,
    condition = NULL) {
  stopifnot(is.data.frame(data))

  required <- c(x, y, subject, trial, time, condition)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  polygon_area <- function(px, py) {
    if (length(px) < 3L) {
      return(NA_real_)
    }
    idx <- grDevices::chull(px, py)
    hx <- px[idx]
    hy <- py[idx]
    hx2 <- c(hx, hx[1])
    hy2 <- c(hy, hy[1])
    abs(sum(hx2[-1] * hy2[-length(hy2)] - hx2[-length(hx2)] * hy2[-1])) / 2
  }

  key <- interaction(data[[subject]], data[[trial]], drop = TRUE)
  groups <- split(data, key)

  out <- lapply(groups, function(d) {
    if (!is.null(time)) {
      d <- d[order(d[[time]]), , drop = FALSE]
    }

    px <- d[[x]]
    py <- d[[y]]
    ok <- is.finite(px) & is.finite(py)
    px <- px[ok]
    py <- py[ok]

    n <- length(px)

    if (n < 2L) {
      scanpath_length <- NA_real_
      straight_line_distance <- NA_real_
      efficiency <- NA_real_
    } else {
      step_dist <- sqrt(diff(px)^2 + diff(py)^2)
      scanpath_length <- sum(step_dist, na.rm = TRUE)
      straight_line_distance <- sqrt((px[n] - px[1])^2 + (py[n] - py[1])^2)
      efficiency <- if (scanpath_length > 0) straight_line_distance / scanpath_length else NA_real_
    }

    centroid_x <- mean(px, na.rm = TRUE)
    centroid_y <- mean(py, na.rm = TRUE)
    spatial_dispersion <- mean(sqrt((px - centroid_x)^2 + (py - centroid_y)^2), na.rm = TRUE)
    if (!is.finite(spatial_dispersion)) {
      spatial_dispersion <- NA_real_
    }

    ans <- data.frame(
      subject = d[[subject]][1],
      trial = d[[trial]][1],
      n_points = n,
      scanpath_length = scanpath_length,
      straight_line_distance = straight_line_distance,
      scanpath_efficiency = efficiency,
      convex_hull_area = polygon_area(px, py),
      spatial_dispersion = spatial_dispersion,
      stringsAsFactors = FALSE
    )

    if (!is.null(condition)) {
      ans$condition <- d[[condition]][1]
    }

    ans
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}
