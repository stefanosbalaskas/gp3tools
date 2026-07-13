#' Fit an optional brms model for Gazepoint-derived data
#'
#' Fits a Bayesian model using brms when brms is installed. brms is treated as
#' an optional external backend; gp3tools does not import or require it.
#'
#' @param data A data frame.
#' @param formula A model formula.
#' @param family brms family specification as a character string or brms family object.
#' @param prior Optional brms prior specification.
#' @param chains Number of MCMC chains.
#' @param iter Total iterations per chain.
#' @param warmup Warmup iterations per chain.
#' @param cores Number of cores.
#' @param backend Optional brms backend, e.g. `"cmdstanr"`.
#' @param ... Additional arguments passed to `brms::brm()`.
#'
#' @return A fitted brms model.
#' @export
fit_gazepoint_brms_model <- function(
    data,
    formula,
    family = "gaussian",
    prior = NULL,
    chains = 4,
    iter = 2000,
    warmup = floor(iter / 2),
    cores = 1,
    backend = NULL,
    ...) {
  stopifnot(is.data.frame(data))

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop(
      "Package 'brms' is required for this optional backend. ",
      "Install it separately with install.packages('brms').",
      call. = FALSE
    )
  }

  if (is.character(family)) {
    family <- switch(
      tolower(family),
      gaussian = stats::gaussian(),
      normal = stats::gaussian(),
      bernoulli = brms::bernoulli(),
      binomial = stats::binomial(),
      negbinomial = brms::negbinomial(),
      negative_binomial = brms::negbinomial(),
      poisson = stats::poisson(),
      lognormal = brms::lognormal(),
      gamma = stats::Gamma(),
      beta = brms::Beta(),
      stop("Unsupported family string: ", family, call. = FALSE)
    )
  }

  args <- list(
    formula = stats::as.formula(formula),
    data = data,
    family = family,
    chains = chains,
    iter = iter,
    warmup = warmup,
    cores = cores,
    ...
  )

  if (!is.null(prior)) {
    args$prior <- prior
  }

  if (!is.null(backend)) {
    args$backend <- backend
  }

  do.call(brms::brm, args)
}


#' Create a Python HDDM fitting script from a Gazepoint HDDM export
#'
#' Writes a Python script for fitting an HDDMRegressor model. The function does
#' not fit HDDM inside R and does not require Python. It creates a reproducible
#' script that can be run in a Python/HDDM environment.
#'
#' @param data_file Path to a CSV file prepared with `prepare_gazepoint_hddm_export()`.
#' @param output_file Path where the Python script should be written.
#' @param regressions Named character vector. Names should be DDM parameters
#'   such as `"v"`, `"a"`, `"t"`, or `"z"`; values should be predictor terms.
#' @param include Character vector of DDM parameters to include.
#' @param draws Number of posterior draws.
#' @param burn Number of burn-in samples.
#' @param dbname HDDM trace database name.
#'
#' @return Invisibly returns `output_file`.
#' @export
create_gazepoint_hddm_fit_script <- function(
    data_file,
    output_file = "fit_gazepoint_hddm.py",
    regressions = c(v = "target_dwell_ms_z", a = "pupil_peak_z"),
    include = c("v", "a", "t"),
    draws = 5000,
    burn = 2000,
    dbname = "hddm_traces.db") {
  if (!length(regressions)) {
    stop("At least one regression must be supplied.", call. = FALSE)
  }

  reg_lines <- vapply(
    names(regressions),
    function(param) {
      predictor <- regressions[[param]]
      paste0(
        "    {\"model\": \"", param, " ~ 1 + ", predictor,
        "\", \"link_func\": lambda x: x}"
      )
    },
    character(1)
  )

  include_py <- paste0("[", paste(sprintf('"%s"', include), collapse = ", "), "]")
  script <- c(
    "import hddm",
    "import pandas as pd",
    "",
    paste0("data = pd.read_csv(r\"", normalizePath(data_file, winslash = "/", mustWork = FALSE), "\")"),
    "",
    "reg_models = [",
    paste(reg_lines, collapse = ",\n"),
    "]",
    "",
    "model = hddm.HDDMRegressor(",
    "    data,",
    "    reg_models,",
    paste0("    include=", include_py, ","),
    "    group_only_regressors=False",
    ")",
    "",
    paste0("model.sample(draws=", draws, ", burn=", burn, ", dbname=\"", dbname, "\", db=\"pickle\")"),
    "model.print_stats()",
    "model.save(\"gazepoint_hddm_model\")"
  )


  writeLines(script, output_file, useBytes = TRUE)
  invisible(output_file)
}


#' Select the next adaptive trial from candidate stimuli
#'
#' Provides a lightweight Bayesian-optimization-style acquisition helper for
#' adaptive testing. It assumes candidate-level posterior means and standard
#' deviations are already available or supplied by the user.
#'
#' @param candidates A data frame of candidate stimuli/trials.
#' @param mean Column containing posterior mean utility or expected information.
#' @param sd Column containing posterior uncertainty.
#' @param acquisition Acquisition rule: `"ucb"`, `"uncertainty"`, or
#'   `"expected_improvement"`.
#' @param kappa Exploration weight for UCB.
#' @param best_observed Best observed value for expected improvement.
#' @param maximize Logical; select maximum acquisition value if `TRUE`.
#'
#' @return One-row data frame corresponding to the selected candidate, with an
#'   added acquisition score.
#' @export
select_gazepoint_adaptive_trial <- function(
    candidates,
    mean,
    sd,
    acquisition = c("ucb", "uncertainty", "expected_improvement"),
    kappa = 2,
    best_observed = NULL,
    maximize = TRUE) {
  stopifnot(is.data.frame(candidates))
  acquisition <- match.arg(acquisition)

  if (!all(c(mean, sd) %in% names(candidates))) {
    stop("mean and sd columns must be present in candidates.", call. = FALSE)
  }

  mu <- candidates[[mean]]
  sigma <- candidates[[sd]]

  if (!is.numeric(mu) || !is.numeric(sigma)) {
    stop("mean and sd columns must be numeric.", call. = FALSE)
  }

  score <- switch(
    acquisition,
    ucb = mu + kappa * sigma,
    uncertainty = sigma,
    expected_improvement = {
      if (is.null(best_observed)) {
        best_observed <- if (maximize) max(mu, na.rm = TRUE) else min(mu, na.rm = TRUE)
      }
      improvement <- if (maximize) mu - best_observed else best_observed - mu
      z <- improvement / sigma
      z[!is.finite(z)] <- 0
      improvement * stats::pnorm(z) + sigma * stats::dnorm(z)
    }
  )

  candidates$acquisition_score <- score

  idx <- if (maximize) {
    which.max(candidates$acquisition_score)
  } else {
    which.min(candidates$acquisition_score)
  }

  candidates[idx, , drop = FALSE]
}


#' Classify gaze events with a lightweight unsupervised HMM
#'
#' Estimates gaze velocity, initializes hidden states by k-means, estimates a
#' Gaussian-emission HMM, and decodes the most likely sequence with Viterbi.
#' This is a lightweight package-internal HMM classifier and not a replacement
#' for validated laboratory event-detection software.
#'
#' @param data A data frame.
#' @param x X-coordinate column.
#' @param y Y-coordinate column.
#' @param time Time column.
#' @param subject Optional subject column for within-subject sequences.
#' @param n_states Number of hidden states.
#' @param state_labels Optional labels for states. If `NULL`, states are ordered
#'   by increasing mean velocity.
#'
#' @return The input data with velocity, hmm_state, and hmm_event columns.
#' @export
classify_gazepoint_events_hmm <- function(
    data,
    x,
    y,
    time,
    subject = NULL,
    n_states = 3,
    state_labels = NULL) {
  stopifnot(is.data.frame(data))

  required <- c(x, y, time, subject)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (n_states < 2L) {
    stop("n_states must be at least 2.", call. = FALSE)
  }

  if (is.null(state_labels)) {
    state_labels <- paste0("state_", seq_len(n_states))
  }

  if (length(state_labels) != n_states) {
    stop("state_labels must have length n_states.", call. = FALSE)
  }

  dat <- data
  dat$.row_id_gp <- seq_len(nrow(dat))

  split_key <- if (is.null(subject)) {
    factor(rep("all", nrow(dat)))
  } else {
    factor(dat[[subject]])
  }

  groups <- split(dat, split_key)

  classify_one <- function(d) {
    d <- d[order(d[[time]]), , drop = FALSE]

    dx <- c(NA_real_, diff(d[[x]]))
    dy <- c(NA_real_, diff(d[[y]]))
    dt <- c(NA_real_, diff(d[[time]]))

    velocity <- sqrt(dx^2 + dy^2) / dt
    velocity[!is.finite(velocity)] <- NA_real_

    obs <- velocity
    good <- is.finite(obs)

    d$gaze_velocity <- velocity
    d$hmm_state <- NA_integer_
    d$hmm_event <- NA_character_

    if (sum(good) < n_states * 3L) {
      return(d)
    }

    obs_good <- obs[good]
    km <- stats::kmeans(obs_good, centers = n_states, nstart = 10)

    state_init <- rep(NA_integer_, length(obs))
    state_init[good] <- km$cluster

    state_means <- tapply(obs_good, km$cluster, mean, na.rm = TRUE)
    order_states <- order(state_means)
    remap <- match(seq_len(n_states), order_states)
    state_init[good] <- remap[state_init[good]]

    means <- tapply(obs_good, state_init[good], mean, na.rm = TRUE)
    sds <- tapply(obs_good, state_init[good], stats::sd, na.rm = TRUE)
    sds[!is.finite(sds) | sds <= 0] <- stats::sd(obs_good, na.rm = TRUE)
    sds[!is.finite(sds) | sds <= 0] <- 1

    trans <- matrix(1, n_states, n_states)
    si <- state_init[good]
    if (length(si) > 1L) {
      for (i in seq_len(length(si) - 1L)) {
        trans[si[i], si[i + 1L]] <- trans[si[i], si[i + 1L]] + 1
      }
    }
    trans <- trans / rowSums(trans)

    init <- rep(1 / n_states, n_states)

    log_emit <- matrix(NA_real_, nrow = length(obs_good), ncol = n_states)
    for (s in seq_len(n_states)) {
      log_emit[, s] <- stats::dnorm(obs_good, mean = means[s], sd = sds[s], log = TRUE)
    }

    log_trans <- log(trans)
    log_init <- log(init)

    n <- nrow(log_emit)
    delta <- matrix(-Inf, nrow = n, ncol = n_states)
    psi <- matrix(1L, nrow = n, ncol = n_states)

    delta[1, ] <- log_init + log_emit[1, ]

    for (i in 2:n) {
      for (s in seq_len(n_states)) {
        vals <- delta[i - 1L, ] + log_trans[, s]
        psi[i, s] <- which.max(vals)
        delta[i, s] <- max(vals) + log_emit[i, s]
      }
    }

    path <- integer(n)
    path[n] <- which.max(delta[n, ])
    if (n > 1L) {
      for (i in seq(n - 1L, 1L)) {
        path[i] <- psi[i + 1L, path[i + 1L]]
      }
    }

    d$hmm_state[good] <- path

    ordered_labels <- state_labels
    d$hmm_event[good] <- ordered_labels[path]

    d
  }

  out <- do.call(rbind, lapply(groups, classify_one))
  out <- out[order(out$.row_id_gp), , drop = FALSE]
  out$.row_id_gp <- NULL
  rownames(out) <- NULL
  out
}


#' Impute missing pupil samples with a lightweight Gaussian-process smoother
#'
#' Performs within-subject/trial Gaussian-process interpolation using a squared
#' exponential kernel. This helper is intended for short missing segments after
#' blink detection, not for reconstructing long unusable trials.
#'
#' @param data A data frame.
#' @param pupil Pupil column.
#' @param time Time column.
#' @param subject Optional subject column.
#' @param trial Optional trial column.
#' @param length_scale Kernel length scale in the same unit as `time`.
#' @param noise Observation noise variance.
#' @param max_train Maximum number of observed samples used per sequence.
#' @param output Name of the imputed output column.
#' @param flag Name of the logical imputation flag column.
#'
#' @return Data frame with imputed pupil values and an imputation flag.
#' @export
impute_gazepoint_pupil_gp <- function(
    data,
    pupil,
    time,
    subject = NULL,
    trial = NULL,
    length_scale = NULL,
    noise = 1e-4,
    max_train = 300,
    output = "pupil_gp_imputed",
    flag = "pupil_was_gp_imputed") {
  stopifnot(is.data.frame(data))

  required <- c(pupil, time, subject, trial)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  dat <- data
  dat$.row_id_gp <- seq_len(nrow(dat))

  if (is.null(subject) && is.null(trial)) {
    key <- factor(rep("all", nrow(dat)))
  } else if (!is.null(subject) && is.null(trial)) {
    key <- factor(dat[[subject]])
  } else if (is.null(subject) && !is.null(trial)) {
    key <- factor(dat[[trial]])
  } else {
    key <- interaction(dat[[subject]], dat[[trial]], drop = TRUE)
  }

  kernel <- function(a, b, ell) {
    outer(a, b, function(x, y) exp(-0.5 * ((x - y) / ell)^2))
  }

  impute_one <- function(d) {
    d <- d[order(d[[time]]), , drop = FALSE]

    t <- d[[time]]
    y <- d[[pupil]]

    imputed <- y
    was_imputed <- rep(FALSE, length(y))

    obs <- is.finite(t) & is.finite(y)
    miss <- is.finite(t) & !is.finite(y)

    if (sum(obs) < 3L || !any(miss)) {
      d[[output]] <- imputed
      d[[flag]] <- was_imputed
      return(d)
    }

    train_t <- t[obs]
    train_y <- y[obs]

    if (length(train_t) > max_train) {
      idx <- unique(round(seq(1, length(train_t), length.out = max_train)))
      train_t <- train_t[idx]
      train_y <- train_y[idx]
    }

    ell <- length_scale
    if (is.null(ell)) {
      ell <- stats::median(diff(sort(unique(train_t))), na.rm = TRUE) * 10
      if (!is.finite(ell) || ell <= 0) {
        ell <- diff(range(train_t, na.rm = TRUE)) / 10
      }
      if (!is.finite(ell) || ell <= 0) {
        ell <- 1
      }
    }

    y_mean <- mean(train_y, na.rm = TRUE)
    y_centered <- train_y - y_mean

    k_tt <- kernel(train_t, train_t, ell)
    diag(k_tt) <- diag(k_tt) + noise

    k_mt <- kernel(t[miss], train_t, ell)

    alpha <- tryCatch(
      solve(k_tt, y_centered),
      error = function(e) NULL
    )

    if (is.null(alpha)) {
      d[[output]] <- imputed
      d[[flag]] <- was_imputed
      return(d)
    }

    pred <- as.vector(k_mt %*% alpha + y_mean)
    imputed[miss] <- pred
    was_imputed[miss] <- TRUE

    d[[output]] <- imputed
    d[[flag]] <- was_imputed
    d
  }

  out <- do.call(rbind, lapply(split(dat, key), impute_one))
  out <- out[order(out$.row_id_gp), , drop = FALSE]
  out$.row_id_gp <- NULL
  rownames(out) <- NULL
  out
}


#' Apply uncertainty filtering to Bayesian CNN or webcam gaze outputs
#'
#' Provides a lightweight post-processing helper for externally generated
#' webcam/CNN gaze predictions. The function does not train a CNN. It filters
#' or down-weights frame-level gaze estimates using an uncertainty column.
#'
#' @param data A data frame containing frame-level gaze predictions.
#' @param x Predicted x-coordinate column.
#' @param y Predicted y-coordinate column.
#' @param uncertainty Optional uncertainty column.
#' @param max_uncertainty Optional maximum allowed uncertainty.
#' @param weight_output Name of the output weight column.
#' @param valid_output Name of the output validity column.
#'
#' @return Data frame with uncertainty weights and validity flags.
#' @export
filter_gazepoint_cnn_uncertainty <- function(
    data,
    x,
    y,
    uncertainty = NULL,
    max_uncertainty = NULL,
    weight_output = "cnn_uncertainty_weight",
    valid_output = "cnn_valid_frame") {
  stopifnot(is.data.frame(data))

  required <- c(x, y, uncertainty)
  required <- required[!is.null(required)]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  out <- data
  valid <- is.finite(out[[x]]) & is.finite(out[[y]])

  if (is.null(uncertainty)) {
    out[[weight_output]] <- as.numeric(valid)
    out[[valid_output]] <- valid
    return(out)
  }

  u <- out[[uncertainty]]
  valid <- valid & is.finite(u)

  if (!is.null(max_uncertainty)) {
    valid <- valid & u <= max_uncertainty
  }

  scale_u <- stats::median(u[is.finite(u)], na.rm = TRUE)
  if (!is.finite(scale_u) || scale_u <= 0) {
    scale_u <- 1
  }

  weight <- exp(-u / scale_u)
  weight[!valid] <- 0

  out[[weight_output]] <- weight
  out[[valid_output]] <- valid
  out
}
