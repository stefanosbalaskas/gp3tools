# Internal helpers for high-priority signal extensions.

.gp3_hp_assert_data_frame <- function(x, arg) {
  if (!is.data.frame(x)) {
    stop(sprintf("`%s` must be a data frame.", arg), call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_hp_assert_columns <- function(data, columns, arg) {
  columns <- unique(columns)
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(
      sprintf(
        "`%s` is missing required column(s): %s.",
        arg,
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gp3_hp_group_keys <- function(data, columns) {
  if (!length(columns)) {
    return(rep(".all", nrow(data)))
  }

  parts <- lapply(data[columns], function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x
  })

  do.call(paste, c(parts, sep = "\r"))
}

.gp3_hp_split_indices <- function(data, columns) {
  if (!nrow(data)) {
    return(list())
  }
  keys <- .gp3_hp_group_keys(data, columns)
  split(seq_len(nrow(data)), keys, drop = TRUE)
}

.gp3_hp_time_info <- function(time, time_unit) {
  time_unit <- match.arg(
    time_unit,
    c("auto", "seconds", "milliseconds")
  )

  if (time_unit == "seconds") {
    return(list(to_seconds = 1, inferred_unit = "seconds"))
  }
  if (time_unit == "milliseconds") {
    return(list(to_seconds = 0.001, inferred_unit = "milliseconds"))
  }

  delta <- diff(sort(unique(time[is.finite(time)])))
  delta <- delta[is.finite(delta) & delta > 0]
  typical_delta <- if (length(delta)) stats::median(delta) else NA_real_

  if (is.finite(typical_delta) && typical_delta >= 1) {
    list(to_seconds = 0.001, inferred_unit = "milliseconds")
  } else {
    list(to_seconds = 1, inferred_unit = "seconds")
  }
}

.gp3_hp_true_runs <- function(flag) {
  flag[is.na(flag)] <- FALSE
  if (!length(flag) || !any(flag)) {
    return(data.frame(start = integer(), end = integer()))
  }

  encoded <- rle(flag)
  ends <- cumsum(encoded$lengths)
  starts <- ends - encoded$lengths + 1L
  keep <- encoded$values

  data.frame(
    start = starts[keep],
    end = ends[keep],
    row.names = NULL
  )
}

.gp3_hp_merge_true_runs <- function(flag, time_sec, max_gap_sec = 0) {
  runs <- .gp3_hp_true_runs(flag)
  if (nrow(runs) <= 1L || max_gap_sec <= 0) {
    return(runs)
  }

  merged <- list()
  current_start <- runs$start[1L]
  current_end <- runs$end[1L]
  counter <- 0L

  for (i in 2:nrow(runs)) {
    gap <- time_sec[runs$start[i]] - time_sec[current_end]
    if (is.finite(gap) && gap <= max_gap_sec) {
      current_end <- runs$end[i]
    } else {
      counter <- counter + 1L
      merged[[counter]] <- c(current_start, current_end)
      current_start <- runs$start[i]
      current_end <- runs$end[i]
    }
  }

  counter <- counter + 1L
  merged[[counter]] <- c(current_start, current_end)

  matrix_out <- do.call(rbind, merged)
  data.frame(
    start = matrix_out[, 1L],
    end = matrix_out[, 2L],
    row.names = NULL
  )
}

.gp3_hp_restore_class <- function(output, template) {
  if (inherits(template, "tbl_df")) {
    return(tibble::as_tibble(output))
  }
  output
}

.gp3_hp_detect_pupil_columns <- function(data, pupil_cols = NULL) {
  if (!is.null(pupil_cols)) {
    pupil_cols <- unique(as.character(pupil_cols))
    .gp3_hp_assert_columns(data, pupil_cols, "data")
    return(pupil_cols)
  }

  candidates <- c(
    "mean_pupil", "pupil_regressed", "pupil_smoothed",
    "pupil_interpolated", "pupil_clean", "pupil",
    "LPupil", "RPupil", "LPD", "RPD", "LPMM", "RPMM"
  )
  detected <- candidates[candidates %in% names(data)]

  if (!length(detected)) {
    stop(
      paste(
        "No pupil column was supplied or detected.",
        "Provide `pupil_col` or `pupil_cols` explicitly."
      ),
      call. = FALSE
    )
  }

  detected[1L]
}

.gp3_hp_roll <- function(x, window, method, min_valid) {
  n <- length(x)
  output <- rep(NA_real_, n)
  left_width <- floor((window - 1L) / 2L)
  right_width <- window - left_width - 1L

  fun <- if (method == "median") stats::median else mean

  for (i in seq_len(n)) {
    lower <- max(1L, i - left_width)
    upper <- min(n, i + right_width)
    values <- x[lower:upper]
    finite <- is.finite(values)

    if (sum(finite) >= min_valid) {
      output[i] <- fun(values[finite])
    }
  }

  output
}

.gp3_hp_interpolate_series <- function(time, value, method) {
  output <- rep(NA_real_, length(value))
  good <- is.finite(time) & is.finite(value)

  if (sum(good) < 2L) {
    return(output)
  }

  good_data <- data.frame(
    time = time[good],
    value = value[good]
  )
  good_data <- stats::aggregate(
    value ~ time,
    data = good_data,
    FUN = mean
  )
  good_data <- good_data[order(good_data$time), , drop = FALSE]

  if (nrow(good_data) < 2L) {
    return(output)
  }

  inside <- is.finite(time) &
    time >= min(good_data$time) &
    time <= max(good_data$time)

  if (method == "linear") {
    output[inside] <- stats::approx(
      x = good_data$time,
      y = good_data$value,
      xout = time[inside],
      rule = 1,
      ties = mean
    )$y
  } else {
    if (nrow(good_data) < 3L) {
      output[inside] <- stats::approx(
        x = good_data$time,
        y = good_data$value,
        xout = time[inside],
        rule = 1,
        ties = mean
      )$y
    } else {
      output[inside] <- stats::spline(
        x = good_data$time,
        y = good_data$value,
        xout = time[inside],
        method = "natural"
      )$y
    }
  }

  output[good] <- value[good]
  output
}

.gp3_hp_fit_line <- function(x, y) {
  design <- cbind(1, x)
  fit <- stats::lm.fit(design, y)
  coefficients <- fit$coefficients
  if (length(coefficients) != 2L ||
      any(!is.finite(coefficients))) {
    return(c(mean(y), 0))
  }
  coefficients
}

.gp3_hp_row_mean_two <- function(x, y) {
  available <- is.finite(x) + is.finite(y)
  x0 <- x
  y0 <- y
  x0[!is.finite(x0)] <- 0
  y0[!is.finite(y0)] <- 0
  output <- (x0 + y0) / pmax(available, 1)
  output[available == 0L] <- NA_real_
  output
}

.gp3_hp_window_to_seconds <- function(value, unit, input_to_seconds) {
  if (unit == "milliseconds") {
    return(value / 1000)
  }
  if (unit == "seconds") {
    return(value)
  }
  value * input_to_seconds
}

.gp3_hp_summary_stat <- function(values, finite, stat) {
  if (stat == "valid_prop") {
    return(if (length(values)) mean(finite) else NA_real_)
  }
  if (!any(finite)) {
    return(NA_real_)
  }

  x <- values[finite]

  switch(
    stat,
    mean = mean(x),
    sd = if (length(x) >= 2L) stats::sd(x) else NA_real_,
    median = stats::median(x),
    min = min(x),
    max = max(x),
    sum = sum(x),
    stop("Unknown summary statistic.", call. = FALSE)
  )
}
