#' Bootstrap time-course summaries
#'
#' Compute simple nonparametric bootstrap confidence intervals for a
#' time-varying gaze or pupil measure. The function is intentionally
#' lightweight and returns a tidy data frame that can be plotted or used as
#' a descriptive robustness check alongside model-based analyses.
#'
#' @param data A data frame.
#' @param time_col Name of the time column.
#' @param value_col Name of the numeric outcome column.
#' @param group_col Optional grouping column, for example condition.
#' @param subject_col Optional subject/participant column. If supplied,
#'   bootstrap resampling is performed over subjects within each time and
#'   group cell; otherwise rows are resampled.
#' @param n_boot Number of bootstrap draws.
#' @param ci Confidence level for the interval.
#' @param statistic Summary statistic, either \code{mean} or \code{median}.
#' @param difference_groups Optional character vector of length two. If
#'   supplied with \code{group_col}, the function also returns a bootstrap
#'   difference curve for group 1 minus group 2.
#' @param seed Optional random seed.
#'
#' @return A data frame with time, group/contrast, estimate, lower and upper
#'   bootstrap interval limits, sample size, and status columns.
#' @export
bootstrap_gazepoint_timecourse <- function(data,
                                           time_col,
                                           value_col,
                                           group_col = NULL,
                                           subject_col = NULL,
                                           n_boot = 1000,
                                           ci = 0.95,
                                           statistic = c("mean", "median"),
                                           difference_groups = NULL,
                                           seed = NULL) {
  .gp3_ext_check_data(data)
  time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  value_col <- .gp3_ext_check_scalar_string(value_col, "value_col")
  cols <- c(time_col, value_col)
  if (!is.null(group_col)) {
    group_col <- .gp3_ext_check_scalar_string(group_col, "group_col")
    cols <- c(cols, group_col)
  }
  if (!is.null(subject_col)) {
    subject_col <- .gp3_ext_check_scalar_string(subject_col, "subject_col")
    cols <- c(cols, subject_col)
  }
  .gp3_ext_check_columns(data, cols)

  if (!is.numeric(data[[value_col]])) {
    stop("value_col must identify a numeric column.", call. = FALSE)
  }
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) || n_boot < 1L) {
    stop("n_boot must be a positive integer.", call. = FALSE)
  }
  n_boot <- as.integer(n_boot)
  if (!is.numeric(ci) || length(ci) != 1L || is.na(ci) || ci <= 0 || ci >= 1) {
    stop("ci must be a single number between 0 and 1.", call. = FALSE)
  }
  statistic <- match.arg(statistic)
  stat_fun <- switch(statistic, mean = base::mean, median = stats::median)

  keep_cols <- unique(cols)
  d <- data[, keep_cols, drop = FALSE]
  d <- d[!is.na(d[[time_col]]) & !is.na(d[[value_col]]), , drop = FALSE]
  if (!is.null(group_col)) {
    d <- d[!is.na(d[[group_col]]), , drop = FALSE]
  }
  if (!is.null(subject_col)) {
    d <- d[!is.na(d[[subject_col]]), , drop = FALSE]
  }

  if (nrow(d) == 0L) {
    return(data.frame(
      time = numeric(0), group = character(0), contrast = character(0),
      estimate = numeric(0), lower = numeric(0), upper = numeric(0),
      n = integer(0), n_boot = integer(0), statistic = character(0),
      ci_level = numeric(0), bootstrap_status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  boot_one <- function(block) {
    x <- block[[value_col]]
    estimate <- stat_fun(x, na.rm = TRUE)
    if (length(x) == 0L || all(is.na(x))) {
      return(c(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
    }
    draws <- numeric(n_boot)
    if (!is.null(subject_col)) {
      ids <- unique(block[[subject_col]])
      for (i in seq_len(n_boot)) {
        sampled <- sample(ids, length(ids), replace = TRUE)
        sampled_x <- unlist(lapply(sampled, function(id) {
          block[block[[subject_col]] == id, value_col, drop = TRUE]
        }), use.names = FALSE)
        draws[i] <- stat_fun(sampled_x, na.rm = TRUE)
      }
    } else {
      for (i in seq_len(n_boot)) {
        draws[i] <- stat_fun(sample(x, length(x), replace = TRUE), na.rm = TRUE)
      }
    }
    alpha <- (1 - ci) / 2
    qs <- stats::quantile(draws, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
    c(estimate = estimate, lower = qs[1L], upper = qs[2L])
  }

  if (is.null(group_col)) {
    d$.gp3_group <- "all"
    group_col2 <- ".gp3_group"
  } else {
    group_col2 <- group_col
  }

  key <- interaction(d[[time_col]], d[[group_col2]], drop = TRUE, lex.order = TRUE)
  blocks <- split(d, key)
  rows <- lapply(blocks, function(block) {
    vals <- boot_one(block)
    data.frame(
      time = block[[time_col]][1L],
      group = as.character(block[[group_col2]][1L]),
      contrast = NA_character_,
      estimate = unname(vals["estimate"]),
      lower = unname(vals["lower"]),
      upper = unname(vals["upper"]),
      n = nrow(block),
      n_boot = n_boot,
      statistic = statistic,
      ci_level = ci,
      bootstrap_status = "ok",
      stringsAsFactors = FALSE
    )
  })
  out <- .gp3_ext_bind_rows(rows)

  if (!is.null(difference_groups)) {
    if (is.null(group_col)) {
      stop("difference_groups requires group_col.", call. = FALSE)
    }
    difference_groups <- as.character(difference_groups)
    if (length(difference_groups) != 2L) {
      stop("difference_groups must have length two.", call. = FALSE)
    }
    diff_rows <- lapply(sort(unique(d[[time_col]])), function(tt) {
      a <- d[d[[time_col]] == tt & as.character(d[[group_col]]) == difference_groups[1L], , drop = FALSE]
      b <- d[d[[time_col]] == tt & as.character(d[[group_col]]) == difference_groups[2L], , drop = FALSE]
      if (nrow(a) == 0L || nrow(b) == 0L) {
        return(NULL)
      }
      est <- stat_fun(a[[value_col]], na.rm = TRUE) - stat_fun(b[[value_col]], na.rm = TRUE)
      draws <- numeric(n_boot)
      for (i in seq_len(n_boot)) {
        xa <- sample(a[[value_col]], nrow(a), replace = TRUE)
        xb <- sample(b[[value_col]], nrow(b), replace = TRUE)
        draws[i] <- stat_fun(xa, na.rm = TRUE) - stat_fun(xb, na.rm = TRUE)
      }
      alpha <- (1 - ci) / 2
      qs <- stats::quantile(draws, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
      data.frame(
        time = tt,
        group = "difference",
        contrast = paste(difference_groups, collapse = " - "),
        estimate = est,
        lower = qs[1L],
        upper = qs[2L],
        n = nrow(a) + nrow(b),
        n_boot = n_boot,
        statistic = statistic,
        ci_level = ci,
        bootstrap_status = "ok",
        stringsAsFactors = FALSE
      )
    })
    out <- rbind(out, .gp3_ext_bind_rows(diff_rows))
  }

  rownames(out) <- NULL
  out
}
