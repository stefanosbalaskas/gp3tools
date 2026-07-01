.gp3_ext_first_existing_col <- function(data, candidates) {
  nms <- names(data)
  idx <- match(tolower(candidates), tolower(nms), nomatch = 0L)
  idx <- idx[idx > 0L]
  if (length(idx) == 0L) NULL else nms[idx[1L]]
}

.gp3_ext_model_values <- function(model, extractor, label) {
  out <- tryCatch(extractor(model), error = function(e) NULL)
  if (is.null(out)) {
    stop("Could not extract ", label, " from model.", call. = FALSE)
  }
  as.numeric(out)
}

#' Plot a time-varying effect curve
#'
#' Plot a time-varying effect, difference curve, or model-prediction contrast
#' from a tidy data frame. This helper is intentionally lightweight: it does
#' not refit a model, but visualises already computed estimates and optional
#' interval bounds from GAMM, GCA, cluster, bootstrap, or prediction workflows.
#'
#' @param data A data frame containing time-varying estimates.
#' @param time_col Name of the time column.
#' @param estimate_col Name of the estimate/effect column.
#' @param lower_col Optional lower interval column.
#' @param upper_col Optional upper interval column.
#' @param group_col Optional grouping/contrast column.
#' @param zero_line Should a horizontal zero reference line be shown?
#' @param title Optional plot title.
#' @param x_label Optional x-axis label.
#' @param y_label Optional y-axis label.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_time_varying_effect <- function(data,
                                               time_col,
                                               estimate_col,
                                               lower_col = NULL,
                                               upper_col = NULL,
                                               group_col = NULL,
                                               zero_line = TRUE,
                                               title = NULL,
                                               x_label = NULL,
                                               y_label = NULL) {
  .gp3_ext_check_data(data)
  time_col <- .gp3_ext_check_scalar_string(time_col, "time_col")
  estimate_col <- .gp3_ext_check_scalar_string(estimate_col, "estimate_col")
  if (!is.null(lower_col)) lower_col <- .gp3_ext_check_scalar_string(lower_col, "lower_col")
  if (!is.null(upper_col)) upper_col <- .gp3_ext_check_scalar_string(upper_col, "upper_col")
  if (!is.null(group_col)) group_col <- .gp3_ext_check_scalar_string(group_col, "group_col")
  .gp3_ext_check_columns(data, c(time_col, estimate_col, lower_col, upper_col, group_col))
  if (!is.numeric(data[[time_col]]) || !is.numeric(data[[estimate_col]])) {
    stop("time_col and estimate_col must identify numeric columns.", call. = FALSE)
  }
  if (!is.null(lower_col) && !is.numeric(data[[lower_col]])) {
    stop("lower_col must identify a numeric column.", call. = FALSE)
  }
  if (!is.null(upper_col) && !is.numeric(data[[upper_col]])) {
    stop("upper_col must identify a numeric column.", call. = FALSE)
  }

  d <- data[!is.na(data[[time_col]]) & !is.na(data[[estimate_col]]), , drop = FALSE]
  d$.gp3_time <- as.numeric(d[[time_col]])
  d$.gp3_estimate <- as.numeric(d[[estimate_col]])
  d$.gp3_group <- if (is.null(group_col)) "effect" else as.character(d[[group_col]])
  d$.gp3_lower <- if (is.null(lower_col)) NA_real_ else as.numeric(d[[lower_col]])
  d$.gp3_upper <- if (is.null(upper_col)) NA_real_ else as.numeric(d[[upper_col]])

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .gp3_time, y = .gp3_estimate, group = .gp3_group))
  if (isTRUE(zero_line)) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed")
  }
  if (!is.null(lower_col) && !is.null(upper_col)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .gp3_lower, ymax = .gp3_upper),
      alpha = 0.20
    )
  }
  if (is.null(group_col)) {
    p <- p + ggplot2::geom_line(linewidth = 0.6)
  } else {
    p <- p + ggplot2::geom_line(ggplot2::aes(linetype = .gp3_group), linewidth = 0.6)
  }
  p +
    ggplot2::labs(
      title = title,
      x = if (is.null(x_label)) time_col else x_label,
      y = if (is.null(y_label)) estimate_col else y_label,
      linetype = if (is.null(group_col)) NULL else group_col
    ) +
    ggplot2::theme_minimal()
}

#' Plot model residual diagnostics
#'
#' Create a compact residual diagnostic plot from either a fitted model object
#' with \code{residuals()} and \code{fitted()} methods, or a data frame that
#' already contains fitted values and residuals.
#'
#' @param model Optional fitted model object.
#' @param data Optional data frame containing fitted and residual columns.
#' @param fitted_col Fitted-value column when \code{data} is supplied.
#' @param residual_col Residual column when \code{data} is supplied.
#' @param type Diagnostic plot type: residuals-versus-fitted or QQ plot.
#' @param title Optional plot title.
#'
#' @return A ggplot object.
#' @export
plot_gazepoint_model_residuals <- function(model = NULL,
                                          data = NULL,
                                          fitted_col = NULL,
                                          residual_col = NULL,
                                          type = c("residuals_fitted", "qq"),
                                          title = NULL) {
  type <- match.arg(type)
  if (!is.null(data)) {
    .gp3_ext_check_data(data)
    fitted_col <- if (is.null(fitted_col)) {
      .gp3_ext_first_existing_col(data, c("fitted", "fitted_value", ".fitted"))
    } else {
      .gp3_ext_check_scalar_string(fitted_col, "fitted_col")
    }
    residual_col <- if (is.null(residual_col)) {
      .gp3_ext_first_existing_col(data, c("residual", "residuals", ".resid", ".residual"))
    } else {
      .gp3_ext_check_scalar_string(residual_col, "residual_col")
    }
    if (is.null(fitted_col) || is.null(residual_col)) {
      stop("Could not identify fitted and residual columns.", call. = FALSE)
    }
    .gp3_ext_check_columns(data, c(fitted_col, residual_col))
    fitted_values <- as.numeric(data[[fitted_col]])
    residual_values <- as.numeric(data[[residual_col]])
  } else {
    if (is.null(model)) {
      stop("Supply either model or data.", call. = FALSE)
    }
    fitted_values <- .gp3_ext_model_values(model, stats::fitted, "fitted values")
    residual_values <- .gp3_ext_model_values(model, stats::residuals, "residuals")
  }
  d <- data.frame(
    fitted = fitted_values,
    residual = residual_values,
    stringsAsFactors = FALSE
  )
  d <- d[is.finite(d$fitted) & is.finite(d$residual), , drop = FALSE]
  if (nrow(d) == 0L) {
    stop("No finite fitted/residual pairs are available.", call. = FALSE)
  }

  if (type == "residuals_fitted") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = fitted, y = residual)) +
      ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
      ggplot2::geom_point() +
      ggplot2::labs(
        title = title,
        x = "Fitted values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal()
    return(p)
  }

  ggplot2::ggplot(d, ggplot2::aes(sample = residual)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(
      title = title,
      x = "Theoretical quantiles",
      y = "Residual quantiles"
    ) +
    ggplot2::theme_minimal()
}

.gp3_ext_as_branch_frame <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x)) {
    dfs <- x[vapply(x, is.data.frame, logical(1))]
    if (length(dfs) > 0L) {
      names(dfs) <- if (is.null(names(dfs))) paste0("branch_", seq_along(dfs)) else names(dfs)
      rows <- lapply(seq_along(dfs), function(i) {
        d <- dfs[[i]]
        d$.gp3_branch <- names(dfs)[i]
        d
      })
      return(.gp3_ext_bind_rows(rows))
    }
  }
  stop("multiverse_results must be a data frame or a list containing data frames.", call. = FALSE)
}

#' Report multiverse-analysis results
#'
#' Create compact branch-, status-, and term-level summaries from a multiverse
#' result object. This helper is intentionally generic and can summarise the
#' package's multiverse output after it has been tidied to data frames.
#'
#' @param multiverse_results A data frame, or a list containing data frames.
#' @param branch_col Optional branch/specification column.
#' @param term_col Optional model term column.
#' @param estimate_col Optional estimate/effect column.
#' @param p_col Optional p-value column.
#' @param status_col Optional status column.
#' @param alpha Significance threshold used for descriptive counts.
#'
#' @return A list with branch, status, and term summaries.
#' @export
report_gazepoint_multiverse <- function(multiverse_results,
                                       branch_col = NULL,
                                       term_col = NULL,
                                       estimate_col = NULL,
                                       p_col = NULL,
                                       status_col = NULL,
                                       alpha = 0.05) {
  d <- .gp3_ext_as_branch_frame(multiverse_results)
  .gp3_ext_check_data(d)
  branch_col <- if (is.null(branch_col)) {
    .gp3_ext_first_existing_col(d, c("branch", "specification", "model", "analysis", ".gp3_branch"))
  } else {
    .gp3_ext_check_scalar_string(branch_col, "branch_col")
  }
  term_col <- if (is.null(term_col)) {
    .gp3_ext_first_existing_col(d, c("term", "parameter", "effect", "coefficient"))
  } else {
    .gp3_ext_check_scalar_string(term_col, "term_col")
  }
  estimate_col <- if (is.null(estimate_col)) {
    .gp3_ext_first_existing_col(d, c("estimate", "effect_size", "beta", "b"))
  } else {
    .gp3_ext_check_scalar_string(estimate_col, "estimate_col")
  }
  p_col <- if (is.null(p_col)) {
    .gp3_ext_first_existing_col(d, c("p.value", "p_value", "p", "Pr(>|z|)", "Pr(>|t|)"))
  } else {
    .gp3_ext_check_scalar_string(p_col, "p_col")
  }
  status_col <- if (is.null(status_col)) {
    .gp3_ext_first_existing_col(d, c("status", "model_status", "fit_status", "branch_status"))
  } else {
    .gp3_ext_check_scalar_string(status_col, "status_col")
  }
  .gp3_ext_check_columns(d, c(branch_col, term_col, estimate_col, p_col, status_col))
  if (is.null(branch_col)) {
    d$.gp3_branch <- "all"
    branch_col <- ".gp3_branch"
  }
  if (is.null(status_col)) {
    d$.gp3_status <- "unknown"
    status_col <- ".gp3_status"
  }

  branch_split <- split(d, d[[branch_col]], drop = TRUE)
  branch_summary <- .gp3_ext_bind_rows(lapply(branch_split, function(block) {
    data.frame(
      branch = as.character(block[[branch_col]][1L]),
      n_rows = nrow(block),
      n_terms = if (!is.null(term_col)) length(unique(block[[term_col]])) else NA_integer_,
      status = paste(sort(unique(as.character(block[[status_col]]))), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(branch_summary) <- NULL

  status_tab <- as.data.frame(table(as.character(d[[status_col]])), stringsAsFactors = FALSE)
  names(status_tab) <- c("status", "n")
  status_tab$prop <- status_tab$n / sum(status_tab$n)

  if (!is.null(term_col) && (!is.null(estimate_col) || !is.null(p_col))) {
    term_split <- split(d, d[[term_col]], drop = TRUE)
    term_summary <- .gp3_ext_bind_rows(lapply(term_split, function(block) {
      est <- if (!is.null(estimate_col)) as.numeric(block[[estimate_col]]) else rep(NA_real_, nrow(block))
      p <- if (!is.null(p_col)) as.numeric(block[[p_col]]) else rep(NA_real_, nrow(block))
      data.frame(
        term = as.character(block[[term_col]][1L]),
        n_branches = length(unique(block[[branch_col]])),
        n_estimates = sum(!is.na(est)),
        mean_estimate = if (all(is.na(est))) NA_real_ else mean(est, na.rm = TRUE),
        median_estimate = if (all(is.na(est))) NA_real_ else stats::median(est, na.rm = TRUE),
        min_estimate = if (all(is.na(est))) NA_real_ else min(est, na.rm = TRUE),
        max_estimate = if (all(is.na(est))) NA_real_ else max(est, na.rm = TRUE),
        prop_positive = if (all(is.na(est))) NA_real_ else mean(est > 0, na.rm = TRUE),
        prop_significant = if (all(is.na(p))) NA_real_ else mean(p < alpha, na.rm = TRUE),
        min_p = if (all(is.na(p))) NA_real_ else min(p, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    term_summary <- data.frame(stringsAsFactors = FALSE)
  }
  rownames(term_summary) <- NULL

  out <- list(
    branch_summary = branch_summary,
    status_summary = status_tab,
    term_summary = term_summary,
    columns = list(
      branch_col = branch_col,
      term_col = term_col,
      estimate_col = estimate_col,
      p_col = p_col,
      status_col = status_col
    ),
    alpha = alpha,
    report_status = "ok"
  )
  class(out) <- c("gp3_multiverse_report", "list")
  out
}
