#' Summarize missingness in Gazepoint-style data
#'
#' Computes missingness and observed-data rates for selected columns, optionally
#' within grouping variables such as participant, trial, condition, or AOI. The
#' helper is intended for transparent data-coverage reporting and does not make
#' exclusion decisions.
#'
#' @param data A data frame.
#' @param cols Optional character vector of columns to summarize. If `NULL`,
#'   all non-grouping columns are summarized.
#' @param group_cols Optional character vector of grouping columns.
#' @param include_group_cols If `TRUE`, grouping columns can also be summarized
#'   when `cols = NULL`.
#'
#' @return A data frame with one row per variable and group.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' summarize_gazepoint_missingness(
#'   x,
#'   cols = c("pupil_left", "pupil_right", "pupil"),
#'   group_cols = "condition"
#' )
summarize_gazepoint_missingness <- function(data,
                                            cols = NULL,
                                            group_cols = NULL,
                                            include_group_cols = FALSE) {
  .gp3_require_data_frame(data, "data")

  if (!is.null(group_cols)) {
    .gp3_require_columns(data, group_cols, "data")
  }

  if (is.null(cols)) {
    cols <- names(data)

    if (!isTRUE(include_group_cols) && !is.null(group_cols)) {
      cols <- setdiff(cols, group_cols)
    }
  } else {
    .gp3_require_columns(data, cols, "data")
  }

  if (!is.character(cols) || length(cols) < 1L) {
    stop("`cols` must identify at least one column.", call. = FALSE)
  }

  if (is.null(group_cols) || length(group_cols) == 0L) {
    group_id <- rep("all", nrow(data))
  } else {
    group_id <- as.character(interaction(data[group_cols], drop = TRUE, lex.order = TRUE))
  }

  split_rows <- split(seq_len(nrow(data)), group_id)

  out <- lapply(names(split_rows), function(id) {
    rows <- split_rows[[id]]

    variable_summaries <- lapply(cols, function(col) {
      values <- data[[col]][rows]
      missing <- is.na(values)

      data.frame(
        group_id = id,
        variable = col,
        n_rows = length(values),
        n_missing = sum(missing),
        n_observed = sum(!missing),
        missing_rate = mean(missing),
        observed_rate = mean(!missing),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, variable_summaries)
  })

  result <- do.call(rbind, out)
  row.names(result) <- NULL

  attr(result, "gp3_missingness_settings") <- list(
    cols = cols,
    group_cols = group_cols,
    include_group_cols = include_group_cols
  )

  result
}


#' @rdname summarize_gazepoint_missingness
#' @export
summarise_gazepoint_missingness <- summarize_gazepoint_missingness


#' Plot a Gazepoint missingness profile
#'
#' Creates a descriptive plot of missingness rates from raw data or from the
#' output of `summarize_gazepoint_missingness()`. The plot is intended for
#' quality-control review and reporting.
#'
#' @param data A data frame or a missingness summary produced by
#'   `summarize_gazepoint_missingness()`.
#' @param cols Optional columns to summarize when `data` is raw data.
#' @param group_cols Optional grouping columns when `data` is raw data.
#' @param plot_type Either `"bar"` or `"tile"`.
#' @param title Optional plot title.
#' @param y_label Optional y-axis label for bar plots.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' plot_gazepoint_missingness_profile(
#'   x,
#'   cols = c("pupil_left", "pupil_right", "pupil"),
#'   group_cols = "condition"
#' )
plot_gazepoint_missingness_profile <- function(data,
                                               cols = NULL,
                                               group_cols = NULL,
                                               plot_type = c("bar", "tile"),
                                               title = NULL,
                                               y_label = "Missingness rate") {
  plot_type <- match.arg(plot_type)

  .gp3_require_data_frame(data, "data")

  if (.gp3_is_missingness_summary(data)) {
    summary <- data
  } else {
    summary <- summarize_gazepoint_missingness(
      data,
      cols = cols,
      group_cols = group_cols
    )
  }

  .gp3_require_columns(
    summary,
    c("group_id", "variable", "missing_rate"),
    "missingness summary"
  )

  plot_data <- summary
  plot_data$.gp3_variable <- factor(plot_data$variable, levels = unique(plot_data$variable))
  plot_data$.gp3_group <- factor(plot_data$group_id, levels = unique(plot_data$group_id))
  plot_data$.gp3_missing_rate <- as.numeric(plot_data$missing_rate)

  if (identical(plot_type, "bar")) {
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .gp3_variable,
        y = .gp3_missing_rate,
        group = .gp3_group,
        fill = .gp3_group
      )
    ) +
      ggplot2::geom_col(position = "dodge", na.rm = TRUE) +
      ggplot2::labs(
        title = title,
        x = "Variable",
        y = y_label,
        fill = "Group"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (length(unique(plot_data$.gp3_group)) == 1L) {
      p <- p + ggplot2::guides(fill = "none")
    }

    return(p)
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .gp3_variable,
      y = .gp3_group,
      fill = .gp3_missing_rate
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::labs(
      title = title,
      x = "Variable",
      y = "Group",
      fill = "Missingness rate"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


#' Report Gazepoint missingness
#'
#' Produces a compact, cautious text summary of missingness rates. The report is
#' intended for transparent methods/results documentation and does not recommend
#' exclusions.
#'
#' @param data A data frame or a missingness summary produced by
#'   `summarize_gazepoint_missingness()`.
#' @param cols Optional columns to summarize when `data` is raw data.
#' @param group_cols Optional grouping columns when `data` is raw data.
#' @param digits Number of decimal places for percentages.
#' @param max_variables Maximum number of highest-missingness variables to name
#'   in the report text.
#'
#' @return A list with `summary`, `overall`, and `report_text`.
#' @export
#'
#' @examples
#' x <- simulate_gazepoint_pupil_data(n_subjects = 2, n_trials = 2, n_time_bins = 5, seed = 1)
#' report_gazepoint_missingness(
#'   x,
#'   cols = c("pupil_left", "pupil_right", "pupil")
#' )
report_gazepoint_missingness <- function(data,
                                         cols = NULL,
                                         group_cols = NULL,
                                         digits = 1,
                                         max_variables = 5) {
  .gp3_require_data_frame(data, "data")
  .gp3_require_positive_integer(max_variables, "max_variables")

  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) || digits < 0) {
    stop("`digits` must be a single non-negative numeric value.", call. = FALSE)
  }

  if (.gp3_is_missingness_summary(data)) {
    summary <- data
  } else {
    summary <- summarize_gazepoint_missingness(
      data,
      cols = cols,
      group_cols = group_cols
    )
  }

  .gp3_require_columns(
    summary,
    c("group_id", "variable", "n_rows", "n_missing", "missing_rate"),
    "missingness summary"
  )

  variable_summary <- stats::aggregate(
    cbind(n_rows, n_missing) ~ variable,
    data = summary,
    FUN = sum
  )

  variable_summary$missing_rate <- with(
    variable_summary,
    ifelse(n_rows > 0, n_missing / n_rows, NA_real_)
  )

  variable_summary <- variable_summary[
    order(variable_summary$missing_rate, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  total_rows <- sum(variable_summary$n_rows)
  total_missing <- sum(variable_summary$n_missing)
  overall_missing_rate <- if (total_rows > 0) total_missing / total_rows else NA_real_

  top_variables <- utils::head(variable_summary, max_variables)

  top_text <- paste(
    paste0(
      top_variables$variable,
      " (",
      round(100 * top_variables$missing_rate, digits),
      "%)"
    ),
    collapse = ", "
  )

  if (!nzchar(top_text)) {
    top_text <- "no variables"
  }

  report_text <- paste0(
    "Missingness was summarized across ",
    length(unique(summary$variable)),
    " variable(s). The overall cell-level missingness rate was ",
    round(100 * overall_missing_rate, digits),
    "%. The highest missingness variable(s) were: ",
    top_text,
    ". These values are descriptive data-coverage diagnostics and do not by themselves define exclusion decisions."
  )

  overall <- data.frame(
    n_variables = length(unique(summary$variable)),
    n_groups = length(unique(summary$group_id)),
    total_cells = total_rows,
    total_missing = total_missing,
    overall_missing_rate = overall_missing_rate,
    stringsAsFactors = FALSE
  )

  list(
    summary = summary,
    overall = overall,
    variable_summary = variable_summary,
    report_text = report_text
  )
}


.gp3_is_missingness_summary <- function(data) {
  is.data.frame(data) &&
    all(c("group_id", "variable", "n_rows", "n_missing", "missing_rate") %in% names(data))
}
