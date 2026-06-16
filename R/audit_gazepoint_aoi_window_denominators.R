#' Audit AOI window denominators before binomial modelling
#'
#' Audit AOI-window sample denominators before confirmatory binomial or
#' logistic mixed-effects modelling. The function is designed for output from
#' `summarise_gazepoint_aoi_windows()`.
#'
#' @param data AOI-window summary data.
#' @param window_col Name of the AOI-window label column.
#' @param window_start_col Optional window-start column.
#' @param window_end_col Optional window-end column.
#' @param denominator_col Name of the denominator column to audit.
#' @param total_col Name of the total window-sample column.
#' @param target_col Name of the target-success count column.
#' @param condition_col Optional condition column.
#' @param group_cols Optional grouping columns for row-level audit context.
#' @param min_denominator_samples Minimum acceptable denominator count.
#' @param min_valid_denominator_prop Minimum acceptable valid-denominator
#'   proportion relative to total window samples.
#' @param max_denominator_cv Maximum acceptable denominator coefficient of
#'   variation within each window.
#' @param max_condition_ratio Maximum acceptable ratio between the largest and
#'   smallest mean denominator across conditions within a window.
#'
#' @return A named list containing overview, row audit, window summary,
#'   condition-window summary, denominator-imbalance summary, flagged rows, and
#'   settings.
#'
#' @export
#' @importFrom rlang .data
audit_gazepoint_aoi_window_denominators <- function(
    data,
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    denominator_col = "n_valid_denominator_samples",
    total_col = "n_window_samples",
    target_col = "n_target_samples",
    condition_col = "condition",
    group_cols = NULL,
    min_denominator_samples = 5,
    min_valid_denominator_prop = 0.70,
    max_denominator_cv = 0.25,
    max_condition_ratio = 2
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_column <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_optional_column <- function(x, arg) {
    if (!is.null(x) &&
        (!is.character(x) ||
         length(x) != 1L ||
         is.na(x) ||
         !nzchar(x))) {
      stop(
        "`", arg, "` must be NULL or a non-missing character scalar.",
        call. = FALSE
      )
    }
  }

  valid_positive_numeric <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x <= 0) {
      stop(
        "`", arg, "` must be a positive finite numeric scalar.",
        call. = FALSE
      )
    }
  }

  valid_probability <- function(x, arg) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !is.finite(x) ||
        x < 0 ||
        x > 1) {
      stop(
        "`", arg, "` must be a finite numeric scalar in [0, 1].",
        call. = FALSE
      )
    }
  }

  valid_column(window_col, "window_col")
  valid_optional_column(window_start_col, "window_start_col")
  valid_optional_column(window_end_col, "window_end_col")
  valid_column(denominator_col, "denominator_col")
  valid_column(total_col, "total_col")
  valid_column(target_col, "target_col")
  valid_optional_column(condition_col, "condition_col")

  if (!is.null(group_cols) &&
      (!is.character(group_cols) ||
       any(is.na(group_cols)) ||
       any(!nzchar(group_cols)))) {
    stop(
      "`group_cols` must be NULL or a character vector of column names.",
      call. = FALSE
    )
  }

  valid_positive_numeric(min_denominator_samples, "min_denominator_samples")
  valid_probability(min_valid_denominator_prop, "min_valid_denominator_prop")
  valid_positive_numeric(max_denominator_cv, "max_denominator_cv")
  valid_positive_numeric(max_condition_ratio, "max_condition_ratio")

  dat <- tibble::as_tibble(data)

  required_cols <- c(window_col, denominator_col, total_col, target_col)

  if (!is.null(window_start_col) && window_start_col %in% names(dat)) {
    required_cols <- c(required_cols, window_start_col)
  }

  if (!is.null(window_end_col) && window_end_col %in% names(dat)) {
    required_cols <- c(required_cols, window_end_col)
  }

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    required_cols <- c(required_cols, condition_col)
  }

  if (!is.null(group_cols)) {
    required_cols <- c(required_cols, group_cols)
  }

  missing_cols <- setdiff(unique(required_cols), names(dat))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(group_cols)) {
    group_cols <- intersect(
      c("subject", condition_col, "MEDIA_ID", "trial_global", "trial"),
      names(dat)
    )
  }

  dat$.gp3_window <- as.character(dat[[window_col]])
  dat$.gp3_window[
    is.na(dat$.gp3_window) |
      !nzchar(trimws(dat$.gp3_window))
  ] <- "unknown_window"
  dat$.gp3_window <- trimws(dat$.gp3_window)

  dat$.gp3_denominator <- suppressWarnings(as.numeric(dat[[denominator_col]]))
  dat$.gp3_total <- suppressWarnings(as.numeric(dat[[total_col]]))
  dat$.gp3_target <- suppressWarnings(as.numeric(dat[[target_col]]))

  if (!is.null(condition_col) && condition_col %in% names(dat)) {
    dat$.gp3_condition <- as.character(dat[[condition_col]])
    dat$.gp3_condition <- trimws(dat$.gp3_condition)
    dat$.gp3_condition[
      is.na(dat$.gp3_condition) |
        !nzchar(dat$.gp3_condition)
    ] <- "all_data"
  } else {
    dat$.gp3_condition <- "all_data"
  }

  if (!is.null(window_start_col) && window_start_col %in% names(dat)) {
    dat$.gp3_window_start <- suppressWarnings(as.numeric(dat[[window_start_col]]))
  } else {
    dat$.gp3_window_start <- NA_real_
  }

  if (!is.null(window_end_col) && window_end_col %in% names(dat)) {
    dat$.gp3_window_end <- suppressWarnings(as.numeric(dat[[window_end_col]]))
  } else {
    dat$.gp3_window_end <- NA_real_
  }

  dat$.gp3_valid_denominator_prop <- dplyr::if_else(
    is.finite(dat$.gp3_total) & dat$.gp3_total > 0,
    dat$.gp3_denominator / dat$.gp3_total,
    NA_real_
  )

  dat$.gp3_failure <- dat$.gp3_denominator - dat$.gp3_target

  row_audit <- dat |>
    dplyr::mutate(
      denominator_missing = !is.finite(.data[[".gp3_denominator"]]),
      total_missing = !is.finite(.data[[".gp3_total"]]),
      target_missing = !is.finite(.data[[".gp3_target"]]),
      denominator_negative = is.finite(.data[[".gp3_denominator"]]) &
        .data[[".gp3_denominator"]] < 0,
      total_non_positive = is.finite(.data[[".gp3_total"]]) &
        .data[[".gp3_total"]] <= 0,
      target_negative = is.finite(.data[[".gp3_target"]]) &
        .data[[".gp3_target"]] < 0,
      target_exceeds_denominator = is.finite(.data[[".gp3_target"]]) &
        is.finite(.data[[".gp3_denominator"]]) &
        .data[[".gp3_target"]] > .data[[".gp3_denominator"]],
      denominator_zero = is.finite(.data[[".gp3_denominator"]]) &
        .data[[".gp3_denominator"]] == 0,
      denominator_low = is.finite(.data[[".gp3_denominator"]]) &
        .data[[".gp3_denominator"]] > 0 &
        .data[[".gp3_denominator"]] < min_denominator_samples,
      valid_denominator_prop_low = is.finite(.data[[".gp3_valid_denominator_prop"]]) &
        .data[[".gp3_valid_denominator_prop"]] < min_valid_denominator_prop,
      target_zero = is.finite(.data[[".gp3_target"]]) &
        .data[[".gp3_target"]] == 0,
      target_all = is.finite(.data[[".gp3_target"]]) &
        is.finite(.data[[".gp3_denominator"]]) &
        .data[[".gp3_denominator"]] > 0 &
        .data[[".gp3_target"]] == .data[[".gp3_denominator"]],
      denominator_audit_status = dplyr::case_when(
        .data[["denominator_missing"]] ~ "missing_denominator",
        .data[["total_missing"]] ~ "missing_total",
        .data[["target_missing"]] ~ "missing_target",
        .data[["denominator_negative"]] ~ "negative_denominator",
        .data[["total_non_positive"]] ~ "non_positive_total",
        .data[["target_negative"]] ~ "negative_target",
        .data[["target_exceeds_denominator"]] ~ "target_exceeds_denominator",
        .data[["denominator_zero"]] ~ "zero_denominator",
        .data[["denominator_low"]] ~ "low_denominator",
        .data[["valid_denominator_prop_low"]] ~ "low_valid_denominator_prop",
        TRUE ~ "ok"
      )
    )

  overview <- row_audit |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      n_windows = dplyr::n_distinct(.data[[".gp3_window"]]),
      n_conditions = dplyr::n_distinct(.data[[".gp3_condition"]]),
      n_missing_denominator = sum(.data[["denominator_missing"]], na.rm = TRUE),
      n_zero_denominator = sum(.data[["denominator_zero"]], na.rm = TRUE),
      n_low_denominator = sum(.data[["denominator_low"]], na.rm = TRUE),
      n_low_valid_denominator_prop = sum(.data[["valid_denominator_prop_low"]], na.rm = TRUE),
      n_target_exceeds_denominator = sum(.data[["target_exceeds_denominator"]], na.rm = TRUE),
      n_target_zero = sum(.data[["target_zero"]], na.rm = TRUE),
      n_target_all = sum(.data[["target_all"]], na.rm = TRUE),
      denominator_min = suppressWarnings(min(.data[[".gp3_denominator"]], na.rm = TRUE)),
      denominator_median = suppressWarnings(stats::median(.data[[".gp3_denominator"]], na.rm = TRUE)),
      denominator_max = suppressWarnings(max(.data[[".gp3_denominator"]], na.rm = TRUE)),
      valid_denominator_prop_min = suppressWarnings(min(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE)),
      valid_denominator_prop_median = suppressWarnings(stats::median(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE)),
      valid_denominator_prop_max = suppressWarnings(max(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      denominator_audit_status = dplyr::case_when(
        .data[["n_target_exceeds_denominator"]] > 0 ~ "invalid_counts",
        .data[["n_missing_denominator"]] > 0 ~ "missing_denominators",
        .data[["n_zero_denominator"]] > 0 ~ "zero_denominators",
        .data[["n_low_denominator"]] > 0 |
          .data[["n_low_valid_denominator_prop"]] > 0 ~ "review_denominators",
        TRUE ~ "ok"
      )
    )

  window_summary <- row_audit |>
    dplyr::group_by(
      .data[[".gp3_window"]],
      .data[[".gp3_window_start"]],
      .data[[".gp3_window_end"]]
    ) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      denominator_min = suppressWarnings(min(.data[[".gp3_denominator"]], na.rm = TRUE)),
      denominator_mean = mean(.data[[".gp3_denominator"]], na.rm = TRUE),
      denominator_median = stats::median(.data[[".gp3_denominator"]], na.rm = TRUE),
      denominator_max = suppressWarnings(max(.data[[".gp3_denominator"]], na.rm = TRUE)),
      denominator_sd = stats::sd(.data[[".gp3_denominator"]], na.rm = TRUE),
      n_zero_denominator = sum(.data[["denominator_zero"]], na.rm = TRUE),
      n_low_denominator = sum(.data[["denominator_low"]], na.rm = TRUE),
      n_low_valid_denominator_prop = sum(.data[["valid_denominator_prop_low"]], na.rm = TRUE),
      n_target_zero = sum(.data[["target_zero"]], na.rm = TRUE),
      n_target_all = sum(.data[["target_all"]], na.rm = TRUE),
      valid_denominator_prop_min = suppressWarnings(min(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE)),
      valid_denominator_prop_mean = mean(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      denominator_cv = dplyr::if_else(
        is.finite(.data[["denominator_mean"]]) &
          .data[["denominator_mean"]] > 0,
        .data[["denominator_sd"]] / .data[["denominator_mean"]],
        NA_real_
      ),
      window_denominator_status = dplyr::case_when(
        .data[["n_zero_denominator"]] > 0 ~ "zero_denominator",
        .data[["n_low_denominator"]] > 0 ~ "low_denominator",
        .data[["n_low_valid_denominator_prop"]] > 0 ~ "low_valid_denominator_prop",
        is.finite(.data[["denominator_cv"]]) &
          .data[["denominator_cv"]] > max_denominator_cv ~ "high_denominator_variability",
        TRUE ~ "ok"
      )
    ) |>
    dplyr::rename_with(
      ~ c("window_label", "window_start_ms", "window_end_ms"),
      dplyr::all_of(c(".gp3_window", ".gp3_window_start", ".gp3_window_end"))
    ) |>
    dplyr::arrange(.data[["window_start_ms"]], .data[["window_label"]])

  condition_window_summary <- row_audit |>
    dplyr::group_by(
      .data[[".gp3_condition"]],
      .data[[".gp3_window"]],
      .data[[".gp3_window_start"]],
      .data[[".gp3_window_end"]]
    ) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      denominator_mean = mean(.data[[".gp3_denominator"]], na.rm = TRUE),
      denominator_median = stats::median(.data[[".gp3_denominator"]], na.rm = TRUE),
      denominator_min = suppressWarnings(min(.data[[".gp3_denominator"]], na.rm = TRUE)),
      denominator_max = suppressWarnings(max(.data[[".gp3_denominator"]], na.rm = TRUE)),
      valid_denominator_prop_mean = mean(.data[[".gp3_valid_denominator_prop"]], na.rm = TRUE),
      n_zero_denominator = sum(.data[["denominator_zero"]], na.rm = TRUE),
      n_low_denominator = sum(.data[["denominator_low"]], na.rm = TRUE),
      n_target_zero = sum(.data[["target_zero"]], na.rm = TRUE),
      n_target_all = sum(.data[["target_all"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename_with(
      ~ c("condition", "window_label", "window_start_ms", "window_end_ms"),
      dplyr::all_of(c(
        ".gp3_condition",
        ".gp3_window",
        ".gp3_window_start",
        ".gp3_window_end"
      ))
    ) |>
    dplyr::arrange(.data[["window_start_ms"]], .data[["condition"]])

  denominator_imbalance <- condition_window_summary |>
    dplyr::group_by(
      .data[["window_label"]],
      .data[["window_start_ms"]],
      .data[["window_end_ms"]]
    ) |>
    dplyr::summarise(
      n_conditions = dplyr::n_distinct(.data[["condition"]]),
      denominator_mean_min = suppressWarnings(min(.data[["denominator_mean"]], na.rm = TRUE)),
      denominator_mean_max = suppressWarnings(max(.data[["denominator_mean"]], na.rm = TRUE)),
      denominator_mean_sd = stats::sd(.data[["denominator_mean"]], na.rm = TRUE),
      denominator_mean_grand = mean(.data[["denominator_mean"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      denominator_condition_ratio = dplyr::if_else(
        is.finite(.data[["denominator_mean_min"]]) &
          .data[["denominator_mean_min"]] > 0,
        .data[["denominator_mean_max"]] / .data[["denominator_mean_min"]],
        NA_real_
      ),
      denominator_condition_cv = dplyr::if_else(
        is.finite(.data[["denominator_mean_grand"]]) &
          .data[["denominator_mean_grand"]] > 0,
        .data[["denominator_mean_sd"]] / .data[["denominator_mean_grand"]],
        NA_real_
      ),
      denominator_imbalance_status = dplyr::case_when(
        .data[["n_conditions"]] < 2 ~ "single_condition",
        is.finite(.data[["denominator_condition_ratio"]]) &
          .data[["denominator_condition_ratio"]] > max_condition_ratio ~ "condition_denominator_ratio_high",
        is.finite(.data[["denominator_condition_cv"]]) &
          .data[["denominator_condition_cv"]] > max_denominator_cv ~ "condition_denominator_cv_high",
        TRUE ~ "ok"
      )
    ) |>
    dplyr::arrange(.data[["window_start_ms"]], .data[["window_label"]])

  flagged_rows <- row_audit |>
    dplyr::filter(.data[["denominator_audit_status"]] != "ok")

  output <- list(
    overview = overview,
    row_audit = row_audit,
    window_summary = window_summary,
    condition_window_summary = condition_window_summary,
    denominator_imbalance = denominator_imbalance,
    flagged_rows = flagged_rows,
    settings = list(
      window_col = window_col,
      window_start_col = window_start_col,
      window_end_col = window_end_col,
      denominator_col = denominator_col,
      total_col = total_col,
      target_col = target_col,
      condition_col = condition_col,
      group_cols = group_cols,
      min_denominator_samples = min_denominator_samples,
      min_valid_denominator_prop = min_valid_denominator_prop,
      max_denominator_cv = max_denominator_cv,
      max_condition_ratio = max_condition_ratio
    )
  )

  class(output) <- c("gp3_aoi_window_denominator_audit", "list")

  output
}
