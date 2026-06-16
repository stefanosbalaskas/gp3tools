#' Run a Gazepoint AOI preprocessing multiverse
#'
#' Run all AOI preprocessing branches defined by
#' `create_gazepoint_preprocessing_multiverse()`. Each branch creates AOI-window
#' summaries and then prepares binomial AOI GLMM data using the branch-specific
#' denominator and minimum denominator threshold.
#'
#' @param data A Gazepoint master table or sample-level AOI table.
#' @param multiverse A `gp3_preprocessing_multiverse` object returned by
#'   `create_gazepoint_preprocessing_multiverse()`.
#' @param branch_ids Optional character vector of AOI branch IDs to run.
#' @param windows Numeric vector or labelled window table passed to
#'   `summarise_gazepoint_aoi_windows()`.
#' @param time_col Time column.
#' @param aoi_col AOI-state column.
#' @param subject_col Subject column.
#' @param condition_col Optional condition column.
#' @param group_cols Optional grouping columns for AOI-window summaries.
#' @param target_aoi_values Target AOI values.
#' @param distractor_aoi_values Optional distractor AOI values.
#' @param success_col Success-count column passed to
#'   `prepare_gazepoint_aoi_glmm_data()`.
#' @param outcome_label Outcome label passed to AOI helpers.
#' @param keep_outputs Logical. If `TRUE`, keep branch outputs in
#'   `branch_outputs`.
#' @param stop_on_error Logical. If `TRUE`, stop when a branch fails. If
#'   `FALSE`, record the branch error and continue.
#'
#' @return A list with class `gp3_aoi_multiverse_results` containing overview,
#'   branch results, optional branch outputs, and settings.
#' @export
run_gazepoint_aoi_multiverse <- function(
    data,
    multiverse,
    branch_ids = NULL,
    windows,
    time_col = "time",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = NULL,
    group_cols = NULL,
    target_aoi_values,
    distractor_aoi_values = NULL,
    success_col = "n_target_samples",
    outcome_label = "target",
    keep_outputs = TRUE,
    stop_on_error = FALSE
) {
  if (missing(data) || is.null(data)) {
    stop("`data` must be supplied.", call. = FALSE)
  }

  if (!inherits(multiverse, "gp3_preprocessing_multiverse")) {
    stop(
      "`multiverse` must be created by `create_gazepoint_preprocessing_multiverse()`.",
      call. = FALSE
    )
  }

  if (missing(windows) || is.null(windows)) {
    stop("`windows` must be supplied.", call. = FALSE)
  }

  if (missing(target_aoi_values) || is.null(target_aoi_values)) {
    stop("`target_aoi_values` must be supplied.", call. = FALSE)
  }

  .gp3_multiverse_check_logical_scalar(keep_outputs, "keep_outputs")
  .gp3_multiverse_check_logical_scalar(stop_on_error, "stop_on_error")

  data <- .gp3_multiverse_standardise_aoi_runner_columns(data)
  group_cols <- .gp3_multiverse_standardise_group_cols(group_cols)

  if (!is.null(condition_col)) {
    condition_col <- .gp3_multiverse_standardise_single_col(condition_col)
  }

  aoi_grid <- multiverse$aoi_grid

  if (!is.data.frame(aoi_grid) || nrow(aoi_grid) == 0L) {
    stop("`multiverse` does not contain AOI branches.", call. = FALSE)
  }

  if (!is.null(branch_ids)) {
    .gp3_multiverse_check_character_vector(branch_ids, "branch_ids")


    missing_branches <- setdiff(branch_ids, aoi_grid$branch_id)

    if (length(missing_branches) > 0L) {
      stop(
        "`branch_ids` contains unknown AOI branch ID(s): ",
        paste(missing_branches, collapse = ", "),
        call. = FALSE
      )
    }

    aoi_grid <- aoi_grid[aoi_grid$branch_id %in% branch_ids, , drop = FALSE]


  }

  branch_outputs <- list()
  branch_rows <- vector("list", nrow(aoi_grid))

  for (i in seq_len(nrow(aoi_grid))) {
    branch <- aoi_grid[i, , drop = FALSE]


    branch_run <- .gp3_run_single_aoi_multiverse_branch(
      data = data,
      branch = branch,
      windows = windows,
      time_col = time_col,
      aoi_col = aoi_col,
      subject_col = subject_col,
      condition_col = condition_col,
      group_cols = group_cols,
      target_aoi_values = target_aoi_values,
      distractor_aoi_values = distractor_aoi_values,
      success_col = success_col,
      outcome_label = outcome_label,
      stop_on_error = stop_on_error
    )

    branch_rows[[i]] <- branch_run$result_row

    if (isTRUE(keep_outputs) && !is.null(branch_run$output)) {
      branch_outputs[[branch$branch_id[[1]]]] <- branch_run$output
    }


  }

  branch_results <- dplyr::bind_rows(branch_rows)

  overview <- tibble::tibble(
    multiverse_family = "aoi",
    n_defined_branches = nrow(multiverse$aoi_grid),
    n_requested_branches = nrow(aoi_grid),
    n_completed_branches = sum(branch_results$branch_status == "completed"),
    n_failed_branches = sum(branch_results$branch_status == "failed"),
    n_skipped_branches = sum(branch_results$branch_status == "skipped"),
    multiverse_status = dplyr::case_when(
      nrow(branch_results) == 0L ~ "not_run",
      all(branch_results$branch_status == "completed") ~ "completed",
      any(branch_results$branch_status == "failed") ~ "completed_with_errors",
      TRUE ~ "completed_with_cautions"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "branch_ids",
      "windows",
      "time_col",
      "aoi_col",
      "subject_col",
      "condition_col",
      "group_cols",
      "target_aoi_values",
      "distractor_aoi_values",
      "success_col",
      "outcome_label",
      "keep_outputs",
      "stop_on_error"
    ),
    value = c(
      .gp3_multiverse_collapse_nullable(branch_ids),
      .gp3_multiverse_collapse_nullable(windows),
      .gp3_multiverse_collapse_nullable(time_col),
      .gp3_multiverse_collapse_nullable(aoi_col),
      .gp3_multiverse_collapse_nullable(subject_col),
      .gp3_multiverse_collapse_nullable(condition_col),
      .gp3_multiverse_collapse_nullable(group_cols),
      .gp3_multiverse_collapse_nullable(target_aoi_values),
      .gp3_multiverse_collapse_nullable(distractor_aoi_values),
      .gp3_multiverse_collapse_nullable(success_col),
      .gp3_multiverse_collapse_nullable(outcome_label),
      as.character(keep_outputs),
      as.character(stop_on_error)
    )
  )

  out <- list(
    overview = overview,
    branch_results = branch_results,
    branch_outputs = branch_outputs,
    settings = settings
  )

  class(out) <- c("gp3_aoi_multiverse_results", "list")

  out
}

.gp3_run_single_aoi_multiverse_branch <- function(
    data,
    branch,
    windows,
    time_col,
    aoi_col,
    subject_col,
    condition_col,
    group_cols,
    target_aoi_values,
    distractor_aoi_values,
    success_col,
    outcome_label,
    stop_on_error
) {
  branch_id <- branch$branch_id[[1]]
  branch_label <- branch$branch_label[[1]]

  branch_denominator <- branch$denominator[[1]]
  model_denominator <- .gp3_multiverse_aoi_model_denominator(branch_denominator)

  result <- tryCatch(
    {
      aoi_windows <- .gp3_multiverse_call_required(
        fun_name = "summarise_gazepoint_aoi_windows",
        data = data,
        args = list(
          windows = windows,
          time_col = time_col,
          aoi_col = aoi_col,
          subject_col = subject_col,
          condition_col = condition_col,
          group_cols = group_cols,
          target_aoi_values = target_aoi_values,
          distractor_aoi_values = distractor_aoi_values,
          denominator = branch_denominator,
          outcome_label = outcome_label
        )
      )


      aoi_glmm_data <- .gp3_multiverse_call_required(
        fun_name = "prepare_gazepoint_aoi_glmm_data",
        data = aoi_windows,
        args = list(
          success_col = success_col,
          denominator = model_denominator,
          subject_col = subject_col,
          condition_col = condition_col,
          window_col = "window_label",
          window_start_col = "window_start_ms",
          window_end_col = "window_end_ms",
          min_denominator_samples = branch$min_denominator_samples[[1]],
          outcome_label = outcome_label
        )
      )

      output <- list(
        aoi_windows = aoi_windows,
        aoi_glmm_data = aoi_glmm_data
      )

      class(output) <- c("gp3_aoi_multiverse_branch_output", "list")

      list(
        output = output,
        result_row = tibble::tibble(
          branch_id = branch_id,
          branch_label = branch_label,
          preprocessing_family = "aoi",
          denominator = branch_denominator,
          min_denominator_samples = branch$min_denominator_samples[[1]],
          branch_status = "completed",
          aoi_window_rows = .gp3_multiverse_nrow_or_na(aoi_windows),
          aoi_window_cols = .gp3_multiverse_ncol_or_na(aoi_windows),
          aoi_glmm_rows = .gp3_multiverse_nrow_or_na(aoi_glmm_data),
          aoi_glmm_cols = .gp3_multiverse_ncol_or_na(aoi_glmm_data),
          message = NA_character_
        )
      )
    },
    error = function(e) {
      if (isTRUE(stop_on_error)) {
        stop(e)
      }

      list(
        output = NULL,
        result_row = tibble::tibble(
          branch_id = branch_id,
          branch_label = branch_label,
          preprocessing_family = "aoi",
          denominator = branch$denominator[[1]],
          min_denominator_samples = branch$min_denominator_samples[[1]],
          branch_status = "failed",
          aoi_window_rows = NA_integer_,
          aoi_window_cols = NA_integer_,
          aoi_glmm_rows = NA_integer_,
          aoi_glmm_cols = NA_integer_,
          message = conditionMessage(e)
        )
      )
    }


  )

  result
}

.gp3_multiverse_standardise_aoi_runner_columns <- function(data) {
  if (!is.data.frame(data)) {
    return(data)
  }

  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_multiverse_standardise_single_col <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- as.character(x)

  x[x == "MEDIA_ID"] <- "media_id"
  x[x == "USER_FILE"] <- "subject"

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}

.gp3_multiverse_aoi_model_denominator <- function(x) {
  if (identical(x, "aoi_only")) {
    return("aoi")
  }

  x
}
