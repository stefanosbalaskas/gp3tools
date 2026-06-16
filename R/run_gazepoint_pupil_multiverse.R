#' Run a Gazepoint pupil preprocessing multiverse
#'
#' Run all pupil preprocessing branches defined by
#' `create_gazepoint_preprocessing_multiverse()`. Each branch can apply pupil
#' artifact flagging, interpolation, baseline correction, smoothing, and
#' optional pupil-window summarisation.
#'
#' @param data A Gazepoint master table or processed pupil table.
#' @param multiverse A `gp3_preprocessing_multiverse` object returned by
#'   `create_gazepoint_preprocessing_multiverse()`.
#' @param branch_ids Optional character vector of pupil branch IDs to run.
#' @param pupil_col Optional pupil column passed to downstream preprocessing
#'   helpers when supported.
#' @param time_col Optional time column passed to downstream preprocessing
#'   helpers when supported.
#' @param group_cols Optional grouping columns passed to downstream
#'   preprocessing helpers when supported.
#' @param summarise_windows Logical. If `TRUE`, summarise each processed pupil
#'   branch into pupil analysis windows.
#' @param windows Optional windows passed to `summarise_gazepoint_pupil_windows()`
#'   when `summarise_windows = TRUE`.
#' @param keep_outputs Logical. If `TRUE`, keep processed branch data in
#'   `branch_outputs`.
#' @param stop_on_error Logical. If `TRUE`, stop when a branch fails. If
#'   `FALSE`, record the branch error and continue.
#'
#' @return A list with class `gp3_pupil_multiverse_results` containing overview,
#'   branch results, optional branch outputs, and settings.
#' @export
run_gazepoint_pupil_multiverse <- function(
    data,
    multiverse,
    branch_ids = NULL,
    pupil_col = NULL,
    time_col = NULL,
    group_cols = NULL,
    summarise_windows = FALSE,
    windows = NULL,
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

  .gp3_multiverse_check_logical_scalar(
    summarise_windows,
    "summarise_windows"
  )
  .gp3_multiverse_check_logical_scalar(keep_outputs, "keep_outputs")
  .gp3_multiverse_check_logical_scalar(stop_on_error, "stop_on_error")

  data <- .gp3_multiverse_standardise_pupil_runner_columns(data)
  group_cols <- .gp3_multiverse_standardise_group_cols(group_cols)

  pupil_grid <- multiverse$pupil_grid

  if (!is.data.frame(pupil_grid) || nrow(pupil_grid) == 0L) {
    stop("`multiverse` does not contain pupil branches.", call. = FALSE)
  }

  if (!is.null(branch_ids)) {
    .gp3_multiverse_check_character_vector(branch_ids, "branch_ids")


    missing_branches <- setdiff(branch_ids, pupil_grid$branch_id)

    if (length(missing_branches) > 0L) {
      stop(
        "`branch_ids` contains unknown pupil branch ID(s): ",
        paste(missing_branches, collapse = ", "),
        call. = FALSE
      )
    }

    pupil_grid <- pupil_grid[pupil_grid$branch_id %in% branch_ids, , drop = FALSE]


  }

  branch_outputs <- list()
  branch_rows <- vector("list", nrow(pupil_grid))

  for (i in seq_len(nrow(pupil_grid))) {
    branch <- pupil_grid[i, , drop = FALSE]


    branch_run <- .gp3_run_single_pupil_multiverse_branch(
      data = data,
      branch = branch,
      pupil_col = pupil_col,
      time_col = time_col,
      group_cols = group_cols,
      summarise_windows = summarise_windows,
      windows = windows,
      stop_on_error = stop_on_error
    )

    branch_rows[[i]] <- branch_run$result_row

    if (isTRUE(keep_outputs)) {
      branch_outputs[[branch$branch_id[[1]]]] <- branch_run$output
    }


  }

  branch_results <- dplyr::bind_rows(branch_rows)

  overview <- tibble::tibble(
    multiverse_family = "pupil",
    n_defined_branches = nrow(multiverse$pupil_grid),
    n_requested_branches = nrow(pupil_grid),
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
      "pupil_col",
      "time_col",
      "group_cols",
      "summarise_windows",
      "windows",
      "keep_outputs",
      "stop_on_error"
    ),
    value = c(
      .gp3_multiverse_collapse_nullable(branch_ids),
      .gp3_multiverse_collapse_nullable(pupil_col),
      .gp3_multiverse_collapse_nullable(time_col),
      .gp3_multiverse_collapse_nullable(group_cols),
      as.character(summarise_windows),
      .gp3_multiverse_collapse_nullable(windows),
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

  class(out) <- c("gp3_pupil_multiverse_results", "list")

  out
}

.gp3_run_single_pupil_multiverse_branch <- function(
    data,
    branch,
    pupil_col,
    time_col,
    group_cols,
    summarise_windows,
    windows,
    stop_on_error
) {
  branch_id <- branch$branch_id[[1]]
  branch_label <- branch$branch_label[[1]]

  result <- tryCatch(
    {
      processed <- data


      processed <- .gp3_multiverse_call_if_available(
        fun_name = "flag_gazepoint_pupil_artifacts",
        data = processed,
        args = list(
          artifact_padding_ms = branch$artifact_padding_ms[[1]],
          padding_ms = branch$artifact_padding_ms[[1]],
          pupil_col = pupil_col,
          time_col = time_col,
          group_cols = group_cols
        )
      )

      processed <- .gp3_multiverse_call_if_available(
        fun_name = "interpolate_gazepoint_pupil",
        data = processed,
        args = list(
          max_gap_ms = branch$max_gap_ms[[1]],
          max_gap_duration_ms = branch$max_gap_ms[[1]],
          pupil_col = pupil_col,
          time_col = time_col,
          group_cols = group_cols
        )
      )

      processed <- .gp3_multiverse_call_if_available(
        fun_name = "baseline_correct_gazepoint_pupil",
        data = processed,
        args = list(
          baseline_window = c(
            branch$baseline_window_start_ms[[1]],
            branch$baseline_window_end_ms[[1]]
          ),
          pupil_col = pupil_col,
          time_col = time_col,
          group_cols = group_cols
        )
      )

      processed <- .gp3_multiverse_call_if_available(
        fun_name = "smooth_gazepoint_pupil",
        data = processed,
        args = list(
          window_samples = branch$smoothing_window_samples[[1]],
          smoothing_window_samples = branch$smoothing_window_samples[[1]],
          pupil_col = pupil_col,
          time_col = time_col,
          group_cols = group_cols
        )
      )

      if (isTRUE(summarise_windows)) {
        if (is.null(windows)) {
          stop(
            "`windows` must be supplied when `summarise_windows = TRUE`.",
            call. = FALSE
          )
        }

        processed <- .gp3_multiverse_call_required(
          fun_name = "summarise_gazepoint_pupil_windows",
          data = processed,
          args = list(
            windows = windows,
            pupil_col = pupil_col,
            time_col = time_col,
            group_cols = group_cols
          )
        )
      }

      list(
        output = processed,
        result_row = tibble::tibble(
          branch_id = branch_id,
          branch_label = branch_label,
          preprocessing_family = "pupil",
          artifact_padding_ms = branch$artifact_padding_ms[[1]],
          max_gap_ms = branch$max_gap_ms[[1]],
          smoothing_window_samples = branch$smoothing_window_samples[[1]],
          baseline_window_start_ms = branch$baseline_window_start_ms[[1]],
          baseline_window_end_ms = branch$baseline_window_end_ms[[1]],
          baseline_window_label = branch$baseline_window_label[[1]],
          branch_status = "completed",
          output_class = paste(class(processed), collapse = ", "),
          output_rows = .gp3_multiverse_nrow_or_na(processed),
          output_cols = .gp3_multiverse_ncol_or_na(processed),
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
          preprocessing_family = "pupil",
          artifact_padding_ms = branch$artifact_padding_ms[[1]],
          max_gap_ms = branch$max_gap_ms[[1]],
          smoothing_window_samples = branch$smoothing_window_samples[[1]],
          baseline_window_start_ms = branch$baseline_window_start_ms[[1]],
          baseline_window_end_ms = branch$baseline_window_end_ms[[1]],
          baseline_window_label = branch$baseline_window_label[[1]],
          branch_status = "failed",
          output_class = NA_character_,
          output_rows = NA_integer_,
          output_cols = NA_integer_,
          message = conditionMessage(e)
        )
      )
    }


  )

  result
}

.gp3_multiverse_call_if_available <- function(fun_name, data, args = list()) {
  if (!exists(fun_name, mode = "function")) {
    return(data)
  }

  .gp3_multiverse_call_required(
    fun_name = fun_name,
    data = data,
    args = args
  )
}

.gp3_multiverse_call_required <- function(fun_name, data, args = list()) {
  fun <- get(fun_name, mode = "function")

  formal_names <- names(formals(fun))

  args <- args[!vapply(args, is.null, logical(1))]

  if (!"..." %in% formal_names) {
    args <- args[names(args) %in% formal_names]
  }

  data_arg <- .gp3_multiverse_first_data_arg(formal_names)

  if (is.na(data_arg)) {
    stop(
      "Could not identify the data argument for `",
      fun_name,
      "`.",
      call. = FALSE
    )
  }

  args <- c(stats::setNames(list(data), data_arg), args)

  do.call(fun, args)
}

.gp3_multiverse_first_data_arg <- function(formal_names) {
  candidates <- c("data", "master", "pupil_data", "x")

  found <- candidates[candidates %in% formal_names]

  if (length(found) == 0L) {
    return(NA_character_)
  }

  found[[1]]
}

.gp3_multiverse_nrow_or_na <- function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  }

  NA_integer_
}

.gp3_multiverse_ncol_or_na <- function(x) {
  if (is.data.frame(x)) {
    return(ncol(x))
  }

  NA_integer_
}

.gp3_multiverse_collapse_nullable <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  if (is.list(x) && !is.data.frame(x)) {
    return(paste(utils::capture.output(utils::str(x, give.attr = FALSE)), collapse = " "))
  }

  paste(as.character(x), collapse = ", ")
}

.gp3_multiverse_standardise_pupil_runner_columns <- function(data) {
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

.gp3_multiverse_standardise_group_cols <- function(group_cols) {
  if (is.null(group_cols)) {
    return(NULL)
  }

  group_cols <- as.character(group_cols)

  group_cols[group_cols == "MEDIA_ID"] <- "media_id"
  group_cols[group_cols == "USER_FILE"] <- "subject"

  group_cols
}
