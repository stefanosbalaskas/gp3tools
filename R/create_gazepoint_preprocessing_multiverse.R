#' Create a Gazepoint preprocessing multiverse
#'
#' Create a preprocessing multiverse specification for Gazepoint pupil and AOI
#' workflows. The returned object defines alternative preprocessing decisions
#' that can later be passed to pupil or AOI multiverse runners.
#'
#' @param pupil_max_gap_ms Numeric vector of maximum pupil-interpolation gap
#'   durations in milliseconds.
#' @param pupil_smoothing_window_samples Integer vector of pupil smoothing
#'   window sizes in samples.
#' @param pupil_baseline_windows List of numeric vectors of length 2 defining
#'   pupil baseline windows in milliseconds.
#' @param pupil_artifact_padding_ms Numeric vector of artifact-padding values in
#'   milliseconds.
#' @param aoi_denominators Character vector of AOI denominator definitions.
#'   Typical values are `"valid"`, `"all"`, and `"aoi_only"`.
#' @param aoi_min_denominator_samples Integer vector of minimum denominator
#'   sample thresholds for AOI-window modelling.
#' @param include_pupil Logical. If `TRUE`, create pupil preprocessing branches.
#' @param include_aoi Logical. If `TRUE`, create AOI preprocessing branches.
#' @param label_prefix Character prefix used for branch identifiers.
#'
#' @return A list with class `gp3_preprocessing_multiverse` containing overview,
#'   pupil grid, AOI grid, combined grid, and settings tables.
#' @export
create_gazepoint_preprocessing_multiverse <- function(
    pupil_max_gap_ms = c(75, 150, 250),
    pupil_smoothing_window_samples = c(3L, 5L, 7L),
    pupil_baseline_windows = list(c(0, 200), c(-200, 0)),
    pupil_artifact_padding_ms = c(0, 50),
    aoi_denominators = c("valid", "all", "aoi_only"),
    aoi_min_denominator_samples = c(1L, 5L, 10L),
    include_pupil = TRUE,
    include_aoi = TRUE,
    label_prefix = "mv"
) {
  .gp3_multiverse_check_logical_scalar(include_pupil, "include_pupil")
  .gp3_multiverse_check_logical_scalar(include_aoi, "include_aoi")
  .gp3_multiverse_check_character_scalar(label_prefix, "label_prefix")

  if (!isTRUE(include_pupil) && !isTRUE(include_aoi)) {
    stop(
      "At least one of `include_pupil` or `include_aoi` must be TRUE.",
      call. = FALSE
    )
  }

  label_prefix <- .gp3_multiverse_sanitise_label(label_prefix)

  pupil_grid <- .gp3_create_empty_pupil_multiverse_grid()
  aoi_grid <- .gp3_create_empty_aoi_multiverse_grid()

  if (isTRUE(include_pupil)) {
    pupil_grid <- .gp3_create_pupil_multiverse_grid(
      pupil_max_gap_ms = pupil_max_gap_ms,
      pupil_smoothing_window_samples = pupil_smoothing_window_samples,
      pupil_baseline_windows = pupil_baseline_windows,
      pupil_artifact_padding_ms = pupil_artifact_padding_ms,
      label_prefix = label_prefix
    )
  }

  if (isTRUE(include_aoi)) {
    aoi_grid <- .gp3_create_aoi_multiverse_grid(
      aoi_denominators = aoi_denominators,
      aoi_min_denominator_samples = aoi_min_denominator_samples,
      label_prefix = label_prefix
    )
  }

  combined_grid <- .gp3_create_combined_multiverse_grid(
    pupil_grid = pupil_grid,
    aoi_grid = aoi_grid,
    label_prefix = label_prefix
  )

  overview <- tibble::tibble(
    include_pupil = include_pupil,
    include_aoi = include_aoi,
    n_pupil_branches = nrow(pupil_grid),
    n_aoi_branches = nrow(aoi_grid),
    n_combined_branches = nrow(combined_grid),
    multiverse_status = "defined"
  )

  settings <- tibble::tibble(
    setting = c(
      "pupil_max_gap_ms",
      "pupil_smoothing_window_samples",
      "pupil_baseline_windows",
      "pupil_artifact_padding_ms",
      "aoi_denominators",
      "aoi_min_denominator_samples",
      "include_pupil",
      "include_aoi",
      "label_prefix"
    ),
    value = c(
      .gp3_multiverse_collapse_value(pupil_max_gap_ms),
      .gp3_multiverse_collapse_value(pupil_smoothing_window_samples),
      .gp3_multiverse_collapse_baseline_windows(pupil_baseline_windows),
      .gp3_multiverse_collapse_value(pupil_artifact_padding_ms),
      .gp3_multiverse_collapse_value(aoi_denominators),
      .gp3_multiverse_collapse_value(aoi_min_denominator_samples),
      as.character(include_pupil),
      as.character(include_aoi),
      label_prefix
    )
  )

  out <- list(
    overview = overview,
    pupil_grid = pupil_grid,
    aoi_grid = aoi_grid,
    combined_grid = combined_grid,
    settings = settings
  )

  class(out) <- c("gp3_preprocessing_multiverse", "list")

  out
}

.gp3_create_pupil_multiverse_grid <- function(
    pupil_max_gap_ms,
    pupil_smoothing_window_samples,
    pupil_baseline_windows,
    pupil_artifact_padding_ms,
    label_prefix
) {
  .gp3_multiverse_check_numeric_vector(
    pupil_max_gap_ms,
    "pupil_max_gap_ms",
    allow_zero = FALSE
  )

  .gp3_multiverse_check_numeric_vector(
    pupil_smoothing_window_samples,
    "pupil_smoothing_window_samples",
    allow_zero = FALSE
  )

  .gp3_multiverse_check_numeric_vector(
    pupil_artifact_padding_ms,
    "pupil_artifact_padding_ms",
    allow_zero = TRUE
  )

  pupil_baseline_windows <- .gp3_multiverse_normalise_baseline_windows(
    pupil_baseline_windows
  )

  rows <- list()
  branch_i <- 0L

  for (padding_ms in pupil_artifact_padding_ms) {
    for (max_gap_ms in pupil_max_gap_ms) {
      for (smooth_n in pupil_smoothing_window_samples) {
        for (baseline_window in pupil_baseline_windows) {
          branch_i <- branch_i + 1L

          baseline_label <- paste0(
            baseline_window[[1]],
            "_to_",
            baseline_window[[2]],
            "ms"
          )

          rows[[branch_i]] <- tibble::tibble(
            branch_id = paste0(label_prefix, "_pupil_", branch_i),
            branch_label = paste0(
              "pupil_gap",
              max_gap_ms,
              "_smooth",
              smooth_n,
              "_baseline",
              baseline_label,
              "_pad",
              padding_ms
            ),
            preprocessing_family = "pupil",
            decision_type = "sensitivity",
            artifact_padding_ms = as.numeric(padding_ms),
            max_gap_ms = as.numeric(max_gap_ms),
            smoothing_window_samples = as.integer(smooth_n),
            baseline_window_start_ms = as.numeric(baseline_window[[1]]),
            baseline_window_end_ms = as.numeric(baseline_window[[2]]),
            baseline_window_label = baseline_label,
            branch_status = "defined"
          )
        }
      }
    }
  }

  dplyr::bind_rows(rows)
}

.gp3_create_aoi_multiverse_grid <- function(
    aoi_denominators,
    aoi_min_denominator_samples,
    label_prefix
) {
  .gp3_multiverse_check_character_vector(
    aoi_denominators,
    "aoi_denominators"
  )

  .gp3_multiverse_check_numeric_vector(
    aoi_min_denominator_samples,
    "aoi_min_denominator_samples",
    allow_zero = FALSE
  )

  rows <- list()
  branch_i <- 0L

  for (denominator in aoi_denominators) {
    for (min_n in aoi_min_denominator_samples) {
      branch_i <- branch_i + 1L

      rows[[branch_i]] <- tibble::tibble(
        branch_id = paste0(label_prefix, "_aoi_", branch_i),
        branch_label = paste0(
          "aoi_denominator_",
          denominator,
          "_min",
          min_n
        ),
        preprocessing_family = "aoi",
        decision_type = "sensitivity",
        denominator = denominator,
        min_denominator_samples = as.integer(min_n),
        branch_status = "defined"
      )
    }
  }

  dplyr::bind_rows(rows)
}

.gp3_create_combined_multiverse_grid <- function(
    pupil_grid,
    aoi_grid,
    label_prefix
) {
  if (nrow(pupil_grid) == 0L && nrow(aoi_grid) == 0L) {
    return(.gp3_create_empty_combined_multiverse_grid())
  }

  if (nrow(pupil_grid) > 0L && nrow(aoi_grid) == 0L) {
    return(tibble::tibble(
      combined_branch_id = paste0(label_prefix, "_combined_", seq_len(nrow(pupil_grid))),
      pupil_branch_id = pupil_grid$branch_id,
      pupil_branch_label = pupil_grid$branch_label,
      aoi_branch_id = NA_character_,
      aoi_branch_label = NA_character_,
      branch_status = "defined"
    ))
  }

  if (nrow(pupil_grid) == 0L && nrow(aoi_grid) > 0L) {
    return(tibble::tibble(
      combined_branch_id = paste0(label_prefix, "_combined_", seq_len(nrow(aoi_grid))),
      pupil_branch_id = NA_character_,
      pupil_branch_label = NA_character_,
      aoi_branch_id = aoi_grid$branch_id,
      aoi_branch_label = aoi_grid$branch_label,
      branch_status = "defined"
    ))
  }

  rows <- list()
  branch_i <- 0L

  for (p_i in seq_len(nrow(pupil_grid))) {
    for (a_i in seq_len(nrow(aoi_grid))) {
      branch_i <- branch_i + 1L

      rows[[branch_i]] <- tibble::tibble(
        combined_branch_id = paste0(label_prefix, "_combined_", branch_i),
        pupil_branch_id = pupil_grid$branch_id[[p_i]],
        pupil_branch_label = pupil_grid$branch_label[[p_i]],
        aoi_branch_id = aoi_grid$branch_id[[a_i]],
        aoi_branch_label = aoi_grid$branch_label[[a_i]],
        branch_status = "defined"
      )
    }
  }

  dplyr::bind_rows(rows)
}

.gp3_create_empty_pupil_multiverse_grid <- function() {
  tibble::tibble(
    branch_id = character(),
    branch_label = character(),
    preprocessing_family = character(),
    decision_type = character(),
    artifact_padding_ms = numeric(),
    max_gap_ms = numeric(),
    smoothing_window_samples = integer(),
    baseline_window_start_ms = numeric(),
    baseline_window_end_ms = numeric(),
    baseline_window_label = character(),
    branch_status = character()
  )
}

.gp3_create_empty_aoi_multiverse_grid <- function() {
  tibble::tibble(
    branch_id = character(),
    branch_label = character(),
    preprocessing_family = character(),
    decision_type = character(),
    denominator = character(),
    min_denominator_samples = integer(),
    branch_status = character()
  )
}

.gp3_create_empty_combined_multiverse_grid <- function() {
  tibble::tibble(
    combined_branch_id = character(),
    pupil_branch_id = character(),
    pupil_branch_label = character(),
    aoi_branch_id = character(),
    aoi_branch_label = character(),
    branch_status = character()
  )
}

.gp3_multiverse_normalise_baseline_windows <- function(x) {
  if (is.numeric(x) && length(x) == 2L) {
    x <- list(x)
  }

  if (!is.list(x) || length(x) == 0L) {
    stop(
      "`pupil_baseline_windows` must be a non-empty list of numeric vectors of length 2.",
      call. = FALSE
    )
  }

  for (i in seq_along(x)) {
    if (!is.numeric(x[[i]]) ||
        length(x[[i]]) != 2L ||
        any(is.na(x[[i]]))) {
      stop(
        "`pupil_baseline_windows` must contain numeric vectors of length 2.",
        call. = FALSE
      )
    }
  }

  x
}

.gp3_multiverse_check_numeric_vector <- function(x, arg, allow_zero = FALSE) {
  if (!is.numeric(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!is.finite(x))) {
    stop("`", arg, "` must be a non-empty finite numeric vector.", call. = FALSE)
  }

  if (isTRUE(allow_zero)) {
    bad <- x < 0
    rule <- "non-negative"
  } else {
    bad <- x <= 0
    rule <- "positive"
  }

  if (any(bad)) {
    stop("`", arg, "` must contain only ", rule, " values.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_multiverse_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_multiverse_check_character_scalar <- function(x, arg) {
  if (!is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_multiverse_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_multiverse_sanitise_label <- function(x) {
  chars <- strsplit(x, split = "", fixed = TRUE)[[1L]]
  allowed <- c(letters, LETTERS, as.character(0:9), "_", ".", "-")
  chars[!chars %in% allowed] <- "_"
  x <- paste(chars, collapse = "")

  while (grepl("__", x, fixed = TRUE)) {
    x <- gsub("__", "_", x, fixed = TRUE)
  }

  while (nzchar(x) && startsWith(x, "_")) {
    x <- substring(x, 2L)
  }

  while (nzchar(x) && endsWith(x, "_")) {
    x <- substring(x, 1L, nchar(x) - 1L)
  }

  if (!nzchar(x)) {
    x <- "mv"
  }

  x
}

.gp3_multiverse_collapse_value <- function(x) {
  paste(as.character(x), collapse = ", ")
}

.gp3_multiverse_collapse_baseline_windows <- function(x) {
  x <- .gp3_multiverse_normalise_baseline_windows(x)

  paste(
    vapply(
      x,
      function(w) {
        paste0("[", w[[1]], ", ", w[[2]], "]")
      },
      character(1)
    ),
    collapse = ", "
  )
}
