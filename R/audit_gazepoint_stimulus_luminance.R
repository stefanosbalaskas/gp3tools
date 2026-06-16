#' Audit stimulus luminance and brightness for Gazepoint studies
#'
#' Compute stimulus-level luminance, brightness, and contrast summaries for
#' image stimuli used in Gazepoint eye-tracking and pupillometry studies. This
#' helper is intended as a publication-readiness audit because pupil size is
#' strongly affected by stimulus brightness.
#'
#' @param data A data frame containing at least a stimulus image/file column.
#' @param stimulus_file_col Name of the stimulus image/file column. If `NULL`,
#'   common file-column names are detected automatically.
#' @param stimulus_id_col Optional stimulus identifier column. If `NULL`, common
#'   stimulus/media/item identifier columns are detected automatically.
#' @param condition_col Optional experimental condition column. If `NULL`, common
#'   condition columns are detected automatically. If no condition column exists,
#'   all rows are assigned to `"all_data"`.
#' @param image_dir Optional directory prepended to relative stimulus paths.
#' @param recursive If `TRUE`, unresolved relative file names are searched for
#'   recursively under `image_dir`.
#' @param name Character label stored in the audit object.
#'
#' @return A list with class `gp3_stimulus_luminance_audit`.
#' @export
audit_gazepoint_stimulus_luminance <- function(
    data,
    stimulus_file_col = NULL,
    stimulus_id_col = NULL,
    condition_col = NULL,
    image_dir = NULL,
    recursive = TRUE,
    name = "gazepoint_stimulus_luminance"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  .gp3_luminance_check_label(name, "name")

  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(image_dir)) {
    .gp3_luminance_check_label(image_dir, "image_dir")
  }

  names_data <- names(data)

  stimulus_file_col <- .gp3_luminance_resolve_or_detect_col(
    col = stimulus_file_col,
    names_data = names_data,
    arg = "stimulus_file_col",
    candidates = c(
      "stimulus_file",
      "STIMULUS_FILE",
      "image_file",
      "IMAGE_FILE",
      "file_name",
      "filename",
      "media_file",
      "MEDIA_FILE",
      "stimulus_path",
      "image_path",
      "file_path"
    ),
    required = TRUE
  )

  stimulus_id_col <- .gp3_luminance_resolve_or_detect_col(
    col = stimulus_id_col,
    names_data = names_data,
    arg = "stimulus_id_col",
    candidates = c(
      "stimulus_id",
      "STIMULUS_ID",
      "media_id",
      "MEDIA_ID",
      "item_id",
      "ITEM_ID",
      "image_id",
      "stimulus",
      "media",
      "item"
    ),
    required = FALSE
  )

  condition_col <- .gp3_luminance_resolve_or_detect_col(
    col = condition_col,
    names_data = names_data,
    arg = "condition_col",
    candidates = c(
      "condition",
      "CONDITION",
      "group",
      "GROUP",
      "trial_type",
      "TRIAL_TYPE"
    ),
    required = FALSE
  )

  stimulus_index <- tibble::tibble(
    stimulus_file = as.character(data[[stimulus_file_col]])
  )

  if (!is.null(stimulus_id_col)) {
    stimulus_index$stimulus_id <- as.character(data[[stimulus_id_col]])
  } else {
    stimulus_index$stimulus_id <- stimulus_index$stimulus_file
  }

  if (!is.null(condition_col)) {
    stimulus_index$condition <- as.character(data[[condition_col]])
  } else {
    stimulus_index$condition <- "all_data"
  }

  stimulus_index$stimulus_file[is.na(stimulus_index$stimulus_file)] <- NA_character_
  stimulus_index$stimulus_id[is.na(stimulus_index$stimulus_id)] <- NA_character_
  stimulus_index$condition[is.na(stimulus_index$condition) | !nzchar(stimulus_index$condition)] <- "missing_condition"

  stimulus_index <- stimulus_index |>
    dplyr::mutate(
      stimulus_file = trimws(.data$stimulus_file),
      stimulus_id = trimws(.data$stimulus_id)
    ) |>
    dplyr::distinct(.data$stimulus_id, .data$stimulus_file, .data$condition)

  stimulus_files <- stimulus_index |>
    dplyr::distinct(.data$stimulus_id, .data$stimulus_file)

  magick_available <- requireNamespace("magick", quietly = TRUE)

  stimulus_luminance <- lapply(seq_len(nrow(stimulus_files)), function(i) {
    .gp3_luminance_read_one(
      stimulus_id = stimulus_files$stimulus_id[[i]],
      stimulus_file = stimulus_files$stimulus_file[[i]],
      image_dir = image_dir,
      recursive = recursive,
      magick_available = magick_available
    )
  }) |>
    dplyr::bind_rows()

  condition_data <- stimulus_index |>
    dplyr::left_join(
      stimulus_luminance,
      by = c("stimulus_id", "stimulus_file")
    )

  condition_summary <- condition_data |>
    dplyr::group_by(.data$condition) |>
    dplyr::summarise(
      n_stimulus_rows = dplyr::n(),
      n_unique_stimuli = dplyr::n_distinct(.data$stimulus_id),
      n_unique_files = dplyr::n_distinct(.data$resolved_path),
      n_files_available = sum(.data$file_exists, na.rm = TRUE),
      n_luminance_available = sum(.data$luminance_available, na.rm = TRUE),
      mean_luminance = mean(.data$mean_luminance, na.rm = TRUE),
      median_luminance = stats::median(.data$mean_luminance, na.rm = TRUE),
      sd_luminance = stats::sd(.data$mean_luminance, na.rm = TRUE),
      mean_rms_contrast = mean(.data$rms_contrast, na.rm = TRUE),
      mean_michelson_contrast = mean(.data$michelson_contrast, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(
          "mean_luminance",
          "median_luminance",
          "sd_luminance",
          "mean_rms_contrast",
          "mean_michelson_contrast"
        ),
        ~ ifelse(is.nan(.x), NA_real_, .x)
      ),
      condition_luminance_status = dplyr::case_when(
        .data$n_luminance_available == 0 ~ "no_luminance_available",
        .data$n_luminance_available < .data$n_stimulus_rows ~ "partial_luminance_available",
        TRUE ~ "complete_luminance_available"
      )
    )

  balance_summary <- .gp3_luminance_balance_summary(condition_summary)

  overview <- tibble::tibble(
    object_name = name,
    n_input_rows = nrow(data),
    n_stimulus_rows = nrow(stimulus_index),
    n_unique_stimuli = dplyr::n_distinct(stimulus_index$stimulus_id),
    n_unique_files = dplyr::n_distinct(stimulus_index$stimulus_file),
    n_conditions = dplyr::n_distinct(stimulus_index$condition),
    n_files_available = sum(stimulus_luminance$file_exists, na.rm = TRUE),
    n_luminance_available = sum(stimulus_luminance$luminance_available, na.rm = TRUE),
    magick_available = magick_available,
    audit_status = dplyr::case_when(
      !magick_available ~ "skipped_missing_magick",
      sum(stimulus_luminance$luminance_available, na.rm = TRUE) == 0 ~ "no_luminance_available",
      any(!stimulus_luminance$luminance_available) ~ "partial_luminance_available",
      TRUE ~ "complete_luminance_available"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "stimulus_file_col",
      "stimulus_id_col",
      "condition_col",
      "image_dir",
      "recursive",
      "name"
    ),
    value = c(
      stimulus_file_col,
      .gp3_luminance_collapse_nullable(stimulus_id_col),
      .gp3_luminance_collapse_nullable(condition_col),
      .gp3_luminance_collapse_nullable(image_dir),
      as.character(recursive),
      name
    )
  )

  out <- list(
    overview = overview,
    stimulus_index = stimulus_index,
    stimulus_luminance = stimulus_luminance,
    condition_summary = condition_summary,
    balance_summary = balance_summary,
    settings = settings
  )

  class(out) <- c("gp3_stimulus_luminance_audit", "list")

  out
}

.gp3_luminance_read_one <- function(
    stimulus_id,
    stimulus_file,
    image_dir,
    recursive,
    magick_available
) {
  resolved_path <- .gp3_luminance_resolve_path(
    stimulus_file = stimulus_file,
    image_dir = image_dir,
    recursive = recursive
  )

  base <- tibble::tibble(
    stimulus_id = stimulus_id,
    stimulus_file = stimulus_file,
    resolved_path = resolved_path,
    file_exists = !is.na(resolved_path) && file.exists(resolved_path),
    luminance_available = FALSE,
    image_width_px = NA_integer_,
    image_height_px = NA_integer_,
    n_pixels = NA_integer_,
    mean_luminance = NA_real_,
    median_luminance = NA_real_,
    sd_luminance = NA_real_,
    min_luminance = NA_real_,
    max_luminance = NA_real_,
    mean_brightness = NA_real_,
    rms_contrast = NA_real_,
    michelson_contrast = NA_real_,
    luminance_status = NA_character_,
    error_message = NA_character_
  )

  if (!magick_available) {
    base$luminance_status <- "skipped_missing_magick"
    return(base)
  }

  if (is.na(stimulus_file) || !nzchar(stimulus_file)) {
    base$luminance_status <- "missing_file_name"
    return(base)
  }

  if (is.na(resolved_path) || !file.exists(resolved_path)) {
    base$luminance_status <- "file_missing"
    return(base)
  }

  tryCatch({
    img <- magick::image_read(resolved_path)
    info <- magick::image_info(img)

    rgb <- magick::image_data(img, channels = "rgb")

    red <- as.integer(rgb[1, , ]) / 255
    green <- as.integer(rgb[2, , ]) / 255
    blue <- as.integer(rgb[3, , ]) / 255

    red_linear <- .gp3_luminance_srgb_to_linear(red)
    green_linear <- .gp3_luminance_srgb_to_linear(green)
    blue_linear <- .gp3_luminance_srgb_to_linear(blue)

    relative_luminance <- 0.2126 * red_linear +
      0.7152 * green_linear +
      0.0722 * blue_linear

    relative_luminance <- as.numeric(relative_luminance)
    relative_luminance <- relative_luminance[
      !is.na(relative_luminance) & is.finite(relative_luminance)
    ]

    mean_lum <- if (length(relative_luminance) == 0L) {
      NA_real_
    } else {
      mean(relative_luminance, na.rm = TRUE)
    }

    sd_lum <- if (length(relative_luminance) < 2L) {
      NA_real_
    } else {
      stats::sd(relative_luminance, na.rm = TRUE)
    }

    min_lum <- if (length(relative_luminance) == 0L) {
      NA_real_
    } else {
      min(relative_luminance, na.rm = TRUE)
    }

    max_lum <- if (length(relative_luminance) == 0L) {
      NA_real_
    } else {
      max(relative_luminance, na.rm = TRUE)
    }

    base$image_width_px <- as.integer(info$width[[1]])
    base$image_height_px <- as.integer(info$height[[1]])
    base$n_pixels <- length(relative_luminance)
    base$mean_luminance <- mean_lum
    base$median_luminance <- if (length(relative_luminance) == 0L) {
      NA_real_
    } else {
      stats::median(relative_luminance, na.rm = TRUE)
    }
    base$sd_luminance <- sd_lum
    base$min_luminance <- min_lum
    base$max_luminance <- max_lum
    base$mean_brightness <- mean_lum
    base$rms_contrast <- if (!is.na(mean_lum) && mean_lum > 0) {
      sd_lum / mean_lum
    } else {
      NA_real_
    }
    base$michelson_contrast <- if (
      !is.na(max_lum) &&
      !is.na(min_lum) &&
      (max_lum + min_lum) > 0
    ) {
      (max_lum - min_lum) / (max_lum + min_lum)
    } else {
      NA_real_
    }
    base$luminance_available <- TRUE
    base$luminance_status <- "available"

    base
  }, error = function(e) {
    base$luminance_status <- "read_error"
    base$error_message <- conditionMessage(e)
    base
  })
}

.gp3_luminance_srgb_to_linear <- function(x) {
  dplyr::if_else(
    x <= 0.04045,
    x / 12.92,
    ((x + 0.055) / 1.055)^2.4
  )
}

.gp3_luminance_resolve_path <- function(stimulus_file, image_dir, recursive) {
  if (is.na(stimulus_file) || !nzchar(stimulus_file)) {
    return(NA_character_)
  }

  if (file.exists(stimulus_file)) {
    return(normalizePath(stimulus_file, winslash = "/", mustWork = FALSE))
  }

  candidate <- if (!is.null(image_dir)) {
    file.path(image_dir, stimulus_file)
  } else {
    stimulus_file
  }

  if (file.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
  }

  if (!is.null(image_dir) && isTRUE(recursive) && dir.exists(image_dir)) {
    matches <- list.files(
      path = image_dir,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = FALSE
    )

    matches <- matches[basename(matches) == basename(stimulus_file)]

    if (length(matches) > 0L) {
      return(normalizePath(matches[[1]], winslash = "/", mustWork = FALSE))
    }
  }

  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

.gp3_luminance_balance_summary <- function(condition_summary) {
  if (nrow(condition_summary) == 0L) {
    return(
      tibble::tibble(
        n_conditions = 0L,
        n_conditions_with_luminance = 0L,
        min_condition_mean_luminance = NA_real_,
        max_condition_mean_luminance = NA_real_,
        range_condition_mean_luminance = NA_real_,
        max_abs_pairwise_condition_difference = NA_real_,
        luminance_balance_status = "no_conditions"
      )
    )
  }

  available <- condition_summary |>
    dplyr::filter(!is.na(.data$mean_luminance))

  if (nrow(available) == 0L) {
    return(
      tibble::tibble(
        n_conditions = nrow(condition_summary),
        n_conditions_with_luminance = 0L,
        min_condition_mean_luminance = NA_real_,
        max_condition_mean_luminance = NA_real_,
        range_condition_mean_luminance = NA_real_,
        max_abs_pairwise_condition_difference = NA_real_,
        luminance_balance_status = "no_luminance_available"
      )
    )
  }

  pairwise_diff <- if (nrow(available) < 2L) {
    NA_real_
  } else {
    max(abs(stats::dist(available$mean_luminance)), na.rm = TRUE)
  }

  tibble::tibble(
    n_conditions = nrow(condition_summary),
    n_conditions_with_luminance = nrow(available),
    min_condition_mean_luminance = min(available$mean_luminance, na.rm = TRUE),
    max_condition_mean_luminance = max(available$mean_luminance, na.rm = TRUE),
    range_condition_mean_luminance = max(available$mean_luminance, na.rm = TRUE) -
      min(available$mean_luminance, na.rm = TRUE),
    max_abs_pairwise_condition_difference = pairwise_diff,
    luminance_balance_status = dplyr::case_when(
      nrow(available) < nrow(condition_summary) ~ "partial_condition_luminance_available",
      nrow(available) < 2L ~ "single_condition_available",
      TRUE ~ "condition_luminance_summarised"
    )
  )
}

.gp3_luminance_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_luminance_resolve_or_detect_col <- function(
    col,
    names_data,
    arg,
    candidates,
    required
) {
  if (!is.null(col)) {
    return(.gp3_luminance_resolve_col(col, names_data, arg))
  }

  found <- candidates[candidates %in% names_data]

  if (length(found) > 0L) {
    return(found[[1]])
  }

  if (isTRUE(required)) {
    stop("`", arg, "` could not be detected and must be supplied.", call. = FALSE)
  }

  NULL
}

.gp3_luminance_check_label <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_luminance_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
