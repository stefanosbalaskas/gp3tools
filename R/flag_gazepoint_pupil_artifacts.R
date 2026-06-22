#' Flag Gazepoint pupil artifacts before interpolation
#'
#' Adds pupil-specific artifact flags for blink/trackloss contamination,
#' physiological implausibility when pupil units are millimetres, pupil-speed
#' outliers, left-right binocular pupil disagreement, and temporal padding
#' around bad samples. The function preserves raw pupil columns and creates
#' `pupil_clean`, which can be used as input for interpolation.
#'
#' @param data A Gazepoint master table or pupil-processing table.
#' @param pupil_col Optional name of the pupil column to clean. If `NULL`, the
#'   function detects one of `mean_pupil`, `pupil_raw`, `pupil`,
#'   `left_pupil`, or `right_pupil`.
#' @param left_pupil_col Optional left-pupil column. If `NULL`, `left_pupil` is
#'   used when available.
#' @param right_pupil_col Optional right-pupil column. If `NULL`, `right_pupil`
#'   is used when available.
#' @param time_col Optional time column. If `NULL`, the function detects one of
#'   `time_ms`, `time`, `time_orig`, or `time_orig_ms`.
#' @param blink_col Optional blink column. If `NULL`, `blink` is used when
#'   available.
#' @param trackloss_col Optional trackloss column. If `NULL`, one of
#'   `trackloss` or `Trackloss` is used when available.
#' @param missing_pupil_col Optional missing-pupil column. If `NULL`,
#'   `missing_pupil` is used when available.
#' @param pupil_unit_col Optional pupil-unit column. If `NULL`, `pupil_unit` is
#'   used when available.
#' @param group_cols Character vector of grouping columns used for speed
#'   outlier detection and artifact-padding windows. Defaults to
#'   `c("subject", "media_id")`. Use `character(0)` for global processing.
#' @param registry Optional preprocessing registry created by
#'   [create_gazepoint_preprocessing_registry()].
#' @param blink_padding_pre_ms Padding before bad samples, in milliseconds. If
#'   `NULL`, taken from `registry` or defaults to `100`.
#' @param blink_padding_post_ms Padding after bad samples, in milliseconds. If
#'   `NULL`, taken from `registry` or defaults to `100`.
#' @param pupil_min_mm Minimum plausible pupil value when units are millimetres.
#'   If `NULL`, taken from `registry` or defaults to `1`.
#' @param pupil_max_mm Maximum plausible pupil value when units are millimetres.
#'   If `NULL`, taken from `registry` or defaults to `9`.
#' @param pupil_speed_mad_k MAD multiplier for pupil-speed outlier detection. If
#'   `NULL`, taken from `registry` or defaults to `6`.
#' @param binocular_mad_k MAD multiplier for binocular-disagreement detection.
#'   If `NULL`, taken from `registry` or defaults to `6`.
#' @param max_physio_outlier_prop Maximum allowed proportion of non-missing
#'   millimetre-labelled pupil samples that may be rejected by the physiological
#'   rule before the rule is automatically suppressed. Defaults to `0.80`.
#'   This prevents raw-unit Gazepoint exports from being silently erased when
#'   the unit label suggests millimetres but the numeric scale is not compatible
#'   with ordinary 1--9 mm thresholds.
#' @param flag_speed_outliers Logical. If `TRUE`, pupil-speed outliers are
#'   flagged. Defaults to `TRUE`.
#' @param flag_binocular_disagreement Logical. If `TRUE`, left-right pupil
#'   disagreement is flagged when both eyes are available. Defaults to `TRUE`.
#' @param flag_physiological_outliers Logical. If `TRUE`, millimetre-based
#'   physiological thresholds are applied only when the pupil unit is identified
#'   as millimetres. Defaults to `TRUE`.
#'
#' @return A tibble containing the original data plus pupil-artifact columns.
#'
#' @examples
#' \donttest{
#' master <- gazepoint_example_master
#' registry <- create_gazepoint_preprocessing_registry()
#'
#' artifact_pupil <- flag_gazepoint_pupil_artifacts(
#'   master,
#'   registry = registry
#' )
#'
#' dplyr::count(artifact_pupil, pupil_artifact_reason)
#' }
#'
#' @importFrom rlang .data
#'
#' @export
flag_gazepoint_pupil_artifacts <- function(
    data,
    pupil_col = NULL,
    left_pupil_col = NULL,
    right_pupil_col = NULL,
    time_col = NULL,
    blink_col = NULL,
    trackloss_col = NULL,
    missing_pupil_col = NULL,
    pupil_unit_col = NULL,
    group_cols = c("subject", "media_id"),
    registry = NULL,
    blink_padding_pre_ms = NULL,
    blink_padding_post_ms = NULL,
    pupil_min_mm = NULL,
    pupil_max_mm = NULL,
    pupil_speed_mad_k = NULL,
    binocular_mad_k = NULL,
    max_physio_outlier_prop = 0.80,
    flag_speed_outliers = TRUE,
    flag_binocular_disagreement = TRUE,
    flag_physiological_outliers = TRUE
) {
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  scalar_char_or_null <- function(x, name) {
    if (!is.null(x) && (!is.character(x) || length(x) != 1)) {
      rlang::abort(paste0("`", name, "` must be `NULL` or a single character string."))
    }
  }

  scalar_char_or_null(pupil_col, "pupil_col")
  scalar_char_or_null(left_pupil_col, "left_pupil_col")
  scalar_char_or_null(right_pupil_col, "right_pupil_col")
  scalar_char_or_null(time_col, "time_col")
  scalar_char_or_null(blink_col, "blink_col")
  scalar_char_or_null(trackloss_col, "trackloss_col")
  scalar_char_or_null(missing_pupil_col, "missing_pupil_col")
  scalar_char_or_null(pupil_unit_col, "pupil_unit_col")

  if (!is.character(group_cols)) {
    rlang::abort("`group_cols` must be a character vector.")
  }

  scalar_logical <- function(x, name) {
    if (!is.logical(x) || length(x) != 1) {
      rlang::abort(paste0("`", name, "` must be `TRUE` or `FALSE`."))
    }
  }

  scalar_logical(flag_speed_outliers, "flag_speed_outliers")
  scalar_logical(flag_binocular_disagreement, "flag_binocular_disagreement")
  scalar_logical(flag_physiological_outliers, "flag_physiological_outliers")

  registry_value <- function(parameter_name, default_value) {
    if (is.null(registry)) {
      return(default_value)
    }

    if (!is.data.frame(registry) || !all(c("parameter", "value") %in% names(registry))) {
      rlang::abort("`registry` must be a preprocessing registry with `parameter` and `value` columns.")
    }

    value <- registry$value[registry$parameter == parameter_name]

    if (length(value) != 1) {
      rlang::abort(paste0("Expected exactly one registry value for: ", parameter_name))
    }

    as.numeric(value)
  }

  blink_padding_pre_ms <- if (is.null(blink_padding_pre_ms)) {
    registry_value("blink_padding_pre_ms", 100)
  } else {
    blink_padding_pre_ms
  }

  blink_padding_post_ms <- if (is.null(blink_padding_post_ms)) {
    registry_value("blink_padding_post_ms", 100)
  } else {
    blink_padding_post_ms
  }

  pupil_min_mm <- if (is.null(pupil_min_mm)) {
    registry_value("pupil_physiological_min", 1)
  } else {
    pupil_min_mm
  }

  pupil_max_mm <- if (is.null(pupil_max_mm)) {
    registry_value("pupil_physiological_max", 9)
  } else {
    pupil_max_mm
  }

  pupil_speed_mad_k <- if (is.null(pupil_speed_mad_k)) {
    registry_value("pupil_speed_mad_k", 6)
  } else {
    pupil_speed_mad_k
  }

  binocular_mad_k <- if (is.null(binocular_mad_k)) {
    registry_value("binocular_mad_k", 6)
  } else {
    binocular_mad_k
  }

  numeric_scalar <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
      rlang::abort(paste0("`", name, "` must be a single non-missing numeric value."))
    }
  }

  numeric_scalar(blink_padding_pre_ms, "blink_padding_pre_ms")
  numeric_scalar(blink_padding_post_ms, "blink_padding_post_ms")
  numeric_scalar(pupil_min_mm, "pupil_min_mm")
  numeric_scalar(pupil_max_mm, "pupil_max_mm")
  numeric_scalar(pupil_speed_mad_k, "pupil_speed_mad_k")
  numeric_scalar(binocular_mad_k, "binocular_mad_k")
  numeric_scalar(max_physio_outlier_prop, "max_physio_outlier_prop")

  if (blink_padding_pre_ms < 0) {
    rlang::abort("`blink_padding_pre_ms` must be greater than or equal to 0.")
  }

  if (blink_padding_post_ms < 0) {
    rlang::abort("`blink_padding_post_ms` must be greater than or equal to 0.")
  }

  if (pupil_max_mm <= pupil_min_mm) {
    rlang::abort("`pupil_max_mm` must be greater than `pupil_min_mm`.")
  }

  if (pupil_speed_mad_k < 0) {
    rlang::abort("`pupil_speed_mad_k` must be greater than or equal to 0.")
  }

  if (binocular_mad_k < 0) {
    rlang::abort("`binocular_mad_k` must be greater than or equal to 0.")
  }

  if (max_physio_outlier_prop < 0 || max_physio_outlier_prop > 1) {
    rlang::abort("`max_physio_outlier_prop` must be between 0 and 1.")
  }

  detect_col <- function(candidates) {
    found <- candidates[candidates %in% names(data)]

    if (length(found) == 0) {
      return(NA_character_)
    }

    found[[1]]
  }

  subject_source <- detect_col(c("subject", "pID", "participant"))
  media_source <- detect_col(c("media_id", "MEDIA_ID"))
  trial_source <- detect_col(c("trial"))
  trial_global_source <- detect_col(c("trial_global"))

  pupil_source <- if (is.null(pupil_col)) {
    detect_col(c("mean_pupil", "pupil_raw", "pupil", "left_pupil", "right_pupil"))
  } else {
    pupil_col
  }

  left_pupil_source <- if (is.null(left_pupil_col)) {
    detect_col(c("left_pupil", "LEFT_PUPIL"))
  } else {
    left_pupil_col
  }

  right_pupil_source <- if (is.null(right_pupil_col)) {
    detect_col(c("right_pupil", "RIGHT_PUPIL"))
  } else {
    right_pupil_col
  }

  time_source <- if (is.null(time_col)) {
    detect_col(c("time_ms", "time", "time_orig", "time_orig_ms"))
  } else {
    time_col
  }

  blink_source <- if (is.null(blink_col)) {
    detect_col(c("blink"))
  } else {
    blink_col
  }

  trackloss_source <- if (is.null(trackloss_col)) {
    detect_col(c("trackloss", "Trackloss"))
  } else {
    trackloss_col
  }

  missing_pupil_source <- if (is.null(missing_pupil_col)) {
    detect_col(c("missing_pupil"))
  } else {
    missing_pupil_col
  }

  pupil_unit_source <- if (is.null(pupil_unit_col)) {
    detect_col(c("pupil_unit", "PUPIL_UNIT", "pupil_unit_text"))
  } else {
    pupil_unit_col
  }

  if (is.na(pupil_source) || !pupil_source %in% names(data)) {
    rlang::abort("No pupil column was found.")
  }

  if (is.na(time_source) || !time_source %in% names(data)) {
    rlang::abort("No time column was found.")
  }

  optional_sources <- c(
    left_pupil_col = left_pupil_source,
    right_pupil_col = right_pupil_source,
    blink_col = blink_source,
    trackloss_col = trackloss_source,
    missing_pupil_col = missing_pupil_source,
    pupil_unit_col = pupil_unit_source
  )

  for (name in names(optional_sources)) {
    value <- optional_sources[[name]]

    if (!is.na(value) && !value %in% names(data)) {
      rlang::abort(paste0("`", name, "` was not found in `data`."))
    }
  }

  role_sources <- c(
    subject = subject_source,
    media_id = media_source,
    trial = trial_source,
    trial_global = trial_global_source
  )

  standard_group_roles <- names(role_sources)

  missing_group_roles <- group_cols[
    group_cols %in% standard_group_roles &
      is.na(role_sources[group_cols])
  ]

  if (length(missing_group_roles) > 0) {
    rlang::abort(
      paste0(
        "The following grouping column role(s) were requested but not found: ",
        paste(missing_group_roles, collapse = ", ")
      )
    )
  }

  non_role_group_cols <- setdiff(group_cols, standard_group_roles)
  missing_non_role_group_cols <- setdiff(non_role_group_cols, names(data))

  if (length(missing_non_role_group_cols) > 0) {
    rlang::abort(
      paste0(
        "The following grouping column(s) were requested but not found: ",
        paste(missing_non_role_group_cols, collapse = ", ")
      )
    )
  }

  to_logical_or_false <- function(x) {
    if (is.null(x)) {
      return(rep(FALSE, nrow(data)))
    }

    out <- suppressWarnings(as.logical(x))
    out[is.na(out)] <- FALSE
    out
  }

  pupil_numeric <- suppressWarnings(as.numeric(data[[pupil_source]]))

  left_numeric <- if (!is.na(left_pupil_source)) {
    suppressWarnings(as.numeric(data[[left_pupil_source]]))
  } else {
    rep(NA_real_, nrow(data))
  }

  right_numeric <- if (!is.na(right_pupil_source)) {
    suppressWarnings(as.numeric(data[[right_pupil_source]]))
  } else {
    rep(NA_real_, nrow(data))
  }

  missing_pupil_flag <- if (!is.na(missing_pupil_source)) {
    to_logical_or_false(data[[missing_pupil_source]])
  } else {
    is.na(pupil_numeric)
  }

  blink_flag <- if (!is.na(blink_source)) {
    to_logical_or_false(data[[blink_source]])
  } else {
    rep(FALSE, nrow(data))
  }

  trackloss_flag <- if (!is.na(trackloss_source)) {
    to_logical_or_false(data[[trackloss_source]])
  } else {
    rep(FALSE, nrow(data))
  }

  prior_invalid_flag <- if ("pupil_flag_invalid" %in% names(data)) {
    to_logical_or_false(data[["pupil_flag_invalid"]])
  } else {
    rep(FALSE, nrow(data))
  }

  pupil_unit_text <- if (!is.na(pupil_unit_source)) {
    tolower(as.character(data[[pupil_unit_source]]))
  } else {
    rep(NA_character_, nrow(data))
  }

  pupil_unit_is_mm <- !is.na(pupil_unit_text) &
    grepl("diameter_mm|\\bmm\\b|millimet", pupil_unit_text, perl = TRUE)

  work <- tibble::tibble(
    row_id = seq_len(nrow(data)),
    subject = if (!is.na(subject_source)) as.character(data[[subject_source]]) else NA_character_,
    media_id = if (!is.na(media_source)) as.character(data[[media_source]]) else NA_character_,
    trial = if (!is.na(trial_source)) as.character(data[[trial_source]]) else NA_character_,
    trial_global = if (!is.na(trial_global_source)) as.character(data[[trial_global_source]]) else NA_character_,
    time_ms = suppressWarnings(as.numeric(data[[time_source]])),
    pupil_artifact_raw_value = pupil_numeric,
    left_pupil_artifact_raw_value = left_numeric,
    right_pupil_artifact_raw_value = right_numeric,
    pupil_unit_text = pupil_unit_text,
    pupil_unit_is_mm = pupil_unit_is_mm,
    pupil_flag_missing_source = missing_pupil_flag,
    pupil_flag_blink_source = blink_flag,
    pupil_flag_trackloss_source = trackloss_flag,
    pupil_flag_prior_invalid_source = prior_invalid_flag
  )

  if (length(non_role_group_cols) > 0) {
    for (col in non_role_group_cols) {
      work[[col]] <- data[[col]]
    }
  }

  processing_group_cols <- intersect(group_cols, names(work))

  work <- work |>
    dplyr::mutate(
      pupil_artifact_nonfinite = !is.na(.data$pupil_artifact_raw_value) &
        !is.finite(.data$pupil_artifact_raw_value),
      pupil_artifact_nonpositive = is.finite(.data$pupil_artifact_raw_value) &
        .data$pupil_artifact_raw_value <= 0,
      pupil_candidate_value = dplyr::if_else(
        is.finite(.data$pupil_artifact_raw_value) &
          .data$pupil_artifact_raw_value > 0,
        .data$pupil_artifact_raw_value,
        NA_real_
      ),
      pupil_physio_outlier_candidate = isTRUE(flag_physiological_outliers) &
        .data$pupil_unit_is_mm &
        !is.na(.data$pupil_candidate_value) &
        (
          .data$pupil_candidate_value < pupil_min_mm |
            .data$pupil_candidate_value > pupil_max_mm
        ),
      pupil_lr_absdiff = abs(
        dplyr::if_else(
          is.finite(.data$left_pupil_artifact_raw_value) &
            .data$left_pupil_artifact_raw_value > 0,
          .data$left_pupil_artifact_raw_value,
          NA_real_
        ) -
          dplyr::if_else(
            is.finite(.data$right_pupil_artifact_raw_value) &
              .data$right_pupil_artifact_raw_value > 0,
            .data$right_pupil_artifact_raw_value,
            NA_real_
          )
      )
    )

  physio_denominator <- sum(
    work$pupil_unit_is_mm & !is.na(work$pupil_candidate_value),
    na.rm = TRUE
  )

  physio_candidate_n <- sum(
    work$pupil_physio_outlier_candidate,
    na.rm = TRUE
  )

  physio_candidate_prop <- if (physio_denominator > 0) {
    physio_candidate_n / physio_denominator
  } else {
    0
  }

  pupil_physio_rule_suppressed <- isTRUE(flag_physiological_outliers) &&
    physio_denominator > 0 &&
    physio_candidate_prop > max_physio_outlier_prop

  work <- work |>
    dplyr::mutate(
      pupil_physio_candidate_prop = physio_candidate_prop,
      pupil_physio_rule_suppressed = pupil_physio_rule_suppressed,
      pupil_physio_outlier = .data$pupil_physio_outlier_candidate &
        !.data$pupil_physio_rule_suppressed
    )

  lr_values <- work$pupil_lr_absdiff[is.finite(work$pupil_lr_absdiff)]

  pupil_binocular_disagreement_threshold <- if (
    length(lr_values) == 0 ||
    !isTRUE(flag_binocular_disagreement)
  ) {
    Inf
  } else {
    lr_median <- stats::median(lr_values, na.rm = TRUE)
    lr_mad <- stats::mad(lr_values, constant = 1, na.rm = TRUE)

    if (!is.finite(lr_mad) || lr_mad == 0) {
      as.numeric(stats::quantile(lr_values, 0.99, na.rm = TRUE, names = FALSE))
    } else {
      lr_median + binocular_mad_k * lr_mad
    }
  }

  if (!is.finite(pupil_binocular_disagreement_threshold)) {
    pupil_binocular_disagreement_threshold <- Inf
  }

  work <- work |>
    dplyr::mutate(
      pupil_binocular_disagreement_threshold = pupil_binocular_disagreement_threshold,
      pupil_binocular_disagreement = isTRUE(flag_binocular_disagreement) &
        is.finite(.data$pupil_lr_absdiff) &
        .data$pupil_lr_absdiff > .data$pupil_binocular_disagreement_threshold
    )

  compute_speed_flags <- function(df) {
    df <- df |>
      dplyr::arrange(.data$time_ms, .data$row_id)

    n <- nrow(df)

    speed <- rep(NA_real_, n)
    abs_speed <- rep(NA_real_, n)

    if (n >= 2) {
      for (i in 2:n) {
        dt <- df$time_ms[[i]] - df$time_ms[[i - 1]]
        dp <- df$pupil_candidate_value[[i]] - df$pupil_candidate_value[[i - 1]]

        if (is.finite(dt) && dt > 0 && is.finite(dp)) {
          speed[[i]] <- dp / dt
          abs_speed[[i]] <- abs(speed[[i]])
        }
      }
    }

    finite_speed <- abs_speed[is.finite(abs_speed)]

    threshold <- if (
      length(finite_speed) < 3 ||
      !isTRUE(flag_speed_outliers)
    ) {
      Inf
    } else {
      speed_median <- stats::median(finite_speed, na.rm = TRUE)
      speed_mad <- stats::mad(finite_speed, constant = 1, na.rm = TRUE)

      if (!is.finite(speed_mad) || speed_mad == 0) {
        as.numeric(stats::quantile(finite_speed, 0.99, na.rm = TRUE, names = FALSE))
      } else {
        speed_median + pupil_speed_mad_k * speed_mad
      }
    }

    if (!is.finite(threshold)) {
      threshold <- Inf
    }

    tibble::tibble(
      row_id = df$row_id,
      pupil_speed = speed,
      pupil_speed_abs = abs_speed,
      pupil_speed_threshold = threshold,
      pupil_speed_outlier = isTRUE(flag_speed_outliers) &
        is.finite(abs_speed) &
        abs_speed > threshold
    )
  }

  speed_flags <- if (length(processing_group_cols) == 0) {
    compute_speed_flags(work)
  } else {
    work |>
      dplyr::group_by(!!!rlang::syms(processing_group_cols)) |>
      dplyr::group_modify(~ compute_speed_flags(.x)) |>
      dplyr::ungroup()
  }

  speed_flags <- speed_flags |>
    dplyr::select(
      dplyr::all_of(c(
        "row_id",
        "pupil_speed",
        "pupil_speed_abs",
        "pupil_speed_threshold",
        "pupil_speed_outlier"
      ))
    )

  work <- work |>
    dplyr::left_join(speed_flags, by = "row_id") |>
    dplyr::mutate(
      pupil_bad_sample_basic = .data$pupil_flag_missing_source |
        .data$pupil_flag_blink_source |
        .data$pupil_flag_trackloss_source |
        .data$pupil_flag_prior_invalid_source |
        .data$pupil_artifact_nonfinite |
        .data$pupil_artifact_nonpositive |
        .data$pupil_physio_outlier |
        .data$pupil_binocular_disagreement |
        .data$pupil_speed_outlier
    )

  compute_padding <- function(df) {
    df <- df |>
      dplyr::arrange(.data$time_ms, .data$row_id)

    event_times <- df$time_ms[df$pupil_bad_sample_basic & is.finite(df$time_ms)]

    padding_flag <- rep(FALSE, nrow(df))

    if (length(event_times) > 0) {
      padding_flag <- vapply(
        df$time_ms,
        function(t) {
          if (!is.finite(t)) {
            return(FALSE)
          }

          any(
            t >= event_times - blink_padding_pre_ms &
              t <= event_times + blink_padding_post_ms,
            na.rm = TRUE
          )
        },
        logical(1)
      )
    }

    tibble::tibble(
      row_id = df$row_id,
      pupil_artifact_padding_flag = padding_flag
    )
  }

  padding_flags <- if (length(processing_group_cols) == 0) {
    compute_padding(work)
  } else {
    work |>
      dplyr::group_by(!!!rlang::syms(processing_group_cols)) |>
      dplyr::group_modify(~ compute_padding(.x)) |>
      dplyr::ungroup()
  }

  padding_flags <- padding_flags |>
    dplyr::select(
      dplyr::all_of(c(
        "row_id",
        "pupil_artifact_padding_flag"
      ))
    )

  append_reason <- function(existing_reason, flag, new_reason) {
    existing_reason <- as.character(existing_reason)
    existing_reason[existing_reason == ""] <- NA_character_

    flag <- as.logical(flag)
    flag[is.na(flag)] <- FALSE

    out <- existing_reason

    needs_new <- flag & is.na(out)
    out[needs_new] <- new_reason

    needs_append <- flag &
      !is.na(out) &
      !grepl(new_reason, out, fixed = TRUE)

    out[needs_append] <- paste0(out[needs_append], ";", new_reason)

    out
  }

  work <- work |>
    dplyr::left_join(padding_flags, by = "row_id") |>
    dplyr::mutate(
      pupil_artifact_flag = .data$pupil_bad_sample_basic |
        .data$pupil_artifact_padding_flag
    )

  reason <- rep(NA_character_, nrow(work))

  reason <- append_reason(reason, work$pupil_flag_missing_source, "missing_pupil")
  reason <- append_reason(reason, work$pupil_flag_blink_source, "blink")
  reason <- append_reason(reason, work$pupil_flag_trackloss_source, "trackloss")
  reason <- append_reason(reason, work$pupil_flag_prior_invalid_source, "prior_pupil_invalid")
  reason <- append_reason(reason, work$pupil_artifact_nonfinite, "nonfinite_pupil")
  reason <- append_reason(reason, work$pupil_artifact_nonpositive, "nonpositive_pupil")
  reason <- append_reason(reason, work$pupil_physio_outlier, "physiologically_implausible_pupil")
  reason <- append_reason(reason, work$pupil_binocular_disagreement, "binocular_pupil_disagreement")
  reason <- append_reason(reason, work$pupil_speed_outlier, "pupil_speed_outlier")
  reason <- append_reason(reason, work$pupil_artifact_padding_flag, "artifact_padding")

  work$pupil_artifact_reason <- ifelse(
    is.na(reason),
    "valid",
    reason
  )

  work <- work |>
    dplyr::mutate(
      pupil_clean = dplyr::if_else(
        .data$pupil_artifact_flag,
        NA_real_,
        .data$pupil_candidate_value
      ),
      pupil_artifact_pupil_column = pupil_source,
      pupil_artifact_left_pupil_column = if (is.na(left_pupil_source)) {
        NA_character_
      } else {
        left_pupil_source
      },
      pupil_artifact_right_pupil_column = if (is.na(right_pupil_source)) {
        NA_character_
      } else {
        right_pupil_source
      },
      pupil_artifact_time_column = time_source,
      pupil_artifact_unit_column = if (is.na(pupil_unit_source)) {
        NA_character_
      } else {
        pupil_unit_source
      },
      pupil_artifact_blink_column = if (is.na(blink_source)) {
        NA_character_
      } else {
        blink_source
      },
      pupil_artifact_trackloss_column = if (is.na(trackloss_source)) {
        NA_character_
      } else {
        trackloss_source
      },
      pupil_artifact_missing_pupil_column = if (is.na(missing_pupil_source)) {
        NA_character_
      } else {
        missing_pupil_source
      },
      pupil_artifact_padding_pre_ms = blink_padding_pre_ms,
      pupil_artifact_padding_post_ms = blink_padding_post_ms,
      pupil_artifact_min_mm = pupil_min_mm,
      pupil_artifact_max_mm = pupil_max_mm,
      pupil_artifact_speed_mad_k = pupil_speed_mad_k,
      pupil_artifact_binocular_mad_k = binocular_mad_k,
      pupil_artifact_max_physio_outlier_prop = max_physio_outlier_prop
    ) |>
    dplyr::arrange(.data$row_id)

  output_cols <- c(
    "pupil_artifact_raw_value",
    "left_pupil_artifact_raw_value",
    "right_pupil_artifact_raw_value",
    "pupil_unit_text",
    "pupil_unit_is_mm",
    "pupil_artifact_nonfinite",
    "pupil_artifact_nonpositive",
    "pupil_physio_outlier",
    "pupil_physio_outlier_candidate",
    "pupil_physio_candidate_prop",
    "pupil_physio_rule_suppressed",
    "pupil_lr_absdiff",
    "pupil_binocular_disagreement_threshold",
    "pupil_binocular_disagreement",
    "pupil_speed",
    "pupil_speed_abs",
    "pupil_speed_threshold",
    "pupil_speed_outlier",
    "pupil_flag_missing_source",
    "pupil_flag_blink_source",
    "pupil_flag_trackloss_source",
    "pupil_flag_prior_invalid_source",
    "pupil_bad_sample_basic",
    "pupil_artifact_padding_flag",
    "pupil_artifact_flag",
    "pupil_artifact_reason",
    "pupil_clean",
    "pupil_artifact_pupil_column",
    "pupil_artifact_left_pupil_column",
    "pupil_artifact_right_pupil_column",
    "pupil_artifact_time_column",
    "pupil_artifact_unit_column",
    "pupil_artifact_blink_column",
    "pupil_artifact_trackloss_column",
    "pupil_artifact_missing_pupil_column",
    "pupil_artifact_padding_pre_ms",
    "pupil_artifact_padding_post_ms",
    "pupil_artifact_min_mm",
    "pupil_artifact_max_mm",
    "pupil_artifact_speed_mad_k",
    "pupil_artifact_binocular_mad_k",
    "pupil_artifact_max_physio_outlier_prop"
  )

  output_cols <- unique(output_cols)

  missing_output_cols <- setdiff(output_cols, names(work))

  if (length(missing_output_cols) > 0) {
    for (col in missing_output_cols) {
      work[[col]] <- NA
    }
  }

  original <- tibble::as_tibble(data)
  original[intersect(names(original), output_cols)] <- NULL

  dplyr::bind_cols(
    original,
    work |>
      dplyr::select(dplyr::all_of(output_cols))
  )
}
