#' Audit Gazepoint experimental design balance
#'
#' Create a publication-level audit of observed design balance across subjects,
#' conditions, and optional stimulus/trial identifiers.
#'
#' @param data A data frame containing trial-level, window-level, or sample-level
#'   Gazepoint-derived data.
#' @param subject_col Subject/participant identifier column.
#' @param condition_col Experimental condition column.
#' @param unit_cols Optional columns defining the repeated unit to count within
#'   each subject and condition, such as media, trial, block, or window.
#' @param expected_conditions Optional character vector of expected condition
#'   labels.
#' @param min_units_per_condition Minimum number of observed units expected per
#'   subject-condition cell.
#' @param max_condition_ratio Maximum allowed ratio between a subject's largest
#'   and smallest non-zero condition counts.
#' @param require_all_conditions_per_subject Logical. If `TRUE`, flag subjects
#'   who do not have all expected or observed conditions.
#'
#' @return A list with class `gp3_design_balance_audit` containing overview,
#'   subject_summary, condition_summary, cell_summary, imbalance_summary,
#'   flagged_cells, and settings tables.
#' @export
audit_gazepoint_design_balance <- function(
    data,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("media_id", "trial_global"),
    expected_conditions = NULL,
    min_units_per_condition = 1L,
    max_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_design_balance_standardise_aliases(data)

  subject_col <- .gp3_design_balance_resolve_col(
    subject_col,
    names(data),
    "subject_col"
  )

  condition_col <- .gp3_design_balance_resolve_col(
    condition_col,
    names(data),
    "condition_col"
  )

  unit_cols <- .gp3_design_balance_standardise_cols(unit_cols)
  unit_cols <- unit_cols[unit_cols %in% names(data)]

  .gp3_design_balance_check_positive_numeric(
    min_units_per_condition,
    "min_units_per_condition"
  )

  .gp3_design_balance_check_positive_numeric(
    max_condition_ratio,
    "max_condition_ratio"
  )

  .gp3_design_balance_check_logical_scalar(
    require_all_conditions_per_subject,
    "require_all_conditions_per_subject"
  )

  if (!is.null(expected_conditions)) {
    .gp3_design_balance_check_character_vector(
      expected_conditions,
      "expected_conditions"
    )
  }

  observed_conditions <- sort(unique(as.character(data[[condition_col]])))
  observed_conditions <- observed_conditions[
    !is.na(observed_conditions) & nzchar(observed_conditions)
  ]

  if (length(observed_conditions) == 0L) {
    stop("`condition_col` must contain at least one non-missing condition.", call. = FALSE)
  }

  conditions <- if (!is.null(expected_conditions)) {
    expected_conditions
  } else {
    observed_conditions
  }

  unit_data <- .gp3_design_balance_create_unit_data(
    data = data,
    subject_col = subject_col,
    condition_col = condition_col,
    unit_cols = unit_cols
  )

  cell_summary <- .gp3_design_balance_create_cell_summary(
    unit_data = unit_data,
    subject_col = subject_col,
    condition_col = condition_col,
    conditions = conditions,
    min_units_per_condition = min_units_per_condition
  )

  subject_summary <- .gp3_design_balance_create_subject_summary(
    cell_summary = cell_summary,
    subject_col = subject_col,
    conditions = conditions,
    max_condition_ratio = max_condition_ratio,
    require_all_conditions_per_subject = require_all_conditions_per_subject
  )

  condition_summary <- .gp3_design_balance_create_condition_summary(
    cell_summary = cell_summary,
    condition_col = condition_col
  )

  imbalance_summary <- .gp3_design_balance_create_imbalance_summary(
    subject_summary = subject_summary
  )

  flagged_cells <- cell_summary[
    cell_summary$design_cell_status != "ok",
    ,
    drop = FALSE
  ]

  n_flagged_subjects <- sum(
    subject_summary$design_balance_status != "ok",
    na.rm = TRUE
  )

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_units = nrow(unit_data),
    n_subjects = length(unique(unit_data[[subject_col]])),
    n_conditions = length(conditions),
    n_flagged_subjects = n_flagged_subjects,
    n_flagged_cells = nrow(flagged_cells),
    design_balance_status = dplyr::case_when(
      n_flagged_subjects == 0L && nrow(flagged_cells) == 0L ~ "ok",
      TRUE ~ "review"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "subject_col",
      "condition_col",
      "unit_cols",
      "expected_conditions",
      "min_units_per_condition",
      "max_condition_ratio",
      "require_all_conditions_per_subject"
    ),
    value = c(
      subject_col,
      condition_col,
      paste(unit_cols, collapse = ", "),
      .gp3_design_balance_collapse_nullable(expected_conditions),
      as.character(min_units_per_condition),
      as.character(max_condition_ratio),
      as.character(require_all_conditions_per_subject)
    )
  )

  out <- list(
    overview = overview,
    subject_summary = subject_summary,
    condition_summary = condition_summary,
    cell_summary = cell_summary,
    imbalance_summary = imbalance_summary,
    flagged_cells = flagged_cells,
    settings = settings
  )

  class(out) <- c("gp3_design_balance_audit", "list")

  out
}

.gp3_design_balance_create_unit_data <- function(
    data,
    subject_col,
    condition_col,
    unit_cols
) {
  keep_cols <- unique(c(subject_col, condition_col, unit_cols))
  unit_data <- data[, keep_cols, drop = FALSE]

  unit_data[[subject_col]] <- as.character(unit_data[[subject_col]])
  unit_data[[condition_col]] <- as.character(unit_data[[condition_col]])

  unit_data <- unit_data[
    !is.na(unit_data[[subject_col]]) &
      nzchar(unit_data[[subject_col]]) &
      !is.na(unit_data[[condition_col]]) &
      nzchar(unit_data[[condition_col]]),
    ,
    drop = FALSE
  ]

  if (nrow(unit_data) == 0L) {
    stop(
      "`subject_col` and `condition_col` must define at least one usable row.",
      call. = FALSE
    )
  }

  unique(unit_data)
}

.gp3_design_balance_create_cell_summary <- function(
    unit_data,
    subject_col,
    condition_col,
    conditions,
    min_units_per_condition
) {
  subjects <- sort(unique(as.character(unit_data[[subject_col]])))

  grid <- expand.grid(
    subject_value = subjects,
    condition_value = conditions,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  counts <- stats::aggregate(
    rep(1L, nrow(unit_data)),
    by = list(
      subject_value = unit_data[[subject_col]],
      condition_value = unit_data[[condition_col]]
    ),
    FUN = length
  )

  names(counts)[names(counts) == "x"] <- "n_units"

  out <- merge(
    grid,
    counts,
    by = c("subject_value", "condition_value"),
    all.x = TRUE,
    sort = FALSE
  )

  out$n_units[is.na(out$n_units)] <- 0L
  out$n_units <- as.integer(out$n_units)

  out$design_cell_status <- dplyr::case_when(
    out$n_units == 0L ~ "missing_condition",
    out$n_units < min_units_per_condition ~ "too_few_units",
    TRUE ~ "ok"
  )

  names(out)[names(out) == "subject_value"] <- subject_col
  names(out)[names(out) == "condition_value"] <- condition_col

  tibble::as_tibble(out)
}

.gp3_design_balance_create_subject_summary <- function(
    cell_summary,
    subject_col,
    conditions,
    max_condition_ratio,
    require_all_conditions_per_subject
) {
  split_idx <- split(
    seq_len(nrow(cell_summary)),
    cell_summary[[subject_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- cell_summary[idx, , drop = FALSE]
    counts <- d$n_units
    nonzero <- counts[counts > 0L]

    n_missing_conditions <- sum(counts == 0L)
    n_low_conditions <- sum(d$design_cell_status == "too_few_units")

    condition_ratio <- if (length(nonzero) <= 1L) {
      NA_real_
    } else {
      max(nonzero) / min(nonzero)
    }

    has_ratio_imbalance <- !is.na(condition_ratio) &&
      condition_ratio > max_condition_ratio

    status <- .gp3_design_balance_subject_status(
      n_missing_conditions = n_missing_conditions,
      n_low_conditions = n_low_conditions,
      has_ratio_imbalance = has_ratio_imbalance,
      require_all_conditions_per_subject = require_all_conditions_per_subject
    )

    tibble::tibble(
      subject = as.character(d[[subject_col]][[1]]),
      n_conditions_expected = length(conditions),
      n_conditions_observed = sum(counts > 0L),
      min_units_per_condition_observed = ifelse(
        length(nonzero) > 0L,
        min(nonzero),
        NA_integer_
      ),
      max_units_per_condition_observed = ifelse(
        length(nonzero) > 0L,
        max(nonzero),
        NA_integer_
      ),
      condition_count_ratio = condition_ratio,
      n_missing_conditions = n_missing_conditions,
      n_low_count_conditions = n_low_conditions,
      design_balance_status = status
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "subject"] <- subject_col

  out
}

.gp3_design_balance_subject_status <- function(
    n_missing_conditions,
    n_low_conditions,
    has_ratio_imbalance,
    require_all_conditions_per_subject
) {
  if (isTRUE(require_all_conditions_per_subject) && n_missing_conditions > 0L) {
    return("missing_condition")
  }

  if (n_low_conditions > 0L) {
    return("too_few_units")
  }

  if (isTRUE(has_ratio_imbalance)) {
    return("condition_count_imbalance")
  }

  "ok"
}

.gp3_design_balance_create_condition_summary <- function(
    cell_summary,
    condition_col
) {
  split_idx <- split(
    seq_len(nrow(cell_summary)),
    cell_summary[[condition_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- cell_summary[idx, , drop = FALSE]
    nonzero <- d$n_units[d$n_units > 0L]

    tibble::tibble(
      condition = as.character(d[[condition_col]][[1]]),
      n_subject_cells = nrow(d),
      n_subjects_with_condition = sum(d$n_units > 0L),
      n_subjects_missing_condition = sum(d$n_units == 0L),
      total_units = sum(d$n_units),
      min_units_per_subject = ifelse(length(nonzero) > 0L, min(nonzero), NA_integer_),
      max_units_per_subject = ifelse(length(nonzero) > 0L, max(nonzero), NA_integer_),
      mean_units_per_subject = mean(d$n_units),
      condition_summary_status = ifelse(
        sum(d$n_units == 0L) == 0L,
        "ok",
        "missing_for_some_subjects"
      )
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "condition"] <- condition_col

  out
}

.gp3_design_balance_create_imbalance_summary <- function(subject_summary) {
  tibble::tibble(
    design_balance_status = sort(unique(subject_summary$design_balance_status)),
    n_subjects = as.integer(tabulate(
      match(
        subject_summary$design_balance_status,
        sort(unique(subject_summary$design_balance_status))
      )
    ))
  )
}

.gp3_design_balance_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_design_balance_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols[cols == "USER_FILE"] <- "subject"
  cols
}

.gp3_design_balance_resolve_col <- function(col, names_data, arg) {
  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  if (col == "MEDIA_ID" && "media_id" %in% names_data) {
    return("media_id")
  }

  if (col == "USER_FILE" && "subject" %in% names_data) {
    return("subject")
  }

  if (!col %in% names_data) {
    stop("`", arg, "` must be present in `data`.", call. = FALSE)
  }

  col
}

.gp3_design_balance_check_positive_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_design_balance_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_design_balance_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_design_balance_collapse_nullable <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
