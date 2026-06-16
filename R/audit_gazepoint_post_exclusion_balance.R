#' Audit post-exclusion condition balance
#'
#' Create a publication-level audit of whether the retained analysis sample
#' remains balanced across subjects and experimental conditions after
#' exclusions.
#'
#' @param data A data frame containing row-, sample-, trial-, or unit-level data.
#' @param subject_col Subject/participant identifier column.
#' @param condition_col Experimental condition column.
#' @param unit_cols Optional columns defining the analysis unit, such as media,
#'   trial, block, or window.
#' @param retained_col Optional logical/numeric/character column indicating
#'   retained units.
#' @param include_col Optional logical/numeric/character inclusion column.
#' @param exclude_col Optional logical/numeric/character exclusion column.
#' @param status_col Optional status column used to infer retained/excluded
#'   units.
#' @param expected_conditions Optional character vector of expected conditions.
#' @param included_values Character values in `status_col` treated as retained.
#' @param excluded_values Character values in `status_col` treated as excluded.
#' @param min_retained_units_per_condition Minimum retained units required per
#'   condition.
#' @param min_retained_units_per_subject_condition Minimum retained units
#'   required per subject-condition cell.
#' @param max_condition_count_ratio Maximum allowed ratio between condition-level
#'   retained counts.
#' @param max_subject_condition_ratio Maximum allowed within-subject retained
#'   condition-count ratio.
#' @param require_all_conditions_per_subject Logical. If `TRUE`, flag subjects
#'   missing retained units in one or more expected conditions.
#'
#' @return A list with class `gp3_post_exclusion_balance_audit` containing
#'   overview, unit_flow, cell_summary, condition_summary, subject_summary,
#'   flagged_cells, flagged_subjects, and settings tables.
#' @export
audit_gazepoint_post_exclusion_balance <- function(
    data,
    subject_col = "subject",
    condition_col = "condition",
    unit_cols = c("media_id", "trial_global"),
    retained_col = NULL,
    include_col = NULL,
    exclude_col = NULL,
    status_col = NULL,
    expected_conditions = NULL,
    included_values = c(
      "included", "include", "kept", "keep", "retained",
      "ok", "ready", "complete", "completed"
    ),
    excluded_values = c(
      "excluded", "exclude", "drop", "dropped", "removed",
      "fail", "failed", "not_ready", "review", "invalid"
    ),
    min_retained_units_per_condition = 1L,
    min_retained_units_per_subject_condition = 1L,
    max_condition_count_ratio = 2,
    max_subject_condition_ratio = 2,
    require_all_conditions_per_subject = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_post_exclusion_standardise_aliases(data)

  subject_col <- .gp3_post_exclusion_resolve_col(
    subject_col,
    names(data),
    "subject_col"
  )

  condition_col <- .gp3_post_exclusion_resolve_col(
    condition_col,
    names(data),
    "condition_col"
  )

  unit_cols <- .gp3_post_exclusion_standardise_cols(unit_cols)
  unit_cols <- unit_cols[unit_cols %in% names(data)]

  retained_col <- .gp3_post_exclusion_resolve_optional_col(
    retained_col,
    names(data),
    "retained_col"
  )

  include_col <- .gp3_post_exclusion_resolve_optional_col(
    include_col,
    names(data),
    "include_col"
  )

  exclude_col <- .gp3_post_exclusion_resolve_optional_col(
    exclude_col,
    names(data),
    "exclude_col"
  )

  status_col <- .gp3_post_exclusion_resolve_optional_col(
    status_col,
    names(data),
    "status_col"
  )

  if (!is.null(expected_conditions)) {
    .gp3_post_exclusion_check_character_vector(
      expected_conditions,
      "expected_conditions"
    )
  }

  .gp3_post_exclusion_check_character_vector(
    included_values,
    "included_values"
  )

  .gp3_post_exclusion_check_character_vector(
    excluded_values,
    "excluded_values"
  )

  .gp3_post_exclusion_check_positive_numeric(
    min_retained_units_per_condition,
    "min_retained_units_per_condition"
  )

  .gp3_post_exclusion_check_positive_numeric(
    min_retained_units_per_subject_condition,
    "min_retained_units_per_subject_condition"
  )

  .gp3_post_exclusion_check_positive_numeric(
    max_condition_count_ratio,
    "max_condition_count_ratio"
  )

  .gp3_post_exclusion_check_positive_numeric(
    max_subject_condition_ratio,
    "max_subject_condition_ratio"
  )

  .gp3_post_exclusion_check_logical_scalar(
    require_all_conditions_per_subject,
    "require_all_conditions_per_subject"
  )

  data[[subject_col]] <- as.character(data[[subject_col]])
  data[[condition_col]] <- as.character(data[[condition_col]])

  data <- data[
    !is.na(data[[subject_col]]) &
      nzchar(data[[subject_col]]) &
      !is.na(data[[condition_col]]) &
      nzchar(data[[condition_col]]),
    ,
    drop = FALSE
  ]

  if (nrow(data) == 0L) {
    stop(
      "`subject_col` and `condition_col` must define at least one usable row.",
      call. = FALSE
    )
  }

  observed_conditions <- sort(unique(data[[condition_col]]))

  conditions <- if (!is.null(expected_conditions)) {
    expected_conditions
  } else {
    observed_conditions
  }

  row_flags <- .gp3_post_exclusion_create_row_flags(
    data = data,
    retained_col = retained_col,
    include_col = include_col,
    exclude_col = exclude_col,
    status_col = status_col,
    included_values = included_values,
    excluded_values = excluded_values
  )

  unit_flow <- .gp3_post_exclusion_create_unit_flow(
    data = data,
    row_flags = row_flags,
    subject_col = subject_col,
    condition_col = condition_col,
    unit_cols = unit_cols
  )

  cell_summary <- .gp3_post_exclusion_create_cell_summary(
    unit_flow = unit_flow,
    subject_col = subject_col,
    condition_col = condition_col,
    conditions = conditions,
    min_retained_units_per_subject_condition =
      min_retained_units_per_subject_condition
  )

  condition_summary <- .gp3_post_exclusion_create_condition_summary(
    cell_summary = cell_summary,
    condition_col = condition_col,
    min_retained_units_per_condition = min_retained_units_per_condition
  )

  subject_summary <- .gp3_post_exclusion_create_subject_summary(
    cell_summary = cell_summary,
    subject_col = subject_col,
    conditions = conditions,
    max_subject_condition_ratio = max_subject_condition_ratio,
    require_all_conditions_per_subject = require_all_conditions_per_subject
  )

  flagged_cells <- cell_summary[
    cell_summary$post_exclusion_cell_status != "ok",
    ,
    drop = FALSE
  ]

  flagged_subjects <- subject_summary[
    subject_summary$post_exclusion_subject_status != "ok",
    ,
    drop = FALSE
  ]

  n_retained_units <- sum(unit_flow$retained, na.rm = TRUE)
  n_excluded_units <- sum(!unit_flow$retained, na.rm = TRUE)
  n_problem_units <- sum(
    unit_flow$post_exclusion_unit_status %in%
      c("conflicting_flags", "unclear_status"),
    na.rm = TRUE
  )

  n_flagged_conditions <- sum(
    condition_summary$post_exclusion_condition_status != "ok",
    na.rm = TRUE
  )

  condition_count_ratio <- .gp3_post_exclusion_condition_count_ratio(
    condition_summary
  )

  condition_ratio_status <- ifelse(
    !is.na(condition_count_ratio) &&
      (is.infinite(condition_count_ratio) ||
         condition_count_ratio > max_condition_count_ratio),
    "condition_count_imbalance",
    "ok"
  )

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_units = nrow(unit_flow),
    n_retained_units = n_retained_units,
    n_excluded_units = n_excluded_units,
    retained_prop = n_retained_units / nrow(unit_flow),
    n_subjects = length(unique(unit_flow[[subject_col]])),
    n_conditions = length(conditions),
    n_problem_units = n_problem_units,
    n_flagged_cells = nrow(flagged_cells),
    n_flagged_subjects = nrow(flagged_subjects),
    n_flagged_conditions = n_flagged_conditions,
    condition_count_ratio = condition_count_ratio,
    condition_ratio_status = condition_ratio_status,
    post_exclusion_balance_status = dplyr::case_when(
      n_problem_units > 0L ~ "review",
      nrow(flagged_cells) > 0L ~ "review",
      nrow(flagged_subjects) > 0L ~ "review",
      n_flagged_conditions > 0L ~ "review",
      condition_ratio_status != "ok" ~ "review",
      TRUE ~ "ok"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "subject_col",
      "condition_col",
      "unit_cols",
      "retained_col",
      "include_col",
      "exclude_col",
      "status_col",
      "expected_conditions",
      "included_values",
      "excluded_values",
      "min_retained_units_per_condition",
      "min_retained_units_per_subject_condition",
      "max_condition_count_ratio",
      "max_subject_condition_ratio",
      "require_all_conditions_per_subject"
    ),
    value = c(
      subject_col,
      condition_col,
      paste(unit_cols, collapse = ", "),
      .gp3_post_exclusion_collapse_nullable(retained_col),
      .gp3_post_exclusion_collapse_nullable(include_col),
      .gp3_post_exclusion_collapse_nullable(exclude_col),
      .gp3_post_exclusion_collapse_nullable(status_col),
      .gp3_post_exclusion_collapse_nullable(expected_conditions),
      paste(included_values, collapse = ", "),
      paste(excluded_values, collapse = ", "),
      as.character(min_retained_units_per_condition),
      as.character(min_retained_units_per_subject_condition),
      as.character(max_condition_count_ratio),
      as.character(max_subject_condition_ratio),
      as.character(require_all_conditions_per_subject)
    )
  )

  out <- list(
    overview = overview,
    unit_flow = unit_flow,
    cell_summary = cell_summary,
    condition_summary = condition_summary,
    subject_summary = subject_summary,
    flagged_cells = flagged_cells,
    flagged_subjects = flagged_subjects,
    settings = settings
  )

  class(out) <- c("gp3_post_exclusion_balance_audit", "list")

  out
}

.gp3_post_exclusion_create_row_flags <- function(
    data,
    retained_col,
    include_col,
    exclude_col,
    status_col,
    included_values,
    excluded_values
) {
  no_flag_cols <- is.null(retained_col) &&
    is.null(include_col) &&
    is.null(exclude_col) &&
    is.null(status_col)

  include_flag <- if (isTRUE(no_flag_cols)) {
    rep(TRUE, nrow(data))
  } else {
    rep(NA, nrow(data))
  }

  if (!is.null(retained_col)) {
    include_flag <- .gp3_post_exclusion_to_logical(
      data[[retained_col]],
      "retained_col"
    )
  }

  if (!is.null(include_col)) {
    include_flag <- .gp3_post_exclusion_to_logical(
      data[[include_col]],
      "include_col"
    )
  }

  if (!is.null(exclude_col)) {
    exclude_flag <- .gp3_post_exclusion_to_logical(
      data[[exclude_col]],
      "exclude_col"
    )

    include_flag[!is.na(exclude_flag)] <- !exclude_flag[!is.na(exclude_flag)]
  }

  if (!is.null(status_col)) {
    status_values <- tolower(as.character(data[[status_col]]))
    included_lookup <- tolower(included_values)
    excluded_lookup <- tolower(excluded_values)

    status_flag <- rep(NA, nrow(data))
    status_flag[status_values %in% included_lookup] <- TRUE
    status_flag[status_values %in% excluded_lookup] <- FALSE

    include_flag[!is.na(status_flag)] <- status_flag[!is.na(status_flag)]
  }

  tibble::tibble(.gp3_row_retained = include_flag)
}

.gp3_post_exclusion_create_unit_flow <- function(
    data,
    row_flags,
    subject_col,
    condition_col,
    unit_cols
) {
  work <- data
  work$.gp3_row_retained <- row_flags$.gp3_row_retained

  id_cols <- unique(c(subject_col, condition_col, unit_cols))
  id_cols <- id_cols[!is.na(id_cols) & nzchar(id_cols)]

  split_key <- interaction(work[id_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(work)), split_key)

  rows <- vector("list", length(split_idx))

  for (i in seq_along(split_idx)) {
    idx <- split_idx[[i]]
    d <- work[idx, , drop = FALSE]
    flags <- d$.gp3_row_retained

    any_retained <- any(flags %in% TRUE, na.rm = TRUE)
    any_excluded <- any(flags %in% FALSE, na.rm = TRUE)
    all_unknown <- all(is.na(flags))

    unit_status <- .gp3_post_exclusion_unit_status(
      any_retained = any_retained,
      any_excluded = any_excluded,
      all_unknown = all_unknown
    )

    retained <- identical(unit_status, "retained")

    id_row <- d[1, id_cols, drop = FALSE]

    rows[[i]] <- cbind(
      tibble::as_tibble(id_row),
      tibble::tibble(
        n_source_rows = nrow(d),
        retained = retained,
        post_exclusion_unit_status = unit_status
      )
    )
  }

  dplyr::bind_rows(rows)
}

.gp3_post_exclusion_unit_status <- function(
    any_retained,
    any_excluded,
    all_unknown
) {
  if (isTRUE(all_unknown)) {
    return("unclear_status")
  }

  if (isTRUE(any_retained) && isTRUE(any_excluded)) {
    return("conflicting_flags")
  }

  if (isTRUE(any_excluded)) {
    return("excluded")
  }

  if (isTRUE(any_retained)) {
    return("retained")
  }

  "unclear_status"
}

.gp3_post_exclusion_create_cell_summary <- function(
    unit_flow,
    subject_col,
    condition_col,
    conditions,
    min_retained_units_per_subject_condition
) {
  subjects <- sort(unique(as.character(unit_flow[[subject_col]])))

  grid <- expand.grid(
    subject_value = subjects,
    condition_value = conditions,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  total_counts <- stats::aggregate(
    rep(1L, nrow(unit_flow)),
    by = list(
      subject_value = unit_flow[[subject_col]],
      condition_value = unit_flow[[condition_col]]
    ),
    FUN = length
  )

  names(total_counts)[names(total_counts) == "x"] <- "n_total_units"

  retained_flow <- unit_flow[unit_flow$retained %in% TRUE, , drop = FALSE]

  if (nrow(retained_flow) > 0L) {
    retained_counts <- stats::aggregate(
      rep(1L, nrow(retained_flow)),
      by = list(
        subject_value = retained_flow[[subject_col]],
        condition_value = retained_flow[[condition_col]]
      ),
      FUN = length
    )

    names(retained_counts)[names(retained_counts) == "x"] <- "n_retained_units"
  } else {
    retained_counts <- data.frame(
      subject_value = character(),
      condition_value = character(),
      n_retained_units = integer(),
      stringsAsFactors = FALSE
    )
  }

  out <- merge(
    grid,
    total_counts,
    by = c("subject_value", "condition_value"),
    all.x = TRUE,
    sort = FALSE
  )

  out <- merge(
    out,
    retained_counts,
    by = c("subject_value", "condition_value"),
    all.x = TRUE,
    sort = FALSE
  )

  out$n_total_units[is.na(out$n_total_units)] <- 0L
  out$n_retained_units[is.na(out$n_retained_units)] <- 0L

  out$n_total_units <- as.integer(out$n_total_units)
  out$n_retained_units <- as.integer(out$n_retained_units)

  out$retained_prop <- ifelse(
    out$n_total_units > 0L,
    out$n_retained_units / out$n_total_units,
    NA_real_
  )

  out$post_exclusion_cell_status <- dplyr::case_when(
    out$n_retained_units == 0L ~ "missing_retained_condition",
    out$n_retained_units < min_retained_units_per_subject_condition ~
      "too_few_retained_units",
    TRUE ~ "ok"
  )

  names(out)[names(out) == "subject_value"] <- subject_col
  names(out)[names(out) == "condition_value"] <- condition_col

  tibble::as_tibble(out)
}

.gp3_post_exclusion_create_condition_summary <- function(
    cell_summary,
    condition_col,
    min_retained_units_per_condition
) {
  split_idx <- split(
    seq_len(nrow(cell_summary)),
    cell_summary[[condition_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- cell_summary[idx, , drop = FALSE]

    total_retained <- sum(d$n_retained_units, na.rm = TRUE)
    total_units <- sum(d$n_total_units, na.rm = TRUE)

    tibble::tibble(
      condition = as.character(d[[condition_col]][[1]]),
      n_subject_cells = nrow(d),
      n_subjects_with_retained = sum(d$n_retained_units > 0L),
      n_subjects_missing_retained = sum(d$n_retained_units == 0L),
      total_units = total_units,
      total_retained_units = total_retained,
      retained_prop = ifelse(total_units > 0L, total_retained / total_units, NA_real_),
      min_retained_units_per_subject = min(d$n_retained_units, na.rm = TRUE),
      max_retained_units_per_subject = max(d$n_retained_units, na.rm = TRUE),
      mean_retained_units_per_subject = mean(d$n_retained_units, na.rm = TRUE),
      post_exclusion_condition_status = ifelse(
        total_retained < min_retained_units_per_condition,
        "too_few_retained_units",
        "ok"
      )
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "condition"] <- condition_col

  out
}

.gp3_post_exclusion_create_subject_summary <- function(
    cell_summary,
    subject_col,
    conditions,
    max_subject_condition_ratio,
    require_all_conditions_per_subject
) {
  split_idx <- split(
    seq_len(nrow(cell_summary)),
    cell_summary[[subject_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- cell_summary[idx, , drop = FALSE]
    counts <- d$n_retained_units
    nonzero <- counts[counts > 0L]

    n_missing <- sum(counts == 0L)
    n_low <- sum(d$post_exclusion_cell_status == "too_few_retained_units")

    ratio <- if (length(nonzero) <= 1L) {
      NA_real_
    } else {
      max(nonzero) / min(nonzero)
    }

    has_ratio_imbalance <- !is.na(ratio) &&
      ratio > max_subject_condition_ratio

    status <- .gp3_post_exclusion_subject_status(
      n_missing = n_missing,
      n_low = n_low,
      has_ratio_imbalance = has_ratio_imbalance,
      require_all_conditions_per_subject = require_all_conditions_per_subject
    )

    tibble::tibble(
      subject = as.character(d[[subject_col]][[1]]),
      n_conditions_expected = length(conditions),
      n_conditions_with_retained = sum(counts > 0L),
      total_retained_units = sum(counts, na.rm = TRUE),
      min_retained_units_per_condition = ifelse(
        length(nonzero) > 0L,
        min(nonzero),
        NA_integer_
      ),
      max_retained_units_per_condition = ifelse(
        length(nonzero) > 0L,
        max(nonzero),
        NA_integer_
      ),
      retained_condition_ratio = ratio,
      n_missing_retained_conditions = n_missing,
      n_low_retained_conditions = n_low,
      post_exclusion_subject_status = status
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "subject"] <- subject_col

  out
}

.gp3_post_exclusion_subject_status <- function(
    n_missing,
    n_low,
    has_ratio_imbalance,
    require_all_conditions_per_subject
) {
  if (isTRUE(require_all_conditions_per_subject) && n_missing > 0L) {
    return("missing_retained_condition")
  }

  if (n_low > 0L) {
    return("too_few_retained_units")
  }

  if (isTRUE(has_ratio_imbalance)) {
    return("retained_condition_imbalance")
  }

  "ok"
}

.gp3_post_exclusion_condition_count_ratio <- function(condition_summary) {
  counts <- condition_summary$total_retained_units
  counts <- counts[is.finite(counts)]

  if (length(counts) <= 1L) {
    return(NA_real_)
  }

  if (all(counts == 0L)) {
    return(NA_real_)
  }

  if (any(counts == 0L) && any(counts > 0L)) {
    return(Inf)
  }

  nonzero <- counts[counts > 0L]

  if (length(nonzero) <= 1L) {
    return(NA_real_)
  }

  max(nonzero) / min(nonzero)
}

.gp3_post_exclusion_to_logical <- function(x, arg) {
  if (is.logical(x)) {
    return(x)
  }

  if (is.numeric(x)) {
    out <- rep(NA, length(x))
    out[x == 1] <- TRUE
    out[x == 0] <- FALSE

    if (any(is.na(out) & !is.na(x))) {
      stop(
        "`",
        arg,
        "` numeric values must be 0, 1, or NA.",
        call. = FALSE
      )
    }

    return(out)
  }

  if (is.character(x) || is.factor(x)) {
    values <- tolower(as.character(x))
    out <- rep(NA, length(values))

    true_values <- c(
      "true", "t", "yes", "y", "1",
      "included", "include", "keep", "kept",
      "retained", "ok", "ready"
    )

    false_values <- c(
      "false", "f", "no", "n", "0",
      "excluded", "exclude", "drop", "dropped",
      "removed", "fail", "failed", "not_ready",
      "review"
    )

    out[values %in% true_values] <- TRUE
    out[values %in% false_values] <- FALSE

    unknown <- is.na(out) & !is.na(values) & nzchar(values)

    if (any(unknown)) {
      stop(
        "`",
        arg,
        "` character values must be interpretable as retained/excluded flags.",
        call. = FALSE
      )
    }

    return(out)
  }

  stop(
    "`",
    arg,
    "` must be logical, numeric 0/1, or character retained/excluded values.",
    call. = FALSE
  )
}

.gp3_post_exclusion_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_post_exclusion_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols[cols == "USER_FILE"] <- "subject"
  cols
}

.gp3_post_exclusion_resolve_col <- function(col, names_data, arg) {
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

.gp3_post_exclusion_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_post_exclusion_resolve_col(col, names_data, arg)
}

.gp3_post_exclusion_check_positive_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_post_exclusion_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_post_exclusion_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_post_exclusion_collapse_nullable <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
