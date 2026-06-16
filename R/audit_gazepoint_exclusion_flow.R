#' Audit Gazepoint exclusion and retention flow
#'
#' Create a publication-level audit of retained and excluded analysis units.
#'
#' @param data A data frame containing row-, trial-, window-, or unit-level data.
#' @param subject_col Subject/participant identifier column.
#' @param condition_col Optional condition column.
#' @param unit_cols Optional columns defining the analysis unit, such as media,
#'   trial, block, or window.
#' @param include_col Optional logical/numeric/character column indicating rows
#'   or units retained for analysis.
#' @param exclude_col Optional logical/numeric/character column indicating rows
#'   or units excluded from analysis.
#' @param status_col Optional status column used to infer inclusion or exclusion.
#' @param reason_col Optional exclusion-reason column.
#' @param included_values Character values in `status_col` treated as retained.
#' @param excluded_values Character values in `status_col` treated as excluded.
#' @param min_retained_prop Minimum acceptable retained-unit proportion.
#' @param max_condition_exclusion_ratio Maximum allowed ratio between condition
#'   exclusion proportions.
#'
#' @return A list with class `gp3_exclusion_flow_audit` containing overview,
#'   unit_flow, reason_summary, condition_summary, subject_summary,
#'   flagged_units, and settings tables.
#' @export
audit_gazepoint_exclusion_flow <- function(
    data,
    subject_col = "subject",
    condition_col = NULL,
    unit_cols = c("media_id", "trial_global"),
    include_col = NULL,
    exclude_col = NULL,
    status_col = NULL,
    reason_col = NULL,
    included_values = c(
      "included", "include", "kept", "keep", "retained",
      "ok", "ready", "complete", "completed"
    ),
    excluded_values = c(
      "excluded", "exclude", "drop", "dropped", "removed",
      "fail", "failed", "not_ready", "review", "invalid"
    ),
    min_retained_prop = 0.70,
    max_condition_exclusion_ratio = 2
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  data <- .gp3_exclusion_flow_standardise_aliases(data)

  subject_col <- .gp3_exclusion_flow_resolve_col(
    subject_col,
    names(data),
    "subject_col"
  )

  condition_col <- .gp3_exclusion_flow_resolve_optional_col(
    condition_col,
    names(data),
    "condition_col"
  )

  unit_cols <- .gp3_exclusion_flow_standardise_cols(unit_cols)
  unit_cols <- unit_cols[unit_cols %in% names(data)]

  include_col <- .gp3_exclusion_flow_resolve_optional_col(
    include_col,
    names(data),
    "include_col"
  )

  exclude_col <- .gp3_exclusion_flow_resolve_optional_col(
    exclude_col,
    names(data),
    "exclude_col"
  )

  status_col <- .gp3_exclusion_flow_resolve_optional_col(
    status_col,
    names(data),
    "status_col"
  )

  reason_col <- .gp3_exclusion_flow_resolve_optional_col(
    reason_col,
    names(data),
    "reason_col"
  )

  if (is.null(include_col) && is.null(exclude_col) && is.null(status_col)) {
    stop(
      "One of `include_col`, `exclude_col`, or `status_col` must be supplied.",
      call. = FALSE
    )
  }

  .gp3_exclusion_flow_check_character_vector(
    included_values,
    "included_values"
  )

  .gp3_exclusion_flow_check_character_vector(
    excluded_values,
    "excluded_values"
  )

  .gp3_exclusion_flow_check_positive_numeric(
    min_retained_prop,
    "min_retained_prop"
  )

  if (min_retained_prop > 1) {
    stop("`min_retained_prop` must be between 0 and 1.", call. = FALSE)
  }

  .gp3_exclusion_flow_check_positive_numeric(
    max_condition_exclusion_ratio,
    "max_condition_exclusion_ratio"
  )

  row_flags <- .gp3_exclusion_flow_create_row_flags(
    data = data,
    include_col = include_col,
    exclude_col = exclude_col,
    status_col = status_col,
    reason_col = reason_col,
    included_values = included_values,
    excluded_values = excluded_values
  )

  unit_flow <- .gp3_exclusion_flow_create_unit_flow(
    data = data,
    row_flags = row_flags,
    subject_col = subject_col,
    condition_col = condition_col,
    unit_cols = unit_cols
  )

  reason_summary <- .gp3_exclusion_flow_create_reason_summary(unit_flow)

  condition_summary <- .gp3_exclusion_flow_create_condition_summary(
    unit_flow = unit_flow,
    condition_col = condition_col,
    min_retained_prop = min_retained_prop
  )

  subject_summary <- .gp3_exclusion_flow_create_subject_summary(
    unit_flow = unit_flow,
    subject_col = subject_col,
    min_retained_prop = min_retained_prop
  )

  flagged_units <- unit_flow[
    unit_flow$exclusion_flow_status != "retained",
    ,
    drop = FALSE
  ]

  condition_exclusion_ratio <- .gp3_exclusion_flow_condition_ratio(
    condition_summary
  )

  n_condition_imbalance <- ifelse(
    is.na(condition_exclusion_ratio),
    0L,
    as.integer(condition_exclusion_ratio > max_condition_exclusion_ratio)
  )

  n_retained_units <- sum(unit_flow$retained, na.rm = TRUE)
  n_excluded_units <- sum(!unit_flow$retained, na.rm = TRUE)
  retained_prop <- n_retained_units / nrow(unit_flow)

  overview <- tibble::tibble(
    n_rows = nrow(data),
    n_units = nrow(unit_flow),
    n_subjects = length(unique(unit_flow[[subject_col]])),
    n_retained_units = n_retained_units,
    n_excluded_units = n_excluded_units,
    retained_prop = retained_prop,
    excluded_prop = 1 - retained_prop,
    n_flagged_units = nrow(flagged_units),
    n_exclusion_reasons = nrow(reason_summary),
    condition_exclusion_ratio = condition_exclusion_ratio,
    exclusion_flow_status = dplyr::case_when(
      any(unit_flow$exclusion_flow_status == "conflicting_flags") ~ "review",
      any(unit_flow$exclusion_flow_status == "unclear_status") ~ "review",
      retained_prop < min_retained_prop ~ "review",
      n_condition_imbalance > 0L ~ "review",
      TRUE ~ "ok"
    )
  )

  settings <- tibble::tibble(
    setting = c(
      "subject_col",
      "condition_col",
      "unit_cols",
      "include_col",
      "exclude_col",
      "status_col",
      "reason_col",
      "included_values",
      "excluded_values",
      "min_retained_prop",
      "max_condition_exclusion_ratio"
    ),
    value = c(
      subject_col,
      .gp3_exclusion_flow_collapse_nullable(condition_col),
      paste(unit_cols, collapse = ", "),
      .gp3_exclusion_flow_collapse_nullable(include_col),
      .gp3_exclusion_flow_collapse_nullable(exclude_col),
      .gp3_exclusion_flow_collapse_nullable(status_col),
      .gp3_exclusion_flow_collapse_nullable(reason_col),
      paste(included_values, collapse = ", "),
      paste(excluded_values, collapse = ", "),
      as.character(min_retained_prop),
      as.character(max_condition_exclusion_ratio)
    )
  )

  out <- list(
    overview = overview,
    unit_flow = unit_flow,
    reason_summary = reason_summary,
    condition_summary = condition_summary,
    subject_summary = subject_summary,
    flagged_units = flagged_units,
    settings = settings
  )

  class(out) <- c("gp3_exclusion_flow_audit", "list")

  out
}

.gp3_exclusion_flow_create_row_flags <- function(
    data,
    include_col,
    exclude_col,
    status_col,
    reason_col,
    included_values,
    excluded_values
) {
  include_flag <- rep(NA, nrow(data))

  if (!is.null(include_col)) {
    include_flag <- .gp3_exclusion_flow_to_logical(
      data[[include_col]],
      "include_col"
    )
  }

  if (!is.null(exclude_col)) {
    exclude_flag <- .gp3_exclusion_flow_to_logical(
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

  reason <- rep(NA_character_, nrow(data))

  if (!is.null(reason_col)) {
    reason <- as.character(data[[reason_col]])
    reason[is.na(reason) | !nzchar(reason)] <- NA_character_
  }

  tibble::tibble(
    .gp3_row_included = include_flag,
    .gp3_exclusion_reason = reason
  )
}

.gp3_exclusion_flow_create_unit_flow <- function(
    data,
    row_flags,
    subject_col,
    condition_col,
    unit_cols
) {
  work <- data
  work$.gp3_row_included <- row_flags$.gp3_row_included
  work$.gp3_exclusion_reason <- row_flags$.gp3_exclusion_reason

  id_cols <- unique(c(subject_col, condition_col, unit_cols))
  id_cols <- id_cols[!is.na(id_cols) & nzchar(id_cols)]

  if (length(id_cols) == 0L) {
    work$.gp3_unit_id <- seq_len(nrow(work))
    id_cols <- ".gp3_unit_id"
  }

  split_key <- interaction(work[id_cols], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(work)), split_key)

  rows <- vector("list", length(split_idx))

  for (i in seq_along(split_idx)) {
    idx <- split_idx[[i]]
    d <- work[idx, , drop = FALSE]
    flags <- d$.gp3_row_included

    any_included <- any(flags %in% TRUE, na.rm = TRUE)
    any_excluded <- any(flags %in% FALSE, na.rm = TRUE)
    all_unknown <- all(is.na(flags))

    status <- .gp3_exclusion_flow_unit_status(
      any_included = any_included,
      any_excluded = any_excluded,
      all_unknown = all_unknown
    )

    retained <- identical(status, "retained")

    reasons <- d$.gp3_exclusion_reason
    reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
    reasons <- sort(unique(reasons))

    if (length(reasons) == 0L) {
      reasons <- .gp3_exclusion_flow_default_reason(status)
    }

    id_row <- d[1, id_cols, drop = FALSE]

    rows[[i]] <- cbind(
      tibble::as_tibble(id_row),
      tibble::tibble(
        n_source_rows = nrow(d),
        retained = retained,
        exclusion_reason = paste(reasons, collapse = "; "),
        exclusion_flow_status = status
      )
    )
  }

  out <- dplyr::bind_rows(rows)

  if (".gp3_unit_id" %in% names(out)) {
    out$.gp3_unit_id <- NULL
  }

  out
}

.gp3_exclusion_flow_unit_status <- function(
    any_included,
    any_excluded,
    all_unknown
) {
  if (isTRUE(all_unknown)) {
    return("unclear_status")
  }

  if (isTRUE(any_included) && isTRUE(any_excluded)) {
    return("conflicting_flags")
  }

  if (isTRUE(any_excluded)) {
    return("excluded")
  }

  if (isTRUE(any_included)) {
    return("retained")
  }

  "unclear_status"
}

.gp3_exclusion_flow_default_reason <- function(status) {
  if (identical(status, "retained")) {
    return("retained")
  }

  if (identical(status, "conflicting_flags")) {
    return("conflicting_flags")
  }

  if (identical(status, "unclear_status")) {
    return("unclear_status")
  }

  "excluded_unspecified"
}

.gp3_exclusion_flow_create_reason_summary <- function(unit_flow) {
  excluded <- unit_flow[
    unit_flow$exclusion_flow_status != "retained",
    ,
    drop = FALSE
  ]

  if (nrow(excluded) == 0L) {
    return(tibble::tibble(
      exclusion_reason = character(),
      n_units = integer(),
      reason_prop = numeric()
    ))
  }

  reason_values <- strsplit(excluded$exclusion_reason, "; ", fixed = TRUE)
  reason_values <- unlist(reason_values, use.names = FALSE)
  reason_values <- reason_values[!is.na(reason_values) & nzchar(reason_values)]

  tab <- as.data.frame(
    table(exclusion_reason = reason_values),
    stringsAsFactors = FALSE
  )

  tibble::tibble(
    exclusion_reason = as.character(tab$exclusion_reason),
    n_units = as.integer(tab$Freq),
    reason_prop = as.integer(tab$Freq) / nrow(excluded)
  )
}

.gp3_exclusion_flow_create_condition_summary <- function(
    unit_flow,
    condition_col,
    min_retained_prop
) {
  if (is.null(condition_col) || !condition_col %in% names(unit_flow)) {
    return(tibble::tibble(
      condition = character(),
      n_units = integer(),
      n_retained_units = integer(),
      n_excluded_units = integer(),
      retained_prop = numeric(),
      excluded_prop = numeric(),
      condition_exclusion_status = character()
    ))
  }

  split_idx <- split(
    seq_len(nrow(unit_flow)),
    unit_flow[[condition_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- unit_flow[idx, , drop = FALSE]
    n_retained <- sum(d$retained, na.rm = TRUE)
    retained_prop <- n_retained / nrow(d)

    tibble::tibble(
      condition = as.character(d[[condition_col]][[1]]),
      n_units = nrow(d),
      n_retained_units = n_retained,
      n_excluded_units = nrow(d) - n_retained,
      retained_prop = retained_prop,
      excluded_prop = 1 - retained_prop,
      condition_exclusion_status = ifelse(
        retained_prop < min_retained_prop,
        "low_retention",
        "ok"
      )
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "condition"] <- condition_col

  out
}

.gp3_exclusion_flow_create_subject_summary <- function(
    unit_flow,
    subject_col,
    min_retained_prop
) {
  split_idx <- split(
    seq_len(nrow(unit_flow)),
    unit_flow[[subject_col]]
  )

  rows <- lapply(split_idx, function(idx) {
    d <- unit_flow[idx, , drop = FALSE]
    n_retained <- sum(d$retained, na.rm = TRUE)
    retained_prop <- n_retained / nrow(d)

    tibble::tibble(
      subject = as.character(d[[subject_col]][[1]]),
      n_units = nrow(d),
      n_retained_units = n_retained,
      n_excluded_units = nrow(d) - n_retained,
      retained_prop = retained_prop,
      excluded_prop = 1 - retained_prop,
      subject_exclusion_status = ifelse(
        retained_prop < min_retained_prop,
        "low_retention",
        "ok"
      )
    )
  })

  out <- dplyr::bind_rows(rows)

  names(out)[names(out) == "subject"] <- subject_col

  out
}

.gp3_exclusion_flow_condition_ratio <- function(condition_summary) {
  if (!is.data.frame(condition_summary) || nrow(condition_summary) <= 1L) {
    return(NA_real_)
  }

  props <- condition_summary$excluded_prop

  if (all(is.na(props))) {
    return(NA_real_)
  }

  if (all(props == 0, na.rm = TRUE)) {
    return(1)
  }

  nonzero <- props[props > 0]

  if (length(nonzero) == 0L) {
    return(1)
  }

  if (any(props == 0, na.rm = TRUE)) {
    return(Inf)
  }

  max(nonzero, na.rm = TRUE) / min(nonzero, na.rm = TRUE)
}

.gp3_exclusion_flow_to_logical <- function(x, arg) {
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

    true_values <- c("true", "t", "yes", "y", "1", "included", "include", "keep", "kept", "retained", "ok", "ready")
    false_values <- c("false", "f", "no", "n", "0", "excluded", "exclude", "drop", "dropped", "removed", "fail", "failed", "not_ready", "review")

    out[values %in% true_values] <- TRUE
    out[values %in% false_values] <- FALSE

    unknown <- is.na(out) & !is.na(values) & nzchar(values)

    if (any(unknown)) {
      stop(
        "`",
        arg,
        "` character values must be interpretable as inclusion/exclusion flags.",
        call. = FALSE
      )
    }

    return(out)
  }

  stop(
    "`",
    arg,
    "` must be logical, numeric 0/1, or character inclusion/exclusion values.",
    call. = FALSE
  )
}

.gp3_exclusion_flow_standardise_aliases <- function(data) {
  if ("MEDIA_ID" %in% names(data) && !"media_id" %in% names(data)) {
    data$media_id <- data$MEDIA_ID
  }

  if ("USER_FILE" %in% names(data) && !"subject" %in% names(data)) {
    data$subject <- data$USER_FILE
  }

  data
}

.gp3_exclusion_flow_standardise_cols <- function(cols) {
  if (is.null(cols)) {
    return(character())
  }

  cols <- as.character(cols)
  cols[cols == "MEDIA_ID"] <- "media_id"
  cols[cols == "USER_FILE"] <- "subject"
  cols
}

.gp3_exclusion_flow_resolve_col <- function(col, names_data, arg) {
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

.gp3_exclusion_flow_resolve_optional_col <- function(col, names_data, arg) {
  if (is.null(col)) {
    return(NULL)
  }

  .gp3_exclusion_flow_resolve_col(col, names_data, arg)
}

.gp3_exclusion_flow_check_positive_numeric <- function(x, arg) {
  if (!is.numeric(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0) {
    stop("`", arg, "` must be a positive numeric scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_exclusion_flow_check_character_vector <- function(x, arg) {
  if (!is.character(x) ||
      length(x) == 0L ||
      any(is.na(x)) ||
      any(!nzchar(x))) {
    stop("`", arg, "` must be a non-empty character vector.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_exclusion_flow_collapse_nullable <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  paste(as.character(x), collapse = ", ")
}
