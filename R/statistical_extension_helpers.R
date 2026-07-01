.gp3_ext_check_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  invisible(data)
}

.gp3_ext_check_scalar_string <- function(x, name) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    stop(name, " must be a single non-empty column name.", call. = FALSE)
  }
  as.character(x)
}

.gp3_ext_check_character_vector <- function(x, name) {
  if (is.null(x) || length(x) == 0L || any(is.na(x)) || any(!nzchar(as.character(x)))) {
    stop(name, " must contain one or more non-empty column names.", call. = FALSE)
  }
  as.character(x)
}

.gp3_ext_check_columns <- function(data, cols) {
  cols <- as.character(cols)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop("`data` is missing required column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_ext_bind_rows <- function(rows) {
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

.gp3_ext_group_key <- function(data, group_cols) {
  if (is.null(group_cols) || length(group_cols) == 0L) {
    return("all")
  }
  paste(paste(group_cols, as.character(data[1L, group_cols, drop = TRUE]), sep = "="), collapse = "|")
}

.gp3_ext_group_values <- function(data, group_cols) {
  if (is.null(group_cols) || length(group_cols) == 0L) {
    return(data.frame(.gp3_dummy = 1L)[, FALSE, drop = FALSE])
  }
  data[1L, group_cols, drop = FALSE]
}

.gp3_ext_split_groups <- function(data, group_cols) {
  if (is.null(group_cols) || length(group_cols) == 0L) {
    return(list(all = data))
  }
  key <- interaction(data[, group_cols, drop = TRUE], drop = TRUE, lex.order = TRUE)
  split(data, key)
}

.gp3_ext_order_data <- function(data, time_col = NULL) {
  if (is.null(time_col)) {
    return(data)
  }
  data[order(data[[time_col]]), , drop = FALSE]
}

.gp3_ext_prepare_aoi <- function(aoi, include_missing = FALSE, missing_label = "missing") {
  aoi <- as.character(aoi)
  missing <- is.na(aoi) | trimws(aoi) == ""
  if (isTRUE(include_missing)) {
    aoi[missing] <- missing_label
  } else {
    aoi <- aoi[!missing]
  }
  aoi
}

.gp3_ext_collapse_repeats <- function(x, collapse = TRUE) {
  if (!isTRUE(collapse) || length(x) <= 1L) {
    return(x)
  }
  x[c(TRUE, x[-1L] != x[-length(x)])]
}

.gp3_ext_normalized_entropy <- function(x, log_base = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  tab <- table(x)
  p <- as.numeric(tab) / sum(tab)
  entropy <- -sum(p * log(p, base = log_base))
  n_categories <- length(tab)
  if (n_categories <= 1L) {
    return(0)
  }
  entropy / log(n_categories, base = log_base)
}
