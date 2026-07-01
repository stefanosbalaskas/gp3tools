
.gp3_sequence_check_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  invisible(TRUE)
}

.gp3_sequence_check_scalar_string <- function(x, arg, allow_null = FALSE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(invisible(TRUE))
  }

  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a non-missing character scalar.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_sequence_check_character_vector <- function(x, arg, allow_null = TRUE) {
  if (is.null(x) && isTRUE(allow_null)) {
    return(invisible(TRUE))
  }

  if (!is.character(x) || any(is.na(x)) || any(!nzchar(x))) {
    stop("`", arg, "` must be a character vector without missing or empty values.",
         call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_sequence_check_columns <- function(data, cols) {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  missing_cols <- setdiff(cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.gp3_sequence_make_group_key <- function(data, group_cols) {
  if (is.null(group_cols) || length(group_cols) == 0L) {
    return(rep(".all", nrow(data)))
  }

  key_data <- data[group_cols]

  for (nm in names(key_data)) {
    key_data[[nm]] <- as.character(key_data[[nm]])
    key_data[[nm]][is.na(key_data[[nm]])] <- "<NA>"
  }

  do.call(interaction, c(key_data, list(drop = TRUE, sep = " | ")))
}

.gp3_sequence_split_groups <- function(data, group_cols) {
  if (nrow(data) == 0L) {
    return(list())
  }

  key <- .gp3_sequence_make_group_key(data, group_cols)
  split(data, key, drop = TRUE)
}

.gp3_sequence_group_values <- function(data, group_cols) {
  if (is.null(group_cols) || length(group_cols) == 0L) {
    # Return a one-row, zero-column data frame so it can be safely
    # combined with one-row metric summaries.
    return(data.frame(.gp3_dummy = 1L)[, FALSE, drop = FALSE])
  }

  data[1L, group_cols, drop = FALSE]
}

.gp3_sequence_order_data <- function(data, time_col = NULL) {
  if (is.null(time_col)) {
    return(data)
  }

  data[order(data[[time_col]], na.last = TRUE), , drop = FALSE]
}

.gp3_sequence_prepare_aoi <- function(x,
                                      include_missing = FALSE,
                                      missing_label = "missing") {
  x <- as.character(x)
  missing <- is.na(x) | !nzchar(trimws(x))

  if (isTRUE(include_missing)) {
    x[missing] <- missing_label
  } else {
    x <- x[!missing]
  }

  x
}

.gp3_sequence_collapse_repeats <- function(x) {
  if (length(x) <= 1L) {
    return(x)
  }

  rle(x)$values
}

.gp3_sequence_entropy_value <- function(counts, log_base = 2) {
  counts <- as.numeric(counts)
  counts <- counts[is.finite(counts) & counts > 0]

  if (!length(counts)) {
    return(NA_real_)
  }

  p <- counts / sum(counts)
  -sum(p * log(p, base = log_base))
}

.gp3_sequence_normalized_entropy <- function(entropy, n_categories, log_base = 2) {
  if (!is.finite(entropy) || !is.finite(n_categories) || n_categories < 1) {
    return(NA_real_)
  }

  if (n_categories <= 1) {
    return(0)
  }

  max_entropy <- log(n_categories, base = log_base)

  if (!is.finite(max_entropy) || max_entropy <= 0) {
    return(NA_real_)
  }

  entropy / max_entropy
}

.gp3_sequence_bind_rows <- function(rows) {
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (!length(rows)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

.gp3_sequence_status <- function(n_observations, n_transitions = NULL) {
  if (n_observations == 0L) {
    return("no_valid_aoi")
  }

  if (!is.null(n_transitions) && n_transitions == 0L) {
    return("no_transitions")
  }

  "ok"
}

.gp3_sequence_levenshtein <- function(a,
                                      b,
                                      substitution_cost = 1,
                                      insertion_cost = 1,
                                      deletion_cost = 1) {
  n <- length(a)
  m <- length(b)

  d <- matrix(0, nrow = n + 1L, ncol = m + 1L)

  d[, 1L] <- seq(0, n) * deletion_cost
  d[1L, ] <- seq(0, m) * insertion_cost

  if (n == 0L || m == 0L) {
    return(d[n + 1L, m + 1L])
  }

  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cost <- if (identical(a[[i]], b[[j]])) 0 else substitution_cost

      d[i + 1L, j + 1L] <- min(
        d[i, j + 1L] + deletion_cost,
        d[i + 1L, j] + insertion_cost,
        d[i, j] + cost
      )
    }
  }

  d[n + 1L, m + 1L]
}
