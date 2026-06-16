#' Compute Gazepoint AOI transition matrices
#'
#' Compute AOI transition count and probability matrices from sample-level
#' Gazepoint AOI data, AOI-entry tables, or AOI-sequence tables. The function
#' returns both matrix and long-table forms.
#'
#' @param data A Gazepoint sample-level data frame, AOI-entry table, or
#'   AOI-sequence table.
#' @param aoi_col Name of the AOI-state column. Used only when `data` is
#'   sample-level data. If `NULL`, the function tries `aoi_current`, `AOI`,
#'   and `aoi_state`.
#' @param time_col Name of the time column, in milliseconds. Used only when
#'   `data` is sample-level data.
#' @param group_cols Character vector of columns defining independent AOI
#'   sequences, usually subject/media/trial.
#' @param by_cols Optional character vector of columns used to compute separate
#'   matrices, for example `condition` or `MEDIA_ID`.
#' @param include_non_aoi Logical. If `TRUE`, non-AOI/background states are
#'   included in the transition matrix.
#' @param include_self_transitions Logical. If `TRUE`, same-state transitions
#'   are retained. These can occur after non-AOI states are removed.
#' @param states Optional character vector giving the desired row/column order
#'   for the transition matrices.
#' @param time_window Optional numeric vector of length 2 giving an entry-start
#'   time window in milliseconds.
#' @param non_aoi_values Character vector of AOI labels treated as background
#'   or non-AOI states.
#' @param missing_aoi_label Label used when the AOI value is missing.
#'
#' @return A list containing count matrices, probability matrices, and long-form
#'   transition counts/probabilities.
#'
#' @export
#' @importFrom rlang .data
compute_gazepoint_aoi_transition_matrix <- function(
    data,
    aoi_col = NULL,
    time_col = "time",
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    by_cols = NULL,
    include_non_aoi = TRUE,
    include_self_transitions = TRUE,
    states = NULL,
    time_window = NULL,
    non_aoi_values = c(
      "non_aoi",
      "none",
      "background",
      "outside",
      "outside_aoi",
      "missing",
      "missing_aoi"
    ),
    missing_aoi_label = "missing_aoi"
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_optional_column <- function(x) {
    is.null(x) ||
      (
        is.character(x) &&
          length(x) == 1L &&
          !is.na(x) &&
          nzchar(x)
      )
  }

  valid_column_vector <- function(x, allow_null = FALSE) {
    if (allow_null && is.null(x)) {
      return(TRUE)
    }

    is.character(x) &&
      length(x) >= 1L &&
      all(!is.na(x)) &&
      all(nzchar(x)) &&
      !anyDuplicated(x)
  }

  if (!valid_optional_column(aoi_col)) {
    stop(
      "`aoi_col` must be NULL or a non-missing character scalar.",
      call. = FALSE
    )
  }

  if (!is.character(time_col) ||
      length(time_col) != 1L ||
      is.na(time_col) ||
      !nzchar(time_col)) {
    stop("`time_col` must be a non-missing character scalar.", call. = FALSE)
  }

  if (!valid_column_vector(group_cols)) {
    stop(
      "`group_cols` must be a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!valid_column_vector(by_cols, allow_null = TRUE)) {
    stop(
      "`by_cols` must be NULL or a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.logical(include_non_aoi) ||
      length(include_non_aoi) != 1L ||
      is.na(include_non_aoi)) {
    stop("`include_non_aoi` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(include_self_transitions) ||
      length(include_self_transitions) != 1L ||
      is.na(include_self_transitions)) {
    stop("`include_self_transitions` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(states) &&
      (!is.character(states) ||
       length(states) < 1L ||
       any(is.na(states)) ||
       any(!nzchar(states)) ||
       anyDuplicated(states))) {
    stop(
      "`states` must be NULL or a character vector of unique AOI labels.",
      call. = FALSE
    )
  }

  if (!is.null(time_window) &&
      (!is.numeric(time_window) ||
       length(time_window) != 2L ||
       any(is.na(time_window)) ||
       any(!is.finite(time_window)))) {
    stop(
      "`time_window` must be NULL or a finite numeric vector of length 2.",
      call. = FALSE
    )
  }

  if (!is.character(non_aoi_values) ||
      any(is.na(non_aoi_values)) ||
      any(!nzchar(non_aoi_values))) {
    stop(
      "`non_aoi_values` must be a character vector of non-missing labels.",
      call. = FALSE
    )
  }

  if (!is.character(missing_aoi_label) ||
      length(missing_aoi_label) != 1L ||
      is.na(missing_aoi_label) ||
      !nzchar(missing_aoi_label)) {
    stop(
      "`missing_aoi_label` must be a non-missing character scalar.",
      call. = FALSE
    )
  }

  matrix_group_cols <- unique(c(group_cols, by_cols))

  has_sequence_columns <- all(
    c(
      "aoi_state",
      "transition_from",
      "transition_to",
      "entry_start_time",
      "is_terminal_state"
    ) %in% names(data)
  )

  if (has_sequence_columns) {
    sequences <- tibble::as_tibble(data)
  } else {
    sequences <- prepare_gazepoint_aoi_sequences(
      data = data,
      aoi_col = aoi_col,
      time_col = time_col,
      group_cols = matrix_group_cols,
      include_non_aoi = include_non_aoi,
      non_aoi_values = non_aoi_values,
      missing_aoi_label = missing_aoi_label,
      include_terminal = TRUE
    )
  }

  required_cols <- unique(
    c(
      matrix_group_cols,
      "aoi_state",
      "transition_from",
      "transition_to",
      "entry_start_time",
      "is_terminal_state"
    )
  )

  missing_cols <- setdiff(required_cols, names(sequences))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(time_window)) {
    lower <- min(time_window)
    upper <- max(time_window)

    sequences <- sequences |>
      dplyr::filter(
        .data[["entry_start_time"]] >= lower,
        .data[["entry_start_time"]] <= upper
      )

    if (nrow(sequences) == 0L) {
      stop(
        "No AOI sequence rows remain after applying `time_window`.",
        call. = FALSE
      )
    }
  }

  transitions <- sequences |>
    dplyr::filter(
      !.data[["is_terminal_state"]],
      !is.na(.data[["transition_from"]]),
      !is.na(.data[["transition_to"]])
    )

  if (!include_self_transitions) {
    transitions <- transitions |>
      dplyr::filter(
        .data[["transition_from"]] != .data[["transition_to"]]
      )
  }

  if (is.null(states)) {
    state_values <- unique(
      c(
        as.character(sequences$aoi_state),
        as.character(sequences$transition_from),
        as.character(sequences$transition_to)
      )
    )

    state_values <- state_values[!is.na(state_values) & nzchar(state_values)]
  } else {
    state_values <- states
  }

  if (length(state_values) == 0L) {
    stop("No AOI states are available for matrix construction.", call. = FALSE)
  }

  make_empty_long <- function() {
    out <- tibble::as_tibble(sequences[0, by_cols, drop = FALSE])
    out$from <- character(0)
    out$to <- character(0)
    out$n <- integer(0)
    out$row_total <- integer(0)
    out$prob <- numeric(0)
    out
  }

  if (nrow(transitions) > 0L) {
    long_table <- transitions |>
      dplyr::mutate(
        from = .data[["transition_from"]],
        to = .data[["transition_to"]]
      ) |>
      dplyr::count(
        dplyr::across(dplyr::all_of(by_cols)),
        .data[["from"]],
        .data[["to"]],
        name = "n"
      ) |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(by_cols, "from")))
      ) |>
      dplyr::mutate(
        row_total = sum(.data[["n"]], na.rm = TRUE),
        prob = .data[["n"]] / .data[["row_total"]]
      ) |>
      dplyr::ungroup()
  } else {
    long_table <- make_empty_long()
  }

  make_matrix <- function(x, value_col, state_values) {
    mat <- matrix(
      0,
      nrow = length(state_values),
      ncol = length(state_values),
      dimnames = list(
        from = state_values,
        to = state_values
      )
    )

    if (nrow(x) == 0L) {
      return(mat)
    }

    row_id <- match(as.character(x$from), state_values)
    col_id <- match(as.character(x$to), state_values)
    valid <- !is.na(row_id) & !is.na(col_id)

    if (any(valid)) {
      mat[cbind(row_id[valid], col_id[valid])] <- x[[value_col]][valid]
    }

    mat
  }

  key_matches <- function(x, key_row, key_cols) {
    keep <- rep(TRUE, nrow(x))

    for (col in key_cols) {
      value <- key_row[[col]][[1]]

      if (is.na(value)) {
        keep <- keep & is.na(x[[col]])
      } else {
        keep <- keep & !is.na(x[[col]]) & x[[col]] == value
      }
    }

    keep
  }

  key_label <- function(key_row, key_cols) {
    paste(
      vapply(
        key_cols,
        function(col) {
          value <- key_row[[col]][[1]]
          paste0(col, "=", ifelse(is.na(value), "NA", as.character(value)))
        },
        character(1)
      ),
      collapse = " | "
    )
  }

  if (length(by_cols) == 0L) {
    count_matrix <- make_matrix(
      long_table,
      "n",
      state_values
    )

    probability_matrix <- make_matrix(
      long_table,
      "prob",
      state_values
    )

    count_matrices <- NULL
    probability_matrices <- NULL
  } else {
    by_keys <- sequences |>
      dplyr::distinct(
        dplyr::across(dplyr::all_of(by_cols))
      )

    count_matrices <- list()
    probability_matrices <- list()

    for (i in seq_len(nrow(by_keys))) {
      key_row <- by_keys[i, , drop = FALSE]
      this_name <- key_label(key_row, by_cols)
      this_long <- long_table[key_matches(long_table, key_row, by_cols), , drop = FALSE]

      count_matrices[[this_name]] <- make_matrix(
        this_long,
        "n",
        state_values
      )

      probability_matrices[[this_name]] <- make_matrix(
        this_long,
        "prob",
        state_values
      )
    }

    count_matrix <- NULL
    probability_matrix <- NULL
  }

  out <- list(
    count_matrix = count_matrix,
    probability_matrix = probability_matrix,
    count_matrices = count_matrices,
    probability_matrices = probability_matrices,
    long_table = long_table,
    states = state_values,
    settings = list(
      group_cols = group_cols,
      by_cols = by_cols,
      include_non_aoi = include_non_aoi,
      include_self_transitions = include_self_transitions,
      time_window = time_window
    )
  )

  class(out) <- c("gp3_aoi_transition_matrix", class(out))

  out
}
