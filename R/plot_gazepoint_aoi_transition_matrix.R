#' Plot Gazepoint AOI transition matrix
#'
#' Plot a heatmap of AOI transition counts or probabilities from the output of
#' `compute_gazepoint_aoi_transition_matrix()` or from a compatible long-form
#' transition table.
#'
#' @param transitions A `gp3_aoi_transition_matrix` object, a long-form
#'   transition table with `from`, `to`, `n`, and/or `prob` columns, or a numeric
#'   matrix with AOI states as row and column names.
#' @param value Which value to plot: `"prob"` for transition probabilities or
#'   `"n"` for transition counts.
#' @param state_order Optional character vector defining the AOI order on the
#'   heatmap axes.
#' @param by_cols Optional character vector of grouping columns to facet by. If
#'   `NULL`, the function uses grouping columns stored in a
#'   `gp3_aoi_transition_matrix` object, when available.
#' @param include_zero Logical. If `TRUE`, all possible state-to-state cells are
#'   shown, with missing transitions displayed as zero.
#' @param show_labels Logical. If `TRUE`, cell values are printed inside tiles.
#' @param label_digits Number of digits used when labelling probabilities.
#' @param label_size Text size for cell labels.
#' @param facet Logical. If `TRUE`, grouped transition tables are faceted.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` plot object.
#'
#' @export
#' @importFrom rlang .data
plot_gazepoint_aoi_transition_matrix <- function(
    transitions,
    value = c("prob", "n"),
    state_order = NULL,
    by_cols = NULL,
    include_zero = TRUE,
    show_labels = TRUE,
    label_digits = 2,
    label_size = 3,
    facet = TRUE,
    title = NULL
) {
  value <- match.arg(value)

  if (!inherits(transitions, "gp3_aoi_transition_matrix") &&
      !is.data.frame(transitions) &&
      !is.matrix(transitions)) {
    stop(
      "`transitions` must be a gp3_aoi_transition_matrix object, a data frame, or a matrix.",
      call. = FALSE
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

  if (!valid_column_vector(state_order, allow_null = TRUE)) {
    stop(
      "`state_order` must be NULL or a character vector of unique AOI labels.",
      call. = FALSE
    )
  }

  if (!valid_column_vector(by_cols, allow_null = TRUE)) {
    stop(
      "`by_cols` must be NULL or a character vector of unique column names.",
      call. = FALSE
    )
  }

  if (!is.logical(include_zero) ||
      length(include_zero) != 1L ||
      is.na(include_zero)) {
    stop("`include_zero` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(show_labels) ||
      length(show_labels) != 1L ||
      is.na(show_labels)) {
    stop("`show_labels` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(facet) ||
      length(facet) != 1L ||
      is.na(facet)) {
    stop("`facet` must be TRUE or FALSE.", call. = FALSE)
  }

  numeric_args <- c(
    label_digits = label_digits,
    label_size = label_size
  )

  valid_numeric_arg <- vapply(
    numeric_args,
    function(x) {
      is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x)
    },
    logical(1)
  )

  if (any(!valid_numeric_arg)) {
    stop(
      "Plot-control arguments must be finite numeric scalars: ",
      paste(names(numeric_args)[!valid_numeric_arg], collapse = ", "),
      call. = FALSE
    )
  }

  if (label_digits < 0) {
    stop("`label_digits` must be greater than or equal to 0.", call. = FALSE)
  }

  if (label_size <= 0) {
    stop("`label_size` must be greater than 0.", call. = FALSE)
  }

  matrix_to_long <- function(x) {
    if (is.null(rownames(x)) || is.null(colnames(x))) {
      stop(
        "Matrix inputs must have row names and column names.",
        call. = FALSE
      )
    }

    tab <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
    names(tab) <- c("from", "to", ".gp3_matrix_value")

    tibble::as_tibble(tab)
  }

  if (inherits(transitions, "gp3_aoi_transition_matrix")) {
    long_table <- transitions$long_table
    object_states <- transitions$states

    if (is.null(by_cols)) {
      by_cols <- transitions$settings$by_cols
    }
  } else if (is.matrix(transitions)) {
    long_table <- matrix_to_long(transitions)
    object_states <- unique(c(rownames(transitions), colnames(transitions)))

    if (value == "prob") {
      long_table$prob <- suppressWarnings(as.numeric(long_table$.gp3_matrix_value))
      long_table$n <- NA_integer_
    } else {
      long_table$n <- suppressWarnings(as.integer(long_table$.gp3_matrix_value))
      long_table$prob <- NA_real_
    }

    long_table <- long_table |>
      dplyr::select(
        dplyr::all_of(c("from", "to", "n", "prob"))
      )
  } else {
    long_table <- tibble::as_tibble(transitions)
    object_states <- NULL
  }

  if (!is.data.frame(long_table)) {
    stop(
      "`transitions` does not contain a valid long-form transition table.",
      call. = FALSE
    )
  }

  required_cols <- c("from", "to", value)
  missing_cols <- setdiff(required_cols, names(long_table))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(by_cols)) {
    by_cols <- character(0)
  }

  missing_by_cols <- setdiff(by_cols, names(long_table))

  if (length(missing_by_cols) > 0L) {
    stop(
      "Missing `by_cols` columns: ",
      paste(missing_by_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(state_order)) {
    if (!is.null(object_states)) {
      state_order <- object_states
    } else {
      state_order <- unique(
        c(
          as.character(long_table$from),
          as.character(long_table$to)
        )
      )

      state_order <- state_order[
        !is.na(state_order) &
          nzchar(state_order)
      ]
    }
  }

  if (length(state_order) == 0L) {
    stop("No AOI states are available for plotting.", call. = FALSE)
  }

  make_cell_grid <- function(keys, by_cols, state_order) {
    state_grid <- expand.grid(
      from = state_order,
      to = state_order,
      stringsAsFactors = FALSE
    )

    if (length(by_cols) == 0L) {
      return(tibble::as_tibble(state_grid))
    }

    if (nrow(keys) == 0L) {
      return(tibble::as_tibble(keys[0, , drop = FALSE]))
    }

    pieces <- vector("list", nrow(keys))

    for (i in seq_len(nrow(keys))) {
      key_row <- keys[i, , drop = FALSE]
      repeated_key <- key_row[rep(1L, nrow(state_grid)), , drop = FALSE]
      pieces[[i]] <- dplyr::bind_cols(
        tibble::as_tibble(repeated_key),
        tibble::as_tibble(state_grid)
      )
    }

    dplyr::bind_rows(pieces)
  }

  join_cols <- c(by_cols, "from", "to")

  if (include_zero) {
    if (length(by_cols) == 0L) {
      keys <- tibble::tibble(.dummy = 1L)[0, , drop = FALSE]
    } else {
      keys <- long_table |>
        dplyr::distinct(
          dplyr::across(dplyr::all_of(by_cols))
        )
    }

    cell_grid <- make_cell_grid(keys, by_cols, state_order)

    plot_data <- cell_grid |>
      dplyr::left_join(
        long_table,
        by = join_cols
      )
  } else {
    plot_data <- long_table
  }

  if (nrow(plot_data) == 0L) {
    stop("No transition rows are available for plotting.", call. = FALSE)
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      .gp3_plot_value = suppressWarnings(as.numeric(.data[[value]])),
      .gp3_plot_value = dplyr::coalesce(.data[[".gp3_plot_value"]], 0),
      .gp3_from_plot = factor(
        .data[["from"]],
        levels = rev(state_order)
      ),
      .gp3_to_plot = factor(
        .data[["to"]],
        levels = state_order
      )
    )

  if (value == "prob") {
    plot_data <- plot_data |>
      dplyr::mutate(
        .gp3_label = sprintf(
          paste0("%.", as.integer(label_digits), "f"),
          .data[[".gp3_plot_value"]]
        )
      )
  } else {
    plot_data <- plot_data |>
      dplyr::mutate(
        .gp3_label = as.character(
          as.integer(round(.data[[".gp3_plot_value"]]))
        )
      )
  }

  if (length(by_cols) > 0L) {
    plot_data <- plot_data |>
      dplyr::mutate(
        .gp3_panel = apply(
          dplyr::pick(dplyr::all_of(by_cols)),
          1,
          function(row) {
            paste(
              paste0(
                by_cols,
                "=",
                ifelse(is.na(row), "NA", as.character(row))
              ),
              collapse = " | "
            )
          }
        )
      )
  }

  fill_label <- if (value == "prob") {
    "Probability"
  } else {
    "Count"
  }

  if (is.null(title)) {
    title <- if (value == "prob") {
      "Gazepoint AOI transition probabilities"
    } else {
      "Gazepoint AOI transition counts"
    }
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[".gp3_to_plot"]],
      y = .data[[".gp3_from_plot"]],
      fill = .data[[".gp3_plot_value"]]
    )
  ) +
    ggplot2::geom_tile(
      colour = "white"
    ) +
    ggplot2::labs(
      title = title,
      x = "To AOI",
      y = "From AOI",
      fill = fill_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (show_labels) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = .data[[".gp3_label"]]
        ),
        size = label_size
      )
  }

  if (length(by_cols) > 0L && facet) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula("~ .gp3_panel")
      )
  }

  p
}
