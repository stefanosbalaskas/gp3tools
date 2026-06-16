#' Export manuscript-ready model tables
#'
#' Export model-summary tables and optional estimated marginal means tables to
#' CSV files. The function is designed for objects returned by
#' `tidy_gazepoint_model_summary()` and `summarise_gazepoint_emmeans()`.
#'
#' @param model_summary Optional object returned by
#'   `tidy_gazepoint_model_summary()`.
#' @param emmeans_summary Optional object returned by
#'   `summarise_gazepoint_emmeans()`.
#' @param output_dir Output directory.
#' @param prefix File-name prefix.
#' @param overwrite Logical. If `FALSE`, existing output files cause an error.
#' @param include_diagnostics Logical. If `TRUE`, export available diagnostic
#'   component tables from `model_summary`.
#'
#' @return A tibble indexing the written files.
#' @export
export_gazepoint_model_tables <- function(
    model_summary = NULL,
    emmeans_summary = NULL,
    output_dir,
    prefix = "gazepoint_model",
    overwrite = TRUE,
    include_diagnostics = TRUE
) {
  if (missing(output_dir) || is.null(output_dir)) {
    stop("`output_dir` must be supplied.", call. = FALSE)
  }

  check_character_scalar <- function(x, arg) {
    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        "`", arg, "` must be a non-missing character scalar.",
        call. = FALSE
      )
    }


    invisible(TRUE)


  }

  check_logical_scalar <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }


    invisible(TRUE)


  }

  check_character_scalar(output_dir, "output_dir")
  check_character_scalar(prefix, "prefix")
  check_logical_scalar(overwrite, "overwrite")
  check_logical_scalar(include_diagnostics, "include_diagnostics")

  if (is.null(model_summary) && is.null(emmeans_summary)) {
    stop(
      "At least one of `model_summary` or `emmeans_summary` must be supplied.",
      call. = FALSE
    )
  }

  if (!is.null(model_summary) &&
      !inherits(model_summary, "gp3_model_summary")) {
    stop(
      "`model_summary` must be an object returned by `tidy_gazepoint_model_summary()`.",
      call. = FALSE
    )
  }

  if (!is.null(emmeans_summary) &&
      !inherits(emmeans_summary, "gp3_emmeans_summary")) {
    stop(
      "`emmeans_summary` must be an object returned by `summarise_gazepoint_emmeans()`.",
      call. = FALSE
    )
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("Could not create `output_dir`.", call. = FALSE)
  }

  prefix <- .gp3_sanitise_file_prefix(prefix)

  tables <- list()

  if (!is.null(model_summary)) {
    tables <- c(
      tables,
      .gp3_collect_model_summary_tables(
        model_summary = model_summary,
        include_diagnostics = include_diagnostics
      )
    )
  }

  if (!is.null(emmeans_summary)) {
    tables <- c(
      tables,
      .gp3_collect_emmeans_summary_tables(
        emmeans_summary = emmeans_summary
      )
    )
  }

  if (length(tables) == 0L) {
    stop("No exportable tables were found.", call. = FALSE)
  }

  written <- vector("list", length(tables))

  for (i in seq_along(tables)) {
    table_info <- tables[[i]]


    file_name <- paste0(prefix, "_", table_info$name, ".csv")
    file_path <- file.path(output_dir, file_name)

    if (file.exists(file_path) && !isTRUE(overwrite)) {
      stop(
        "Output file already exists and `overwrite = FALSE`: ",
        file_path,
        call. = FALSE
      )
    }

    utils::write.csv(
      table_info$table,
      file = file_path,
      row.names = FALSE,
      na = ""
    )

    written[[i]] <- tibble::tibble(
      table_name = table_info$name,
      table_type = table_info$type,
      n_rows = nrow(table_info$table),
      n_cols = ncol(table_info$table),
      file = file_path
    )


  }

  index <- dplyr::bind_rows(written)

  index_file <- file.path(output_dir, paste0(prefix, "_export_index.csv"))

  if (file.exists(index_file) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists and `overwrite = FALSE`: ",
      index_file,
      call. = FALSE
    )
  }

  utils::write.csv(
    index,
    file = index_file,
    row.names = FALSE,
    na = ""
  )

  index <- dplyr::bind_rows(
    index,
    tibble::tibble(
      table_name = "export_index",
      table_type = "index",
      n_rows = nrow(index),
      n_cols = ncol(index),
      file = index_file
    )
  )

  class(index) <- c("gp3_model_table_export", class(index))

  index
}

.gp3_collect_model_summary_tables <- function(
    model_summary,
    include_diagnostics
) {
  tables <- list()

  tables$model_overview <- list(
    name = "model_overview",
    type = "model_summary",
    table = model_summary$overview
  )

  tables$model_info <- list(
    name = "model_info",
    type = "model_summary",
    table = model_summary$model_info
  )

  tables$fixed_effects <- list(
    name = "fixed_effects",
    type = "model_summary",
    table = model_summary$fixed_effects
  )

  tables$model_settings <- list(
    name = "model_settings",
    type = "settings",
    table = .gp3_settings_to_table(
      model_summary$settings,
      settings_name = "model_summary"
    )
  )

  if (isTRUE(include_diagnostics) &&
      !is.null(model_summary$diagnostics) &&
      is.list(model_summary$diagnostics)) {
    diagnostics <- model_summary$diagnostics


    for (nm in names(diagnostics)) {
      if (is.data.frame(diagnostics[[nm]])) {
        export_name <- paste0("diagnostics_", nm)

        tables[[export_name]] <- list(
          name = export_name,
          type = "diagnostics",
          table = diagnostics[[nm]]
        )
      }
    }


  }

  .gp3_keep_exportable_tables(tables)
}

.gp3_collect_emmeans_summary_tables <- function(emmeans_summary) {
  tables <- list()

  tables$emmeans_overview <- list(
    name = "emmeans_overview",
    type = "emmeans_summary",
    table = emmeans_summary$overview
  )

  tables$emmeans <- list(
    name = "emmeans",
    type = "emmeans_summary",
    table = emmeans_summary$emmeans
  )

  tables$contrasts <- list(
    name = "contrasts",
    type = "emmeans_summary",
    table = emmeans_summary$contrasts
  )

  tables$emmeans_settings <- list(
    name = "emmeans_settings",
    type = "settings",
    table = .gp3_settings_to_table(
      emmeans_summary$settings,
      settings_name = "emmeans_summary"
    )
  )

  .gp3_keep_exportable_tables(tables)
}

.gp3_keep_exportable_tables <- function(tables) {
  keep <- vapply(
    tables,
    function(x) {
      is.list(x) &&
        !is.null(x$table) &&
        is.data.frame(x$table) &&
        nrow(x$table) >= 1L
    },
    logical(1)
  )

  tables[keep]
}

.gp3_settings_to_table <- function(settings, settings_name) {
  if (is.null(settings) || length(settings) == 0L) {
    return(tibble::tibble(
      settings_name = settings_name,
      setting = NA_character_,
      value = NA_character_
    ))
  }

  tibble::tibble(
    settings_name = settings_name,
    setting = names(settings),
    value = vapply(
      settings,
      .gp3_setting_value_to_character,
      character(1)
    )
  )
}

.gp3_setting_value_to_character <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  if (inherits(x, "formula")) {
    return(paste(deparse(x), collapse = " "))
  }

  if (is.atomic(x)) {
    return(paste(as.character(x), collapse = ", "))
  }

  paste(
    utils::capture.output(utils::str(x, give.attr = FALSE)),
    collapse = " "
  )
}

.gp3_sanitise_file_prefix <- function(prefix) {
  chars <- strsplit(prefix, split = "", fixed = TRUE)[[1L]]
  allowed <- c(letters, LETTERS, as.character(0:9), "_", ".", "-")
  chars[!chars %in% allowed] <- "_"
  prefix <- paste(chars, collapse = "")

  while (grepl("__", prefix, fixed = TRUE)) {
    prefix <- gsub("__", "_", prefix, fixed = TRUE)
  }

  while (nzchar(prefix) && startsWith(prefix, "_")) {
    prefix <- substring(prefix, 2L)
  }

  while (nzchar(prefix) && endsWith(prefix, "_")) {
    prefix <- substring(prefix, 1L, nchar(prefix) - 1L)
  }

  if (!nzchar(prefix)) {
    prefix <- "gazepoint_model"
  }

  prefix
}
