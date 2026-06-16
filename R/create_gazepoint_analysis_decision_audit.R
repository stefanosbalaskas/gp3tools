#' Create a final Gazepoint analysis-decision audit
#'
#' Create a final audit table for a Gazepoint analysis workflow. The function
#' records which analysis branches were run, classifies them as confirmatory,
#' sensitivity, exploratory, diagnostic, preprocessing, or reporting branches,
#' summarises available diagnostics, flags interpretation cautions, and creates
#' a final analysis-readiness table.
#'
#' @param ... Named analysis result objects. Each named object is treated as one
#'   analysis branch.
#' @param results Optional named list of analysis result objects. This can be
#'   used instead of, or together with, `...`.
#' @param branch_roles Optional data frame describing branch roles. It must
#'   contain `branch_name` and `decision_type`. Optional columns include
#'   `analysis_family`, `interpretation_scope`, and `notes`.
#' @param required_confirmatory Character vector of confirmatory branches that
#'   must be present for the analysis to be considered complete.
#' @param diagnostics_required Logical. If `TRUE`, confirmatory model branches
#'   without extractable diagnostics are flagged with a caution.
#' @param require_clean_diagnostics Logical. If `TRUE`, diagnostic warnings in
#'   required confirmatory branches make the final readiness status
#'   `not_ready`.
#'
#' @return A list with class `gp3_analysis_decision_audit` containing overview,
#'   branch audit, diagnostics summary, interpretation cautions, readiness, and
#'   settings tables.
#' @export
create_gazepoint_analysis_decision_audit <- function(
    ...,
    results = NULL,
    branch_roles = NULL,
    required_confirmatory = character(),
    diagnostics_required = TRUE,
    require_clean_diagnostics = FALSE
) {
  dots <- list(...)

  if (!is.null(results)) {
    if (!is.list(results) || is.data.frame(results)) {
      stop("`results` must be a named list when supplied.", call. = FALSE)
    }


    dots <- c(results, dots)


  }

  if (length(dots) == 0L) {
    stop(
      "At least one named analysis result must be supplied through `...` or `results`.",
      call. = FALSE
    )
  }

  if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
    stop("All analysis result objects must be named.", call. = FALSE)
  }

  .gp3_check_logical_scalar(
    diagnostics_required,
    "diagnostics_required"
  )

  .gp3_check_logical_scalar(
    require_clean_diagnostics,
    "require_clean_diagnostics"
  )

  branch_roles <- .gp3_normalise_branch_roles(
    branch_names = names(dots),
    branch_roles = branch_roles
  )

  branch_audit <- .gp3_create_branch_audit(
    results = dots,
    branch_roles = branch_roles
  )

  diagnostics_summary <- .gp3_create_diagnostics_summary(
    results = dots,
    branch_audit = branch_audit
  )

  interpretation_cautions <- .gp3_create_interpretation_cautions(
    branch_audit = branch_audit,
    diagnostics_summary = diagnostics_summary,
    required_confirmatory = required_confirmatory,
    diagnostics_required = diagnostics_required,
    require_clean_diagnostics = require_clean_diagnostics
  )

  readiness <- .gp3_create_analysis_readiness(
    branch_audit = branch_audit,
    diagnostics_summary = diagnostics_summary,
    interpretation_cautions = interpretation_cautions,
    required_confirmatory = required_confirmatory,
    require_clean_diagnostics = require_clean_diagnostics
  )

  overview <- tibble::tibble(
    n_branches = nrow(branch_audit),
    n_confirmatory = sum(branch_audit$decision_type == "confirmatory"),
    n_sensitivity = sum(branch_audit$decision_type == "sensitivity"),
    n_exploratory = sum(branch_audit$decision_type == "exploratory"),
    n_diagnostic = sum(branch_audit$decision_type == "diagnostic"),
    n_preprocessing = sum(branch_audit$decision_type == "preprocessing"),
    n_reporting = sum(branch_audit$decision_type == "reporting"),
    n_unknown = sum(branch_audit$decision_type == "unknown"),
    n_diagnostic_warnings = sum(
      diagnostics_summary$diagnostic_status %in%
        c("warning", "diagnostic_warning", "singular_fit", "overdispersed"),
      na.rm = TRUE
    ),
    n_cautions = nrow(interpretation_cautions),
    readiness_status = readiness$readiness_status[[1]],
    readiness_message = readiness$message[[1]]
  )

  out <- list(
    overview = overview,
    branch_audit = branch_audit,
    diagnostics_summary = diagnostics_summary,
    interpretation_cautions = interpretation_cautions,
    readiness = readiness,
    settings = tibble::tibble(
      setting = c(
        "required_confirmatory",
        "diagnostics_required",
        "require_clean_diagnostics"
      ),
      value = c(
        paste(required_confirmatory, collapse = ", "),
        as.character(diagnostics_required),
        as.character(require_clean_diagnostics)
      )
    )
  )

  class(out) <- c("gp3_analysis_decision_audit", "list")

  out
}

.gp3_check_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }

  invisible(TRUE)
}

.gp3_normalise_branch_roles <- function(branch_names, branch_roles = NULL) {
  allowed_decision_types <- c(
    "confirmatory",
    "sensitivity",
    "exploratory",
    "diagnostic",
    "preprocessing",
    "reporting",
    "unknown"
  )

  if (is.null(branch_roles)) {
    return(tibble::tibble(
      branch_name = branch_names,
      decision_type = "unknown",
      analysis_family = NA_character_,
      interpretation_scope = NA_character_,
      notes = NA_character_
    ))
  }

  if (!is.data.frame(branch_roles)) {
    stop("`branch_roles` must be a data frame when supplied.", call. = FALSE)
  }

  required_cols <- c("branch_name", "decision_type")
  missing_cols <- setdiff(required_cols, names(branch_roles))

  if (length(missing_cols) > 0L) {
    stop(
      "`branch_roles` is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  branch_roles <- tibble::as_tibble(branch_roles)

  branch_roles$branch_name <- as.character(branch_roles$branch_name)
  branch_roles$decision_type <- tolower(as.character(branch_roles$decision_type))

  bad_types <- setdiff(
    unique(branch_roles$decision_type),
    allowed_decision_types
  )

  if (length(bad_types) > 0L) {
    stop(
      "`branch_roles$decision_type` contains unsupported value(s): ",
      paste(bad_types, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"analysis_family" %in% names(branch_roles)) {
    branch_roles$analysis_family <- NA_character_
  }

  if (!"interpretation_scope" %in% names(branch_roles)) {
    branch_roles$interpretation_scope <- NA_character_
  }

  if (!"notes" %in% names(branch_roles)) {
    branch_roles$notes <- NA_character_
  }

  branch_roles <- branch_roles[
    c(
      "branch_name",
      "decision_type",
      "analysis_family",
      "interpretation_scope",
      "notes"
    )
  ]

  missing_roles <- setdiff(branch_names, branch_roles$branch_name)

  if (length(missing_roles) > 0L) {
    branch_roles <- dplyr::bind_rows(
      branch_roles,
      tibble::tibble(
        branch_name = missing_roles,
        decision_type = "unknown",
        analysis_family = NA_character_,
        interpretation_scope = NA_character_,
        notes = NA_character_
      )
    )
  }

  branch_roles[match(branch_names, branch_roles$branch_name), ]
}

.gp3_create_branch_audit <- function(results, branch_roles) {
  rows <- vector("list", length(results))

  for (i in seq_along(results)) {
    branch_name <- names(results)[[i]]
    object <- results[[i]]
    role <- branch_roles[branch_roles$branch_name == branch_name, , drop = FALSE]


    rows[[i]] <- tibble::tibble(
      branch_name = branch_name,
      decision_type = role$decision_type[[1]],
      analysis_family = role$analysis_family[[1]],
      interpretation_scope = role$interpretation_scope[[1]],
      notes = role$notes[[1]],
      branch_run = !is.null(object),
      object_class = paste(class(object), collapse = ", "),
      object_type = typeof(object),
      branch_status = .gp3_extract_branch_status(object),
      fallback_used = .gp3_extract_logical_field(object, "fallback_used"),
      singular_fit = .gp3_extract_logical_field(object, "singular_fit"),
      has_model = .gp3_object_has_model(object),
      has_diagnostics = .gp3_object_has_diagnostics(object)
    )


  }

  dplyr::bind_rows(rows)
}

.gp3_extract_branch_status <- function(object) {
  if (is.null(object)) {
    return("not_run")
  }

  if (is.data.frame(object)) {
    status_cols <- names(object)[grepl("status", names(object), fixed = TRUE)]


    if (length(status_cols) > 0L) {
      vals <- unique(as.character(object[[status_cols[[1]]]]))
      vals <- vals[!is.na(vals)]

      if (length(vals) > 0L) {
        return(paste(vals, collapse = ", "))
      }
    }

    return("table_available")


  }

  if (!is.list(object)) {
    return("object_available")
  }

  candidate_names <- c(
    "model_status",
    "sensitivity_status",
    "cluster_status",
    "summary_status",
    "diagnostic_status",
    "workflow_status",
    "validation_status",
    "status"
  )

  for (nm in candidate_names) {
    if (!is.null(object[[nm]]) && length(object[[nm]]) >= 1L) {
      return(paste(as.character(object[[nm]]), collapse = ", "))
    }
  }

  if (!is.null(object$overview) && is.data.frame(object$overview)) {
    status_cols <- names(object$overview)[
      grepl("status", names(object$overview), fixed = TRUE)
    ]


    if (length(status_cols) > 0L && nrow(object$overview) > 0L) {
      vals <- unique(as.character(object$overview[[status_cols[[1]]]]))
      vals <- vals[!is.na(vals)]

      if (length(vals) > 0L) {
        return(paste(vals, collapse = ", "))
      }
    }


  }

  "object_available"
}

.gp3_extract_logical_field <- function(object, field) {
  if (is.null(object) || !is.list(object) || is.null(object[[field]])) {
    return(NA)
  }

  value <- object[[field]]

  if (is.logical(value) && length(value) >= 1L) {
    return(value[[1]])
  }

  NA
}

.gp3_object_has_model <- function(object) {
  is.list(object) && !is.null(object$model)
}

.gp3_object_has_diagnostics <- function(object) {
  if (inherits(object, "gp3_model_diagnostics")) {
    return(TRUE)
  }

  if (inherits(object, "gp3_model_summary")) {
    return(!is.null(object$diagnostics))
  }

  is.list(object) && !is.null(object$diagnostics)
}

.gp3_create_diagnostics_summary <- function(results, branch_audit) {
  rows <- vector("list", length(results))

  for (i in seq_along(results)) {
    branch_name <- names(results)[[i]]
    object <- results[[i]]


    rows[[i]] <- .gp3_extract_diagnostics_row(
      branch_name = branch_name,
      object = object
    )


  }

  diagnostics_summary <- dplyr::bind_rows(rows)

  diagnostics_summary <- dplyr::left_join(
    branch_audit[c("branch_name", "decision_type")],
    diagnostics_summary,
    by = "branch_name"
  )

  diagnostics_summary
}

.gp3_extract_diagnostics_row <- function(branch_name, object) {
  if (is.null(object)) {
    return(tibble::tibble(
      branch_name = branch_name,
      diagnostic_source = "none",
      diagnostic_status = "not_run",
      n_warning = NA_integer_,
      n_error = NA_integer_,
      n_skipped = NA_integer_,
      message = "Branch was not run."
    ))
  }

  diagnostics <- NULL

  if (inherits(object, "gp3_model_diagnostics")) {
    diagnostics <- object
  } else if (inherits(object, "gp3_model_summary")) {
    diagnostics <- object$diagnostics
  } else if (is.list(object) && !is.null(object$diagnostics)) {
    diagnostics <- object$diagnostics
  }

  if (is.null(diagnostics)) {
    return(tibble::tibble(
      branch_name = branch_name,
      diagnostic_source = "none",
      diagnostic_status = "not_available",
      n_warning = NA_integer_,
      n_error = NA_integer_,
      n_skipped = NA_integer_,
      message = "No diagnostics component was found."
    ))
  }

  diagnostic_tables <- diagnostics[
    vapply(diagnostics, is.data.frame, logical(1))
  ]

  if (length(diagnostic_tables) == 0L) {
    return(tibble::tibble(
      branch_name = branch_name,
      diagnostic_source = "diagnostics",
      diagnostic_status = "not_available",
      n_warning = NA_integer_,
      n_error = NA_integer_,
      n_skipped = NA_integer_,
      message = "Diagnostics object did not contain data-frame components."
    ))
  }

  combined <- dplyr::bind_rows(
    lapply(
      names(diagnostic_tables),
      function(nm) {
        tab <- diagnostic_tables[[nm]]
        tab$diagnostic_component <- nm
        tab
      }
    )
  )

  status_values <- .gp3_collect_status_values(combined)
  messages <- .gp3_collect_message_values(combined)

  status <- .gp3_collapse_diagnostic_status(status_values)

  tibble::tibble(
    branch_name = branch_name,
    diagnostic_source = "diagnostics",
    diagnostic_status = status,
    n_warning = sum(.gp3_status_is_warning(status_values)),
    n_error = sum(.gp3_status_is_error(status_values)),
    n_skipped = sum(.gp3_status_is_skipped(status_values)),
    message = .gp3_collapse_nonempty(messages)
  )
}

.gp3_collect_status_values <- function(x) {
  status_cols <- names(x)[grepl("status", names(x), fixed = TRUE)]

  if (length(status_cols) == 0L) {
    return(character())
  }

  values <- unlist(x[status_cols], use.names = FALSE)
  values <- as.character(values)
  values[!is.na(values) & nzchar(values)]
}

.gp3_collect_message_values <- function(x) {
  message_cols <- names(x)[
    grepl("message", names(x), fixed = TRUE) |
      grepl("warning", names(x), fixed = TRUE)
  ]

  if (length(message_cols) == 0L) {
    return(character())
  }

  values <- unlist(x[message_cols], use.names = FALSE)
  values <- as.character(values)
  values[!is.na(values) & nzchar(values)]
}

.gp3_collapse_diagnostic_status <- function(status_values) {
  if (length(status_values) == 0L) {
    return("not_available")
  }

  if (any(.gp3_status_is_error(status_values))) {
    return("error")
  }

  if (any(.gp3_status_is_warning(status_values))) {
    return("diagnostic_warning")
  }

  if (any(.gp3_status_is_skipped(status_values))) {
    return("skipped")
  }

  "ok"
}

.gp3_status_is_warning <- function(x) {
  x <- tolower(as.character(x))

  grepl("warning", x, fixed = TRUE) |
    grepl("singular", x, fixed = TRUE) |
    grepl("overdispers", x, fixed = TRUE) |
    grepl("failed", x, fixed = TRUE)
}

.gp3_status_is_error <- function(x) {
  x <- tolower(as.character(x))

  grepl("error", x, fixed = TRUE)
}

.gp3_status_is_skipped <- function(x) {
  x <- tolower(as.character(x))

  grepl("skipped", x, fixed = TRUE) |
    grepl("not_applicable", x, fixed = TRUE)
}

.gp3_collapse_nonempty <- function(x) {
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = " | ")
}

.gp3_create_interpretation_cautions <- function(
    branch_audit,
    diagnostics_summary,
    required_confirmatory,
    diagnostics_required,
    require_clean_diagnostics
) {
  cautions <- list()

  unknown <- branch_audit$branch_name[branch_audit$decision_type == "unknown"]

  if (length(unknown) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = unknown,
      caution_type = "unclassified_branch",
      caution_level = "moderate",
      message = "Branch was run but not classified as confirmatory, sensitivity, exploratory, diagnostic, preprocessing, or reporting."
    )
  }

  exploratory <- branch_audit$branch_name[
    branch_audit$decision_type == "exploratory"
  ]

  if (length(exploratory) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = exploratory,
      caution_type = "exploratory_not_confirmatory",
      caution_level = "moderate",
      message = "Exploratory branches should not be reported as confirmatory hypothesis tests."
    )
  }

  sensitivity <- branch_audit$branch_name[
    branch_audit$decision_type == "sensitivity"
  ]

  if (length(sensitivity) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = sensitivity,
      caution_type = "sensitivity_not_primary",
      caution_level = "low",
      message = "Sensitivity branches should be interpreted as robustness checks rather than primary confirmatory tests."
    )
  }

  missing_required <- setdiff(
    required_confirmatory,
    branch_audit$branch_name[branch_audit$branch_run]
  )

  if (length(missing_required) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = missing_required,
      caution_type = "missing_required_confirmatory_branch",
      caution_level = "high",
      message = "A required confirmatory branch was not supplied or was not run."
    )
  }

  if (isTRUE(diagnostics_required)) {
    confirmatory_missing_diag <- branch_audit$branch_name[
      branch_audit$decision_type == "confirmatory" &
        branch_audit$has_model &
        !branch_audit$has_diagnostics
    ]


    if (length(confirmatory_missing_diag) > 0L) {
      cautions[[length(cautions) + 1L]] <- tibble::tibble(
        branch_name = confirmatory_missing_diag,
        caution_type = "confirmatory_model_without_diagnostics",
        caution_level = "moderate",
        message = "Confirmatory model branch has no extractable diagnostics component."
      )
    }


  }

  diag_warning <- diagnostics_summary$branch_name[
    diagnostics_summary$diagnostic_status %in%
      c("warning", "diagnostic_warning", "singular_fit", "overdispersed")
  ]

  if (length(diag_warning) > 0L) {
    caution_level <- if (isTRUE(require_clean_diagnostics)) {
      "high"
    } else {
      "moderate"
    }


    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = diag_warning,
      caution_type = "diagnostic_warning",
      caution_level = caution_level,
      message = "At least one diagnostic component returned a warning-like status."
    )


  }

  diag_error <- diagnostics_summary$branch_name[
    diagnostics_summary$diagnostic_status == "error"
  ]

  if (length(diag_error) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = diag_error,
      caution_type = "diagnostic_error",
      caution_level = "high",
      message = "At least one diagnostic component returned an error status."
    )
  }

  fallback <- branch_audit$branch_name[
    !is.na(branch_audit$fallback_used) & branch_audit$fallback_used
  ]

  if (length(fallback) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = fallback,
      caution_type = "fallback_model_used",
      caution_level = "moderate",
      message = "A fallback model or fallback analysis path was used."
    )
  }

  singular <- branch_audit$branch_name[
    !is.na(branch_audit$singular_fit) & branch_audit$singular_fit
  ]

  if (length(singular) > 0L) {
    cautions[[length(cautions) + 1L]] <- tibble::tibble(
      branch_name = singular,
      caution_type = "singular_fit",
      caution_level = "moderate",
      message = "A singular random-effects structure was reported."
    )
  }

  if (length(cautions) == 0L) {
    return(tibble::tibble(
      branch_name = character(),
      caution_type = character(),
      caution_level = character(),
      message = character()
    ))
  }

  dplyr::bind_rows(cautions)
}

.gp3_create_analysis_readiness <- function(
    branch_audit,
    diagnostics_summary,
    interpretation_cautions,
    required_confirmatory,
    require_clean_diagnostics
) {
  missing_required <- setdiff(
    required_confirmatory,
    branch_audit$branch_name[branch_audit$branch_run]
  )

  has_diagnostic_error <- any(
    diagnostics_summary$diagnostic_status == "error",
    na.rm = TRUE
  )

  has_required_diag_warning <- any(
    diagnostics_summary$branch_name %in% required_confirmatory &
      diagnostics_summary$diagnostic_status %in%
      c("warning", "diagnostic_warning", "singular_fit", "overdispersed"),
    na.rm = TRUE
  )

  has_high_caution <- any(
    interpretation_cautions$caution_level == "high",
    na.rm = TRUE
  )

  has_any_caution <- nrow(interpretation_cautions) > 0L

  if (length(missing_required) > 0L) {
    status <- "not_ready"
    message <- paste(
      "Missing required confirmatory branch(es):",
      paste(missing_required, collapse = ", ")
    )
  } else if (has_diagnostic_error) {
    status <- "not_ready"
    message <- "At least one branch has a diagnostic error."
  } else if (isTRUE(require_clean_diagnostics) && has_required_diag_warning) {
    status <- "not_ready"
    message <- "A required confirmatory branch has diagnostic warnings and clean diagnostics were required."
  } else if (has_high_caution || has_any_caution) {
    status <- "ready_with_cautions"
    message <- "Analysis branches are available, but interpretation cautions should be reported."
  } else {
    status <- "ready"
    message <- "Analysis branches are available with no flagged interpretation cautions."
  }

  tibble::tibble(
    readiness_status = status,
    message = message,
    n_required_confirmatory = length(required_confirmatory),
    n_missing_required_confirmatory = length(missing_required),
    n_cautions = nrow(interpretation_cautions),
    n_high_cautions = sum(
      interpretation_cautions$caution_level == "high",
      na.rm = TRUE
    ),
    require_clean_diagnostics = require_clean_diagnostics
  )
}
