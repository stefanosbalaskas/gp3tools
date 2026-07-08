#' Create a reporting checklist for external facial-behaviour workflows
#'
#' Creates a compact checklist for reporting external facial-behaviour analyses
#' alongside Gazepoint data. The checklist is designed for reviewer-facing
#' transparency: it records whether import, quality auditing, synchronisation,
#' window summaries, reactivity summaries, and modelling outputs are available.
#' It also includes interpretation cautions. The helper does not infer facial
#' expressions or emotional states.
#'
#' @param face_data Optional imported or standardised face-analysis data.
#' @param quality_audit Optional object returned by
#'   `audit_gazepoint_face_quality()`.
#' @param sync_audit Optional object returned by `audit_gazepoint_face_sync()`.
#' @param window_summary Optional object returned by
#'   `summarize_gazepoint_face_windows()`.
#' @param reactivity_summary Optional object returned by
#'   `summarize_gazepoint_face_reactivity()`.
#' @param multimodal_model Optional object returned by
#'   `fit_gazepoint_face_window_lmm()` or
#'   `fit_gazepoint_multimodal_response_model()`.
#' @param include_interpretation_cautions Should interpretation-caution checklist
#'   items be included?
#'
#' @return A tibble with class `gp3_face_reporting_checklist`.
#' @export
#'
#' @examples
#' quality <- list(
#'   overview = data.frame(
#'     n_rows = 10,
#'     valid_percent = 95,
#'     face_quality_status = "pass"
#'   ),
#'   issue_summary = data.frame(
#'     issue = "missing_confidence",
#'     n_groups_affected = 0
#'   )
#' )
#' class(quality) <- c("gp3_face_quality_audit", "list")
#'
#' create_gazepoint_face_reporting_checklist(quality_audit = quality)
create_gazepoint_face_reporting_checklist <- function(
    face_data = NULL,
    quality_audit = NULL,
    sync_audit = NULL,
    window_summary = NULL,
    reactivity_summary = NULL,
    multimodal_model = NULL,
    include_interpretation_cautions = TRUE) {
  rows <- list()
  i <- 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Input and provenance",
    item = "External face-analysis data are available",
    status = if (!is.null(face_data)) "pass" else "not_available",
    evidence = .gp3_face_reporting_data_evidence(face_data),
    recommendation = if (!is.null(face_data)) {
      "Report the external face-analysis tool, version, input files, and exported columns."
    } else {
      "Provide imported or standardised external face-analysis data when facial-behaviour analyses are reported."
    }
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Input and provenance",
    item = "Standardised face columns are available",
    status = .gp3_face_reporting_standard_cols_status(face_data),
    evidence = .gp3_face_reporting_standard_cols_evidence(face_data),
    recommendation = "Report standardised timing, frame, confidence, success, and validity fields where available."
  )
  i <- i + 1L

  quality_status <- .gp3_face_reporting_audit_status(
    quality_audit,
    overview_col = "face_quality_status"
  )

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Quality control",
    item = "Face-data quality audit is available",
    status = if (!is.null(quality_audit)) "pass" else "not_available",
    evidence = .gp3_face_reporting_object_evidence(
      quality_audit,
      expected_class = "gp3_face_quality_audit"
    ),
    recommendation = "Use audit_gazepoint_face_quality() before reporting facial-behaviour summaries."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Quality control",
    item = "Face-data quality status is acceptable",
    status = quality_status$status,
    evidence = quality_status$evidence,
    recommendation = "Report valid-row percentage, confidence coverage, duplicate-frame checks, and timing-gap checks."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Quality control",
    item = "Quality issues are documented",
    status = .gp3_face_reporting_issue_status(quality_audit),
    evidence = .gp3_face_reporting_issue_evidence(quality_audit),
    recommendation = "Document groups requiring review and explain any exclusions or sensitivity analyses."
  )
  i <- i + 1L

  sync_status <- .gp3_face_reporting_audit_status(
    sync_audit,
    overview_col = "face_sync_audit_status"
  )

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Synchronisation",
    item = "Face-data synchronisation audit is available",
    status = if (!is.null(sync_audit)) "pass" else "not_available",
    evidence = .gp3_face_reporting_object_evidence(
      sync_audit,
      expected_class = "gp3_face_sync_audit"
    ),
    recommendation = "Use audit_gazepoint_face_sync() when face data are aligned to Gazepoint rows."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Synchronisation",
    item = "Synchronisation status is acceptable",
    status = sync_status$status,
    evidence = sync_status$evidence,
    recommendation = "Report matching method, tolerance, matched percentage, unmatched rows, and timing differences."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Window summaries",
    item = "Face-window summary is available",
    status = if (!is.null(window_summary)) "pass" else "not_available",
    evidence = .gp3_face_reporting_data_evidence(window_summary),
    recommendation = "Report window definitions, grouping variables, validity filtering, and summarised facial-behaviour measures."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Window summaries",
    item = "Window-summary coverage is documented",
    status = .gp3_face_reporting_window_status(window_summary),
    evidence = .gp3_face_reporting_window_evidence(window_summary),
    recommendation = "Report n_rows, n_used, valid_percent, confidence summaries, and measure summaries for each window."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Reactivity summaries",
    item = "Baseline-to-response reactivity is available when used",
    status = if (!is.null(reactivity_summary)) "pass" else "not_available",
    evidence = .gp3_face_reporting_reactivity_evidence(reactivity_summary),
    recommendation = "Define baseline and response windows and report reactivity as response minus baseline."
  )
  i <- i + 1L

  rows[[i]] <- .gp3_face_reporting_row(
    section = "Modelling",
    item = "Multimodal or face-window model object is available when models are reported",
    status = if (!is.null(multimodal_model)) "pass" else "not_available",
    evidence = .gp3_face_reporting_model_evidence(multimodal_model),
    recommendation = "Report formula, predictors, covariates, random effects, family, missing-data handling, and model sample size."
  )
  i <- i + 1L

  if (include_interpretation_cautions) {
    rows[[i]] <- .gp3_face_reporting_row(
      section = "Interpretation",
      item = "Facial-behaviour variables are not interpreted as direct emotion measures",
      status = "review",
      evidence = "Manual manuscript/reporting review required.",
      recommendation = "Use cautious language such as facial-behaviour measure, action-unit intensity, confidence, synchronisation coverage, or window-level feature."
    )
    i <- i + 1L

    rows[[i]] <- .gp3_face_reporting_row(
      section = "Interpretation",
      item = "Unsupported claims are avoided",
      status = "review",
      evidence = "Manual manuscript/reporting review required.",
      recommendation = "Avoid claims of true emotion detection, hidden affect, micro-expression evidence, diagnosis, or causal mechanism without design support."
    )
  }

  out <- .gp3_face_reporting_bind_rows(rows)
  out <- tibble::as_tibble(out)
  class(out) <- c("gp3_face_reporting_checklist", class(out))

  out
}


#' Report external facial-behaviour QC and reporting readiness
#'
#' Creates a compact markdown or list report from facial-behaviour quality,
#' synchronisation, window-summary, reactivity, and modelling objects. The report
#' is designed for transparent methods/supplementary reporting. It does not
#' infer facial expressions or emotional states.
#'
#' @param face_data Optional imported or standardised face-analysis data.
#' @param quality_audit Optional object returned by
#'   `audit_gazepoint_face_quality()`.
#' @param sync_audit Optional object returned by `audit_gazepoint_face_sync()`.
#' @param window_summary Optional object returned by
#'   `summarize_gazepoint_face_windows()`.
#' @param reactivity_summary Optional object returned by
#'   `summarize_gazepoint_face_reactivity()`.
#' @param multimodal_model Optional object returned by a gp3tools multimodal
#'   modelling helper.
#' @param checklist Optional checklist returned by
#'   `create_gazepoint_face_reporting_checklist()`. If `NULL`, one is created.
#' @param output Output format. `"markdown"` returns a character vector;
#'   `"list"` returns a structured list.
#' @param include_cautions Should interpretation cautions be included?
#'
#' @return A markdown character vector with class `gp3_face_qc_report`, or a list
#'   with class `gp3_face_qc_report_list`.
#' @export
#'
#' @examples
#' quality <- list(
#'   overview = data.frame(
#'     n_rows = 10,
#'     valid_percent = 95,
#'     face_quality_status = "pass"
#'   ),
#'   issue_summary = data.frame(
#'     issue = "missing_confidence",
#'     n_groups_affected = 0
#'   )
#' )
#' class(quality) <- c("gp3_face_quality_audit", "list")
#'
#' report_gazepoint_face_qc(quality_audit = quality)
report_gazepoint_face_qc <- function(
    face_data = NULL,
    quality_audit = NULL,
    sync_audit = NULL,
    window_summary = NULL,
    reactivity_summary = NULL,
    multimodal_model = NULL,
    checklist = NULL,
    output = c("markdown", "list"),
    include_cautions = TRUE) {
  output <- match.arg(output)

  if (is.null(checklist)) {
    checklist <- create_gazepoint_face_reporting_checklist(
      face_data = face_data,
      quality_audit = quality_audit,
      sync_audit = sync_audit,
      window_summary = window_summary,
      reactivity_summary = reactivity_summary,
      multimodal_model = multimodal_model,
      include_interpretation_cautions = include_cautions
    )
  }

  if (!is.data.frame(checklist)) {
    stop("`checklist` must be a data frame.", call. = FALSE)
  }

  sections <- list(
    checklist = tibble::as_tibble(checklist),
    quality_overview = .gp3_face_reporting_extract_table(
      quality_audit,
      "overview"
    ),
    quality_issues = .gp3_face_reporting_extract_table(
      quality_audit,
      "issue_summary"
    ),
    sync_overview = .gp3_face_reporting_extract_table(sync_audit, "overview"),
    sync_issues = .gp3_face_reporting_extract_table(
      sync_audit,
      "issue_summary"
    ),
    window_summary_overview = .gp3_face_reporting_window_report_table(
      window_summary
    ),
    reactivity_overview = .gp3_face_reporting_reactivity_report_table(
      reactivity_summary
    ),
    model_summary = .gp3_face_reporting_model_table(multimodal_model),
    cautions = .gp3_face_reporting_cautions(include_cautions)
  )

  if (identical(output, "list")) {
    out <- sections
    class(out) <- c("gp3_face_qc_report_list", class(out))
    return(out)
  }

  out <- .gp3_face_reporting_markdown(sections)
  class(out) <- c("gp3_face_qc_report", class(out))

  out
}


.gp3_face_reporting_row <- function(section,
                                    item,
                                    status,
                                    evidence,
                                    recommendation) {
  data.frame(
    section = section,
    item = item,
    status = status,
    evidence = evidence,
    recommendation = recommendation,
    stringsAsFactors = FALSE
  )
}


.gp3_face_reporting_data_evidence <- function(x) {
  if (is.null(x)) {
    return("No object supplied.")
  }

  if (!is.data.frame(x)) {
    return(paste0("Object supplied with class: ", paste(class(x), collapse = ", ")))
  }

  paste0(
    nrow(x),
    " row(s), ",
    ncol(x),
    " column(s)."
  )
}


.gp3_face_reporting_standard_cols_status <- function(face_data) {
  if (is.null(face_data) || !is.data.frame(face_data)) {
    return("not_available")
  }

  required <- c("face_time_sec", "face_confidence", "face_valid")
  present <- intersect(required, names(face_data))

  if (length(present) == length(required)) {
    return("pass")
  }

  if (length(present) > 0L) {
    return("warn")
  }

  "not_available"
}


.gp3_face_reporting_standard_cols_evidence <- function(face_data) {
  if (is.null(face_data) || !is.data.frame(face_data)) {
    return("No face-data table supplied.")
  }

  required <- c(
    "face_frame",
    "face_time_sec",
    "face_confidence",
    "face_success",
    "face_valid"
  )

  present <- intersect(required, names(face_data))
  missing <- setdiff(required, names(face_data))

  paste0(
    "Present: ",
    if (length(present) > 0L) paste(present, collapse = ", ") else "none",
    ". Missing: ",
    if (length(missing) > 0L) paste(missing, collapse = ", ") else "none",
    "."
  )
}


.gp3_face_reporting_object_evidence <- function(x, expected_class) {
  if (is.null(x)) {
    return("No object supplied.")
  }

  paste0(
    "Class: ",
    paste(class(x), collapse = ", "),
    if (inherits(x, expected_class)) " (expected class present)." else " (expected class not found)."
  )
}


.gp3_face_reporting_audit_status <- function(x, overview_col) {
  if (is.null(x) || is.null(x$overview) || !is.data.frame(x$overview)) {
    return(list(status = "not_available", evidence = "No audit overview supplied."))
  }

  overview <- as.data.frame(x$overview, stringsAsFactors = FALSE)

  if (!overview_col %in% names(overview) || nrow(overview) < 1L) {
    return(list(status = "unknown", evidence = "Audit overview is missing the status column."))
  }

  raw_status <- as.character(overview[[overview_col]][[1L]])
  status <- .gp3_face_reporting_status_map(raw_status)

  evidence_cols <- intersect(
    c(
      "n_rows",
      "valid_percent",
      "matched_percent",
      "face_quality_status",
      "face_sync_audit_status",
      "max_abs_diff_sec",
      "max_time_gap_sec"
    ),
    names(overview)
  )

  evidence <- if (length(evidence_cols) > 0L) {
    paste(
      paste0(evidence_cols, "=", unlist(overview[1L, evidence_cols], use.names = FALSE)),
      collapse = "; "
    )
  } else {
    paste0("Status=", raw_status)
  }

  list(status = status, evidence = evidence)
}


.gp3_face_reporting_status_map <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return("unknown")
  }

  x <- tolower(x)

  if (x %in% c("pass", "ok")) {
    return("pass")
  }

  if (x %in% c("warn", "warning", "review")) {
    return("warn")
  }

  if (x %in% c("fail", "failed")) {
    return("fail")
  }

  if (x %in% c("unknown", "not_available")) {
    return(x)
  }

  "review"
}


.gp3_face_reporting_issue_status <- function(x) {
  if (is.null(x) || is.null(x$issue_summary) || !is.data.frame(x$issue_summary)) {
    return("not_available")
  }

  issues <- as.data.frame(x$issue_summary, stringsAsFactors = FALSE)

  if (!"n_groups_affected" %in% names(issues)) {
    return("unknown")
  }

  affected <- suppressWarnings(as.numeric(issues$n_groups_affected))

  if (any(affected > 0, na.rm = TRUE)) {
    return("review")
  }

  "pass"
}


.gp3_face_reporting_issue_evidence <- function(x) {
  if (is.null(x) || is.null(x$issue_summary) || !is.data.frame(x$issue_summary)) {
    return("No issue summary supplied.")
  }

  issues <- as.data.frame(x$issue_summary, stringsAsFactors = FALSE)

  if (!all(c("issue", "n_groups_affected") %in% names(issues))) {
    return(paste0(nrow(issues), " issue-summary row(s) supplied."))
  }

  affected <- issues[issues$n_groups_affected > 0, , drop = FALSE]

  if (nrow(affected) < 1L) {
    return("No affected groups reported in issue summary.")
  }

  paste(
    paste0(affected$issue, "=", affected$n_groups_affected),
    collapse = "; "
  )
}


.gp3_face_reporting_window_status <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return("not_available")
  }

  if (nrow(x) < 1L) {
    return("fail")
  }

  if ("n_used" %in% names(x) && any(x$n_used < 1, na.rm = TRUE)) {
    return("review")
  }

  "pass"
}


.gp3_face_reporting_window_evidence <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return("No window-summary table supplied.")
  }

  evidence <- paste0(nrow(x), " window-summary row(s).")

  if ("n_used" %in% names(x)) {
    evidence <- paste0(
      evidence,
      " n_used range: ",
      min(x$n_used, na.rm = TRUE),
      "-",
      max(x$n_used, na.rm = TRUE),
      "."
    )
  }

  evidence
}


.gp3_face_reporting_reactivity_evidence <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return("No reactivity-summary table supplied.")
  }

  measures <- if ("measure" %in% names(x)) {
    unique(as.character(x$measure))
  } else {
    character(0)
  }

  paste0(
    nrow(x),
    " reactivity row(s)",
    if (length(measures) > 0L) {
      paste0("; measure(s): ", paste(measures, collapse = ", "))
    } else {
      ""
    },
    "."
  )
}


.gp3_face_reporting_model_evidence <- function(x) {
  if (is.null(x)) {
    return("No model object supplied.")
  }

  if (is.list(x) && !is.null(x$settings)) {
    outcome <- x$settings$outcome
    n_rows_model <- x$settings$n_rows_model
    return(
      paste0(
        "Outcome: ",
        outcome,
        "; model rows: ",
        n_rows_model,
        "; class: ",
        paste(class(x), collapse = ", "),
        "."
      )
    )
  }

  paste0("Model-like object supplied with class: ", paste(class(x), collapse = ", "))
}


.gp3_face_reporting_extract_table <- function(x, name) {
  if (is.null(x) || is.null(x[[name]]) || !is.data.frame(x[[name]])) {
    return(tibble::tibble())
  }

  tibble::as_tibble(x[[name]])
}


.gp3_face_reporting_window_report_table <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return(tibble::tibble())
  }

  cols <- intersect(
    c(
      "participant_id",
      "trial_id",
      "face_window_label",
      "n_rows",
      "n_used",
      "valid_percent",
      "face_confidence_mean"
    ),
    names(x)
  )

  if (length(cols) < 1L) {
    return(tibble::as_tibble(x))
  }

  tibble::as_tibble(x[, cols, drop = FALSE])
}


.gp3_face_reporting_reactivity_report_table <- function(x) {
  if (is.null(x) || !is.data.frame(x)) {
    return(tibble::tibble())
  }

  cols <- intersect(
    c(
      "participant_id",
      "trial_id",
      "measure",
      "statistic",
      "baseline_window",
      "response_window",
      "baseline_value",
      "response_value",
      "reactivity",
      "absolute_reactivity"
    ),
    names(x)
  )

  if (length(cols) < 1L) {
    return(tibble::as_tibble(x))
  }

  tibble::as_tibble(x[, cols, drop = FALSE])
}


.gp3_face_reporting_model_table <- function(x) {
  if (is.null(x) || !is.list(x) || is.null(x$settings)) {
    return(tibble::tibble())
  }

  tibble::tibble(
    model_class = paste(class(x), collapse = ", "),
    outcome = x$settings$outcome,
    predictors = paste(x$settings$predictors, collapse = ", "),
    covariates = paste(x$settings$covariates, collapse = ", "),
    random_effects = if (is.null(x$settings$random_effects)) {
      NA_character_
    } else {
      x$settings$random_effects
    },
    n_rows_input = x$settings$n_rows_input,
    n_rows_model = x$settings$n_rows_model
  )
}


.gp3_face_reporting_cautions <- function(include_cautions = TRUE) {
  if (!include_cautions) {
    return(character(0))
  }

  c(
    "External facial-behaviour outputs should be reported as algorithmic or tool-derived measurements, not as direct evidence of emotional states.",
    "Report face-data quality, confidence, validity, synchronisation, and window coverage before interpreting model estimates.",
    "Avoid claims of true emotion detection, hidden affect, psychological diagnosis, micro-expression evidence, or causal mechanism unless the study design and validation evidence support them."
  )
}


.gp3_face_reporting_markdown <- function(sections) {
  lines <- c(
    "# External facial-behaviour QC report",
    "",
    "This report summarises technical reporting readiness for external facial-behaviour data used with Gazepoint workflows. It does not infer facial expressions or emotional states.",
    "",
    "## Reporting checklist",
    "",
    .gp3_face_reporting_table_lines(sections$checklist),
    "",
    "## Face-data quality overview",
    "",
    .gp3_face_reporting_table_or_missing(sections$quality_overview),
    "",
    "## Face-data quality issues",
    "",
    .gp3_face_reporting_table_or_missing(sections$quality_issues),
    "",
    "## Synchronisation overview",
    "",
    .gp3_face_reporting_table_or_missing(sections$sync_overview),
    "",
    "## Synchronisation issues",
    "",
    .gp3_face_reporting_table_or_missing(sections$sync_issues),
    "",
    "## Window-summary overview",
    "",
    .gp3_face_reporting_table_or_missing(sections$window_summary_overview),
    "",
    "## Reactivity overview",
    "",
    .gp3_face_reporting_table_or_missing(sections$reactivity_overview),
    "",
    "## Model summary",
    "",
    .gp3_face_reporting_table_or_missing(sections$model_summary)
  )

  if (length(sections$cautions) > 0L) {
    lines <- c(
      lines,
      "",
      "## Interpretation cautions",
      "",
      paste0("- ", sections$cautions)
    )
  }

  lines
}


.gp3_face_reporting_table_or_missing <- function(x) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) < 1L) {
    return("_Not supplied._")
  }

  .gp3_face_reporting_table_lines(x)
}


.gp3_face_reporting_table_lines <- function(x) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  if (ncol(x) < 1L) {
    return("_No columns._")
  }

  x[] <- lapply(x, function(col) {
    col <- as.character(col)
    col[is.na(col)] <- ""
    gsub("\\|", "/", col)
  })

  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  body <- apply(x, 1L, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })

  c(header, divider, body)
}


.gp3_face_reporting_bind_rows <- function(x) {
  all_names <- unique(unlist(lapply(x, names), use.names = FALSE))

  x <- lapply(x, function(dat) {
    missing <- setdiff(all_names, names(dat))
    for (m in missing) {
      dat[[m]] <- NA
    }
    dat[, all_names, drop = FALSE]
  })

  do.call(rbind, x)
}
