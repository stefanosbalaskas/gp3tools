#' gp3tools user-facing naming policy
#'
#' Documents the package policy that British English `summarise_*` names are
#' canonical for new summary helpers. Existing American English
#' `summarize_*` exports remain available for backward compatibility.
#'
#' @return A list describing the policy and migration rules.
#' @export
gp3tools_naming_policy <- function() {
  structure(
    list(
      language = "British English",
      canonical_prefix = "summarise_",
      compatibility_prefix = "summarize_",
      effective_development_version = "2.2.0",
      rules = data.frame(
        rule = c(
          "new_summary_helpers",
          "existing_british_names",
          "existing_american_names",
          "documentation",
          "deprecation"
        ),
        policy = c(
          "Use summarise_* for newly introduced summary functions.",
          "Retain existing summarise_* names unchanged.",
          paste(
            "Retain exported summarize_* names and provide a British alias",
            "when one is missing."
          ),
          paste(
            "Present British names as canonical while explicitly listing",
            "American compatibility aliases."
          ),
          paste(
            "Do not deprecate or remove an American spelling without a",
            "separate versioned migration decision."
          )
        ),
        stringsAsFactors = FALSE
      )
    ),
    class = c("gp3tools_naming_policy", "list")
  )
}

#' Audit British and American summary-function spellings
#'
#' @param exports Optional character vector of exported function names. When
#'   `NULL`, current gp3tools namespace exports are inspected.
#'
#' @return A `"gazepoint_naming_audit"` object containing paired names and an
#'   overall status.
#' @export
audit_gazepoint_naming_consistency <- function(exports = NULL) {
  if (is.null(exports)) {
    exports <- getNamespaceExports("gp3tools")
  }
  exports <- unique(as.character(exports))
  exports <- exports[!is.na(exports) & nzchar(exports)]

  british <- grep("^summarise_", exports, value = TRUE)
  american <- grep("^summarize_", exports, value = TRUE)
  stems <- unique(c(
    sub("^summarise_", "", british),
    sub("^summarize_", "", american)
  ))
  stems <- sort(stems)

  if (length(stems) == 0L) {
    pairs <- data.frame(
      stem = character(),
      british_name = character(),
      american_name = character(),
      british_exported = logical(),
      american_exported = logical(),
      canonical_name = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
  } else {
    pairs <- do.call(
      rbind,
      lapply(stems, function(stem) {
        british_name <- paste0("summarise_", stem)
        american_name <- paste0("summarize_", stem)
        british_exported <- british_name %in% exports
        american_exported <- american_name %in% exports
        status <- if (british_exported && american_exported) {
          "paired"
        } else if (british_exported) {
          "canonical_only"
        } else {
          "missing_british_alias"
        }
        data.frame(
          stem = stem,
          british_name = british_name,
          american_name = american_name,
          british_exported = british_exported,
          american_exported = american_exported,
          canonical_name = british_name,
          status = status,
          stringsAsFactors = FALSE
        )
      })
    )
    rownames(pairs) <- NULL
  }

  summary <- data.frame(
    status = if (
      any(pairs$status == "missing_british_alias")
    ) {
      "needs_review"
    } else {
      "pass"
    },
    n_summary_stems = nrow(pairs),
    n_paired = sum(pairs$status == "paired"),
    n_canonical_only = sum(pairs$status == "canonical_only"),
    n_missing_british_alias = sum(
      pairs$status == "missing_british_alias"
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      summary = summary,
      pairs = pairs,
      policy = gp3tools_naming_policy()
    ),
    class = c("gazepoint_naming_audit", "list")
  )
}

#' Write a gp3tools naming-consistency audit
#'
#' @param x A `"gazepoint_naming_audit"` object.
#' @param output_file CSV output path.
#'
#' @return Normalized path to the written file.
#' @export
write_gazepoint_naming_audit <- function(x, output_file) {
  if (!inherits(x, "gazepoint_naming_audit")) {
    stop(
      "`x` must be a gazepoint_naming_audit object.",
      call. = FALSE
    )
  }
  output_file <- as.character(output_file)
  if (length(output_file) != 1L || is.na(output_file) ||
      !nzchar(trimws(output_file))) {
    stop(
      "`output_file` must be one non-empty path.",
      call. = FALSE
    )
  }
  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x$pairs, output_file, row.names = FALSE)
  normalizePath(output_file, winslash = "/", mustWork = TRUE)
}

#' @export
print.gp3tools_naming_policy <- function(x, ...) {
  cat("gp3tools naming policy\n")
  cat("  Canonical language: ", x$language, "\n", sep = "")
  cat("  Canonical prefix: ", x$canonical_prefix, "\n", sep = "")
  cat(
    "  Compatibility prefix: ",
    x$compatibility_prefix,
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @export
print.gazepoint_naming_audit <- function(x, ...) {
  cat("gp3tools naming-consistency audit\n")
  cat("  Status: ", x$summary$status, "\n", sep = "")
  cat("  Summary stems: ", x$summary$n_summary_stems, "\n", sep = "")
  cat(
    "  Missing British aliases: ",
    x$summary$n_missing_british_alias,
    "\n",
    sep = ""
  )
  invisible(x)
}
