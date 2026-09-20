gp3tools_test_source_root <- function() {
  candidates <- unique(c(
    Sys.getenv("GP3TOOLS_SOURCE_ROOT", unset = ""),
    Sys.getenv("GITHUB_WORKSPACE", unset = ""),
    normalizePath(
      file.path(testthat::test_path(), "..", ".."),
      winslash = "/",
      mustWork = FALSE
    ),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  ))

  candidates <- candidates[nzchar(candidates)]

  for (candidate in candidates) {
    description <- file.path(candidate, "DESCRIPTION")
    if (!file.exists(description)) {
      next
    }

    package_name <- tryCatch(
      unname(read.dcf(description)[1L, "Package"]),
      error = function(...) NA_character_
    )

    if (identical(package_name, "gp3tools")) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  NULL
}

gp3tools_require_source_contract_root <- function() {
  root <- gp3tools_test_source_root()
  testthat::skip_if(
    is.null(root),
    paste(
      "Repository-level website contract requires a gp3tools source",
      "checkout; package-tarball checks do not contain build-ignored site files."
    )
  )
  root
}
