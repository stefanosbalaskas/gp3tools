## Test environments

* Local Windows 11, R 4.6.0: `devtools::check()` and `devtools::check(cran = TRUE, args = c("--no-manual"))`
* GitHub Actions: Windows release, macOS release, Ubuntu release, Ubuntu devel
* win-builder R-devel: 1 NOTE, expected for first CRAN submission and `Gazepoint` product-name spelling

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

* This is the first CRAN submission of `gp3tools`.
* The package provides tools for importing, validating, analysing, visualising, and reporting Gazepoint GP3 / Gazepoint Analysis CSV exports.
* The package includes lightweight synthetic example data for reproducible examples and vignettes.
* A larger paper-only synthetic showcase is kept in the GitHub repository but excluded from the R package build using `.Rbuildignore`.
* `CITATION.cff`, `.github/`, and other repository-maintenance files are excluded from the package build.

## Reverse dependencies

There are no reverse dependencies because this is a first CRAN submission.

## win-builder notes

The first win-builder R-devel check returned 1 NOTE for a new submission, possible DESCRIPTION spelling of the product name `Gazepoint`, and an old-style `inst/CITATION` author specification. The `inst/CITATION` file has been updated to use `person()` inside `c()`. `Gazepoint` is the name of the eye-tracking hardware/software ecosystem targeted by the package.

## Notes explained

* win-builder R-devel reports 1 NOTE.
* The NOTE is expected for a first CRAN submission.
* `Gazepoint` is the official name of the eye-tracking hardware/software ecosystem targeted by the package.
* The previous `inst/CITATION` author-format NOTE was fixed by using `person()` inside `c()`.
