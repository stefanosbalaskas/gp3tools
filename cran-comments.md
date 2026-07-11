## Resubmission

This is a major release of gp3tools for version 2.0.0.

## Release summary

gp3tools 2.0.0 expands the package from a Gazepoint import and QC toolkit into
a broader reproducible workflow system for Gazepoint GP3 / Gazepoint Analysis
exports. The release includes expanded pupil, AOI, fixation/transition,
time-course, diagnostics, reporting, ecosystem-adapter, external face-data,
and website-documentation layers.

This release also adds Zenodo citation metadata for the 2.0.0 software archive:

* DOI: 10.5281/zenodo.21292384

## Test environments

* Local Windows 11 x64, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macos-latest, and ubuntu devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no known reverse dependencies.

## Notes

The package includes synthetic example datasets and synthetic demonstration
exports. These are artificial and are not private participant data.

The package uses optional suggested packages for some extended workflows.
When optional packages are unavailable, affected helpers return controlled
status messages or skip optional branches rather than failing core workflows.
