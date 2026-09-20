## R CMD check results

0 errors | 0 warnings | 1 note

## Note

- `eyeprocess` is an optional suggested scientific engine that is installed from its certified GitHub release in project CI. `gp3tools` remains installable without it; functionality that delegates to that engine handles its absence explicitly.

## Changes in this release

- Updates gp3tools from version 2.3.0 to version 2.4.0.
- Adds standardized Gazepoint spatial-quality adapters for accuracy, precision, BCEA, sampling behavior, data loss, dashboards, and reporting while retaining scientific formula ownership in eyeprocess.
- Adds Gazepoint AOI perturbation-uncertainty adapters with explicit coordinate, geometry, observation-level, boundary, overlap, provenance, sensitivity, and failure contracts.
- Adds censored gaze-latency survival adapters for right-censoring, repeated-participant Cox and AFT analysis, diagnostics, reporting, and reproducibility workflows delegated to eyeprocess.
- Expands methodological guidance, worked examples, plots, interpretation, limitations, reporting guidance, and website/API navigation.
- Hardens repository/site contract testing and cross-platform release certification, including Ubuntu oldrel-1.
- Adds no new mandatory package dependency.
- Preserves established public interfaces and scientific package boundaries.

## Downstream dependencies

There are no known downstream CRAN dependencies.

## Test environments

- Local Windows 11, R 4.6.1.
- GitHub Actions: Windows release, macOS release, Ubuntu release, Ubuntu oldrel-1, and Ubuntu R-devel.
- The exact 2.4.0 release-preparation commit will be re-certified on the same five-lane GitHub Actions matrix before creation of the immutable v2.4.0 tag.
