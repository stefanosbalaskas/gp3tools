# Gazepoint AOI Robustness Reporting Bundle

## Purpose

A Gazepoint AOI robustness bundle should preserve **both** the
vendor-adapter settings and the untouched `eyeprocess` core evidence.
This makes the coordinate conversion reproducible while keeping
scientific interpretation in the vendor-neutral core.

## Recommended bundle

Include:

- the Gazepoint analysis plan;
- `adapter-settings` describing source x/y columns, coordinate unit,
  screen geometry, observation level, overlap policy, and boundary
  policy;
- the delegated core perturbation audit;
- assignment-stability and observation-level assignment tables;
- model results, inference stability, and failures;
- core provenance and report text;
- delegated geometry/assignment/coefficient figures;
- a file manifest when the project’s archival workflow requires one.

## Export the adapter and core layers

``` r

out_dir <- "workflow-output/gazepoint-aoi-reporting-bundle"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

capture.output(str(result$adapter_settings),
               file = file.path(out_dir, "adapter-settings.txt"))
core <- result$core_result
utils::write.csv(core$grid_result$audit,
                 file.path(out_dir, "perturbation-audit.csv"), row.names = FALSE)
utils::write.csv(core$stability$overall,
                 file.path(out_dir, "assignment-stability.csv"), row.names = FALSE)
utils::write.csv(core$models,
                 file.path(out_dir, "model-results.csv"), row.names = FALSE)
utils::write.csv(core$failures,
                 file.path(out_dir, "failures.csv"), row.names = FALSE)
writeLines(
  getExportedValue("eyeprocess", "report_aoi_sensitivity")(core),
  file.path(out_dir, "report.md")
)
```

## Reviewer-facing interpretation

The adapter layer answers **how Gazepoint coordinates became core
coordinates**. The core layer answers **how AOI geometry changes
affected assignments, features, and models**. Review both layers
together.

## Reporting example

> The robustness package preserves the original Gazepoint source columns
> and coordinate-unit conversion settings together with the delegated
> `eyeprocess` perturbation audit, assignment summaries, model outputs,
> failures, provenance, and visual diagnostics. The evidence bundle
> supports reproducibility of the declared workflow but is not a
> probability statement about the correctness of the AOI boundaries.

## Limitations and privacy

Observation-level exports may contain participant/trial identifiers.
Review them before sharing. A bundle also does not replace
tracking-quality, calibration, event-detector, or model diagnostics.

## API map

Use
[`audit_gazepoint_aoi_uncertainty()`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_aoi_uncertainty.md)
to document the adapter boundary,
[`run_gazepoint_aoi_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_aoi_uncertainty.md)
for the delegated result, and
[`plot_gazepoint_aoi_sensitivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/gazepoint_aoi_uncertainty.md)
for the visual evidence.
