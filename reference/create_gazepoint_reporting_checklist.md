# Create a Gazepoint reporting checklist

Create an auto-generated reporting checklist for Gazepoint/gp3tools
analyses. The checklist summarises whether key dataset, preprocessing,
quality-control, AOI, pupil, modelling, sensitivity, and reproducibility
elements are present or still need reporting.

## Usage

``` r
create_gazepoint_reporting_checklist(
  data = NULL,
  objects = NULL,
  analysis_type = c("general", "pupil", "aoi", "combined"),
  study_title = NULL,
  required_sections = NULL,
  include_optional = TRUE,
  name = "gazepoint_reporting_checklist"
)
```

## Arguments

- data:

  Optional Gazepoint or gp3tools data frame.

- objects:

  Optional list of gp3tools audit, model, workflow, readiness, or
  external-check objects.

- analysis_type:

  Analysis target. Options are `"general"`, `"pupil"`, `"aoi"`, and
  `"combined"`.

- study_title:

  Optional study title or short label.

- required_sections:

  Optional character vector of checklist item IDs that should be treated
  as required. If `NULL`, a default set is used.

- include_optional:

  Logical. If `TRUE`, include optional advanced-methods reporting items.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_reporting_checklist`.

## Details

This helper is intended as a reporting aid. It does not replace the
underlying audit, preprocessing, modelling, or readiness-gate functions.
