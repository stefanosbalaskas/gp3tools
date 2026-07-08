# Create a reporting checklist for external facial-behaviour workflows

Creates a compact checklist for reporting external facial-behaviour
analyses alongside Gazepoint data. The checklist is designed for
reviewer-facing transparency: it records whether import, quality
auditing, synchronisation, window summaries, reactivity summaries, and
modelling outputs are available. It also includes interpretation
cautions. The helper does not infer facial expressions or emotional
states.

## Usage

``` r
create_gazepoint_face_reporting_checklist(
  face_data = NULL,
  quality_audit = NULL,
  sync_audit = NULL,
  window_summary = NULL,
  reactivity_summary = NULL,
  multimodal_model = NULL,
  include_interpretation_cautions = TRUE
)
```

## Arguments

- face_data:

  Optional imported or standardised face-analysis data.

- quality_audit:

  Optional object returned by
  [`audit_gazepoint_face_quality()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_quality.md).

- sync_audit:

  Optional object returned by
  [`audit_gazepoint_face_sync()`](https://stefanosbalaskas.github.io/gp3tools/reference/audit_gazepoint_face_sync.md).

- window_summary:

  Optional object returned by
  [`summarize_gazepoint_face_windows()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_windows.md).

- reactivity_summary:

  Optional object returned by
  [`summarize_gazepoint_face_reactivity()`](https://stefanosbalaskas.github.io/gp3tools/reference/summarize_gazepoint_face_reactivity.md).

- multimodal_model:

  Optional object returned by
  [`fit_gazepoint_face_window_lmm()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_face_window_lmm.md)
  or
  [`fit_gazepoint_multimodal_response_model()`](https://stefanosbalaskas.github.io/gp3tools/reference/fit_gazepoint_multimodal_response_model.md).

- include_interpretation_cautions:

  Should interpretation-caution checklist items be included?

## Value

A tibble with class `gp3_face_reporting_checklist`.

## Examples

``` r
quality <- list(
  overview = data.frame(
    n_rows = 10,
    valid_percent = 95,
    face_quality_status = "pass"
  ),
  issue_summary = data.frame(
    issue = "missing_confidence",
    n_groups_affected = 0
  )
)
class(quality) <- c("gp3_face_quality_audit", "list")

create_gazepoint_face_reporting_checklist(quality_audit = quality)
#> # A tibble: 13 × 5
#>    section              item                      status evidence recommendation
#>    <chr>                <chr>                     <chr>  <chr>    <chr>         
#>  1 Input and provenance External face-analysis d… not_a… No obje… Provide impor…
#>  2 Input and provenance Standardised face column… not_a… No face… Report standa…
#>  3 Quality control      Face-data quality audit … pass   Class: … Use audit_gaz…
#>  4 Quality control      Face-data quality status… pass   n_rows=… Report valid-…
#>  5 Quality control      Quality issues are docum… pass   No affe… Document grou…
#>  6 Synchronisation      Face-data synchronisatio… not_a… No obje… Use audit_gaz…
#>  7 Synchronisation      Synchronisation status i… not_a… No audi… Report matchi…
#>  8 Window summaries     Face-window summary is a… not_a… No obje… Report window…
#>  9 Window summaries     Window-summary coverage … not_a… No wind… Report n_rows…
#> 10 Reactivity summaries Baseline-to-response rea… not_a… No reac… Define baseli…
#> 11 Modelling            Multimodal or face-windo… not_a… No mode… Report formul…
#> 12 Interpretation       Facial-behaviour variabl… review Manual … Use cautious …
#> 13 Interpretation       Unsupported claims are a… review Manual … Avoid claims …
```
