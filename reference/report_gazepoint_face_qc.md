# Report external facial-behaviour QC and reporting readiness

Creates a compact markdown or list report from facial-behaviour quality,
synchronisation, window-summary, reactivity, and modelling objects. The
report is designed for transparent methods/supplementary reporting. It
does not infer facial expressions or emotional states.

## Usage

``` r
report_gazepoint_face_qc(
  face_data = NULL,
  quality_audit = NULL,
  sync_audit = NULL,
  window_summary = NULL,
  reactivity_summary = NULL,
  multimodal_model = NULL,
  checklist = NULL,
  output = c("markdown", "list"),
  include_cautions = TRUE
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

  Optional object returned by a gp3tools multimodal modelling helper.

- checklist:

  Optional checklist returned by
  [`create_gazepoint_face_reporting_checklist()`](https://stefanosbalaskas.github.io/gp3tools/reference/create_gazepoint_face_reporting_checklist.md).
  If `NULL`, one is created.

- output:

  Output format. `"markdown"` returns a character vector; `"list"`
  returns a structured list.

- include_cautions:

  Should interpretation cautions be included?

## Value

A markdown character vector with class `gp3_face_qc_report`, or a list
with class `gp3_face_qc_report_list`.

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

report_gazepoint_face_qc(quality_audit = quality)
#>  [1] "# External facial-behaviour QC report"                                                                                                                                                                                                                                                                 
#>  [2] ""                                                                                                                                                                                                                                                                                                      
#>  [3] "This report summarises technical reporting readiness for external facial-behaviour data used with Gazepoint workflows. It does not infer facial expressions or emotional states."                                                                                                                      
#>  [4] ""                                                                                                                                                                                                                                                                                                      
#>  [5] "## Reporting checklist"                                                                                                                                                                                                                                                                                
#>  [6] ""                                                                                                                                                                                                                                                                                                      
#>  [7] "| section | item | status | evidence | recommendation |"                                                                                                                                                                                                                                               
#>  [8] "| --- | --- | --- | --- | --- |"                                                                                                                                                                                                                                                                       
#>  [9] "| Input and provenance | External face-analysis data are available | not_available | No object supplied. | Provide imported or standardised external face-analysis data when facial-behaviour analyses are reported. |"                                                                                
#> [10] "| Input and provenance | Standardised face columns are available | not_available | No face-data table supplied. | Report standardised timing, frame, confidence, success, and validity fields where available. |"                                                                                      
#> [11] "| Quality control | Face-data quality audit is available | pass | Class: gp3_face_quality_audit, list (expected class present). | Use audit_gazepoint_face_quality() before reporting facial-behaviour summaries. |"                                                                                   
#> [12] "| Quality control | Face-data quality status is acceptable | pass | n_rows=10; valid_percent=95; face_quality_status=pass | Report valid-row percentage, confidence coverage, duplicate-frame checks, and timing-gap checks. |"                                                                        
#> [13] "| Quality control | Quality issues are documented | pass | No affected groups reported in issue summary. | Document groups requiring review and explain any exclusions or sensitivity analyses. |"                                                                                                     
#> [14] "| Synchronisation | Face-data synchronisation audit is available | not_available | No object supplied. | Use audit_gazepoint_face_sync() when face data are aligned to Gazepoint rows. |"                                                                                                              
#> [15] "| Synchronisation | Synchronisation status is acceptable | not_available | No audit overview supplied. | Report matching method, tolerance, matched percentage, unmatched rows, and timing differences. |"                                                                                             
#> [16] "| Window summaries | Face-window summary is available | not_available | No object supplied. | Report window definitions, grouping variables, validity filtering, and summarised facial-behaviour measures. |"                                                                                          
#> [17] "| Window summaries | Window-summary coverage is documented | not_available | No window-summary table supplied. | Report n_rows, n_used, valid_percent, confidence summaries, and measure summaries for each window. |"                                                                                 
#> [18] "| Reactivity summaries | Baseline-to-response reactivity is available when used | not_available | No reactivity-summary table supplied. | Define baseline and response windows and report reactivity as response minus baseline. |"                                                                    
#> [19] "| Modelling | Multimodal or face-window model object is available when models are reported | not_available | No model object supplied. | Report formula, predictors, covariates, random effects, family, missing-data handling, and model sample size. |"                                              
#> [20] "| Interpretation | Facial-behaviour variables are not interpreted as direct emotion measures | review | Manual manuscript/reporting review required. | Use cautious language such as facial-behaviour measure, action-unit intensity, confidence, synchronisation coverage, or window-level feature. |"
#> [21] "| Interpretation | Unsupported claims are avoided | review | Manual manuscript/reporting review required. | Avoid claims of true emotion detection, hidden affect, micro-expression evidence, diagnosis, or causal mechanism without design support. |"                                                
#> [22] ""                                                                                                                                                                                                                                                                                                      
#> [23] "## Face-data quality overview"                                                                                                                                                                                                                                                                         
#> [24] ""                                                                                                                                                                                                                                                                                                      
#> [25] "| n_rows | valid_percent | face_quality_status |"                                                                                                                                                                                                                                                      
#> [26] "| --- | --- | --- |"                                                                                                                                                                                                                                                                                   
#> [27] "| 10 | 95 | pass |"                                                                                                                                                                                                                                                                                    
#> [28] ""                                                                                                                                                                                                                                                                                                      
#> [29] "## Face-data quality issues"                                                                                                                                                                                                                                                                           
#> [30] ""                                                                                                                                                                                                                                                                                                      
#> [31] "| issue | n_groups_affected |"                                                                                                                                                                                                                                                                         
#> [32] "| --- | --- |"                                                                                                                                                                                                                                                                                         
#> [33] "| missing_confidence | 0 |"                                                                                                                                                                                                                                                                            
#> [34] ""                                                                                                                                                                                                                                                                                                      
#> [35] "## Synchronisation overview"                                                                                                                                                                                                                                                                           
#> [36] ""                                                                                                                                                                                                                                                                                                      
#> [37] "_Not supplied._"                                                                                                                                                                                                                                                                                       
#> [38] ""                                                                                                                                                                                                                                                                                                      
#> [39] "## Synchronisation issues"                                                                                                                                                                                                                                                                             
#> [40] ""                                                                                                                                                                                                                                                                                                      
#> [41] "_Not supplied._"                                                                                                                                                                                                                                                                                       
#> [42] ""                                                                                                                                                                                                                                                                                                      
#> [43] "## Window-summary overview"                                                                                                                                                                                                                                                                            
#> [44] ""                                                                                                                                                                                                                                                                                                      
#> [45] "_Not supplied._"                                                                                                                                                                                                                                                                                       
#> [46] ""                                                                                                                                                                                                                                                                                                      
#> [47] "## Reactivity overview"                                                                                                                                                                                                                                                                                
#> [48] ""                                                                                                                                                                                                                                                                                                      
#> [49] "_Not supplied._"                                                                                                                                                                                                                                                                                       
#> [50] ""                                                                                                                                                                                                                                                                                                      
#> [51] "## Model summary"                                                                                                                                                                                                                                                                                      
#> [52] ""                                                                                                                                                                                                                                                                                                      
#> [53] "_Not supplied._"                                                                                                                                                                                                                                                                                       
#> [54] ""                                                                                                                                                                                                                                                                                                      
#> [55] "## Interpretation cautions"                                                                                                                                                                                                                                                                            
#> [56] ""                                                                                                                                                                                                                                                                                                      
#> [57] "- External facial-behaviour outputs should be reported as algorithmic or tool-derived measurements, not as direct evidence of emotional states."                                                                                                                                                       
#> [58] "- Report face-data quality, confidence, validity, synchronisation, and window coverage before interpreting model estimates."                                                                                                                                                                           
#> [59] "- Avoid claims of true emotion detection, hidden affect, psychological diagnosis, micro-expression evidence, or causal mechanism unless the study design and validation evidence support them."                                                                                                        
#> attr(,"class")
#> [1] "gp3_face_qc_report" "character"         
```
