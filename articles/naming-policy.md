# User-facing naming policy

## Canonical spelling

gp3tools uses British English `summarise_*` names as the canonical
spelling for new user-facing summary helpers.

``` r

gp3tools_naming_policy()
#> gp3tools naming policy
#>   Canonical language: British English
#>   Canonical prefix: summarise_
#>   Compatibility prefix: summarize_
```

Existing exported American English `summarize_*` functions remain
available for backward compatibility. The development installer creates
a British alias when an established American export does not yet have
one. No existing user code is removed or silently redirected to a
different analytical implementation.

## Audit the exported API

``` r

naming_audit <- audit_gazepoint_naming_consistency()
naming_audit
#> gp3tools naming-consistency audit
#>   Status: pass
#>   Summary stems: 31
#>   Missing British aliases: 0
naming_audit$pairs
#>                                    stem
#> 1                           aoi_samples
#> 2                             fixations
#> 3                         gazepoint_aoi
#> 4                 gazepoint_aoi_entries
#> 5             gazepoint_aoi_transitions
#> 6          gazepoint_aoi_trial_features
#> 7                 gazepoint_aoi_windows
#> 8                    gazepoint_clusters
#> 9         gazepoint_coordinate_coverage
#> 10                    gazepoint_emmeans
#> 11   gazepoint_event_detector_agreement
#> 12   gazepoint_event_detector_benchmark
#> 13               gazepoint_face_quality
#> 14            gazepoint_face_reactivity
#> 15               gazepoint_face_windows
#> 16            gazepoint_fixation_trials
#> 17              gazepoint_fixed_effects
#> 18                gazepoint_markovchain
#> 19                gazepoint_missingness
#> 20         gazepoint_multiverse_results
#> 21             gazepoint_phase_coverage
#> 22                      gazepoint_pupil
#> 23    gazepoint_pupil_response_features
#> 24       gazepoint_pupil_trial_features
#> 25              gazepoint_pupil_windows
#> 26                  gazepoint_qc_status
#> 27 gazepoint_scanpath_cluster_stability
#> 28                 gazepoint_semimarkov
#> 29              gazepoint_time_clusters
#> 30                   gazepoint_workflow
#> 31                     tracking_quality
#>                                      british_name
#> 1                           summarise_aoi_samples
#> 2                             summarise_fixations
#> 3                         summarise_gazepoint_aoi
#> 4                 summarise_gazepoint_aoi_entries
#> 5             summarise_gazepoint_aoi_transitions
#> 6          summarise_gazepoint_aoi_trial_features
#> 7                 summarise_gazepoint_aoi_windows
#> 8                    summarise_gazepoint_clusters
#> 9         summarise_gazepoint_coordinate_coverage
#> 10                    summarise_gazepoint_emmeans
#> 11   summarise_gazepoint_event_detector_agreement
#> 12   summarise_gazepoint_event_detector_benchmark
#> 13               summarise_gazepoint_face_quality
#> 14            summarise_gazepoint_face_reactivity
#> 15               summarise_gazepoint_face_windows
#> 16            summarise_gazepoint_fixation_trials
#> 17              summarise_gazepoint_fixed_effects
#> 18                summarise_gazepoint_markovchain
#> 19                summarise_gazepoint_missingness
#> 20         summarise_gazepoint_multiverse_results
#> 21             summarise_gazepoint_phase_coverage
#> 22                      summarise_gazepoint_pupil
#> 23    summarise_gazepoint_pupil_response_features
#> 24       summarise_gazepoint_pupil_trial_features
#> 25              summarise_gazepoint_pupil_windows
#> 26                  summarise_gazepoint_qc_status
#> 27 summarise_gazepoint_scanpath_cluster_stability
#> 28                 summarise_gazepoint_semimarkov
#> 29              summarise_gazepoint_time_clusters
#> 30                   summarise_gazepoint_workflow
#> 31                     summarise_tracking_quality
#>                                     american_name british_exported
#> 1                           summarize_aoi_samples             TRUE
#> 2                             summarize_fixations             TRUE
#> 3                         summarize_gazepoint_aoi             TRUE
#> 4                 summarize_gazepoint_aoi_entries             TRUE
#> 5             summarize_gazepoint_aoi_transitions             TRUE
#> 6          summarize_gazepoint_aoi_trial_features             TRUE
#> 7                 summarize_gazepoint_aoi_windows             TRUE
#> 8                    summarize_gazepoint_clusters             TRUE
#> 9         summarize_gazepoint_coordinate_coverage             TRUE
#> 10                    summarize_gazepoint_emmeans             TRUE
#> 11   summarize_gazepoint_event_detector_agreement             TRUE
#> 12   summarize_gazepoint_event_detector_benchmark             TRUE
#> 13               summarize_gazepoint_face_quality             TRUE
#> 14            summarize_gazepoint_face_reactivity             TRUE
#> 15               summarize_gazepoint_face_windows             TRUE
#> 16            summarize_gazepoint_fixation_trials             TRUE
#> 17              summarize_gazepoint_fixed_effects             TRUE
#> 18                summarize_gazepoint_markovchain             TRUE
#> 19                summarize_gazepoint_missingness             TRUE
#> 20         summarize_gazepoint_multiverse_results             TRUE
#> 21             summarize_gazepoint_phase_coverage             TRUE
#> 22                      summarize_gazepoint_pupil             TRUE
#> 23    summarize_gazepoint_pupil_response_features             TRUE
#> 24       summarize_gazepoint_pupil_trial_features             TRUE
#> 25              summarize_gazepoint_pupil_windows             TRUE
#> 26                  summarize_gazepoint_qc_status             TRUE
#> 27 summarize_gazepoint_scanpath_cluster_stability             TRUE
#> 28                 summarize_gazepoint_semimarkov             TRUE
#> 29              summarize_gazepoint_time_clusters             TRUE
#> 30                   summarize_gazepoint_workflow             TRUE
#> 31                     summarize_tracking_quality             TRUE
#>    american_exported                                 canonical_name
#> 1              FALSE                          summarise_aoi_samples
#> 2              FALSE                            summarise_fixations
#> 3              FALSE                        summarise_gazepoint_aoi
#> 4              FALSE                summarise_gazepoint_aoi_entries
#> 5              FALSE            summarise_gazepoint_aoi_transitions
#> 6              FALSE         summarise_gazepoint_aoi_trial_features
#> 7              FALSE                summarise_gazepoint_aoi_windows
#> 8              FALSE                   summarise_gazepoint_clusters
#> 9               TRUE        summarise_gazepoint_coordinate_coverage
#> 10             FALSE                    summarise_gazepoint_emmeans
#> 11             FALSE   summarise_gazepoint_event_detector_agreement
#> 12             FALSE   summarise_gazepoint_event_detector_benchmark
#> 13              TRUE               summarise_gazepoint_face_quality
#> 14              TRUE            summarise_gazepoint_face_reactivity
#> 15              TRUE               summarise_gazepoint_face_windows
#> 16             FALSE            summarise_gazepoint_fixation_trials
#> 17             FALSE              summarise_gazepoint_fixed_effects
#> 18             FALSE                summarise_gazepoint_markovchain
#> 19              TRUE                summarise_gazepoint_missingness
#> 20             FALSE         summarise_gazepoint_multiverse_results
#> 21              TRUE             summarise_gazepoint_phase_coverage
#> 22             FALSE                      summarise_gazepoint_pupil
#> 23              TRUE    summarise_gazepoint_pupil_response_features
#> 24             FALSE       summarise_gazepoint_pupil_trial_features
#> 25             FALSE              summarise_gazepoint_pupil_windows
#> 26              TRUE                  summarise_gazepoint_qc_status
#> 27             FALSE summarise_gazepoint_scanpath_cluster_stability
#> 28             FALSE                 summarise_gazepoint_semimarkov
#> 29              TRUE              summarise_gazepoint_time_clusters
#> 30             FALSE                   summarise_gazepoint_workflow
#> 31             FALSE                     summarise_tracking_quality
#>            status
#> 1  canonical_only
#> 2  canonical_only
#> 3  canonical_only
#> 4  canonical_only
#> 5  canonical_only
#> 6  canonical_only
#> 7  canonical_only
#> 8  canonical_only
#> 9          paired
#> 10 canonical_only
#> 11 canonical_only
#> 12 canonical_only
#> 13         paired
#> 14         paired
#> 15         paired
#> 16 canonical_only
#> 17 canonical_only
#> 18 canonical_only
#> 19         paired
#> 20 canonical_only
#> 21         paired
#> 22 canonical_only
#> 23         paired
#> 24 canonical_only
#> 25 canonical_only
#> 26         paired
#> 27 canonical_only
#> 28 canonical_only
#> 29         paired
#> 30 canonical_only
#> 31 canonical_only
```

The audit distinguishes:

- `paired`: both spellings are exported;
- `canonical_only`: only the British canonical name exists;
- `missing_british_alias`: an established American export lacks its
  British counterpart.

The final development state should contain no `missing_british_alias`
rows.
