# Plot gallery

This article is a website-only showcase of representative visual outputs
in `gp3tools`. It is intended for users, reviewers, and readers who want
to see the visual diagnostics and reporting plots produced by the
package.

The gallery uses only package-safe synthetic or example data. It should
not be read as an empirical analysis of real participants.

## Important note

`gp3tools` can parse official Gazepoint Analysis summary exports, but
metrics recomputed from all-gaze and fixation files may not always
exactly reproduce Gazepoint’s internal summary calculations. For
official Gazepoint summary values, use
[`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint_summary.md).
For transparent reproducible calculations from exported rows, use the
sample-level and fixation-level summary functions.

## Exported plot functions

    #>                                plot_function
    #> 1                    plot_gazepoint_aoi_gamm
    #> 2                plot_gazepoint_aoi_timeline
    #> 3       plot_gazepoint_aoi_transition_matrix
    #> 4            plot_gazepoint_aoi_verification
    #> 5   plot_gazepoint_cluster_null_distribution
    #> 6         plot_gazepoint_cluster_permutation
    #> 7             plot_gazepoint_cluster_results
    #> 8    plot_gazepoint_event_detector_agreement
    #> 9                plot_gazepoint_face_quality
    #> 10                        plot_gazepoint_gca
    #> 11                    plot_gazepoint_heatmap
    #> 12            plot_gazepoint_heatmap_overlay
    #> 13        plot_gazepoint_missingness_profile
    #> 14          plot_gazepoint_model_predictions
    #> 15            plot_gazepoint_model_residuals
    #> 16         plot_gazepoint_multiverse_results
    #> 17             plot_gazepoint_phase_timeline
    #> 18        plot_gazepoint_pupil_preprocessing
    #> 19               plot_gazepoint_pupil_status
    #> 20           plot_gazepoint_pupil_timecourse
    #> 21                plot_gazepoint_qc_overview
    #> 22                   plot_gazepoint_scanpath
    #> 23 plot_gazepoint_scanpath_cluster_stability
    #> 24          plot_gazepoint_scanpath_clusters
    #> 25                  plot_gazepoint_scanpaths
    #> 26         plot_gazepoint_stimulus_layout_qc
    #> 27                plot_gazepoint_time_series
    #> 28        plot_gazepoint_time_varying_effect
    #> 29                        plot_sampling_rate
    #> 30                     plot_tracking_quality
    #> 31                   plot_transition_heatmap

## Sampling rate

![](plot-gallery_files/figure-html/sampling-rate-plot-1.png)

## Tracking quality

![](plot-gallery_files/figure-html/tracking-quality-plot-1.png)

## Pupil status

![](plot-gallery_files/figure-html/pupil-status-plot-1.png)

## Pupil preprocessing

![](plot-gallery_files/figure-html/pupil-preprocessing-plot-1.png)

## Pupil time course

![](plot-gallery_files/figure-html/pupil-timecourse-plot-1.png)

## AOI verification

![](plot-gallery_files/figure-html/aoi-verification-plot-1.png)

## AOI transition matrix

![](plot-gallery_files/figure-html/aoi-transition-matrix-plot-1.png)

## Transition heatmap

![](plot-gallery_files/figure-html/transition-heatmap-plot-1.png)

## GCA model plot

![](plot-gallery_files/figure-html/gca-model-plot-1.png)

## Model-prediction plot

![](plot-gallery_files/figure-html/model-prediction-plot-1.png)

## AOI GAMM plot

![](plot-gallery_files/figure-html/aoi-gamm-plot-1.png)

## Cluster-results plot

![](plot-gallery_files/figure-html/cluster-results-plot-1.png)

## Multiverse-results plot

![](plot-gallery_files/figure-html/multiverse-results-plot-1.png)
