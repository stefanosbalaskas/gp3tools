# Create a Gazepoint pupil-preprocessing registry

Creates a compact registry of commonly used preprocessing parameters for
Gazepoint pupil and gaze analyses. The registry is designed to make
preprocessing choices explicit, auditable, and easy to report.

## Usage

``` r
create_gazepoint_preprocessing_registry(
  blink_padding_pre_ms = 100,
  blink_padding_post_ms = 100,
  max_interpolation_gap_ms = 150,
  smoothing_window_ms = 50,
  baseline_start_ms = -200,
  baseline_end_ms = 0,
  pupil_physiological_min = 1,
  pupil_physiological_max = 9,
  pupil_speed_mad_k = 6,
  binocular_mad_k = 6,
  baseline_missing_prop_threshold = 0.3,
  baseline_interpolated_prop_threshold = 0.3,
  baseline_artifact_prop_threshold = 0.3,
  overlap_trial_duration_ms = 3000,
  overlap_event_gap_ms = 1000
)
```

## Arguments

- blink_padding_pre_ms:

  Padding before bad pupil samples, blinks, or tracking artifacts, in
  milliseconds. Defaults to `100`.

- blink_padding_post_ms:

  Padding after bad pupil samples, blinks, or tracking artifacts, in
  milliseconds. Defaults to `100`.

- max_interpolation_gap_ms:

  Maximum missing-pupil gap duration to interpolate, in milliseconds.
  Defaults to `150`.

- smoothing_window_ms:

  Rolling smoothing window, in milliseconds. Defaults to `50`.

- baseline_start_ms:

  Baseline-window start, in milliseconds. Defaults to `-200`.

- baseline_end_ms:

  Baseline-window end, in milliseconds. Defaults to `0`.

- pupil_physiological_min:

  Minimum plausible pupil value when the pupil unit is known to be
  millimetres. Defaults to `1`.

- pupil_physiological_max:

  Maximum plausible pupil value when the pupil unit is known to be
  millimetres. Defaults to `9`.

- pupil_speed_mad_k:

  MAD multiplier for pupil-speed outlier detection. Defaults to `6`.

- binocular_mad_k:

  MAD multiplier for left-right pupil disagreement. Defaults to `6`.

- baseline_missing_prop_threshold:

  Baseline missingness threshold used for baseline-quality audits.
  Defaults to `0.30`.

- baseline_interpolated_prop_threshold:

  Baseline interpolation threshold used for baseline-quality audits.
  Defaults to `0.30`.

- baseline_artifact_prop_threshold:

  Baseline artifact threshold used for baseline-quality audits. Defaults
  to `0.30`.

- overlap_trial_duration_ms:

  Trial-duration threshold below which pupil overlap/deconvolution risk
  should be considered. Defaults to `3000`.

- overlap_event_gap_ms:

  Event-gap threshold below which pupil-response overlap should be
  considered. Defaults to `1000`.

## Value

A tibble with one row per preprocessing parameter.

## Examples

``` r
registry <- create_gazepoint_preprocessing_registry()
registry
#> # A tibble: 15 × 5
#>    parameter                             value unit         category description
#>    <chr>                                 <dbl> <chr>        <chr>    <chr>      
#>  1 blink_padding_pre_ms                  100   ms           artifac… Millisecon…
#>  2 blink_padding_post_ms                 100   ms           artifac… Millisecon…
#>  3 max_interpolation_gap_ms              150   ms           interpo… Maximum sh…
#>  4 smoothing_window_ms                    50   ms           smoothi… Rolling sm…
#>  5 baseline_start_ms                    -200   ms           baseline Start of t…
#>  6 baseline_end_ms                         0   ms           baseline End of the…
#>  7 pupil_physiological_min                 1   mm_if_unit_… pupil_p… Minimum ph…
#>  8 pupil_physiological_max                 9   mm_if_unit_… pupil_p… Maximum ph…
#>  9 pupil_speed_mad_k                       6   MAD_multipl… artifac… Robust MAD…
#> 10 binocular_mad_k                         6   MAD_multipl… binocul… Robust MAD…
#> 11 baseline_missing_prop_threshold         0.3 proportion   baselin… Baseline-w…
#> 12 baseline_interpolated_prop_threshold    0.3 proportion   baselin… Baseline-w…
#> 13 baseline_artifact_prop_threshold        0.3 proportion   baselin… Baseline-w…
#> 14 overlap_trial_duration_ms            3000   ms           overlap… Short tria…
#> 15 overlap_event_gap_ms                 1000   ms           overlap… Short even…
```
