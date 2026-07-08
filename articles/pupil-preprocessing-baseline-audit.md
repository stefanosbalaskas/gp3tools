# Pupil preprocessing and baseline audit

This article focuses on pupil preprocessing and baseline-readiness
checks.

## Recommended order

1.  inspect raw pupil coverage;
2.  flag missingness and implausible values;
3.  interpolate short gaps only when justified;
4.  audit baseline coverage;
5.  create baseline-corrected or change-score variables;
6.  document preprocessing choices.

## Example workflow

``` r

gap_qc <- audit_gazepoint_pupil_gaps(all_gaze)

pupil_flagged <- flag_gazepoint_pupil_hampel(
  all_gaze,
  pupil_col = 'pupil_diameter'
  )

pupil_interp <- interpolate_gazepoint_pupil_pchip(
  pupil_flagged,
  pupil_col = 'pupil_diameter'
  )

baseline_qc <- audit_gazepoint_pupil_baseline(
  pupil_interp,
  baseline_start = -0.500,
  baseline_end = 0
  )

reliability_qc <- audit_gazepoint_pupil_reliability(pupil_interp)
```

## Reporting note

Baseline correction should be reported with the baseline window, minimum
coverage rule, interpolation rule, excluded cases, and sensitivity
checks.
