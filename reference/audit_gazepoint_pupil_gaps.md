# Audit Gazepoint pupil interpolation gaps

Summarise observed, interpolated, and remaining missing pupil samples,
together with gap-level counts and gap duration/sample-size summaries.

## Usage

``` r
audit_gazepoint_pupil_gaps(
  data,
  group_cols = c("subject", "media_id"),
  status_col = "pupil_interpolation_status",
  gap_id_col = "pupil_gap_id",
  gap_n_samples_col = "pupil_gap_n_samples",
  gap_duration_col = "pupil_gap_duration_ms",
  interpolated_col = "pupil_was_interpolated",
  pupil_col = "pupil_interpolated"
)
```

## Arguments

- data:

  A data frame containing pupil interpolation status columns.

- group_cols:

  Character vector of grouping columns. Use `character(0)` for an
  overall audit.

- status_col:

  Name of the interpolation status column.

- gap_id_col:

  Name of the gap identifier column.

- gap_n_samples_col:

  Name of the column containing gap size in samples.

- gap_duration_col:

  Name of the column containing gap duration in ms.

- interpolated_col:

  Name of the logical column indicating whether a sample was
  interpolated.

- pupil_col:

  Name of the pupil column after interpolation.

## Value

A tibble with one row per group, or one row overall when
`group_cols = character(0)`.

## Details

This function is intended for data returned by
[`interpolate_gazepoint_pupil()`](https://stefanosbalaskas.github.io/gp3tools/reference/interpolate_gazepoint_pupil.md),
but it can also be used with any table that contains compatible
interpolation-status and gap columns.
