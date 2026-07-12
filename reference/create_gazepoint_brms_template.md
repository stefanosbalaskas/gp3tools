# Create brms formula and prior templates for Gazepoint-derived metrics

Returns formula, family, prior, and reporting-note templates. The
function does not require or call brms.

## Usage

``` r
create_gazepoint_brms_template(
  metric_type,
  outcome,
  time = NULL,
  condition = NULL,
  subject = NULL,
  item = NULL
)
```

## Arguments

- metric_type:

  Metric type, such as `"pupil_timecourse"`, `"fixation_duration"`,
  `"fixation_count"`, `"aoi_proportion"`, or `"binary_choice"`.

- outcome:

  Outcome column name.

- time:

  Optional time column.

- condition:

  Optional condition column.

- subject:

  Optional subject column.

- item:

  Optional item/stimulus column.

## Value

A list containing formula, family, priors, and notes.
