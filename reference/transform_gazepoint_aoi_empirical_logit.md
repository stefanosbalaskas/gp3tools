# Transform AOI proportions to empirical logits

Convert bounded AOI proportions into empirical logits for linear mixed
models, growth-curve analysis, or other approximately Gaussian
time-course models.

## Usage

``` r
transform_gazepoint_aoi_empirical_logit(
  data,
  numerator_col = NULL,
  denominator_col = NULL,
  proportion_col = NULL,
  correction = 0.5,
  pseudo_denominator = 1,
  output_col = "aoi_empirical_logit",
  adjusted_proportion_col = "aoi_proportion_adjusted",
  raw_proportion_col = "aoi_proportion_raw",
  numerator_output_col = "aoi_numerator",
  denominator_output_col = "aoi_denominator",
  status_col = "aoi_empirical_logit_status",
  overwrite = FALSE,
  name = "gazepoint_aoi_empirical_logit"
)
```

## Arguments

- data:

  A data frame containing AOI proportions or AOI count data.

- numerator_col:

  Optional numerator column, for example number of samples or fixations
  inside the AOI.

- denominator_col:

  Optional denominator column, for example total valid samples or total
  fixations in the window.

- proportion_col:

  Optional bounded AOI proportion column. If `numerator_col` and
  `denominator_col` are supplied, the raw proportion is computed from
  those columns. If only `proportion_col` is supplied, a
  pseudo-denominator is used and recorded in the output.

- correction:

  Positive correction constant added to numerator and non-AOI count. The
  common empirical-logit correction is `0.5`.

- pseudo_denominator:

  Positive pseudo-denominator used only when `proportion_col` is
  supplied without `denominator_col`.

- output_col:

  Name of the empirical-logit output column.

- adjusted_proportion_col:

  Name of the adjusted proportion output column.

- raw_proportion_col:

  Name of the raw proportion output column.

- numerator_output_col:

  Name of the numerator output column used in the transformation.

- denominator_output_col:

  Name of the denominator output column used in the transformation.

- status_col:

  Name of the row-level transformation status column.

- overwrite:

  Logical. If `FALSE`, the function errors when output columns already
  exist in `data`.

- name:

  Character label stored in object attributes.

## Value

A tibble with empirical-logit transformation columns added. The object
has class `gp3_aoi_empirical_logit_data`.

## Details

Binomial GLMMs are usually preferable when numerator and denominator
counts are available. This helper is intended for sensitivity analyses,
GCA-style models, and linear time-course summaries where a transformed
AOI proportion is needed.
