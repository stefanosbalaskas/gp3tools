# Fit optional negative-binomial transition-count sensitivity models

Fit an optional negative-binomial sensitivity model for AOI/state
transition counts using `glmmTMB` when it is installed. This helper is
intended as a publication sensitivity branch for overdispersed
transition-count outcomes.

## Usage

``` r
fit_gazepoint_transition_count_nb_sensitivity(
  data,
  count_col = NULL,
  from_col = NULL,
  to_col = NULL,
  condition_cols = NULL,
  random_effect_cols = NULL,
  exposure_col = NULL,
  offset_col = NULL,
  formula = NULL,
  family = c("nbinom2", "nbinom1"),
  zero_inflation = FALSE,
  ziformula = NULL,
  dispformula = NULL,
  control = NULL,
  name = "gazepoint_transition_count_nb_sensitivity"
)
```

## Arguments

- data:

  A data frame containing transition-count rows.

- count_col:

  Transition-count outcome column. If `NULL`, common count columns are
  detected automatically.

- from_col:

  Transition origin column. If `NULL`, common origin columns are
  detected automatically.

- to_col:

  Transition destination column. If `NULL`, common destination columns
  are detected automatically.

- condition_cols:

  Optional fixed-effect condition columns.

- random_effect_cols:

  Optional random-intercept grouping columns.

- exposure_col:

  Optional positive exposure column. If supplied, the model includes
  `offset(log(exposure_col))`.

- offset_col:

  Optional numeric offset column. Use either `exposure_col` or
  `offset_col`, not both.

- formula:

  Optional model formula. If `NULL`, a formula is constructed from
  transition origin, destination, condition columns, optional offset,
  and random intercepts.

- family:

  Negative-binomial family. Options are `"nbinom2"` and `"nbinom1"`.

- zero_inflation:

  Logical. If `TRUE`, use `ziformula = ~1` unless `ziformula` is
  supplied.

- ziformula:

  Optional zero-inflation formula passed to `glmmTMB`.

- dispformula:

  Optional dispersion formula passed to `glmmTMB`.

- control:

  Optional control object passed to `glmmTMB`.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_transition_count_nb_sensitivity`.

## Details

The helper keeps `glmmTMB` optional. If `glmmTMB` is not installed, it
returns a structured skipped object rather than failing.
