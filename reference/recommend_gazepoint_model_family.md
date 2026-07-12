# Recommend model families for Gazepoint-derived metrics

Maps common eye-tracking and pupillometry metrics to suitable
statistical model families, transformations, and modelling notes. The
helper is intended for planning and reporting; it does not fit a model.

## Usage

``` r
recommend_gazepoint_model_family(metric = NULL)
```

## Arguments

- metric:

  Character vector of metric names. If `NULL`, all available
  recommendations are returned.

## Value

A data frame with metric, data property, recommended family, common
transformation, and notes.
