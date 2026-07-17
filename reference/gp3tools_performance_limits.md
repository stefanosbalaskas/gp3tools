# Default performance limits for gp3tools export workflows

Returns conservative, explicit limits for elapsed time, approximate
R-heap growth, and scaling behaviour. The limits are intended as
regression gates, not as hardware-independent claims about absolute
package speed.

## Usage

``` r
gp3tools_performance_limits()
```

## Value

A data frame with one row per benchmarked operation.
