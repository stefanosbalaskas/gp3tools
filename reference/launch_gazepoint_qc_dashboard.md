# Launch or describe a lightweight QC dashboard

Provide a minimal optional Shiny dashboard launcher for inspecting a
data frame. With `launch = FALSE`, the function returns a dashboard
specification without requiring Shiny.

## Usage

``` r
launch_gazepoint_qc_dashboard(
  data = NULL,
  title = "gp3tools QC dashboard",
  launch = FALSE
)
```

## Arguments

- data:

  Optional data frame to inspect.

- title:

  Dashboard title.

- launch:

  Should a Shiny app be launched?

## Value

A dashboard specification, or a Shiny app object when launched.
