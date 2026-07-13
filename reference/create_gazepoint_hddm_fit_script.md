# Create a Python HDDM fitting script from a Gazepoint HDDM export

Writes a Python script for fitting an HDDMRegressor model. The function
does not fit HDDM inside R and does not require Python. It creates a
reproducible script that can be run in a Python/HDDM environment.

## Usage

``` r
create_gazepoint_hddm_fit_script(
  data_file,
  output_file = "fit_gazepoint_hddm.py",
  regressions = c(v = "target_dwell_ms_z", a = "pupil_peak_z"),
  include = c("v", "a", "t"),
  draws = 5000,
  burn = 2000,
  dbname = "hddm_traces.db"
)
```

## Arguments

- data_file:

  Path to a CSV file prepared with
  [`prepare_gazepoint_hddm_export()`](https://stefanosbalaskas.github.io/gp3tools/reference/prepare_gazepoint_hddm_export.md).

- output_file:

  Path where the Python script should be written.

- regressions:

  Named character vector. Names should be DDM parameters such as `"v"`,
  `"a"`, `"t"`, or `"z"`; values should be predictor terms.

- include:

  Character vector of DDM parameters to include.

- draws:

  Number of posterior draws.

- burn:

  Number of burn-in samples.

- dbname:

  HDDM trace database name.

## Value

Invisibly returns `output_file`.
