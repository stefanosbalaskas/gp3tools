# Avoid R CMD check notes for data-masked ggplot2 aesthetics.
utils::globalVariables(c(
  ".gp3_estimate",
  ".gp3_group",
  ".gp3_label",
  ".gp3_lower",
  ".gp3_time",
  ".gp3_upper",
  ".gp3_x",
  ".gp3_y",
  "fitted",
  "residual"
))
utils::globalVariables(c(
  ".gp3_intensity",
  ".gp3_x_px",
  ".gp3_x_tile",
  ".gp3_y_px",
  ".gp3_y_tile"
))

utils::globalVariables(c(
  "null_statistic",
  "observed_statistic"
))

utils::globalVariables(c(
  ".gp3_missing_rate",
  ".gp3_variable"
))

utils::globalVariables(c(
  ".gp3_phase",
  ".gp3_min_time",
  ".gp3_max_time"
))

utils::globalVariables(c(
  "n_rows"
))
