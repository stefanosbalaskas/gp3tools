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
