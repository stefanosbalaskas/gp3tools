make_binocular_test_data <- function(n = 120L, groups = 2L) {
  right <- seq(3.0, 5.0, length.out = n)
  left <- 0.4 + 1.1 * right
  subject <- rep(paste0("S", seq_len(groups)), length.out = n)
  condition <- rep(c("A", "B"), length.out = n)
  data.frame(
    subject = subject,
    condition = condition,
    timestamp_ms = seq(0, by = 1000 / 60, length.out = n),
    pupil_left = left,
    pupil_right = right,
    unrelated = seq_len(n),
    stringsAsFactors = FALSE
  )
}
