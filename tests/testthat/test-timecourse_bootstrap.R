test_that("bootstrap_gazepoint_timecourse returns intervals", {
  d <- data.frame(
    time = rep(1:3, each = 6),
    condition = rep(rep(c("A", "B"), each = 3), times = 3),
    subject = rep(1:3, times = 6),
    value = c(1, 2, 3, 2, 3, 4, 2, 3, 4, 3, 4, 5, 3, 4, 5, 4, 5, 6)
  )
  out <- bootstrap_gazepoint_timecourse(
    d, time_col = "time", value_col = "value", group_col = "condition",
    subject_col = "subject", n_boot = 25, seed = 1
  )
  expect_true(all(c("time", "group", "estimate", "lower", "upper") %in% names(out)))
  expect_equal(nrow(out), 6)
  expect_true(all(out$lower <= out$upper))
})

test_that("bootstrap_gazepoint_timecourse can return a difference curve", {
  d <- data.frame(
    time = rep(1:2, each = 6),
    condition = rep(rep(c("A", "B"), each = 3), times = 2),
    value = c(4, 4, 4, 1, 1, 1, 5, 5, 5, 2, 2, 2)
  )
  out <- bootstrap_gazepoint_timecourse(
    d, "time", "value", group_col = "condition", n_boot = 20,
    difference_groups = c("A", "B"), seed = 1
  )
  diff <- out[out$group == "difference", ]
  expect_equal(nrow(diff), 2)
  expect_true(all(diff$estimate > 0))
})
