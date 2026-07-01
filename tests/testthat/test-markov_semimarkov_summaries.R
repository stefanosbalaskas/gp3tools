test_that("summarise_gazepoint_markovchain summarises matrices", {
  m <- matrix(c(1, 2, 3, 0), nrow = 2, byrow = TRUE)
  rownames(m) <- c("A", "B")
  colnames(m) <- c("A", "B")
  out <- summarise_gazepoint_markovchain(m, include_zero = TRUE)
  expect_equal(nrow(out), 4)
  expect_equal(out$transition_value[out$from_state == "A" & out$to_state == "A"], 1)
  expect_equal(out$transition_probability[out$from_state == "A" & out$to_state == "B"], 2 / 3)
})

test_that("summarise_gazepoint_markovchain summarises data frames", {
  d <- data.frame(
    from = c("A", "A", "B"),
    to = c("A", "B", "A"),
    count = c(1, 3, 2)
  )
  out <- summarise_gazepoint_markovchain(d)
  expect_equal(nrow(out), 3)
  expect_equal(out$transition_probability[out$from_state == "A" & out$to_state == "B"], 3 / 4)
})

test_that("summarise_gazepoint_semimarkov returns state and transition summaries", {
  d <- data.frame(
    id = c(1, 1, 1, 2, 2, 2),
    time = c(1, 2, 3, 1, 2, 3),
    state = c("A", "B", "B", "A", "C", "A"),
    duration = c(100, 200, 300, 100, 150, 250)
  )
  out <- summarise_gazepoint_semimarkov(d)
  expect_s3_class(out, "gp3_semimarkov_summary")
  expect_true(all(c("state_summary", "transition_summary") %in% names(out)))
  expect_equal(out$state_summary$n_visits[out$state_summary$state == "A"], 3)
  expect_true(nrow(out$transition_summary) > 0)
})

test_that("summarise_gazepoint_semimarkov uses explicit transition columns", {
  d <- data.frame(
    state = c("A", "B", "C"),
    duration = c(1, 2, 3),
    from_state = c("A", "A", "B"),
    to_state = c("B", "C", "C")
  )
  out <- summarise_gazepoint_semimarkov(d)
  expect_equal(sum(out$transition_summary$transition_count), 3)
})
