test_that("detect_gazepoint_fixations_ivt detects stationary samples", {
  d <- data.frame(
    trial = rep(1, 6),
    time = seq(0, 250, by = 50),
    x = c(0, 0.001, 0.002, 0.003, 0.5, 0.9),
    y = c(0, 0.001, 0.002, 0.003, 0.5, 0.9)
  )
  out <- detect_gazepoint_fixations_ivt(d, "x", "y", "time", group_cols = "trial", velocity_threshold = 0.001, min_duration_ms = 50)
  expect_true(nrow(out) >= 1)
  expect_true(all(out$duration_ms >= 50))
})

test_that("prepare_gazepoint_traminer_data creates wide sequences", {
  d <- data.frame(id = c(1, 1, 2, 2), time = c(1, 2, 1, 2), aoi = c("A", "B", "A", "C"))
  out <- prepare_gazepoint_traminer_data(d, aoi_col = "aoi", sequence_cols = "id", time_col = "time")
  expect_s3_class(out, "gp3_traminer_data")
  expect_equal(nrow(out$wide_data), 2)
  expect_true(all(c("A", "B", "C") %in% out$alphabet))
})

test_that("compute_gazepoint_sequence_recurrence works for vectors", {
  out <- compute_gazepoint_sequence_recurrence(sequence = c("A", "B", "A", "B", "A"))
  expect_equal(out$sequence_length, 5)
  expect_true(out$recurrence_rate > 0)
})

test_that("compute_gazepoint_sequence_recurrence works by group", {
  d <- data.frame(id = c(1, 1, 1, 2, 2, 2), time = c(1, 2, 3, 1, 2, 3), aoi = c("A", "B", "A", "C", "D", "E"))
  out <- compute_gazepoint_sequence_recurrence(d, aoi_col = "aoi", group_cols = "id", time_col = "time")
  expect_equal(nrow(out), 2)
  expect_true(out$recurrence_rate[out$id == 1] > out$recurrence_rate[out$id == 2])
})

test_that("compute_gazepoint_transition_network_metrics works from raw AOIs", {
  d <- data.frame(id = c(1, 1, 1, 1), time = 1:4, aoi = c("A", "B", "C", "A"))
  out <- compute_gazepoint_transition_network_metrics(d, aoi_col = "aoi", group_cols = "id", time_col = "time")
  expect_s3_class(out, "gp3_transition_network_metrics")
  expect_equal(out$graph_summary$n_states, 3)
  expect_true(out$graph_summary$n_edges >= 3)
})

test_that("compute_gazepoint_transition_network_metrics works from transition rows", {
  d <- data.frame(from = c("A", "A", "B"), to = c("B", "C", "A"))
  out <- compute_gazepoint_transition_network_metrics(d, from_col = "from", to_col = "to")
  expect_equal(out$graph_summary$total_transitions, 3)
})

test_that("launch_gazepoint_qc_dashboard returns a spec without Shiny", {
  d <- data.frame(x = 1:3, y = 4:6)
  out <- launch_gazepoint_qc_dashboard(d, launch = FALSE)
  expect_s3_class(out, "gp3_qc_dashboard_spec")
  expect_equal(out$n_rows, 3)
})
