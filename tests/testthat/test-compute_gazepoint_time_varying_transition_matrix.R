make_test_tvtm_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 8),
    condition = rep(c("A", "B"), each = 4, times = 2),
    time = rep(seq(0, 700, by = 100), times = 2),
    from_aoi = c(
      "logo", "claim", "claim", "product",
      "logo", "product", "claim", "logo",
      "claim", "logo", "product", "claim",
      "product", "logo", "claim", "product"
    ),
    to_aoi = c(
      "claim", "product", "logo", "claim",
      "product", "claim", "logo", "product",
      "logo", "claim", "claim", "product",
      "logo", "product", "product", "claim"
    )
  )
}

test_that("compute_gazepoint_time_varying_transition_matrix creates a complete audit object", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    by_cols = "condition",
    normalise = "row",
    complete_states = TRUE,
    drop_self_transitions = TRUE,
    name = "toy_tvtm"
  )

  expect_s3_class(out, "gp3_time_varying_transition_matrix")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "time_windows",
      "matrix_long",
      "count_wide",
      "probability_wide",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$time_windows, "tbl_df")
  expect_s3_class(out$matrix_long, "tbl_df")
  expect_s3_class(out$count_wide, "tbl_df")
  expect_s3_class(out$probability_wide, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_tvtm")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_rows_used, nrow(toy_data))
  expect_equal(out$overview$n_states, 3)
  expect_equal(out$overview$n_time_windows, 2)
  expect_equal(out$overview$n_by_groups, 2)
  expect_equal(out$overview$n_matrix_rows, 12)
  expect_equal(out$overview$total_transition_count, 16)
  expect_equal(out$overview$normalise, "row")
  expect_true(out$overview$complete_states)
  expect_true(out$overview$drop_self_transitions)
})

test_that("compute_gazepoint_time_varying_transition_matrix computes row-normalised probabilities", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    by_cols = "condition",
    normalise = "row",
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  prob_sums <- out$matrix_long |>
    dplyr::filter(.data$transition_denominator > 0) |>
    dplyr::group_by(
      .data$condition,
      .data$.gp3_time_window,
      .data$.gp3_from
    ) |>
    dplyr::summarise(
      prob_sum = sum(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    )

  expect_true(all(abs(prob_sums$prob_sum - 1) < 1e-8))
  expect_false(any(out$matrix_long$.gp3_from == out$matrix_long$.gp3_to))
  expect_true(all(out$matrix_long$transition_count >= 0))
})

test_that("compute_gazepoint_time_varying_transition_matrix creates wide count and probability matrices", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    by_cols = "condition",
    normalise = "row",
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  expect_true(all(c("claim", "logo", "product") %in% names(out$count_wide)))
  expect_true(all(c("claim", "logo", "product") %in% names(out$probability_wide)))

  expect_equal(sum(out$count_wide$claim, na.rm = TRUE), 6)
  expect_equal(sum(out$count_wide$logo, na.rm = TRUE), 4)
  expect_equal(sum(out$count_wide$product, na.rm = TRUE), 6)

  expect_true(any(is.na(out$probability_wide$claim)))
  expect_true(any(is.na(out$probability_wide$logo)))
  expect_true(any(is.na(out$probability_wide$product)))
})

test_that("compute_gazepoint_time_varying_transition_matrix supports count columns and global normalisation", {
  toy_data <- make_test_tvtm_data() |>
    dplyr::mutate(transition_count = 2)

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    count_col = "transition_count",
    normalise = "global",
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  expect_equal(out$overview$total_transition_count, 32)
  expect_equal(out$overview$n_by_groups, 1)
  expect_equal(out$settings$value[out$settings$setting == "count_col"], "transition_count")

  global_sums <- out$matrix_long |>
    dplyr::filter(.data$transition_denominator > 0) |>
    dplyr::group_by(.data$.gp3_time_window) |>
    dplyr::summarise(
      prob_sum = sum(.data$transition_probability, na.rm = TRUE),
      .groups = "drop"
    )

  expect_true(all(abs(global_sums$prob_sum - 1) < 1e-8))
})

test_that("compute_gazepoint_time_varying_transition_matrix supports existing window columns", {
  toy_data <- make_test_tvtm_data() |>
    dplyr::mutate(window_label = ifelse(.data$time < 400, "early", "late"))

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    window_col = "window_label",
    by_cols = "condition",
    normalise = "row",
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  expect_equal(sort(unique(out$matrix_long$.gp3_time_window)), c("early", "late"))
  expect_true(all(is.na(out$matrix_long$.gp3_time_window_start)))
  expect_true(all(is.na(out$matrix_long$.gp3_time_window_end)))
  expect_equal(out$settings$value[out$settings$setting == "window_col"], "window_label")
  expect_true(is.na(out$settings$value[out$settings$setting == "window_size_ms"]))
})

test_that("compute_gazepoint_time_varying_transition_matrix supports no normalisation", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    normalise = "none",
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  expect_equal(out$overview$normalise, "none")
  expect_true(all(is.na(out$matrix_long$transition_denominator)))
  expect_true(all(is.na(out$matrix_long$transition_probability)))
})

test_that("compute_gazepoint_time_varying_transition_matrix supports incomplete state grids", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    by_cols = "condition",
    complete_states = FALSE,
    drop_self_transitions = TRUE
  )

  expect_false(out$overview$complete_states)
  expect_equal(nrow(out$matrix_long), 9)
  expect_true(all(out$matrix_long$transition_count > 0))
})

test_that("compute_gazepoint_time_varying_transition_matrix supports explicit states", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    from_col = "from_aoi",
    to_col = "to_aoi",
    time_col = "time",
    window_size_ms = 400,
    states = c("logo", "claim"),
    complete_states = TRUE,
    drop_self_transitions = TRUE
  )

  expect_equal(out$overview$n_states, 2)
  expect_true(all(out$matrix_long$.gp3_from %in% c("logo", "claim")))
  expect_true(all(out$matrix_long$.gp3_to %in% c("logo", "claim")))
  expect_false(any(out$matrix_long$.gp3_from == "product"))
  expect_false(any(out$matrix_long$.gp3_to == "product"))
})

test_that("compute_gazepoint_time_varying_transition_matrix auto-detects common columns", {
  toy_data <- make_test_tvtm_data()

  out <- compute_gazepoint_time_varying_transition_matrix(
    toy_data,
    window_size_ms = 400
  )

  expect_s3_class(out, "gp3_time_varying_transition_matrix")
  expect_equal(out$settings$value[out$settings$setting == "from_col"], "from_aoi")
  expect_equal(out$settings$value[out$settings$setting == "to_col"], "to_aoi")
  expect_equal(out$settings$value[out$settings$setting == "time_col"], "time")
})

test_that("compute_gazepoint_time_varying_transition_matrix checks invalid inputs", {
  toy_data <- make_test_tvtm_data()

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      from_col = "bad_from",
      window_size_ms = 400
    ),
    "`from_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      to_col = "bad_to",
      window_size_ms = 400
    ),
    "`to_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      time_col = "bad_time",
      window_size_ms = 400
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_col = "bad_window"
    ),
    "`window_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      by_cols = "bad_group",
      window_size_ms = 400
    ),
    "All `by_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      count_col = "bad_count",
      window_size_ms = 400
    ),
    "`count_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_size_ms = 0
    ),
    "`window_size_ms` must be a finite positive number",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_size_ms = 400,
      complete_states = NA
    ),
    "`complete_states` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_size_ms = 400,
      drop_self_transitions = NA
    ),
    "`drop_self_transitions` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_size_ms = 400,
      states = character(0)
    ),
    "`states` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      toy_data,
      window_size_ms = 400,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("compute_gazepoint_time_varying_transition_matrix validates transition and count data", {
  toy_data <- make_test_tvtm_data()

  missing_transitions <- toy_data
  missing_transitions$from_aoi <- NA_character_

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      missing_transitions,
      from_col = "from_aoi",
      to_col = "to_aoi",
      time_col = "time",
      window_size_ms = 400
    ),
    "No valid non-missing transitions were found",
    fixed = TRUE
  )

  bad_time <- toy_data
  bad_time$time[1] <- NA_real_

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      bad_time,
      from_col = "from_aoi",
      to_col = "to_aoi",
      time_col = "time",
      window_size_ms = 400
    ),
    "`time_col` must contain finite numeric values when constructing time windows",
    fixed = TRUE
  )

  bad_count <- toy_data |>
    dplyr::mutate(transition_count = 1)
  bad_count$transition_count[1] <- -1

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      bad_count,
      from_col = "from_aoi",
      to_col = "to_aoi",
      time_col = "time",
      window_size_ms = 400,
      count_col = "transition_count"
    ),
    "`count_col` must contain finite non-negative values",
    fixed = TRUE
  )

  filtered_out <- toy_data

  expect_error(
    compute_gazepoint_time_varying_transition_matrix(
      filtered_out,
      from_col = "from_aoi",
      to_col = "to_aoi",
      time_col = "time",
      window_size_ms = 400,
      states = c("not_present"),
      drop_self_transitions = TRUE
    ),
    "No transitions remain after applying `states` and `drop_self_transitions`",
    fixed = TRUE
  )
})
