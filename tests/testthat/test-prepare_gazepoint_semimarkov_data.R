make_test_semimarkov_sequence <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
    trial = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    time = c(0, 100, 200, 300, 400, 0, 100, 200, 300),
    condition = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    aoi_current = c(
      "logo", "product", "product", "price", "logo",
      "logo", "price", "outside", "product"
    )
  )
}

test_that("prepare_gazepoint_semimarkov_data creates complete semi-Markov data", {
  toy_sequence <- make_test_semimarkov_sequence()

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    covariate_cols = "condition",
    exclude_states = "outside",
    collapse_repeated_states = TRUE,
    include_terminal_states = TRUE,
    terminal_next_state_label = "END",
    name = "toy_semimarkov"
  )

  expect_s3_class(out, "gp3_semimarkov_data")
  expect_type(out, "list")

  expect_true(
    all(
      c(
        "overview",
        "state_sequence",
        "dwell_data",
        "transition_data",
        "state_summary",
        "transition_summary",
        "settings"
      ) %in% names(out)
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$state_sequence, "tbl_df")
  expect_s3_class(out$dwell_data, "tbl_df")
  expect_s3_class(out$transition_data, "tbl_df")
  expect_s3_class(out$state_summary, "tbl_df")
  expect_s3_class(out$transition_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_semimarkov")
  expect_equal(out$overview$n_input_rows, 9)
  expect_equal(out$overview$n_rows_used, 8)
  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_states, 3)
  expect_equal(out$overview$n_state_visits, 7)
  expect_equal(out$overview$n_transitions, 7)
  expect_equal(out$overview$n_terminal_transitions, 2)
  expect_equal(out$overview$n_excluded_states_removed, 1)
  expect_true(out$overview$collapse_repeated_states)
  expect_true(out$overview$include_terminal_states)

  expect_true(all(c("logo", "price", "product") %in% out$state_summary$state))
  expect_false("outside" %in% out$dwell_data$state)

  s1_product <- out$dwell_data[
    out$dwell_data$.sequence_key == "S1||1" &
      out$dwell_data$state == "product",
  ]

  expect_equal(nrow(s1_product), 1)
  expect_equal(s1_product$n_samples, 2)
  expect_equal(s1_product$dwell_duration, 200)
  expect_equal(s1_product$next_state, "price")

  expect_true("condition" %in% names(out$dwell_data))
  expect_true("condition" %in% names(out$transition_data))
  expect_equal(
    out$transition_data$condition[out$transition_data$.sequence_key == "S1||1"],
    rep("A", 4)
  )

  expect_true(any(out$transition_data$to_state == "END"))
  expect_equal(sum(out$transition_data$is_terminal), 2)
})

test_that("prepare_gazepoint_semimarkov_data auto-detects common columns", {
  toy_sequence <- make_test_semimarkov_sequence()

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    exclude_states = "outside"
  )

  expect_s3_class(out, "gp3_semimarkov_data")
  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_state_visits, 7)

  settings <- out$settings

  expect_equal(settings$value[settings$setting == "state_col"], "aoi_current")
  expect_equal(settings$value[settings$setting == "participant_col"], "subject")
  expect_equal(settings$value[settings$setting == "trial_col"], "trial")
  expect_equal(settings$value[settings$setting == "time_col"], "time")
})

test_that("prepare_gazepoint_semimarkov_data can keep repeated states uncollapsed", {
  toy_sequence <- make_test_semimarkov_sequence()

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    collapse_repeated_states = FALSE,
    include_terminal_states = TRUE
  )

  expect_equal(out$overview$n_rows_used, 8)
  expect_equal(out$overview$n_state_visits, 8)
  expect_equal(out$overview$n_transitions, 8)
  expect_true(any(out$transition_data$from_state == "product" & out$transition_data$to_state == "product"))
})

test_that("prepare_gazepoint_semimarkov_data can remove terminal transitions", {
  toy_sequence <- make_test_semimarkov_sequence()

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    include_terminal_states = FALSE
  )

  expect_false(any(out$transition_data$to_state == "END"))
  expect_false(any(out$transition_data$is_terminal))
  expect_equal(out$overview$n_terminal_transitions, 0)
  expect_equal(out$overview$n_state_visits, 5)
  expect_equal(out$overview$n_transitions, 5)
})

test_that("prepare_gazepoint_semimarkov_data supports custom terminal labels", {
  toy_sequence <- make_test_semimarkov_sequence()

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    terminal_next_state_label = "TERMINAL"
  )

  expect_true(any(out$transition_data$to_state == "TERMINAL"))
  expect_false(any(out$transition_data$to_state == "END"))
})

test_that("prepare_gazepoint_semimarkov_data supports duration columns", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    sample_duration_ms = c(20, 20, 30, 30),
    state = c("logo", "logo", "price", "price")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    duration_col = "sample_duration_ms",
    exclude_states = NULL
  )

  expect_equal(out$overview$duration_source, "duration_col")
  expect_equal(out$dwell_data$dwell_duration, c(40, 60))
  expect_equal(out$transition_data$dwell_duration, c(40, 60))
})

test_that("prepare_gazepoint_semimarkov_data falls back to sample counts without time or duration", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    state = c("logo", "logo", "price", "price")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    exclude_states = NULL
  )

  expect_equal(out$overview$duration_source, "state_visit_sample_count")
  expect_equal(out$dwell_data$dwell_duration, c(2, 2))
})

test_that("prepare_gazepoint_semimarkov_data supports missing-state labelling", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    state = c("logo", NA_character_, "", "product")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = NULL,
    missing_state_label = "missing_state"
  )

  expect_true("missing_state" %in% out$dwell_data$state)
  expect_equal(out$overview$n_missing_states_labelled, 2)
  expect_equal(out$overview$n_missing_states_removed, 0)
})

test_that("prepare_gazepoint_semimarkov_data removes missing states by default", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    state = c("logo", NA_character_, "", "product")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = NULL
  )

  expect_false(any(out$dwell_data$state == "missing_state"))
  expect_equal(out$overview$n_missing_states_removed, 2)
  expect_equal(out$overview$n_state_visits, 2)
})

test_that("prepare_gazepoint_semimarkov_data supports sequence_id_cols", {
  toy_sequence <- tibble::tibble(
    participant = c("S1", "S1", "S1", "S1", "S1", "S1"),
    block = c("A", "A", "A", "B", "B", "B"),
    time = c(0, 100, 200, 0, 100, 200),
    state = c("logo", "price", "product", "product", "price", "logo")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    time_col = "time",
    sequence_id_cols = c("participant", "block"),
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 2)
  expect_true(all(c("S1||A", "S1||B") %in% out$state_sequence$.sequence_key))
})

test_that("prepare_gazepoint_semimarkov_data supports one sequence when no IDs are available", {
  toy_sequence <- tibble::tibble(
    order = c(1, 2, 3, 4),
    state = c("logo", "price", "product", "logo")
  )

  out <- prepare_gazepoint_semimarkov_data(
    toy_sequence,
    state_col = "state",
    time_col = "order",
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 1)
  expect_true(all(out$state_sequence$.sequence_key == "sequence_1"))
})

test_that("prepare_gazepoint_semimarkov_data checks invalid inputs", {
  toy_sequence <- make_test_semimarkov_sequence()

  expect_error(
    prepare_gazepoint_semimarkov_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(toy_sequence[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      state_col = "bad_state"
    ),
    "`state_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      sequence_id_cols = "bad_id"
    ),
    "All `sequence_id_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      covariate_cols = "bad_covariate"
    ),
    "All `covariate_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      exclude_states = character()
    ),
    "`exclude_states` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      missing_state_label = ""
    ),
    "`missing_state_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      collapse_repeated_states = NA
    ),
    "`collapse_repeated_states` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      include_terminal_states = NA
    ),
    "`include_terminal_states` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      terminal_next_state_label = ""
    ),
    "`terminal_next_state_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_semimarkov_data(
      toy_sequence,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
