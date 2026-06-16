make_test_hmm_sequence <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
    trial = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    time = c(0, 100, 200, 300, 400, 0, 100, 200, 300),
    condition = c("A", "A", "A", "A", "A", "B", "B", "B", "B"),
    aoi_current = c(
      "logo", "product", "product", "price", "logo",
      "logo", "price", "outside", "product"
    ),
    x = c(0.20, 0.50, 0.55, 0.80, 0.25, 0.20, 0.75, NA, 0.55),
    y = c(0.20, 0.50, 0.55, 0.30, 0.25, 0.20, 0.35, NA, 0.55),
    pupil = c(1000, 1020, 1025, 1015, 1005, 1000, 1010, NA, 1030)
  )
}

test_that("prepare_gazepoint_hmm_data creates complete HMM-ready data", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = c("x", "y", "pupil"),
    covariate_cols = "condition",
    exclude_states = "outside",
    scale_numeric_observations = TRUE,
    include_terminal_state = TRUE,
    terminal_state_label = "END",
    name = "toy_hmm"
  )

  expect_s3_class(out, "gp3_hmm_data")
  expect_type(out, "list")

  expect_true(
    all(
      c(
        "overview",
        "states",
        "observation_cols",
        "scaled_observation_cols",
        "sequence_data",
        "observation_data",
        "initial_state_data",
        "initial_state_probabilities",
        "transition_data",
        "transition_summary",
        "transition_counts",
        "transition_probabilities",
        "transition_count_matrix",
        "transition_probability_matrix",
        "sequence_summary",
        "state_summary",
        "observation_summary",
        "emission_data",
        "settings"
      ) %in% names(out)
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$sequence_data, "tbl_df")
  expect_s3_class(out$observation_data, "tbl_df")
  expect_s3_class(out$initial_state_data, "tbl_df")
  expect_s3_class(out$initial_state_probabilities, "tbl_df")
  expect_s3_class(out$transition_data, "tbl_df")
  expect_s3_class(out$transition_summary, "tbl_df")
  expect_s3_class(out$sequence_summary, "tbl_df")
  expect_s3_class(out$state_summary, "tbl_df")
  expect_s3_class(out$observation_summary, "tbl_df")
  expect_s3_class(out$emission_data, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_hmm")
  expect_equal(out$overview$n_input_rows, 9)
  expect_equal(out$overview$n_rows_used, 8)
  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_states, 3)
  expect_equal(out$overview$n_observation_cols, 3)
  expect_equal(out$overview$n_scaled_observation_cols, 3)
  expect_equal(out$overview$n_observations, 8)
  expect_equal(out$overview$n_transitions, 8)
  expect_equal(out$overview$n_terminal_transitions, 2)
  expect_equal(out$overview$n_excluded_states_removed, 1)
  expect_true(out$overview$include_terminal_state)
  expect_true(out$overview$scale_numeric_observations)

  expect_equal(out$states, c("logo", "price", "product"))
  expect_equal(out$observation_cols, c("x", "y", "pupil"))
  expect_equal(out$scaled_observation_cols, c("x_z", "y_z", "pupil_z"))

  expect_true(all(c("x_z", "y_z", "pupil_z") %in% names(out$sequence_data)))
  expect_false("outside" %in% out$sequence_data$state)

  expect_equal(nrow(out$observation_data), 8)
  expect_equal(nrow(out$emission_data), 24)

  expect_equal(
    out$initial_state_probabilities$initial_probability,
    c(1, 0, 0)
  )

  expect_equal(
    out$transition_count_matrix,
    matrix(
      c(
        0, 1, 0,
        1, 0, 1,
        1, 1, 1,
        1, 0, 1
      ),
      nrow = 3,
      ncol = 4,
      dimnames = list(
        c("logo", "price", "product"),
        c("logo", "price", "product", "END")
      )
    )
  )

  expect_equal(
    out$transition_probability_matrix["logo", ],
    c(logo = 0, price = 1 / 3, product = 1 / 3, END = 1 / 3)
  )
  expect_equal(
    out$transition_probability_matrix["price", ],
    c(logo = 1 / 2, price = 0, product = 1 / 2, END = 0)
  )
  expect_equal(
    out$transition_probability_matrix["product", ],
    c(logo = 0, price = 1 / 3, product = 1 / 3, END = 1 / 3)
  )

  expect_equal(
    rowSums(out$transition_probability_matrix),
    c(logo = 1, price = 1, product = 1)
  )
})

test_that("prepare_gazepoint_hmm_data auto-detects common columns and observations", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    exclude_states = "outside"
  )

  expect_s3_class(out, "gp3_hmm_data")
  expect_equal(out$states, c("logo", "price", "product"))
  expect_true(all(c("x", "y", "pupil") %in% out$observation_cols))
  expect_equal(out$overview$n_sequences, 2)

  settings <- out$settings

  expect_equal(settings$value[settings$setting == "state_col"], "aoi_current")
  expect_equal(settings$value[settings$setting == "participant_col"], "subject")
  expect_equal(settings$value[settings$setting == "trial_col"], "trial")
  expect_equal(settings$value[settings$setting == "time_col"], "time")
})

test_that("prepare_gazepoint_hmm_data can omit terminal transitions", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = c("x", "y", "pupil"),
    exclude_states = "outside",
    include_terminal_state = FALSE
  )

  expect_false("END" %in% colnames(out$transition_count_matrix))
  expect_false(any(out$transition_data$is_terminal))
  expect_equal(out$overview$n_terminal_transitions, 0)
  expect_equal(out$overview$n_transitions, 6)
})

test_that("prepare_gazepoint_hmm_data supports custom terminal labels", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = c("x", "y", "pupil"),
    exclude_states = "outside",
    include_terminal_state = TRUE,
    terminal_state_label = "TERMINAL"
  )

  expect_true("TERMINAL" %in% colnames(out$transition_count_matrix))
  expect_false("END" %in% colnames(out$transition_count_matrix))
  expect_true(any(out$transition_data$to_state == "TERMINAL"))
})

test_that("prepare_gazepoint_hmm_data respects state order", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = c("x", "y", "pupil"),
    exclude_states = "outside",
    state_order = c("product", "logo", "price")
  )

  expect_equal(out$states, c("product", "logo", "price"))
  expect_equal(rownames(out$transition_count_matrix), c("product", "logo", "price"))
})

test_that("prepare_gazepoint_hmm_data supports missing-state labelling", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    state = c("logo", NA_character_, "", "product"),
    pupil = c(1000, 1010, 1020, 1030)
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = "pupil",
    exclude_states = NULL,
    missing_state_label = "missing_state"
  )

  expect_true("missing_state" %in% out$states)
  expect_equal(out$overview$n_missing_states_labelled, 2)
  expect_equal(out$overview$n_missing_states_removed, 0)
  expect_equal(out$overview$n_rows_used, 4)
})

test_that("prepare_gazepoint_hmm_data removes missing states by default", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    state = c("logo", NA_character_, "", "product"),
    pupil = c(1000, 1010, 1020, 1030)
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = "pupil",
    exclude_states = NULL
  )

  expect_false("missing_state" %in% out$states)
  expect_equal(out$overview$n_missing_states_removed, 2)
  expect_equal(out$overview$n_rows_used, 2)
  expect_equal(out$overview$n_transitions, 1)
})

test_that("prepare_gazepoint_hmm_data supports sequence_id_cols", {
  toy_sequence <- tibble::tibble(
    participant = c("S1", "S1", "S1", "S1", "S1", "S1"),
    block = c("A", "A", "A", "B", "B", "B"),
    time = c(0, 100, 200, 0, 100, 200),
    state = c("logo", "price", "product", "product", "price", "logo"),
    pupil = c(1000, 1010, 1020, 1030, 1040, 1050)
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    time_col = "time",
    observation_cols = "pupil",
    sequence_id_cols = c("participant", "block"),
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 2)
  expect_true(all(c("S1||A", "S1||B") %in% out$sequence_data$.sequence_key))
})

test_that("prepare_gazepoint_hmm_data supports one sequence when no IDs are available", {
  toy_sequence <- tibble::tibble(
    order = c(1, 2, 3, 4),
    state = c("logo", "price", "product", "logo"),
    pupil = c(1000, 1010, 1020, 1030)
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    time_col = "order",
    observation_cols = "pupil",
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 1)
  expect_true(all(out$sequence_data$.sequence_key == "sequence_1"))
})

test_that("prepare_gazepoint_hmm_data carries covariates", {
  toy_sequence <- make_test_hmm_sequence()

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = c("x", "y", "pupil"),
    covariate_cols = "condition",
    exclude_states = "outside",
    include_terminal_state = TRUE
  )

  expect_true("condition" %in% names(out$sequence_data))
  expect_true("condition" %in% names(out$observation_data))
  expect_true("condition" %in% names(out$transition_data))
})

test_that("prepare_gazepoint_hmm_data supports categorical observations", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    state = c("logo", "price", "price", "logo"),
    fixation_type = c("short", "long", "long", "short")
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = "fixation_type",
    exclude_states = NULL
  )

  expect_equal(out$overview$n_observation_cols, 1)
  expect_equal(unique(out$observation_summary$observation_type), "categorical")
  expect_true("short" %in% out$observation_summary$most_common_value)
  expect_true("long" %in% out$observation_summary$most_common_value)
})

test_that("prepare_gazepoint_hmm_data handles no observation columns", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial = c(1, 1, 1),
    time = c(0, 100, 200),
    state = c("logo", "price", "logo")
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = NULL,
    exclude_states = NULL
  )

  expect_equal(out$overview$n_observation_cols, 0)
  expect_equal(nrow(out$observation_summary), 0)
  expect_equal(nrow(out$emission_data), 0)
})

test_that("prepare_gazepoint_hmm_data scales constant numeric observations safely", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial = c(1, 1, 1),
    time = c(0, 100, 200),
    state = c("logo", "price", "logo"),
    pupil = c(1000, 1000, 1000)
  )

  out <- prepare_gazepoint_hmm_data(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    observation_cols = "pupil",
    exclude_states = NULL,
    scale_numeric_observations = TRUE
  )

  expect_equal(out$scaled_observation_cols, "pupil_z")
  expect_equal(out$sequence_data$pupil_z, c(0, 0, 0))
})

test_that("prepare_gazepoint_hmm_data checks invalid inputs", {
  toy_sequence <- make_test_hmm_sequence()

  expect_error(
    prepare_gazepoint_hmm_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(toy_sequence[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      state_col = "bad_state"
    ),
    "`state_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      observation_cols = "bad_observation"
    ),
    "All `observation_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      sequence_id_cols = "bad_id"
    ),
    "All `sequence_id_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      covariate_cols = "bad_covariate"
    ),
    "All `covariate_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      state_order = character()
    ),
    "`state_order` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      exclude_states = character()
    ),
    "`exclude_states` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      missing_state_label = ""
    ),
    "`missing_state_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      scale_numeric_observations = NA
    ),
    "`scale_numeric_observations` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      include_terminal_state = NA
    ),
    "`include_terminal_state` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      terminal_state_label = ""
    ),
    "`terminal_state_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      toy_sequence,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_hmm_data errors when no transitions can be created", {
  one_state <- tibble::tibble(
    subject = "S1",
    trial = 1,
    time = 0,
    state = "logo",
    pupil = 1000
  )

  expect_error(
    prepare_gazepoint_hmm_data(
      one_state,
      state_col = "state",
      participant_col = "subject",
      trial_col = "trial",
      time_col = "time",
      observation_cols = "pupil",
      exclude_states = NULL,
      include_terminal_state = FALSE
    ),
    "No HMM transitions could be created",
    fixed = TRUE
  )
})
