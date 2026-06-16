make_test_markovchain_sequence <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
    trial = c(1, 1, 1, 1, 1, 1, 1, 1, 1),
    time = c(0, 100, 200, 300, 400, 0, 100, 200, 300),
    aoi_current = c(
      "logo", "product", "product", "price", "logo",
      "logo", "price", "outside", "product"
    )
  )
}

test_that("create_gazepoint_markovchain_object creates a complete Markov object", {
  toy_sequence <- make_test_markovchain_sequence()

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    include_self_transitions = TRUE,
    laplace = 0,
    name = "toy_aoi_chain"
  )

  expect_s3_class(out, "gp3_markovchain_object")
  expect_type(out, "list")

  expect_true(
    all(
      c(
        "overview",
        "states",
        "sequence_data",
        "transitions",
        "transition_summary",
        "transition_counts",
        "transition_probabilities",
        "transition_count_matrix",
        "transition_probability_matrix",
        "settings"
      ) %in% names(out)
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$sequence_data, "tbl_df")
  expect_s3_class(out$transitions, "tbl_df")
  expect_s3_class(out$transition_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_aoi_chain")
  expect_equal(out$overview$n_input_rows, 9)
  expect_equal(out$overview$n_rows_used, 8)
  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_states, 3)
  expect_equal(out$overview$n_transitions, 6)
  expect_equal(out$overview$n_self_transitions, 1)
  expect_equal(out$overview$n_excluded_states_removed, 1)

  expect_equal(out$states, c("logo", "price", "product"))

  expect_equal(
    out$transition_count_matrix,
    matrix(
      c(
        0, 1, 0,
        1, 0, 1,
        1, 1, 1
      ),
      nrow = 3,
      ncol = 3,
      dimnames = list(
        c("logo", "price", "product"),
        c("logo", "price", "product")
      )
    )
  )

  expect_equal(
    out$transition_probability_matrix["logo", ],
    c(logo = 0, price = 0.5, product = 0.5)
  )
  expect_equal(
    out$transition_probability_matrix["price", ],
    c(logo = 0.5, price = 0, product = 0.5)
  )
  expect_equal(
    out$transition_probability_matrix["product", ],
    c(logo = 0, price = 0.5, product = 0.5)
  )

  expect_equal(
    rowSums(out$transition_probability_matrix),
    c(logo = 1, price = 1, product = 1)
  )
})

test_that("create_gazepoint_markovchain_object auto-detects common columns", {
  toy_sequence <- make_test_markovchain_sequence()

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    exclude_states = "outside"
  )

  expect_s3_class(out, "gp3_markovchain_object")
  expect_equal(out$states, c("logo", "price", "product"))
  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_transitions, 6)

  settings <- out$settings

  expect_equal(settings$value[settings$setting == "state_col"], "aoi_current")
  expect_equal(settings$value[settings$setting == "participant_col"], "subject")
  expect_equal(settings$value[settings$setting == "trial_col"], "trial")
  expect_equal(settings$value[settings$setting == "time_col"], "time")
})

test_that("create_gazepoint_markovchain_object respects state order", {
  toy_sequence <- make_test_markovchain_sequence()

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    state_order = c("product", "logo", "price")
  )

  expect_equal(out$states, c("product", "logo", "price"))
  expect_equal(rownames(out$transition_count_matrix), c("product", "logo", "price"))
  expect_equal(colnames(out$transition_count_matrix), c("product", "logo", "price"))
})

test_that("create_gazepoint_markovchain_object removes self transitions when requested", {
  toy_sequence <- make_test_markovchain_sequence()

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    include_self_transitions = FALSE
  )

  expect_equal(out$overview$n_self_transitions, 0)
  expect_equal(out$overview$n_transitions, 5)
  expect_equal(out$transition_count_matrix["product", "product"], 0)
})

test_that("create_gazepoint_markovchain_object supports Laplace smoothing", {
  toy_sequence <- make_test_markovchain_sequence()

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = "outside",
    laplace = 1
  )

  expect_equal(out$overview$laplace, 1)
  expect_equal(rowSums(out$transition_probability_matrix), c(logo = 1, price = 1, product = 1))

  expect_equal(
    out$transition_probability_matrix["logo", ],
    c(logo = 1 / 5, price = 2 / 5, product = 2 / 5)
  )
})

test_that("create_gazepoint_markovchain_object supports missing-state labelling", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    aoi_current = c("logo", NA_character_, "", "product")
  )

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = NULL,
    missing_state_label = "missing_state"
  )

  expect_true("missing_state" %in% out$states)
  expect_equal(out$overview$n_missing_states_labelled, 2)
  expect_equal(out$overview$n_missing_states_removed, 0)
  expect_equal(out$overview$n_transitions, 3)
})

test_that("create_gazepoint_markovchain_object removes missing states by default", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    trial = c(1, 1, 1, 1),
    time = c(0, 100, 200, 300),
    aoi_current = c("logo", NA_character_, "", "product")
  )

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "aoi_current",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    exclude_states = NULL
  )

  expect_false("missing_state" %in% out$states)
  expect_equal(out$overview$n_missing_states_removed, 2)
  expect_equal(out$overview$n_transitions, 1)
  expect_equal(out$transition_count_matrix["logo", "product"], 1)
})

test_that("create_gazepoint_markovchain_object supports sequence_id_cols", {
  toy_sequence <- tibble::tibble(
    participant = c("S1", "S1", "S1", "S1", "S1", "S1"),
    block = c("A", "A", "A", "B", "B", "B"),
    time = c(0, 100, 200, 0, 100, 200),
    state = c("logo", "price", "product", "product", "price", "logo")
  )

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "state",
    time_col = "time",
    sequence_id_cols = c("participant", "block"),
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 2)
  expect_equal(out$overview$n_transitions, 4)
  expect_true(all(c("S1||A", "S1||B") %in% out$sequence_data$.sequence_key))
})

test_that("create_gazepoint_markovchain_object uses one sequence when no IDs are available", {
  toy_sequence <- tibble::tibble(
    order = c(1, 2, 3, 4),
    state = c("logo", "price", "product", "logo")
  )

  out <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "state",
    time_col = "order",
    exclude_states = NULL
  )

  expect_equal(out$overview$n_sequences, 1)
  expect_equal(out$overview$n_transitions, 3)
  expect_true(all(out$sequence_data$.sequence_key == "sequence_1"))
})

test_that("create_gazepoint_markovchain_object handles empty outgoing rows", {
  toy_sequence <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial = c(1, 1, 1),
    time = c(0, 100, 200),
    state = c("logo", "price", "logo")
  )

  out_self <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    state_order = c("logo", "price", "product"),
    exclude_states = NULL,
    empty_state_handling = "self"
  )

  expect_equal(out_self$transition_probability_matrix["product", ], c(logo = 0, price = 0, product = 1))

  out_zero <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    state_order = c("logo", "price", "product"),
    exclude_states = NULL,
    empty_state_handling = "zero"
  )

  expect_equal(out_zero$transition_probability_matrix["product", ], c(logo = 0, price = 0, product = 0))

  out_na <- create_gazepoint_markovchain_object(
    toy_sequence,
    state_col = "state",
    participant_col = "subject",
    trial_col = "trial",
    time_col = "time",
    state_order = c("logo", "price", "product"),
    exclude_states = NULL,
    empty_state_handling = "NA"
  )

  expect_true(all(is.na(out_na$transition_probability_matrix["product", ])))
})

test_that("create_gazepoint_markovchain_object checks invalid inputs", {
  toy_sequence <- make_test_markovchain_sequence()

  expect_error(
    create_gazepoint_markovchain_object(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(toy_sequence[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      state_col = "bad_state"
    ),
    "`state_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      sequence_id_cols = "bad_id"
    ),
    "All `sequence_id_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      state_order = character()
    ),
    "`state_order` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      exclude_states = character()
    ),
    "`exclude_states` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      missing_state_label = ""
    ),
    "`missing_state_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      include_self_transitions = NA
    ),
    "`include_self_transitions` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      laplace = -1
    ),
    "`laplace` must be a finite non-negative number",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_markovchain_object(
      toy_sequence,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("create_gazepoint_markovchain_object errors when no transitions remain", {
  one_state <- tibble::tibble(
    subject = "S1",
    trial = 1,
    time = 0,
    state = "logo"
  )

  expect_error(
    create_gazepoint_markovchain_object(
      one_state,
      state_col = "state",
      participant_col = "subject",
      trial_col = "trial",
      time_col = "time",
      exclude_states = NULL
    ),
    "No transitions could be created",
    fixed = TRUE
  )

  self_only <- tibble::tibble(
    subject = c("S1", "S1"),
    trial = c(1, 1),
    time = c(0, 100),
    state = c("logo", "logo")
  )

  expect_error(
    create_gazepoint_markovchain_object(
      self_only,
      state_col = "state",
      participant_col = "subject",
      trial_col = "trial",
      time_col = "time",
      include_self_transitions = FALSE,
      exclude_states = NULL
    ),
    "No transitions could be created",
    fixed = TRUE
  )
})
