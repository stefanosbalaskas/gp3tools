make_test_aoi_transition_matrix_data <- function() {
  tibble::tibble(
    subject = rep("S1", 10),
    MEDIA_ID = rep(0, 10),
    trial_global = rep("S1_M0", 10),
    condition = rep("A", 10),
    time = seq(0, 900, by = 100),
    aoi_current = c(
      "non_aoi", "AOI 1", "AOI 1", "non_aoi", "AOI 2",
      "AOI 2", "AOI 1", "AOI 1", "non_aoi", "AOI 2"
    )
  )
}

test_that("compute_gazepoint_aoi_transition_matrix returns expected full matrix", {
  x <- make_test_aoi_transition_matrix_data()

  out <- compute_gazepoint_aoi_transition_matrix(x)

  expect_s3_class(out, "gp3_aoi_transition_matrix")
  expect_true(is.matrix(out$count_matrix))
  expect_true(is.matrix(out$probability_matrix))
  expect_s3_class(out$long_table, "tbl_df")

  expect_equal(out$count_matrix["non_aoi", "AOI 1"], 1)
  expect_equal(out$count_matrix["non_aoi", "AOI 2"], 2)
  expect_equal(out$count_matrix["AOI 1", "non_aoi"], 2)
  expect_equal(out$count_matrix["AOI 2", "AOI 1"], 1)

  expect_equal(out$probability_matrix["non_aoi", "AOI 1"], 1 / 3)
  expect_equal(out$probability_matrix["non_aoi", "AOI 2"], 2 / 3)
  expect_equal(out$probability_matrix["AOI 1", "non_aoi"], 1)
  expect_equal(out$probability_matrix["AOI 2", "AOI 1"], 1)
})

test_that("compute_gazepoint_aoi_transition_matrix returns expected AOI-only matrix", {
  x <- make_test_aoi_transition_matrix_data()

  out <- compute_gazepoint_aoi_transition_matrix(
    x,
    include_non_aoi = FALSE
  )

  expect_equal(out$count_matrix["AOI 1", "AOI 2"], 2)
  expect_equal(out$count_matrix["AOI 2", "AOI 1"], 1)
  expect_equal(out$probability_matrix["AOI 1", "AOI 2"], 1)
  expect_equal(out$probability_matrix["AOI 2", "AOI 1"], 1)
  expect_false("non_aoi" %in% rownames(out$count_matrix))
})

test_that("compute_gazepoint_aoi_transition_matrix can remove self-transitions", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = c("non_aoi", "AOI 1", "non_aoi", "AOI 1", "non_aoi")
  )

  with_self <- compute_gazepoint_aoi_transition_matrix(
    x,
    include_non_aoi = FALSE,
    include_self_transitions = TRUE
  )

  without_self <- compute_gazepoint_aoi_transition_matrix(
    x,
    include_non_aoi = FALSE,
    include_self_transitions = FALSE
  )

  expect_equal(with_self$count_matrix["AOI 1", "AOI 1"], 1)
  expect_equal(without_self$count_matrix["AOI 1", "AOI 1"], 0)
  expect_equal(nrow(without_self$long_table), 0L)
})

test_that("compute_gazepoint_aoi_transition_matrix supports explicit state order", {
  x <- make_test_aoi_transition_matrix_data()

  out <- compute_gazepoint_aoi_transition_matrix(
    x,
    states = c("AOI 1", "AOI 2", "non_aoi")
  )

  expect_equal(rownames(out$count_matrix), c("AOI 1", "AOI 2", "non_aoi"))
  expect_equal(colnames(out$count_matrix), c("AOI 1", "AOI 2", "non_aoi"))
})

test_that("compute_gazepoint_aoi_transition_matrix supports grouped matrices", {
  x <- dplyr::bind_rows(
    make_test_aoi_transition_matrix_data(),
    make_test_aoi_transition_matrix_data() |>
      dplyr::mutate(
        subject = "S2",
        MEDIA_ID = 1,
        trial_global = "S2_M1",
        condition = "B"
      )
  )

  out <- compute_gazepoint_aoi_transition_matrix(
    x,
    by_cols = "condition"
  )

  expect_null(out$count_matrix)
  expect_null(out$probability_matrix)
  expect_true(is.list(out$count_matrices))
  expect_true(is.list(out$probability_matrices))
  expect_equal(names(out$count_matrices), c("condition=A", "condition=B"))
  expect_true("condition" %in% names(out$long_table))
})

test_that("compute_gazepoint_aoi_transition_matrix supports time-window filtering", {
  x <- make_test_aoi_transition_matrix_data()

  out <- compute_gazepoint_aoi_transition_matrix(
    x,
    time_window = c(0, 450)
  )

  expect_s3_class(out, "gp3_aoi_transition_matrix")
  expect_true(nrow(out$long_table) > 0)
})

test_that("compute_gazepoint_aoi_transition_matrix works from AOI entry tables", {
  x <- make_test_aoi_transition_matrix_data()

  entries <- summarise_gazepoint_aoi_entries(x)

  out <- compute_gazepoint_aoi_transition_matrix(entries)

  expect_s3_class(out, "gp3_aoi_transition_matrix")
  expect_equal(out$count_matrix["non_aoi", "AOI 1"], 1)
  expect_equal(out$count_matrix["non_aoi", "AOI 2"], 2)
})

test_that("compute_gazepoint_aoi_transition_matrix works from AOI sequence tables", {
  x <- make_test_aoi_transition_matrix_data()

  sequences <- prepare_gazepoint_aoi_sequences(x)

  out <- compute_gazepoint_aoi_transition_matrix(sequences)

  expect_s3_class(out, "gp3_aoi_transition_matrix")
  expect_equal(out$count_matrix["AOI 1", "non_aoi"], 2)
  expect_equal(out$count_matrix["AOI 2", "AOI 1"], 1)
})

test_that("compute_gazepoint_aoi_transition_matrix handles no observed transitions", {
  x <- tibble::tibble(
    subject = "S1",
    MEDIA_ID = 0,
    trial_global = "S1_M0",
    time = 0,
    aoi_current = "AOI 1"
  )

  out <- compute_gazepoint_aoi_transition_matrix(x)

  expect_s3_class(out, "gp3_aoi_transition_matrix")
  expect_equal(nrow(out$long_table), 0L)
  expect_equal(out$count_matrix["AOI 1", "AOI 1"], 0)
  expect_equal(out$probability_matrix["AOI 1", "AOI 1"], 0)
})

test_that("compute_gazepoint_aoi_transition_matrix errors for invalid inputs", {
  x <- make_test_aoi_transition_matrix_data()

  expect_error(
    compute_gazepoint_aoi_transition_matrix("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      aoi_col = NA_character_
    ),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      by_cols = c("condition", "condition")
    ),
    "`by_cols` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      include_self_transitions = NA
    ),
    "`include_self_transitions` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      states = c("AOI 1", "AOI 1")
    ),
    "`states` must be NULL or a character vector of unique AOI labels",
    fixed = TRUE
  )

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      time_window = c(0, NA)
    ),
    "`time_window` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )
})

test_that("compute_gazepoint_aoi_transition_matrix errors when time-window removes all rows", {
  x <- make_test_aoi_transition_matrix_data()

  expect_error(
    compute_gazepoint_aoi_transition_matrix(
      x,
      time_window = c(10000, 20000)
    ),
    "No AOI sequence rows remain after applying `time_window`"
  )
})

test_that("compute_gazepoint_aoi_transition_matrix works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      out <- compute_gazepoint_aoi_transition_matrix(real_data)

      expect_s3_class(out, "gp3_aoi_transition_matrix")
      expect_true(is.matrix(out$count_matrix))
      expect_true(is.matrix(out$probability_matrix))
      expect_s3_class(out$long_table, "tbl_df")
      expect_true(all(c("from", "to", "n", "row_total", "prob") %in% names(out$long_table)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
