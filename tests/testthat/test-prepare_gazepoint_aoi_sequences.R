make_test_aoi_sequence_data <- function() {
  tibble::tibble(
    subject = rep("S1", 10),
    MEDIA_ID = rep(0, 10),
    trial_global = rep("S1_M0", 10),
    time = seq(0, 900, by = 100),
    aoi_current = c(
      "non_aoi", "AOI 1", "AOI 1", "non_aoi", "AOI 2",
      "AOI 2", "AOI 1", "AOI 1", "non_aoi", "AOI 2"
    )
  )
}

test_that("prepare_gazepoint_aoi_sequences returns expected full sequence", {
  x <- make_test_aoi_sequence_data()

  out <- prepare_gazepoint_aoi_sequences(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7L)
  expect_equal(
    out$aoi_state,
    c("non_aoi", "AOI 1", "non_aoi", "AOI 2", "AOI 1", "non_aoi", "AOI 2")
  )
  expect_equal(out$state_order, seq_len(7))
  expect_equal(out$transition_order, c(1L, 2L, 3L, 4L, 5L, 6L, NA_integer_))
  expect_equal(out$transition_from, out$aoi_state)
  expect_equal(
    out$transition_to,
    c("AOI 1", "non_aoi", "AOI 2", "AOI 1", "non_aoi", "AOI 2", NA)
  )
  expect_equal(out$is_terminal_state, c(rep(FALSE, 6), TRUE))
})

test_that("prepare_gazepoint_aoi_sequences can remove non-AOI states", {
  x <- make_test_aoi_sequence_data()

  out <- prepare_gazepoint_aoi_sequences(
    x,
    include_non_aoi = FALSE
  )

  expect_equal(nrow(out), 4L)
  expect_equal(out$aoi_state, c("AOI 1", "AOI 2", "AOI 1", "AOI 2"))
  expect_equal(out$transition_to, c("AOI 2", "AOI 1", "AOI 2", NA))
  expect_false(any(out$is_non_aoi))
})

test_that("prepare_gazepoint_aoi_sequences can remove terminal states", {
  x <- make_test_aoi_sequence_data()

  out <- prepare_gazepoint_aoi_sequences(
    x,
    include_terminal = FALSE
  )

  expect_equal(nrow(out), 6L)
  expect_false(any(out$is_terminal_state))
  expect_false(any(is.na(out$transition_to)))
})

test_that("prepare_gazepoint_aoi_sequences works from AOI entry tables", {
  x <- make_test_aoi_sequence_data()

  entries <- summarise_gazepoint_aoi_entries(x)

  out <- prepare_gazepoint_aoi_sequences(entries)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7L)
  expect_equal(out$aoi_state, entries$aoi_state)
  expect_equal(out$entry_duration_ms, entries$entry_duration_ms)
})

test_that("prepare_gazepoint_aoi_sequences detects same-AOI reentries after removing non-AOI states", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = c("non_aoi", "AOI 1", "non_aoi", "AOI 1", "non_aoi")
  )

  out <- prepare_gazepoint_aoi_sequences(
    x,
    include_non_aoi = FALSE
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$transition_from[[1]], "AOI 1")
  expect_equal(out$transition_to[[1]], "AOI 1")
  expect_true(out$is_self_transition[[1]])
  expect_true(out$is_terminal_state[[2]])
})

test_that("prepare_gazepoint_aoi_sequences supports custom grouping columns", {
  x <- make_test_aoi_sequence_data() |>
    dplyr::mutate(block = "B1")

  out <- prepare_gazepoint_aoi_sequences(
    x,
    group_cols = c("subject", "block")
  )

  expect_true("block" %in% names(out))
  expect_equal(nrow(out), 7L)
})

test_that("prepare_gazepoint_aoi_sequences auto-detects AOI fallback column", {
  x <- make_test_aoi_sequence_data() |>
    dplyr::rename(AOI = aoi_current)

  out <- prepare_gazepoint_aoi_sequences(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7L)
})

test_that("prepare_gazepoint_aoi_sequences errors when no AOI states remain", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = rep("non_aoi", 5)
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      include_non_aoi = FALSE
    ),
    "No AOI states remain after applying `include_non_aoi`"
  )
})

test_that("prepare_gazepoint_aoi_sequences errors for invalid inputs", {
  x <- make_test_aoi_sequence_data()

  expect_error(
    prepare_gazepoint_aoi_sequences("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      aoi_col = NA_character_
    ),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      include_terminal = NA
    ),
    "`include_terminal` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_sequences errors when required columns are missing", {
  x <- make_test_aoi_sequence_data()

  expect_error(
    prepare_gazepoint_aoi_sequences(
      dplyr::select(x, -time)
    ),
    "Missing required columns|Could not automatically detect|No non-missing time values",
    perl = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns"
  )

  expect_error(
    prepare_gazepoint_aoi_sequences(
      dplyr::select(x, -aoi_current)
    ),
    "Could not automatically detect an AOI column"
  )
})

test_that("prepare_gazepoint_aoi_sequences works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      out <- prepare_gazepoint_aoi_sequences(real_data)

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "state_order",
        "transition_order",
        "aoi_state",
        "previous_state",
        "next_state",
        "transition_from",
        "transition_to",
        "dwell_before_transition_ms",
        "is_self_transition",
        "is_terminal_state"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
