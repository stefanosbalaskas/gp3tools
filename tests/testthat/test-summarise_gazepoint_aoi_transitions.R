make_test_aoi_transition_data <- function() {
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

test_that("summarise_gazepoint_aoi_transitions returns expected full transition summary", {
  x <- make_test_aoi_transition_data()

  out <- summarise_gazepoint_aoi_transitions(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_states, 7L)
  expect_equal(out$n_aoi_states, 4L)
  expect_equal(out$n_non_aoi_states, 3L)
  expect_equal(out$total_transitions, 6L)
  expect_equal(out$self_reentries, 0L)
  expect_equal(out$background_to_target, 1L)
  expect_equal(out$target_to_background, 2L)
  expect_equal(out$background_to_distractor, 2L)
  expect_equal(out$distractor_to_target, 1L)
  expect_equal(out$target_to_distractor, 0L)
  expect_equal(out$transition_feature_status, "ok")
})

test_that("summarise_gazepoint_aoi_transitions supports AOI-only summaries", {
  x <- make_test_aoi_transition_data()

  out <- summarise_gazepoint_aoi_transitions(
    x,
    include_non_aoi = FALSE,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$n_states, 4L)
  expect_equal(out$n_aoi_states, 4L)
  expect_equal(out$n_non_aoi_states, 0L)
  expect_equal(out$total_transitions, 3L)
  expect_equal(out$target_to_distractor, 2L)
  expect_equal(out$distractor_to_target, 1L)
  expect_equal(out$background_to_target, 0L)
  expect_equal(out$target_to_background, 0L)
  expect_equal(out$transition_feature_status, "ok")
})

test_that("summarise_gazepoint_aoi_transitions reports status when target and distractor are undefined", {
  x <- make_test_aoi_transition_data()

  out <- summarise_gazepoint_aoi_transitions(x)

  expect_equal(out$total_transitions, 6L)
  expect_false(out$target_aoi_defined)
  expect_false(out$distractor_aoi_defined)
  expect_equal(
    out$transition_feature_status,
    "no_target_or_distractor_defined"
  )
})

test_that("summarise_gazepoint_aoi_transitions reports status when only target is defined", {
  x <- make_test_aoi_transition_data()

  out <- summarise_gazepoint_aoi_transitions(
    x,
    target_aoi_values = "AOI 1"
  )

  expect_true(out$target_aoi_defined)
  expect_false(out$distractor_aoi_defined)
  expect_equal(out$transition_feature_status, "no_distractor_defined")
})

test_that("summarise_gazepoint_aoi_transitions reports status when only distractor is defined", {
  x <- make_test_aoi_transition_data()

  out <- summarise_gazepoint_aoi_transitions(
    x,
    distractor_aoi_values = "AOI 2"
  )

  expect_false(out$target_aoi_defined)
  expect_true(out$distractor_aoi_defined)
  expect_equal(out$transition_feature_status, "no_target_defined")
})

test_that("summarise_gazepoint_aoi_transitions works from AOI entry tables", {
  x <- make_test_aoi_transition_data()

  entries <- summarise_gazepoint_aoi_entries(x)

  out <- summarise_gazepoint_aoi_transitions(
    entries,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$total_transitions, 6L)
  expect_equal(out$background_to_target, 1L)
  expect_equal(out$distractor_to_target, 1L)
})

test_that("summarise_gazepoint_aoi_transitions works from AOI sequence tables", {
  x <- make_test_aoi_transition_data()

  sequences <- prepare_gazepoint_aoi_sequences(x)

  out <- summarise_gazepoint_aoi_transitions(
    sequences,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$total_transitions, 6L)
  expect_equal(out$target_to_background, 2L)
  expect_equal(out$background_to_distractor, 2L)
})

test_that("summarise_gazepoint_aoi_transitions detects same-AOI reentries", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = c("non_aoi", "AOI 1", "non_aoi", "AOI 1", "non_aoi")
  )

  out <- summarise_gazepoint_aoi_transitions(
    x,
    include_non_aoi = FALSE,
    target_aoi_values = "AOI 1"
  )

  expect_equal(out$total_transitions, 1L)
  expect_equal(out$self_reentries, 1L)
  expect_equal(out$target_to_target, 1L)
})

test_that("summarise_gazepoint_aoi_transitions handles sequences with no transitions", {
  x <- tibble::tibble(
    subject = "S1",
    MEDIA_ID = 0,
    trial_global = "S1_M0",
    time = 0,
    aoi_current = "AOI 1"
  )

  out <- summarise_gazepoint_aoi_transitions(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$n_states, 1L)
  expect_equal(out$total_transitions, 0L)
  expect_equal(out$self_reentries, 0L)
  expect_equal(out$total_pre_transition_dwell_ms, 0)
  expect_equal(out$transition_feature_status, "no_transitions")
})

test_that("summarise_gazepoint_aoi_transitions supports custom grouping columns", {
  x <- make_test_aoi_transition_data() |>
    dplyr::mutate(block = "B1")

  out <- summarise_gazepoint_aoi_transitions(
    x,
    group_cols = c("subject", "block"),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_true("block" %in% names(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$total_transitions, 6L)
})

test_that("summarise_gazepoint_aoi_transitions errors for invalid inputs", {
  x <- make_test_aoi_transition_data()

  expect_error(
    summarise_gazepoint_aoi_transitions("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      aoi_col = NA_character_
    ),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      target_aoi_values = NA_character_
    ),
    "`target_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      distractor_aoi_values = NA_character_
    ),
    "`distractor_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_transitions errors when required columns are missing", {
  x <- make_test_aoi_transition_data()

  expect_error(
    summarise_gazepoint_aoi_transitions(
      dplyr::select(x, -time)
    ),
    "Missing required columns|Could not automatically detect|No non-missing time values",
    perl = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns"
  )

  expect_error(
    summarise_gazepoint_aoi_transitions(
      dplyr::select(x, -aoi_current)
    ),
    "Could not automatically detect an AOI column"
  )
})

test_that("summarise_gazepoint_aoi_transitions works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      out <- summarise_gazepoint_aoi_transitions(
        real_data,
        target_aoi_values = "AOI 2",
        distractor_aoi_values = c("AOI 0", "AOI 1")
      )

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "total_transitions",
        "self_reentries",
        "target_to_distractor",
        "distractor_to_target",
        "background_to_target",
        "target_to_background",
        "mean_pre_transition_dwell_ms",
        "transition_feature_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
