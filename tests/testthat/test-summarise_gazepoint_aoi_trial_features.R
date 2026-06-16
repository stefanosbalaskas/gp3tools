make_test_aoi_trial_feature_data <- function() {
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

test_that("summarise_gazepoint_aoi_trial_features returns expected toy features", {
  x <- make_test_aoi_trial_feature_data()

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_equal(out$n_entries, 7L)
  expect_equal(out$n_aoi_entries, 4L)
  expect_equal(out$n_non_aoi_entries, 3L)
  expect_equal(out$n_unique_aoi_states, 2L)

  expect_equal(out$total_entry_dwell_ms, 1000)
  expect_equal(out$total_aoi_dwell_ms, 700)
  expect_equal(out$total_non_aoi_dwell_ms, 300)
  expect_equal(out$aoi_dwell_prop, 0.7)
  expect_equal(out$non_aoi_dwell_prop, 0.3)

  expect_equal(out$first_aoi_state, "AOI 1")
  expect_equal(out$last_aoi_state, "AOI 2")
  expect_equal(out$first_aoi_time_ms, 100)
  expect_equal(out$last_aoi_time_ms, 900)

  expect_equal(out$target_entries, 2L)
  expect_equal(out$target_revisits, 1L)
  expect_equal(out$target_dwell_ms, 400)
  expect_equal(out$target_ttff_ms, 100)

  expect_equal(out$distractor_entries, 2L)
  expect_equal(out$distractor_revisits, 1L)
  expect_equal(out$distractor_dwell_ms, 300)
  expect_equal(out$distractor_ttff_ms, 400)

  expect_equal(out$total_transitions, 6L)
  expect_equal(out$background_to_target, 1L)
  expect_equal(out$target_to_background, 2L)
  expect_equal(out$background_to_distractor, 2L)
  expect_equal(out$distractor_to_target, 1L)

  expect_equal(out$aoi_trial_feature_status, "ok")
  expect_equal(out$transition_feature_status, "ok")
})

test_that("summarise_gazepoint_aoi_trial_features supports AOI-only summaries", {
  x <- make_test_aoi_trial_feature_data()

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    include_non_aoi = FALSE,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$n_entries, 4L)
  expect_equal(out$n_aoi_entries, 4L)
  expect_equal(out$n_non_aoi_entries, 0L)
  expect_equal(out$total_aoi_dwell_ms, 700)
  expect_equal(out$total_non_aoi_dwell_ms, 0)
  expect_equal(out$total_transitions, 3L)
  expect_equal(out$target_to_distractor, 2L)
  expect_equal(out$distractor_to_target, 1L)
  expect_equal(out$self_reentries, 0L)
  expect_equal(out$aoi_trial_feature_status, "ok")
})

test_that("summarise_gazepoint_aoi_trial_features works from AOI entry tables", {
  x <- make_test_aoi_trial_feature_data()

  entries <- summarise_gazepoint_aoi_entries(x)

  out <- summarise_gazepoint_aoi_trial_features(
    entries,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(out$n_entries, 7L)
  expect_equal(out$total_aoi_dwell_ms, 700)
  expect_equal(out$total_transitions, 6L)
})

test_that("summarise_gazepoint_aoi_trial_features reports status when target and distractor are undefined", {
  x <- make_test_aoi_trial_feature_data()

  out <- summarise_gazepoint_aoi_trial_features(x)

  expect_false(out$target_aoi_defined)
  expect_false(out$distractor_aoi_defined)
  expect_equal(out$aoi_trial_feature_status, "no_target_or_distractor_defined")
})

test_that("summarise_gazepoint_aoi_trial_features reports target-not-observed status", {
  x <- make_test_aoi_trial_feature_data()

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    target_aoi_values = "AOI X",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$target_entries, 0L)
  expect_equal(out$distractor_entries, 2L)
  expect_equal(out$aoi_trial_feature_status, "target_not_observed")
})

test_that("summarise_gazepoint_aoi_trial_features reports distractor-not-observed status", {
  x <- make_test_aoi_trial_feature_data()

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI X"
  )

  expect_equal(out$target_entries, 2L)
  expect_equal(out$distractor_entries, 0L)
  expect_equal(out$aoi_trial_feature_status, "distractor_not_observed")
})

test_that("summarise_gazepoint_aoi_trial_features supports custom grouping columns", {
  x <- make_test_aoi_trial_feature_data() |>
    dplyr::mutate(block = "B1")

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    group_cols = c("subject", "block"),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_true("block" %in% names(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$total_transitions, 6L)
})

test_that("summarise_gazepoint_aoi_trial_features handles multiple trials", {
  x <- dplyr::bind_rows(
    make_test_aoi_trial_feature_data(),
    make_test_aoi_trial_feature_data() |>
      dplyr::mutate(
        subject = "S2",
        MEDIA_ID = 1,
        trial_global = "S2_M1"
      )
  )

  out <- summarise_gazepoint_aoi_trial_features(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$n_entries, c(7L, 7L))
})

test_that("summarise_gazepoint_aoi_trial_features errors when no AOI entries remain", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = rep("non_aoi", 5)
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      include_non_aoi = FALSE
    ),
    "No AOI entries remain after applying `include_non_aoi`"
  )
})

test_that("summarise_gazepoint_aoi_trial_features errors for invalid inputs", {
  x <- make_test_aoi_trial_feature_data()

  expect_error(
    summarise_gazepoint_aoi_trial_features("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      aoi_col = NA_character_
    ),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      target_aoi_values = NA_character_
    ),
    "`target_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      distractor_aoi_values = NA_character_
    ),
    "`distractor_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_trial_features errors when required columns are missing", {
  x <- make_test_aoi_trial_feature_data()

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      dplyr::select(x, -time)
    ),
    "Missing required columns|Could not automatically detect|No non-missing time values",
    perl = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns"
  )

  expect_error(
    summarise_gazepoint_aoi_trial_features(
      dplyr::select(x, -aoi_current)
    ),
    "Could not automatically detect an AOI column"
  )
})

test_that("summarise_gazepoint_aoi_trial_features works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      out <- summarise_gazepoint_aoi_trial_features(
        real_data,
        target_aoi_values = "AOI 2",
        distractor_aoi_values = c("AOI 0", "AOI 1")
      )

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "n_entries",
        "n_aoi_entries",
        "total_aoi_dwell_ms",
        "aoi_dwell_prop",
        "target_entries",
        "target_revisits",
        "distractor_entries",
        "total_transitions",
        "background_to_target",
        "target_to_background",
        "aoi_trial_feature_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
