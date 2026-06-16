make_test_fixation_trial_data <- function() {
  tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    FPOGID = 1:5,
    FPOGS = c(0.10, 0.40, 0.70, 1.20, 1.60),
    FPOGD = c(0.10, 0.20, 0.15, 0.25, 0.10),
    FPOGX = c(0.2, 0.4, 0.5, 0.6, 0.7),
    FPOGY = c(0.3, 0.3, 0.4, 0.5, 0.6),
    FPOGV = c(1, 1, 1, 1, 1),
    AOI = c("non_aoi", "AOI 1", "AOI 1", "AOI 2", "non_aoi")
  )
}

test_that("summarise_gazepoint_fixation_trials returns expected toy features", {
  x <- make_test_fixation_trial_data()

  out <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1L)

  expect_equal(out$trial_start_time_ms, 100)
  expect_equal(out$trial_end_time_ms, 1700)
  expect_equal(out$trial_duration_ms, 1600)

  expect_equal(out$n_fixations, 5L)
  expect_equal(out$n_valid_fixations, 5L)
  expect_equal(out$n_rows_represented, 5)

  expect_equal(out$total_fixation_duration_ms, 800)
  expect_equal(out$mean_fixation_duration_ms, 160)
  expect_equal(out$median_fixation_duration_ms, 150)
  expect_equal(out$min_fixation_duration_ms, 100)
  expect_equal(out$max_fixation_duration_ms, 250)

  expect_equal(out$n_aoi_fixations, 3L)
  expect_equal(out$n_non_aoi_fixations, 2L)
  expect_equal(out$n_unique_aoi_fixated, 2L)
  expect_equal(out$first_aoi_fixated, "AOI 1")
  expect_equal(out$last_aoi_fixated, "AOI 2")
  expect_equal(out$first_aoi_fixation_time_ms, 400)

  expect_equal(out$target_fixation_count, 2L)
  expect_equal(out$target_revisits, 1L)
  expect_equal(out$target_fixation_duration_ms, 350)
  expect_equal(out$target_ttff_ms, 400)
  expect_equal(out$mean_target_fixation_duration_ms, 175)

  expect_equal(out$distractor_fixation_count, 1L)
  expect_equal(out$distractor_revisits, 0L)
  expect_equal(out$distractor_fixation_duration_ms, 250)
  expect_equal(out$distractor_ttff_ms, 1200)
  expect_equal(out$mean_distractor_fixation_duration_ms, 250)

  expect_equal(out$fixation_rate_per_sec, 5 / 1.6)
  expect_equal(out$fixation_duration_prop, 0.5)
  expect_equal(out$aoi_fixation_prop, 3 / 5)
  expect_equal(out$non_aoi_fixation_prop, 2 / 5)
  expect_equal(out$target_fixation_prop_of_aoi, 2 / 3)
  expect_equal(out$distractor_fixation_prop_of_aoi, 1 / 3)
  expect_equal(out$target_duration_prop_of_aoi, 350 / 600)
  expect_equal(out$distractor_duration_prop_of_aoi, 250 / 600)

  expect_true(out$aoi_available)
  expect_true(out$target_aoi_defined)
  expect_true(out$distractor_aoi_defined)
  expect_equal(out$fixation_trial_feature_status, "ok")
})

test_that("summarise_gazepoint_fixation_trials supports AOI-only fixation summaries", {
  x <- make_test_fixation_trial_data()

  out <- summarise_gazepoint_fixation_trials(
    x,
    include_non_aoi = FALSE,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$n_fixations, 3L)
  expect_equal(out$n_aoi_fixations, 3L)
  expect_equal(out$n_non_aoi_fixations, 0L)
  expect_equal(out$total_fixation_duration_ms, 600)
  expect_equal(out$target_fixation_count, 2L)
  expect_equal(out$distractor_fixation_count, 1L)
  expect_equal(out$fixation_trial_feature_status, "ok")
})

test_that("summarise_gazepoint_fixation_trials filters invalid fixations by default", {
  x <- make_test_fixation_trial_data()
  x$FPOGV <- c(1, 0, 1, 1, 1)

  out_valid <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  out_all <- summarise_gazepoint_fixation_trials(
    x,
    valid_only = FALSE,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out_valid$n_fixations, 4L)
  expect_equal(out_valid$target_fixation_count, 1L)
  expect_equal(out_valid$target_fixation_duration_ms, 150)

  expect_equal(out_all$n_fixations, 5L)
  expect_equal(out_all$target_fixation_count, 2L)
  expect_equal(out_all$target_fixation_duration_ms, 350)
})

test_that("summarise_gazepoint_fixation_trials detects USER_FILE before uninformative USER", {
  x <- dplyr::bind_rows(
    make_test_fixation_trial_data() |>
      dplyr::mutate(
        USER = NA,
        USER_FILE = "User 1_fixations.csv"
      ) |>
      dplyr::select(-subject, -trial_global),
    make_test_fixation_trial_data() |>
      dplyr::mutate(
        USER = NA,
        USER_FILE = "User 2_fixations.csv",
        MEDIA_ID = 1
      ) |>
      dplyr::select(-subject, -trial_global)
  )

  out <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_true("USER_FILE" %in% names(out))
  expect_equal(nrow(out), 2L)
  expect_equal(
    sort(unique(out$USER_FILE)),
    c("User 1_fixations.csv", "User 2_fixations.csv")
  )
})

test_that("summarise_gazepoint_fixation_trials works without an AOI column", {
  x <- make_test_fixation_trial_data() |>
    dplyr::select(-AOI)

  out <- summarise_gazepoint_fixation_trials(x)

  expect_s3_class(out, "tbl_df")
  expect_false(out$aoi_available)
  expect_equal(out$n_aoi_fixations, 0L)
  expect_equal(out$fixation_trial_feature_status, "no_aoi_column")
})

test_that("summarise_gazepoint_fixation_trials supports explicit column names and millisecond units", {
  x <- tibble::tibble(
    participant = rep("P1", 3),
    stimulus = rep("Stimulus A", 3),
    fix_id = 1:3,
    start_time_ms = c(100, 500, 900),
    duration_ms = c(100, 200, 300),
    x = c(0.1, 0.2, 0.3),
    y = c(0.4, 0.5, 0.6),
    valid = c(TRUE, TRUE, TRUE),
    aoi = c("AOI 1", "AOI 2", "none")
  )

  out <- summarise_gazepoint_fixation_trials(
    x,
    group_cols = c("participant", "stimulus"),
    fixation_id_col = "fix_id",
    start_col = "start_time_ms",
    duration_col = "duration_ms",
    x_col = "x",
    y_col = "y",
    valid_col = "valid",
    aoi_col = "aoi",
    start_time_unit = "ms",
    duration_unit = "ms",
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(out$n_fixations, 3L)
  expect_equal(out$total_fixation_duration_ms, 600)
  expect_equal(out$target_fixation_count, 1L)
  expect_equal(out$distractor_fixation_count, 1L)
})

test_that("summarise_gazepoint_fixation_trials supports multiple groups", {
  x <- dplyr::bind_rows(
    make_test_fixation_trial_data(),
    make_test_fixation_trial_data() |>
      dplyr::mutate(
        subject = "S2",
        MEDIA_ID = 1,
        trial_global = "S2_M1"
      )
  )

  out <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_equal(nrow(out), 2L)
  expect_equal(out$n_fixations, c(5L, 5L))
})

test_that("summarise_gazepoint_fixation_trials reports target and distractor status cases", {
  x <- make_test_fixation_trial_data()

  no_defs <- summarise_gazepoint_fixation_trials(x)
  expect_equal(
    no_defs$fixation_trial_feature_status,
    "no_target_or_distractor_defined"
  )

  target_missing <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI X",
    distractor_aoi_values = "AOI 2"
  )
  expect_equal(target_missing$fixation_trial_feature_status, "target_not_observed")

  distractor_missing <- summarise_gazepoint_fixation_trials(
    x,
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI X"
  )
  expect_equal(
    distractor_missing$fixation_trial_feature_status,
    "distractor_not_observed"
  )
})

test_that("summarise_gazepoint_fixation_trials errors when no rows remain after filtering", {
  x <- make_test_fixation_trial_data()
  x$FPOGV <- 0

  expect_error(
    summarise_gazepoint_fixation_trials(x),
    "No fixation rows remain after filtering",
    fixed = TRUE
  )

  only_non_aoi <- make_test_fixation_trial_data()
  only_non_aoi$AOI <- "non_aoi"

  expect_error(
    summarise_gazepoint_fixation_trials(
      only_non_aoi,
      include_non_aoi = FALSE
    ),
    "No fixation rows remain after filtering",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_fixation_trials errors for invalid inputs", {
  x <- make_test_fixation_trial_data()

  expect_error(
    summarise_gazepoint_fixation_trials("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      fixation_id_col = NA_character_
    ),
    "`fixation_id_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      start_col = NA_character_
    ),
    "`start_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      duration_col = NA_character_
    ),
    "`duration_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      valid_only = NA
    ),
    "`valid_only` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      target_aoi_values = NA_character_
    ),
    "`target_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      distractor_aoi_values = NA_character_
    ),
    "`distractor_aoi_values` must be NULL or a character vector of non-missing labels",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_fixation_trials errors when required columns are missing", {
  x <- make_test_fixation_trial_data()

  expect_error(
    summarise_gazepoint_fixation_trials(
      dplyr::select(x, -FPOGS)
    ),
    "Could not automatically detect required fixation columns: start_col",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      dplyr::select(x, -FPOGD)
    ),
    "Could not automatically detect required fixation columns: duration_col",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_fixation_trials(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns"
  )
})

test_that("summarise_gazepoint_fixation_trials works with real all_fix object when available", {
  if (exists("all_fix", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("all_fix", envir = .GlobalEnv, inherits = TRUE)

    if (all(c("FPOGS", "FPOGD") %in% names(real_data))) {
      out <- summarise_gazepoint_fixation_trials(
        real_data,
        target_aoi_values = "AOI 2",
        distractor_aoi_values = c("AOI 0", "AOI 1")
      )

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "n_fixations",
        "total_fixation_duration_ms",
        "mean_fixation_duration_ms",
        "fixation_rate_per_sec",
        "n_aoi_fixations",
        "target_fixation_count",
        "distractor_fixation_count",
        "fixation_trial_feature_status"
      ) %in% names(out)))

      if ("USER_FILE" %in% names(real_data)) {
        expect_true("USER_FILE" %in% names(out))
      }
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
