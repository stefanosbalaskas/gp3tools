make_test_aoi_entry_data <- function() {
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

test_that("summarise_gazepoint_aoi_entries returns expected entry episodes", {
  x <- make_test_aoi_entry_data()

  out <- summarise_gazepoint_aoi_entries(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7L)
  expect_equal(
    out$aoi_state,
    c("non_aoi", "AOI 1", "non_aoi", "AOI 2", "AOI 1", "non_aoi", "AOI 2")
  )
  expect_equal(out$entry_order, seq_len(7))
  expect_equal(out$entry_start_time, c(0, 100, 300, 400, 600, 800, 900))
  expect_equal(out$entry_duration_ms, c(100, 200, 100, 200, 200, 100, 100))
})

test_that("summarise_gazepoint_aoi_entries records neighbouring AOI states", {
  x <- make_test_aoi_entry_data()

  out <- summarise_gazepoint_aoi_entries(x)

  expect_equal(out$previous_aoi_state[[1]], NA_character_)
  expect_equal(out$next_aoi_state[[1]], "AOI 1")
  expect_equal(out$previous_aoi_state[[2]], "non_aoi")
  expect_equal(out$next_aoi_state[[2]], "non_aoi")
  expect_equal(out$next_aoi_state[[7]], NA_character_)
})

test_that("summarise_gazepoint_aoi_entries can remove non-AOI entries", {
  x <- make_test_aoi_entry_data()

  out <- summarise_gazepoint_aoi_entries(
    x,
    include_non_aoi = FALSE
  )

  expect_equal(nrow(out), 4L)
  expect_false(any(out$is_non_aoi))
  expect_true(all(out$aoi_state %in% c("AOI 1", "AOI 2")))
})

test_that("summarise_gazepoint_aoi_entries treats missing-like AOI labels as non-AOI", {
  x <- tibble::tibble(
    subject = rep("S1", 5),
    MEDIA_ID = rep(0, 5),
    trial_global = rep("S1_M0", 5),
    time = seq(0, 400, by = 100),
    aoi_current = c("missing", NA, "", "AOI 1", "non_aoi")
  )

  out <- summarise_gazepoint_aoi_entries(x)

  expect_true(out$is_non_aoi[out$aoi_state == "missing"])
  expect_true(out$is_non_aoi[out$aoi_state == "missing_aoi"])
  expect_true(out$is_non_aoi[out$aoi_state == "non_aoi"])
  expect_false(out$is_non_aoi[out$aoi_state == "AOI 1"])
})

test_that("summarise_gazepoint_aoi_entries auto-detects AOI fallback column", {
  x <- make_test_aoi_entry_data() |>
    dplyr::rename(AOI = aoi_current)

  out <- summarise_gazepoint_aoi_entries(x)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 7L)
})

test_that("summarise_gazepoint_aoi_entries supports custom grouping columns", {
  x <- make_test_aoi_entry_data() |>
    dplyr::mutate(block = "B1")

  out <- summarise_gazepoint_aoi_entries(
    x,
    group_cols = c("subject", "block")
  )

  expect_true("block" %in% names(out))
  expect_equal(nrow(out), 7L)
})

test_that("summarise_gazepoint_aoi_entries handles one-sample entries", {
  x <- tibble::tibble(
    subject = "S1",
    MEDIA_ID = 0,
    trial_global = "S1_M0",
    time = 0,
    aoi_current = "AOI 1"
  )

  out <- summarise_gazepoint_aoi_entries(x)

  expect_equal(nrow(out), 1L)
  expect_equal(out$entry_duration_ms, 0)
  expect_equal(out$n_samples, 1L)
})

test_that("summarise_gazepoint_aoi_entries errors for invalid inputs", {
  x <- make_test_aoi_entry_data()

  expect_error(
    summarise_gazepoint_aoi_entries("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      x,
      aoi_col = NA_character_
    ),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      x,
      group_cols = c("subject", "subject")
    ),
    "`group_cols` must be a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      x,
      include_non_aoi = NA
    ),
    "`include_non_aoi` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_entries errors when required columns are missing", {
  x <- make_test_aoi_entry_data()

  expect_error(
    summarise_gazepoint_aoi_entries(
      dplyr::select(x, -time)
    ),
    "Missing required columns"
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns"
  )

  expect_error(
    summarise_gazepoint_aoi_entries(
      dplyr::select(x, -aoi_current)
    ),
    "Could not automatically detect an AOI column"
  )
})

test_that("summarise_gazepoint_aoi_entries errors when no valid time remains", {
  x <- make_test_aoi_entry_data() |>
    dplyr::mutate(time = NA_real_)

  expect_error(
    summarise_gazepoint_aoi_entries(x),
    "No non-missing time values remain after filtering"
  )
})

test_that("summarise_gazepoint_aoi_entries works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      out <- summarise_gazepoint_aoi_entries(real_data)

      expect_s3_class(out, "tbl_df")
      expect_true(all(c(
        "entry_id",
        "entry_order",
        "aoi_state",
        "entry_start_time",
        "entry_end_time",
        "entry_duration_ms",
        "n_samples",
        "is_non_aoi"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
