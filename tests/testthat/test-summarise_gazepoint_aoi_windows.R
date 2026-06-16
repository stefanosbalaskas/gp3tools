make_test_aoi_window_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 10),
    condition = rep(c("A", "B"), each = 10),
    MEDIA_ID = rep(c(1, 2), each = 10),
    trial_global = rep(c("S1_M1", "S2_M2"), each = 10),
    time = rep(seq(0, 900, by = 100), 2),
    aoi_current = rep(
      c(
        "non_aoi",
        "AOI 1",
        "AOI 1",
        "AOI 2",
        "AOI 2",
        "non_aoi",
        "AOI 1",
        "AOI 3",
        "missing",
        "AOI 1"
      ),
      2
    )
  )
}

test_that("summarise_gazepoint_aoi_windows summarises numeric windows", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = c("AOI 2", "AOI 3")
  )

  expect_s3_class(out, "gp3_aoi_window_summary")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4L)

  expect_true(all(c(
    "subject",
    "condition",
    "MEDIA_ID",
    "trial_global",
    "window_label",
    "window_start_ms",
    "window_end_ms",
    "n_window_samples",
    "n_target_samples",
    "n_distractor_samples",
    "n_non_aoi_samples",
    "n_missing_aoi_samples",
    "n_aoi_samples",
    "n_valid_denominator_samples",
    "target_sample_prop_all",
    "target_sample_prop_valid",
    "target_sample_prop_aoi",
    "distractor_sample_prop_all",
    "valid_denominator_prop",
    "target_aoi_defined",
    "distractor_aoi_defined",
    "aoi_window_status"
  ) %in% names(out)))

  first_row <- out |>
    dplyr::filter(subject == "S1", window_label == "0_500ms")

  expect_equal(first_row$n_window_samples, 5L)
  expect_equal(first_row$n_target_samples, 2L)
  expect_equal(first_row$n_distractor_samples, 2L)
  expect_equal(first_row$n_non_aoi_samples, 1L)
  expect_equal(first_row$n_missing_aoi_samples, 0L)
  expect_equal(first_row$n_valid_denominator_samples, 5L)
  expect_equal(first_row$target_sample_prop_valid, 2 / 5)
  expect_equal(first_row$aoi_window_status, "ok")
})

test_that("summarise_gazepoint_aoi_windows preserves condition when group_cols omit it", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  expect_true("condition" %in% names(out))
  expect_equal(sort(unique(out$condition)), c("A", "B"))
})

test_that("summarise_gazepoint_aoi_windows orders windows chronologically", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000, 2000),
    group_cols = c("subject", "MEDIA_ID", "trial_global"),
    target_aoi_values = "AOI 1"
  )

  labels_s1 <- out |>
    dplyr::filter(subject == "S1") |>
    dplyr::pull(window_label)

  expect_equal(
    labels_s1,
    c("0_500ms", "500_1000ms")
  )
})

test_that("summarise_gazepoint_aoi_windows supports data-frame windows", {
  x <- make_test_aoi_window_data()

  windows <- tibble::tibble(
    label = c("early", "late"),
    start = c(0, 500),
    end = c(500, 1000)
  )

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = windows,
    window_label_col = "label",
    window_start_col = "start",
    window_end_col = "end",
    target_aoi_values = "AOI 1",
    distractor_aoi_values = c("AOI 2", "AOI 3")
  )

  expect_equal(sort(unique(out$window_label)), c("early", "late"))
  expect_equal(sort(unique(out$window_start_ms)), c(0, 500))
  expect_equal(sort(unique(out$window_end_ms)), c(500, 1000))
})

test_that("summarise_gazepoint_aoi_windows auto-detects AOI column", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    aoi_col = NULL,
    target_aoi_values = "AOI 1"
  )

  expect_s3_class(out, "gp3_aoi_window_summary")
  expect_equal(attr(out, "settings")$aoi_col, "aoi_current")
})

test_that("summarise_gazepoint_aoi_windows handles AOI column named AOI", {
  x <- make_test_aoi_window_data() |>
    dplyr::rename(AOI = aoi_current)

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1"
  )

  expect_s3_class(out, "gp3_aoi_window_summary")
  expect_equal(attr(out, "settings")$aoi_col, "AOI")
})

test_that("summarise_gazepoint_aoi_windows handles missing condition column", {
  x <- make_test_aoi_window_data() |>
    dplyr::select(-condition)

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1"
  )

  expect_true("condition" %in% names(out))
  expect_equal(unique(out$condition), "all_data")
})

test_that("summarise_gazepoint_aoi_windows supports condition_col = NULL", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    condition_col = NULL,
    target_aoi_values = "AOI 1"
  )

  expect_false("condition" %in% names(out))
  expect_s3_class(out, "gp3_aoi_window_summary")
})

test_that("summarise_gazepoint_aoi_windows handles missing AOI values", {
  x <- make_test_aoi_window_data()
  x$aoi_current[1] <- NA_character_
  x$aoi_current[2] <- ""

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  s1_early <- out |>
    dplyr::filter(subject == "S1", window_label == "0_500ms")

  expect_equal(s1_early$n_missing_aoi_samples, 2L)
  expect_equal(s1_early$n_valid_denominator_samples, 3L)
})

test_that("summarise_gazepoint_aoi_windows supports right endpoint inclusion", {
  x <- tibble::tibble(
    subject = "S1",
    condition = "A",
    time = c(0, 500, 1000),
    aoi_current = c("AOI 1", "AOI 1", "AOI 2")
  )

  out_left_closed <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1",
    include_right_endpoint = FALSE
  )

  out_right_included <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = "AOI 1",
    include_right_endpoint = TRUE
  )

  early_left <- out_left_closed |>
    dplyr::filter(window_label == "0_500ms")

  early_right <- out_right_included |>
    dplyr::filter(window_label == "0_500ms")

  expect_equal(early_left$n_window_samples, 1L)
  expect_equal(early_right$n_window_samples, 2L)
})

test_that("summarise_gazepoint_aoi_windows returns no-target status when target undefined", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    target_aoi_values = NULL
  )

  expect_true(all(out$aoi_window_status == "no_target_aoi_defined"))
  expect_false(any(out$target_aoi_defined))
})

test_that("summarise_gazepoint_aoi_windows stores settings", {
  x <- make_test_aoi_window_data()

  out <- summarise_gazepoint_aoi_windows(
    x,
    windows = c(0, 500, 1000),
    time_col = "time",
    aoi_col = "aoi_current",
    subject_col = "subject",
    condition_col = "condition",
    group_cols = c("subject", "condition"),
    target_aoi_values = "AOI 1",
    distractor_aoi_values = "AOI 2"
  )

  settings <- attr(out, "settings")

  expect_equal(settings$time_col, "time")
  expect_equal(settings$aoi_col, "aoi_current")
  expect_equal(settings$subject_col, "subject")
  expect_equal(settings$condition_col, "condition")
  expect_equal(settings$group_cols, c("subject", "condition"))
  expect_equal(settings$target_aoi_values, "AOI 1")
  expect_equal(settings$distractor_aoi_values, "AOI 2")
})

test_that("summarise_gazepoint_aoi_windows errors for invalid inputs", {
  x <- make_test_aoi_window_data()

  expect_error(
    summarise_gazepoint_aoi_windows("not data", windows = c(0, 500)),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 500), time_col = NA_character_),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 500), aoi_col = NA_character_),
    "`aoi_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 500), subject_col = NA_character_),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 500), condition_col = NA_character_),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 500), include_right_endpoint = NA),
    "`include_right_endpoint` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors for invalid AOI value inputs", {
  x <- make_test_aoi_window_data()

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(0, 500),
      target_aoi_values = c("AOI 1", NA)
    ),
    "`target_aoi_values` must be NULL or a character vector",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(0, 500),
      distractor_aoi_values = c("AOI 2", NA)
    ),
    "`distractor_aoi_values` must be NULL or a character vector",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(0, 500),
      non_aoi_values = c("non_aoi", NA)
    ),
    "`non_aoi_values` must be a character vector",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors when required columns are missing", {
  x <- make_test_aoi_window_data()

  expect_error(
    summarise_gazepoint_aoi_windows(
      dplyr::select(x, -time),
      windows = c(0, 500)
    ),
    "Missing required columns: time",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      dplyr::select(x, -aoi_current),
      windows = c(0, 500),
      aoi_col = "aoi_current"
    ),
    "Missing required columns: aoi_current",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      dplyr::select(x, -subject),
      windows = c(0, 500)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors when AOI column cannot be detected", {
  x <- make_test_aoi_window_data() |>
    dplyr::select(-aoi_current)

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(0, 500)
    ),
    "Could not detect an AOI column. Please provide `aoi_col`.",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors for invalid numeric windows", {
  x <- make_test_aoi_window_data()

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0)),
    "`windows` must contain at least two finite numeric breakpoints.",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, NA)),
    "`windows` must contain at least two finite numeric breakpoints.",
    fixed = TRUE
  )

  expect_error(
    summarise_gazepoint_aoi_windows(x, windows = c(0, 0)),
    "`windows` must contain at least two distinct breakpoints.",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors for invalid data-frame windows", {
  x <- make_test_aoi_window_data()

  bad_windows_missing <- tibble::tibble(
    label = "early",
    start = 0
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = bad_windows_missing,
      window_label_col = "label",
      window_start_col = "start",
      window_end_col = "end"
    ),
    "Missing required window columns: end",
    fixed = TRUE
  )

  bad_windows_direction <- tibble::tibble(
    label = "bad",
    start = 500,
    end = 0
  )

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = bad_windows_direction,
      window_label_col = "label",
      window_start_col = "start",
      window_end_col = "end"
    ),
    "Each AOI window must have `window_end_ms` greater than `window_start_ms`.",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors when no finite time values remain", {
  x <- make_test_aoi_window_data() |>
    dplyr::mutate(time = NA_real_)

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(0, 500)
    ),
    "No rows contain finite time values.",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows errors when no rows fall inside windows", {
  x <- make_test_aoi_window_data()

  expect_error(
    summarise_gazepoint_aoi_windows(
      x,
      windows = c(2000, 3000)
    ),
    "No rows fall inside the supplied AOI windows.",
    fixed = TRUE
  )
})

test_that("summarise_gazepoint_aoi_windows works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    if (all(c("time", "aoi_current", "subject") %in% names(real_data))) {
      out <- summarise_gazepoint_aoi_windows(
        real_data,
        windows = c(0, 500, 1000, 2000),
        time_col = "time",
        aoi_col = "aoi_current",
        subject_col = "subject",
        condition_col = if ("condition" %in% names(real_data)) {
          "condition"
        } else {
          NULL
        },
        group_cols = intersect(
          c("subject", "MEDIA_ID", "trial_global"),
          names(real_data)
        ),
        target_aoi_values = "AOI 2",
        distractor_aoi_values = c("AOI 0", "AOI 1")
      )

      expect_s3_class(out, "gp3_aoi_window_summary")
      expect_true(all(c(
        "window_label",
        "n_window_samples",
        "n_target_samples",
        "n_valid_denominator_samples",
        "target_sample_prop_valid",
        "aoi_window_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
