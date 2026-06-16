make_test_gca_data <- function() {
  tibble::tibble(
    subject = rep(c("S1", "S2"), each = 6),
    condition = rep(c("A", "B"), each = 6),
    time_bin_center_ms = rep(seq(0, 500, by = 100), 2),
    mean_pupil = c(
      0.10, 0.15, 0.22, 0.24, 0.20, 0.16,
      0.05, 0.09, 0.15, 0.19, 0.17, 0.12
    ),
    n_samples = rep(5L, 12),
    n_valid_samples = rep(4L, 12),
    pupil_col = "old_input_metadata",
    time_col = "old_input_metadata"
  )
}

test_that("prepare_gazepoint_gca_data creates orthogonal polynomial terms", {
  x <- make_test_gca_data()

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 3
  )

  expect_s3_class(out, "gp3_gca_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 12L)

  expect_true(all(c(
    "subject",
    "condition",
    "gca_time",
    "gca_time_centered",
    "gca_time_z",
    "time_poly_1",
    "time_poly_2",
    "time_poly_3",
    "gca_pupil",
    "gca_weight",
    "n_samples",
    "n_valid_samples",
    "pupil_col",
    "time_col",
    "degree",
    "orthogonal",
    "condition_status",
    "gca_data_status"
  ) %in% names(out)))

  expect_equal(unique(out$degree), 3L)
  expect_true(all(out$orthogonal))
  expect_equal(unique(out$condition_status), "ok")
  expect_equal(unique(out$gca_data_status), "ok")

  expect_equal(unique(out$pupil_col), "mean_pupil")
  expect_equal(unique(out$time_col), "time_bin_center_ms")
  expect_equal(out$gca_weight, rep(4, 12))
})

test_that("prepare_gazepoint_gca_data stores settings and polynomial attributes", {
  x <- make_test_gca_data()

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 3,
    orthogonal = TRUE
  )

  settings <- attr(out, "settings")

  expect_equal(settings$pupil_col, "mean_pupil")
  expect_equal(settings$time_col, "time_bin_center_ms")
  expect_equal(settings$subject_col, "subject")
  expect_equal(settings$condition_col, "condition")
  expect_equal(settings$degree, 3L)
  expect_true(settings$orthogonal)
  expect_equal(settings$valid_samples_col, "n_valid_samples")
  expect_equal(settings$min_valid_samples, 1)

  expect_false(is.null(attr(out, "poly_coefs")))
  expect_true(is.finite(attr(out, "time_mean")))
  expect_true(is.finite(attr(out, "time_sd")))
})

test_that("prepare_gazepoint_gca_data supports raw polynomial terms", {
  x <- make_test_gca_data()

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 3,
    orthogonal = FALSE
  )

  expect_equal(unique(out$orthogonal), FALSE)
  expect_equal(out$time_poly_1, out$gca_time_z)
  expect_equal(out$time_poly_2, out$gca_time_z ^ 2)
  expect_equal(out$time_poly_3, out$gca_time_z ^ 3)
  expect_null(attr(out, "poly_coefs"))
})

test_that("prepare_gazepoint_gca_data handles missing condition column", {
  x <- make_test_gca_data() |>
    dplyr::select(-condition)

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2
  )

  expect_true("condition" %in% names(out))
  expect_equal(unique(out$condition), "all_data")
  expect_equal(unique(out$condition_status), "no_condition_column")
  expect_equal(unique(out$gca_data_status), "no_condition_column")
})

test_that("prepare_gazepoint_gca_data handles entirely missing condition values", {
  x <- make_test_gca_data() |>
    dplyr::mutate(condition = NA_character_)

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2
  )

  expect_equal(unique(out$condition), "all_data")
  expect_equal(unique(out$condition_status), "condition_missing_all_data")
  expect_equal(unique(out$gca_data_status), "condition_missing_all_data")
})

test_that("prepare_gazepoint_gca_data supports time-window filtering", {
  x <- make_test_gca_data()

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2,
    time_window = c(100, 400)
  )

  expect_true(all(out$gca_time >= 100))
  expect_true(all(out$gca_time <= 400))
  expect_equal(nrow(out), 8L)
})

test_that("prepare_gazepoint_gca_data filters by minimum valid samples", {
  x <- make_test_gca_data()
  x$n_valid_samples[1:2] <- 0L

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2,
    min_valid_samples = 1
  )

  expect_equal(nrow(out), 10L)
  expect_true(all(out$n_valid_samples >= 1))

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      degree = 2,
      min_valid_samples = 10
    ),
    "No rows remain after applying GCA data filters",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_gca_data uses explicit weights when provided", {
  x <- make_test_gca_data() |>
    dplyr::mutate(custom_weight = seq_len(dplyr::n()))

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2,
    weights_col = "custom_weight"
  )

  expect_equal(out$gca_weight, x$custom_weight)
  expect_equal(attr(out, "settings")$weights_col, "custom_weight")
})

test_that("prepare_gazepoint_gca_data supports custom columns", {
  x <- make_test_gca_data() |>
    dplyr::rename(
      participant = subject,
      group = condition,
      time_ms = time_bin_center_ms,
      pupil_value = mean_pupil,
      valid_n = n_valid_samples
    )

  out <- prepare_gazepoint_gca_data(
    x,
    pupil_col = "pupil_value",
    time_col = "time_ms",
    subject_col = "participant",
    condition_col = "group",
    valid_samples_col = "valid_n",
    degree = 2
  )

  expect_equal(unique(out$pupil_col), "pupil_value")
  expect_equal(unique(out$time_col), "time_ms")
  expect_equal(sort(unique(out$subject)), c("S1", "S2"))
  expect_equal(sort(unique(out$condition)), c("A", "B"))
  expect_equal(out$gca_weight, rep(4, 12))
})

test_that("prepare_gazepoint_gca_data drops missing outcome and time rows", {
  x <- make_test_gca_data()
  x$mean_pupil[1] <- NA_real_
  x$time_bin_center_ms[2] <- NA_real_

  out <- prepare_gazepoint_gca_data(
    x,
    degree = 2
  )

  expect_equal(nrow(out), 10L)
  expect_false(any(is.na(out$gca_pupil)))
  expect_false(any(is.na(out$gca_time)))
})

test_that("prepare_gazepoint_gca_data errors for invalid inputs", {
  x <- make_test_gca_data()

  expect_error(
    prepare_gazepoint_gca_data("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      pupil_col = NA_character_
    ),
    "`pupil_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      subject_col = NA_character_
    ),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      condition_col = NA_character_
    ),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      degree = 0
    ),
    "`degree` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      orthogonal = NA
    ),
    "`orthogonal` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      time_window = c(0, NA)
    ),
    "`time_window` must be NULL or a finite numeric vector of length 2",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      min_valid_samples = 0
    ),
    "`min_valid_samples` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      drop_missing = NA
    ),
    "`drop_missing` must be TRUE or FALSE",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_gca_data errors when required columns are missing", {
  x <- make_test_gca_data()

  expect_error(
    prepare_gazepoint_gca_data(
      dplyr::select(x, -mean_pupil)
    ),
    "Missing required columns: mean_pupil",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      dplyr::select(x, -time_bin_center_ms)
    ),
    "Missing required columns: time_bin_center_ms",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      dplyr::select(x, -subject)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      weights_col = "missing_weight"
    ),
    "Missing required columns: missing_weight",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_gca_data errors when degree is too high for time values", {
  x <- make_test_gca_data() |>
    dplyr::filter(time_bin_center_ms %in% c(0, 100, 200))

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      degree = 3
    ),
    "The number of unique time values must be greater than `degree`",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_gca_data errors when time has no variation", {
  x <- make_test_gca_data() |>
    dplyr::mutate(time_bin_center_ms = 100)

  expect_error(
    prepare_gazepoint_gca_data(
      x,
      degree = 1
    ),
    "The number of unique time values must be greater than `degree`",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_gca_data works with real pupil_gca_data source when available", {
  if (exists("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("pupil_gamm_data", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "mean_pupil",
      "time_bin_center_ms",
      "subject"
    ) %in% names(real_data))) {
      out <- prepare_gazepoint_gca_data(
        real_data,
        pupil_col = "mean_pupil",
        time_col = "time_bin_center_ms",
        subject_col = "subject",
        condition_col = if ("condition" %in% names(real_data)) {
          "condition"
        } else {
          NULL
        },
        degree = 3,
        valid_samples_col = if ("n_valid_samples" %in% names(real_data)) {
          "n_valid_samples"
        } else {
          NULL
        },
        min_valid_samples = 1
      )

      expect_s3_class(out, "gp3_gca_data")
      expect_true(all(c(
        "subject",
        "condition",
        "gca_time",
        "time_poly_1",
        "time_poly_2",
        "time_poly_3",
        "gca_pupil",
        "gca_data_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
