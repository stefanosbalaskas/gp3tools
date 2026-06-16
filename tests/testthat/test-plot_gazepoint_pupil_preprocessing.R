make_test_pupil_preprocessing_data <- function() {
  tibble::tibble(
    subject = c(rep("S1", 10), rep("S2", 10)),
    MEDIA_ID = c(rep("M1", 10), rep("M2", 10)),
    trial = c(rep("T1", 10), rep("T2", 10)),
    trial_global = c(rep("S1_T1", 10), rep("S2_T2", 10)),
    condition = c(rep("A", 10), rep("B", 10)),
    time = rep(seq(0, 900, by = 100), 2),
    pupil = c(
      3.0, 3.1, NA, 3.3, 3.4, 8.5, 3.5, 3.6, NA, 3.8,
      5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9
    ),
    pupil_clean = c(
      3.0, 3.1, NA, 3.3, 3.4, NA, 3.5, 3.6, NA, 3.8,
      5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9
    ),
    pupil_interpolated = c(
      3.0, 3.1, 3.2, 3.3, 3.4, NA, 3.5, 3.6, 3.7, 3.8,
      5.0, 5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9
    ),
    pupil_baseline_corrected = c(
      0.0, 0.1, 0.2, 0.3, 0.4, NA, 0.5, 0.6, 0.7, 0.8,
      rep(NA_real_, 10)
    ),
    pupil_smoothed = c(
      0.05, 0.1, 0.2, 0.3, 0.35, NA, 0.55, 0.6, 0.7, 0.75,
      rep(NA_real_, 10)
    ),
    pupil_artifact_flag = c(
      FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE
    ),
    pupil_interpolation_status = c(
      "observed", "observed", "interpolated", "observed", "observed",
      "missing_long_gap", "observed", "observed", "interpolated", "observed",
      "observed", "observed", "observed", "observed", "observed",
      "observed", "observed", "observed", "observed", "observed"
    )
  )
}

test_that("plot_gazepoint_pupil_preprocessing returns ggplot object", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S1",
    trial_global = "S1_T1"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_preprocessing supports overlaid plots", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S1",
    trial_global = "S1_T1",
    plot_style = "overlaid"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_preprocessing filters selected trial correctly", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S2",
    trial_global = "S2_T2",
    plot_style = "faceted"
  )

  expect_true(all(p$data$.gp3_pre_pupil >= 5))
  expect_false("smoothed" %in% unique(p$data$pupil_series))
  expect_false("baseline_corrected" %in% unique(p$data$pupil_series))
})

test_that("plot_gazepoint_pupil_preprocessing keeps smoothed series when available", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S1",
    trial_global = "S1_T1",
    plot_style = "faceted"
  )

  expect_true("smoothed" %in% unique(p$data$pupil_series))
  expect_true("baseline_corrected" %in% unique(p$data$pupil_series))
})

test_that("plot_gazepoint_pupil_preprocessing supports media, trial, and condition filters", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    media_id = "M1",
    trial = "T1",
    condition = "A"
  )

  expect_s3_class(p, "ggplot")
  expect_true("raw" %in% unique(p$data$pupil_series))
})

test_that("plot_gazepoint_pupil_preprocessing supports artifact reason column", {
  x <- make_test_pupil_preprocessing_data() |>
    dplyr::select(-pupil_artifact_flag) |>
    dplyr::mutate(
      pupil_artifact_reason = dplyr::if_else(
        trial_global == "S1_T1" & time == 500,
        "blink",
        "valid"
      )
    )

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S1",
    trial_global = "S1_T1"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_preprocessing supports binning and event thinning", {
  x <- make_test_pupil_preprocessing_data()

  p <- plot_gazepoint_pupil_preprocessing(
    x,
    subject = "S1",
    trial_global = "S1_T1",
    bin_width_ms = 200,
    max_event_marks = 2
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_pupil_preprocessing errors for invalid inputs", {
  x <- make_test_pupil_preprocessing_data()

  expect_error(
    plot_gazepoint_pupil_preprocessing("not a data frame"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      time_col = NA_character_
    ),
    "`time_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      bin_width_ms = 0
    ),
    "`bin_width_ms` must be greater than 0",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      max_event_marks = 0
    ),
    "`max_event_marks` must be at least 1",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      alpha = 2
    ),
    "`alpha` must be between 0 and 1",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_pupil_preprocessing errors when filters remove all rows", {
  x <- make_test_pupil_preprocessing_data()

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      subject = "missing_subject"
    ),
    "No rows remain after applying the requested filters"
  )
})

test_that("plot_gazepoint_pupil_preprocessing errors when required columns are missing", {
  x <- make_test_pupil_preprocessing_data()

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      dplyr::select(x, -time)
    ),
    "Missing required columns"
  )

  expect_error(
    plot_gazepoint_pupil_preprocessing(
      x,
      subject = "S1",
      subject_col = "missing_subject_col"
    ),
    "Missing required columns"
  )
})

test_that("plot_gazepoint_pupil_preprocessing errors when no pupil columns are found", {
  x <- make_test_pupil_preprocessing_data() |>
    dplyr::select(
      subject,
      MEDIA_ID,
      trial,
      trial_global,
      condition,
      time
    )

  expect_error(
    plot_gazepoint_pupil_preprocessing(x),
    "No requested pupil columns were found in `data`"
  )
})

test_that("plot_gazepoint_pupil_preprocessing works with real pipeline object when available", {
  if (exists("smoothed_artifact_pupil", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get(
      "smoothed_artifact_pupil",
      envir = .GlobalEnv,
      inherits = TRUE
    )

    required_cols <- c(
      "subject",
      "trial_global",
      "time"
    )

    if (all(required_cols %in% names(real_data))) {
      first_trial <- real_data |>
        dplyr::filter(!is.na(.data$subject), !is.na(.data$trial_global)) |>
        dplyr::slice(1)

      if (nrow(first_trial) == 1L) {
        p <- plot_gazepoint_pupil_preprocessing(
          real_data,
          subject = first_trial$subject[[1]],
          trial_global = first_trial$trial_global[[1]]
        )

        expect_s3_class(p, "ggplot")
      } else {
        expect_true(TRUE)
      }
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
