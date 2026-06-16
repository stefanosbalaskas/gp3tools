make_test_aoi_glmm_window_data <- function() {
  base <- expand.grid(
    subject = paste0("S", 1:4),
    condition = c("A", "B"),
    window_label = c("early", "late"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, window_label) |>
    dplyr::mutate(
      MEDIA_ID = rep(c(1, 2), length.out = dplyr::n()),
      trial_global = paste0(subject, "_M", MEDIA_ID),
      window_start_ms = dplyr::if_else(window_label == "early", 0, 500),
      window_end_ms = dplyr::if_else(window_label == "early", 500, 1000),
      n_window_samples = dplyr::if_else(window_label == "early", 30L, 60L),
      n_valid_denominator_samples = dplyr::if_else(window_label == "early", 28L, 55L),
      n_aoi_samples = dplyr::if_else(window_label == "early", 10L, 20L),
      n_target_samples = dplyr::case_when(
        condition == "A" & window_label == "early" ~ 5L,
        condition == "B" & window_label == "early" ~ 8L,
        condition == "A" & window_label == "late" ~ 12L,
        TRUE ~ 15L
      ),
      custom_denominator = n_valid_denominator_samples + 1L
    )
}

test_that("prepare_gazepoint_aoi_glmm_data prepares valid-denominator data", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    success_col = "n_target_samples",
    denominator = "valid",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms"
  )

  expect_s3_class(out, "gp3_aoi_glmm_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(x))

  expect_true(all(c(
    "aoi_glmm_success",
    "aoi_glmm_failure",
    "aoi_glmm_denominator",
    "aoi_glmm_prop",
    "aoi_glmm_weight",
    "aoi_glmm_subject",
    "aoi_glmm_condition",
    "aoi_glmm_window",
    "aoi_glmm_window_start_ms",
    "aoi_glmm_window_end_ms",
    "aoi_glmm_outcome",
    "aoi_glmm_denominator_type",
    "aoi_glmm_success_col",
    "aoi_glmm_denominator_col",
    "aoi_glmm_success_zero",
    "aoi_glmm_success_all",
    "aoi_glmm_status"
  ) %in% names(out)))

  expect_true(all(out$aoi_glmm_status == "ok"))
  expect_equal(out$aoi_glmm_success, x$n_target_samples)
  expect_equal(out$aoi_glmm_denominator, x$n_valid_denominator_samples)
  expect_equal(out$aoi_glmm_failure, x$n_valid_denominator_samples - x$n_target_samples)
  expect_equal(out$aoi_glmm_prop, x$n_target_samples / x$n_valid_denominator_samples)

  expect_true(is.factor(out$aoi_glmm_subject))
  expect_true(is.factor(out$aoi_glmm_condition))
  expect_true(is.factor(out$aoi_glmm_window))
})

test_that("prepare_gazepoint_aoi_glmm_data supports all denominator", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    denominator = "all"
  )

  expect_equal(out$aoi_glmm_denominator, x$n_window_samples)
  expect_true(all(out$aoi_glmm_denominator_type == "all"))
  expect_true(all(out$aoi_glmm_denominator_col == "n_window_samples"))
})

test_that("prepare_gazepoint_aoi_glmm_data supports AOI-only denominator", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    denominator = "aoi"
  )

  expect_equal(out$aoi_glmm_denominator, x$n_aoi_samples)
  expect_true(all(out$aoi_glmm_denominator_type == "aoi"))
  expect_true(all(out$aoi_glmm_denominator_col == "n_aoi_samples"))
})

test_that("prepare_gazepoint_aoi_glmm_data supports custom denominator", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    denominator = "custom",
    denominator_col = "custom_denominator"
  )

  expect_equal(out$aoi_glmm_denominator, x$custom_denominator)
  expect_true(all(out$aoi_glmm_denominator_type == "custom"))
  expect_true(all(out$aoi_glmm_denominator_col == "custom_denominator"))
})

test_that("prepare_gazepoint_aoi_glmm_data stores settings", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    success_col = "n_target_samples",
    denominator = "valid",
    subject_col = "subject",
    condition_col = "condition",
    window_col = "window_label",
    window_start_col = "window_start_ms",
    window_end_col = "window_end_ms",
    group_cols = c("subject", "condition"),
    min_denominator_samples = 5,
    drop_invalid = TRUE,
    missing_condition_label = "all_data",
    outcome_label = "target"
  )

  settings <- attr(out, "settings")

  expect_equal(settings$success_col, "n_target_samples")
  expect_equal(settings$denominator, "valid")
  expect_equal(settings$denominator_col, "n_valid_denominator_samples")
  expect_equal(settings$subject_col, "subject")
  expect_equal(settings$condition_col, "condition")
  expect_equal(settings$window_col, "window_label")
  expect_equal(settings$window_start_col, "window_start_ms")
  expect_equal(settings$window_end_col, "window_end_ms")
  expect_equal(settings$group_cols, c("subject", "condition"))
  expect_equal(settings$min_denominator_samples, 5)
  expect_true(settings$drop_invalid)
  expect_equal(settings$missing_condition_label, "all_data")
  expect_equal(settings$outcome_label, "target")
})

test_that("prepare_gazepoint_aoi_glmm_data preserves chronological window order", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(x)

  expect_equal(
    levels(out$aoi_glmm_window),
    c("early", "late")
  )
})

test_that("prepare_gazepoint_aoi_glmm_data handles missing condition column", {
  x <- make_test_aoi_glmm_window_data() |>
    dplyr::select(-condition)

  out <- prepare_gazepoint_aoi_glmm_data(x)

  expect_true("aoi_glmm_condition" %in% names(out))
  expect_equal(levels(out$aoi_glmm_condition), "all_data")
})

test_that("prepare_gazepoint_aoi_glmm_data supports condition_col = NULL", {
  x <- make_test_aoi_glmm_window_data()

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    condition_col = NULL
  )

  expect_true("aoi_glmm_condition" %in% names(out))
  expect_equal(levels(out$aoi_glmm_condition), "all_data")
})

test_that("prepare_gazepoint_aoi_glmm_data handles missing window start and end columns", {
  x <- make_test_aoi_glmm_window_data() |>
    dplyr::select(-window_start_ms, -window_end_ms)

  out <- prepare_gazepoint_aoi_glmm_data(x)

  expect_true(all(is.na(out$aoi_glmm_window_start_ms)))
  expect_true(all(is.na(out$aoi_glmm_window_end_ms)))
  expect_s3_class(out, "gp3_aoi_glmm_data")
})

test_that("prepare_gazepoint_aoi_glmm_data creates success-zero and success-all flags", {
  x <- make_test_aoi_glmm_window_data()

  x$n_target_samples[1] <- 0
  x$n_target_samples[2] <- x$n_valid_denominator_samples[2]

  out <- prepare_gazepoint_aoi_glmm_data(x)

  expect_true(out$aoi_glmm_success_zero[1])
  expect_true(out$aoi_glmm_success_all[2])
})

test_that("prepare_gazepoint_aoi_glmm_data drops invalid rows by default", {
  x <- make_test_aoi_glmm_window_data()

  x$n_target_samples[1] <- NA_real_
  x$n_valid_denominator_samples[2] <- NA_real_
  x$n_target_samples[3] <- -1
  x$n_valid_denominator_samples[4] <- -1
  x$n_target_samples[5] <- x$n_valid_denominator_samples[5] + 1
  x$n_valid_denominator_samples[6] <- 0
  x$n_valid_denominator_samples[7] <- 0.5

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    min_denominator_samples = 1,
    drop_invalid = TRUE
  )

  expect_true(all(out$aoi_glmm_status == "ok"))
  expect_lt(nrow(out), nrow(x))
})

test_that("prepare_gazepoint_aoi_glmm_data keeps invalid rows when requested", {
  x <- make_test_aoi_glmm_window_data()

  x$n_target_samples[1] <- NA_real_

  x$n_valid_denominator_samples[2] <- NA_real_

  x$n_target_samples[3] <- -1

  x$n_valid_denominator_samples[4] <- -1
  x$n_target_samples[4] <- 0

  x$n_target_samples[5] <- x$n_valid_denominator_samples[5] + 1

  x$n_valid_denominator_samples[6] <- 0
  x$n_target_samples[6] <- 0

  x$n_valid_denominator_samples[7] <- 0.5
  x$n_target_samples[7] <- 0

  out <- prepare_gazepoint_aoi_glmm_data(
    x,
    min_denominator_samples = 1,
    drop_invalid = FALSE
  )

  expect_equal(nrow(out), nrow(x))
  expect_true("missing_success" %in% out$aoi_glmm_status)
  expect_true("missing_denominator" %in% out$aoi_glmm_status)
  expect_true("negative_success" %in% out$aoi_glmm_status)
  expect_true("negative_denominator" %in% out$aoi_glmm_status)
  expect_true("success_exceeds_denominator" %in% out$aoi_glmm_status)
  expect_true("zero_denominator" %in% out$aoi_glmm_status)
  expect_true("low_denominator" %in% out$aoi_glmm_status)
})

test_that("prepare_gazepoint_aoi_glmm_data errors for invalid inputs", {
  x <- make_test_aoi_glmm_window_data()

  expect_error(
    prepare_gazepoint_aoi_glmm_data("not data"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, success_col = NA_character_),
    "`success_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, denominator_col = NA_character_),
    "`denominator_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, subject_col = NA_character_),
    "`subject_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, condition_col = NA_character_),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, window_col = NA_character_),
    "`window_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, window_start_col = NA_character_),
    "`window_start_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, window_end_col = NA_character_),
    "`window_end_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, min_denominator_samples = 0),
    "`min_denominator_samples` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, drop_invalid = NA),
    "`drop_invalid` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(x, outcome_label = NA_character_),
    "`outcome_label` must be a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_glmm_data errors for custom denominator without column", {
  x <- make_test_aoi_glmm_window_data()

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      x,
      denominator = "custom"
    ),
    "`denominator_col` must be provided when `denominator = \"custom\"`.",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_glmm_data errors when required columns are missing", {
  x <- make_test_aoi_glmm_window_data()

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      dplyr::select(x, -n_target_samples)
    ),
    "Missing required columns: n_target_samples",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      dplyr::select(x, -n_valid_denominator_samples)
    ),
    "Missing required columns: n_valid_denominator_samples",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      dplyr::select(x, -subject)
    ),
    "Missing required columns: subject",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      dplyr::select(x, -window_label)
    ),
    "Missing required columns: window_label",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_aoi_glmm_data(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns: missing_group",
    fixed = TRUE
  )
})

test_that("prepare_gazepoint_aoi_glmm_data works with real aoi_windows object when available", {
  if (exists("aoi_windows", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("aoi_windows", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "n_target_samples",
      "n_valid_denominator_samples",
      "subject",
      "window_label"
    ) %in% names(real_data))) {
      out <- prepare_gazepoint_aoi_glmm_data(
        real_data,
        success_col = "n_target_samples",
        denominator = "valid",
        subject_col = "subject",
        condition_col = if ("condition" %in% names(real_data)) {
          "condition"
        } else {
          NULL
        },
        window_col = "window_label",
        window_start_col = if ("window_start_ms" %in% names(real_data)) {
          "window_start_ms"
        } else {
          NULL
        },
        window_end_col = if ("window_end_ms" %in% names(real_data)) {
          "window_end_ms"
        } else {
          NULL
        }
      )

      expect_s3_class(out, "gp3_aoi_glmm_data")
      expect_true(all(c(
        "aoi_glmm_success",
        "aoi_glmm_failure",
        "aoi_glmm_denominator",
        "aoi_glmm_prop",
        "aoi_glmm_status"
      ) %in% names(out)))
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
