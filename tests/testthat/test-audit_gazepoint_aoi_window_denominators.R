make_test_aoi_window_denominator_data <- function() {
  base <- expand.grid(
    subject = paste0("S", 1:4),
    condition = c("A", "B"),
    window_label = c("early", "late"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  base <- tibble::as_tibble(base) |>
    dplyr::arrange(subject, condition, window_label) |>
    dplyr::mutate(
      window_start_ms = dplyr::if_else(window_label == "early", 0, 500),
      window_end_ms = dplyr::if_else(window_label == "early", 500, 1000),
      n_window_samples = dplyr::if_else(window_label == "early", 30L, 60L),
      n_valid_denominator_samples = dplyr::if_else(
        window_label == "early",
        30L,
        58L
      ),
      n_target_samples = dplyr::case_when(
        condition == "A" & window_label == "early" ~ 10L,
        condition == "B" & window_label == "early" ~ 15L,
        condition == "A" & window_label == "late" ~ 20L,
        TRUE ~ 25L
      )
    )

  base
}

test_that("audit_gazepoint_aoi_window_denominators audits valid data", {
  x <- make_test_aoi_window_denominator_data()

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    min_denominator_samples = 5,
    min_valid_denominator_prop = 0.70,
    max_denominator_cv = 0.25,
    max_condition_ratio = 2
  )

  expect_s3_class(out, "gp3_aoi_window_denominator_audit")
  expect_type(out, "list")

  expect_true(all(c(
    "overview",
    "row_audit",
    "window_summary",
    "condition_window_summary",
    "denominator_imbalance",
    "flagged_rows",
    "settings"
  ) %in% names(out)))

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$row_audit, "tbl_df")
  expect_s3_class(out$window_summary, "tbl_df")
  expect_s3_class(out$condition_window_summary, "tbl_df")
  expect_s3_class(out$denominator_imbalance, "tbl_df")
  expect_s3_class(out$flagged_rows, "tbl_df")

  expect_equal(out$overview$n_rows, nrow(x))
  expect_equal(out$overview$n_windows, 2L)
  expect_equal(out$overview$n_conditions, 2L)
  expect_equal(out$overview$denominator_audit_status, "ok")

  expect_true(all(out$row_audit$denominator_audit_status == "ok"))
  expect_equal(nrow(out$flagged_rows), 0L)
})

test_that("audit_gazepoint_aoi_window_denominators returns expected summary columns", {
  x <- make_test_aoi_window_denominator_data()

  out <- audit_gazepoint_aoi_window_denominators(x)

  expect_true(all(c(
    "n_rows",
    "n_windows",
    "n_conditions",
    "n_missing_denominator",
    "n_zero_denominator",
    "n_low_denominator",
    "n_low_valid_denominator_prop",
    "n_target_exceeds_denominator",
    "n_target_zero",
    "n_target_all",
    "denominator_min",
    "denominator_median",
    "denominator_max",
    "valid_denominator_prop_min",
    "valid_denominator_prop_median",
    "valid_denominator_prop_max",
    "denominator_audit_status"
  ) %in% names(out$overview)))

  expect_true(all(c(
    "window_label",
    "window_start_ms",
    "window_end_ms",
    "denominator_min",
    "denominator_mean",
    "denominator_median",
    "denominator_max",
    "denominator_sd",
    "denominator_cv",
    "window_denominator_status"
  ) %in% names(out$window_summary)))

  expect_true(all(c(
    "condition",
    "window_label",
    "window_start_ms",
    "window_end_ms",
    "denominator_mean",
    "valid_denominator_prop_mean"
  ) %in% names(out$condition_window_summary)))

  expect_true(all(c(
    "window_label",
    "denominator_condition_ratio",
    "denominator_condition_cv",
    "denominator_imbalance_status"
  ) %in% names(out$denominator_imbalance)))
})

test_that("audit_gazepoint_aoi_window_denominators flags row-level denominator problems", {
  x <- make_test_aoi_window_denominator_data()

  x$n_valid_denominator_samples[1] <- NA_real_

  x$n_valid_denominator_samples[2] <- -1
  x$n_target_samples[2] <- 0

  x$n_target_samples[3] <- -1

  x$n_target_samples[4] <- x$n_valid_denominator_samples[4] + 1

  x$n_valid_denominator_samples[5] <- 0
  x$n_target_samples[5] <- 0

  x$n_valid_denominator_samples[6] <- 3
  x$n_target_samples[6] <- 1

  x$n_valid_denominator_samples[7] <- 20
  x$n_window_samples[7] <- 100
  x$n_target_samples[7] <- 5

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    min_denominator_samples = 5,
    min_valid_denominator_prop = 0.70
  )

  expect_true("missing_denominator" %in% out$row_audit$denominator_audit_status)
  expect_true("negative_denominator" %in% out$row_audit$denominator_audit_status)
  expect_true("negative_target" %in% out$row_audit$denominator_audit_status)
  expect_true("target_exceeds_denominator" %in% out$row_audit$denominator_audit_status)
  expect_true("zero_denominator" %in% out$row_audit$denominator_audit_status)
  expect_true("low_denominator" %in% out$row_audit$denominator_audit_status)
  expect_true("low_valid_denominator_prop" %in% out$row_audit$denominator_audit_status)

  expect_gt(nrow(out$flagged_rows), 0L)
})

test_that("audit_gazepoint_aoi_window_denominators flags missing total and target values", {
  x <- make_test_aoi_window_denominator_data()

  x$n_window_samples[1] <- NA_real_
  x$n_target_samples[2] <- NA_real_

  out <- audit_gazepoint_aoi_window_denominators(x)

  expect_true("missing_total" %in% out$row_audit$denominator_audit_status)
  expect_true("missing_target" %in% out$row_audit$denominator_audit_status)
})

test_that("audit_gazepoint_aoi_window_denominators flags non-positive total samples", {
  x <- make_test_aoi_window_denominator_data()

  x$n_window_samples[1] <- 0

  out <- audit_gazepoint_aoi_window_denominators(x)

  expect_true("non_positive_total" %in% out$row_audit$denominator_audit_status)
})

test_that("audit_gazepoint_aoi_window_denominators identifies zero and all-target outcomes", {
  x <- make_test_aoi_window_denominator_data()

  x$n_target_samples[1] <- 0
  x$n_target_samples[2] <- x$n_valid_denominator_samples[2]

  out <- audit_gazepoint_aoi_window_denominators(x)

  expect_true(out$row_audit$target_zero[1])
  expect_true(out$row_audit$target_all[2])
  expect_gt(out$overview$n_target_zero, 0L)
  expect_gt(out$overview$n_target_all, 0L)
})

test_that("audit_gazepoint_aoi_window_denominators flags high window denominator variability", {
  x <- make_test_aoi_window_denominator_data()

  x$n_valid_denominator_samples[x$window_label == "early"] <- c(
    5, 50, 5, 50, 5, 50, 5, 50
  )

  x$n_window_samples[x$window_label == "early"] <- 50
  x$n_target_samples[x$window_label == "early"] <- 2

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    min_denominator_samples = 5,
    min_valid_denominator_prop = 0,
    max_denominator_cv = 0.10
  )

  early <- out$window_summary |>
    dplyr::filter(window_label == "early")

  expect_equal(
    early$window_denominator_status,
    "high_denominator_variability"
  )
})

test_that("audit_gazepoint_aoi_window_denominators flags condition denominator imbalance", {
  x <- make_test_aoi_window_denominator_data() |>
    dplyr::mutate(
      n_valid_denominator_samples = dplyr::if_else(
        condition == "A",
        10,
        40
      ),
      n_window_samples = 40,
      n_target_samples = 5
    )

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    max_condition_ratio = 2
  )

  expect_true(
    "condition_denominator_ratio_high" %in%
      out$denominator_imbalance$denominator_imbalance_status
  )
})

test_that("audit_gazepoint_aoi_window_denominators handles single-condition data", {
  x <- make_test_aoi_window_denominator_data() |>
    dplyr::mutate(condition = "all_data")

  out <- audit_gazepoint_aoi_window_denominators(x)

  expect_true(all(
    out$denominator_imbalance$denominator_imbalance_status ==
      "single_condition"
  ))
})

test_that("audit_gazepoint_aoi_window_denominators handles condition_col = NULL", {
  x <- make_test_aoi_window_denominator_data()

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    condition_col = NULL
  )

  expect_equal(out$overview$n_conditions, 1L)
  expect_true(all(out$condition_window_summary$condition == "all_data"))
})

test_that("audit_gazepoint_aoi_window_denominators stores settings", {
  x <- make_test_aoi_window_denominator_data()

  out <- audit_gazepoint_aoi_window_denominators(
    x,
    window_col = "window_label",
    denominator_col = "n_valid_denominator_samples",
    total_col = "n_window_samples",
    target_col = "n_target_samples",
    condition_col = "condition",
    group_cols = c("subject", "condition"),
    min_denominator_samples = 6,
    min_valid_denominator_prop = 0.80,
    max_denominator_cv = 0.30,
    max_condition_ratio = 3
  )

  expect_equal(out$settings$window_col, "window_label")
  expect_equal(out$settings$denominator_col, "n_valid_denominator_samples")
  expect_equal(out$settings$total_col, "n_window_samples")
  expect_equal(out$settings$target_col, "n_target_samples")
  expect_equal(out$settings$condition_col, "condition")
  expect_equal(out$settings$group_cols, c("subject", "condition"))
  expect_equal(out$settings$min_denominator_samples, 6)
  expect_equal(out$settings$min_valid_denominator_prop, 0.80)
  expect_equal(out$settings$max_denominator_cv, 0.30)
  expect_equal(out$settings$max_condition_ratio, 3)
})

test_that("audit_gazepoint_aoi_window_denominators errors for invalid inputs", {
  x <- make_test_aoi_window_denominator_data()

  expect_error(
    audit_gazepoint_aoi_window_denominators("not data"),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, window_col = NA_character_),
    "`window_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, window_start_col = NA_character_),
    "`window_start_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, window_end_col = NA_character_),
    "`window_end_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, denominator_col = NA_character_),
    "`denominator_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, total_col = NA_character_),
    "`total_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, target_col = NA_character_),
    "`target_col` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, condition_col = NA_character_),
    "`condition_col` must be NULL or a non-missing character scalar",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_aoi_window_denominators errors for invalid threshold inputs", {
  x <- make_test_aoi_window_denominator_data()

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, min_denominator_samples = 0),
    "`min_denominator_samples` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, min_valid_denominator_prop = -0.1),
    "`min_valid_denominator_prop` must be a finite numeric scalar in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, min_valid_denominator_prop = 1.1),
    "`min_valid_denominator_prop` must be a finite numeric scalar in [0, 1]",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, max_denominator_cv = 0),
    "`max_denominator_cv` must be a positive finite numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(x, max_condition_ratio = 0),
    "`max_condition_ratio` must be a positive finite numeric scalar",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_aoi_window_denominators errors when required columns are missing", {
  x <- make_test_aoi_window_denominator_data()

  expect_error(
    audit_gazepoint_aoi_window_denominators(
      dplyr::select(x, -window_label)
    ),
    "Missing required columns: window_label",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(
      dplyr::select(x, -n_valid_denominator_samples)
    ),
    "Missing required columns: n_valid_denominator_samples",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(
      dplyr::select(x, -n_window_samples)
    ),
    "Missing required columns: n_window_samples",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(
      dplyr::select(x, -n_target_samples)
    ),
    "Missing required columns: n_target_samples",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_window_denominators(
      x,
      group_cols = "missing_group"
    ),
    "Missing required columns: missing_group",
    fixed = TRUE
  )
})

test_that("audit_gazepoint_aoi_window_denominators works with real aoi_windows object when available", {
  if (exists("aoi_windows", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("aoi_windows", envir = .GlobalEnv, inherits = TRUE)

    if (all(c(
      "window_label",
      "n_valid_denominator_samples",
      "n_window_samples",
      "n_target_samples"
    ) %in% names(real_data))) {
      out <- audit_gazepoint_aoi_window_denominators(real_data)

      expect_s3_class(out, "gp3_aoi_window_denominator_audit")
      expect_s3_class(out$overview, "tbl_df")
      expect_s3_class(out$row_audit, "tbl_df")
      expect_s3_class(out$window_summary, "tbl_df")
      expect_s3_class(out$denominator_imbalance, "tbl_df")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
