test_that("transform_gazepoint_aoi_empirical_logit transforms numerator and denominator counts", {
  toy_data <- tibble::tibble(
    subject = paste0("S", 1:5),
    aoi_hits = c(0, 2, 5, 8, 10),
    valid_samples = c(10, 10, 10, 10, 10)
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    numerator_col = "aoi_hits",
    denominator_col = "valid_samples",
    correction = 0.5,
    name = "toy_empirical_logit"
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")
  expect_s3_class(out, "tbl_df")

  expect_true(all(c(
    "aoi_proportion_raw",
    "aoi_numerator",
    "aoi_denominator",
    "aoi_proportion_adjusted",
    "aoi_empirical_logit",
    "aoi_empirical_logit_status"
  ) %in% names(out)))

  expect_equal(out$aoi_proportion_raw, c(0, 0.2, 0.5, 0.8, 1))
  expect_equal(out$aoi_numerator, toy_data$aoi_hits)
  expect_equal(out$aoi_denominator, toy_data$valid_samples)

  expected_adjusted <- (toy_data$aoi_hits + 0.5) / (toy_data$valid_samples + 1)
  expected_logit <- log((toy_data$aoi_hits + 0.5) / (toy_data$valid_samples - toy_data$aoi_hits + 0.5))

  expect_equal(out$aoi_proportion_adjusted, expected_adjusted)
  expect_equal(out$aoi_empirical_logit, expected_logit)
  expect_equal(out$aoi_empirical_logit[3], 0)
  expect_true(is.finite(out$aoi_empirical_logit[1]))
  expect_true(is.finite(out$aoi_empirical_logit[5]))
  expect_true(all(out$aoi_empirical_logit_status == "complete"))

  overview <- attr(out, "gp3_empirical_logit_overview")
  status_summary <- attr(out, "gp3_empirical_logit_status_summary")
  settings <- attr(out, "gp3_empirical_logit_settings")

  expect_s3_class(overview, "tbl_df")
  expect_s3_class(status_summary, "tbl_df")
  expect_s3_class(settings, "tbl_df")

  expect_equal(overview$object_name, "toy_empirical_logit")
  expect_equal(overview$transformation, "aoi_empirical_logit")
  expect_equal(overview$numerator_col, "aoi_hits")
  expect_equal(overview$denominator_col, "valid_samples")
  expect_true(is.na(overview$proportion_col))
  expect_equal(overview$denominator_source, "observed_denominator")
  expect_equal(overview$correction, 0.5)
  expect_equal(overview$n_input_rows, 5)
  expect_equal(overview$n_complete, 5)
  expect_equal(overview$n_problem_rows, 0)

  expect_equal(status_summary$status, "complete")
  expect_equal(status_summary$n, 5)

  expect_equal(
    settings$value[settings$setting == "name"],
    "toy_empirical_logit"
  )
})

test_that("transform_gazepoint_aoi_empirical_logit supports proportion-only input", {
  toy_data <- tibble::tibble(
    subject = paste0("S", 1:3),
    aoi_prop = c(0, 0.5, 1)
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    proportion_col = "aoi_prop",
    pseudo_denominator = 1,
    correction = 0.5,
    name = "proportion_only"
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")

  expect_equal(out$aoi_proportion_raw, c(0, 0.5, 1))
  expect_equal(out$aoi_denominator, c(1, 1, 1))
  expect_equal(out$aoi_numerator, c(0, 0.5, 1))

  expected_logit <- log((out$aoi_numerator + 0.5) / (out$aoi_denominator - out$aoi_numerator + 0.5))

  expect_equal(out$aoi_empirical_logit, expected_logit)
  expect_equal(out$aoi_empirical_logit[2], 0)
  expect_true(all(is.finite(out$aoi_empirical_logit)))
  expect_true(all(out$aoi_empirical_logit_status == "complete"))

  overview <- attr(out, "gp3_empirical_logit_overview")

  expect_equal(overview$denominator_source, "pseudo_denominator_from_proportion")
  expect_equal(overview$pseudo_denominator, 1)
  expect_equal(overview$n_complete, 3)
})

test_that("transform_gazepoint_aoi_empirical_logit supports proportion with observed denominator", {
  toy_data <- tibble::tibble(
    subject = paste0("S", 1:4),
    aoi_prop = c(0, 0.25, 0.75, 1),
    valid_samples = c(20, 20, 20, 20)
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    proportion_col = "aoi_prop",
    denominator_col = "valid_samples",
    correction = 0.5,
    name = "proportion_with_denominator"
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")

  expect_equal(out$aoi_numerator, c(0, 5, 15, 20))
  expect_equal(out$aoi_denominator, c(20, 20, 20, 20))
  expect_equal(out$aoi_proportion_raw, toy_data$aoi_prop)
  expect_true(all(out$aoi_empirical_logit_status == "complete"))

  overview <- attr(out, "gp3_empirical_logit_overview")

  expect_equal(overview$denominator_source, "observed_denominator_from_proportion")
  expect_equal(overview$n_complete, 4)
})

test_that("transform_gazepoint_aoi_empirical_logit records invalid rows", {
  toy_data <- tibble::tibble(
    row_id = 1:7,
    numerator = c(5, NA, 2, 11, -1, 1, 2),
    denominator = c(10, 10, NA, 10, 10, 0, 10)
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    numerator_col = "numerator",
    denominator_col = "denominator",
    correction = 0.5,
    name = "invalid_rows"
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")

  expect_equal(out$aoi_empirical_logit_status[1], "complete")
  expect_equal(out$aoi_empirical_logit_status[2], "missing_or_nonfinite_numerator")
  expect_equal(out$aoi_empirical_logit_status[3], "missing_or_nonfinite_denominator")
  expect_equal(out$aoi_empirical_logit_status[4], "proportion_out_of_bounds")
  expect_equal(out$aoi_empirical_logit_status[5], "proportion_out_of_bounds")
  expect_equal(out$aoi_empirical_logit_status[6], "numerator_exceeds_denominator")
  expect_equal(out$aoi_empirical_logit_status[7], "complete")

  expect_true(is.finite(out$aoi_empirical_logit[1]))
  expect_true(is.na(out$aoi_empirical_logit[2]))
  expect_true(is.na(out$aoi_empirical_logit[3]))
  expect_true(is.na(out$aoi_empirical_logit[4]))
  expect_true(is.na(out$aoi_empirical_logit[5]))
  expect_true(is.na(out$aoi_empirical_logit[6]))
  expect_true(is.finite(out$aoi_empirical_logit[7]))

  overview <- attr(out, "gp3_empirical_logit_overview")
  status_summary <- attr(out, "gp3_empirical_logit_status_summary")

  expect_equal(overview$n_complete, 2)
  expect_equal(overview$n_problem_rows, 5)
  expect_true("complete" %in% status_summary$status)
  expect_true("missing_or_nonfinite_numerator" %in% status_summary$status)
  expect_true("missing_or_nonfinite_denominator" %in% status_summary$status)
  expect_true("proportion_out_of_bounds" %in% status_summary$status)
})

test_that("transform_gazepoint_aoi_empirical_logit supports custom output columns", {
  toy_data <- tibble::tibble(
    hits = c(1, 2, 3),
    total = c(4, 4, 4)
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    numerator_col = "hits",
    denominator_col = "total",
    output_col = "elogit",
    adjusted_proportion_col = "prop_adj",
    raw_proportion_col = "prop_raw",
    numerator_output_col = "num_used",
    denominator_output_col = "den_used",
    status_col = "elogit_status",
    name = "custom_columns"
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")

  expect_true(all(c(
    "elogit",
    "prop_adj",
    "prop_raw",
    "num_used",
    "den_used",
    "elogit_status"
  ) %in% names(out)))

  expect_false("aoi_empirical_logit" %in% names(out))
  expect_true(all(out$elogit_status == "complete"))

  settings <- attr(out, "gp3_empirical_logit_settings")

  expect_equal(settings$value[settings$setting == "output_col"], "elogit")
  expect_equal(settings$value[settings$setting == "status_col"], "elogit_status")
})

test_that("transform_gazepoint_aoi_empirical_logit supports overwrite", {
  toy_data <- tibble::tibble(
    hits = c(1, 2, 3),
    total = c(4, 4, 4),
    aoi_empirical_logit = 999
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total"
    ),
    "Output column(s) already exist in `data`",
    fixed = TRUE
  )

  out <- transform_gazepoint_aoi_empirical_logit(
    toy_data,
    numerator_col = "hits",
    denominator_col = "total",
    overwrite = TRUE
  )

  expect_s3_class(out, "gp3_aoi_empirical_logit_data")
  expect_false(any(out$aoi_empirical_logit == 999))
  expect_true(all(out$aoi_empirical_logit_status == "complete"))
})

test_that("transform_gazepoint_aoi_empirical_logit checks invalid inputs", {
  toy_data <- tibble::tibble(
    hits = c(1, 2, 3),
    total = c(4, 4, 4),
    prop = c(0.25, 0.5, 0.75)
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      list(),
      numerator_col = "hits",
      denominator_col = "total"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data[0, ],
      numerator_col = "hits",
      denominator_col = "total"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(toy_data),
    "Supply either `proportion_col` or both `numerator_col` and `denominator_col`",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "bad_hits",
      denominator_col = "total"
    ),
    "`numerator_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "bad_total"
    ),
    "`denominator_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      proportion_col = "bad_prop"
    ),
    "`proportion_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total",
      correction = 0
    ),
    "`correction` must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      proportion_col = "prop",
      pseudo_denominator = 0
    ),
    "`pseudo_denominator` must be a positive finite number",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total",
      output_col = ""
    ),
    "Each output column must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total",
      output_col = "duplicate",
      adjusted_proportion_col = "duplicate"
    ),
    "Output column names must be unique",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total",
      overwrite = NA
    ),
    "`overwrite` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    transform_gazepoint_aoi_empirical_logit(
      toy_data,
      numerator_col = "hits",
      denominator_col = "total",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
