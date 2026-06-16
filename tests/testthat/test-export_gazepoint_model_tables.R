test_that("export_gazepoint_model_tables exports model summary tables", {
  dat <- tibble::tibble(
    y = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0),
    x = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 1, 0)
  )

  mod <- stats::glm(y ~ x, data = dat, family = stats::binomial())

  model_summary <- tidy_gazepoint_model_summary(
    mod,
    model_name = "glm_export",
    use_dharma = FALSE
  )

  output_dir <- tempfile("gp3_model_tables_")

  written <- export_gazepoint_model_tables(
    model_summary = model_summary,
    output_dir = output_dir,
    prefix = "glm_export"
  )

  expect_s3_class(written, "gp3_model_table_export")
  expect_s3_class(written, "tbl_df")

  expect_true(all(file.exists(written$file)))
  expect_true("model_overview" %in% written$table_name)
  expect_true("model_info" %in% written$table_name)
  expect_true("fixed_effects" %in% written$table_name)
  expect_true("model_settings" %in% written$table_name)
  expect_true("diagnostics_overview" %in% written$table_name)
  expect_true("export_index" %in% written$table_name)

  fixed_file <- written$file[written$table_name == "fixed_effects"][[1]]
  fixed_read <- utils::read.csv(fixed_file)

  expect_true("term" %in% names(fixed_read))
  expect_true("estimate" %in% names(fixed_read))
  expect_true(nrow(fixed_read) >= 1)
})

test_that("export_gazepoint_model_tables can skip diagnostics", {
  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9),
    x = c(1, 2, 3, 4, 5, 6)
  )

  mod <- stats::lm(y ~ x, data = dat)

  model_summary <- tidy_gazepoint_model_summary(
    mod,
    model_name = "lm_export",
    include_diagnostics = FALSE
  )

  output_dir <- tempfile("gp3_model_tables_")

  written <- export_gazepoint_model_tables(
    model_summary = model_summary,
    output_dir = output_dir,
    prefix = "lm_export",
    include_diagnostics = FALSE
  )

  expect_s3_class(written, "gp3_model_table_export")
  expect_false(any(grepl("^diagnostics_", written$table_name)))
  expect_true("model_overview" %in% written$table_name)
  expect_true("fixed_effects" %in% written$table_name)
})

test_that("export_gazepoint_model_tables exports emmeans summary tables", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 1.3, 2.1, 3.1, 4.2, 5.2, 6.1),
    condition = factor(rep(c("A", "B"), each = 6))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  emmeans_summary <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    model_name = "emm_export"
  )

  output_dir <- tempfile("gp3_model_tables_")

  written <- export_gazepoint_model_tables(
    emmeans_summary = emmeans_summary,
    output_dir = output_dir,
    prefix = "emm_export"
  )

  expect_s3_class(written, "gp3_model_table_export")
  expect_true(all(file.exists(written$file)))
  expect_true("emmeans_overview" %in% written$table_name)
  expect_true("emmeans" %in% written$table_name)
  expect_true("contrasts" %in% written$table_name)
  expect_true("emmeans_settings" %in% written$table_name)

  emm_file <- written$file[written$table_name == "emmeans"][[1]]
  emm_read <- utils::read.csv(emm_file)

  expect_true("term" %in% names(emm_read))
  expect_true("estimate" %in% names(emm_read))
  expect_true(nrow(emm_read) >= 1)
})

test_that("export_gazepoint_model_tables exports combined model and emmeans summaries", {
  testthat::skip_if_not_installed("emmeans")

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 1.3, 2.1, 3.1, 4.2, 5.2, 6.1),
    condition = factor(rep(c("A", "B"), each = 6))
  )

  mod <- stats::lm(y ~ condition, data = dat)

  model_summary <- tidy_gazepoint_model_summary(
    mod,
    model_name = "combined_export",
    include_diagnostics = FALSE
  )

  emmeans_summary <- summarise_gazepoint_emmeans(
    mod,
    specs = "condition",
    model_name = "combined_export"
  )

  output_dir <- tempfile("gp3_model_tables_")

  written <- export_gazepoint_model_tables(
    model_summary = model_summary,
    emmeans_summary = emmeans_summary,
    output_dir = output_dir,
    prefix = "combined export"
  )

  expect_s3_class(written, "gp3_model_table_export")
  expect_true(all(file.exists(written$file)))
  expect_true(any(grepl("combined_export", basename(written$file))))
  expect_true("fixed_effects" %in% written$table_name)
  expect_true("emmeans" %in% written$table_name)
  expect_true("contrasts" %in% written$table_name)
  expect_true("export_index" %in% written$table_name)
})

test_that("export_gazepoint_model_tables respects overwrite = FALSE", {
  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9),
    x = c(1, 2, 3, 4, 5, 6)
  )

  mod <- stats::lm(y ~ x, data = dat)

  model_summary <- tidy_gazepoint_model_summary(
    mod,
    model_name = "overwrite_export",
    include_diagnostics = FALSE
  )

  output_dir <- tempfile("gp3_model_tables_")

  export_gazepoint_model_tables(
    model_summary = model_summary,
    output_dir = output_dir,
    prefix = "overwrite_export"
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = model_summary,
      output_dir = output_dir,
      prefix = "overwrite_export",
      overwrite = FALSE
    ),
    "Output file already exists and `overwrite = FALSE`",
    fixed = TRUE
  )
})

test_that("export_gazepoint_model_tables checks invalid inputs", {
  expect_error(
    export_gazepoint_model_tables(output_dir = tempdir()),
    "At least one of `model_summary` or `emmeans_summary` must be supplied",
    fixed = TRUE
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = list(),
      output_dir = tempdir()
    ),
    "`model_summary` must be an object returned by `tidy_gazepoint_model_summary()`",
    fixed = TRUE
  )

  expect_error(
    export_gazepoint_model_tables(
      emmeans_summary = list(),
      output_dir = tempdir()
    ),
    "`emmeans_summary` must be an object returned by `summarise_gazepoint_emmeans()`",
    fixed = TRUE
  )

  dat <- tibble::tibble(
    y = c(1.1, 1.9, 3.2, 3.8),
    x = c(1, 2, 3, 4)
  )

  mod <- stats::lm(y ~ x, data = dat)

  model_summary <- tidy_gazepoint_model_summary(
    mod,
    include_diagnostics = FALSE
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = model_summary,
      output_dir = NA_character_
    ),
    "`output_dir` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = model_summary,
      output_dir = tempdir(),
      prefix = NA_character_
    ),
    "`prefix` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = model_summary,
      output_dir = tempdir(),
      overwrite = NA
    ),
    "`overwrite` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    export_gazepoint_model_tables(
      model_summary = model_summary,
      output_dir = tempdir(),
      include_diagnostics = NA
    ),
    "`include_diagnostics` must be TRUE or FALSE",
    fixed = TRUE
  )
})
