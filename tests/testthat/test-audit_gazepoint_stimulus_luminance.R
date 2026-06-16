make_test_luminance_images <- function() {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- tempfile("gp3_luminance_test_")
  dir.create(tmp_dir)

  dark <- magick::image_blank(width = 20, height = 20, color = "#333333")
  light <- magick::image_blank(width = 20, height = 20, color = "#CCCCCC")

  mixed_top <- magick::image_blank(width = 20, height = 10, color = "#333333")
  mixed_bottom <- magick::image_blank(width = 20, height = 10, color = "#E6E6E6")
  mixed <- magick::image_append(c(mixed_top, mixed_bottom), stack = TRUE)

  magick::image_write(dark, file.path(tmp_dir, "dark.png"))
  magick::image_write(light, file.path(tmp_dir, "light.png"))
  magick::image_write(mixed, file.path(tmp_dir, "mixed.png"))

  tmp_dir
}

make_test_luminance_data <- function() {
  tibble::tibble(
    stimulus_id = c("stim_dark", "stim_light", "stim_mixed", "stim_missing"),
    stimulus_file = c("dark.png", "light.png", "mixed.png", "missing.png"),
    condition = c("A", "B", "B", "A")
  )
}

test_that("audit_gazepoint_stimulus_luminance creates a complete audit object", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  toy_data <- make_test_luminance_data()

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    stimulus_file_col = "stimulus_file",
    stimulus_id_col = "stimulus_id",
    condition_col = "condition",
    image_dir = tmp_dir,
    name = "toy_luminance"
  )

  expect_s3_class(out, "gp3_stimulus_luminance_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "stimulus_index",
      "stimulus_luminance",
      "condition_summary",
      "balance_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$stimulus_index, "tbl_df")
  expect_s3_class(out$stimulus_luminance, "tbl_df")
  expect_s3_class(out$condition_summary, "tbl_df")
  expect_s3_class(out$balance_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_luminance")
  expect_equal(out$overview$n_input_rows, 4)
  expect_equal(out$overview$n_stimulus_rows, 4)
  expect_equal(out$overview$n_unique_stimuli, 4)
  expect_equal(out$overview$n_unique_files, 4)
  expect_equal(out$overview$n_conditions, 2)
  expect_equal(out$overview$n_files_available, 3)
  expect_equal(out$overview$n_luminance_available, 3)
  expect_true(out$overview$magick_available)
  expect_equal(out$overview$audit_status, "partial_luminance_available")
})

test_that("audit_gazepoint_stimulus_luminance computes expected stimulus statuses", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  toy_data <- make_test_luminance_data()

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    image_dir = tmp_dir
  )

  stim <- out$stimulus_luminance

  expect_equal(nrow(stim), 4)
  expect_equal(sum(stim$file_exists), 3)
  expect_equal(sum(stim$luminance_available), 3)

  expect_equal(
    stim$luminance_status[stim$stimulus_id == "stim_missing"],
    "file_missing"
  )

  expect_true(all(stim$image_width_px[stim$luminance_available] == 20))
  expect_true(all(stim$image_height_px[stim$luminance_available] == 20))
  expect_true(all(stim$n_pixels[stim$luminance_available] == 400))

  dark_lum <- stim$mean_luminance[stim$stimulus_id == "stim_dark"]
  light_lum <- stim$mean_luminance[stim$stimulus_id == "stim_light"]
  mixed_lum <- stim$mean_luminance[stim$stimulus_id == "stim_mixed"]

  expect_true(dark_lum < mixed_lum)
  expect_true(mixed_lum < light_lum)

  mixed_rms <- stim$rms_contrast[stim$stimulus_id == "stim_mixed"]
  dark_rms <- stim$rms_contrast[stim$stimulus_id == "stim_dark"]

  expect_true(mixed_rms > dark_rms)
})

test_that("audit_gazepoint_stimulus_luminance creates condition and balance summaries", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  toy_data <- make_test_luminance_data()

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    stimulus_file_col = "stimulus_file",
    stimulus_id_col = "stimulus_id",
    condition_col = "condition",
    image_dir = tmp_dir
  )

  expect_equal(nrow(out$condition_summary), 2)
  expect_equal(sort(out$condition_summary$condition), c("A", "B"))

  condition_a <- out$condition_summary[out$condition_summary$condition == "A", ]
  condition_b <- out$condition_summary[out$condition_summary$condition == "B", ]

  expect_equal(condition_a$n_stimulus_rows, 2)
  expect_equal(condition_a$n_luminance_available, 1)
  expect_equal(condition_a$condition_luminance_status, "partial_luminance_available")

  expect_equal(condition_b$n_stimulus_rows, 2)
  expect_equal(condition_b$n_luminance_available, 2)
  expect_equal(condition_b$condition_luminance_status, "complete_luminance_available")

  expect_equal(out$balance_summary$n_conditions, 2)
  expect_equal(out$balance_summary$n_conditions_with_luminance, 2)
  expect_true(out$balance_summary$range_condition_mean_luminance > 0)
  expect_equal(
    out$balance_summary$luminance_balance_status,
    "condition_luminance_summarised"
  )
})

test_that("audit_gazepoint_stimulus_luminance auto-detects common columns", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  toy_data <- make_test_luminance_data()

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    image_dir = tmp_dir
  )

  expect_s3_class(out, "gp3_stimulus_luminance_audit")
  expect_equal(out$settings$value[out$settings$setting == "stimulus_file_col"], "stimulus_file")
  expect_equal(out$settings$value[out$settings$setting == "stimulus_id_col"], "stimulus_id")
  expect_equal(out$settings$value[out$settings$setting == "condition_col"], "condition")
})

test_that("audit_gazepoint_stimulus_luminance falls back to all_data without condition column", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  toy_data <- make_test_luminance_data() |>
    dplyr::select(-condition)

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    image_dir = tmp_dir
  )

  expect_equal(out$overview$n_conditions, 1)
  expect_equal(unique(out$stimulus_index$condition), "all_data")
  expect_equal(out$condition_summary$condition, "all_data")
  expect_true(is.na(out$settings$value[out$settings$setting == "condition_col"]))
})

test_that("audit_gazepoint_stimulus_luminance supports recursive file lookup", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()
  nested_dir <- file.path(tmp_dir, "nested")
  dir.create(nested_dir)

  file.copy(
    file.path(tmp_dir, "dark.png"),
    file.path(nested_dir, "nested_dark.png")
  )

  toy_data <- tibble::tibble(
    stimulus_id = "nested_dark",
    stimulus_file = "nested_dark.png",
    condition = "A"
  )

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    image_dir = tmp_dir,
    recursive = TRUE
  )

  expect_equal(out$overview$n_files_available, 1)
  expect_equal(out$overview$n_luminance_available, 1)
  expect_true(out$stimulus_luminance$file_exists)
  expect_true(out$stimulus_luminance$luminance_available)
})

test_that("audit_gazepoint_stimulus_luminance reports missing files", {
  testthat::skip_if_not_installed("magick")

  tmp_dir <- make_test_luminance_images()

  toy_data <- tibble::tibble(
    stimulus_id = "missing_only",
    stimulus_file = "missing.png",
    condition = "A"
  )

  out <- audit_gazepoint_stimulus_luminance(
    toy_data,
    image_dir = tmp_dir
  )

  expect_equal(out$overview$n_files_available, 0)
  expect_equal(out$overview$n_luminance_available, 0)
  expect_equal(out$overview$audit_status, "no_luminance_available")
  expect_equal(out$stimulus_luminance$luminance_status, "file_missing")
  expect_false(out$stimulus_luminance$file_exists)
  expect_false(out$stimulus_luminance$luminance_available)
})

test_that("audit_gazepoint_stimulus_luminance checks invalid inputs", {
  toy_data <- make_test_luminance_data()

  expect_error(
    audit_gazepoint_stimulus_luminance(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(toy_data[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(
      toy_data,
      stimulus_file_col = "bad_file"
    ),
    "`stimulus_file_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(
      toy_data,
      stimulus_id_col = "bad_id"
    ),
    "`stimulus_id_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(
      toy_data,
      condition_col = "bad_condition"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(
      toy_data,
      recursive = NA
    ),
    "`recursive` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_stimulus_luminance(
      toy_data,
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
