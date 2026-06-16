test_that("summarise_gazepoint_workflow returns expected workflow counts", {
  results <- list(
    file_pairs = tibble::tibble(
      source_id = paste0("User ", 0:5),
      status = rep("complete", 6)
    ),
    all_gaze = tibble::tibble(x = seq_len(7340)),
    all_fix = tibble::tibble(x = seq_len(337)),
    sampling = tibble::tibble(x = seq_len(12)),
    quality = tibble::tibble(x = seq_len(12)),
    flagged_quality = tibble::tibble(
      review_required = c(TRUE, TRUE, rep(FALSE, 10))
    ),
    aoi_table = tibble::tibble(x = seq_len(13)),
    written_files = list(
      sampling = "study1_sampling.csv",
      quality = "study1_quality.csv",
      flagged_quality = "study1_flagged_quality.csv",
      aoi_table = "study1_aoi_table.csv"
    ),
    written_plots = list(
      tracking_quality = "study1_tracking_quality_plot.png",
      sampling_rate = "study1_sampling_rate_plot.png"
    ),
    written_report = list(
      report = "gazepoint_report.html"
    )
  )

  summary <- summarise_gazepoint_workflow(results)

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 1)
  expect_equal(ncol(summary), 13)

  expect_equal(summary$all_gaze_rows, 7340)
  expect_equal(summary$fixation_rows, 337)
  expect_equal(summary$sampling_rows, 12)
  expect_equal(summary$tracking_quality_rows, 12)
  expect_equal(summary$flagged_quality_rows, 12)
  expect_equal(summary$aoi_rows, 13)
  expect_equal(summary$review_required_rows, 2)

  expect_equal(summary$file_pair_rows, 6)
  expect_equal(summary$complete_file_pairs, 6)
  expect_equal(summary$problem_file_pairs, 0)

  expect_equal(summary$output_table_files, 4)
  expect_equal(summary$output_plot_files, 2)
  expect_true(summary$report_created)
})

test_that("summarise_gazepoint_workflow handles missing optional outputs", {
  results <- list(
    all_gaze = tibble::tibble(x = 1:10),
    all_fix = tibble::tibble(x = 1:5),
    sampling = tibble::tibble(x = 1:2),
    quality = tibble::tibble(x = 1:2),
    flagged_quality = tibble::tibble(
      review_required = c(FALSE, FALSE)
    ),
    aoi_table = tibble::tibble(x = 1:3)
  )

  summary <- summarise_gazepoint_workflow(results)

  expect_equal(summary$output_table_files, 0)
  expect_equal(summary$output_plot_files, 0)
  expect_false(summary$report_created)

  expect_true(is.na(summary$file_pair_rows))
  expect_true(is.na(summary$complete_file_pairs))
  expect_true(is.na(summary$problem_file_pairs))
})

test_that("summarise_gazepoint_workflow handles NULL written_report", {
  results <- list(
    all_gaze = tibble::tibble(x = 1:10),
    all_fix = tibble::tibble(x = 1:5),
    sampling = tibble::tibble(x = 1:2),
    quality = tibble::tibble(x = 1:2),
    flagged_quality = tibble::tibble(
      review_required = c(FALSE, TRUE)
    ),
    aoi_table = tibble::tibble(x = 1:3),
    written_files = list(a = "a.csv"),
    written_plots = list(),
    written_report = NULL
  )

  summary <- summarise_gazepoint_workflow(results)

  expect_equal(summary$review_required_rows, 1)
  expect_equal(summary$output_table_files, 1)
  expect_equal(summary$output_plot_files, 0)
  expect_false(summary$report_created)
})

test_that("summarise_gazepoint_workflow errors for invalid input", {
  expect_error(
    summarise_gazepoint_workflow("not a results object"),
    "`results` must be a list"
  )
})

test_that("summarise_gazepoint_workflow errors when required elements are missing", {
  incomplete_results <- list(
    all_gaze = tibble::tibble(x = 1:10)
  )

  expect_error(
    summarise_gazepoint_workflow(incomplete_results),
    "`results` is missing required elements"
  )
})
