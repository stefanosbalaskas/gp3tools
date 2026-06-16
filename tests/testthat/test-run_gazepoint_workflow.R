testthat::test_that("run_gazepoint_workflow runs full workflow and exports outputs", {
  export_dir <- tempfile()
  output_dir <- tempfile()

  dir.create(export_dir)
  dir.create(output_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.032,0.52,0.50,1,0.52,0.50,1,1,1,1,1,AOI 2"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1",
      "0,Stimulus 1,0.032,0.120,0.52,0.50,AOI 2"
    ),
    fixation_file
  )

  results <- run_gazepoint_workflow(
    export_dir = export_dir,
    output_dir = output_dir,
    prefix = "test"
  )

  testthat::expect_named(
    results,
    c(
      "file_pairs",
      "all_gaze",
      "all_fix",
      "sampling",
      "quality",
      "flagged_quality",
      "aoi_table",
      "written_files",
      "written_plots",
      "written_report"
    )
  )

  testthat::expect_s3_class(results$file_pairs, "tbl_df")
  testthat::expect_true(all(results$file_pairs$status == "complete"))
  testthat::expect_s3_class(results$all_gaze, "tbl_df")
  testthat::expect_s3_class(results$all_fix, "tbl_df")
  testthat::expect_s3_class(results$sampling, "tbl_df")
  testthat::expect_s3_class(results$quality, "tbl_df")
  testthat::expect_s3_class(results$flagged_quality, "tbl_df")
  testthat::expect_s3_class(results$aoi_table, "tbl_df")
  testthat::expect_s3_class(results$written_files, "tbl_df")

  testthat::expect_null(results$written_plots)
  testthat::expect_null(results$written_report)

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_sampling.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_quality.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_flagged_quality.csv"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "test_aoi_table.csv"))
  )
})

testthat::test_that("run_gazepoint_workflow can run without exporting", {
  export_dir <- tempfile()
  dir.create(export_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1"
    ),
    fixation_file
  )

  results <- run_gazepoint_workflow(
    export_dir = export_dir
  )

  testthat::expect_s3_class(results$file_pairs, "tbl_df")
  testthat::expect_true(all(results$file_pairs$status == "complete"))
  testthat::expect_s3_class(results$aoi_table, "tbl_df")
  testthat::expect_s3_class(results$flagged_quality, "tbl_df")

  testthat::expect_null(results$written_files)
  testthat::expect_null(results$written_plots)
  testthat::expect_null(results$written_report)
})

testthat::test_that("run_gazepoint_workflow can save diagnostic plots", {
  export_dir <- tempfile()
  output_dir <- tempfile()

  dir.create(export_dir)
  dir.create(output_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.032,0.52,0.50,1,0.52,0.50,1,1,1,1,1,AOI 2"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1",
      "0,Stimulus 1,0.032,0.120,0.52,0.50,AOI 2"
    ),
    fixation_file
  )

  results <- run_gazepoint_workflow(
    export_dir = export_dir,
    output_dir = output_dir,
    prefix = "plot_test",
    save_plots = TRUE
  )

  testthat::expect_s3_class(results$written_plots, "tbl_df")
  testthat::expect_null(results$written_report)

  testthat::expect_true(
    file.exists(file.path(output_dir, "plot_test_tracking_quality_plot.png"))
  )

  testthat::expect_true(
    file.exists(file.path(output_dir, "plot_test_sampling_rate_plot.png"))
  )
})

testthat::test_that("run_gazepoint_workflow can create an HTML report", {
  export_dir <- tempfile()
  output_dir <- tempfile()

  dir.create(export_dir)
  dir.create(output_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.032,0.52,0.50,1,0.52,0.50,1,1,1,1,1,AOI 2"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1",
      "0,Stimulus 1,0.032,0.120,0.52,0.50,AOI 2"
    ),
    fixation_file
  )

  results <- run_gazepoint_workflow(
    export_dir = export_dir,
    output_dir = output_dir,
    prefix = "report_test",
    create_report = TRUE
  )

  testthat::expect_s3_class(results$written_report, "tbl_df")

  testthat::expect_true(
    file.exists(file.path(output_dir, "report_test_report.html"))
  )
})

testthat::test_that("run_gazepoint_workflow requires an existing export directory", {
  testthat::expect_error(
    run_gazepoint_workflow("folder_that_does_not_exist"),
    "`export_dir` does not exist"
  )
})

testthat::test_that("run_gazepoint_workflow requires plot output folder when saving plots without output_dir", {
  export_dir <- tempfile()
  dir.create(export_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1"
    ),
    fixation_file
  )

  testthat::expect_error(
    run_gazepoint_workflow(
      export_dir = export_dir,
      save_plots = TRUE
    ),
    "`output_dir` or `plot_output_dir` must be provided"
  )
})

testthat::test_that("run_gazepoint_workflow requires report output when creating report without output_dir", {
  export_dir <- tempfile()
  dir.create(export_dir)

  all_gaze_file <- file.path(export_dir, "User 0_all_gaze.csv")
  fixation_file <- file.path(export_dir, "User 0_fixations.csv")

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1",
      "0,Stimulus 1,0.016,0.51,0.50,1,0.51,0.50,1,1,1,1,1,AOI 1"
    ),
    all_gaze_file
  )

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,FPOGS,FPOGD,FPOGX,FPOGY,AOI",
      "0,Stimulus 1,0.000,0.100,0.50,0.50,AOI 1"
    ),
    fixation_file
  )

  testthat::expect_error(
    run_gazepoint_workflow(
      export_dir = export_dir,
      create_report = TRUE
    ),
    "`output_dir` or `report_file` must be provided"
  )
})

testthat::test_that("run_gazepoint_workflow stops when file pairs are incomplete", {
  export_dir <- tempfile()
  dir.create(export_dir)

  writeLines(
    c(
      "MEDIA_ID,MEDIA_NAME,TIME(2026/01/01 00:00:00),FPOGX,FPOGY,FPOGV,BPOGX,BPOGY,BPOGV,LPV,RPV,LPMMV,RPMMV,AOI",
      "0,Stimulus 1,0.000,0.50,0.50,1,0.50,0.50,1,1,1,1,1,AOI 1"
    ),
    file.path(export_dir, "User 0_all_gaze.csv")
  )

  testthat::expect_error(
    run_gazepoint_workflow(
      export_dir = export_dir
    ),
    "Gazepoint file-pair check failed"
  )
})
