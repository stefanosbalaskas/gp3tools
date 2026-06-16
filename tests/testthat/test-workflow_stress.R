testthat::test_that("run_gazepoint_workflow handles many participant files", {
  export_dir <- tempfile()
  output_dir <- tempfile()

  dir.create(export_dir)
  dir.create(output_dir)

  n_users <- 70

  for (user_id in seq_len(n_users) - 1) {
    all_gaze_file <- file.path(
      export_dir,
      paste0("User ", user_id, "_all_gaze.csv")
    )

    fixation_file <- file.path(
      export_dir,
      paste0("User ", user_id, "_fixations.csv")
    )

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
  }

  results <- run_gazepoint_workflow(
    export_dir = export_dir,
    output_dir = output_dir,
    prefix = "stress_test",
    save_plots = TRUE,
    create_report = TRUE
  )

  testthat::expect_equal(nrow(results$all_gaze), n_users * 3)
  testthat::expect_equal(nrow(results$all_fix), n_users * 2)

  testthat::expect_equal(nrow(results$sampling), n_users)
  testthat::expect_equal(nrow(results$quality), n_users)

  testthat::expect_s3_class(results$aoi_table, "tbl_df")
  testthat::expect_s3_class(results$written_files, "tbl_df")
  testthat::expect_s3_class(results$written_plots, "tbl_df")
  testthat::expect_s3_class(results$written_report, "tbl_df")
  testthat::expect_equal(nrow(results$file_pairs), n_users)
  testthat::expect_true(all(results$file_pairs$status == "complete"))

  testthat::expect_true(
    file.exists(file.path(output_dir, "stress_test_report.html"))
  )
})
