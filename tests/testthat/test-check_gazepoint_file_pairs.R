testthat::test_that("check_gazepoint_file_pairs identifies complete file pairs", {
  export_dir <- tempfile()
  dir.create(export_dir)

  file.create(file.path(export_dir, "User 0_all_gaze.csv"))
  file.create(file.path(export_dir, "User 0_fixations.csv"))
  file.create(file.path(export_dir, "User 1_all_gaze.csv"))
  file.create(file.path(export_dir, "User 1_fixations.csv"))

  pairs <- check_gazepoint_file_pairs(export_dir)

  testthat::expect_s3_class(pairs, "tbl_df")
  testthat::expect_equal(nrow(pairs), 2)
  testthat::expect_true(all(pairs$status == "complete"))
  testthat::expect_true(all(pairs$has_all_gaze))
  testthat::expect_true(all(pairs$has_fixation))
})

testthat::test_that("check_gazepoint_file_pairs identifies missing files", {
  export_dir <- tempfile()
  dir.create(export_dir)

  file.create(file.path(export_dir, "User 0_all_gaze.csv"))
  file.create(file.path(export_dir, "User 1_fixations.csv"))

  pairs <- check_gazepoint_file_pairs(export_dir)

  testthat::expect_equal(nrow(pairs), 2)

  status_user0 <- pairs$status[pairs$participant == "User 0"]
  status_user1 <- pairs$status[pairs$participant == "User 1"]

  testthat::expect_equal(status_user0, "missing_fixation")
  testthat::expect_equal(status_user1, "missing_all_gaze")
})

testthat::test_that("check_gazepoint_file_pairs identifies duplicates recursively", {
  export_dir <- tempfile()
  dir.create(export_dir)

  sub_dir <- file.path(export_dir, "duplicate_folder")
  dir.create(sub_dir)

  file.create(file.path(export_dir, "User 2_all_gaze.csv"))
  file.create(file.path(sub_dir, "User 2_all_gaze.csv"))
  file.create(file.path(export_dir, "User 2_fixations.csv"))

  pairs <- check_gazepoint_file_pairs(
    export_dir,
    recursive = TRUE
  )

  testthat::expect_equal(nrow(pairs), 1)
  testthat::expect_equal(pairs$participant, "User 2")
  testthat::expect_equal(pairs$n_all_gaze, 2)
  testthat::expect_equal(pairs$n_fixation, 1)
  testthat::expect_true(pairs$duplicate_all_gaze)
  testthat::expect_equal(pairs$status, "duplicate_files")
})

testthat::test_that("check_gazepoint_file_pairs requires an existing folder", {
  testthat::expect_error(
    check_gazepoint_file_pairs("folder_that_does_not_exist"),
    "`folder` does not exist"
  )
})

testthat::test_that("check_gazepoint_file_pairs requires matching files", {
  export_dir <- tempfile()
  dir.create(export_dir)

  testthat::expect_error(
    check_gazepoint_file_pairs(export_dir),
    "No files matching"
  )
})
