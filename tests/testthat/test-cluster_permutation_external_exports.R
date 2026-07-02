.make_gp3_external_cluster_demo <- function() {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 4,
    n_time_bins = 5,
    seed = 10
  )

  prepare_gazepoint_timecourse_test_data(
    raw,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )
}


test_that("export_gazepoint_mne_cluster_input writes conservative export files", {
  prepared <- .make_gp3_external_cluster_demo()
  outdir <- tempfile("gp3_mne_export_")

  written <- export_gazepoint_mne_cluster_input(prepared, outdir)

  expect_s3_class(written, "gp3_external_cluster_export")
  expect_true(all(file.exists(written$file)))
  expect_true(any(written$file_type == "mne_long_csv"))
  expect_true(any(grepl("README", basename(written$file))))
})


test_that("export_gazepoint_permuco_cluster_input writes conservative export files", {
  prepared <- .make_gp3_external_cluster_demo()
  outdir <- tempfile("gp3_permuco_export_")

  written <- export_gazepoint_permuco_cluster_input(prepared, outdir)

  expect_s3_class(written, "gp3_external_cluster_export")
  expect_true(all(file.exists(written$file)))
  expect_true(any(written$file_type == "permuco_long_csv"))
  expect_true(any(grepl("README", basename(written$file))))
})


test_that("export_gazepoint_permutes_cluster_input writes conservative export files", {
  prepared <- .make_gp3_external_cluster_demo()
  outdir <- tempfile("gp3_permutes_export_")

  written <- export_gazepoint_permutes_cluster_input(prepared, outdir)

  expect_s3_class(written, "gp3_external_cluster_export")
  expect_true(all(file.exists(written$file)))
  expect_true(any(written$file_type == "permutes_long_csv"))
  expect_true(any(grepl("README", basename(written$file))))
})


test_that("external cluster exports protect existing directories", {
  prepared <- .make_gp3_external_cluster_demo()
  outdir <- tempfile("gp3_existing_external_export_")
  dir.create(outdir)

  expect_error(
    export_gazepoint_mne_cluster_input(prepared, outdir),
    "already exists"
  )

  expect_error(
    export_gazepoint_permuco_cluster_input(prepared, outdir),
    "already exists"
  )

  expect_error(
    export_gazepoint_permutes_cluster_input(prepared, outdir),
    "already exists"
  )
})


test_that("external cluster exports support explicit raw columns", {
  raw <- simulate_gazepoint_cluster_timecourse_data(
    n_subjects = 4,
    n_time_bins = 5,
    seed = 11
  )

  outdir <- tempfile("gp3_raw_external_export_")

  written <- export_gazepoint_mne_cluster_input(
    raw,
    outdir = outdir,
    subject_col = "subject",
    condition_col = "condition",
    time_col = "time_bin",
    outcome_col = "outcome"
  )

  expect_true(all(file.exists(written$file)))

  csv_file <- written$file[written$file_type == "mne_long_csv"]
  exported <- utils::read.csv(csv_file)

  expect_true(all(c("subject", "condition", "time_bin", "outcome") %in% names(exported)))
  expect_true(nrow(exported) > 0)
})
