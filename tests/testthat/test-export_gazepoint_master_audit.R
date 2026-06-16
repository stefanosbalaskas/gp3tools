make_export_master <- function() {
  tibble::tibble(
    subject = c("P1", "P1", "P1", "P2", "P2", "P2"),
    MEDIA_ID = c("M1", "M1", "M2", "M1", "M2", "M2"),
    time = c(0, 10, 20, 0, 10, 20),
    x = c(100, NA, 120, -10, 200, 300),
    y = c(100, NA, 130, 100, 300, 200),
    raw_x = c(0.10, NA, 0.12, -0.01, 0.20, 0.30),
    raw_y = c(0.20, NA, 0.26, 0.20, 0.30, 0.40),
    valid_sample = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
    missing_gaze = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
    missing_pupil = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    gaze_offscreen = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    mean_pupil = c(3.5, NA, 3.6, 3.7, NA, 3.8),
    aoi_current = c("AOI 1", "missing", "non_aoi", "offscreen", "non_aoi", "AOI 2"),
    aoi_count = c(1L, 0L, 0L, 0L, 0L, 1L),
    screen_width_px = rep(1000, 6),
    screen_height_px = rep(500, 6)
  )
}

test_that("export_gazepoint_master_audit exports master, audit, and validation tables", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_")

  exported <- export_gazepoint_master_audit(
    master = master,
    output_dir = output_dir,
    prefix = "study1"
  )

  expect_s3_class(exported, "tbl_df")
  expect_equal(nrow(exported), 13)

  expect_equal(
    exported$table,
    c(
      "master",
      "audit_overview",
      "audit_by_subject",
      "audit_by_media",
      "audit_by_subject_media",
      "audit_aoi_states",
      "audit_pupil_summary",
      "audit_coordinate_summary",
      "validation_summary",
      "validation_checks",
      "validation_failed_checks",
      "validation_warning_checks",
      "validation_column_map"
    )
  )

  expect_true(all(file.exists(exported$file)))

  expect_equal(exported$n_rows[exported$table == "master"], 6)
  expect_equal(exported$n_cols[exported$table == "master"], ncol(master))

  expect_equal(exported$n_rows[exported$table == "audit_overview"], 1)
  expect_equal(exported$n_rows[exported$table == "validation_summary"], 1)
  expect_equal(exported$n_rows[exported$table == "validation_checks"], 15)
})

test_that("export_gazepoint_master_audit accepts supplied audit and validation objects", {
  master <- make_export_master()
  audit <- audit_gazepoint_master(master)
  validation <- validate_gazepoint_master(master)

  output_dir <- tempfile("gp3_export_supplied_")

  exported <- export_gazepoint_master_audit(
    master = master,
    audit = audit,
    validation = validation,
    output_dir = output_dir,
    prefix = "supplied"
  )

  expect_equal(nrow(exported), 13)
  expect_true(all(file.exists(exported$file)))
  expect_equal(exported$n_rows[exported$table == "audit_by_subject"], 2)
  expect_equal(exported$n_rows[exported$table == "validation_column_map"], 14)
})

test_that("export_gazepoint_master_audit can export only selected table groups", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_selected_")

  exported <- export_gazepoint_master_audit(
    master = master,
    output_dir = output_dir,
    prefix = "selected",
    export_master = FALSE,
    export_audit = TRUE,
    export_validation = FALSE
  )

  expect_equal(nrow(exported), 7)
  expect_false("master" %in% exported$table)
  expect_false(any(grepl("^validation_", exported$table)))
  expect_true(all(grepl("^audit_", exported$table)))
  expect_true(all(file.exists(exported$file)))
})

test_that("export_gazepoint_master_audit can export only the master table", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_master_only_")

  exported <- export_gazepoint_master_audit(
    master = master,
    output_dir = output_dir,
    prefix = "master_only",
    export_master = TRUE,
    export_audit = FALSE,
    export_validation = FALSE
  )

  expect_equal(nrow(exported), 1)
  expect_equal(exported$table, "master")
  expect_true(file.exists(exported$file))
})

test_that("export_gazepoint_master_audit respects overwrite = FALSE", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_overwrite_")

  export_gazepoint_master_audit(
    master = master,
    output_dir = output_dir,
    prefix = "overwrite_test"
  )

  expect_error(
    export_gazepoint_master_audit(
      master = master,
      output_dir = output_dir,
      prefix = "overwrite_test",
      overwrite = FALSE
    ),
    "already exist"
  )
})

test_that("export_gazepoint_master_audit validates supplied audit and validation objects", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_invalid_objects_")

  expect_error(
    export_gazepoint_master_audit(
      master = master,
      audit = list(overview = tibble::tibble(x = 1)),
      output_dir = output_dir,
      prefix = "bad_audit"
    ),
    "`audit` is missing required element"
  )

  expect_error(
    export_gazepoint_master_audit(
      master = master,
      validation = list(summary = tibble::tibble(x = 1)),
      output_dir = output_dir,
      prefix = "bad_validation"
    ),
    "`validation` is missing required element"
  )
})

test_that("export_gazepoint_master_audit errors when nothing is selected for export", {
  master <- make_export_master()
  output_dir <- tempfile("gp3_export_none_")

  expect_error(
    export_gazepoint_master_audit(
      master = master,
      output_dir = output_dir,
      prefix = "none",
      export_master = FALSE,
      export_audit = FALSE,
      export_validation = FALSE
    ),
    "Nothing to export"
  )
})

test_that("export_gazepoint_master_audit errors for invalid arguments", {
  master <- make_export_master()

  expect_error(
    export_gazepoint_master_audit("not a data frame"),
    "`master` must be a data frame"
  )

  expect_error(
    export_gazepoint_master_audit(master, output_dir = c("a", "b")),
    "`output_dir` must be a single character string"
  )

  expect_error(
    export_gazepoint_master_audit(master, prefix = ""),
    "`prefix` must be a single non-empty character string"
  )

  expect_error(
    export_gazepoint_master_audit(master, export_master = c(TRUE, FALSE)),
    "`export_master` must be `TRUE` or `FALSE`"
  )

  expect_error(
    export_gazepoint_master_audit(master, export_audit = c(TRUE, FALSE)),
    "`export_audit` must be `TRUE` or `FALSE`"
  )

  expect_error(
    export_gazepoint_master_audit(master, export_validation = c(TRUE, FALSE)),
    "`export_validation` must be `TRUE` or `FALSE`"
  )

  expect_error(
    export_gazepoint_master_audit(master, overwrite = c(TRUE, FALSE)),
    "`overwrite` must be `TRUE` or `FALSE`"
  )

  expect_error(
    export_gazepoint_master_audit(master, na = c("", "NA")),
    "`na` must be a single character string"
  )
})
