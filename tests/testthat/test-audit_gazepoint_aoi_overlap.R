make_test_aoi_overlap_data <- function() {
  tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("logo", "product", "price"),
    x_min = c(0.10, 0.25, 0.70),
    y_min = c(0.10, 0.25, 0.70),
    x_max = c(0.40, 0.55, 0.90),
    y_max = c(0.40, 0.55, 0.90)
  )
}

test_that("audit_gazepoint_aoi_overlap creates a complete overlap audit", {
  toy_overlap <- make_test_aoi_overlap_data()

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_s3_class(out, "gp3_aoi_overlap_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "geometry_summary",
      "pairwise_overlap",
      "overlap_summary",
      "flagged_overlaps",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$geometry_summary, "tbl_df")
  expect_true(is.data.frame(out$pairwise_overlap))
  expect_s3_class(out$overlap_summary, "tbl_df")
  expect_true(is.data.frame(out$flagged_overlaps))
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_rows, 3)
  expect_equal(out$overview$n_aois, 3)
  expect_equal(out$overview$n_aois_used, 3)
  expect_equal(out$overview$n_stimuli, 1)
  expect_equal(out$overview$n_aoi_pairs, 3)
  expect_equal(out$overview$n_overlapping_pairs, 1)
  expect_equal(out$overview$n_flagged_overlaps, 1)
  expect_equal(out$overview$max_overlap_area, 0.0225)
  expect_equal(out$overview$max_overlap_prop_smaller, 0.25)
  expect_equal(out$overview$aoi_overlap_status, "review")

  expect_equal(nrow(out$pairwise_overlap), 3)
  expect_equal(nrow(out$flagged_overlaps), 1)
  expect_equal(out$flagged_overlaps$aoi_1, "logo")
  expect_equal(out$flagged_overlaps$aoi_2, "product")
  expect_equal(out$flagged_overlaps$overlap_area, 0.0225)
  expect_equal(out$flagged_overlaps$overlap_prop_smaller, 0.25)
  expect_equal(out$flagged_overlaps$aoi_overlap_status, "overlap")

  expect_equal(nrow(out$overlap_summary), 1)
  expect_equal(out$overlap_summary$n_aoi_pairs, 3)
  expect_equal(out$overlap_summary$n_overlapping_pairs, 1)
  expect_equal(out$overlap_summary$n_flagged_overlaps, 1)
  expect_equal(out$overlap_summary$aoi_overlap_summary_status, "review")
})

test_that("audit_gazepoint_aoi_overlap reports ok when AOIs do not overlap", {
  toy_overlap <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("logo", "product", "price"),
    x_min = c(0.05, 0.35, 0.70),
    y_min = c(0.05, 0.35, 0.70),
    x_max = c(0.20, 0.55, 0.90),
    y_max = c(0.20, 0.55, 0.90)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_equal(out$overview$n_aoi_pairs, 3)
  expect_equal(out$overview$n_overlapping_pairs, 0)
  expect_equal(out$overview$n_flagged_overlaps, 0)
  expect_equal(out$overview$max_overlap_area, 0)
  expect_equal(out$overview$max_overlap_prop_smaller, 0)
  expect_equal(out$overview$aoi_overlap_status, "ok")
  expect_equal(nrow(out$flagged_overlaps), 0)
  expect_true(all(out$pairwise_overlap$aoi_overlap_status == "ok"))
})

test_that("audit_gazepoint_aoi_overlap respects overlap thresholds", {
  toy_overlap <- make_test_aoi_overlap_data()

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0.05,
    min_overlap_prop = 0.50
  )

  expect_equal(out$overview$n_overlapping_pairs, 1)
  expect_equal(out$overview$n_flagged_overlaps, 0)
  expect_equal(out$overview$aoi_overlap_status, "ok")
  expect_true(all(out$pairwise_overlap$aoi_overlap_status == "ok"))
})

test_that("audit_gazepoint_aoi_overlap supports origin-size geometry", {
  toy_overlap <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("logo", "product", "price"),
    x = c(0.10, 0.25, 0.70),
    y = c(0.10, 0.25, 0.70),
    width = c(0.30, 0.30, 0.20),
    height = c(0.30, 0.30, 0.20)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_col = "x",
    y_col = "y",
    width_col = "width",
    height_col = "height",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_equal(out$overview$n_aoi_pairs, 3)
  expect_equal(out$overview$n_flagged_overlaps, 1)
  expect_equal(out$flagged_overlaps$aoi_1, "logo")
  expect_equal(out$flagged_overlaps$aoi_2, "product")
})

test_that("audit_gazepoint_aoi_overlap works across multiple stimuli", {
  toy_overlap <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim2", "stim2"),
    aoi = c("logo", "product", "logo", "product"),
    x_min = c(0.10, 0.25, 0.10, 0.50),
    y_min = c(0.10, 0.25, 0.10, 0.50),
    x_max = c(0.40, 0.55, 0.30, 0.70),
    y_max = c(0.40, 0.55, 0.30, 0.70)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_equal(out$overview$n_stimuli, 2)
  expect_equal(out$overview$n_aoi_pairs, 2)
  expect_equal(out$overview$n_overlapping_pairs, 1)
  expect_equal(out$overview$n_flagged_overlaps, 1)
  expect_equal(nrow(out$overlap_summary), 2)

  stim1 <- out$overlap_summary[
    out$overlap_summary$media_id == "stim1",
    ,
    drop = FALSE
  ]

  stim2 <- out$overlap_summary[
    out$overlap_summary$media_id == "stim2",
    ,
    drop = FALSE
  ]

  expect_equal(stim1$aoi_overlap_summary_status, "review")
  expect_equal(stim2$aoi_overlap_summary_status, "ok")
})

test_that("audit_gazepoint_aoi_overlap works without stimulus column", {
  toy_overlap <- tibble::tibble(
    aoi = c("logo", "product", "price"),
    x_min = c(0.10, 0.25, 0.70),
    y_min = c(0.10, 0.25, 0.70),
    x_max = c(0.40, 0.55, 0.90),
    y_max = c(0.40, 0.55, 0.90)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_true(is.na(out$overview$n_stimuli))
  expect_equal(out$overview$n_aoi_pairs, 3)
  expect_equal(out$overview$n_flagged_overlaps, 1)
  expect_false("media_id" %in% names(out$pairwise_overlap))
})

test_that("audit_gazepoint_aoi_overlap returns empty pair table for single AOI", {
  toy_overlap <- tibble::tibble(
    media_id = "stim1",
    aoi = "logo",
    x_min = 0.10,
    y_min = 0.10,
    x_max = 0.40,
    y_max = 0.40
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max"
  )

  expect_equal(out$overview$n_aoi_pairs, 0)
  expect_equal(out$overview$n_overlapping_pairs, 0)
  expect_equal(out$overview$n_flagged_overlaps, 0)
  expect_true(is.na(out$overview$max_overlap_area))
  expect_true(is.na(out$overview$max_overlap_prop_smaller))
  expect_equal(out$overview$aoi_overlap_status, "ok")
  expect_equal(nrow(out$pairwise_overlap), 0)
  expect_equal(nrow(out$flagged_overlaps), 0)
})

test_that("audit_gazepoint_aoi_overlap ignores invalid geometry by default", {
  toy_overlap <- tibble::tibble(
    media_id = c("stim1", "stim1", "stim1"),
    aoi = c("valid1", "valid2", "invalid"),
    x_min = c(0.10, 0.25, NA_real_),
    y_min = c(0.10, 0.25, 0.10),
    x_max = c(0.40, 0.55, 0.30),
    y_max = c(0.40, 0.55, 0.30)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    aoi_col = "aoi",
    stimulus_col = "media_id",
    x_min_col = "x_min",
    y_min_col = "y_min",
    x_max_col = "x_max",
    y_max_col = "y_max",
    min_overlap_area = 0,
    min_overlap_prop = 0,
    ignore_invalid_geometry = TRUE
  )

  expect_equal(out$overview$n_aois, 3)
  expect_equal(out$overview$n_aois_used, 2)
  expect_equal(out$overview$n_aoi_pairs, 1)
  expect_equal(out$overview$n_flagged_overlaps, 1)
})

test_that("audit_gazepoint_aoi_overlap supports aliases and automatic detection", {
  toy_overlap <- tibble::tibble(
    MEDIA_ID = c("stim1", "stim1"),
    AOI = c("logo", "product"),
    left = c(0.10, 0.25),
    top = c(0.10, 0.25),
    right = c(0.40, 0.55),
    bottom = c(0.40, 0.55)
  )

  out <- audit_gazepoint_aoi_overlap(
    toy_overlap,
    stimulus_col = "MEDIA_ID",
    min_overlap_area = 0,
    min_overlap_prop = 0
  )

  expect_equal(out$overview$n_aoi_pairs, 1)
  expect_equal(out$overview$n_flagged_overlaps, 1)
  expect_true("media_id" %in% names(out$pairwise_overlap))
  expect_equal(
    out$settings$value[out$settings$setting == "aoi_col"],
    "aoi"
  )
  expect_equal(
    out$settings$value[out$settings$setting == "stimulus_col"],
    "media_id"
  )
})

test_that("audit_gazepoint_aoi_overlap checks invalid inputs", {
  toy_overlap <- make_test_aoi_overlap_data()

  expect_error(
    audit_gazepoint_aoi_overlap(
      data = list()
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap[0, ]
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap,
      min_overlap_area = -1
    ),
    "`min_overlap_area` must be a non-negative numeric scalar",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap,
      min_overlap_prop = 2
    ),
    "`min_overlap_prop` must be a numeric scalar between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap,
      ignore_invalid_geometry = NA
    ),
    "`ignore_invalid_geometry` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap,
      aoi_col = "bad_aoi"
    ),
    "`aoi_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    audit_gazepoint_aoi_overlap(
      toy_overlap,
      stimulus_col = "bad_stimulus"
    ),
    "`stimulus_col` must be present in `data`",
    fixed = TRUE
  )
})
