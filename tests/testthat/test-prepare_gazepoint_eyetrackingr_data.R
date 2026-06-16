make_test_eyetrackingr_master <- function() {
  tibble::tibble(
    subject = c("S1", "S1", "S1", "S1"),
    MEDIA_ID = c("stim1", "stim1", "stim1", "stim1"),
    trial_global = c(1, 1, 1, 1),
    time = c(0, 16, 32, 48),
    FPOGX = c(0.20, 0.60, NA, 0.80),
    FPOGY = c(0.20, 0.60, 0.30, 0.80),
    FPOGV = c(1, 1, 0, 1),
    aoi_current = c("logo", "product", "logo", "outside"),
    condition = c("A", "A", "A", "A")
  )
}

test_that("prepare_gazepoint_eyetrackingr_data creates a complete adapter table", {
  toy_master <- make_test_eyetrackingr_master()

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "FPOGX",
    y_col = "FPOGY",
    media_col = "MEDIA_ID",
    condition_col = "condition",
    validity_cols = "FPOGV",
    aoi_values = c("logo", "product")
  )

  expect_s3_class(out, "gp3_eyetrackingr_data")
  expect_s3_class(out, "tbl_df")
  expect_equal(attr(out, "gp3_adapter"), "eyetrackingr")
  expect_equal(
    attr(out, "gp3_aoi_indicator_cols"),
    c("aoi_logo", "aoi_product")
  )
  expect_s3_class(attr(out, "gp3_settings"), "tbl_df")

  expect_equal(nrow(out), 4)
  expect_true(
    all(
      c(
        "participant",
        "trial",
        "time",
        "gaze_x",
        "gaze_y",
        "media_id",
        "condition",
        "aoi",
        "aoi_raw",
        "trackloss",
        "eyetrackingr_data_status",
        "aoi_logo",
        "aoi_product"
      ) %in% names(out)
    )
  )

  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$time, c(0, 16, 32, 48))
  expect_equal(out$gaze_x, c(0.20, 0.60, NA, 0.80))
  expect_equal(out$gaze_y, c(0.20, 0.60, 0.30, 0.80))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$aoi_raw, c("logo", "product", "logo", "outside"))

  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(
    out$eyetrackingr_data_status,
    c("ready", "ready", "trackloss", "ready")
  )

  expect_equal(out$aoi_logo, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$aoi_product, c(FALSE, TRUE, FALSE, FALSE))

  expect_true("subject" %in% names(out))
  expect_true("MEDIA_ID" %in% names(out))
})

test_that("prepare_gazepoint_eyetrackingr_data auto-detects common Gazepoint columns", {
  toy_master <- make_test_eyetrackingr_master()

  out <- prepare_gazepoint_eyetrackingr_data(toy_master)

  expect_s3_class(out, "gp3_eyetrackingr_data")
  expect_equal(out$participant, rep("S1", 4))
  expect_equal(out$trial, rep("1", 4))
  expect_equal(out$media_id, rep("stim1", 4))
  expect_equal(out$condition, rep("A", 4))
  expect_equal(out$aoi, c("logo", "product", "logo", "outside"))
  expect_equal(out$trackloss, c(FALSE, FALSE, TRUE, FALSE))

  expect_equal(
    attr(out, "gp3_aoi_indicator_cols"),
    c("aoi_logo", "aoi_product")
  )

  settings <- attr(out, "gp3_settings")

  expect_equal(
    settings$value[settings$setting == "participant_col"],
    "subject"
  )
  expect_equal(
    settings$value[settings$setting == "trial_col"],
    "trial_global"
  )
  expect_equal(
    settings$value[settings$setting == "time_col"],
    "time"
  )
  expect_equal(
    settings$value[settings$setting == "aoi_col"],
    "aoi_current"
  )
  expect_equal(
    settings$value[settings$setting == "x_col"],
    "FPOGX"
  )
  expect_equal(
    settings$value[settings$setting == "y_col"],
    "FPOGY"
  )
  expect_equal(
    settings$value[settings$setting == "media_col"],
    "MEDIA_ID"
  )
})

test_that("prepare_gazepoint_eyetrackingr_data can drop original columns", {
  toy_master <- make_test_eyetrackingr_master()

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    keep_original_cols = FALSE
  )

  expect_s3_class(out, "gp3_eyetrackingr_data")
  expect_false("subject" %in% names(out))
  expect_false("MEDIA_ID" %in% names(out))
  expect_false("FPOGX" %in% names(out))
  expect_true("participant" %in% names(out))
  expect_true("gaze_x" %in% names(out))
})

test_that("prepare_gazepoint_eyetrackingr_data uses media as trial fallback", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    MEDIA_ID = c("stim1", "stim2"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3),
    aoi_current = c("logo", "product")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    media_col = "MEDIA_ID",
    aoi_col = "aoi_current",
    x_col = "FPOGX",
    y_col = "FPOGY"
  )

  expect_equal(out$trial, c("stim1", "stim2"))
  expect_equal(out$media_id, c("stim1", "stim2"))
})

test_that("prepare_gazepoint_eyetrackingr_data uses trial_1 fallback without trial or media", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    time = c(0, 16),
    FPOGX = c(0.2, 0.3),
    FPOGY = c(0.2, 0.3),
    aoi_current = c("logo", "product")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "FPOGX",
    y_col = "FPOGY"
  )

  expect_equal(out$trial, c("trial_1", "trial_1"))
  expect_true(all(is.na(out$media_id)))
})

test_that("prepare_gazepoint_eyetrackingr_data supports explicit trackloss columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    aoi_current = c("logo", "logo", "product"),
    track_loss = c(FALSE, TRUE, FALSE)
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y",
    trackloss_col = "track_loss",
    aoi_values = c("logo", "product")
  )

  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$eyetrackingr_data_status, c("ready", "trackloss", "ready"))
  expect_equal(out$aoi_logo, c(TRUE, FALSE, FALSE))
  expect_equal(out$aoi_product, c(FALSE, FALSE, TRUE))
})

test_that("prepare_gazepoint_eyetrackingr_data supports logical and character validity columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    aoi_current = c("logo", "logo", "product"),
    valid_gaze = c(TRUE, FALSE, TRUE),
    validity_label = c("valid", "invalid", "ok")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y",
    validity_cols = c("valid_gaze", "validity_label")
  )

  expect_equal(out$trackloss, c(FALSE, TRUE, FALSE))
  expect_equal(out$eyetrackingr_data_status, c("ready", "trackloss", "ready"))
})

test_that("prepare_gazepoint_eyetrackingr_data standardises missing AOI values", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1", "S1"),
    trial_global = c(1, 1, 1),
    time = c(0, 16, 32),
    x = c(0.2, 0.3, 0.4),
    y = c(0.2, 0.3, 0.4),
    aoi_current = c("logo", NA_character_, "")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y",
    missing_aoi_label = "missing_aoi"
  )

  expect_equal(out$aoi, c("logo", "missing_aoi", "missing_aoi"))
  expect_equal(attr(out, "gp3_aoi_indicator_cols"), "aoi_logo")
})

test_that("prepare_gazepoint_eyetrackingr_data detects AOI values and excludes non-AOI labels", {
  toy_master <- tibble::tibble(
    subject = rep("S1", 5),
    trial_global = rep(1, 5),
    time = c(0, 16, 32, 48, 64),
    x = c(0.1, 0.2, 0.3, 0.4, 0.5),
    y = c(0.1, 0.2, 0.3, 0.4, 0.5),
    aoi_current = c("logo", "product", "outside", "background", NA)
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(
    attr(out, "gp3_aoi_indicator_cols"),
    c("aoi_logo", "aoi_product")
  )
  expect_true("aoi_logo" %in% names(out))
  expect_true("aoi_product" %in% names(out))
  expect_false("aoi_outside" %in% names(out))
  expect_false("aoi_background" %in% names(out))
})

test_that("prepare_gazepoint_eyetrackingr_data creates safe AOI indicator names", {
  toy_master <- tibble::tibble(
    subject = rep("S1", 3),
    trial_global = rep(1, 3),
    time = c(0, 16, 32),
    x = c(0.1, 0.2, 0.3),
    y = c(0.1, 0.2, 0.3),
    aoi_current = c("AOI 1", "Product-Image", "Brand Logo")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y",
    aoi_values = c("AOI 1", "Product-Image", "Brand Logo")
  )

  expect_equal(
    attr(out, "gp3_aoi_indicator_cols"),
    c("aoi_aoi_1", "aoi_product_image", "aoi_brand_logo")
  )
  expect_true(all(c("aoi_aoi_1", "aoi_product_image", "aoi_brand_logo") %in% names(out)))
})

test_that("prepare_gazepoint_eyetrackingr_data reports row status problems", {
  toy_master <- tibble::tibble(
    subject = c("S1", NA, "S3"),
    trial_global = c(1, 1, NA),
    time = c(0, 16, NA),
    x = c(0.1, 0.2, 0.3),
    y = c(0.1, 0.2, 0.3),
    aoi_current = c("logo", "logo", "logo")
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time",
    aoi_col = "aoi_current",
    x_col = "x",
    y_col = "y"
  )

  expect_equal(
    out$eyetrackingr_data_status,
    c("ready", "missing_participant", "missing_time")
  )
})

test_that("prepare_gazepoint_eyetrackingr_data handles missing optional AOI and coordinate columns", {
  toy_master <- tibble::tibble(
    subject = c("S1", "S1"),
    trial_global = c(1, 1),
    time = c(0, 16)
  )

  out <- prepare_gazepoint_eyetrackingr_data(
    toy_master,
    participant_col = "subject",
    trial_col = "trial_global",
    time_col = "time"
  )

  expect_equal(out$gaze_x, c(NA_real_, NA_real_))
  expect_equal(out$gaze_y, c(NA_real_, NA_real_))
  expect_equal(out$aoi, c("missing_aoi", "missing_aoi"))
  expect_equal(out$trackloss, c(FALSE, FALSE))
  expect_equal(attr(out, "gp3_aoi_indicator_cols"), character(0))
})

test_that("prepare_gazepoint_eyetrackingr_data checks invalid inputs", {
  toy_master <- make_test_eyetrackingr_master()

  expect_error(
    prepare_gazepoint_eyetrackingr_data(list()),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(toy_master[0, ]),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      participant_col = "bad_subject"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      time_col = "bad_time"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      validity_cols = "bad_validity"
    ),
    "All `validity_cols` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      aoi_values = character()
    ),
    "`aoi_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      aoi_prefix = ""
    ),
    "`aoi_prefix` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      missing_aoi_label = ""
    ),
    "`missing_aoi_label` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      non_aoi_values = character()
    ),
    "`non_aoi_values` must be a non-empty character vector",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_eyetrackingr_data(
      toy_master,
      keep_original_cols = NA
    ),
    "`keep_original_cols` must be TRUE or FALSE",
    fixed = TRUE
  )
})
