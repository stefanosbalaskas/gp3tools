make_test_aoi_transition_plot_data <- function() {
  tibble::tibble(
    subject = rep("S1", 10),
    MEDIA_ID = rep(0, 10),
    trial_global = rep("S1_M0", 10),
    condition = rep("A", 10),
    time = seq(0, 900, by = 100),
    aoi_current = c(
      "non_aoi", "AOI 1", "AOI 1", "non_aoi", "AOI 2",
      "AOI 2", "AOI 1", "AOI 1", "non_aoi", "AOI 2"
    )
  )
}

test_that("plot_gazepoint_aoi_transition_matrix plots probability heatmap from matrix object", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(mat)

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Gazepoint AOI transition probabilities")
  expect_equal(p$labels$fill, "Probability")
})

test_that("plot_gazepoint_aoi_transition_matrix plots count heatmap from matrix object", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(
    mat,
    value = "n"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Gazepoint AOI transition counts")
  expect_equal(p$labels$fill, "Count")
})

test_that("plot_gazepoint_aoi_transition_matrix supports long-form transition tables", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(
    mat$long_table,
    value = "prob"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_gazepoint_aoi_transition_matrix supports numeric matrix inputs", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p_count <- plot_gazepoint_aoi_transition_matrix(
    mat$count_matrix,
    value = "n"
  )

  p_prob <- plot_gazepoint_aoi_transition_matrix(
    mat$probability_matrix,
    value = "prob"
  )

  expect_s3_class(p_count, "ggplot")
  expect_s3_class(p_prob, "ggplot")
})

test_that("plot_gazepoint_aoi_transition_matrix supports grouped transition objects", {
  x <- dplyr::bind_rows(
    make_test_aoi_transition_plot_data(),
    make_test_aoi_transition_plot_data() |>
      dplyr::mutate(
        subject = "S2",
        MEDIA_ID = 1,
        trial_global = "S2_M1",
        condition = "B"
      )
  )

  mat <- compute_gazepoint_aoi_transition_matrix(
    x,
    by_cols = "condition"
  )

  p <- plot_gazepoint_aoi_transition_matrix(mat)

  expect_s3_class(p, "ggplot")
  expect_true(".gp3_panel" %in% names(p$data))
  expect_equal(
    sort(unique(p$data$.gp3_panel)),
    c("condition=A", "condition=B")
  )
})

test_that("plot_gazepoint_aoi_transition_matrix can plot grouped long tables with explicit by_cols", {
  x <- dplyr::bind_rows(
    make_test_aoi_transition_plot_data(),
    make_test_aoi_transition_plot_data() |>
      dplyr::mutate(
        subject = "S2",
        MEDIA_ID = 1,
        trial_global = "S2_M1",
        condition = "B"
      )
  )

  mat <- compute_gazepoint_aoi_transition_matrix(
    x,
    by_cols = "condition"
  )

  p <- plot_gazepoint_aoi_transition_matrix(
    mat$long_table,
    by_cols = "condition"
  )

  expect_s3_class(p, "ggplot")
  expect_true(".gp3_panel" %in% names(p$data))
})

test_that("plot_gazepoint_aoi_transition_matrix supports explicit state order", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(
    mat,
    state_order = c("AOI 1", "AOI 2", "non_aoi")
  )

  expect_s3_class(p, "ggplot")
  expect_equal(
    levels(p$data$.gp3_to_plot),
    c("AOI 1", "AOI 2", "non_aoi")
  )
})

test_that("plot_gazepoint_aoi_transition_matrix can omit zero-completion grid", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(
    mat,
    include_zero = FALSE
  )

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), nrow(mat$long_table))
})

test_that("plot_gazepoint_aoi_transition_matrix can hide labels and use custom title", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  p <- plot_gazepoint_aoi_transition_matrix(
    mat,
    show_labels = FALSE,
    title = "Custom AOI transition plot"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Custom AOI transition plot")
})

test_that("plot_gazepoint_aoi_transition_matrix errors for invalid input objects", {
  expect_error(
    plot_gazepoint_aoi_transition_matrix("not valid"),
    "`transitions` must be a gp3_aoi_transition_matrix object, a data frame, or a matrix",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_transition_matrix errors for invalid arguments", {
  x <- make_test_aoi_transition_plot_data()

  mat <- compute_gazepoint_aoi_transition_matrix(x)

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      state_order = c("AOI 1", "AOI 1")
    ),
    "`state_order` must be NULL or a character vector of unique AOI labels",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      by_cols = c("condition", "condition")
    ),
    "`by_cols` must be NULL or a character vector of unique column names",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      include_zero = NA
    ),
    "`include_zero` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      show_labels = NA
    ),
    "`show_labels` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      facet = NA
    ),
    "`facet` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      label_digits = -1
    ),
    "`label_digits` must be greater than or equal to 0",
    fixed = TRUE
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      mat,
      label_size = 0
    ),
    "`label_size` must be greater than 0",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_transition_matrix errors when required columns are missing", {
  bad_long <- tibble::tibble(
    from = "AOI 1",
    to = "AOI 2"
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(bad_long),
    "Missing required columns"
  )

  good_long <- tibble::tibble(
    condition = "A",
    from = "AOI 1",
    to = "AOI 2",
    prob = 1
  )

  expect_error(
    plot_gazepoint_aoi_transition_matrix(
      good_long,
      by_cols = "missing_condition"
    ),
    "Missing `by_cols` columns"
  )
})

test_that("plot_gazepoint_aoi_transition_matrix errors for matrix without names", {
  bad_matrix <- matrix(1, nrow = 1, ncol = 1)

  expect_error(
    plot_gazepoint_aoi_transition_matrix(bad_matrix),
    "Matrix inputs must have row names and column names",
    fixed = TRUE
  )
})

test_that("plot_gazepoint_aoi_transition_matrix works with real master object when available", {
  if (exists("master", envir = .GlobalEnv, inherits = TRUE)) {
    real_data <- get("master", envir = .GlobalEnv, inherits = TRUE)

    required_cols <- c("subject", "MEDIA_ID", "trial_global", "time")

    if (all(required_cols %in% names(real_data)) &&
        any(c("aoi_current", "AOI", "aoi_state") %in% names(real_data))) {
      mat <- compute_gazepoint_aoi_transition_matrix(real_data)

      p <- plot_gazepoint_aoi_transition_matrix(mat)

      expect_s3_class(p, "ggplot")
    } else {
      expect_true(TRUE)
    }
  } else {
    expect_true(TRUE)
  }
})
