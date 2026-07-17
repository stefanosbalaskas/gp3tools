.make_benchmark_reviewed_events <- function() {
  data.frame(
    USER_ID = rep("P01", 3),
    trial = c("T01", "T01", "T02"),
    review_event_id = c(1L, 2L, 1L),
    start_time = c(0, 2, 0),
    end_time = c(1, 3, 1),
    stringsAsFactors = FALSE
  )
}

.make_benchmark_detected_events <- function() {
  data.frame(
    USER_ID = rep("P01", 7),
    trial = c(
      "T01", "T01", "T02",
      "T01", "T01", "T01", "T02"
    ),
    detector = c(
      rep("detector_A", 3),
      rep("detector_B", 4)
    ),
    family = c(
      rep("native", 3),
      rep("external", 4)
    ),
    threshold = NA_real_,
    event_id = c(1L, 2L, 1L, 1L, 2L, 3L, 1L),
    start_time = c(
      0.05, 2, 0,
      0, 2.2, 4, 3
    ),
    end_time = c(
      1.05, 3, 1,
      1, 3.2, 5, 4
    ),
    duration_ms = rep(1000, 7),
    stringsAsFactors = FALSE
  )
}

test_that("manual review templates preserve sequence boundaries", {
  gaze <- data.frame(
    USER_ID = rep(c("P01", "P02"), each = 5),
    trial = rep("T01", 10),
    TIME = rep(seq(0, 0.04, by = 0.01), 2),
    stringsAsFactors = FALSE
  )

  template <- create_gazepoint_event_review_template(
    gaze,
    trial_col = "trial",
    rows_per_sequence = 2,
    reviewer = "reviewer_1"
  )

  expect_equal(nrow(template), 4L)
  expect_equal(unique(template$sequence_start), 0)
  expect_equal(unique(template$sequence_end), 0.04)
  expect_true(all(template$review_status == "pending"))
  expect_true(all(template$reviewer == "reviewer_1"))
  expect_true(all(is.na(template$start_time)))
  expect_true(all(is.na(template$end_time)))
})

test_that("detector benchmarks return deterministic event metrics", {
  result <- benchmark_gazepoint_event_detectors(
    .make_benchmark_detected_events(),
    .make_benchmark_reviewed_events(),
    sequence_cols = c("USER_ID", "trial"),
    min_overlap = 0.5,
    time_unit = "seconds"
  )

  expect_s3_class(
    result,
    "gp3_event_detector_benchmark"
  )

  expect_true(is.data.frame(result$detector_metrics))
  expect_true(is.data.frame(result$sequence_metrics))
  expect_true(is.data.frame(result$matches))
  expect_true(is.data.frame(result$errors))

  detector_a <- result$detector_metrics[
    result$detector_metrics$detector == "detector_A",
    ,
    drop = FALSE
  ]

  detector_b <- result$detector_metrics[
    result$detector_metrics$detector == "detector_B",
    ,
    drop = FALSE
  ]

  expect_equal(detector_a$true_positive, 3L)
  expect_equal(detector_a$false_positive, 0L)
  expect_equal(detector_a$false_negative, 0L)
  expect_equal(detector_a$precision, 1)
  expect_equal(detector_a$recall, 1)
  expect_equal(detector_a$f1, 1)

  expect_equal(detector_b$true_positive, 2L)
  expect_equal(detector_b$false_positive, 2L)
  expect_equal(detector_b$false_negative, 1L)
  expect_equal(detector_b$precision, 0.5)
  expect_equal(detector_b$recall, 2 / 3)
  expect_equal(detector_b$f1, 4 / 7)

  expect_equal(nrow(result$matches), 5L)
  expect_equal(nrow(result$errors), 3L)
  expect_equal(
    sort(unique(result$errors$error_type)),
    c("false_negative", "false_positive")
  )
})

test_that("pending review rows are excluded", {
  reviewed <- .make_benchmark_reviewed_events()
  reviewed$review_status <- "accepted"

  pending <- reviewed[1L, , drop = FALSE]
  pending$review_event_id <- 99L
  pending$start_time <- 10
  pending$end_time <- 11
  pending$review_status <- "pending"

  reviewed <- rbind(reviewed, pending)

  result <- benchmark_gazepoint_event_detectors(
    .make_benchmark_detected_events(),
    reviewed,
    sequence_cols = c("USER_ID", "trial")
  )

  expect_equal(nrow(result$reviewed_events), 3L)
  expect_false(99L %in% result$reviewed_events$review_event_id)
})

test_that("comparison objects retain successful detector metadata", {
  detected <- .make_benchmark_detected_events()
  detected <- detected[
    detected$detector == "detector_A",
    ,
    drop = FALSE
  ]

  comparison <- list(
    events = detected,
    runs = data.frame(
      detector = "detector_A",
      family = "native",
      status = "ok",
      n_events = nrow(detected),
      message = NA_character_,
      stringsAsFactors = FALSE
    ),
    settings = list(
      sequence_cols = c("USER_ID", "trial")
    )
  )

  class(comparison) <- c(
    "gp3_event_detector_comparison",
    "list"
  )

  result <- benchmark_gazepoint_event_detectors(
    comparison,
    .make_benchmark_reviewed_events()
  )

  expect_equal(result$detector_metrics$detector, "detector_A")
  expect_equal(result$detector_metrics$family, "native")
  expect_equal(result$detector_metrics$f1, 1)
})

test_that("benchmark summaries expose each output level", {
  result <- benchmark_gazepoint_event_detectors(
    .make_benchmark_detected_events(),
    .make_benchmark_reviewed_events(),
    sequence_cols = c("USER_ID", "trial")
  )

  expect_equal(
    nrow(summarise_gazepoint_event_detector_benchmark(
      result,
      level = "detector"
    )),
    2L
  )

  expect_true(nrow(
    summarise_gazepoint_event_detector_benchmark(
      result,
      level = "sequence"
    )
  ) >= 2L)

  expect_equal(
    nrow(summarise_gazepoint_event_detector_benchmark(
      result,
      level = "matches"
    )),
    5L
  )

  expect_equal(
    nrow(summarise_gazepoint_event_detector_benchmark(
      result,
      level = "errors"
    )),
    3L
  )
})

test_that("benchmark plots return detector metrics", {
  result <- benchmark_gazepoint_event_detectors(
    .make_benchmark_detected_events(),
    .make_benchmark_reviewed_events(),
    sequence_cols = c("USER_ID", "trial")
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit(
    if (grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    },
    add = TRUE
  )

  plot_types <- c(
    "f1",
    "precision_recall",
    "overlap",
    "timing_error",
    "counts"
  )

  plotted <- lapply(
    plot_types,
    function(plot_type) {
      plot_gazepoint_event_detector_benchmark(
        result,
        plot = plot_type
      )
    }
  )

  grDevices::dev.off()

  expect_true(all(vapply(plotted, is.data.frame, logical(1))))
  expect_true(file.exists(plot_file))
})

test_that("invalid benchmark inputs are rejected", {
  expect_error(
    benchmark_gazepoint_event_detectors(
      .make_benchmark_detected_events(),
      .make_benchmark_reviewed_events(),
      sequence_cols = c("USER_ID", "trial"),
      min_overlap = 2
    ),
    "between 0 and 1"
  )

  pending <- .make_benchmark_reviewed_events()
  pending$review_status <- "pending"

  expect_error(
    benchmark_gazepoint_event_detectors(
      .make_benchmark_detected_events(),
      pending,
      sequence_cols = c("USER_ID", "trial")
    ),
    "No accepted reviewed events"
  )
})
