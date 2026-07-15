.make_detector_comparison_data <- function() {
  n <- 100L

  data.frame(
    USER_ID = rep("P01", n),
    trial = rep("T01", n),
    TIME = seq(0, by = 0.01, length.out = n),
    FPOGX = c(
      rep(0.20, 35),
      seq(0.20, 0.80, length.out = 10),
      rep(0.80, 55)
    ),
    FPOGY = rep(0.50, n),
    stringsAsFactors = FALSE
  )
}

test_that("velocity detector comparisons are deterministic", {
  result <- compare_gazepoint_event_detectors(
    .make_detector_comparison_data(),
    trial_col = "trial",
    methods = "velocity",
    velocity_thresholds = c(5, 10),
    min_duration = 40
  )

  expect_s3_class(
    result,
    "gp3_event_detector_comparison"
  )

  expect_true(is.data.frame(result$events))
  expect_true(is.data.frame(result$runs))
  expect_true(is.data.frame(result$detector_summary))
  expect_true(is.data.frame(result$pairwise_agreement))
  expect_true(is.data.frame(result$unmatched_events))

  expect_equal(
    sort(unique(result$events$detector)),
    c("velocity_10", "velocity_5")
  )

  expect_true(
    all(result$runs$status == "ok")
  )

  expect_equal(
    nrow(result$detector_summary),
    2L
  )
})

test_that("HMM failures are recorded without discarding velocity results", {
  result <- compare_gazepoint_event_detectors(
    .make_detector_comparison_data(),
    trial_col = "trial",
    methods = c("velocity", "hmm"),
    velocity_thresholds = 5,
    min_duration = 40,
    hmm_states = 3
  )

  expect_true(
    any(result$runs$family == "velocity")
  )

  expect_true(
    any(result$runs$family == "hmm")
  )

  expect_true(
    any(result$runs$status == "ok")
  )
})

test_that("optional eyetools branch can be skipped explicitly", {
  result <- compare_gazepoint_event_detectors(
    .make_detector_comparison_data(),
    trial_col = "trial",
    methods = c("velocity", "eyetools"),
    velocity_thresholds = 5,
    run_optional_eyetools = FALSE
  )

  eyetools_row <- result$runs[
    result$runs$family == "eyetools",
    ,
    drop = FALSE
  ]

  expect_equal(
    eyetools_row$status,
    "skipped_disabled"
  )
})

test_that("agreement summaries identify matched and unmatched events", {
  events <- data.frame(
    USER_ID = rep("P01", 5),
    trial = rep("T01", 5),
    detector = c(
      "A",
      "A",
      "B",
      "B",
      "B"
    ),
    family = c(
      "native",
      "native",
      "external",
      "external",
      "external"
    ),
    threshold = NA_real_,
    event_id = c(1, 2, 1, 2, 3),
    start_time = c(0, 2, 0.1, 2.1, 5),
    end_time = c(1, 3, 1.1, 3.1, 6),
    duration_ms = rep(1000, 5),
    mean_x = NA_real_,
    mean_y = NA_real_,
    n_samples = NA_integer_,
    source_status = "ok",
    stringsAsFactors = FALSE
  )

  comparison <- list(
    events = events,
    settings = list(
      sequence_cols = c("USER_ID", "trial"),
      min_overlap = 0.5
    )
  )

  class(comparison) <- c(
    "gp3_event_detector_comparison",
    "list"
  )

  summary <- summarise_gazepoint_event_detector_agreement(
    comparison,
    min_overlap = 0.5
  )

  expect_equal(
    nrow(summary$pairwise_agreement),
    1L
  )

  expect_equal(
    summary$pairwise_agreement$matched_a,
    2L
  )

  expect_equal(
    summary$pairwise_agreement$matched_b,
    2L
  )

  expect_equal(
    nrow(summary$unmatched_events),
    1L
  )

  expect_equal(
    summary$unmatched_events$event_id,
    3L
  )
})

test_that("comparison plots return their source data", {
  result <- compare_gazepoint_event_detectors(
    .make_detector_comparison_data(),
    trial_col = "trial",
    methods = "velocity",
    velocity_thresholds = c(5, 10)
  )

  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)

  on.exit(
    if (grDevices::dev.cur() > 1L) {
      grDevices::dev.off()
    },
    add = TRUE
  )

  counts <- plot_gazepoint_event_detector_agreement(
    result,
    plot = "counts"
  )

  durations <- plot_gazepoint_event_detector_agreement(
    result,
    plot = "durations"
  )

  agreement <- plot_gazepoint_event_detector_agreement(
    result,
    plot = "agreement"
  )

  grDevices::dev.off()

  expect_true(is.data.frame(counts))
  expect_true(is.data.frame(durations))
  expect_true(is.data.frame(agreement))
  expect_true(file.exists(plot_file))
})

test_that("invalid overlap thresholds are rejected", {
  expect_error(
    compare_gazepoint_event_detectors(
      .make_detector_comparison_data(),
      methods = "velocity",
      min_overlap = 2
    ),
    "between 0 and 1"
  )
})
