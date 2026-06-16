make_test_fixalign_data <- function() {
  tidyr::expand_grid(
    subject = paste0("S", 1:4),
    trial = paste0("T", 1:3),
    time = seq(0, 800, by = 100)
  ) |>
    dplyr::mutate(
      aoi = dplyr::case_when(
        .data$time < 300 ~ "Background",
        .data$time >= 300 ~ "Target"
      ),
      fixation = .data$time %% 200 == 0,
      saccade_to_aoi = .data$time == 300,
      custom_event = .data$time == 500,
      event_label = dplyr::if_else(.data$time == 600, "align_here", "none"),
      outcome = 1 + .data$time / 1000
    )
}

make_test_fixalign_prelook_data <- function() {
  tidyr::expand_grid(
    subject = paste0("S", 1:3),
    trial = paste0("T", 1:2),
    time = seq(0, 600, by = 100)
  ) |>
    dplyr::mutate(
      aoi = dplyr::case_when(
        .data$time >= 100 & .data$time <= 400 ~ "Target",
        TRUE ~ "Background"
      ),
      fixation = .data$time == 400,
      saccade_to_aoi = .data$time == 300
    )
}

test_that("prepare_gazepoint_fixation_aligned_data aligns to first target entry", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    fixation_col = "fixation",
    saccade_col = "saccade_to_aoi",
    alignment_event = "first_target_entry",
    baseline_window = c(-200, 0),
    analysis_window = c(0, 500),
    name = "toy_first_target_entry"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "aligned_data",
      "event_table",
      "trial_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$aligned_data, "tbl_df")
  expect_s3_class(out$event_table, "tbl_df")
  expect_s3_class(out$trial_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_first_target_entry")
  expect_equal(out$overview$alignment_status, "complete")
  expect_equal(out$overview$alignment_event, "first_target_entry")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_output_rows, nrow(toy_data))
  expect_equal(out$overview$n_groups, 12)
  expect_equal(out$overview$n_aligned_groups, 12)
  expect_equal(out$overview$n_unaligned_groups, 0)
  expect_equal(out$overview$target_aoi, "Target")
  expect_equal(out$overview$baseline_window, "-200, 0")
  expect_equal(out$overview$analysis_window, "0, 500")
  expect_false(out$overview$keep_unaligned)

  expect_equal(nrow(out$event_table), 12)
  expect_true(all(out$event_table$gp3_has_alignment_event))
  expect_true(all(out$event_table$gp3_alignment_time == 300))
  expect_true(all(out$event_table$gp3_event_aoi == "Target"))
  expect_true(all(out$event_table$gp3_event_is_target_aoi))
  expect_false(any(out$event_table$gp3_target_present_before_event))
  expect_false(any(out$event_table$gp3_already_on_target_at_trial_start))

  expect_true(all(c(
    "gp3_alignment_time",
    "gp3_aligned_time",
    "gp3_alignment_phase",
    "gp3_is_alignment_event_row",
    "gp3_target_present_before_event",
    "gp3_already_on_target_at_trial_start"
  ) %in% names(out$aligned_data)))

  one_group <- out$aligned_data |>
    dplyr::filter(.data$subject == "S1", .data$trial == "T1") |>
    dplyr::arrange(.data$time)

  expect_equal(one_group$gp3_alignment_time, rep(300, nrow(one_group)))
  expect_equal(one_group$gp3_aligned_time, one_group$time - 300)
  expect_equal(
    one_group$gp3_alignment_phase[one_group$time == 200],
    "pre_event"
  )
  expect_equal(
    one_group$gp3_alignment_phase[one_group$time == 300],
    "alignment_event"
  )
  expect_equal(
    one_group$gp3_alignment_phase[one_group$time == 400],
    "post_event"
  )
  expect_true(one_group$gp3_is_alignment_event_row[one_group$time == 300])
  expect_true(one_group$gp3_in_baseline_window[one_group$time == 100])
  expect_true(one_group$gp3_in_baseline_window[one_group$time == 300])
  expect_true(one_group$gp3_in_analysis_window[one_group$time == 300])
  expect_true(one_group$gp3_in_analysis_window[one_group$time == 800])
})

test_that("prepare_gazepoint_fixation_aligned_data aligns to first fixation to target", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    fixation_col = "fixation",
    alignment_event = "first_fixation_to_target",
    name = "toy_first_fixation_to_target"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_equal(out$overview$alignment_event, "first_fixation_to_target")
  expect_true(all(out$event_table$gp3_alignment_time == 400))
  expect_true(all(out$event_table$gp3_event_is_target_aoi))
  expect_true(all(out$event_table$gp3_event_is_fixation))
  expect_true(all(out$event_table$gp3_target_present_before_event))

  one_group <- out$aligned_data |>
    dplyr::filter(.data$subject == "S1", .data$trial == "T1")

  expect_equal(one_group$gp3_aligned_time[one_group$time == 400], 0)
  expect_true(one_group$gp3_is_alignment_event_row[one_group$time == 400])
})

test_that("prepare_gazepoint_fixation_aligned_data aligns to first saccade to AOI", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    saccade_col = "saccade_to_aoi",
    alignment_event = "first_saccade_to_aoi",
    name = "toy_first_saccade_to_aoi"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_equal(out$overview$alignment_event, "first_saccade_to_aoi")
  expect_true(all(out$event_table$gp3_alignment_time == 300))
  expect_true(all(out$event_table$gp3_event_is_saccade))
  expect_true(any(out$aligned_data$gp3_is_saccade_sample))
})

test_that("prepare_gazepoint_fixation_aligned_data aligns to first fixation regardless of AOI", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    fixation_col = "fixation",
    alignment_event = "first_fixation",
    name = "toy_first_fixation"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_equal(out$overview$alignment_event, "first_fixation")
  expect_true(all(out$event_table$gp3_alignment_time == 0))
  expect_true(all(out$event_table$gp3_event_is_fixation))
  expect_true(all(out$aligned_data$gp3_alignment_time == 0))
})

test_that("prepare_gazepoint_fixation_aligned_data supports logical custom events", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    event_col = "custom_event",
    alignment_event = "custom",
    name = "toy_custom_event"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_equal(out$overview$alignment_event, "custom")
  expect_true(all(out$event_table$gp3_alignment_time == 500))
})

test_that("prepare_gazepoint_fixation_aligned_data supports custom event values", {
  toy_data <- make_test_fixalign_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    event_col = "event_label",
    event_value = "align_here",
    alignment_event = "custom",
    name = "toy_custom_value_event"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_true(all(out$event_table$gp3_alignment_time == 600))
  expect_equal(
    out$settings$value[out$settings$setting == "event_value"],
    "align_here"
  )
})

test_that("prepare_gazepoint_fixation_aligned_data identifies pre-existing target looking", {
  toy_data <- make_test_fixalign_prelook_data()

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    fixation_col = "fixation",
    alignment_event = "first_fixation_to_target",
    name = "toy_pre_existing_target"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_true(all(out$event_table$gp3_alignment_time == 400))
  expect_true(all(out$event_table$gp3_target_present_before_event))
  expect_false(any(out$event_table$gp3_already_on_target_at_trial_start))
  expect_true(all(out$event_table$gp3_pre_event_target_n_samples > 0))
  expect_true(all(out$aligned_data$gp3_target_present_before_event))
})

test_that("prepare_gazepoint_fixation_aligned_data identifies already-on-target trials", {
  toy_data <- tidyr::expand_grid(
    subject = paste0("S", 1:2),
    trial = paste0("T", 1:2),
    time = seq(0, 400, by = 100)
  ) |>
    dplyr::mutate(
      aoi = dplyr::if_else(.data$time <= 200, "Target", "Background"),
      fixation = .data$time == 100
    )

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    fixation_col = "fixation",
    alignment_event = "first_fixation_to_target",
    name = "toy_already_on_target"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "complete")
  expect_true(all(out$event_table$gp3_already_on_target_at_trial_start))
  expect_true(all(out$event_table$gp3_alignment_time == 100))
})

test_that("prepare_gazepoint_fixation_aligned_data handles unaligned groups", {
  toy_data <- make_test_fixalign_data() |>
    dplyr::mutate(
      aoi = dplyr::if_else(.data$subject == "S4", "Background", .data$aoi)
    )

  out_drop <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    alignment_event = "first_target_entry",
    keep_unaligned = FALSE,
    name = "toy_partial_drop"
  )

  expect_s3_class(out_drop, "gp3_fixation_aligned_data")
  expect_equal(out_drop$overview$alignment_status, "partial_complete")
  expect_equal(out_drop$overview$n_groups, 12)
  expect_equal(out_drop$overview$n_aligned_groups, 9)
  expect_equal(out_drop$overview$n_unaligned_groups, 3)
  expect_false(any(out_drop$aligned_data$subject == "S4"))

  out_keep <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    alignment_event = "first_target_entry",
    keep_unaligned = TRUE,
    name = "toy_partial_keep"
  )

  expect_s3_class(out_keep, "gp3_fixation_aligned_data")
  expect_equal(out_keep$overview$alignment_status, "partial_complete")
  expect_equal(nrow(out_keep$aligned_data), nrow(toy_data))
  expect_true(any(out_keep$aligned_data$subject == "S4"))
  expect_true(all(is.na(out_keep$aligned_data$gp3_aligned_time[out_keep$aligned_data$subject == "S4"])))
  expect_true(all(out_keep$aligned_data$gp3_alignment_phase[out_keep$aligned_data$subject == "S4"] == "unaligned"))
})

test_that("prepare_gazepoint_fixation_aligned_data handles no alignment events", {
  toy_data <- make_test_fixalign_data() |>
    dplyr::mutate(aoi = "Background")

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    participant_col = "subject",
    trial_col = "trial",
    aoi_col = "aoi",
    target_aoi = "Target",
    alignment_event = "first_target_entry",
    keep_unaligned = TRUE,
    name = "toy_no_alignment"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$alignment_status, "no_alignment_events")
  expect_equal(out$overview$n_aligned_groups, 0)
  expect_equal(out$overview$n_unaligned_groups, 12)
  expect_true(all(!out$event_table$gp3_has_alignment_event))
  expect_true(all(out$aligned_data$gp3_alignment_phase == "unaligned"))
})

test_that("prepare_gazepoint_fixation_aligned_data works without participant and trial columns", {
  toy_data <- tibble::tibble(
    time = seq(0, 400, by = 100),
    aoi = c("Background", "Background", "Target", "Target", "Target")
  )

  out <- prepare_gazepoint_fixation_aligned_data(
    toy_data,
    time_col = "time",
    aoi_col = "aoi",
    target_aoi = "Target",
    alignment_event = "first_target_entry",
    name = "single_series_alignment"
  )

  expect_s3_class(out, "gp3_fixation_aligned_data")
  expect_equal(out$overview$n_groups, 1)
  expect_equal(out$overview$n_aligned_groups, 1)
  expect_equal(out$event_table$gp3_alignment_time, 200)
  expect_equal(out$aligned_data$gp3_aligned_time, toy_data$time - 200)
})

test_that("prepare_gazepoint_fixation_aligned_data checks invalid inputs", {
  toy_data <- make_test_fixalign_data()

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      list(),
      time_col = "time",
      alignment_event = "first_fixation"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data[0, ],
      time_col = "time",
      alignment_event = "first_fixation"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "bad_time",
      alignment_event = "first_target_entry"
    ),
    "`time_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      participant_col = "bad_subject",
      alignment_event = "first_target_entry"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      alignment_event = "first_target_entry"
    ),
    "`aoi_col` is required for target-AOI alignment events",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      aoi_col = "aoi",
      alignment_event = "first_target_entry"
    ),
    "`target_aoi` must be a non-empty character vector for target-AOI alignment events",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      aoi_col = "aoi",
      target_aoi = "",
      alignment_event = "first_target_entry"
    ),
    "`target_aoi` must contain at least one non-empty value",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      alignment_event = "first_fixation"
    ),
    "`fixation_col` is required when `alignment_event = 'first_fixation'`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      aoi_col = "aoi",
      target_aoi = "Target",
      alignment_event = "first_fixation_to_target"
    ),
    "`fixation_col` is required when `alignment_event = 'first_fixation_to_target'`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      aoi_col = "aoi",
      target_aoi = "Target",
      alignment_event = "first_saccade_to_aoi"
    ),
    "`saccade_col` is required when `alignment_event = 'first_saccade_to_aoi'`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      alignment_event = "custom"
    ),
    "`event_col` is required when `alignment_event = 'custom'`",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      fixation_col = "fixation",
      alignment_event = "first_fixation",
      baseline_window = c(0, -100)
    ),
    "`baseline_window` lower bound must be less than or equal to its upper bound",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      fixation_col = "fixation",
      alignment_event = "first_fixation",
      analysis_window = c(0, NA_real_)
    ),
    "`analysis_window` must be NULL or a finite numeric vector of length two",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      fixation_col = "fixation",
      alignment_event = "first_fixation",
      keep_unaligned = NA
    ),
    "`keep_unaligned` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      toy_data,
      time_col = "time",
      fixation_col = "fixation",
      alignment_event = "first_fixation",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )

  bad_time <- toy_data |>
    dplyr::mutate(time = "not_numeric")

  expect_error(
    prepare_gazepoint_fixation_aligned_data(
      bad_time,
      time_col = "time",
      fixation_col = "fixation",
      alignment_event = "first_fixation"
    ),
    "`time_col` must be numeric or coercible to finite numeric values",
    fixed = TRUE
  )
})
