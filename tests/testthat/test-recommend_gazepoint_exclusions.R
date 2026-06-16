make_test_exclusion_data <- function() {
  tidyr::expand_grid(
    participant = paste0("S", 1:3),
    trial = paste0("T", 1:4),
    sample = 1:10
  ) |>
    dplyr::mutate(
      condition = dplyr::if_else(.data$trial %in% c("T1", "T2"), "A", "B"),
      valid = TRUE,
      gaze_x = 500,
      gaze_y = 400,
      pupil = 3,
      artifact = FALSE
    ) |>
    dplyr::mutate(
      valid = dplyr::if_else(
        .data$participant == "S2" & .data$trial == "T2" & .data$sample <= 8,
        FALSE,
        .data$valid
      ),
      pupil = dplyr::if_else(
        .data$participant == "S3",
        NA_real_,
        .data$pupil
      )
    )
}

test_that("recommend_gazepoint_exclusions recommends trial and participant exclusions", {
  toy_data <- make_test_exclusion_data()

  out <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    condition_col = "condition",
    validity_col = "valid",
    x_col = "gaze_x",
    y_col = "gaze_y",
    pupil_col = "pupil",
    artifact_col = "artifact",
    min_trial_samples = 8,
    max_trial_missing_prop = 0.50,
    max_trial_artifact_prop = 0.50,
    min_participant_trials = 3,
    min_participant_valid_trials = 2,
    max_participant_missing_prop = 0.60,
    max_participant_artifact_prop = 0.50,
    name = "toy_exclusion_recommendations"
  )

  expect_s3_class(out, "gp3_exclusion_recommendations")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "participant_recommendations",
      "trial_recommendations",
      "exclusion_table",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$participant_recommendations, "tbl_df")
  expect_s3_class(out$trial_recommendations, "tbl_df")
  expect_s3_class(out$exclusion_table, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_exclusion_recommendations")
  expect_equal(out$overview$recommendation_status, "complete")
  expect_equal(out$overview$participant_col, "participant")
  expect_equal(out$overview$trial_col, "trial")
  expect_equal(out$overview$condition_col, "condition")
  expect_equal(out$overview$validity_col, "valid")
  expect_equal(out$overview$x_col, "gaze_x")
  expect_equal(out$overview$y_col, "gaze_y")
  expect_equal(out$overview$pupil_col, "pupil")
  expect_equal(out$overview$artifact_col, "artifact")
  expect_equal(out$overview$n_input_rows, nrow(toy_data))
  expect_equal(out$overview$n_participants, 3)
  expect_equal(out$overview$n_trials, 12)
  expect_equal(out$overview$n_recommended_participant_exclusions, 1)
  expect_equal(out$overview$n_recommended_trial_exclusions, 5)

  s2_t2 <- out$trial_recommendations |>
    dplyr::filter(.data$participant == "S2", .data$trial == "T2")

  expect_equal(nrow(s2_t2), 1)
  expect_true(s2_t2$recommend_exclude)
  expect_equal(s2_t2$recommendation_status, "exclude")
  expect_equal(s2_t2$n_missing_or_unusable, 8)
  expect_equal(s2_t2$missing_or_unusable_prop, 0.8)
  expect_match(s2_t2$exclusion_reason, "high_trial_missingness", fixed = TRUE)

  s3_trials <- out$trial_recommendations |>
    dplyr::filter(.data$participant == "S3")

  expect_equal(nrow(s3_trials), 4)
  expect_true(all(s3_trials$recommend_exclude))
  expect_true(all(s3_trials$missing_or_unusable_prop == 1))
  expect_true(all(grepl("high_trial_missingness", s3_trials$exclusion_reason)))

  s3_participant <- out$participant_recommendations |>
    dplyr::filter(.data$participant == "S3")

  expect_equal(nrow(s3_participant), 1)
  expect_true(s3_participant$recommend_exclude)
  expect_equal(s3_participant$recommendation_status, "exclude")
  expect_equal(s3_participant$n_retained_trials, 0)
  expect_match(s3_participant$exclusion_reason, "too_few_retained_trials", fixed = TRUE)
  expect_match(s3_participant$exclusion_reason, "high_participant_missingness", fixed = TRUE)

  s1_participant <- out$participant_recommendations |>
    dplyr::filter(.data$participant == "S1")

  expect_false(s1_participant$recommend_exclude)
  expect_equal(s1_participant$recommendation_status, "retain")
  expect_equal(s1_participant$exclusion_reason, "")

  expect_true(all(c(
    "exclusion_level",
    "participant",
    "trial",
    "condition",
    "n_samples",
    "n_trials",
    "n_retained_trials",
    "missing_or_unusable_prop",
    "artifact_prop",
    "recommend_exclude",
    "recommendation_status",
    "exclusion_reason"
  ) %in% names(out$exclusion_table)))

  expect_equal(
    out$settings$value[out$settings$setting == "name"],
    "toy_exclusion_recommendations"
  )
})

test_that("recommend_gazepoint_exclusions handles artifact-based exclusions", {
  toy_data <- tidyr::expand_grid(
    participant = "S1",
    trial = c("T1", "T2"),
    sample = 1:10
  ) |>
    dplyr::mutate(
      valid = TRUE,
      pupil = 3,
      artifact = .data$trial == "T2" & .data$sample <= 7
    )

  out <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    validity_col = "valid",
    pupil_col = "pupil",
    artifact_col = "artifact",
    min_trial_samples = 5,
    max_trial_missing_prop = 0.50,
    max_trial_artifact_prop = 0.50,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    max_participant_missing_prop = 0.90,
    max_participant_artifact_prop = 0.90,
    name = "artifact_exclusion"
  )

  expect_s3_class(out, "gp3_exclusion_recommendations")

  t2 <- out$trial_recommendations |>
    dplyr::filter(.data$trial == "T2")

  expect_true(t2$recommend_exclude)
  expect_equal(t2$artifact_prop, 0.7)
  expect_match(t2$exclusion_reason, "high_trial_artifact_rate", fixed = TRUE)

  expect_equal(out$overview$n_recommended_trial_exclusions, 1)
  expect_equal(out$overview$n_recommended_participant_exclusions, 0)
})

test_that("recommend_gazepoint_exclusions handles too-few-sample trials", {
  toy_data <- tibble::tibble(
    participant = c(rep("S1", 10), rep("S1", 4)),
    trial = c(rep("T1", 10), rep("T2", 4)),
    valid = TRUE,
    pupil = 3
  )

  out <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    validity_col = "valid",
    pupil_col = "pupil",
    min_trial_samples = 8,
    max_trial_missing_prop = 0.50,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    name = "few_samples"
  )

  expect_s3_class(out, "gp3_exclusion_recommendations")

  t2 <- out$trial_recommendations |>
    dplyr::filter(.data$trial == "T2")

  expect_true(t2$recommend_exclude)
  expect_equal(t2$n_samples, 4)
  expect_match(t2$exclusion_reason, "too_few_trial_samples", fixed = TRUE)
})

test_that("recommend_gazepoint_exclusions supports numeric and character quality flags", {
  toy_data <- tibble::tibble(
    participant = rep("S1", 6),
    trial = rep("T1", 6),
    validity_numeric = c(1, 1, 1, 0, 0, 0),
    artifact_character = c("no", "no", "no", "yes", "yes", "yes"),
    pupil = 3
  )

  out <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    validity_col = "validity_numeric",
    pupil_col = "pupil",
    artifact_col = "artifact_character",
    min_trial_samples = 5,
    max_trial_missing_prop = 0.40,
    max_trial_artifact_prop = 0.40,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    max_participant_missing_prop = 0.90,
    max_participant_artifact_prop = 0.90,
    name = "numeric_character_flags"
  )

  expect_s3_class(out, "gp3_exclusion_recommendations")
  expect_true(out$trial_recommendations$recommend_exclude)
  expect_equal(out$trial_recommendations$missing_or_unusable_prop, 0.5)
  expect_equal(out$trial_recommendations$artifact_prop, 0.5)
  expect_match(out$trial_recommendations$exclusion_reason, "high_trial_missingness", fixed = TRUE)
  expect_match(out$trial_recommendations$exclusion_reason, "high_trial_artifact_rate", fixed = TRUE)
})

test_that("recommend_gazepoint_exclusions supports gaze-coordinate missingness rules", {
  toy_data <- tibble::tibble(
    participant = rep("S1", 6),
    trial = rep("T1", 6),
    gaze_x = c(1, 2, NA, 4, NA, 6),
    gaze_y = c(1, 2, 3, NA, NA, 6)
  )

  out_both <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    x_col = "gaze_x",
    y_col = "gaze_y",
    require_both_gaze_coordinates = TRUE,
    min_trial_samples = 5,
    max_trial_missing_prop = 0.40,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    name = "both_coordinates_required"
  )

  expect_s3_class(out_both, "gp3_exclusion_recommendations")
  expect_equal(out_both$trial_recommendations$missing_or_unusable_prop, 3 / 6)
  expect_true(out_both$trial_recommendations$recommend_exclude)

  out_either <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    trial_col = "trial",
    x_col = "gaze_x",
    y_col = "gaze_y",
    require_both_gaze_coordinates = FALSE,
    min_trial_samples = 5,
    max_trial_missing_prop = 0.40,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    name = "either_coordinate_allowed"
  )

  expect_s3_class(out_either, "gp3_exclusion_recommendations")
  expect_equal(out_either$trial_recommendations$missing_or_unusable_prop, 1 / 6)
  expect_false(out_either$trial_recommendations$recommend_exclude)
})

test_that("recommend_gazepoint_exclusions works without trial or condition columns", {
  toy_data <- tibble::tibble(
    participant = rep(c("S1", "S2"), each = 10),
    valid = c(rep(TRUE, 10), rep(FALSE, 10)),
    pupil = c(rep(3, 10), rep(NA_real_, 10))
  )

  out <- recommend_gazepoint_exclusions(
    toy_data,
    participant_col = "participant",
    validity_col = "valid",
    pupil_col = "pupil",
    min_trial_samples = 5,
    max_trial_missing_prop = 0.50,
    min_participant_trials = 1,
    min_participant_valid_trials = 1,
    max_participant_missing_prop = 0.50,
    name = "no_trial_column"
  )

  expect_s3_class(out, "gp3_exclusion_recommendations")
  expect_equal(out$overview$trial_col, NA_character_)
  expect_equal(out$overview$condition_col, NA_character_)
  expect_equal(out$overview$n_trials, 2)

  expect_true(all(is.na(out$exclusion_table$trial[out$exclusion_table$exclusion_level == "trial"])))

  s2 <- out$participant_recommendations |>
    dplyr::filter(.data$participant == "S2")

  expect_true(s2$recommend_exclude)
  expect_match(s2$exclusion_reason, "high_participant_missingness", fixed = TRUE)
})

test_that("recommend_gazepoint_exclusions checks invalid inputs", {
  toy_data <- make_test_exclusion_data()

  expect_error(
    recommend_gazepoint_exclusions(
      data = list(),
      participant_col = "participant",
      validity_col = "valid"
    ),
    "`data` must be a data frame",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data[0, ],
      participant_col = "participant",
      validity_col = "valid"
    ),
    "`data` must contain at least one row",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "bad_participant",
      validity_col = "valid"
    ),
    "`participant_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      trial_col = "bad_trial",
      validity_col = "valid"
    ),
    "`trial_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      condition_col = "bad_condition",
      validity_col = "valid"
    ),
    "`condition_col` must be present in `data`",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant"
    ),
    "Supply at least one quality indicator",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      x_col = "gaze_x",
      require_both_gaze_coordinates = TRUE
    ),
    "When `require_both_gaze_coordinates = TRUE`, supply both `x_col` and `y_col` or neither",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      min_trial_samples = 0
    ),
    "`min_trial_samples` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      max_trial_missing_prop = 1.1
    ),
    "`max_trial_missing_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      max_trial_artifact_prop = -0.1
    ),
    "`max_trial_artifact_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      min_participant_trials = 0
    ),
    "`min_participant_trials` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      min_participant_valid_trials = 0
    ),
    "`min_participant_valid_trials` must be a positive integer",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      max_participant_missing_prop = 2
    ),
    "`max_participant_missing_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      max_participant_artifact_prop = NA
    ),
    "`max_participant_artifact_prop` must be a finite number between 0 and 1",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      require_both_gaze_coordinates = NA
    ),
    "`require_both_gaze_coordinates` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    recommend_gazepoint_exclusions(
      data = toy_data,
      participant_col = "participant",
      validity_col = "valid",
      name = ""
    ),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )
})
