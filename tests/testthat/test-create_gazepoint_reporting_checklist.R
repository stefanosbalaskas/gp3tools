make_test_reporting_data <- function() {
  set.seed(123)

  tibble::tibble(
    subject = rep(paste0("S", 1:6), each = 20),
    trial = rep(rep(1:4, each = 5), times = 6),
    time = rep(seq(0, 400, by = 100), times = 24),
    condition = rep(c("A", "B"), each = 60),
    stimulus = rep(c("stim_1", "stim_2"), each = 60),
    aoi = rep(c("logo", "claim", "product", "none"), length.out = 120),
    pupil_clean = stats::rnorm(120, mean = 1000, sd = 40),
    gaze_x = stats::runif(120, 0, 1),
    gaze_y = stats::runif(120, 0, 1)
  )
}

make_mock_reporting_objects <- function() {
  real_gate <- list(
    overview = tibble::tibble(
      object_name = "mock_real_gate",
      readiness_status = "pass",
      ready_for_real_data_analysis = TRUE,
      message = NA_character_
    )
  )
  class(real_gate) <- c("gp3_real_data_readiness_gate", "list")

  tvtm <- list(
    overview = tibble::tibble(
      object_name = "mock_tvtm",
      status = "complete",
      message = NA_character_
    )
  )
  class(tvtm) <- c("gp3_time_varying_transition_matrix", "list")

  transition_nb <- list(
    overview = tibble::tibble(
      object_name = "mock_transition_nb",
      model_status = "complete",
      message = "Negative-binomial transition-count sensitivity model fitted."
    )
  )
  class(transition_nb) <- c("gp3_transition_count_nb_sensitivity", "list")

  eyetools_detection <- list(
    overview = tibble::tibble(
      object_name = "mock_eyetools",
      detector_status = "partial_complete",
      message = "eyetools detector branch statuses: fixation_dispersion_complete, error_fixation_vti"
    )
  )
  class(eyetools_detection) <- c("gp3_eyetools_fixation_detection", "list")

  list(
    real_gate = real_gate,
    tvtm = tvtm,
    transition_nb = transition_nb,
    eyetools_detection = eyetools_detection
  )
}

test_that("create_gazepoint_reporting_checklist creates a complete checklist object", {
  toy_data <- make_test_reporting_data()
  mock_objects <- make_mock_reporting_objects()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    objects = mock_objects,
    analysis_type = "combined",
    study_title = "Toy reporting study",
    name = "toy_reporting_checklist"
  )

  expect_s3_class(out, "gp3_reporting_checklist")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "checklist",
      "section_summary",
      "object_summary",
      "data_summary",
      "text_summary",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$checklist, "tbl_df")
  expect_s3_class(out$section_summary, "tbl_df")
  expect_s3_class(out$object_summary, "tbl_df")
  expect_s3_class(out$data_summary, "tbl_df")
  expect_s3_class(out$text_summary, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$object_name, "toy_reporting_checklist")
  expect_equal(out$overview$study_title, "Toy reporting study")
  expect_equal(out$overview$analysis_type, "combined")
  expect_equal(out$overview$checklist_status, "warn")
  expect_true(out$overview$ready_for_reporting)
  expect_equal(out$overview$n_fail, 0)
  expect_true(out$overview$n_warn >= 1)
  expect_true(out$overview$n_items >= 20)
  expect_equal(out$overview$n_objects_supplied, 4)

  expect_true(all(out$checklist$status %in% c("pass", "warn", "fail", "info")))
  expect_true(all(c("reporting_area", "item_id", "item", "status", "evidence", "recommendation", "required") %in% names(out$checklist)))
  expect_true(any(out$checklist$item_id == "real_data_readiness_gate"))
  expect_true(any(out$checklist$item_id == "sensitivity_analyses_reported"))
  expect_true(any(out$checklist$item_id == "external_detector_or_adapter_reporting"))
})

test_that("create_gazepoint_reporting_checklist summarises supplied objects", {
  toy_data <- make_test_reporting_data()
  mock_objects <- make_mock_reporting_objects()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    objects = mock_objects,
    analysis_type = "combined",
    study_title = "Toy reporting study"
  )

  expect_equal(nrow(out$object_summary), 4)
  expect_true(all(c("real_gate", "tvtm", "transition_nb", "eyetools_detection") %in% out$object_summary$object_label))

  expect_equal(
    out$object_summary$status[out$object_summary$object_label == "real_gate"],
    "pass"
  )

  expect_equal(
    out$object_summary$status[out$object_summary$object_label == "eyetools_detection"],
    "warn"
  )

  readiness_item <- out$checklist[out$checklist$item_id == "real_data_readiness_gate", ]

  expect_equal(readiness_item$status, "pass")
  expect_match(readiness_item$evidence, "gp3_real_data_readiness_gate", fixed = TRUE)

  sensitivity_item <- out$checklist[out$checklist$item_id == "sensitivity_analyses_reported", ]

  expect_equal(sensitivity_item$status, "warn")
  expect_match(sensitivity_item$evidence, "transition_nb", fixed = TRUE)
})

test_that("create_gazepoint_reporting_checklist summarises data structure", {
  toy_data <- make_test_reporting_data()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "combined",
    study_title = "Toy reporting study"
  )

  expect_equal(out$data_summary$n_rows, nrow(toy_data))
  expect_equal(out$data_summary$n_columns, ncol(toy_data))
  expect_equal(out$data_summary$participant_col, "subject")
  expect_equal(out$data_summary$trial_col, "trial")
  expect_equal(out$data_summary$time_col, "time")
  expect_equal(out$data_summary$condition_col, "condition")
  expect_equal(out$data_summary$pupil_col, "pupil_clean")
  expect_equal(out$data_summary$aoi_col, "aoi")
  expect_equal(out$data_summary$n_participants, 6)
  expect_equal(out$data_summary$n_trial_units, 24)

  participant_item <- out$checklist[out$checklist$item_id == "participant_trial_structure", ]

  expect_equal(participant_item$status, "pass")
  expect_match(participant_item$evidence, "Participants: 6", fixed = TRUE)
})

test_that("create_gazepoint_reporting_checklist works without data or objects", {
  out <- create_gazepoint_reporting_checklist(
    analysis_type = "general",
    name = "empty_reporting_checklist"
  )

  expect_s3_class(out, "gp3_reporting_checklist")
  expect_equal(out$overview$object_name, "empty_reporting_checklist")
  expect_equal(out$overview$analysis_type, "general")
  expect_equal(out$overview$checklist_status, "warn")
  expect_true(out$overview$ready_for_reporting)
  expect_equal(out$overview$n_objects_supplied, 0)

  expect_true(is.na(out$data_summary$n_rows))
  expect_true(is.na(out$data_summary$n_columns))
  expect_true(is.na(out$data_summary$n_participants))
  expect_true(is.na(out$data_summary$n_trial_units))

  dataset_item <- out$checklist[out$checklist$item_id == "dataset_available", ]
  expect_equal(dataset_item$status, "warn")

  structure_item <- out$checklist[out$checklist$item_id == "participant_trial_structure", ]
  expect_equal(structure_item$status, "warn")
})

test_that("create_gazepoint_reporting_checklist supports data-frame objects", {
  toy_data <- make_test_reporting_data()

  object_df <- tibble::tibble(
    object_name = "mock_dataframe_audit",
    audit_status = "complete",
    message = "mock data-frame audit complete"
  )

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    objects = object_df,
    analysis_type = "general",
    study_title = "Data frame object study"
  )

  expect_s3_class(out, "gp3_reporting_checklist")
  expect_equal(nrow(out$object_summary), 1)
  expect_equal(out$object_summary$object_name, "mock_dataframe_audit")
  expect_equal(out$object_summary$status, "pass")
})

test_that("create_gazepoint_reporting_checklist supports required section overrides", {
  toy_data <- make_test_reporting_data()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "general",
    required_sections = c("sensitivity_analyses_reported", "advanced_sequence_or_transition_methods"),
    include_optional = TRUE
  )

  sensitivity_item <- out$checklist[out$checklist$item_id == "sensitivity_analyses_reported", ]
  sequence_item <- out$checklist[out$checklist$item_id == "advanced_sequence_or_transition_methods", ]

  expect_true(sensitivity_item$required)
  expect_true(sequence_item$required)

  expect_equal(
    out$settings$value[out$settings$setting == "required_sections"],
    "sensitivity_analyses_reported, advanced_sequence_or_transition_methods"
  )
})

test_that("create_gazepoint_reporting_checklist can omit optional advanced-method items", {
  toy_data <- make_test_reporting_data()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "general",
    include_optional = FALSE
  )

  expect_false(any(out$checklist$reporting_area == "advanced_optional_methods"))
  expect_equal(
    out$settings$value[out$settings$setting == "include_optional"],
    "FALSE"
  )
})

test_that("create_gazepoint_reporting_checklist includes AOI items only for AOI or combined analyses", {
  toy_data <- make_test_reporting_data()

  general_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "general"
  )

  pupil_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "pupil"
  )

  aoi_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "aoi"
  )

  combined_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "combined"
  )

  expect_false(any(general_out$checklist$reporting_area == "aoi_reporting"))
  expect_false(any(pupil_out$checklist$reporting_area == "aoi_reporting"))
  expect_true(any(aoi_out$checklist$reporting_area == "aoi_reporting"))
  expect_true(any(combined_out$checklist$reporting_area == "aoi_reporting"))
})

test_that("create_gazepoint_reporting_checklist includes pupil items only for pupil or combined analyses", {
  toy_data <- make_test_reporting_data()

  general_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "general"
  )

  aoi_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "aoi"
  )

  pupil_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "pupil"
  )

  combined_out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "combined"
  )

  expect_false(any(general_out$checklist$reporting_area == "pupil_reporting"))
  expect_false(any(aoi_out$checklist$reporting_area == "pupil_reporting"))
  expect_true(any(pupil_out$checklist$reporting_area == "pupil_reporting"))
  expect_true(any(combined_out$checklist$reporting_area == "pupil_reporting"))
})

test_that("create_gazepoint_reporting_checklist detects failing supplied objects", {
  toy_data <- make_test_reporting_data()

  failing_object <- list(
    overview = tibble::tibble(
      object_name = "mock_failing_audit",
      audit_status = "fail",
      message = "mock failure"
    )
  )
  class(failing_object) <- c("gp3_real_data_readiness_gate", "list")

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    objects = list(failing_object = failing_object),
    analysis_type = "general",
    study_title = "Failing object study"
  )

  expect_s3_class(out, "gp3_reporting_checklist")
  expect_equal(out$object_summary$status, "fail")

  readiness_item <- out$checklist[out$checklist$item_id == "real_data_readiness_gate", ]

  expect_equal(readiness_item$status, "fail")
  expect_equal(out$overview$checklist_status, "fail")
  expect_false(out$overview$ready_for_reporting)
  expect_true(out$overview$n_fail >= 1)
})

test_that("create_gazepoint_reporting_checklist produces section and text summaries", {
  toy_data <- make_test_reporting_data()

  out <- create_gazepoint_reporting_checklist(
    data = toy_data,
    analysis_type = "combined",
    study_title = "Text summary study"
  )

  expect_true(all(c("reporting_area", "n_items", "n_required", "n_fail", "n_warn", "n_pass", "n_info", "area_status") %in% names(out$section_summary)))
  expect_true(all(out$section_summary$area_status %in% c("pass", "warn", "fail")))

  expect_equal(out$text_summary$study_title, "Text summary study")
  expect_equal(out$text_summary$analysis_type, "combined")
  expect_match(out$text_summary$text, "Text summary study", fixed = TRUE)
  expect_match(out$text_summary$text, "Gazepoint/gp3tools analysis", fixed = TRUE)
})

test_that("create_gazepoint_reporting_checklist checks invalid inputs", {
  toy_data <- make_test_reporting_data()

  expect_error(
    create_gazepoint_reporting_checklist(data = list()),
    "`data` must be NULL or a data frame",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_reporting_checklist(objects = "bad"),
    "`objects` must be NULL, a data frame, or a list",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_reporting_checklist(study_title = ""),
    "`study_title` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_reporting_checklist(required_sections = NA_character_),
    "`required_sections` must be NULL or a character vector",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_reporting_checklist(include_optional = NA),
    "`include_optional` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_reporting_checklist(name = ""),
    "`name` must be a non-missing character scalar",
    fixed = TRUE
  )

  expect_s3_class(
    create_gazepoint_reporting_checklist(
      data = toy_data,
      analysis_type = "general"
    ),
    "gp3_reporting_checklist"
  )
})
