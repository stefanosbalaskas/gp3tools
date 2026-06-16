test_that("create_gazepoint_analysis_decision_audit creates a complete audit object", {
  branch_roles <- tibble::tibble(
    branch_name = c(
      "aoi_glmm",
      "aoi_sensitivity",
      "cluster_test",
      "model_summary"
    ),
    decision_type = c(
      "confirmatory",
      "sensitivity",
      "exploratory",
      "reporting"
    ),
    analysis_family = c(
      "aoi_window_glmm",
      "aoi_model_sensitivity",
      "cluster_permutation",
      "model_tables"
    )
  )

  out <- create_gazepoint_analysis_decision_audit(
    aoi_glmm = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars),
      diagnostics = list(
        overview = tibble::tibble(
          diagnostic_status = "ok",
          message = "No diagnostic issue."
        )
      )
    ),
    aoi_sensitivity = list(
      sensitivity_status = "ok"
    ),
    cluster_test = list(
      cluster_status = "ok"
    ),
    model_summary = list(
      summary_status = "ok"
    ),
    branch_roles = branch_roles,
    required_confirmatory = "aoi_glmm"
  )

  expect_s3_class(out, "gp3_analysis_decision_audit")
  expect_type(out, "list")

  expect_named(
    out,
    c(
      "overview",
      "branch_audit",
      "diagnostics_summary",
      "interpretation_cautions",
      "readiness",
      "settings"
    )
  )

  expect_s3_class(out$overview, "tbl_df")
  expect_s3_class(out$branch_audit, "tbl_df")
  expect_s3_class(out$diagnostics_summary, "tbl_df")
  expect_s3_class(out$interpretation_cautions, "tbl_df")
  expect_s3_class(out$readiness, "tbl_df")
  expect_s3_class(out$settings, "tbl_df")

  expect_equal(out$overview$n_branches, 4)
  expect_equal(out$overview$n_confirmatory, 1)
  expect_equal(out$overview$n_sensitivity, 1)
  expect_equal(out$overview$n_exploratory, 1)
  expect_equal(out$overview$n_reporting, 1)

  expect_equal(out$readiness$readiness_status, "ready_with_cautions")

  expect_true("exploratory_not_confirmatory" %in% out$interpretation_cautions$caution_type)
  expect_true("sensitivity_not_primary" %in% out$interpretation_cautions$caution_type)

  expect_true("aoi_glmm" %in% out$branch_audit$branch_name)
  expect_true(out$branch_audit$has_model[out$branch_audit$branch_name == "aoi_glmm"])
  expect_true(out$branch_audit$has_diagnostics[out$branch_audit$branch_name == "aoi_glmm"])

  expect_equal(
    out$diagnostics_summary$diagnostic_status[
      out$diagnostics_summary$branch_name == "aoi_glmm"
    ],
    "ok"
  )
})

test_that("create_gazepoint_analysis_decision_audit accepts a results list", {
  results <- list(
    main_model = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars),
      diagnostics = list(
        overview = tibble::tibble(
          diagnostic_status = "ok",
          message = "No diagnostic issue."
        )
      )
    )
  )

  branch_roles <- tibble::tibble(
    branch_name = "main_model",
    decision_type = "confirmatory"
  )

  out <- create_gazepoint_analysis_decision_audit(
    results = results,
    branch_roles = branch_roles,
    required_confirmatory = "main_model"
  )

  expect_s3_class(out, "gp3_analysis_decision_audit")
  expect_equal(out$overview$n_branches, 1)
  expect_equal(out$readiness$readiness_status, "ready")
})

test_that("create_gazepoint_analysis_decision_audit flags missing required confirmatory branches", {
  branch_roles <- tibble::tibble(
    branch_name = "sensitivity_model",
    decision_type = "sensitivity"
  )

  out <- create_gazepoint_analysis_decision_audit(
    sensitivity_model = list(
      sensitivity_status = "ok"
    ),
    branch_roles = branch_roles,
    required_confirmatory = "main_confirmatory_model"
  )

  expect_equal(out$readiness$readiness_status, "not_ready")

  expect_true(
    "missing_required_confirmatory_branch" %in%
      out$interpretation_cautions$caution_type
  )

  expect_equal(out$readiness$n_missing_required_confirmatory, 1)
})

test_that("create_gazepoint_analysis_decision_audit flags confirmatory models without diagnostics", {
  branch_roles <- tibble::tibble(
    branch_name = "main_model",
    decision_type = "confirmatory"
  )

  out <- create_gazepoint_analysis_decision_audit(
    main_model = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars)
    ),
    branch_roles = branch_roles,
    required_confirmatory = "main_model",
    diagnostics_required = TRUE
  )

  expect_equal(out$readiness$readiness_status, "ready_with_cautions")

  expect_true(
    "confirmatory_model_without_diagnostics" %in%
      out$interpretation_cautions$caution_type
  )
})

test_that("create_gazepoint_analysis_decision_audit handles diagnostic warnings", {
  branch_roles <- tibble::tibble(
    branch_name = "main_model",
    decision_type = "confirmatory"
  )

  out <- create_gazepoint_analysis_decision_audit(
    main_model = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars),
      diagnostics = list(
        overview = tibble::tibble(
          diagnostic_status = "diagnostic_warning",
          message = "Diagnostic warning."
        )
      )
    ),
    branch_roles = branch_roles,
    required_confirmatory = "main_model",
    require_clean_diagnostics = FALSE
  )

  expect_equal(out$diagnostics_summary$diagnostic_status, "diagnostic_warning")
  expect_equal(out$readiness$readiness_status, "ready_with_cautions")

  expect_true(
    "diagnostic_warning" %in%
      out$interpretation_cautions$caution_type
  )
})

test_that("create_gazepoint_analysis_decision_audit can require clean diagnostics", {
  branch_roles <- tibble::tibble(
    branch_name = "main_model",
    decision_type = "confirmatory"
  )

  out <- create_gazepoint_analysis_decision_audit(
    main_model = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars),
      diagnostics = list(
        overview = tibble::tibble(
          diagnostic_status = "diagnostic_warning",
          message = "Diagnostic warning."
        )
      )
    ),
    branch_roles = branch_roles,
    required_confirmatory = "main_model",
    require_clean_diagnostics = TRUE
  )

  expect_equal(out$readiness$readiness_status, "not_ready")
  expect_equal(out$readiness$n_high_cautions, 1)
})

test_that("create_gazepoint_analysis_decision_audit records fallback and singular cautions", {
  branch_roles <- tibble::tibble(
    branch_name = "main_model",
    decision_type = "confirmatory"
  )

  out <- create_gazepoint_analysis_decision_audit(
    main_model = list(
      model_status = "ok",
      model = stats::lm(mpg ~ wt, data = mtcars),
      fallback_used = TRUE,
      singular_fit = TRUE,
      diagnostics = list(
        overview = tibble::tibble(
          diagnostic_status = "ok",
          message = "No diagnostic issue."
        )
      )
    ),
    branch_roles = branch_roles,
    required_confirmatory = "main_model"
  )

  expect_true("fallback_model_used" %in% out$interpretation_cautions$caution_type)
  expect_true("singular_fit" %in% out$interpretation_cautions$caution_type)
  expect_equal(out$readiness$readiness_status, "ready_with_cautions")
})

test_that("create_gazepoint_analysis_decision_audit checks invalid inputs", {
  expect_error(
    create_gazepoint_analysis_decision_audit(),
    "At least one named analysis result must be supplied",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      list(model_status = "ok")
    ),
    "All analysis result objects must be named",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      results = data.frame(x = 1)
    ),
    "`results` must be a named list when supplied",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      main_model = list(model_status = "ok"),
      branch_roles = list()
    ),
    "`branch_roles` must be a data frame when supplied",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      main_model = list(model_status = "ok"),
      branch_roles = tibble::tibble(branch_name = "main_model")
    ),
    "`branch_roles` is missing required column",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      main_model = list(model_status = "ok"),
      branch_roles = tibble::tibble(
        branch_name = "main_model",
        decision_type = "primary"
      )
    ),
    "`branch_roles$decision_type` contains unsupported value",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      main_model = list(model_status = "ok"),
      diagnostics_required = NA
    ),
    "`diagnostics_required` must be TRUE or FALSE",
    fixed = TRUE
  )

  expect_error(
    create_gazepoint_analysis_decision_audit(
      main_model = list(model_status = "ok"),
      require_clean_diagnostics = NA
    ),
    "`require_clean_diagnostics` must be TRUE or FALSE",
    fixed = TRUE
  )
})
