test_that("collect_gazepoint_qc_summaries collects overview tables", {
  pass_object <- list(
    overview = data.frame(
      audit_status = "ok",
      message = "Audit passed.",
      stringsAsFactors = FALSE
    )
  )

  warn_object <- list(
    overview = data.frame(
      audit_status = "review",
      message = "Coverage should be reviewed.",
      stringsAsFactors = FALSE
    )
  )

  out <- collect_gazepoint_qc_summaries(
    list(pass = pass_object, warn = warn_object)
  )

  expect_s3_class(out, "gp3_qc_summary_bundle")
  expect_s3_class(out$overview, "data.frame")
  expect_s3_class(out$object_summary, "data.frame")
  expect_s3_class(out$status_counts, "data.frame")
  expect_s3_class(out$overview_rows, "data.frame")

  expect_equal(nrow(out$object_summary), 2)
  expect_true(all(c("pass", "warn") %in% out$object_summary$qc_status))
  expect_equal(out$overview$qc_bundle_status, "warn")
})


test_that("collect_gazepoint_qc_summaries accepts single overview data frame", {
  overview <- data.frame(
    readiness_status = "ready",
    decision_message = "Ready for review.",
    stringsAsFactors = FALSE
  )

  out <- collect_gazepoint_qc_summaries(overview)

  expect_s3_class(out, "gp3_qc_summary_bundle")
  expect_equal(nrow(out$object_summary), 1)
  expect_equal(out$object_summary$qc_status, "pass")
})


test_that("collect_gazepoint_qc_summaries marks missing overviews as unknown", {
  out <- collect_gazepoint_qc_summaries(
    list(raw_vector = 1:3)
  )

  expect_equal(out$object_summary$qc_status, "unknown")
  expect_false(out$object_summary$overview_available)
  expect_match(out$object_summary$qc_message, "no interpretable overview")
})


test_that("collect_gazepoint_qc_summaries validates inputs", {
  expect_error(
    collect_gazepoint_qc_summaries(NULL),
    "at least one object"
  )

  expect_error(
    collect_gazepoint_qc_summaries(list(a = 1), object_names = c("a", "b")),
    "one name per object"
  )

  expect_error(
    collect_gazepoint_qc_summaries(list(a = 1), include_overview_rows = NA),
    "TRUE or FALSE"
  )
})


test_that("summarize_gazepoint_qc_status summarizes bundle status", {
  bundle <- collect_gazepoint_qc_summaries(
    list(
      pass = list(overview = data.frame(audit_status = "ok")),
      fail = list(overview = data.frame(audit_status = "failed")),
      info = list(overview = data.frame(audit_status = "not_run"))
    )
  )

  out <- summarize_gazepoint_qc_status(bundle)

  expect_s3_class(out, "gp3_qc_status_summary")
  expect_s3_class(out$overview, "data.frame")
  expect_equal(out$overview$n_objects, 3)
  expect_equal(out$overview$n_fail, 1)
  expect_equal(out$overview$qc_overview_status, "fail")
})


test_that("summarise_gazepoint_qc_status is an alias", {
  bundle <- collect_gazepoint_qc_summaries(
    list(pass = list(overview = data.frame(audit_status = "ok")))
  )

  us <- summarize_gazepoint_qc_status(bundle)
  uk <- summarise_gazepoint_qc_status(bundle)

  expect_equal(us, uk)
})


test_that("summarize_gazepoint_qc_status accepts object-summary input", {
  bundle <- collect_gazepoint_qc_summaries(
    list(pass = list(overview = data.frame(audit_status = "ok")))
  )

  out <- summarize_gazepoint_qc_status(bundle$object_summary)

  expect_s3_class(out, "gp3_qc_status_summary")
  expect_equal(out$overview$n_pass, 1)
})


test_that("report_gazepoint_qc_overview returns cautious text", {
  bundle <- collect_gazepoint_qc_summaries(
    list(
      pass = list(overview = data.frame(audit_status = "ok")),
      warn = list(overview = data.frame(audit_status = "review", message = "Inspect.")),
      unknown = 1:3
    )
  )

  out <- report_gazepoint_qc_overview(bundle, max_objects = 2)

  expect_s3_class(out, "gp3_qc_overview_report")
  expect_s3_class(out$summary, "gp3_qc_status_summary")
  expect_s3_class(out$object_summary, "data.frame")
  expect_match(out$report_text, "QC overview collected")
  expect_match(out$report_text, "reporting aid only")
})


test_that("report_gazepoint_qc_overview validates max_objects", {
  bundle <- collect_gazepoint_qc_summaries(
    list(pass = list(overview = data.frame(audit_status = "ok")))
  )

  expect_error(
    report_gazepoint_qc_overview(bundle, max_objects = 0),
    "positive integer"
  )
})
test_that("plot_gazepoint_qc_overview returns status-count plot", {
  bundle <- collect_gazepoint_qc_summaries(
    list(
      pass = list(overview = data.frame(audit_status = "ok")),
      warn = list(overview = data.frame(audit_status = "review"))
    )
  )

  p <- plot_gazepoint_qc_overview(
    bundle,
    plot_type = "status_counts",
    title = "QC overview"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_qc_overview returns object-level plot", {
  bundle <- collect_gazepoint_qc_summaries(
    list(
      pass = list(overview = data.frame(audit_status = "ok")),
      fail = list(overview = data.frame(audit_status = "failed")),
      unknown = 1:3
    )
  )

  p <- plot_gazepoint_qc_overview(
    bundle,
    plot_type = "objects"
  )

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_qc_overview accepts raw object lists", {
  objects <- list(
    pass = list(overview = data.frame(audit_status = "ok")),
    warn = list(overview = data.frame(audit_status = "review"))
  )

  p <- plot_gazepoint_qc_overview(objects)

  expect_s3_class(p, "ggplot")
})


test_that("plot_gazepoint_qc_overview validates plot type", {
  bundle <- collect_gazepoint_qc_summaries(
    list(pass = list(overview = data.frame(audit_status = "ok")))
  )

  expect_error(
    plot_gazepoint_qc_overview(bundle, plot_type = "bad"),
    "should be one of"
  )
})
