test_that("pupil latency resolution audit is public and reports estimator sensitivity", {
  expect_true("audit_gp3_pupil_latency_resolution" %in% getNamespaceExports("gp3tools"))

  time <- seq(-0.5, 2, by = 0.01)
  pupil <- ifelse(
    time < 0.30,
    4,
    4 - 0.8 * (1 - exp(-(time - 0.30) / 0.20))
  )

  out <- gp3tools::audit_gp3_pupil_latency_resolution(time, pupil)

  expect_s3_class(out, "gp3_pupil_latency_resolution")
  expect_named(
    out$estimates_s,
    c("sustained_threshold", "max_slope_tangent")
  )
  expect_true(out$latency_resolvability %in% c("high", "moderate", "low"))
  expect_equal(out$sampling_hz, 100, tolerance = 1e-6)
  expect_equal(out$sampling_resolution_ms, 10, tolerance = 1e-6)
  expect_match(out$reporting_note, "estimator choice/spread")
  expect_match(out$reporting_note, "hardware- or algorithm-independent")
})

test_that("pupil latency audit exposes estimator spread without forcing agreement", {
  time <- seq(-0.5, 2, by = 0.02)
  pupil <- 4 - ifelse(time > 0.35, 0.6 * (1 - exp(-(time - 0.35) / 0.18)), 0)

  out <- gp3tools::audit_gp3_pupil_latency_resolution(time, pupil)

  expect_equal(out$sampling_hz, 50, tolerance = 1e-6)
  expect_equal(out$sampling_resolution_ms, 20, tolerance = 1e-6)
  expect_true(is.numeric(out$estimator_spread_ms))
  expect_length(out$estimates_s, 2)
})
