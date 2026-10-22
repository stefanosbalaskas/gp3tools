testthat::test_that("synthetic AOI perturbation reports stable reference and monotone assignment", {
  script <- system.file("research", "october_2026_aoi_boundary_sensitivity.R",
                        package = "gp3tools")
  testthat::expect_true(nzchar(script))
  e <- new.env(parent = baseenv())
  sys.source(script, envir = e)
  x <- e$gp3tools_october_aoi_points()
  audit <- e$gp3tools_october_aoi_sensitivity(x)
  testthat::expect_equal(nrow(x), 288L)
  testthat::expect_true(all(diff(audit$participant_equal_fraction) >= -1e-12))
  testthat::expect_equal(audit$changed_fraction[audit$boundary == 50], 0)
  testthat::expect_true(all(audit$changed_fraction >= 0 &
                              audit$changed_fraction <= 1))
  testthat::expect_identical(x, e$gp3tools_october_aoi_points())
  testthat::expect_error(e$gp3tools_october_aoi_sensitivity(
    data.frame(participant = 1:3, x = c(1, NA, 3), y = 1:3)))
})
