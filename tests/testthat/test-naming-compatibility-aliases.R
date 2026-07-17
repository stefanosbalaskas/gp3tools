test_that("generated British naming aliases are exported", {
  exports <- getNamespaceExports("gp3tools")
  expected <- c("summarise_gazepoint_coordinate_coverage", "summarise_gazepoint_face_reactivity", "summarise_gazepoint_face_windows", "summarise_gazepoint_pupil_response_features", "summarise_gazepoint_time_clusters")
  expect_true(all(expected %in% exports))
})

