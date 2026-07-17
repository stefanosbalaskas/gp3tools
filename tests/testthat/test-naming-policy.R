test_that("naming policy selects British English as canonical", {
  policy <- gp3tools_naming_policy()

  expect_s3_class(policy, "gp3tools_naming_policy")
  expect_equal(policy$canonical_prefix, "summarise_")
  expect_equal(policy$compatibility_prefix, "summarize_")
  expect_match(
    policy$rules$policy[policy$rules$rule == "existing_american_names"],
    "Retain"
  )
})

test_that("naming audit identifies paired and unpaired names", {
  audit <- audit_gazepoint_naming_consistency(
    c(
      "summarise_alpha",
      "summarize_alpha",
      "summarise_beta",
      "summarize_gamma"
    )
  )

  expect_s3_class(audit, "gazepoint_naming_audit")
  expect_equal(audit$summary$n_paired, 1)
  expect_equal(audit$summary$n_canonical_only, 1)
  expect_equal(audit$summary$n_missing_british_alias, 1)
  expect_equal(audit$summary$status, "needs_review")
})

test_that("current namespace contains no American-only summary names", {
  audit <- audit_gazepoint_naming_consistency()

  expect_equal(audit$summary$n_missing_british_alias, 0)
  expect_equal(audit$summary$status, "pass")
})

test_that("naming audit can be exported", {
  audit <- audit_gazepoint_naming_consistency(
    c("summarise_alpha", "summarize_alpha")
  )
  path <- tempfile(fileext = ".csv")

  written <- write_gazepoint_naming_audit(audit, path)

  expect_true(file.exists(written))
  exported <- utils::read.csv(written)
  expect_equal(exported$status, "paired")
})
