# tests/testthat/test-install-model-deps.R

testthat::test_that("install_model_deps(dry_run=TRUE) returns package names", {
  pkgs <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_type(pkgs, "character")
  # the DESCRIPTION Suggests isn't empty, so we expect >=1
  testthat::expect_true(length(pkgs) >= 1)
})
