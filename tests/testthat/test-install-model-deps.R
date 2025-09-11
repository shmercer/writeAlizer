testthat::test_that("install_model_deps(dry_run=TRUE) returns package names", {
  pkgs <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_type(pkgs, "character")
  testthat::expect_true(length(pkgs) >= 1)
})
