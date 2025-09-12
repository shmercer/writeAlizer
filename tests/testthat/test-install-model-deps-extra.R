testthat::test_that("install_model_deps(dry_run=TRUE) returns character vector (stable)", {
  pkgs <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_type(pkgs, "character")
  testthat::expect_true(all(nzchar(pkgs)) || length(pkgs) == 0)
})
