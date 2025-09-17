testthat::test_that("model_deps() returns list(required, missing) of character", {
  # We don't set any hook here; we just check structure and that a message is emitted.
  # The message can be one of:
  #  - "No optional model dependencies..."
  #  - "All required packages are installed..."
  #  - "Missing required packages..."
  pattern <- "No optional model dependencies|All required packages are installed|Missing required packages"

  testthat::expect_message(
    res <- writeAlizer::model_deps(),
    regexp = pattern
  )

  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("required", "missing") %in% names(res)))
  testthat::expect_type(res$required, "character")
  testthat::expect_type(res$missing,  "character")
})

testthat::test_that("model_deps() respects writeAlizer.required_pkgs and prints install hint", {
  # Inject two fake packages: one with a version qualifier
  withr::local_options(writeAlizer.required_pkgs = c("thispkgdoesnotexist123", "anotherFakePkg (>= 1.0)"))

  # Should emit an "Install them manually" hint
  testthat::expect_message(
    res <- writeAlizer::model_deps(),
    regexp = "Install them manually"
  )

  # Missing must contain base names (qualifier stripped)
  testthat::expect_true("thispkgdoesnotexist123" %in% res$missing)
  testthat::expect_true("anotherFakePkg" %in% res$missing)

  # Required must preserve tokens (including qualifier)
  testthat::expect_true(any(res$required == "thispkgdoesnotexist123"))
  testthat::expect_true(any(res$required == "anotherFakePkg (>= 1.0)"))
})

testthat::test_that("model_deps() does not flag installed packages as missing", {
  # 'stats' should be available on any R install
  withr::local_options(writeAlizer.required_pkgs = "stats")

  # Message should be the 'All required packages are installed' path
  testthat::expect_message(
    res <- writeAlizer::model_deps(),
    regexp = "All required packages are installed"
  )

  testthat::expect_false("stats" %in% res$missing)
})
