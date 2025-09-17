testthat::test_that("model_deps() returns list(required, missing) of character", {
  # We don't set any hook here; we just check structure and that a message is emitted.
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
  withr::local_options(writeAlizer.required_pkgs = c("thispkgdoesnotexist123", "anotherFakePkg (>= 1.0)"))

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
  withr::local_options(writeAlizer.required_pkgs = "stats")

  testthat::expect_message(
    res <- writeAlizer::model_deps(),
    regexp = "All required packages are installed"
  )

  testthat::expect_false("stats" %in% res$missing)
})

# ---- FIXED: switch to capture_messages() for message text ----

testthat::test_that("model_deps() is quiet, fast, and returns a named list", {
  withr::local_options(writeAlizer.required_pkgs = c("stats", "utils"))

  msg <- testthat::capture_messages({
    md <- writeAlizer::model_deps()
    testthat::expect_type(md$required, "character")
    testthat::expect_type(md$missing,  "character")
    testthat::expect_named(md, c("required", "missing"))
    testthat::expect_false("stats" %in% md$missing)
    testthat::expect_false("utils" %in% md$missing)
  })

  testthat::expect_true(any(grepl("^All required packages are installed:", msg)))
})

testthat::test_that("model_deps() prints install hint when something is missing", {
  withr::local_options(
    writeAlizer.required_pkgs = c("thispkgdoesnotexist123", "anotherFakePkg", "stats")
  )

  msg <- testthat::capture_messages({
    md <- writeAlizer::model_deps()
    testthat::expect_true(all(c("thispkgdoesnotexist123", "anotherFakePkg") %in% md$missing))
    testthat::expect_false("stats" %in% md$missing)
  })

  testthat::expect_true(any(grepl("Install them manually", msg, fixed = TRUE)))
  testthat::expect_true(any(grepl('install.packages\\(c\\("thispkgdoesnotexist123", "anotherFakePkg"\\)\\)', msg)))
})

testthat::test_that("model_deps() preserves version qualifiers in `required` but strips them in `missing`", {
  withr::local_options(writeAlizer.required_pkgs = c("fakePkgA (>= 9.9)", "fakePkgB"))

  msg <- testthat::capture_messages({
    md <- writeAlizer::model_deps()
    testthat::expect_true("fakePkgA (>= 9.9)" %in% md$required)
    testthat::expect_true("fakePkgB" %in% md$required)
    testthat::expect_true("fakePkgA" %in% md$missing)
    testthat::expect_true("fakePkgB" %in% md$missing)
  })

  testthat::expect_true(any(grepl("Install them manually", msg, fixed = TRUE)))
})

# Optional coverage for deprecated alias (suppress warning)
testthat::test_that("internal install_model_deps() forwards to model_deps()", {
  withr::local_options(writeAlizer.required_pkgs = c("stats"))

  msg <- suppressWarnings(testthat::capture_messages({
    out <- writeAlizer:::install_model_deps()
    testthat::expect_true(is.list(out))
    testthat::expect_named(out, c("required", "missing"))
  }))

  testthat::expect_true(any(grepl("^All required packages are installed:", msg)))
})
