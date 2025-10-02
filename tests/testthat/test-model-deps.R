# model_deps() tests aligned with cli alerts, with stdout silenced

testthat::test_that("model_deps() returns list(required, missing) of character", {
  pattern <- "No optional model dependencies discovered\\.|All required packages are installed:|Missing required packages:"

  out <- testthat::capture_output({
    testthat::expect_message(
      res <- writeAlizer::model_deps(),
      regexp = pattern
    )
  })

  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("required", "missing") %in% names(res)))
  testthat::expect_type(res$required, "character")
  testthat::expect_type(res$missing,  "character")
})

testthat::test_that("model_deps() respects writeAlizer.required_pkgs and preserves qualifiers", {
  withr::local_options(writeAlizer.required_pkgs = c("thispkgdoesnotexist123", "anotherFakePkg (>= 1.0)"))

  no_op <- function(...) invisible(NULL)

  # Silence cli output by mocking the cli emitters to no-ops
  testthat::with_mocked_bindings(
    cli_alert_info    = no_op,
    cli_alert_success = no_op,
    cli_alert_danger  = no_op,
    cli_ul            = no_op,
    cli_code          = no_op,
    .env = asNamespace("cli"),
    {
      md <- writeAlizer::model_deps()
    }
  )

  # Missing must contain base names (qualifier stripped)
  testthat::expect_true("thispkgdoesnotexist123" %in% md$missing)
  testthat::expect_true("anotherFakePkg" %in% md$missing)

  # Required must preserve tokens (including qualifier)
  testthat::expect_true(any(md$required == "thispkgdoesnotexist123"))
  testthat::expect_true(any(md$required == "anotherFakePkg (>= 1.0)"))
})

testthat::test_that("model_deps() does not flag installed packages as missing", {
  withr::local_options(writeAlizer.required_pkgs = "stats")

  out <- testthat::capture_output({
    testthat::expect_message(
      res <- writeAlizer::model_deps(),
      regexp = "All required packages are installed:"
    )
  })

  testthat::expect_false("stats" %in% res$missing)
})

# ---- UPDATED: capture cli output via capture_output + capture_messages ----

testthat::test_that("model_deps() is quick, emits cli alerts, and returns a named list", {
  withr::local_options(writeAlizer.required_pkgs = c("stats", "utils"))

  out <- testthat::capture_output({
    msg <- testthat::capture_messages({
      md <- writeAlizer::model_deps()
      testthat::expect_type(md$required, "character")
      testthat::expect_type(md$missing,  "character")
      testthat::expect_named(md, c("required", "missing"))
      testthat::expect_false("stats" %in% md$missing)
      testthat::expect_false("utils" %in% md$missing)
    })
    testthat::expect_true(any(grepl("All required packages are installed:", msg)))
  })
})

testthat::test_that("model_deps() prints missing list and install command when something is missing", {
  withr::local_options(
    writeAlizer.required_pkgs = c("thispkgdoesnotexist123", "anotherFakePkg", "stats")
  )

  out <- testthat::capture_output({
    msg <- testthat::capture_messages({
      md <- writeAlizer::model_deps()
      testthat::expect_true(all(c("thispkgdoesnotexist123", "anotherFakePkg") %in% md$missing))
      testthat::expect_false("stats" %in% md$missing)
    })

    testthat::expect_true(any(grepl("Missing required packages:", msg)))
    testthat::expect_true(any(grepl('install\\.packages\\(c\\("thispkgdoesnotexist123", "anotherFakePkg"\\)\\)', msg)))
  })
})

testthat::test_that("model_deps() preserves version qualifiers in `required` but strips them in `missing`", {
  withr::local_options(writeAlizer.required_pkgs = c("fakePkgA (>= 9.9)", "fakePkgB"))

  out <- testthat::capture_output({
    msg <- testthat::capture_messages({
      md <- writeAlizer::model_deps()
      testthat::expect_true("fakePkgA (>= 9.9)" %in% md$required)
      testthat::expect_true("fakePkgB" %in% md$required)
      testthat::expect_true("fakePkgA" %in% md$missing)
      testthat::expect_true("fakePkgB" %in% md$missing)
    })

    testthat::expect_true(any(grepl("Missing required packages:", msg)))
  })
})

# Optional coverage for deprecated alias (suppress warning)
testthat::test_that("internal install_model_deps() forwards to model_deps()", {
  withr::local_options(writeAlizer.required_pkgs = c("stats"))

  out <- testthat::capture_output({
    msg <- suppressWarnings(testthat::capture_messages({
      out_val <- writeAlizer:::install_model_deps()
      testthat::expect_true(is.list(out_val))
      testthat::expect_named(out_val, c("required", "missing"))
    }))
    testthat::expect_true(any(grepl("All required packages are installed:", msg)))
  })
})
