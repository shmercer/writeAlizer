# tests/testthat/test-install-model-deps.R

testthat::test_that("dry_run returns unique tokens parsed from Suggests", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Simulate DESCRIPTION: duplicate + version-qualified token
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) {
      testthat::expect_identical(pkg, "writeAlizer")
      testthat::expect_identical(fields, "Suggests")
      "withr, testthat (>= 3.2.0), withr"
    }
  )

  pkgs <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_identical(pkgs, c("withr", "testthat (>= 3.2.0)"))
  testthat::expect_true(all(nzchar(pkgs)))
})

testthat::test_that("options hook overrides Suggests on dry_run", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))

  options(writeAlizer.require_pkgs_for_fits = function() c("A_pkg_from_hook", "B_pkg_from_hook (>= 1.2.3)"))

  # Even if Suggests had something else, hook must win
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) "withr, testthat (>= 3.2.0)"
  )

  pk <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_identical(pk, c("A_pkg_from_hook", "B_pkg_from_hook (>= 1.2.3)"))
})

testthat::test_that("empty options hook returns character(0) on dry_run", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))

  options(writeAlizer.require_pkgs_for_fits = function() character(0))

  # packageDescription should not be consulted when a hook is supplied
  called_pd <- FALSE
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) { called_pd <<- TRUE; "ignored" }
  )

  pk <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_identical(pk, character(0))
  testthat::expect_false(called_pd)
})

testthat::test_that("non-dry-run installs only truly missing packages (strip versions, dedupe)", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))

  # Provide duplicates and version-qualified token
  options(writeAlizer.require_pkgs_for_fits = function() c("withr", "testthat (>= 3.2.0)", "withr"))

  # Pretend withr is installed, testthat is missing
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) {
      testthat::expect_true(quietly)
      !identical(pkg, "testthat")
    }
  )

  installed <- NULL
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { installed <<- pkgs; invisible(NULL) }
  )

  ret <- writeAlizer::install_model_deps(dry_run = FALSE)
  # Should have requested only "testthat" (no version suffix)
  testthat::expect_identical(installed, "testthat")
  # Return value: vector of base names checked (order not critical, but both should appear)
  testthat::expect_setequal(ret, c("withr", "testthat"))
})

testthat::test_that("non-dry-run: no installs when all packages present", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Mock Suggests
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) "withr, testthat (>= 3.2.0)"
  )

  # Everything installed
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) TRUE
  )

  called_install <- FALSE
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called_install <<- TRUE; invisible(NULL) }
  )

  ret <- writeAlizer::install_model_deps(dry_run = FALSE)
  testthat::expect_false(isTRUE(called_install))
  testthat::expect_setequal(ret, c("withr", "testthat"))
})

testthat::test_that("non-dry-run canonicalizes Suggests (strip versions, unique) when no hook", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Fabricate Suggests with duplicates and version qualifier
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) "withr, testthat (>= 3.1.0), withr"
  )

  # Pretend both are missing to force install
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) FALSE
  )

  called <- NULL
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called <<- pkgs; invisible(NULL) }
  )

  ret <- writeAlizer::install_model_deps(dry_run = FALSE)
  testthat::expect_setequal(called, c("withr", "testthat"))
  testthat::expect_setequal(ret, c("withr", "testthat"))
})

testthat::test_that("empty discovery behavior: returns character(0) (dry_run) or invisible(character(0))", {
  old_opt <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old_opt))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Suggests empty/NA
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) NA_character_
  )

  pk_dry <- writeAlizer::install_model_deps(dry_run = TRUE)
  testthat::expect_identical(pk_dry, character(0))

  # Non-dry-run should still return character(0) (invisibly), and not attempt to install
  called_install <- FALSE
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called_install <<- TRUE; invisible(NULL) }
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  testthat::expect_identical(pk, character(0))
  testthat::expect_false(isTRUE(called_install))
})
