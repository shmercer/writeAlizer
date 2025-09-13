# tests/testthat/test-install-model-deps-more.R

testthat::test_that("install_model_deps(dry_run=TRUE) returns Suggests tokens", {
  pkgs <- writeAlizer::install_model_deps(dry_run = TRUE)
  expect_true(is.character(pkgs))
  expect_true(length(pkgs) >= 1L)
  # Should include at least one version-qualified token from your DESCRIPTION
  expect_true(any(grepl("^testthat\\s*\\(>=", pkgs)))
})

testthat::test_that("helper override wins on dry_run and empty helper falls back", {
  old <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old))

  # A) Helper returns custom list
  options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) {
    if (isTRUE(return_pkgs)) return(c("A_pkg_from_helper","B_pkg_from_helper"))
    invisible(NULL)
  })
  pk <- writeAlizer::install_model_deps(dry_run = TRUE)
  expect_identical(pk, c("A_pkg_from_helper","B_pkg_from_helper"))

  # B) Helper returns empty -> fall back to Suggests
  options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) {
    if (isTRUE(return_pkgs)) return(character(0))
    invisible(NULL)
  })
  pk2 <- writeAlizer::install_model_deps(dry_run = TRUE)
  expect_true(length(pk2) >= 1L)
  expect_true(any(grepl("withr", pk2)))
})

testthat::test_that("install_model_deps installs only truly missing pkgs (version stripped)", {
  old <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Capture installs; pretend 'Cubist' is missing, others are present
  installed <- NULL
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) {
      # present for all except Cubist
      !identical(pkg, "Cubist")
    }
  )
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { installed <<- pkgs; invisible(NULL) }
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  expect_true(is.character(pk) && length(pk) >= 1L)
  # Only Cubist should be requested (no version suffix)
  expect_identical(installed, "Cubist")
})

# ---- Non-dry-run: all packages present -> no install.packages call ----
testthat::test_that("install_model_deps: no installs when all pkgs present", {
  old <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  called_install <- FALSE

  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) TRUE  # everything is installed
  )
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called_install <<- TRUE; invisible(NULL) }
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  expect_false(isTRUE(called_install))
  expect_true(is.character(pk) && length(pk) >= 1L)
})

# ---- Non-dry-run: helper is called, but canonical Suggests are still returned ----
testthat::test_that("install_model_deps: helper called on non-dry-run; returns canonical Suggests", {
  helper_called <- FALSE
  old <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old))

  options(writeAlizer.require_pkgs_for_fits = function(model, install, return_pkgs) {
    if (isTRUE(install)) helper_called <<- TRUE
    if (isTRUE(return_pkgs)) return(c("SHOULD_NOT_BE_RETURNED"))
    invisible(NULL)
  })

  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) TRUE
  )
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) invisible(NULL)
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  expect_true(helper_called)
  # Should be the canonical Suggests tokens, not the helper's dry-run list
  expect_false(any(pk == "SHOULD_NOT_BE_RETURNED"))
  expect_true(any(grepl("withr", pk)))
})

# ---- Non-dry-run: multiple missing packages -> install both (names stripped, de-duped) ----
testthat::test_that("install_model_deps installs multiple missing pkgs with stripped names", {
  old <- getOption("writeAlizer.require_pkgs_for_fits", NULL)
  withr::defer(options(writeAlizer.require_pkgs_for_fits = old))
  options(writeAlizer.require_pkgs_for_fits = NULL)

  # Pretend only 'gbm' and 'glmnet' are missing; everything else is installed
  called <- NULL
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) {
      !(pkg %in% c("gbm", "glmnet"))
    }
  )
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called <<- pkgs; invisible(NULL) }
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  expect_true(is.character(pk) && length(pk) >= 1L)

  # We should have attempted to install exactly these two, with no version tokens
  expect_setequal(called, c("gbm","glmnet"))
})

# ---- Dry-run: displays canonical tokens (may include version qualifier) but unique ----
testthat::test_that("install_model_deps(dry_run=TRUE) returns unique Suggests tokens", {
  pk1 <- writeAlizer::install_model_deps(dry_run = TRUE)
  pk2 <- unique(pk1)
  expect_identical(pk1, pk2)
  # keep behavior flexible but ensure at least one entry remains
  expect_gt(length(pk1), 0L)
})

# ---- Non-dry-run: duplicates + version tokens cleaned for install.packages ----
testthat::test_that("install_model_deps canonicalizes tokens for installation", {
  # Fabricate Suggests: duplicated 'withr' and versioned 'testthat'
  testthat::local_mocked_bindings(
    .package = "utils",
    packageDescription = function(pkg, fields) "withr, testthat (>= 3.1.0), withr"
  )

  # Pretend both are missing
  called <- NULL
  testthat::local_mocked_bindings(
    .package = "base",
    requireNamespace = function(pkg, quietly = TRUE) FALSE
  )
  testthat::local_mocked_bindings(
    .package = "utils",
    install.packages = function(pkgs, ...) { called <<- pkgs; invisible(NULL) }
  )

  pk <- writeAlizer::install_model_deps(dry_run = FALSE)
  expect_true(is.character(pk) && length(pk) >= 1L)
  # Must have stripped/unique names for install, not "testthat (>= ...)"
  expect_setequal(called, c("withr", "testthat"))
})
