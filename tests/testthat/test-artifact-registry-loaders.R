# Tests that exercise the artifact registry helpers with and without mocks.
# All tests run offline and never hit the network.

testthat::test_that(".wa_parts_for() returns ordered rows for a known model", {
  # Pick a model that definitely exists in inst/metadata/artifacts.csv
  parts <- writeAlizer:::.wa_parts_for("rda", "coh_mod1")
  testthat::expect_s3_class(parts, "data.frame")
  testthat::expect_true(nrow(parts) >= 1L)
  testthat::expect_true(all(c("kind","model","part","file","url") %in% names(parts)))

  # Ensure we return parts sorted by 'part'
  if ("part" %in% names(parts)) {
    testthat::expect_true(is.unsorted(parts$part, strictly = FALSE) == FALSE)
  }
})

testthat::test_that(".wa_load_model_rdas() short-circuits for the built-in 'example' with mock_dir", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Seed a tiny example model (creates example.rda in tmp)
  writeAlizer:::wa_seed_example_models("example", dir = tmp)

  env <- new.env(parent = emptyenv())
  ok <- writeAlizer:::.wa_load_model_rdas("example", envir = env)

  testthat::expect_true(isTRUE(ok))
  testthat::expect_true("fit" %in% ls(env, all.names = TRUE))
  testthat::expect_s3_class(get("fit", env), "lm")
})

testthat::test_that(".wa_load_model_rdas() errors if mock_dir set but example.rda missing", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  env <- new.env(parent = emptyenv())
  testthat::expect_error(
    writeAlizer:::.wa_load_model_rdas("example", envir = env),
    "seed it via wa_seed_example_models",
    fixed = TRUE
  )
})

testthat::test_that(".wa_load_model_rdas() prefers mock candidates for non-example models", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Use an actual multi-part model so we exercise the loop
  rda_parts <- writeAlizer:::.wa_parts_for("rda", "coh_mod1")
  testthat::skip_if(nrow(rda_parts) == 0L, "No registry rows for 'coh_mod1'")

  # Create mock .rda files named exactly like the registry expects
  for (i in seq_len(nrow(rda_parts))) {
    p <- rda_parts[i, ]
    fit <- stats::lm(mpg ~ wt, data = mtcars)   # small, no extra deps
    save(fit, file = file.path(tmp, basename(p$file)))
  }

  env <- new.env(parent = emptyenv())
  ok <- writeAlizer:::.wa_load_model_rdas("coh_mod1", envir = env)

  testthat::expect_true(isTRUE(ok))
  testthat::expect_true("fit" %in% ls(env, all.names = TRUE))
  testthat::expect_s3_class(get("fit", env), "lm")
})

testthat::test_that(".wa_load_fits_list() short-circuits for 'example' and returns a named list", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  writeAlizer:::wa_seed_example_models("example", dir = tmp)
  fits <- writeAlizer:::.wa_load_fits_list("example")

  testthat::expect_true(is.list(fits))
  testthat::expect_identical(names(fits), "example")
  testthat::expect_s3_class(fits$example, "lm")
})

testthat::test_that(".wa_load_fits_list() uses mock candidates and returns canonical names", {
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  # Prepare mocks for a real model
  rda_parts <- writeAlizer:::.wa_parts_for("rda", "coh_mod1")
  testthat::skip_if(nrow(rda_parts) == 0L, "No registry rows for 'coh_mod1'")

  # Save small fits under the exact filenames expected by the registry
  for (i in seq_len(nrow(rda_parts))) {
    p <- rda_parts[i, ]
    fit <- stats::lm(mpg ~ wt, data = mtcars)
    save(fit, file = file.path(tmp, basename(p$file)))
  }

  fits <- writeAlizer:::.wa_load_fits_list("coh_mod1")
  testthat::expect_true(is.list(fits))
  testthat::expect_equal(length(fits), nrow(rda_parts))

  expected_names <- tools::file_path_sans_ext(basename(rda_parts$file))
  testthat::expect_setequal(names(fits), expected_names)
  lapply(fits, function(fit) testthat::expect_s3_class(fit, "lm"))
})

testthat::test_that(".wa_require_pkgs_for_fits() consults train$modelInfo$library", {
  # Construct a minimal 'train'-like object with empty library so it won't error
  fake_train <- structure(
    list(modelInfo = list(library = character(0))),
    class = "train"
  )
  # Should complete silently (no missing packages)
  testthat::expect_invisible(writeAlizer:::.wa_require_pkgs_for_fits(list(fake_train)))
})
