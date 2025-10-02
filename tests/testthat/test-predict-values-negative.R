testthat::test_that("preprocess() errors on unknown model key", {
  testthat::skip_on_cran()
  dat <- data.frame(ID = 1:3, x = 1:3, y = 3:1)
  testthat::expect_error(writeAlizer::preprocess("no_such_model", dat), "No variable lists|Unknown model")
})

testthat::test_that("predict_quality() errors when trained fits are missing", {
  dat <- data.frame(ID = c("a","b"))
  testthat::with_mocked_bindings(
    preprocess = function(model, data) list(data, data, data), # expect 3 splits
    .wa_load_fits_list = function(model) list(),               # but no fits present
    .env = asNamespace("writeAlizer"),
    {
      testthat::expect_error(
        writeAlizer::predict_quality("rb_mod3all_v2", dat),
        "Missing trained objects",
        class = "writeAlizer_artifact_missing"
      )
    }
  )
})

testthat::test_that("predict_quality() detects mismatch between expected submodels and preprocessed splits", {
  dat <- data.frame(ID = 1:2, x = 1:2)

  testthat::with_mocked_bindings(
    # Force preprocessing to return a single split, while coh_mod3all expects 3
    preprocess = function(model, data) list(data),
    # Provide a non-empty fits list so we get to the mismatch branch
    .wa_load_fits_list = function(model) list(coh_mod3exp = 1, coh_mod3narr = 1, coh_mod3per = 1),
    .env = asNamespace("writeAlizer"),
    {
      testthat::expect_error(
        writeAlizer::predict_quality("coh_mod3all", dat),
        regexp = "Internal mismatch: expected 3 sub-model\\(s\\), but preprocessing produced 1 split\\(s\\)\\.",
        class  = "writeAlizer_internal_mismatch"
      )
    }
  )
})

testthat::test_that("predict_quality() missing fits includes mock_dir hint when set", {
  dat <- data.frame(ID = c("a","b"))
  withr::local_options(writeAlizer.mock_dir = tempdir())

  testthat::with_mocked_bindings(
    preprocess = function(model, data) list(data, data, data),
    .wa_load_fits_list = function(model) list(),    # force missing fits
    .env = asNamespace("writeAlizer"),
    {
      expect_error(
        writeAlizer::predict_quality("rb_mod3all_v2", dat),
        "writeAlizer.mock_dir is set|wa_seed_example_models\\(\"example\"\\)",
        class = "writeAlizer_artifact_missing"
      )
    }
  )
})

testthat::test_that("predict_quality() errors clearly on unknown model key", {
  dat <- data.frame(ID = 1:2, x = 1:2)

  testthat::with_mocked_bindings(
    # Let preprocess succeed (so we can hit the unknown-key branch in predict_quality)
    preprocess = function(model, data) list(data),
    # Prevent spurious errors from artifact loading before the switch() runs
    .wa_load_fits_list = function(model) list(),
    .env = asNamespace("writeAlizer"),
    {
      testthat::expect_error(
        writeAlizer::predict_quality("not_a_real_model", dat),
        regexp = "Unknown model key|Valid options are",
        class = "writeAlizer_model_unknown"
      )
    }
  )
})

