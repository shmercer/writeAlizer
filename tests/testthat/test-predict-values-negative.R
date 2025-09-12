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
        "Missing trained objects"
      )
    }
  )
})

testthat::test_that("predict_quality() detects mismatch between expected submodels and preprocessed splits", {
  dat <- data.frame(ID = "x")
  testthat::with_mocked_bindings(
    preprocess = function(model, data) list(data),  # return 1 split
    .wa_load_fits_list = function(model) list(),    # value won’t be used
    .env = asNamespace("writeAlizer"),
    {
      testthat::expect_error(
        writeAlizer::predict_quality("coh_mod3all", dat),
        "Mismatch: expected"
      )
    }
  )
})
