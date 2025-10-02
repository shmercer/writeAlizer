testthat::test_that("import_rb happy path on sample file", {
  testthat::skip_on_cran()

  fp <- wa_sample_path("sample_rb.csv")
  testthat::skip_if(!nzchar(fp) || !file.exists(fp),
                    sprintf("sample_rb.csv not found at: %s", fp))

  rb <- writeAlizer::import_rb(fp)
  testthat::expect_s3_class(rb, "data.frame")
  testthat::expect_true("ID" %in% names(rb))
})

testthat::test_that("import_coh happy path on sample file", {
  testthat::skip_on_cran()

  fp <- wa_sample_path("sample_coh.csv")
  testthat::skip_if(!nzchar(fp) || !file.exists(fp),
                    sprintf("sample_coh.csv not found at: %s", fp))

  coh <- writeAlizer::import_coh(fp)
  testthat::expect_s3_class(coh, "data.frame")
  testthat::expect_true("ID" %in% names(coh))
})

testthat::test_that("import_gamet happy path on sample file", {
  testthat::skip_on_cran()

  fp <- wa_sample_path("sample_gamet.csv")
  testthat::skip_if(!nzchar(fp) || !file.exists(fp),
                    sprintf("sample_gamet.csv not found at: %s", fp))

  gam <- writeAlizer::import_gamet(fp)
  testthat::expect_s3_class(gam, "data.frame")
  testthat::expect_true("ID" %in% names(gam))
})

# Negative path tests stay combined (they don't depend on sample files).
testthat::test_that("import_* error on missing file", {
  testthat::skip_on_cran()
  testthat::expect_error(suppressWarnings(writeAlizer::import_rb("nope.csv")))
  testthat::expect_error(suppressWarnings(writeAlizer::import_coh("nope.csv")))
  testthat::expect_error(suppressWarnings(writeAlizer::import_gamet("nope.csv")))
})

