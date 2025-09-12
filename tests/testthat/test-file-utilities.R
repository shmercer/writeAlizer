testthat::test_that("import_* happy paths on sample files", {
  testthat::skip_on_cran()

  fp_rb  <- system.file("extdata", "sample_rb.csv",   package = "writeAlizer")
  fp_coh <- system.file("extdata", "sample_coh.csv",  package = "writeAlizer")
  fp_gam <- system.file("extdata", "sample_gamet.csv",package = "writeAlizer")

  testthat::skip_if(!file.exists(fp_rb)  || !file.exists(fp_coh) || !file.exists(fp_gam),
                    "sample files not installed")

  rb  <- writeAlizer::import_rb(fp_rb)
  coh <- writeAlizer::import_coh(fp_coh)
  gam <- writeAlizer::import_gamet(fp_gam)

  for (df in list(rb, coh, gam)) {
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_true("ID" %in% names(df))
  }
})

testthat::test_that("import_* error on missing file", {
  testthat::skip_on_cran()
  testthat::expect_error(suppressWarnings(writeAlizer::import_rb("nope.csv")))
  testthat::expect_error(suppressWarnings(writeAlizer::import_coh("nope.csv")))
  testthat::expect_error(suppressWarnings(writeAlizer::import_gamet("nope.csv")))
})
