# tests/testthat/test-preprocess-and-download.R

testthat::test_that("registry-driven preprocess works for 3-part RB models (rb_mod2)", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  # Make a temp dir and create fake varlists there
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  all_cols <- setdiff(colnames(rb), "ID")
  testthat::skip_if(length(all_cols) < 15, "Not enough columns in sample RB file for this test")

  v1 <- all_cols[1:5]; v2 <- all_cols[6:10]; v3 <- all_cols[11:15]
  saveRDS(v1, file.path(tmp, "rb_mod2a_vars.rds"))
  saveRDS(v2, file.path(tmp, "rb_mod2b_vars.rds"))
  saveRDS(v3, file.path(tmp, "rb_mod2c_vars.rds"))

  out <- writeAlizer:::preprocess("rb_mod2", rb)
  testthat::expect_type(out, "list"); testthat::expect_length(out, 3)

  for (i in 1:3) {
    df <- out[[i]]
    testthat::expect_s3_class(df, "data.frame")
    testthat::expect_true("ID" %in% names(df))
    sel <- list(v1, v2, v3)[[i]]
    testthat::expect_true(all(sel %in% names(df)))
    testthat::expect_true(all(vapply(df[sel], is.numeric, logical(1))))
    m <- colMeans(df[sel], na.rm = TRUE)
    testthat::expect_true(all(abs(m) < 1e-6))
  }
})

testthat::test_that("registry-driven preprocess works for 1-part Coh models (coh_mod3narr)", {
  coh_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
  testthat::skip_if_not(coh_path != "", "sample_coh.csv not found in inst/extdata")
  coh <- writeAlizer::import_coh(coh_path)

  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  all_cols <- setdiff(colnames(coh), "ID")
  testthat::skip_if(length(all_cols) < 6, "Not enough columns in sample Coh file for this test")

  v1 <- all_cols[1:6]
  saveRDS(v1, file.path(tmp, "coh_narr_vars.rds"))

  out <- writeAlizer:::preprocess("coh_mod3narr", coh)

  testthat::expect_type(out, "list"); testthat::expect_length(out, 1)
  df <- out[[1]]
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true("ID" %in% names(df))
  testthat::expect_true(all(v1 %in% names(df)))
  testthat::expect_true(all(vapply(df[v1], is.numeric, logical(1))))
  m <- colMeans(df[v1], na.rm = TRUE)
  testthat::expect_true(all(abs(m) < 1e-6))
})

testthat::test_that("models with no preprocessing preserve behavior", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  out1 <- writeAlizer:::preprocess("rb_mod1", rb)
  testthat::expect_length(out1, 6)
  for (df in out1) testthat::expect_identical(nrow(df), nrow(rb))

  out2 <- writeAlizer:::preprocess("gamet_cws1", rb) # structure check only
  testthat::expect_length(out2, 1)
  testthat::expect_identical(nrow(out2[[1]]), nrow(rb))
})

testthat::test_that("unknown model key produces a clear error", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(rb_path != "", "sample_rb.csv not found in inst/extdata")
  rb <- writeAlizer::import_rb(rb_path)

  testthat::expect_error(
    writeAlizer:::preprocess("no_such_model", rb),
    regexp = "No variable lists registered|Unknown model key"
  )
})

testthat::test_that(".wa_load_fits_list loads trained models by filename", {
  # We create six tiny .rda files that match rb_mod1 registry filenames.
  # Each contains an object named 'fit' so the loader's preference picks it.
  tmp <- withr::local_tempdir()
  withr::local_options(writeAlizer.mock_dir = tmp)

  mk_rda <- function(path) { fit <- stats::lm(mpg ~ wt, data = mtcars); save(fit, file = path) }
  files <- c("rb_mod1a.rda","rb_mod1b.rda","rb_mod1c.rda","rb_mod1d.rda","rb_mod1e.rda","rb_mod1f.rda")
  for (f in files) mk_rda(file.path(tmp, f))

  fits <- writeAlizer:::.wa_load_fits_list("rb_mod1")

  # We expect six fits, named after the filenames (without extension)
  testthat::expect_type(fits, "list")
  testthat::expect_length(fits, 6)
  testthat::expect_setequal(names(fits), sub("\\.rda$", "", files))

  # Spot check classes; our dummy is an 'lm'
  testthat::expect_true(all(vapply(fits, inherits, logical(1), what = "lm")))
})
