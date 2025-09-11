testthat::test_that("import_rb keeps 1:404 by NAME and drops 405+ by NAME from sample header", {
  rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
  testthat::skip_if_not(nzchar(rb_path) && file.exists(rb_path))

  # Read header only from the packaged sample (replicates helper logic)
  first <- readLines(rb_path, n = 1, warn = FALSE)
  skip  <- if (identical(first, "SEP=,")) 1L else 0L
  hdr   <- utils::read.csv(rb_path, nrows = 0, check.names = TRUE, skip = skip)
  nm    <- colnames(hdr)
  testthat::skip_if(length(nm) < 405, "Sample RB header has <405 columns")

  keep <- nm[seq_len(404L)]
  drop <- nm[(404L + 1L):length(nm)]

  rb <- writeAlizer::import_rb(rb_path)
  testthat::expect_s3_class(rb, "data.frame")
  testthat::expect_true("ID" %in% names(rb))

  # all 'keep' names (except File.name, which is renamed to ID) should be present
  exp_present <- setdiff(keep, "File.name")
  testthat::expect_true(all(exp_present %in% names(rb)))

  # all 'drop' names must be absent
  testthat::expect_true(all(!drop %in% names(rb)))
})
