testthat::test_that("import_rb keeps 1:404 by NAME and drops 405+ by NAME from sample header", {
  withr::local_options(writeAlizer.offline = FALSE)  # <- prevents helper-network from skipping

  rb_path <- wa_sample_path("sample_rb.csv")
  testthat::skip_if(!nzchar(rb_path) || !file.exists(rb_path),
                    sprintf("sample_rb.csv not found at: %s", rb_path))

  first <- readLines(rb_path, n = 1, warn = FALSE)
  skip  <- if (identical(first, "SEP=,")) 1L else 0L
  hdr   <- utils::read.csv(rb_path, nrows = 0, check.names = TRUE, skip = skip)
  nm    <- colnames(hdr)

  testthat::skip_if(length(nm) < 10,
                    sprintf("Sample RB header too narrow (%d cols); need >= 10", length(nm)))

  keep_boundary <- min(404L, length(nm) - 1L)
  keep <- nm[seq_len(keep_boundary)]
  drop <- if (keep_boundary + 1L <= length(nm)) nm[(keep_boundary + 1L):length(nm)] else character(0)

  rb <- writeAlizer::import_rb(rb_path)
  testthat::expect_s3_class(rb, "data.frame")
  testthat::expect_true("ID" %in% names(rb))

  exp_present <- setdiff(keep, "File.name")
  testthat::expect_true(all(exp_present %in% names(rb)))

  if (length(drop) > 0) {
    testthat::expect_true(all(!drop %in% names(rb)))
  } else {
    testthat::expect_true(TRUE)
  }
})
