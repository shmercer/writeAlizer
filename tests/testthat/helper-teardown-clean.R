# tests/testthat/helper-teardown-clean.R
testthat::teardown({
  md <- getOption("writeAlizer.mock_dir")
  if (is.character(md) && nzchar(md) && dir.exists(md)) {
    unlink(md, recursive = TRUE, force = TRUE)
  }
})
